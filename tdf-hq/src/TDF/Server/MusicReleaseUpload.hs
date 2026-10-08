{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.MusicReleaseUpload
  ( createMusicUpload
  , bindMusicUploadProvider
  , signMusicUploadPart
  , recordMusicUploadPart
  , completeMusicUpload
  , confirmMusicUpload
  , cancelMusicUpload
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Servant
import System.Environment (lookupEnv)

import TDF.API.MusicRelease
import TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.CMS.Models as CMS
import TDF.DB (Env(..))
import TDF.MusicRelease.Storage.S3
import TDF.Server.MusicReleaseAssets (loadMusicS3SigningConfig)

type AppM = ReaderT Env Handler

data UploadContext = UploadContext
  { uploadSessionId :: UUID
  , uploadReleaseId :: UUID
  , uploadVersionId :: UUID
  , uploadRecordingId :: Maybe UUID
  , uploadAssetRole :: Text
  , uploadBucket :: Text
  , uploadObjectKey :: Text
  , uploadProviderId :: Maybe Text
  , uploadExpectedMediaType :: Maybe Text
  , uploadExpectedSize :: Int64
  , uploadExpectedSha256 :: Text
  , uploadPartSize :: Int
  , uploadStatus :: Text
  , uploadExpiresAt :: UTCTime
  , uploadFilename :: Maybe Text
  }

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

currentPartyId :: AuthedUser -> Int64
currentPartyId = fromSqlKey . auPartyId

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

conflict :: Text -> ServerError
conflict message = err409 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

createMusicUpload :: AuthedUser -> UUID -> UUID -> Maybe Text -> MusicUploadCreateRequest -> AppM Value
createMusicUpload user releaseId versionId idempotencyHeader request@MusicUploadCreateRequest{..} = do
  requireFeature "music_releases.processing"
  requireUploadPermission user releaseId
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  validateUploadRequest request
  existing <- uploadByIdempotency (currentPartyId user) idempotencyKey
  context <- case existing of
    Just prior -> do
      unless (uploadMatches prior request releaseId versionId) $
        throwError (conflict "Idempotency-Key is already bound to a different upload request")
      pure prior
    Nothing -> do
      ensureEditableVersion releaseId versionId
      validateRecordingBinding versionId musicUploadRecordingId musicUploadAssetRole
      enforceUploadQuota (currentPartyId user) musicUploadExpectedSize
      bucket <- requiredEnv "MUSIC_S3_QUARANTINE_BUCKET" 63
      sessionId <- liftIO nextRandom
      now <- liftIO getCurrentTime
      let objectKey = "quarantine/" <> T.take 2 (UUID.toText sessionId) <> "/" <> UUID.toText sessionId <> "/source"
          partSize :: Int64
          partSize = 16 * 1024 * 1024
      inserted <- runDB (rawSql
        "INSERT INTO music_upload_session(id,release_version_id,recording_id,asset_role,provider,bucket_name,quarantine_object_key,original_filename,expected_media_type,expected_size,expected_sha256,idempotency_key,part_size_bytes,expires_at,created_by) VALUES(?::uuid,?::uuid,?::uuid,?,'s3_compatible',?,?,?,?,?,?,?,?,?,?) RETURNING id"
        [ toPersistValue sessionId, toPersistValue versionId, optionalUuid musicUploadRecordingId
        , PersistText musicUploadAssetRole, PersistText bucket, PersistText objectKey
        , PersistText (T.strip musicUploadOriginalFilename), PersistText musicUploadExpectedMediaType, PersistInt64 musicUploadExpectedSize
        , PersistText (T.toLower musicUploadExpectedSha256), PersistText idempotencyKey
        , PersistInt64 partSize, PersistUTCTime (addUTCTime 3600 now)
        , PersistInt64 (currentPartyId user)
        ] :: SqlPersistT IO [Single UUID])
      when (null inserted) (throwError (conflict "Upload session could not be created"))
      runDB $ do
        rawExecute
          "UPDATE music_release_version SET state='uploading' WHERE id=?::uuid AND release_id=?::uuid AND state IN ('draft','validation_failed','changes_requested')"
          [toPersistValue versionId, toPersistValue releaseId]
        rawExecute
          "INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,idempotency_key,data) VALUES(?::uuid,?::uuid,?,'upload_created',?,jsonb_build_object('upload_session_id',?::text,'asset_role',?::text))"
          [toPersistValue releaseId, toPersistValue versionId, PersistInt64 (currentPartyId user), PersistText idempotencyKey, PersistText (UUID.toText sessionId), PersistText musicUploadAssetRole]
      loadUpload user sessionId
  uploadResponse context

bindMusicUploadProvider :: AuthedUser -> UUID -> MusicUploadProviderRequest -> AppM Value
bindMusicUploadProvider user sessionId MusicUploadProviderRequest{..} = do
  requireFeature "music_releases.processing"
  context <- loadUpload user sessionId
  ensureNotExpired context
  let providerId = T.strip musicUploadProviderUploadId
  signingConfig <- loadMusicS3SigningConfig
  now <- liftIO getCurrentTime
  case presignCompleteMultipart signingConfig now 60 (uploadBucket context) (uploadObjectKey context) providerId of
    Left _ -> throwError (badRequest "providerUploadId is invalid")
    Right _ -> pure ()
  case uploadProviderId context of
    Just existing | existing /= providerId -> throwError (conflict "Upload session is already bound to another provider upload ID")
    _ -> pure ()
  changed <- runDB (rawSql
    "UPDATE music_upload_session SET provider_upload_id=?,status='uploading' WHERE id=?::uuid AND created_by=? AND status IN ('initiated','uploading') AND (provider_upload_id IS NULL OR provider_upload_id=?) RETURNING id"
    [PersistText providerId, toPersistValue sessionId, PersistInt64 (currentPartyId user), PersistText providerId]
    :: SqlPersistT IO [Single UUID])
  when (null changed) (throwError (conflict "Upload session can no longer accept a provider ID"))
  uploadResponse context{ uploadProviderId=Just providerId, uploadStatus="uploading" }

signMusicUploadPart :: AuthedUser -> UUID -> Int -> AppM Value
signMusicUploadPart user sessionId partNumber = do
  requireFeature "music_releases.processing"
  context <- loadUpload user sessionId
  ensureUploading context
  signingConfig <- loadMusicS3SigningConfig
  now <- liftIO getCurrentTime
  providerId <- maybe (throwError (conflict "Bind the provider upload ID first")) pure (uploadProviderId context)
  url <- either (const (throwError (badRequest "Invalid multipart part request"))) pure
    (presignUploadPart signingConfig now 300 (uploadBucket context) (uploadObjectKey context) providerId partNumber)
  pure (object
    [ "uploadId" .= sessionId, "partNumber" .= partNumber, "url" .= url
    , "expiresAt" .= addUTCTime 300 now, "requiredHeaders" .= object []
    ])

recordMusicUploadPart :: AuthedUser -> UUID -> Int -> MusicUploadPartRequest -> AppM Value
recordMusicUploadPart user sessionId partNumber MusicUploadPartRequest{..} = do
  requireFeature "music_releases.processing"
  context <- loadUpload user sessionId
  ensureUploading context
  unless (partNumber >= 1 && partNumber <= 10000) (throwError (badRequest "partNumber must be between 1 and 10000"))
  unless (musicUploadPartByteSize > 0 && musicUploadPartByteSize <= fromIntegral (uploadPartSize context)) $
    throwError (badRequest "Part size is outside the session limit")
  let etag = T.strip musicUploadPartEtag
      sha256 = T.toLower (T.strip musicUploadPartSha256)
  unless (validEtag etag) (throwError (badRequest "etag is invalid"))
  unless (validSha256 sha256) (throwError (badRequest "part sha256 is invalid"))
  rows <- runDB (rawSql
    "INSERT INTO music_upload_part(upload_session_id,part_number,byte_size,etag,sha256) VALUES(?::uuid,?,?,?,?) ON CONFLICT(upload_session_id,part_number) DO UPDATE SET uploaded_at=music_upload_part.uploaded_at WHERE music_upload_part.byte_size=EXCLUDED.byte_size AND music_upload_part.etag=EXCLUDED.etag AND music_upload_part.sha256=EXCLUDED.sha256 RETURNING part_number,uploaded_at"
    [toPersistValue sessionId, PersistInt64 (fromIntegral partNumber), PersistInt64 musicUploadPartByteSize, PersistText etag, PersistText sha256]
    :: SqlPersistT IO [(Single Int, Single UTCTime)])
  case rows of
    [(Single recordedPart, Single uploadedAt)] -> pure (object ["uploadId" .= sessionId, "partNumber" .= recordedPart, "uploadedAt" .= uploadedAt])
    _ -> throwError (conflict "This part number was already recorded with different immutable evidence")

completeMusicUpload :: AuthedUser -> UUID -> AppM Value
completeMusicUpload user sessionId = do
  requireFeature "music_releases.processing"
  context <- loadUpload user sessionId
  ensureUploading context
  providerId <- maybe (throwError (conflict "Bind the provider upload ID first")) pure (uploadProviderId context)
  parts <- verifiedParts context
  signingConfig <- loadMusicS3SigningConfig
  now <- liftIO getCurrentTime
  url <- either (const (throwError err500 { errBody = "Could not sign multipart completion" })) pure
    (presignCompleteMultipart signingConfig now 300 (uploadBucket context) (uploadObjectKey context) providerId)
  let body = "<CompleteMultipartUpload>" <> T.concat
        [ "<Part><PartNumber>" <> T.pack (show number) <> "</PartNumber><ETag>" <> etag <> "</ETag></Part>"
        | (number, etag, _) <- parts
        ] <> "</CompleteMultipartUpload>"
  pure (object
    [ "uploadId" .= sessionId, "url" .= url, "method" .= ("POST" :: Text)
    , "contentType" .= ("application/xml" :: Text), "body" .= body
    , "expiresAt" .= addUTCTime 300 now
    ])

confirmMusicUpload :: AuthedUser -> UUID -> MusicUploadConfirmRequest -> AppM Value
confirmMusicUpload user sessionId MusicUploadConfirmRequest{..} = do
  requireFeature "music_releases.processing"
  context <- loadUpload user sessionId
  case uploadStatus context of
    "completed" -> completedUploadJson sessionId
    "uploading" -> do
      _ <- verifiedParts context
      let finalEtag = T.strip musicUploadConfirmEtag
      unless (validEtag finalEtag) (throwError (badRequest "Final object etag is invalid"))
      result <- runDB $ do
        assets <- rawSql
          "INSERT INTO music_asset(release_version_id,recording_id,asset_role,storage_provider,storage_class,bucket_name,object_key,original_filename,media_type,byte_size,sha256,etag,processing_state,provenance,immutable,created_by) SELECT session.release_version_id,session.recording_id,session.asset_role,'s3_compatible','quarantine',session.bucket_name,session.quarantine_object_key,session.original_filename,COALESCE(session.expected_media_type,'application/octet-stream'),session.expected_size,session.expected_sha256,?,'uploaded',jsonb_build_object('upload_session_id',session.id,'provider_upload_id',session.provider_upload_id),FALSE,session.created_by FROM music_upload_session session WHERE session.id=?::uuid AND session.created_by=? AND session.status='uploading' ON CONFLICT(release_version_id,asset_role,sha256) DO NOTHING RETURNING id,release_version_id,asset_role"
          [PersistText finalEtag, toPersistValue sessionId, PersistInt64 (currentPartyId user)]
          :: SqlPersistT IO [(Single UUID, Single UUID, Single Text)]
        selected <- case assets of
          [value] -> pure [value]
          [] -> rawSql
            "SELECT asset.id,asset.release_version_id,asset.asset_role FROM music_asset asset JOIN music_upload_session session ON session.release_version_id=asset.release_version_id AND session.asset_role=asset.asset_role AND session.expected_sha256=asset.sha256 WHERE session.id=?::uuid AND session.created_by=?"
            [toPersistValue sessionId, PersistInt64 (currentPartyId user)]
          _ -> pure []
        case selected of
          [(Single assetId, Single versionId, Single role)] -> do
            rawExecute
              "UPDATE music_upload_session SET status='completed',completed_asset_id=?::uuid,completed_at=NOW() WHERE id=?::uuid AND status='uploading'"
              [toPersistValue assetId, toPersistValue sessionId]
            let jobKind = if role == "master_audio" then "inspect_audio" else if role == "cover_original" then "inspect_artwork" else "validate_release"
            rawExecute
              "INSERT INTO music_processing_job(release_version_id,source_asset_id,job_kind,job_key) VALUES(?::uuid,?::uuid,?,?) ON CONFLICT(job_kind,job_key) DO NOTHING"
              [toPersistValue versionId, toPersistValue assetId, PersistText jobKind, PersistText (UUID.toText sessionId)]
            rawExecute
              "UPDATE music_release_version SET state='processing' WHERE id=?::uuid AND state IN ('draft','uploading','validation_failed','changes_requested')"
              [toPersistValue versionId]
            pure (Right assetId)
          _ -> pure (Left "Uploaded object could not be bound to one canonical asset")
      either (throwError . conflict) (const (completedUploadJson sessionId)) result
    _ -> throwError (conflict "Upload session cannot be confirmed in its current state")

cancelMusicUpload :: AuthedUser -> UUID -> AppM Value
cancelMusicUpload user sessionId = do
  requireFeature "music_releases.processing"
  context <- loadUpload user sessionId
  changed <- runDB (rawSql
    "UPDATE music_upload_session SET status='cancelled' WHERE id=?::uuid AND created_by=? AND status IN ('initiated','uploading','failed') RETURNING id"
    [toPersistValue sessionId, PersistInt64 (currentPartyId user)] :: SqlPersistT IO [Single UUID])
  when (null changed && uploadStatus context /= "cancelled") $
    throwError (conflict "Upload session can no longer be cancelled")
  abortUrl <- case uploadProviderId context of
    Nothing -> pure Nothing
    Just providerId -> do
      config <- loadMusicS3SigningConfig
      now <- liftIO getCurrentTime
      pure (either (const Nothing) Just (presignAbortMultipart config now 300 (uploadBucket context) (uploadObjectKey context) providerId))
  pure (object ["uploadId" .= sessionId, "status" .= ("cancelled" :: Text), "abortUrl" .= abortUrl])

uploadResponse :: UploadContext -> AppM Value
uploadResponse context = do
  createUrl <- case (uploadStatus context, uploadProviderId context) of
    ("initiated", Nothing) -> do
      config <- loadMusicS3SigningConfig
      now <- liftIO getCurrentTime
      either (const (throwError err500 { errBody = "Could not sign multipart creation" })) (pure . Just)
        (presignCreateMultipart config now 300 (uploadBucket context) (uploadObjectKey context))
    _ -> pure Nothing
  partRows <- runDB (rawSql
    "SELECT jsonb_build_object('partNumber',part_number,'byteSize',byte_size,'etag',etag,'sha256',sha256,'uploadedAt',uploaded_at) FROM music_upload_part WHERE upload_session_id=?::uuid ORDER BY part_number"
    [toPersistValue (uploadSessionId context)] :: SqlPersistT IO [Single CMS.AesonValue])
  let recordedParts = [CMS.unAesonValue value | Single value <- partRows]
  pure (object
    [ "id" .= uploadSessionId context, "releaseVersionId" .= uploadVersionId context
    , "recordingId" .= uploadRecordingId context, "assetRole" .= uploadAssetRole context
    , "status" .= uploadStatus context, "expectedSize" .= uploadExpectedSize context
    , "partSizeBytes" .= uploadPartSize context, "expiresAt" .= uploadExpiresAt context
    , "providerUploadIdBound" .= maybe False (const True) (uploadProviderId context)
    , "createMultipartUrl" .= createUrl
    , "parts" .= recordedParts
    ])

verifiedParts :: UploadContext -> AppM [(Int, Text, Int64)]
verifiedParts context = do
  rows <- runDB (rawSql
    "SELECT part_number,etag,byte_size FROM music_upload_part WHERE upload_session_id=?::uuid ORDER BY part_number"
    [toPersistValue (uploadSessionId context)] :: SqlPersistT IO [(Single Int, Single Text, Single Int64)])
  let parts = sortOn (\(number,_,_) -> number) [(number,etag,size) | (Single number,Single etag,Single size) <- rows]
      numbers = [number | (number,_,_) <- parts]
      sizes = [size | (_,_,size) <- parts]
      contiguous = numbers == [1 .. length numbers]
      nonFinalSizes = if null sizes then [] else init sizes
  unless (not (null parts) && contiguous && sum sizes == uploadExpectedSize context
    && all (== fromIntegral (uploadPartSize context)) nonFinalSizes) $
    throwError (conflict "Uploaded parts are incomplete, non-contiguous, or do not match the declared byte size")
  pure parts

completedUploadJson :: UUID -> AppM Value
completedUploadJson sessionId = do
  rows <- runDB (rawSql
    "SELECT jsonb_build_object('uploadId',session.id,'status',session.status,'assetId',asset.id,'processingState',asset.processing_state,'job',COALESCE((SELECT jsonb_build_object('id',job.id,'kind',job.job_kind,'status',job.status,'attemptCount',job.attempt_count,'errorCode',job.error_code,'errorSummary',job.error_summary) FROM music_processing_job job WHERE job.source_asset_id=asset.id ORDER BY job.created_at DESC LIMIT 1),'{}'::jsonb)) FROM music_upload_session session JOIN music_asset asset ON asset.id=session.completed_asset_id WHERE session.id=?::uuid"
    [toPersistValue sessionId] :: SqlPersistT IO [Single CMS.AesonValue])
  case rows of
    [Single value] -> pure (CMS.unAesonValue value)
    _ -> throwError err404

loadUpload :: AuthedUser -> UUID -> AppM UploadContext
loadUpload user sessionId = do
  rows <- runDB (rawSql
    "SELECT session.id,version.release_id,session.release_version_id,session.recording_id,session.asset_role,session.bucket_name,session.quarantine_object_key,session.provider_upload_id,session.expected_media_type,session.expected_size,session.expected_sha256,session.part_size_bytes,session.status,session.expires_at,session.original_filename FROM music_upload_session session JOIN music_release_version version ON version.id=session.release_version_id JOIN music_release release ON release.id=version.release_id WHERE session.id=?::uuid AND (session.created_by=? OR music_can(?,release.artist_party_id,'release.upload'))"
    [toPersistValue sessionId, PersistInt64 (currentPartyId user), PersistInt64 (currentPartyId user)]
    :: SqlPersistT IO [(Single UUID,Single UUID,Single UUID,Single (Maybe UUID),Single Text,Single Text,Single Text,Single (Maybe Text),Single (Maybe Text),Single Int64,Single Text,Single Int,Single Text,Single UTCTime,Single Text)])
  case rows of
    [(Single sid,Single releaseId,Single versionId,Single recordingId,Single role,Single bucket,Single key,Single providerId,Single mediaType,Single size,Single sha256,Single partSize,Single status,Single expiresAt,Single filename)] ->
      pure UploadContext
        { uploadSessionId=sid,uploadReleaseId=releaseId,uploadVersionId=versionId
        , uploadRecordingId=recordingId,uploadAssetRole=role,uploadBucket=bucket,uploadObjectKey=key
        , uploadProviderId=providerId,uploadExpectedMediaType=mediaType,uploadExpectedSize=size
        , uploadExpectedSha256=sha256,uploadPartSize=partSize,uploadStatus=status
        , uploadExpiresAt=expiresAt,uploadFilename=Just filename
        }
    _ -> throwError err404

uploadByIdempotency :: Int64 -> Text -> AppM (Maybe UploadContext)
uploadByIdempotency actor idempotencyKey = do
  ids <- runDB (rawSql
    "SELECT id FROM music_upload_session WHERE created_by=? AND idempotency_key=?"
    [PersistInt64 actor,PersistText idempotencyKey] :: SqlPersistT IO [Single UUID])
  case listToMaybe ids of
    Nothing -> pure Nothing
    Just (Single sessionId) -> do
      rows <- runDB (rawSql
        "SELECT session.id,version.release_id,session.release_version_id,session.recording_id,session.asset_role,session.bucket_name,session.quarantine_object_key,session.provider_upload_id,session.expected_media_type,session.expected_size,session.expected_sha256,session.part_size_bytes,session.status,session.expires_at,session.original_filename FROM music_upload_session session JOIN music_release_version version ON version.id=session.release_version_id WHERE session.id=?::uuid"
        [toPersistValue sessionId]
        :: SqlPersistT IO [(Single UUID,Single UUID,Single UUID,Single (Maybe UUID),Single Text,Single Text,Single Text,Single (Maybe Text),Single (Maybe Text),Single Int64,Single Text,Single Int,Single Text,Single UTCTime,Single Text)])
      case rows of
        [(Single sid,Single releaseId,Single versionId,Single recordingId,Single role,Single bucket,Single key,Single providerId,Single mediaType,Single size,Single sha256,Single partSize,Single status,Single expiresAt,Single filename)] ->
          pure (Just UploadContext{uploadSessionId=sid,uploadReleaseId=releaseId,uploadVersionId=versionId,uploadRecordingId=recordingId,uploadAssetRole=role,uploadBucket=bucket,uploadObjectKey=key,uploadProviderId=providerId,uploadExpectedMediaType=mediaType,uploadExpectedSize=size,uploadExpectedSha256=sha256,uploadPartSize=partSize,uploadStatus=status,uploadExpiresAt=expiresAt,uploadFilename=Just filename})
        _ -> throwError err500

uploadMatches :: UploadContext -> MusicUploadCreateRequest -> UUID -> UUID -> Bool
uploadMatches context MusicUploadCreateRequest{..} releaseId versionId =
  uploadReleaseId context == releaseId && uploadVersionId context == versionId
    && uploadRecordingId context == musicUploadRecordingId && uploadAssetRole context == musicUploadAssetRole
    && uploadExpectedMediaType context == Just musicUploadExpectedMediaType
    && uploadFilename context == Just (T.strip musicUploadOriginalFilename)
    && uploadExpectedSize context == musicUploadExpectedSize
    && uploadExpectedSha256 context == T.toLower musicUploadExpectedSha256

validateUploadRequest :: MusicUploadCreateRequest -> AppM ()
validateUploadRequest MusicUploadCreateRequest{..} = do
  unless (musicUploadAssetRole `elem` ["master_audio","cover_original","rights_evidence"]) $
    throwError (badRequest "assetRole must be master_audio, cover_original, or rights_evidence")
  let maximumSize = if musicUploadAssetRole == "master_audio" then 8 * 1024 * 1024 * 1024 else 100 * 1024 * 1024
  unless (musicUploadExpectedSize > 0 && musicUploadExpectedSize <= maximumSize) $
    throwError (badRequest "Upload size is outside the allowed range for this asset role")
  unless (validSha256 (T.toLower musicUploadExpectedSha256)) (throwError (badRequest "expectedSha256 must be 64 lowercase hexadecimal characters"))
  unless (not (T.null (T.strip musicUploadOriginalFilename)) && T.length musicUploadOriginalFilename <= 500) $
    throwError (badRequest "originalFilename is required and limited to 500 characters")
  unless (not (T.null (T.strip musicUploadExpectedMediaType)) && T.length musicUploadExpectedMediaType <= 200) $
    throwError (badRequest "expectedMediaType is required and limited to 200 characters")

validateRecordingBinding :: UUID -> Maybe UUID -> Text -> AppM ()
validateRecordingBinding versionId recordingId role
  | role == "master_audio" = case recordingId of
      Nothing -> throwError (badRequest "recordingId is required for a master audio upload")
      Just value -> do
        exists <- runDB (rawSql
          "SELECT EXISTS(SELECT 1 FROM music_release_track WHERE release_version_id=?::uuid AND recording_id=?::uuid)"
          [toPersistValue versionId,toPersistValue value] :: SqlPersistT IO [Single Bool])
        unless (exists == [Single True]) (throwError (badRequest "recordingId is not a track in this release version"))
  | role == "cover_original" && recordingId /= Nothing = throwError (badRequest "Cover uploads cannot be bound to one recording")
  | otherwise = pure ()

ensureEditableVersion :: UUID -> UUID -> AppM ()
ensureEditableVersion releaseId versionId = do
  rows <- runDB (rawSql
    "SELECT EXISTS(SELECT 1 FROM music_release_version WHERE id=?::uuid AND release_id=?::uuid AND state IN ('draft','uploading','processing','validation_failed','changes_requested'))"
    [toPersistValue versionId,toPersistValue releaseId] :: SqlPersistT IO [Single Bool])
  unless (rows == [Single True]) (throwError (conflict "Release version is not editable"))

requireUploadPermission :: AuthedUser -> UUID -> AppM ()
requireUploadPermission user releaseId
  | hasStrictAdminAccess user = pure ()
  | otherwise = do
      rows <- runDB (rawSql
        "SELECT EXISTS(SELECT 1 FROM music_release release WHERE release.id=?::uuid AND music_can(?,release.artist_party_id,'release.upload'))"
        [toPersistValue releaseId,PersistInt64 (currentPartyId user)] :: SqlPersistT IO [Single Bool])
      unless (rows == [Single True]) (throwError err403)

enforceUploadQuota :: Int64 -> Int64 -> AppM ()
enforceUploadQuota actor requestedSize = do
  rows <- runDB (rawSql
    "SELECT count(*),COALESCE(sum(expected_size),0) FROM music_upload_session WHERE created_by=? AND created_at>=NOW()-interval '24 hours' AND status IN ('initiated','uploading','completing','completed')"
    [PersistInt64 actor] :: SqlPersistT IO [(Single Int64,Single Int64)])
  case rows of
    [(Single count,Single bytes)]
      | count < 50 && bytes + requestedSize <= 20 * 1024 * 1024 * 1024 -> pure ()
      | otherwise -> throwError err429 { errBody = "Music upload quota exceeded; retry after older sessions expire" }
    _ -> throwError err500

ensureUploading :: UploadContext -> AppM ()
ensureUploading context = do
  ensureNotExpired context
  unless (uploadStatus context == "uploading") (throwError (conflict "Upload session is not accepting parts"))

ensureNotExpired :: UploadContext -> AppM ()
ensureNotExpired context = do
  now <- liftIO getCurrentTime
  unless (uploadExpiresAt context > now) (throwError err410 { errBody = "Upload session expired" })

requireFeature :: Text -> AppM ()
requireFeature flag = do
  raw <- liftIO (lookupEnv "APP_ENV")
  let environment = case fmap (T.toLower . T.strip . T.pack) raw of
        Just "production" -> "production"
        Just "prod" -> "production"
        _ -> "sandbox"
  rows <- runDB (rawSql "SELECT enabled FROM revenue_feature_flag WHERE flag_key=? AND environment=?"
    [PersistText flag,PersistText environment] :: SqlPersistT IO [Single Bool])
  unless (rows == [Single True]) $ throwError err503 { errBody = "Music processing is not enabled" }

requireIdempotencyKey :: Maybe Text -> AppM Text
requireIdempotencyKey supplied = do
  let value = T.strip (fromMaybe "" supplied)
  unless (T.length value >= 8 && T.length value <= 200 && T.all (\c -> c >= '!' && c <= '~') value) $
    throwError (badRequest "Idempotency-Key must contain 8-200 visible ASCII characters")
  pure value

requiredEnv :: String -> Int -> AppM Text
requiredEnv name maximumLength = do
  raw <- liftIO (lookupEnv name)
  case fmap (T.strip . T.pack) raw of
    Just value | not (T.null value) && T.length value <= maximumLength -> pure value
    _ -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack name <> " is required")) }

validSha256 :: Text -> Bool
validSha256 value = T.length value == 64 && T.all (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) value

validEtag :: Text -> Bool
validEtag value = not (T.null value) && T.length value <= 256
  && T.all (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || c `elem` ("\"-" :: String)) value

optionalUuid :: Maybe UUID -> PersistValue
optionalUuid = maybe PersistNull toPersistValue
