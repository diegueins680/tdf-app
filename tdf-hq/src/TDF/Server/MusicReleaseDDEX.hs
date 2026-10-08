{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.MusicReleaseDDEX
  ( listDdexParties
  , registerDdexParty
  , listDdexExports
  , createDdexExport
  , downloadDdexExport
  ) where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (addUTCTime, getCurrentTime)
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
import TDF.MusicRelease.Domain (IdentifierType(DPID), validateIdentifier)
import TDF.MusicRelease.DDEX.ERN432
  (parseErn432Credits, Ern432ExportError(..))
import TDF.MusicRelease.Storage.S3 (presignGetObject)
import TDF.Server.MusicReleaseAssets (loadMusicS3SigningConfig)

type AppM = ReaderT Env Handler

runDB :: SqlPersistT IO a -> AppM a
runDB action = asks envPool >>= liftIO . runSqlPool action

currentPartyId :: AuthedUser -> Int64
currentPartyId = fromSqlKey . auPartyId

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

requireDdexFeature :: AppM ()
requireDdexFeature = do
  raw <- liftIO (lookupEnv "APP_ENV")
  let environment = case fmap (map asciiLower) raw of
        Just "production" -> "production"
        Just "prod" -> "production"
        _ -> "sandbox"
  rows <- runDB (rawSql
    "SELECT enabled FROM revenue_feature_flag WHERE flag_key='music_releases.ddex_export' AND environment=?"
    [PersistText environment] :: SqlPersistT IO [Single Bool])
  unless (rows == [Single True]) $ throwError err503
    { errBody = "DDEX export is disabled until licensed schema/profile validation and real verified DPID configuration are complete" }

requireAdmin :: AuthedUser -> AppM ()
requireAdmin user = unless (hasStrictAdminAccess user) (throwError err403)

listDdexParties :: AuthedUser -> AppM [Value]
listDdexParties user = do
  requireAdmin user
  jsonRows
    "SELECT jsonb_build_object('id',id,'name',party_name,'dpid',dpid,'role',party_role,'verificationAuthority',verification_authority,'verifiedBy',verified_by,'verifiedAt',verified_at,'active',active,'createdAt',created_at) FROM music_ddex_party_registry ORDER BY active DESC,party_name,created_at"
    []

registerDdexParty :: AuthedUser -> MusicDdexPartyRequest -> AppM Value
registerDdexParty user MusicDdexPartyRequest{..} = do
  requireAdmin user
  let name = T.strip musicDdexPartyName
      dpid = T.strip musicDdexPartyDpid
      role = T.toLower (T.strip musicDdexPartyRole)
      authority = T.strip musicDdexVerificationAuthority
  unless (not (T.null name) && T.length name <= 500) (throwError (badRequest "name is required and limited to 500 characters"))
  unless (role `elem` ["sender","recipient","both"]) (throwError (badRequest "role must be sender, recipient, or both"))
  unless (not (T.null authority) && T.length authority <= 500) (throwError (badRequest "verificationAuthority is required"))
  unless (BL.length (encode musicDdexVerificationEvidence) <= 16384 && encode musicDdexVerificationEvidence /= "{}") $
    throwError (badRequest "verificationEvidence must be a non-empty JSON value of at most 16 KiB")
  case validateIdentifier DPID dpid of
    Left _ -> throwError (badRequest "dpid is not syntactically valid; TDF never invents a replacement")
    Right _ -> pure ()
  now <- liftIO getCurrentTime
  jsonOne err409
    "INSERT INTO music_ddex_party_registry(party_name,dpid,party_role,verification_authority,verification_evidence,verified_by,verified_at) VALUES(?,?,?,?,?::jsonb,?,?) RETURNING jsonb_build_object('id',id,'name',party_name,'dpid',dpid,'role',party_role,'verificationAuthority',verification_authority,'verifiedBy',verified_by,'verifiedAt',verified_at,'active',active,'createdAt',created_at)"
    [ PersistText name, PersistText dpid, PersistText role, PersistText authority
    , PersistText (TE.decodeUtf8 (BL.toStrict (encode musicDdexVerificationEvidence)))
    , PersistInt64 (currentPartyId user), PersistUTCTime now
    ]

listDdexExports :: AuthedUser -> UUID -> UUID -> AppM [Value]
listDdexExports user releaseId versionId = do
  requireReleaseDownload user releaseId
  jsonRows exportJsonSql [toPersistValue versionId,toPersistValue releaseId]

createDdexExport
  :: AuthedUser -> UUID -> UUID -> Maybe Text -> MusicDdexExportRequest -> AppM Value
createDdexExport user releaseId versionId idempotencyHeader MusicDdexExportRequest{..} = do
  requireDdexFeature
  requireAdmin user
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  let operation = T.toLower (T.strip musicDdexOperation)
  unless (operation `elem` ["new_release","update","takedown"]) $
    throwError (badRequest "operation must be new_release, update, or takedown")
  -- Invariants: key binds the whole request; export and job commit together.
  -- Locks end at commit/rollback; no rendering or network work inside this transaction.
  runDdexDB $ do
    _ <- rawSql
      "SELECT 1::bigint FROM (SELECT pg_advisory_xact_lock(hashtextextended(?,0))) locked"
      [PersistText ("music-ddex:" <> T.pack (show (currentPartyId user)) <> ":" <> idempotencyKey)]
      :: SqlPersistT IO [Single Int64]
    versions <- rawSql
      "SELECT id FROM music_release_version WHERE id=?::uuid AND release_id=?::uuid FOR UPDATE"
      [toPersistValue versionId,toPersistValue releaseId] :: SqlPersistT IO [Single UUID]
    unless (versions == [Single versionId]) (abortDdex err404)
    existing <- rawSql
      "SELECT id,release_version_id,operation,sender_registry_id,recipient_registry_id,status FROM music_ddex_export WHERE generated_by=? AND idempotency_key=?"
      [PersistInt64 (currentPartyId user),PersistText idempotencyKey]
      :: SqlPersistT IO [(Single UUID,Single UUID,Single Text,Single UUID,Single UUID,Single Text)]
    case existing of
      (Single exportId,Single originalVersion,Single originalOperation,
       Single originalSender,Single originalRecipient,Single status) : _ -> do
        unless (originalVersion == versionId && originalOperation == operation
          && originalSender == musicDdexSenderRegistryId
          && originalRecipient == musicDdexRecipientRegistryId) $
          abortDdex err409 {errBody="Idempotency-Key is already bound to another DDEX request"}
        -- Recover a legacy queued export stranded before its job insert. Never
        -- reset an existing attempt or enqueue a valid/failed historical package.
        when (status == "queued") (ensureDdexJob versionId exportId)
        loadDdexResponse releaseId versionId exportId
      [] -> do
        validationErrors <- jsonRowsDB
          "SELECT jsonb_build_object('fieldPath',field_path,'code',error_code,'message',message) FROM music_check_ddex_operation(?::uuid,?::uuid,?::uuid,?)"
          [toPersistValue versionId,toPersistValue musicDdexSenderRegistryId,
           toPersistValue musicDdexRecipientRegistryId,PersistText operation]
        unless (null validationErrors) $ abortDdex err422
          { errBody = encode (object ["message" .= ("DDEX export prerequisites are incomplete" :: Text), "errors" .= validationErrors])
          , errHeaders = [("Content-Type","application/json")]
          }
        snapshots <- jsonRowsDB
          "SELECT immutable_snapshot FROM music_release_version WHERE id=?::uuid"
          [toPersistValue versionId]
        snapshot <- maybe (abortDdex err404) pure (listToMaybe snapshots)
        case parseErn432Credits snapshot of
          Right _ -> pure ()
          Left errors -> abortDdex err422
            { errBody = encode (object
                [ "message" .= ("Los créditos aprobados no son exportables por este adaptador ERN." :: Text)
                , "errors" .= [object ["fieldPath" .= exportErrorField e, "code" .= exportErrorCode e,
                    "message" .= exportErrorMessage e] | e <- errors]
                ])
            , errHeaders = [("Content-Type","application/json")]
            }
        _ <- rawSql
          "SELECT id FROM music_ddex_party_registry WHERE id IN (?::uuid,?::uuid) ORDER BY id FOR SHARE"
          [toPersistValue musicDdexSenderRegistryId,toPersistValue musicDdexRecipientRegistryId]
          :: SqlPersistT IO [Single UUID]
        unless (musicDdexSenderRegistryId /= musicDdexRecipientRegistryId) $
          abortDdex (badRequest "Sender and recipient must be different registry entries")
        exportId <- liftIO nextRandom
        -- Version lock serializes the natural unique key, even for different
        -- actors/request keys. DO NOTHING also fails safely with old writers.
        inserted <- rawSql
          "WITH version AS (SELECT id,snapshot_sha256 FROM music_release_version WHERE id=?::uuid), sender AS (SELECT * FROM music_ddex_party_registry WHERE id=?::uuid AND active AND party_role IN ('sender','both')), recipient AS (SELECT * FROM music_ddex_party_registry WHERE id=?::uuid AND active AND party_role IN ('recipient','both')) INSERT INTO music_ddex_export(id,release_version_id,operation,standard,ern_version,release_profile,release_profile_version,business_profile_version,avs_version,structural_dictionary_version,choreography,choreography_version,sender_registry_id,recipient_registry_id,sender_dpid,recipient_dpid,message_id,status,validation_report,canonical_snapshot_sha256,idempotency_key,generated_by) SELECT ?::uuid,version.id,?,'ERN','4.3.2','Audio','2.3.1',NULL,'011','DD-ERN-432','Cloud Storage','1.8.1',sender.id,recipient.id,sender.dpid,recipient.dpid,?,'queued',jsonb_build_object('status','queued','checkedAt',NOW()),version.snapshot_sha256,?,? FROM version CROSS JOIN sender CROSS JOIN recipient ON CONFLICT DO NOTHING RETURNING id"
          [ toPersistValue versionId,toPersistValue musicDdexSenderRegistryId
          , toPersistValue musicDdexRecipientRegistryId,toPersistValue exportId,PersistText operation
          , PersistText ("TDF-" <> UUID.toText exportId),PersistText idempotencyKey,PersistInt64 (currentPartyId user)
          ] :: SqlPersistT IO [Single UUID]
        unless (inserted == [Single exportId]) $
          abortDdex err409 {errBody="DDEX export already exists or sender/recipient is inactive or incompatible; review existing exports and registry"}
        ensureDdexJob versionId exportId
        loadDdexResponse releaseId versionId exportId

-- An exception is necessary here: returning Left inside runSqlPool would commit
-- earlier writes. Convert controlled errors to HTTP only after rollback.
newtype DdexAbort = DdexAbort ServerError deriving Show
instance Exception DdexAbort

abortDdex :: ServerError -> SqlPersistT IO a
abortDdex = liftIO . throwIO . DdexAbort

runDdexDB :: SqlPersistT IO a -> AppM a
runDdexDB action = do
  pool <- asks envPool
  outcome <- liftIO (try (runSqlPool action pool))
  case outcome of
    Left (DdexAbort failure) -> throwError failure
    Right value -> pure value

ensureDdexJob :: UUID -> UUID -> SqlPersistT IO ()
ensureDdexJob versionId exportId = do
  rawExecute
    "INSERT INTO music_processing_job(release_version_id,job_kind,job_key,output) VALUES(?::uuid,'generate_ddex',?,jsonb_build_object('export_id',?::uuid)) ON CONFLICT(job_kind,job_key) DO NOTHING"
    [toPersistValue versionId,PersistText (UUID.toText exportId),toPersistValue exportId]
  matches <- rawSql
    "SELECT EXISTS(SELECT 1 FROM music_processing_job WHERE job_kind='generate_ddex' AND job_key=? AND release_version_id=?::uuid AND output->>'export_id'=?)"
    [PersistText (UUID.toText exportId),toPersistValue versionId,PersistText (UUID.toText exportId)]
    :: SqlPersistT IO [Single Bool]
  unless (matches == [Single True]) $
    abortDdex err409 {errBody="DDEX job linkage is inconsistent; request operator review"}

loadDdexResponse :: UUID -> UUID -> UUID -> SqlPersistT IO Value
loadDdexResponse releaseId versionId exportId = do
  rows <- jsonRowsDB (exportJsonSql <> " AND export.id=?::uuid")
    [toPersistValue versionId,toPersistValue releaseId,toPersistValue exportId]
  maybe (abortDdex err404) pure (listToMaybe rows)

downloadDdexExport :: AuthedUser -> UUID -> AppM Value
downloadDdexExport user exportId = do
  rows <- runDB (rawSql
    "SELECT release.id,release.artist_party_id,asset.bucket_name,asset.object_key,asset.media_type,asset.byte_size,asset.sha256 FROM music_ddex_export export JOIN music_release_version version ON version.id=export.release_version_id JOIN music_release release ON release.id=version.release_id JOIN music_asset asset ON asset.id=export.package_asset_id WHERE export.id=?::uuid AND export.status='valid' AND asset.asset_role='ddex_package' AND asset.processing_state='ready'"
    [toPersistValue exportId]
    :: SqlPersistT IO [(Single UUID,Single Int64,Single Text,Single Text,Single Text,Single Int64,Single Text)])
  (releaseId,artistId,bucket,key,mediaType,byteSize,sha256) <- case rows of
    [(Single rid,Single aid,Single bucket,Single key,Single mediaType,Single byteSize,Single sha256)] -> pure (rid,aid,bucket,key,mediaType,byteSize,sha256)
    _ -> throwError err404
  unless (hasStrictAdminAccess user) $ do
    allowed <- booleanQuery "SELECT music_can(?,?,'release.downloads')"
      [PersistInt64 (currentPartyId user),PersistInt64 artistId]
    unless allowed (throwError err403)
  config <- loadMusicS3SigningConfig
  now <- liftIO getCurrentTime
  url <- either (const (throwError err500 { errBody = "DDEX package signing failed" })) pure
    (presignGetObject config now 300 bucket key)
  runDB $ rawExecute
    "INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,data) SELECT ?,export.release_version_id,?,'ddex_package_downloaded',jsonb_build_object('export_id',export.id) FROM music_ddex_export export WHERE export.id=?::uuid"
    [toPersistValue releaseId,PersistInt64 (currentPartyId user),toPersistValue exportId]
  pure (object ["url" .= url,"expiresAt" .= addUTCTime 300 now,"mediaType" .= mediaType,"byteSize" .= byteSize,"sha256" .= sha256])

requireReleaseDownload :: AuthedUser -> UUID -> AppM ()
requireReleaseDownload user releaseId
  | hasStrictAdminAccess user = pure ()
  | otherwise = do
      rows <- runDB (rawSql
        "SELECT music_can(?,artist_party_id,'release.downloads') FROM music_release WHERE id=?::uuid"
        [PersistInt64 (currentPartyId user),toPersistValue releaseId] :: SqlPersistT IO [Single Bool])
      case rows of [Single True] -> pure (); [Single False] -> throwError err403; _ -> throwError err404

requireIdempotencyKey :: Maybe Text -> AppM Text
requireIdempotencyKey supplied = do
  let value = T.strip (fromMaybe "" supplied)
  unless (T.length value >= 8 && T.length value <= 200 && T.all (\c -> c >= '!' && c <= '~') value) $
    throwError (badRequest "Idempotency-Key must contain 8-200 visible ASCII characters")
  pure value

exportJsonSql :: Text
exportJsonSql =
  "SELECT jsonb_build_object('id',export.id,'releaseVersionId',export.release_version_id,'operation',export.operation,'ernVersion',export.ern_version,'releaseProfile',export.release_profile,'releaseProfileVersion',export.release_profile_version,'businessProfileVersion',export.business_profile_version,'avsVersion',export.avs_version,'structuralDictionaryVersion',export.structural_dictionary_version,'choreographyVersion',export.choreography_version,'senderDpid',export.sender_dpid,'recipientDpid',export.recipient_dpid,'messageId',export.message_id,'status',export.status,'validationReport',export.validation_report,'packageSha256',export.package_sha256,'generatedAt',export.generated_at,'createdAt',export.created_at) FROM music_ddex_export export JOIN music_release_version version ON version.id=export.release_version_id WHERE export.release_version_id=?::uuid AND version.release_id=?::uuid"

jsonRows :: Text -> [PersistValue] -> AppM [Value]
jsonRows statement params = runDB (jsonRowsDB statement params)

jsonRowsDB :: Text -> [PersistValue] -> SqlPersistT IO [Value]
jsonRowsDB statement params = do
  rows <- rawSql statement params :: SqlPersistT IO [Single CMS.AesonValue]
  pure [CMS.unAesonValue value | Single value <- rows]

jsonOne :: ServerError -> Text -> [PersistValue] -> AppM Value
jsonOne missing statement params = jsonRows statement params >>= maybe (throwError missing) pure . listToMaybe

booleanQuery :: Text -> [PersistValue] -> AppM Bool
booleanQuery statement params = do
  rows <- runDB (rawSql statement params :: SqlPersistT IO [Single Bool])
  pure (rows == [Single True])

asciiLower :: Char -> Char
asciiLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
asciiLower c = c
