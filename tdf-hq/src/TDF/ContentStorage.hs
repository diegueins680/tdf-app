{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TDF.ContentStorage
  ( contentStorageServer, storeManagedUpload, storageError, managedImageExtension
  , bindExternalDriveAttempt, requireUploadsEnabled, bindReference
  , storageFileIdFromUrl, publicObjectAllowed, runReferenceMutation, requireReference
  , publicRootSafe, publicRequestSafe ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Control.Exception (fromException)
import qualified Control.Exception.Safe as Safe
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value(..), encode, decodeStrict', object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Servant
import Servant.Multipart (FileData(..), Tmp)
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, doesFileExist, getFileSize, listDirectory, pathIsSymbolicLink, renameFile, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>), addTrailingPathSeparator)
import System.Posix.Files (setFileMode)

import TDF.API.ContentStorage
import TDF.API.Types (DriveUploadDTO(..))
import TDF.Auth (AuthedUser(..), ModuleAccess(..), withCurrentAuthSession, withCurrentModuleAccess)
import TDF.Config (assetsRootDir, resolveConfiguredAssetsBase)
import TDF.DB (Env(..))

type AppM = ReaderT Env Handler

runDB :: SqlPersistT IO a -> AppM a
runDB action = asks envPool >>= liftIO . runSqlPool action

runSessionDB :: AuthedUser -> SqlPersistT IO a -> AppM a
runSessionDB user action = do
  result <- runDB (withCurrentAuthSession user action)
  maybe (throwError err401) pure result

storageError :: ServerError -> Text -> ServerError
storageError base code = base
  { errBody = encode (object ["code" .= code])
  , errHeaders = [("Content-Type", "application/json; charset=utf-8")]
  }

-- No deployment or generated client enables account uploads implicitly.
requireUploadsEnabled :: AppM ()
requireUploadsEnabled = do
  enabled <- liftIO (lookupEnv "CONTENT_STORAGE_UPLOADS_ENABLED")
  unless (enabled == Just "true") $
    throwError (storageError err503 "storage_activation_pending")

storageFileIdFromUrl :: Text -> Text -> Either ServerError (Maybe UUID)
storageFileIdFromUrl apiBase url
  | Just identifier <- T.stripPrefix "/storage/files/" url = parse identifier
  | Just identifier <- T.stripPrefix (T.dropWhileEnd (== '/') apiBase <> "/storage/files/") url = parse identifier
  | "/storage/files/" `T.isInfixOf` url = Left err400
  | otherwise = Right Nothing
  where
    parse raw = case UUID.fromText raw of
      Just identifier | UUID.toText identifier == raw -> Right (Just identifier)
      _ -> Left err400

-- Must run in the SAME transaction as the domain write. An existing binding
-- may be retained by another authorized editor; a new binding requires the
-- authenticated uploader. A caller-supplied URL can never create a read grant.
bindReference :: AuthedUser -> Text -> Text -> Int64 -> Maybe Text -> SqlPersistT IO (Either ServerError ())
bindReference user apiBase kind resource maybeUrl = case traverse (storageFileIdFromUrl apiBase) maybeUrl of
  Left err -> pure (Left err)
  Right parsed -> do
    case parsed >>= id of
      Nothing -> do
        rawExecute "DELETE FROM content_storage_reference WHERE resource_kind=? AND resource_id=?"
          [PersistText kind,PersistInt64 resource]
        pure (Right ())
      Just identifier -> do
        rows <- rawSql
          "SELECT id FROM content_storage_upload file WHERE id=?::uuid AND destination='tdf' AND status='completed' AND visibility='private' AND (owner_party_id=? OR EXISTS(SELECT 1 FROM content_storage_reference ref WHERE ref.file_id=file.id AND ref.resource_kind=? AND ref.resource_id=?)) FOR SHARE"
          [PersistText (UUID.toText identifier),PersistInt64 (fromSqlKey (auPartyId user)),PersistText kind,PersistInt64 resource]
          :: SqlPersistT IO [Single UUID]
        if null rows then pure (Left err404) else do
          rawExecute "INSERT INTO content_storage_reference(resource_kind,resource_id,file_id,bound_by) VALUES(?,?,?::uuid,?) ON CONFLICT(resource_kind,resource_id) DO UPDATE SET file_id=EXCLUDED.file_id,bound_by=EXCLUDED.bound_by"
            [PersistText kind,PersistInt64 resource,PersistText (UUID.toText identifier),PersistInt64 (fromSqlKey (auPartyId user))]
          pure (Right ())

requireReference :: AuthedUser -> Text -> Text -> Int64 -> Maybe Text -> SqlPersistT IO ()
requireReference user apiBase kind resource url = do
  outcome <- bindReference user apiBase kind resource url
  either (liftIO . Safe.throwIO) pure outcome

runReferenceMutation :: AuthedUser -> SqlPersistT IO a -> AppM a
runReferenceMutation user action = do
  pool <- asks envPool
  result <- liftIO $ Safe.tryAny $ runSqlPool (withCurrentModuleAccess ModuleAdmin user action) pool
  case result of
    Right accepted -> either throwError pure accepted
    Left failure -> case fromException failure of
      Just err -> throwError (err :: ServerError)
      Nothing -> throwError (storageError err503 "storage_reference_unavailable")

liveReferenceSql :: Text
liveReferenceSql = "EXISTS(SELECT 1 FROM content_storage_reference ref WHERE ref.file_id=file.id AND ((ref.resource_kind='course_receipt' AND EXISTS(SELECT 1 FROM course_registration_receipt receipt WHERE receipt.id=ref.resource_id)) OR (ref.resource_kind='course_follow_up' AND EXISTS(SELECT 1 FROM course_registration_follow_up follow_up WHERE follow_up.id=ref.resource_id)) OR (ref.resource_kind='payment' AND EXISTS(SELECT 1 FROM payment WHERE payment.id=ref.resource_id))))"

-- Static serving must consult the committed public manifest on EVERY read.
-- Unknown, private, cancelled and uncommitted objects fail closed, including a
-- process failure after rename but before the database transaction commits.
publicRootSafe :: FilePath -> IO Bool
publicRootSafe publicRoot = do
  result <- Safe.tryAny $ do
    private <- canonicalizePath privateRoot
    public <- canonicalizePath publicRoot
    pure (private /= public && not (addTrailingPathSeparator public `isPrefixOf` addTrailingPathSeparator private)
      && not (addTrailingPathSeparator private `isPrefixOf` addTrailingPathSeparator public))
  pure (either (const False) id result)

publicRequestSafe :: FilePath -> [Text] -> IO Bool
publicRequestSafe publicRoot segments = do
  rootsSafe <- publicRootSafe publicRoot
  if not rootsSafe then pure False else do
    result <- Safe.tryAny $ do
      root <- canonicalizePath publicRoot
      target <- canonicalizePath (foldl (</>) publicRoot (map T.unpack segments))
      -- A symlink inside the static mount may not escape its canonical root.
      pure (target == root || addTrailingPathSeparator root `isPrefixOf` addTrailingPathSeparator target)
    pure (either (const False) id result)

publicObjectAllowed :: Env -> [Text] -> IO Bool
publicObjectAllowed env ["content",name] = do
  let raw = T.take 36 name
  case UUID.fromText raw of
    Nothing -> pure False
    Just identifier -> do
      outcome <- Safe.tryAny $ runSqlPool (do
        rows <- rawSql "SELECT media_type FROM content_storage_upload WHERE id=?::uuid AND destination='tdf' AND status='completed' AND visibility='public'"
          [PersistText (UUID.toText identifier)] :: SqlPersistT IO [Single Text]
        pure (case rows of [Single mime] -> name == raw <> fromMaybe "" (publicMediaExtension mime); _ -> False)) (envPool env)
      pure (either (const False) id outcome)
publicObjectAllowed _ ("content":_) = pure False
publicObjectAllowed _ _ = pure True

bindExternalDriveAttempt :: AuthedUser -> Text -> Text -> Text -> Text -> AppM Text
bindExternalDriveAttempt user key fingerprint accountId proposedFileId = do
  let owner = fromSqlKey (auPartyId user)
  rows <- runSessionDB user $ do
    rawExecute
      "INSERT INTO content_storage_drive_attempt(owner_party_id,request_key,fingerprint,provider_account_id,provider_file_id) VALUES(?,?,?,?,?) ON CONFLICT(owner_party_id,request_key) DO NOTHING"
      [PersistInt64 owner, PersistText key, PersistText fingerprint, PersistText accountId, PersistText proposedFileId]
    rawSql
      "SELECT provider_file_id FROM content_storage_drive_attempt WHERE owner_party_id=? AND request_key=? AND fingerprint=? AND provider_account_id=?"
      [PersistInt64 owner, PersistText key, PersistText fingerprint, PersistText accountId]
      :: SqlPersistT IO [Single Text]
  case rows of
    [Single fileId] -> pure fileId
    _ -> throwError (storageError err409 "storage_upload_conflict")

contentStorageServer :: AuthedUser -> ServerT ContentStorageAPI AppM
contentStorageServer user = account :<|> setDestination :<|> listFiles :<|> readPrivate :<|> deleteFile
  where
    owner = fromSqlKey (auPartyId user)
    -- Reading the account never creates it: identity merges are blocked by any
    -- row referencing a party, so accounts exist only once storage is used.
    account = runSessionDB user $ do
      rows <- rawSql
        "SELECT GREATEST(COALESCE(account.allowance_bytes,0),5000000000),COALESCE(account.legacy_bytes,0)+COALESCE(sum(upload.byte_size) FILTER(WHERE upload.destination='tdf' AND upload.status='completed'),0)::bigint,COALESCE(sum(upload.byte_size) FILTER(WHERE upload.destination='tdf' AND (upload.status='uploading' OR (upload.status='cancelled' AND upload.deleted_at IS NULL))),0)::bigint,account.default_destination,COALESCE(account.revision,0),COALESCE(account.legacy_usage_verified,content_storage_new_party(owner.party_id)) FROM (SELECT ?::bigint AS party_id) owner LEFT JOIN content_storage_account account ON account.party_id=owner.party_id LEFT JOIN content_storage_upload upload ON upload.owner_party_id=owner.party_id GROUP BY owner.party_id,account.party_id"
        [PersistInt64 owner]
      case rows of
        [(Single capacity, Single used, Single reserved, Single preferred, Single version, Single verified)] ->
          pure (StorageAccountDTO capacity used reserved preferred version verified)
        _ -> fail "Storage account is unavailable"
    setDestination StorageDestinationUpdate{..} = do
      unless (destination `elem` ["tdf", "google_drive"] && expectedRevision >= 0) $
        throwError (storageError err400 "storage_destination_invalid")
      rows <- runSessionDB user $ do
        -- Choosing a destination uses storage, so the account is created here.
        _ <- rawSql "SELECT 1::bigint FROM (SELECT content_storage_ensure_account(?)) ensured" [PersistInt64 owner]
          :: SqlPersistT IO [Single Int64]
        rawSql
          "UPDATE content_storage_account SET default_destination=?,revision=revision+1 WHERE party_id=? AND revision=? RETURNING party_id"
          [PersistText destination, PersistInt64 owner, PersistInt64 expectedRevision]
          :: SqlPersistT IO [Single Int64]
      when (null rows) (throwError (storageError err409 "storage_preference_conflict"))
      account
    listFiles after requestedLimit = do
      let limit = fromMaybe 20 requestedLimit
          cursor = maybe PersistNull (PersistText . UUID.toText) after
      unless (limit >= 1 && limit <= 100) (throwError err400)
      rows <- runSessionDB user (rawSql
        "SELECT id,file_name,byte_size,status,visibility FROM content_storage_upload WHERE owner_party_id=? AND destination='tdf' AND (status<>'cancelled' OR deleted_at IS NULL) AND (?::uuid IS NULL OR id>?::uuid) ORDER BY id LIMIT ?"
        [PersistInt64 owner,cursor,cursor,PersistInt64 (fromIntegral limit)]
        :: SqlPersistT IO [(Single UUID,Single Text,Single Int64,Single Text,Single Text)])
      pure [StorageFileDTO fileId name bytes state sharing | (Single fileId,Single name,Single bytes,Single state,Single sharing) <- rows]
    readPrivate fileId = do
      cfg <- asks envConfig
      let readAuthorized delegated = do
            rows <- rawSql
              ("SELECT visibility,media_type,sha256,byte_size FROM content_storage_upload file WHERE id=?::uuid AND status='completed' AND destination='tdf' AND (owner_party_id=? OR (?::boolean AND " <> liveReferenceSql <> ")) FOR SHARE")
              [PersistText (UUID.toText fileId),PersistInt64 owner,PersistBool delegated]
              :: SqlPersistT IO [(Single Text,Single Text,Single Text,Single Int64)]
            case rows of
              [(Single sharing,Single mime,Single expectedHash,Single expectedSize)] -> do
                outcome <- liftIO $ Safe.tryAny $ do
                  validateRoots (assetsRootDir cfg)
                  let path = storedFilePath (assetsRootDir cfg) (UUID.toText fileId) sharing mime
                  symbolic <- pathIsSymbolicLink path
                  when symbolic (Safe.throwIO (userError "Invalid stored file"))
                  -- Force the complete read and integrity check while session,
                  -- permission and object locks are still held.
                  bytes <- BS.readFile path
                  unless (fromIntegral (BS.length bytes) == expectedSize && digest (BL.fromStrict bytes) == expectedHash)
                    (Safe.throwIO (userError "Stored file integrity failed"))
                  pure (BL.fromStrict bytes)
                pure (either (const (Left (storageError err503 "storage_integrity_failed"))) Right outcome)
              _ -> pure (Left err404)
      result <- runDB $ withCurrentAuthSession user $ do
        own <- readAuthorized False
        case own of
          Left err | errHTTPCode err == 404 -> do
            delegated <- withCurrentModuleAccess ModuleAdmin user (readAuthorized True)
            pure (either (const (Left err404)) id delegated)
          _ -> pure own
      bytes <- either throwError pure (fromMaybe (Left err401) result)
      pure (addHeader ("private, no-store" :: Text) (addHeader ("attachment; filename=\"tdf-file\"" :: Text) (addHeader ("nosniff" :: Text) bytes)))
    deleteFile fileId = do
      let identifier = UUID.toText fileId
      admitted <- runDB $ withCurrentAuthSession user $ do
        found <- rawSql
          "SELECT visibility,media_type,(status='uploading' AND lease_until>clock_timestamp()) FROM content_storage_upload WHERE id=?::uuid AND owner_party_id=? AND destination='tdf' FOR UPDATE"
          [PersistText identifier,PersistInt64 owner]
          :: SqlPersistT IO [(Single Text,Single Text,Single Bool)]
        case found of
          [(Single sharing,Single mime,Single False)] -> do
            references <- rawSql
              ("SELECT 1::bigint FROM content_storage_upload file WHERE file.id=?::uuid AND " <> liveReferenceSql)
              [PersistText identifier] :: SqlPersistT IO [Single Int64]
            if not (null references) then pure (Left (storageError err409 "storage_file_in_use")) else do
              -- Persist intent before bytes; an uncertain deletion retains quota.
              rawExecute "UPDATE content_storage_upload SET status='cancelled',original_completed_at=COALESCE(original_completed_at,completed_at),completed_at=NULL WHERE id=?::uuid AND owner_party_id=?"
                [PersistText identifier,PersistInt64 owner]
              pure (Right (sharing,mime))
          [(Single _,Single _,Single True)] -> pure (Left (storageError err409 "storage_upload_in_progress"))
          _ -> pure (Left err404)
      (sharing,mime) <- either throwError pure (fromMaybe (Left err401) admitted)
      cfg <- asks envConfig
      let target = storedFilePath (assetsRootDir cfg) identifier sharing mime
      outcome <- liftIO $ Safe.tryAny $ do
        validateRoots (assetsRootDir cfg)
        exists <- doesFileExist target
        when exists (removeFile target)
        -- Expired/failed staging bytes count until this cleanup succeeds.
        forM_ [privateRoot,assetsRootDir cfg </> ".content-storage-staging"] $ \root -> do
          createDirectoryIfMissing True root
          names <- listDirectory root
          forM_ names $ \name -> when ((T.unpack identifier <> "-") `isPrefixOf` name && ".pending" `T.isSuffixOf` T.pack name) $
            removeFile (root </> name)
      case outcome of
        Left _ -> throwError (storageError err503 "storage_delete_pending")
        Right _ -> do
          runDB (rawExecute "UPDATE content_storage_upload SET deleted_at=COALESCE(deleted_at,clock_timestamp()) WHERE id=?::uuid AND owner_party_id=? AND status='cancelled'"
            [PersistText identifier,PersistInt64 owner])
          pure NoContent

privateRoot :: FilePath
privateRoot = "uploads/content-storage"

validateRoots :: FilePath -> IO ()
validateRoots publicRoot = do
  createDirectoryIfMissing True privateRoot
  private <- canonicalizePath privateRoot
  public <- canonicalizePath publicRoot
  when (private == public || addTrailingPathSeparator public `isPrefixOf` addTrailingPathSeparator private
        || addTrailingPathSeparator private `isPrefixOf` addTrailingPathSeparator public)
    (Safe.throwIO (userError "Private and public storage must be disjoint"))
  setFileMode privateRoot 0o700

storedFilePath :: FilePath -> Text -> Text -> Text -> FilePath
storedFilePath publicRoot identifier sharing mime
  | sharing /= "public" = privateRoot </> T.unpack identifier
  | otherwise = publicRoot </> T.unpack ("content/" <> identifier <> extension)
  where
    extension = fromMaybe "" (publicMediaExtension mime)

digest :: BL.ByteString -> Text
digest bytes = T.pack (show (hash (BL.toStrict bytes) :: Digest SHA256))

-- Only passive raster formats can enter the public assets mount. MIME alone
-- does not establish the file's format; executable/document payloads stay private.
managedImageExtension :: Text -> BS.ByteString -> Maybe Text
managedImageExtension mime bytes
  | mime == "image/png", BS.pack [137,80,78,71,13,10,26,10] `BS.isPrefixOf` bytes = Just ".png"
  | mime == "image/jpeg", BS.pack [255,216,255] `BS.isPrefixOf` bytes = Just ".jpg"
  | mime == "image/gif", any (`BS.isPrefixOf` bytes) ["GIF87a","GIF89a"] = Just ".gif"
  | mime == "image/webp", "RIFF" `BS.isPrefixOf` bytes, BS.take 4 (BS.drop 8 bytes) == "WEBP" = Just ".webp"
  | otherwise = Nothing

publicMediaExtension :: Text -> Maybe Text
publicMediaExtension mime = case mime of
  "image/png" -> Just ".png"; "image/jpeg" -> Just ".jpg"
  "image/gif" -> Just ".gif"; "image/webp" -> Just ".webp"
  "audio/mpeg" -> Just ".mp3"; "audio/mp3" -> Just ".mp3"
  "audio/wav" -> Just ".wav"; "audio/x-wav" -> Just ".wav"
  "audio/flac" -> Just ".flac"; "audio/ogg" -> Just ".ogg"
  "audio/mp4" -> Just ".m4a"; "audio/x-m4a" -> Just ".m4a"
  "video/mp4" -> Just ".mp4"; "video/webm" -> Just ".webm"
  _ -> Nothing

managedPublicExtension :: Text -> BS.ByteString -> Maybe Text
managedPublicExtension mime bytes = case managedImageExtension mime bytes of
  Just extension -> Just extension
  Nothing | validPassiveHeader -> publicMediaExtension mime
          | otherwise -> Nothing
  where
    validPassiveHeader
      | mime `elem` ["audio/mpeg","audio/mp3"] = "ID3" `BS.isPrefixOf` bytes || (BS.length bytes>=2 && BS.index bytes 0==255 && BS.index bytes 1>=224)
      | mime `elem` ["audio/wav","audio/x-wav"] = "RIFF" `BS.isPrefixOf` bytes && BS.take 4 (BS.drop 8 bytes)=="WAVE"
      | mime == "audio/flac" = "fLaC" `BS.isPrefixOf` bytes
      | mime == "audio/ogg" = "OggS" `BS.isPrefixOf` bytes
      | mime `elem` ["audio/mp4","audio/x-m4a","video/mp4"] = BS.take 4 (BS.drop 4 bytes)=="ftyp"
      | mime == "video/webm" = BS.pack [26,69,223,163] `BS.isPrefixOf` bytes
      | otherwise = False

storeManagedUpload
  :: AuthedUser -> FileData Tmp -> Text -> Text -> Text -> Text -> AppM DriveUploadDTO
storeManagedUpload user file name mime visibility requestKey = do
  requireUploadsEnabled
  unless (visibility `elem` ["private", "public"]) $
    throwError (storageError err400 "storage_visibility_invalid")
  unless (T.length requestKey == 64 && T.all (`elem` ("0123456789abcdef" :: String)) requestKey) $
    throwError (storageError err400 "storage_idempotency_required")
  size <- liftIO (getFileSize (fdPayload file))
  when (size <= 0 || size > 50 * 1024 * 1024) $
    throwError (storageError err413 "storage_file_too_large")
  bytes <- liftIO (BL.readFile (fdPayload file))
  let extension = managedPublicExtension mime (BL.toStrict (BL.take 16 bytes))
  when (visibility == "public" && extension == Nothing) $
    throwError (storageError err400 "storage_public_media_invalid")
  cfg <- asks envConfig
  freshId <- liftIO nextRandom
  leaseId <- liftIO nextRandom
  let owner = fromSqlKey (auPartyId user)
      contentHash = digest bytes
      fingerprint = digest (encode (owner,name,mime,visibility,contentHash))
  reservation <- runSessionDB user (rawSql
    "SELECT content_storage_reserve(?,?,?,?,?,?,?,?,?,?::uuid,?::uuid)::text"
    [PersistInt64 owner, PersistText requestKey, PersistText fingerprint, PersistInt64 (fromInteger size)
    ,PersistText "tdf", PersistText visibility, PersistText name, PersistText mime, PersistText contentHash
    ,PersistText (UUID.toText freshId), PersistText (UUID.toText leaseId)] :: SqlPersistT IO [Single Text])
  value <- case reservation of
    [Single raw] -> maybe (throwError err500) pure (decodeStrict' (TE.encodeUtf8 raw))
    _ -> throwError err500
  (fileId, state) <- parseReservation value
  let identifier = UUID.toText fileId
      relative = "content/" <> identifier <> maybe "" id extension
      target = if visibility == "public" then assetsRootDir cfg </> T.unpack relative
               else privateRoot </> T.unpack identifier
      publicUrl = T.dropWhileEnd (== '/') (resolveConfiguredAssetsBase cfg) <> "/" <> relative
      privateUrl = "/storage/files/" <> identifier
      result = DriveUploadDTO identifier Nothing (Just (if visibility == "public" then publicUrl else privateUrl))
                 (if visibility == "public" then Just publicUrl else Nothing)
  if state == "completed" then do
    exists <- liftIO (doesFileExist target)
    unless exists (throwError (storageError err503 "storage_file_unavailable"))
    existingHash <- liftIO (digest <$> BL.readFile target)
    unless (existingHash == contentHash) (throwError (storageError err503 "storage_integrity_failed"))
    pure result
  else do
    let stagingRoot = if visibility == "public" then assetsRootDir cfg </> ".content-storage-staging" else privateRoot
        staged = stagingRoot </> T.unpack (identifier <> "-" <> UUID.toText leaseId <> ".pending")
    pool <- asks envPool
    committed <- liftIO $ Safe.tryAny $ runSqlPool (do
      authorized <- withCurrentAuthSession user $ do
        rows <- rawSql
          "SELECT id::text FROM content_storage_upload WHERE id=?::uuid AND owner_party_id=? AND lease_id=?::uuid AND status='uploading' AND lease_until>clock_timestamp() FOR UPDATE"
          [PersistText identifier,PersistInt64 owner,PersistText (UUID.toText leaseId)] :: SqlPersistT IO [Single Text]
        if null rows then pure False else do
          liftIO $ do
            validateRoots (assetsRootDir cfg)
            createDirectoryIfMissing True stagingRoot
            setFileMode stagingRoot 0o700
            copyFile (fdPayload file) staged `Safe.onException` removeStaged staged
            setFileMode staged 0o600
            when (visibility == "public") (createDirectoryIfMissing True (assetsRootDir cfg </> "content"))
            renameFile staged target
          rawExecute "UPDATE content_storage_upload SET status='completed',completed_at=clock_timestamp() WHERE id=?::uuid"
            [PersistText identifier]
          pure True
      pure (authorized == Just True)) pool
    case committed of
      Right True -> pure result
      _ -> do
        liftIO (removeStaged staged)
        throwError (storageError err409 "storage_upload_in_progress")

-- Cleanup failure keeps a reservation pending for operator reconciliation.
-- Never mask the original copy/commit failure with a cleanup exception.
removeStaged :: FilePath -> IO ()
removeStaged staged = do
  _ <- Safe.tryAny $ do
    exists <- doesFileExist staged
    when exists (removeFile staged)
  pure ()

parseReservation :: Value -> AppM (UUID, Text)
parseReservation (Object value) = case KM.lookup "code" value of
  Just (String code) -> throwError (storageError (if code == "storage_quota_exceeded" then err413 else err409) code)
  _ -> case (KM.lookup "id" value, KM.lookup "status" value) of
    (Just (String rawId), Just (String state)) -> case UUID.fromText rawId of
      Just fileId -> pure (fileId,state)
      Nothing -> throwError err500
    _ -> throwError err500
parseReservation _ = throwError err500
