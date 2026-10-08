{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.MusicReleaseAssets
  ( getPublicMusicAssetAccess
  , authorizeMusicDownload
  , authorizeFreeMusicDownload
  , loadMusicS3SigningConfig
  , trustedEdgeTerritory
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value, object, (.=))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Servant
import System.Environment (lookupEnv)

import TDF.API.MusicRelease
import TDF.Auth (AuthedUser(..))
import TDF.DB (Env(..))
import TDF.MusicRelease.Storage.S3

type AppM = ReaderT Env Handler

data AssetReference = AssetReference
  { assetProvider :: Text
  , assetBucket :: Text
  , assetObjectKey :: Text
  , assetMediaType :: Text
  , assetByteSize :: Int64
  , assetSha256 :: Text
  }

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

currentPartyId :: AuthedUser -> Int64
currentPartyId = fromSqlKey . auPartyId

-- The worker keeps inspected/promoted originals in `valid`, while generated
-- delivery assets use `ready`. This does not make a master publicly streamable:
-- only the separately authorized download paths may accept an immutable master.
downloadableAssetPredicate :: Text
downloadableAssetPredicate =
  "(asset.processing_state='ready' OR (asset.processing_state='valid' AND asset.asset_role='master_audio' AND asset.immutable=TRUE AND asset.storage_class<>'quarantine'))"

getPublicMusicAssetAccess :: UUID -> Maybe Text -> AppM Value
getPublicMusicAssetAccess assetId edgeCountry = do
  requireFeature "music_releases.public"
  territory <- trustedEdgeTerritory edgeCountry
  rows <- runDB (rawSql
    "SELECT asset.storage_provider,asset.bucket_name,asset.object_key,asset.media_type,asset.byte_size,asset.sha256 FROM music_asset asset WHERE asset.id=?::uuid AND music_public_asset_accessible(asset.id,?::text) LIMIT 1"
    [toPersistValue assetId, optionalText territory]
    :: SqlPersistT IO [(Single Text, Single Text, Single Text, Single Text, Single Int64, Single Text)])
  asset <- maybe (throwError err404) pure (assetFromRows rows)
  signedAssetResponse asset

authorizeMusicDownload :: AuthedUser -> UUID -> MusicDownloadAuthorizeRequest -> AppM Value
authorizeMusicDownload user entitlementId MusicDownloadAuthorizeRequest{..} = do
  requireFeature "music_releases.commerce"
  now <- liftIO getCurrentTime
  result <- runDB (authorizeDownloadDB (currentPartyId user) entitlementId musicDownloadRequestId now)
  asset <- case result of
    Left "not_found" -> throwError err404
    Left "request_conflict" -> throwError err409 { errBody = "Download requestId belongs to another entitlement" }
    Left "limit_reached" -> throwError err429 { errBody = "Download limit reached for this entitlement" }
    Left message -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 message) }
    Right value -> pure value
  signedAssetResponse asset

authorizeFreeMusicDownload :: AuthedUser -> Maybe Text -> MusicFreeDownloadRequest -> AppM Value
authorizeFreeMusicDownload user edgeCountry MusicFreeDownloadRequest{..} = do
  requireFeature "music_releases.public"
  territory <- trustedEdgeTerritory edgeCountry
  now <- liftIO getCurrentTime
  entitlementRows <- runDB (rawSql
    ("WITH offer AS (SELECT public.id AS release_id,public.release_version_id,rule.downloadable_asset_id FROM music_availability_rule rule JOIN music_public_release public ON public.release_version_id=rule.release_version_id JOIN music_asset asset ON asset.id=rule.downloadable_asset_id AND asset.release_version_id=public.release_version_id WHERE rule.id=?::uuid AND rule.download_policy='free' AND rule.purchasable=FALSE AND rule.downloadable_asset_id IS NOT NULL AND " <> downloadableAssetPredicate <> " AND (rule.starts_at IS NULL OR rule.starts_at<=NOW()) AND (rule.ends_at IS NULL OR rule.ends_at>NOW()) AND ((rule.territory_mode='include' AND ('Worldwide'=ANY(rule.territories) OR ?::text=ANY(rule.territories))) OR (rule.territory_mode='exclude' AND ?::text IS NOT NULL AND NOT (?::text=ANY(rule.territories))))), inserted AS (INSERT INTO music_entitlement(buyer_party_id,release_version_id,asset_id,source_kind,status,max_downloads,granted_by) SELECT ?,release_version_id,downloadable_asset_id,'free_grant','active',5,? FROM offer ON CONFLICT (buyer_party_id,release_version_id,asset_id,source_kind,purchase_order_id) DO UPDATE SET status='active',revoked_at=NULL RETURNING id,release_version_id) SELECT id,release_version_id FROM inserted")
    [ toPersistValue musicFreeAvailabilityRuleId, optionalText territory, optionalText territory
    , optionalText territory, PersistInt64 (currentPartyId user), PersistInt64 (currentPartyId user)
    ] :: SqlPersistT IO [(Single UUID, Single UUID)])
  (entitlementId, releaseVersionId) <- case entitlementRows of
    [(Single entitlementId, Single releaseVersionId)] -> pure (entitlementId, releaseVersionId)
    _ -> throwError err404
  result <- runDB (authorizeDownloadDB (currentPartyId user) entitlementId musicFreeRequestId now)
  asset <- case result of
    Left "request_conflict" -> throwError err409 { errBody = "Download requestId belongs to another entitlement" }
    Left "limit_reached" -> throwError err429 { errBody = "Download limit reached for this free grant" }
    Left _ -> throwError err404
    Right value -> pure value
  runDB $ rawExecute
    "INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,data) SELECT version.release_id,version.id,?,'free_download_authorized',jsonb_build_object('entitlement_id',?::uuid,'request_id',?::uuid) FROM music_release_version version WHERE version.id=?::uuid AND NOT EXISTS(SELECT 1 FROM music_release_audit_event audit WHERE audit.event_type='free_download_authorized' AND audit.data->>'request_id'=?)"
    [ PersistInt64 (currentPartyId user), toPersistValue entitlementId, toPersistValue musicFreeRequestId
    , toPersistValue releaseVersionId, PersistText (UUID.toText musicFreeRequestId)
    ]
  signedAssetResponse asset

authorizeDownloadDB
  :: Int64 -> UUID -> UUID -> UTCTime
  -> SqlPersistT IO (Either Text AssetReference)
authorizeDownloadDB actor entitlementId requestId now = do
  locked <- rawSql
    ("SELECT asset.storage_provider,asset.bucket_name,asset.object_key,asset.media_type,asset.byte_size,asset.sha256,entitlement.max_downloads FROM music_entitlement entitlement JOIN music_asset asset ON asset.id=entitlement.asset_id AND asset.release_version_id=entitlement.release_version_id JOIN music_public_release public ON public.release_version_id=entitlement.release_version_id WHERE entitlement.id=?::uuid AND entitlement.buyer_party_id=? AND entitlement.status='active' AND (entitlement.expires_at IS NULL OR entitlement.expires_at>NOW()) AND " <> downloadableAssetPredicate <> " FOR UPDATE OF entitlement")
    [toPersistValue entitlementId, PersistInt64 actor]
    :: SqlPersistT IO [(Single Text, Single Text, Single Text, Single Text, Single Int64, Single Text, Single (Maybe Int))]
  case locked of
    [(Single provider, Single bucket, Single key, Single mediaType, Single byteSize, Single sha256, Single maxDownloads)] -> do
      conflicts <- rawSql
        "SELECT entitlement_id=?::uuid FROM music_download_event WHERE request_id=?::uuid"
        [toPersistValue entitlementId, toPersistValue requestId] :: SqlPersistT IO [Single Bool]
      case conflicts of
        [Single True] -> pure (Right (AssetReference provider bucket key mediaType byteSize sha256))
        [Single False] -> pure (Left "request_conflict")
        [] -> do
          counts <- rawSql "SELECT count(*) FROM music_download_event WHERE entitlement_id=?::uuid"
            [toPersistValue entitlementId] :: SqlPersistT IO [Single Int64]
          let count = case counts of [Single value] -> value; _ -> maxBound
          if maybe False (count >=) (fromIntegral <$> maxDownloads)
            then pure (Left "limit_reached")
            else do
              rawExecute
                "INSERT INTO music_download_event(entitlement_id,request_id,authorized_at) VALUES(?::uuid,?::uuid,?)"
                [toPersistValue entitlementId, toPersistValue requestId, PersistUTCTime now]
              pure (Right (AssetReference provider bucket key mediaType byteSize sha256))
        _ -> pure (Left "request_conflict")
    [] -> pure (Left "not_found")
    _ -> pure (Left "ambiguous_entitlement")

signedAssetResponse :: AssetReference -> AppM Value
signedAssetResponse AssetReference{..} = do
  unless (assetProvider == "s3_compatible") $
    throwError err503 { errBody = "The local private asset adapter cannot issue browser URLs" }
  signingConfig <- loadMusicS3SigningConfig
  now <- liftIO getCurrentTime
  url <- either (const (throwError err500 { errBody = "Object storage signing configuration is invalid" })) pure
    (presignGetObject signingConfig now 300 assetBucket assetObjectKey)
  pure (object
    [ "url" .= url, "expiresAt" .= addUTCTime 300 now, "mediaType" .= assetMediaType
    , "byteSize" .= assetByteSize, "sha256" .= assetSha256, "acceptRanges" .= True
    ])

loadMusicS3SigningConfig :: AppM S3SigningConfig
loadMusicS3SigningConfig = do
  endpoint <- requiredEnv "MUSIC_S3_ENDPOINT" 512
  accessKey <- requiredEnv "MUSIC_S3_ACCESS_KEY_ID" 256
  secret <- requiredEnv "MUSIC_S3_SECRET_ACCESS_KEY" 512
  region <- fmap (T.strip . T.pack . fromMaybe "auto") (liftIO (lookupEnv "MUSIC_S3_REGION"))
  sessionToken <- fmap (fmap (T.strip . T.pack)) (liftIO (lookupEnv "MUSIC_S3_SESSION_TOKEN"))
  pure S3SigningConfig
    { s3Endpoint=endpoint, s3Region=region, s3AccessKeyId=accessKey
    , s3SecretAccessKey=secret, s3SessionToken=sessionToken
    }

requiredEnv :: String -> Int -> AppM Text
requiredEnv name maximumLength = do
  raw <- liftIO (lookupEnv name)
  case fmap (T.strip . T.pack) raw of
    Just value | not (T.null value) && T.length value <= maximumLength -> pure value
    _ -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack name <> " is required")) }

requireFeature :: Text -> AppM ()
requireFeature flag = do
  rawEnvironment <- liftIO (lookupEnv "APP_ENV")
  let environment = case fmap (T.toLower . T.strip . T.pack) rawEnvironment of
        Just "production" -> "production"
        Just "prod" -> "production"
        _ -> "sandbox"
  rows <- runDB (rawSql
    "SELECT enabled FROM revenue_feature_flag WHERE flag_key=? AND environment=?"
    [PersistText flag, PersistText environment] :: SqlPersistT IO [Single Bool])
  unless (rows == [Single True]) (throwError err404)

-- This header affects authorization. Ignore it unless the deployment proves
-- that clients cannot bypass or spoof the Cloudflare edge.
trustedEdgeTerritory :: Maybe Text -> AppM (Maybe Text)
trustedEdgeTerritory supplied = do
  trustHeader <- fmap (maybe False parseBoolean) (liftIO (lookupEnv "MUSIC_TRUST_CF_IPCOUNTRY"))
  pure $ if trustHeader then supplied >>= normalizeCountry else Nothing
  where
    parseBoolean value = map asciiLower value `elem` ["1", "true", "yes", "on"]
    asciiLower character
      | character >= 'A' && character <= 'Z' = toEnum (fromEnum character + 32)
      | otherwise = character
    normalizeCountry raw =
      let territory = T.toUpper (T.strip raw)
      in if T.length territory == 2
           && T.all (\character -> character >= 'A' && character <= 'Z') territory
           && territory `notElem` ["XX", "T1"]
         then Just territory
         else Nothing

assetFromRows
  :: [(Single Text, Single Text, Single Text, Single Text, Single Int64, Single Text)]
  -> Maybe AssetReference
assetFromRows rows = case rows of
  [(Single provider, Single bucket, Single key, Single mediaType, Single byteSize, Single sha256)] ->
    Just (AssetReference provider bucket key mediaType byteSize sha256)
  _ -> Nothing

optionalText :: Maybe Text -> PersistValue
optionalText = maybe PersistNull PersistText
