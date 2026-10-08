{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.MusicRelease
  ( musicReleasePublicServer
  , musicReleaseProtectedServer
  ) where

import Control.Exception (throwIO, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (ToJSON, Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Database.PostgreSQL.Simple (SqlError(..))
import Servant
import System.Environment (lookupEnv)

import TDF.API.MusicRelease
import TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.CMS.Models as CMS
import TDF.DB (Env(..))
import TDF.MusicRelease.Domain (CorrectionFailure(..), classifyCorrectionFailure)
import qualified TDF.Server.MusicReleaseCommerce as Commerce
import qualified TDF.Server.MusicReleaseCatalog as Catalog
import qualified TDF.Server.MusicReleaseDDEX as DDEX
import qualified TDF.Server.MusicReleaseAssets as Assets
import qualified TDF.Server.MusicReleaseUpload as Upload

type AppM = ReaderT Env Handler

musicReleasePublicServer :: ServerT MusicReleasePublicAPI AppM
musicReleasePublicServer =
       listPublicReleases
  :<|> getPublicRelease
  :<|> Assets.getPublicMusicAssetAccess
  :<|> recordAnonymousPlaybackEvent

musicReleaseProtectedServer :: AuthedUser -> ServerT MusicReleaseProtectedAPI AppM
musicReleaseProtectedServer user =
       listMyReleases user
  :<|> createRelease user
  :<|> Catalog.getMusicReleaseVersion user
  :<|> createCorrectionVersion user
  :<|> saveDraft user
  :<|> Catalog.replaceMusicReleaseContent user
  :<|> Catalog.acceptMusicReleaseTerms user
  :<|> validateReleaseVersion user
  :<|> transitionReleaseVersion user
  :<|> createEditorialComment user
  :<|> resolveEditorialComment user
  :<|> Upload.createMusicUpload user
  :<|> Upload.bindMusicUploadProvider user
  :<|> Upload.signMusicUploadPart user
  :<|> Upload.recordMusicUploadPart user
  :<|> Upload.completeMusicUpload user
  :<|> Upload.confirmMusicUpload user
  :<|> Upload.cancelMusicUpload user
  :<|> listFavorites user
  :<|> addFavorite user
  :<|> removeFavorite user
  :<|> listPlaylists user
  :<|> createPlaylist user
  :<|> updatePlaylist user
  :<|> deletePlaylist user
  :<|> addPlaylistItem user
  :<|> movePlaylistItem user
  :<|> deletePlaylistItem user
  :<|> recordAuthenticatedPlaybackEvent user
  :<|> listPlaybackHistory user
  :<|> createInfringementReport user
  :<|> listInfringementReports user
  :<|> actionInfringementReport user
  :<|> releaseAnalytics user
  :<|> Commerce.createMusicPurchase user
  :<|> Commerce.listMusicPurchases user
  :<|> Commerce.createMusicDatafastCheckout user
  :<|> Commerce.confirmMusicDatafastPayment user
  :<|> Commerce.createMusicPaypalOrder user
  :<|> Commerce.captureMusicPaypalOrder user
  :<|> Commerce.listMusicEntitlements user
  :<|> Assets.authorizeMusicDownload user
  :<|> Assets.authorizeFreeMusicDownload user
  :<|> DDEX.listDdexParties user
  :<|> DDEX.registerDdexParty user
  :<|> DDEX.listDdexExports user
  :<|> DDEX.createDdexExport user
  :<|> DDEX.downloadDdexExport user

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

jsonRows :: Text -> [PersistValue] -> AppM [Value]
jsonRows statement params = do
  rows <- runDB (rawSql statement params :: SqlPersistT IO [Single CMS.AesonValue])
  pure [CMS.unAesonValue value | Single value <- rows]

jsonOne :: ServerError -> Text -> [PersistValue] -> AppM Value
jsonOne missing statement params =
  jsonRows statement params >>= maybe (throwError missing) pure . listToMaybe

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

conflict :: Text -> ServerError
conflict message = err409 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

currentPartyId :: AuthedUser -> Int64
currentPartyId = fromSqlKey . auPartyId

featureEnvironment :: IO Text
featureEnvironment = do
  raw <- lookupEnv "APP_ENV"
  pure $ case fmap (T.toLower . T.strip . T.pack) raw of
    Just "production" -> "production"
    Just "prod" -> "production"
    _ -> "sandbox"

featureEnabled :: Text -> AppM Bool
featureEnabled flag = do
  environment <- liftIO featureEnvironment
  rows <- runDB (rawSql
    "SELECT enabled FROM revenue_feature_flag WHERE flag_key=? AND environment=?"
    [PersistText flag, PersistText environment] :: SqlPersistT IO [Single Bool])
  pure (rows == [Single True])

requireFeature :: Text -> AppM ()
requireFeature flag = do
  enabled <- featureEnabled flag
  unless enabled $ throwError err503
    { errBody = BL.fromStrict (TE.encodeUtf8 ("Feature " <> flag <> " is not enabled in this environment")) }

requirePublicFeature :: AppM ()
requirePublicFeature = do
  enabled <- featureEnabled "music_releases.public"
  unless enabled (throwError err404)

requireIdempotencyKey :: Maybe Text -> AppM Text
requireIdempotencyKey supplied = do
  let value = T.strip (fromMaybe "" supplied)
  unless (T.length value >= 8 && T.length value <= 200 && T.all safe value) $
    throwError (badRequest "Idempotency-Key must contain 8-200 visible ASCII characters")
  pure value
  where
    safe character = character >= '!' && character <= '~'

requireMusicPermission :: AuthedUser -> UUID -> Text -> AppM Int64
requireMusicPermission user releaseId permission = do
  rows <- runDB (rawSql
    "SELECT r.artist_party_id, music_can(?,r.artist_party_id,?) FROM music_release r WHERE r.id=?::uuid"
    [PersistInt64 (currentPartyId user), PersistText permission, toPersistValue releaseId]
    :: SqlPersistT IO [(Single Int64, Single Bool)])
  case rows of
    [(Single artistId, Single True)] -> pure artistId
    [(Single _, Single False)] -> throwError err403
    _ -> throwError err404

requireOwnerPermission :: AuthedUser -> UUID -> Text -> AppM ()
requireOwnerPermission user releaseId permission =
  if hasStrictAdminAccess user
    then pure ()
    else requireMusicPermission user releaseId permission >> pure ()

listPublicReleases :: Maybe Text -> Maybe Text -> Maybe Int64 -> Maybe Int -> Maybe Int -> AppM [Value]
listPublicReleases edgeCountry query artistPartyId requestedLimit requestedOffset = do
  requirePublicFeature
  territory <- Assets.trustedEdgeTerritory edgeCountry
  let limit = max 1 (min 100 (fromMaybe 24 requestedLimit))
      offset = max 0 (fromMaybe 0 requestedOffset)
      normalizedQuery = T.strip (fromMaybe "" query)
  jsonRows
    "SELECT jsonb_build_object(\
    \ 'id',public.id,'artistPartyId',public.artist_party_id,'slug',public.canonical_slug,\
    \ 'kind',public.release_kind,'versionId',public.release_version_id,'versionNumber',public.version_number,\
    \ 'title',public.title,'subtitle',public.subtitle,'versionTitle',public.version_title,\
    \ 'displayArtist',public.display_artist,'explicitContent',public.explicit_content,\
    \ 'originalReleaseDate',public.original_release_date,'releaseAtUtc',public.release_at_utc,\
    \ 'labelName',public.label_name,'catalogNumber',public.catalog_number,'publishedAt',public.published_at,\
    \ 'coverAssetId',(SELECT asset.id FROM music_asset asset WHERE asset.release_version_id=public.release_version_id AND asset.asset_role='cover_display' AND asset.processing_state='ready' ORDER BY asset.created_at DESC LIMIT 1)\
    \) FROM music_public_release public\
    \ WHERE (?='' OR public.title ILIKE '%' || ? || '%' OR public.display_artist ILIKE '%' || ? || '%')\
    \   AND (?::bigint IS NULL OR public.artist_party_id=?::bigint)\
    \   AND EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=public.release_version_id\
    \     AND (rule.starts_at IS NULL OR rule.starts_at<=NOW()) AND (rule.ends_at IS NULL OR rule.ends_at>NOW())\
    \     AND ((rule.territory_mode='include' AND ('Worldwide'=ANY(rule.territories) OR ?::text=ANY(rule.territories)))\
    \       OR (rule.territory_mode='exclude' AND ?::text IS NOT NULL AND NOT (?::text=ANY(rule.territories)))))\
    \ ORDER BY public.published_at DESC, public.id LIMIT ? OFFSET ?"
    [ PersistText normalizedQuery, PersistText normalizedQuery, PersistText normalizedQuery
    , optionalInt64 artistPartyId, optionalInt64 artistPartyId
    , optionalText territory, optionalText territory, optionalText territory
    , PersistInt64 (fromIntegral limit), PersistInt64 (fromIntegral offset)
    ]

getPublicRelease :: Text -> Maybe Text -> AppM Value
getPublicRelease rawSlug edgeCountry = do
  requirePublicFeature
  territory <- Assets.trustedEdgeTerritory edgeCountry
  let slug = T.toLower (T.strip rawSlug)
  unless (validSlug slug) (throwError err404)
  jsonOne err404
    "SELECT jsonb_build_object(\
    \ 'id',public.id,'artistPartyId',public.artist_party_id,'slug',public.canonical_slug,\
    \ 'kind',public.release_kind,'versionId',public.release_version_id,'versionNumber',public.version_number,\
    \ 'title',public.title,'subtitle',public.subtitle,'versionTitle',public.version_title,\
    \ 'displayArtist',public.display_artist,'explicitContent',public.explicit_content,\
    \ 'originalReleaseDate',public.original_release_date,'releaseAtUtc',public.release_at_utc,\
    \ 'labelName',public.label_name,'catalogNumber',public.catalog_number,'publishedAt',public.published_at,\
    \ 'tracks',COALESCE((SELECT jsonb_agg(jsonb_build_object(\
    \    'trackId',track.id,'recordingId',recording.id,'discNumber',track.disc_number,\
    \    'trackNumber',track.track_number,'title',recording.canonical_title,\
    \    'displayArtist',track.display_artist,'durationMs',recording.duration_ms,\
    \    'explicitContent',recording.explicit_content,\
    \    'sources',COALESCE((SELECT jsonb_agg(jsonb_build_object('assetId',asset.id,'role',asset.asset_role,'mediaType',asset.media_type,'technicalMetadata',asset.technical_metadata) ORDER BY asset.asset_role,asset.created_at)\
    \       FROM music_asset asset WHERE asset.release_version_id=public.release_version_id AND asset.recording_id=recording.id AND asset.asset_role IN ('stream_audio','preview_audio') AND asset.processing_state='ready' AND (asset.asset_role<>'preview_audio' OR music_preview_matches(asset.id))),'[]'::jsonb)\
    \  ) ORDER BY track.disc_number,track.track_number)\
    \  FROM music_release_track track JOIN music_recording recording ON recording.id=track.recording_id\
    \  WHERE track.release_version_id=public.release_version_id),'[]'::jsonb),\
    \ 'coverAssets',COALESCE((SELECT jsonb_agg(jsonb_build_object('assetId',asset.id,'role',asset.asset_role,'mediaType',asset.media_type,'technicalMetadata',asset.technical_metadata) ORDER BY asset.asset_role)\
    \  FROM music_asset asset WHERE asset.release_version_id=public.release_version_id AND asset.asset_role IN ('cover_display','thumbnail') AND asset.processing_state='ready'),'[]'::jsonb),\
    \ 'availability',COALESCE((SELECT jsonb_agg(jsonb_build_object('ruleId',rule.id,'trackId',rule.release_track_id,'territoryMode',rule.territory_mode,'territories',rule.territories,'startsAt',rule.starts_at,'endsAt',rule.ends_at,'listeningPolicy',rule.listening_policy,'downloadPolicy',rule.download_policy,'purchasable',rule.purchasable,'priceMinor',rule.price_minor,'currency',rule.currency)) FROM music_availability_rule rule WHERE rule.release_version_id=public.release_version_id),'[]'::jsonb)\
    \) FROM music_public_release public WHERE public.canonical_slug=?\
    \ AND EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=public.release_version_id\
    \   AND (rule.starts_at IS NULL OR rule.starts_at<=NOW()) AND (rule.ends_at IS NULL OR rule.ends_at>NOW())\
    \   AND ((rule.territory_mode='include' AND ('Worldwide'=ANY(rule.territories) OR ?::text=ANY(rule.territories)))\
    \     OR (rule.territory_mode='exclude' AND ?::text IS NOT NULL AND NOT (?::text=ANY(rule.territories)))))"
    [PersistText slug, optionalText territory, optionalText territory, optionalText territory]

listMyReleases :: AuthedUser -> Maybe Int64 -> AppM [Value]
listMyReleases user requestedArtistId = do
  requireFeature "music_releases.authoring"
  let actor = currentPartyId user
      artistId = fromMaybe actor requestedArtistId
  unless (hasStrictAdminAccess user) $ do
    allowed <- booleanQuery "SELECT music_can(?,?,'release.read')" [PersistInt64 actor, PersistInt64 artistId]
    unless allowed (throwError err403)
  jsonRows
    "SELECT jsonb_build_object('id',release.id,'artistPartyId',release.artist_party_id,'slug',release.canonical_slug,'kind',release.release_kind,'publishedVersionId',release.published_version_id,'withdrawnAt',release.withdrawn_at,'createdAt',release.created_at,'updatedAt',release.updated_at,'versions',COALESCE((SELECT jsonb_agg(jsonb_build_object('id',version.id,'number',version.version_number,'state',version.state,'title',version.title,'displayArtist',version.display_artist,'updatedAt',version.updated_at) ORDER BY version.version_number DESC) FROM music_release_version version WHERE version.release_id=release.id),'[]'::jsonb)) FROM music_release release WHERE release.artist_party_id=? AND (release.created_by=? OR music_can(?,release.artist_party_id,'release.read')) ORDER BY release.updated_at DESC"
    [PersistInt64 artistId, PersistInt64 actor, PersistInt64 actor]

createRelease :: AuthedUser -> Maybe Text -> MusicReleaseCreateRequest -> AppM Value
createRelease user idempotencyHeader MusicReleaseCreateRequest{..} = do
  requireFeature "music_releases.authoring"
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  let actor = currentPartyId user
      slug = T.toLower (T.strip musicCreateCanonicalSlug)
      kind = T.toLower (T.strip musicCreateReleaseKind)
      title = T.strip musicCreateTitle
      displayArtist = T.strip musicCreateDisplayArtist
      language = T.strip musicCreateTitleLanguage
  unless (validSlug slug) (throwError (badRequest "canonicalSlug must be a lowercase URL-safe slug"))
  unless (kind `elem` ["single","ep","album"]) (throwError (badRequest "releaseKind must be single, ep, or album"))
  unless (validRequiredText 500 title && validRequiredText 500 displayArtist) (throwError (badRequest "title and displayArtist are required and limited to 500 characters"))
  allowed <- booleanQuery "SELECT music_can(?,?,'release.create')" [PersistInt64 actor, PersistInt64 musicCreateArtistPartyId]
  unless allowed (throwError err403)
  existing <- jsonRows
    "SELECT jsonb_build_object('id',release.id,'versionId',version.id,'state',version.state,'slug',release.canonical_slug,'kind',release.release_kind,'title',version.title,'displayArtist',version.display_artist,'updatedAt',version.updated_at) FROM music_release_audit_event audit JOIN music_release release ON release.id=audit.release_id JOIN music_release_version version ON version.id=audit.release_version_id WHERE audit.actor_party_id=? AND audit.idempotency_key=? AND audit.event_type='release_created' LIMIT 1"
    [PersistInt64 actor, PersistText idempotencyKey]
  case existing of
    value : _ -> pure value
    [] -> jsonOne (conflict "Release could not be created")
      "WITH new_release AS (\
      \ INSERT INTO music_release(artist_party_id,canonical_slug,release_kind,created_by) VALUES(?,?,?,?) RETURNING *\
      \), new_version AS (\
      \ INSERT INTO music_release_version(release_id,version_number,title,display_artist,title_language,created_by) SELECT id,1,?,?,?,? FROM new_release RETURNING *\
      \), audit AS (\
      \ INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,idempotency_key,data) SELECT release.id,version.id,?,'release_created',?,jsonb_build_object('slug',release.canonical_slug) FROM new_release release CROSS JOIN new_version version\
      \) SELECT jsonb_build_object('id',release.id,'versionId',version.id,'state',version.state,'slug',release.canonical_slug,'kind',release.release_kind,'title',version.title,'displayArtist',version.display_artist,'updatedAt',version.updated_at) FROM new_release release CROSS JOIN new_version version"
      [ PersistInt64 musicCreateArtistPartyId, PersistText slug, PersistText kind, PersistInt64 actor
      , PersistText title, PersistText displayArtist, PersistText language, PersistInt64 actor
      , PersistInt64 actor, PersistText idempotencyKey
      ]

saveDraft :: AuthedUser -> UUID -> UUID -> MusicReleaseDraftSaveRequest -> AppM Value
saveDraft user releaseId versionId MusicReleaseDraftSaveRequest{..} = do
  requireFeature "music_releases.authoring"
  requireOwnerPermission user releaseId "release.edit"
  unless (validRequiredText 500 (T.strip musicDraftTitle) && validRequiredText 500 (T.strip musicDraftDisplayArtist)) $
    throwError (badRequest "title and displayArtist are required and limited to 500 characters")
  unless ((musicDraftReleaseAtUtc == Nothing) == (musicDraftReleaseTimezone == Nothing)) $
    throwError (badRequest "releaseAtUtc and releaseTimezone must be supplied together")
  mapM_ validateCanonicalGenre musicDraftPrimaryGenreId
  mapM_ validateCanonicalGenre musicDraftSecondaryGenreId
  when (musicDraftPrimaryGenreId /= Nothing && musicDraftPrimaryGenreId == musicDraftSecondaryGenreId) $
    throwError (badRequest "primaryGenreId and secondaryGenreId must be different")
  updated <- runDB (rawSql
    "UPDATE music_release_version SET title=?,subtitle=?,version_title=?,display_artist=?,title_language=?,title_script=?,primary_genre_id=?::uuid,secondary_genre_id=?::uuid,explicit_content=?,original_release_date=?,release_at_utc=?,release_timezone=?,embargo_until_utc=?,label_name=?,catalog_number=?,recording_copyright_text=?,work_copyright_text=?,metadata_valid=FALSE,rights_valid=FALSE,updated_at=NOW() WHERE id=?::uuid AND release_id=?::uuid AND updated_at=? AND state IN ('draft','uploading','processing','validation_failed','changes_requested') RETURNING id"
    [ PersistText (T.strip musicDraftTitle), optionalText musicDraftSubtitle, optionalText musicDraftVersionTitle
    , PersistText (T.strip musicDraftDisplayArtist), PersistText (T.strip musicDraftTitleLanguage), optionalText musicDraftTitleScript
    , optionalUuid musicDraftPrimaryGenreId, optionalUuid musicDraftSecondaryGenreId
    , PersistText musicDraftExplicitContent, optionalDay musicDraftOriginalReleaseDate, optionalTime musicDraftReleaseAtUtc
    , optionalText musicDraftReleaseTimezone, optionalTime musicDraftEmbargoUntilUtc, optionalText musicDraftLabelName
    , optionalText musicDraftCatalogNumber, optionalText musicDraftRecordingCopyrightText, optionalText musicDraftWorkCopyrightText
    , toPersistValue versionId, toPersistValue releaseId, PersistUTCTime musicDraftExpectedUpdatedAt
    ] :: SqlPersistT IO [Single UUID])
  when (null updated) $ throwError (conflict "Draft changed, is no longer editable, or expectedUpdatedAt is stale")
  appendAudit releaseId (Just versionId) (Just (currentPartyId user)) "draft_autosaved" Nothing Nothing Nothing Nothing
  releaseVersionJson releaseId versionId

validateReleaseVersion :: AuthedUser -> UUID -> UUID -> AppM Value
validateReleaseVersion user releaseId versionId = do
  requireFeature "music_releases.authoring"
  requireOwnerPermission user releaseId "release.edit"
  ensureVersion releaseId versionId
  jsonOne err404
    "SELECT jsonb_build_object('valid',NOT EXISTS(SELECT 1 FROM music_check_submission(?::uuid)),'errors',COALESCE((SELECT jsonb_agg(jsonb_build_object('fieldPath',field_path,'code',error_code,'message',message)) FROM music_check_submission(?::uuid)),'[]'::jsonb))"
    [toPersistValue versionId, toPersistValue versionId]

transitionReleaseVersion :: AuthedUser -> UUID -> UUID -> Maybe Text -> MusicReleaseTransitionRequest -> AppM Value
transitionReleaseVersion user releaseId versionId idempotencyHeader request@MusicReleaseTransitionRequest{..} = do
  requireFeature "music_releases.authoring"
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  let target = T.toLower (T.strip musicTransitionTargetState)
      actor = currentPartyId user
      staffStates = ["in_review","changes_requested","approved","scheduled","suspended","replacement_pending","takedown_scheduled","withdrawn"]
  unless (target `elem` allStates) (throwError (badRequest "Unknown release lifecycle state"))
  when (target == "published") (throwError (badRequest "Published is scheduler-only; use scheduled with a future UTC time"))
  if target `elem` staffStates
    then unless (hasStrictAdminAccess user) (throwError err403)
    else requireOwnerPermission user releaseId "release.submit"
  when (target == "scheduled" && (musicTransitionReleaseAtUtc == Nothing || musicTransitionReleaseTimezone == Nothing)) $
    throwError (badRequest "Scheduling requires releaseAtUtc and releaseTimezone")
  when (target == "changes_requested" && maybe True (T.null . T.strip) musicTransitionReason) $
    throwError (badRequest "A change request requires a reason")
  when (target == "takedown_scheduled" && (musicTransitionTakedownAtUtc == Nothing || musicTransitionTakedownTimezone == Nothing)) $
    throwError (badRequest "A scheduled takedown requires takedownAtUtc and takedownTimezone")
  outcome <- runDB $ do
    _ <- rawSql
      "SELECT 1::bigint FROM (SELECT pg_advisory_xact_lock(hashtextextended(?,0))) locked"
      [PersistText (UUID.toText releaseId <> ":" <> idempotencyKey)] :: SqlPersistT IO [Single Int64]
    duplicate <- rawSql
      "SELECT jsonb_build_object('id',version.id,'releaseId',version.release_id,'state',version.state,'title',version.title,'updatedAt',version.updated_at) FROM music_release_audit_event audit JOIN music_release_version version ON version.id=audit.release_version_id WHERE audit.release_id=?::uuid AND audit.idempotency_key=? LIMIT 1"
      [toPersistValue releaseId, PersistText idempotencyKey] :: SqlPersistT IO [Single CMS.AesonValue]
    case duplicate of
      Single existing : _ -> pure (Left (CMS.unAesonValue existing))
      [] -> do
        -- Editorial changes and content saves use this same row lock. Read the
        -- approved graph only after acquiring it, inside the approving transaction.
        _ <- rawSql
          "SELECT 1::bigint FROM music_release_version WHERE id=?::uuid AND release_id=?::uuid FOR UPDATE"
          [toPersistValue versionId,toPersistValue releaseId] :: SqlPersistT IO [Single Int64]
        issues <- if target `elem` ["ready_for_review","in_review","approved","scheduled"]
          then rawSql
            "SELECT jsonb_build_object('message','Corrige los errores de validación antes de continuar.','errors',jsonb_agg(jsonb_build_object('fieldPath',field_path,'code',error_code,'message',message))) FROM music_check_submission(?::uuid) HAVING count(*)>0"
            [toPersistValue versionId]
          else pure []
          :: SqlPersistT IO [Single CMS.AesonValue]
        case issues of
          Single problem : _ -> pure (Right (Left err422
            { errHeaders = [("Content-Type","application/json; charset=utf-8")]
            , errBody = encode (CMS.unAesonValue problem)
            }))
          [] -> do
            immutableSnapshot <- if target == "approved"
              then canonicalSnapshot releaseId versionId else pure musicTransitionSnapshot
            let calculatedHash = hashValue <$> immutableSnapshot
            case (musicTransitionSnapshotSha256, calculatedHash) of
              (Just supplied, Just calculated) | T.toLower supplied /= calculated ->
                pure (Right (Left (badRequest "snapshotSha256 does not match the server-generated canonical snapshot")))
              _ -> do
                changed <- applyTransitionDB actor releaseId versionId request immutableSnapshot calculatedHash
                when changed $ rawExecute
                  "INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,idempotency_key,next_state,data) VALUES(?::uuid,?::uuid,?,'state_transition_requested',?,?,jsonb_build_object('reason',?::text))"
                  [toPersistValue releaseId,toPersistValue versionId,PersistInt64 actor,PersistText idempotencyKey,PersistText target,optionalText musicTransitionReason]
                pure (Right (Right changed))
  case outcome of
    Left existing -> pure existing
    Right (Left problem) -> throwError problem
    Right (Right False) -> throwError err404
    Right (Right True) -> releaseVersionJson releaseId versionId

applyTransitionDB :: Int64 -> UUID -> UUID -> MusicReleaseTransitionRequest -> Maybe Value -> Maybe Text -> SqlPersistT IO Bool
applyTransitionDB actor releaseId versionId MusicReleaseTransitionRequest{..} snapshot snapshotHash = do
  let target = T.toLower (T.strip musicTransitionTargetState)
  changed <- case target of
    "approved" -> rawSql
      "UPDATE music_release_version SET state='approved',approved_by=?,approved_at=NOW(),immutable_snapshot=?::jsonb,snapshot_sha256=?,updated_at=NOW() WHERE id=?::uuid AND release_id=?::uuid RETURNING id"
      [ PersistInt64 actor, maybe PersistNull (PersistText . jsonText) snapshot, maybe PersistNull PersistText snapshotHash
      , toPersistValue versionId, toPersistValue releaseId
      ]
    "scheduled" -> rawSql
      "UPDATE music_release_version SET state='scheduled',scheduled_by=?,release_at_utc=?,release_timezone=?,embargo_until_utc=?,updated_at=NOW() WHERE id=?::uuid AND release_id=?::uuid RETURNING id"
      [ PersistInt64 actor, optionalTime musicTransitionReleaseAtUtc, optionalText musicTransitionReleaseTimezone
      , optionalTime musicTransitionEmbargoUntilUtc, toPersistValue versionId, toPersistValue releaseId
      ]
    "takedown_scheduled" -> rawSql
      "UPDATE music_release_version SET state='takedown_scheduled',takedown_at_utc=?,takedown_timezone=?,updated_at=NOW() WHERE id=?::uuid AND release_id=?::uuid RETURNING id"
      [ optionalTime musicTransitionTakedownAtUtc, optionalText musicTransitionTakedownTimezone
      , toPersistValue versionId, toPersistValue releaseId
      ]
    _ -> rawSql
      "UPDATE music_release_version SET state=?,updated_at=NOW() WHERE id=?::uuid AND release_id=?::uuid RETURNING id"
      [PersistText target, toPersistValue versionId, toPersistValue releaseId]
  pure (not (null (changed :: [Single UUID])))

createEditorialComment :: AuthedUser -> UUID -> UUID -> MusicReleaseCommentRequest -> AppM Value
createEditorialComment user releaseId versionId MusicReleaseCommentRequest{..} = do
  requireFeature "music_releases.authoring"
  if musicCommentStaffOnly || musicCommentRequestChanges
    then unless (hasStrictAdminAccess user) (throwError err403)
    else requireOwnerPermission user releaseId "release.edit"
  ensureVersion releaseId versionId
  let body = T.strip musicCommentBody
  unless (validRequiredText 10000 body) (throwError (badRequest "Comment body is required and limited to 10000 characters"))
  let params =
        [ toPersistValue versionId, optionalUuid musicCommentParentId, optionalText musicCommentFieldPath
        , PersistText body, PersistText (if musicCommentStaffOnly then "staff_only" else "artist_and_staff")
        , PersistInt64 (currentPartyId user)
        ]
      insertSql =
        "INSERT INTO music_editorial_comment(release_version_id,parent_comment_id,field_path,body,visibility,created_by) VALUES(?::uuid,?::uuid,?,?,?,?) RETURNING jsonb_build_object('id',id,'releaseVersionId',release_version_id,'parentCommentId',parent_comment_id,'fieldPath',field_path,'body',body,'visibility',visibility,'resolutionState',resolution_state,'createdBy',created_by,'createdAt',created_at)"
  if not musicCommentRequestChanges
    then jsonOne (conflict "Comment could not be created") insertSql params
    else jsonOne (conflict "A change request can only be created while the version is in review")
      "WITH changed AS (UPDATE music_release_version SET state='changes_requested',updated_at=NOW() WHERE id=?::uuid AND release_id=?::uuid AND state='in_review' RETURNING id), inserted AS (INSERT INTO music_editorial_comment(release_version_id,parent_comment_id,field_path,body,visibility,created_by) SELECT changed.id,?::uuid,?,?,?,? FROM changed RETURNING *) SELECT jsonb_build_object('id',id,'releaseVersionId',release_version_id,'parentCommentId',parent_comment_id,'fieldPath',field_path,'body',body,'visibility',visibility,'resolutionState',resolution_state,'createdBy',created_by,'createdAt',created_at) FROM inserted"
      [ toPersistValue versionId, toPersistValue releaseId, optionalUuid musicCommentParentId
      , optionalText musicCommentFieldPath, PersistText body
      , PersistText (if musicCommentStaffOnly then "staff_only" else "artist_and_staff")
      , PersistInt64 (currentPartyId user)
      ]

resolveEditorialComment :: AuthedUser -> UUID -> UUID -> UUID -> AppM Value
resolveEditorialComment user releaseId versionId commentId = do
  requireFeature "music_releases.authoring"
  requireOwnerPermission user releaseId "release.edit"
  ensureVersion releaseId versionId
  value <- jsonOne (conflict "The change request is not open or does not belong to this version")
    "UPDATE music_editorial_comment SET resolution_state='resolved',resolved_by=?,resolved_at=NOW() WHERE id=?::uuid AND release_version_id=?::uuid AND resolution_state='open' RETURNING jsonb_build_object('id',id,'releaseVersionId',release_version_id,'parentCommentId',parent_comment_id,'fieldPath',field_path,'body',body,'visibility',visibility,'resolutionState',resolution_state,'createdBy',created_by,'createdAt',created_at,'resolvedBy',resolved_by,'resolvedAt',resolved_at)"
    [PersistInt64 (currentPartyId user),toPersistValue commentId,toPersistValue versionId]
  runDB $ rawExecute
    "INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,data) VALUES(?::uuid,?::uuid,?,'change_request_resolved',jsonb_build_object('comment_id',?::uuid))"
    [toPersistValue releaseId,toPersistValue versionId,PersistInt64 (currentPartyId user),toPersistValue commentId]
  pure value

createCorrectionVersion :: AuthedUser -> UUID -> UUID -> Maybe Text -> AppM Value
createCorrectionVersion user releaseId sourceVersionId idempotencyHeader = do
  requireFeature "music_releases.authoring"
  requireOwnerPermission user releaseId "release.edit"
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  eligible <- booleanQuery
    "SELECT EXISTS(SELECT 1 FROM music_release_version WHERE id=?::uuid AND release_id=?::uuid AND state IN ('approved','scheduled','published','suspended','replacement_pending','takedown_scheduled','withdrawn'))"
    [toPersistValue sourceVersionId,toPersistValue releaseId]
  unless eligible (throwError (conflict "Only an immutable approved or formerly published version can be corrected"))
  outcome <- runCorrectionDB $ do
    _ <- rawSql
      "SELECT 1::bigint FROM (SELECT pg_advisory_xact_lock(hashtextextended(?,0))) locked"
      [PersistText (UUID.toText releaseId <> ":" <> idempotencyKey)]
      :: SqlPersistT IO [Single Int64]
    existing <- rawSql
      "SELECT (data->>'correctionVersionId')::uuid,(data->>'sourceVersionId')::uuid FROM music_release_audit_event WHERE release_id=?::uuid AND idempotency_key=? AND event_type='correction_created'"
      [toPersistValue releaseId,PersistText idempotencyKey] :: SqlPersistT IO [(Single UUID,Single (Maybe UUID))]
    case existing of
      (Single existingId,Single originalSource) : _
        | originalSource == Just sourceVersionId -> pure (Right existingId)
        | otherwise -> pure (Left (conflict
            "Esta clave de idempotencia pertenece a otra versión de origen. Usa una clave nueva para una corrección distinta."))
      [] -> do
        created <- rawSql "SELECT music_create_release_correction(?::uuid,?::uuid,?)"
          [toPersistValue releaseId,toPersistValue sourceVersionId,PersistInt64 (currentPartyId user)]
          :: SqlPersistT IO [Single UUID]
        newId <- case created of
          [Single value] -> pure value
          _ -> liftIO (fail "Source release version is not eligible for correction")
        rawExecute
          "INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,idempotency_key,data) VALUES(?::uuid,?::uuid,?,'correction_created',?,jsonb_build_object('sourceVersionId',?::uuid,'correctionVersionId',?::uuid))"
          [toPersistValue releaseId,toPersistValue newId,PersistInt64 (currentPartyId user),PersistText idempotencyKey,toPersistValue sourceVersionId,toPersistValue newId]
        pure (Right newId)
  correctionId <- either throwError pure outcome
  Catalog.getMusicReleaseVersion user releaseId correctionId

-- Catch outside runSqlPool: the transaction (including cloned rows/audit) has
-- already rolled back before a known failure is returned to the client.
runCorrectionDB :: SqlPersistT IO a -> AppM a
runCorrectionDB action = do
  pool <- asks envPool
  outcome <- liftIO (tryCorrectionSql (runSqlPool action pool))
  case outcome of
    Right value -> pure value
    Left sqlError ->
      case classifyCorrectionFailure
        (TE.decodeUtf8With lenientDecode (sqlState sqlError))
        (TE.decodeUtf8With lenientDecode (sqlErrorMsg sqlError)) of
        Nothing -> liftIO (throwIO sqlError)
        Just CorrectionFailure{..} -> throwError
          (if correctionFailureStatus == 422 then err422 else err409)
            { errHeaders = [("Content-Type","application/json; charset=utf-8")]
            , errBody = encode (object
                [ "message" .= correctionFailureMessage
                , "errors" .= [object
                    [ "code" .= correctionFailureCode
                    , "fieldPath" .= correctionFailureField
                    , "message" .= correctionFailureMessage
                    ]]
                ])
            }

tryCorrectionSql :: IO a -> IO (Either SqlError a)
tryCorrectionSql = try

listFavorites :: AuthedUser -> Maybe Text -> AppM [Value]
listFavorites user edgeCountry = do
  requirePublicFeature
  territory <- Assets.trustedEdgeTerritory edgeCountry
  jsonRows
    "SELECT jsonb_build_object('recordingId',recording.id,'trackId',public.resolved_track_id,'title',recording.canonical_title,'durationMs',recording.duration_ms,'createdAt',favorite.created_at,'available',public.release_version_id IS NOT NULL,'releaseId',public.id,'releaseVersionId',public.release_version_id,'slug',public.canonical_slug,'displayArtist',public.display_artist,'sources',COALESCE((SELECT jsonb_agg(jsonb_build_object('assetId',asset.id,'role',asset.asset_role,'mediaType',asset.media_type,'technicalMetadata',asset.technical_metadata) ORDER BY asset.asset_role,asset.created_at) FROM music_asset asset WHERE asset.release_version_id=public.release_version_id AND asset.recording_id=recording.id AND asset.asset_role IN ('stream_audio','preview_audio') AND asset.processing_state='ready' AND (asset.asset_role<>'preview_audio' OR music_preview_matches(asset.id))),'[]'::jsonb)) FROM music_favorite favorite JOIN music_recording recording ON recording.id=favorite.recording_id LEFT JOIN LATERAL (SELECT candidate.*,track.id AS resolved_track_id FROM music_public_release candidate JOIN music_release_track track ON track.release_version_id=candidate.release_version_id WHERE track.recording_id=recording.id AND EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=candidate.release_version_id AND (rule.release_track_id IS NULL OR rule.release_track_id=track.id) AND (rule.starts_at IS NULL OR rule.starts_at<=NOW()) AND (rule.ends_at IS NULL OR rule.ends_at>NOW()) AND rule.listening_policy<>'none' AND ((rule.territory_mode='include' AND ('Worldwide'=ANY(rule.territories) OR ?::text=ANY(rule.territories))) OR (rule.territory_mode='exclude' AND ?::text IS NOT NULL AND NOT (?::text=ANY(rule.territories))))) ORDER BY candidate.published_at DESC LIMIT 1) public ON TRUE WHERE favorite.party_id=? ORDER BY favorite.created_at DESC"
    [optionalText territory,optionalText territory,optionalText territory,PersistInt64 (currentPartyId user)]

addFavorite :: AuthedUser -> Maybe Text -> MusicFavoriteRequest -> AppM NoContent
addFavorite user edgeCountry MusicFavoriteRequest{..} = do
  requirePublicFeature
  territory <- Assets.trustedEdgeTerritory edgeCountry
  allowed <- recordingIsPublic musicFavoriteRecordingId territory
  unless allowed (throwError err404)
  runDB $ rawExecute
    "INSERT INTO music_favorite(party_id,recording_id) VALUES(?,?::uuid) ON CONFLICT DO NOTHING"
    [PersistInt64 (currentPartyId user), toPersistValue musicFavoriteRecordingId]
  pure NoContent

removeFavorite :: AuthedUser -> UUID -> AppM NoContent
removeFavorite user recordingId = do
  requirePublicFeature
  runDB $ rawExecute "DELETE FROM music_favorite WHERE party_id=? AND recording_id=?::uuid"
    [PersistInt64 (currentPartyId user), toPersistValue recordingId]
  pure NoContent

listPlaylists :: AuthedUser -> Maybe Text -> AppM [Value]
listPlaylists user edgeCountry = do
  requirePublicFeature
  territory <- Assets.trustedEdgeTerritory edgeCountry
  jsonRows
    "SELECT jsonb_build_object('id',playlist.id,'name',playlist.name,'visibility',playlist.visibility,'createdAt',playlist.created_at,'updatedAt',playlist.updated_at,'items',COALESCE((SELECT jsonb_agg(jsonb_build_object('id',item.id,'recordingId',recording.id,'trackId',public.resolved_track_id,'position',item.position,'addedAt',item.added_at,'title',recording.canonical_title,'durationMs',recording.duration_ms,'available',public.release_version_id IS NOT NULL,'releaseId',public.id,'releaseVersionId',public.release_version_id,'slug',public.canonical_slug,'displayArtist',public.display_artist,'sources',COALESCE((SELECT jsonb_agg(jsonb_build_object('assetId',asset.id,'role',asset.asset_role,'mediaType',asset.media_type,'technicalMetadata',asset.technical_metadata) ORDER BY asset.asset_role,asset.created_at) FROM music_asset asset WHERE asset.release_version_id=public.release_version_id AND asset.recording_id=recording.id AND asset.asset_role IN ('stream_audio','preview_audio') AND asset.processing_state='ready' AND (asset.asset_role<>'preview_audio' OR music_preview_matches(asset.id))),'[]'::jsonb)) ORDER BY item.position) FROM music_playlist_item item JOIN music_recording recording ON recording.id=item.recording_id LEFT JOIN LATERAL (SELECT candidate.*,track.id AS resolved_track_id FROM music_public_release candidate JOIN music_release_track track ON track.release_version_id=candidate.release_version_id WHERE track.recording_id=recording.id AND EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=candidate.release_version_id AND (rule.release_track_id IS NULL OR rule.release_track_id=track.id) AND (rule.starts_at IS NULL OR rule.starts_at<=NOW()) AND (rule.ends_at IS NULL OR rule.ends_at>NOW()) AND rule.listening_policy<>'none' AND ((rule.territory_mode='include' AND ('Worldwide'=ANY(rule.territories) OR ?::text=ANY(rule.territories))) OR (rule.territory_mode='exclude' AND ?::text IS NOT NULL AND NOT (?::text=ANY(rule.territories))))) ORDER BY candidate.published_at DESC LIMIT 1) public ON TRUE WHERE item.playlist_id=playlist.id),'[]'::jsonb)) FROM music_playlist playlist WHERE playlist.owner_party_id=? ORDER BY playlist.updated_at DESC,playlist.id"
    [optionalText territory,optionalText territory,optionalText territory,PersistInt64 (currentPartyId user)]

createPlaylist :: AuthedUser -> MusicPlaylistCreateRequest -> AppM Value
createPlaylist user MusicPlaylistCreateRequest{..} = do
  requirePublicFeature
  let name = T.strip musicPlaylistName
      visibility = T.toLower (T.strip musicPlaylistVisibility)
  unless (validRequiredText 200 name) (throwError (badRequest "Playlist name is required and limited to 200 characters"))
  unless (visibility `elem` ["private","unlisted","public"]) (throwError (badRequest "Playlist visibility is invalid"))
  jsonOne (conflict "Playlist could not be created")
    "INSERT INTO music_playlist(owner_party_id,name,visibility) VALUES(?,?,?) RETURNING jsonb_build_object('id',id,'name',name,'visibility',visibility,'createdAt',created_at,'updatedAt',updated_at)"
    [PersistInt64 (currentPartyId user), PersistText name, PersistText visibility]

updatePlaylist :: AuthedUser -> UUID -> MusicPlaylistCreateRequest -> AppM Value
updatePlaylist user playlistId MusicPlaylistCreateRequest{..} = do
  requirePublicFeature
  let name = T.strip musicPlaylistName
      visibility = T.toLower (T.strip musicPlaylistVisibility)
  unless (validRequiredText 200 name) (throwError (badRequest "Playlist name is required and limited to 200 characters"))
  unless (visibility `elem` ["private","unlisted","public"]) (throwError (badRequest "Playlist visibility is invalid"))
  jsonOne err404
    "UPDATE music_playlist SET name=?,visibility=?,updated_at=NOW() WHERE id=?::uuid AND owner_party_id=? RETURNING jsonb_build_object('id',id,'name',name,'visibility',visibility,'createdAt',created_at,'updatedAt',updated_at)"
    [PersistText name,PersistText visibility,toPersistValue playlistId,PersistInt64 (currentPartyId user)]

deletePlaylist :: AuthedUser -> UUID -> AppM NoContent
deletePlaylist user playlistId = do
  requirePublicFeature
  deleted <- runDB (rawSql
    "DELETE FROM music_playlist WHERE id=?::uuid AND owner_party_id=? RETURNING id"
    [toPersistValue playlistId,PersistInt64 (currentPartyId user)] :: SqlPersistT IO [Single UUID])
  unless (deleted == [Single playlistId]) (throwError err404)
  pure NoContent

addPlaylistItem :: AuthedUser -> UUID -> Maybe Text -> MusicPlaylistItemRequest -> AppM Value
addPlaylistItem user playlistId edgeCountry MusicPlaylistItemRequest{..} = do
  requirePublicFeature
  territory <- Assets.trustedEdgeTerritory edgeCountry
  allowed <- recordingIsPublic musicPlaylistRecordingId territory
  unless allowed (throwError err404)
  when (musicPlaylistPosition < 0 || musicPlaylistPosition > 9999) (throwError (badRequest "Playlist position is invalid"))
  rows <- runDB $ do
    owned <- rawSql "SELECT id FROM music_playlist WHERE id=?::uuid AND owner_party_id=? FOR UPDATE"
      [toPersistValue playlistId,PersistInt64 (currentPartyId user)] :: SqlPersistT IO [Single UUID]
    case owned of
      [] -> pure []
      _ -> do
        counts <- rawSql "SELECT count(*) FROM music_playlist_item WHERE playlist_id=?::uuid"
          [toPersistValue playlistId] :: SqlPersistT IO [Single Int64]
        let count = case counts of [Single value] -> value; _ -> 10000
            position = min (fromIntegral musicPlaylistPosition) count
        if count >= 10000
          then pure []
          else do
            rawExecute "UPDATE music_playlist_item SET position=position+1 WHERE playlist_id=?::uuid AND position>=?"
              [toPersistValue playlistId,PersistInt64 position]
            inserted <- rawSql
              "INSERT INTO music_playlist_item(playlist_id,recording_id,position,added_by) VALUES(?::uuid,?::uuid,?,?) RETURNING jsonb_build_object('id',id,'playlistId',playlist_id,'recordingId',recording_id,'position',position,'addedAt',added_at)"
              [toPersistValue playlistId,toPersistValue musicPlaylistRecordingId,PersistInt64 position,PersistInt64 (currentPartyId user)]
              :: SqlPersistT IO [Single CMS.AesonValue]
            rawExecute "UPDATE music_playlist SET updated_at=NOW() WHERE id=?::uuid" [toPersistValue playlistId]
            pure inserted
  maybe (throwError (conflict "Playlist item could not be added")) (pure . CMS.unAesonValue . unSingle) (listToMaybe rows)

movePlaylistItem :: AuthedUser -> UUID -> UUID -> MusicPlaylistMoveRequest -> AppM Value
movePlaylistItem user playlistId itemId MusicPlaylistMoveRequest{..} = do
  requirePublicFeature
  when (musicPlaylistMovePosition < 0 || musicPlaylistMovePosition > 9999) (throwError (badRequest "Playlist position is invalid"))
  rows <- runDB $ do
    current <- rawSql
      "SELECT item.position,(SELECT count(*) FROM music_playlist_item sibling WHERE sibling.playlist_id=item.playlist_id) FROM music_playlist_item item JOIN music_playlist playlist ON playlist.id=item.playlist_id WHERE item.id=?::uuid AND item.playlist_id=?::uuid AND playlist.owner_party_id=? FOR UPDATE OF playlist,item"
      [toPersistValue itemId,toPersistValue playlistId,PersistInt64 (currentPartyId user)]
      :: SqlPersistT IO [(Single Int64,Single Int64)]
    case current of
      [(Single oldPosition,Single count)] -> do
        let newPosition = min (fromIntegral musicPlaylistMovePosition) (max 0 (count-1))
        if newPosition < oldPosition
          then rawExecute "UPDATE music_playlist_item SET position=position+1 WHERE playlist_id=?::uuid AND position>=? AND position<?"
            [toPersistValue playlistId,PersistInt64 newPosition,PersistInt64 oldPosition]
          else when (newPosition > oldPosition) $ rawExecute "UPDATE music_playlist_item SET position=position-1 WHERE playlist_id=?::uuid AND position>? AND position<=?"
            [toPersistValue playlistId,PersistInt64 oldPosition,PersistInt64 newPosition]
        moved <- rawSql
          "UPDATE music_playlist_item SET position=? WHERE id=?::uuid AND playlist_id=?::uuid RETURNING jsonb_build_object('id',id,'playlistId',playlist_id,'recordingId',recording_id,'position',position,'addedAt',added_at)"
          [PersistInt64 newPosition,toPersistValue itemId,toPersistValue playlistId] :: SqlPersistT IO [Single CMS.AesonValue]
        rawExecute "UPDATE music_playlist SET updated_at=NOW() WHERE id=?::uuid" [toPersistValue playlistId]
        pure moved
      _ -> pure []
  maybe (throwError err404) (pure . CMS.unAesonValue . unSingle) (listToMaybe rows)

deletePlaylistItem :: AuthedUser -> UUID -> UUID -> AppM NoContent
deletePlaylistItem user playlistId itemId = do
  requirePublicFeature
  deleted <- runDB $ do
    current <- rawSql
      "SELECT item.position FROM music_playlist_item item JOIN music_playlist playlist ON playlist.id=item.playlist_id WHERE item.id=?::uuid AND item.playlist_id=?::uuid AND playlist.owner_party_id=? FOR UPDATE OF playlist,item"
      [toPersistValue itemId,toPersistValue playlistId,PersistInt64 (currentPartyId user)] :: SqlPersistT IO [Single Int64]
    case current of
      [Single position] -> do
        rawExecute "DELETE FROM music_playlist_item WHERE id=?::uuid AND playlist_id=?::uuid"
          [toPersistValue itemId,toPersistValue playlistId]
        rawExecute "UPDATE music_playlist_item SET position=position-1 WHERE playlist_id=?::uuid AND position>?"
          [toPersistValue playlistId,PersistInt64 position]
        rawExecute "UPDATE music_playlist SET updated_at=NOW() WHERE id=?::uuid" [toPersistValue playlistId]
        pure True
      _ -> pure False
  unless deleted (throwError err404)
  pure NoContent

recordAnonymousPlaybackEvent :: Maybe Text -> MusicPlaybackEventRequest -> AppM NoContent
recordAnonymousPlaybackEvent edgeCountry request = do
  requirePublicFeature
  anonymousId <- maybe (throwError (badRequest "anonymousId is required for unauthenticated playback events")) pure (musicEventAnonymousId request)
  unless (T.length (T.strip anonymousId) >= 16) (throwError (badRequest "anonymousId must be an opaque value of at least 16 characters"))
  territory <- trustedEdgeTerritory edgeCountry
  recordPlaybackEvent Nothing (Just (hashText anonymousId)) territory request

recordAuthenticatedPlaybackEvent :: AuthedUser -> Maybe Text -> MusicPlaybackEventRequest -> AppM NoContent
recordAuthenticatedPlaybackEvent user edgeCountry request = do
  territory <- trustedEdgeTerritory edgeCountry
  recordPlaybackEvent (Just (currentPartyId user)) Nothing territory request

recordPlaybackEvent :: Maybe Int64 -> Maybe Text -> Maybe Text -> MusicPlaybackEventRequest -> AppM NoContent
recordPlaybackEvent partyId anonymousHash trustedTerritory request@MusicPlaybackEventRequest{..} = do
  requirePublicFeature
  validatePlaybackEvent request
  accessRows <- runDB (rawSql
    "SELECT recording.duration_ms FROM music_recording recording JOIN music_release_track track ON track.recording_id=recording.id JOIN music_public_release public ON public.release_version_id=track.release_version_id WHERE recording.id=?::uuid AND public.release_version_id=?::uuid AND EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=public.release_version_id AND (rule.release_track_id IS NULL OR rule.release_track_id=track.id) AND rule.listening_policy<>'none' AND (rule.starts_at IS NULL OR rule.starts_at<=NOW()) AND (rule.ends_at IS NULL OR rule.ends_at>NOW()) AND ((rule.territory_mode='include' AND ('Worldwide'=ANY(rule.territories) OR ?::text=ANY(rule.territories))) OR (rule.territory_mode='exclude' AND NOT (?::text=ANY(rule.territories))))) LIMIT 1"
    [ toPersistValue musicEventRecordingId, toPersistValue musicEventReleaseVersionId
    , optionalText trustedTerritory, optionalText trustedTerritory
    ] :: SqlPersistT IO [Single Int64])
  _ <- case accessRows of
    [Single value] -> pure value
    _ -> throwError err404
  when (BL.length (encode musicEventMetadata) > 16384) $
    throwError (badRequest "Playback event metadata exceeds 16 KiB")
  resultRows <- runDB $ do
    rows <- rawSql
      "SELECT music_record_playback_event(?::uuid,?::uuid,?,?::bigint,?::text,?::uuid,?::uuid,?,?,?,?,?,?,?::jsonb)"
      [ toPersistValue musicEventId, toPersistValue musicEventSessionId, PersistInt64 (fromIntegral musicEventSequenceNumber)
      , optionalInt64 partyId, optionalText anonymousHash, toPersistValue musicEventReleaseVersionId
      , toPersistValue musicEventRecordingId, PersistText musicEventType, PersistInt64 musicEventPositionMs
      , PersistInt64 musicEventListenedDeltaMs, optionalText musicEventQuality, optionalText trustedTerritory
      , PersistUTCTime musicEventOccurredAt, PersistText (jsonText musicEventMetadata)
      ] :: SqlPersistT IO [Single Text]
    when (rows == [Single "inserted"]) $ case partyId of
      Nothing -> pure ()
      Just authenticatedParty -> rawExecute
        "INSERT INTO music_playback_history(party_id,recording_id,last_release_version_id,last_position_ms,play_count,last_played_at) VALUES(?,?::uuid,?::uuid,?,CASE WHEN ?='play_start' THEN 1 ELSE 0 END,?) ON CONFLICT(party_id,recording_id) DO UPDATE SET last_release_version_id=CASE WHEN EXCLUDED.last_played_at>=music_playback_history.last_played_at THEN EXCLUDED.last_release_version_id ELSE music_playback_history.last_release_version_id END,last_position_ms=CASE WHEN EXCLUDED.last_played_at>=music_playback_history.last_played_at THEN EXCLUDED.last_position_ms ELSE music_playback_history.last_position_ms END,play_count=music_playback_history.play_count+EXCLUDED.play_count,last_played_at=GREATEST(music_playback_history.last_played_at,EXCLUDED.last_played_at)"
        [ PersistInt64 authenticatedParty, toPersistValue musicEventRecordingId, toPersistValue musicEventReleaseVersionId
        , PersistInt64 musicEventPositionMs, PersistText musicEventType, PersistUTCTime musicEventOccurredAt
        ]
    pure rows
  case resultRows of
    [Single "inserted"] -> pure NoContent
    [Single "duplicate"] -> pure NoContent
    [Single "conflict"] -> throwError err409
      { errBody = encode (object
          [ "code" .= ("playback_identity_conflict" :: Text)
          , "message" .= ("La sesión, secuencia o evento pertenece a otra solicitud. Al cambiar de identidad usa una sesión nueva; un reintento debe conservar el evento original." :: Text)
          ])
      , errHeaders = [("Content-Type","application/json")]
      }
    _ -> throwError err500 {errBody="Playback event could not be recorded"}

-- The client-reported country remains analytics metadata only. Access uses
-- the edge-derived value, and defaults to Worldwide-only when no trusted edge
-- is configured.
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

listPlaybackHistory :: AuthedUser -> Maybe Text -> AppM [Value]
listPlaybackHistory user edgeCountry = do
  requirePublicFeature
  territory <- Assets.trustedEdgeTerritory edgeCountry
  jsonRows
    "SELECT jsonb_build_object('recordingId',history.recording_id,'trackId',public.resolved_track_id,'releaseVersionId',public.release_version_id,'positionMs',history.last_position_ms,'playCount',history.play_count,'lastPlayedAt',history.last_played_at,'title',recording.canonical_title,'durationMs',recording.duration_ms,'available',public.release_version_id IS NOT NULL,'releaseId',public.id,'slug',public.canonical_slug,'displayArtist',public.display_artist,'sources',COALESCE((SELECT jsonb_agg(jsonb_build_object('assetId',asset.id,'role',asset.asset_role,'mediaType',asset.media_type,'technicalMetadata',asset.technical_metadata) ORDER BY asset.asset_role,asset.created_at) FROM music_asset asset WHERE asset.release_version_id=public.release_version_id AND asset.recording_id=recording.id AND asset.asset_role IN ('stream_audio','preview_audio') AND asset.processing_state='ready' AND (asset.asset_role<>'preview_audio' OR music_preview_matches(asset.id))),'[]'::jsonb)) FROM music_playback_history history JOIN music_recording recording ON recording.id=history.recording_id LEFT JOIN LATERAL (SELECT candidate.*,track.id AS resolved_track_id FROM music_public_release candidate JOIN music_release_track track ON track.release_version_id=candidate.release_version_id WHERE track.recording_id=recording.id AND EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=candidate.release_version_id AND (rule.release_track_id IS NULL OR rule.release_track_id=track.id) AND (rule.starts_at IS NULL OR rule.starts_at<=NOW()) AND (rule.ends_at IS NULL OR rule.ends_at>NOW()) AND rule.listening_policy<>'none' AND ((rule.territory_mode='include' AND ('Worldwide'=ANY(rule.territories) OR ?::text=ANY(rule.territories))) OR (rule.territory_mode='exclude' AND ?::text IS NOT NULL AND NOT (?::text=ANY(rule.territories))))) ORDER BY (candidate.release_version_id=history.last_release_version_id) DESC,candidate.published_at DESC LIMIT 1) public ON TRUE WHERE history.party_id=? ORDER BY history.last_played_at DESC LIMIT 200"
    [optionalText territory,optionalText territory,optionalText territory,PersistInt64 (currentPartyId user)]

createInfringementReport :: AuthedUser -> Maybe Text -> MusicInfringementReportRequest -> AppM Value
createInfringementReport user idempotencyHeader MusicInfringementReportRequest{..} = do
  requirePublicFeature
  idempotencyKey <- requireIdempotencyKey idempotencyHeader
  let reason = T.toLower (T.strip musicReportReasonCode)
      description = T.strip musicReportDescription
      actor = currentPartyId user
  unless (reason `elem` ["copyright","master_rights","composition_rights","impersonation","metadata","other"]) $
    throwError (badRequest "Unsupported infringement reason code")
  unless (validRequiredText 10000 description) $
    throwError (badRequest "Report description is required and limited to 10000 characters")
  outcome <- runDB $ do
    _ <- rawSql
      "SELECT 1::bigint FROM (SELECT pg_advisory_xact_lock(hashtextextended(?,0))) locked"
      [PersistText (T.pack (show actor) <> ":" <> idempotencyKey)] :: SqlPersistT IO [Single Int64]
    existing <- rawSql
      "SELECT jsonb_build_object('id',id,'releaseId',release_id,'reasonCode',reason_code,'description',description,'status',status,'createdAt',created_at,'updatedAt',updated_at) FROM music_infringement_report WHERE reporter_party_id=? AND idempotency_key=?"
      [PersistInt64 actor,PersistText idempotencyKey] :: SqlPersistT IO [Single CMS.AesonValue]
    case existing of
      value : _ -> pure (Just (CMS.unAesonValue (unSingle value)))
      [] -> do
        inserted <- rawSql
          "INSERT INTO music_infringement_report(release_id,reporter_party_id,reason_code,description,idempotency_key) SELECT public.id,?,?,?,? FROM music_public_release public WHERE public.id=?::uuid LIMIT 1 RETURNING id,release_id,reason_code,description,status,created_at,updated_at"
          [PersistInt64 actor,PersistText reason,PersistText description,PersistText idempotencyKey,toPersistValue musicReportReleaseId]
          :: SqlPersistT IO [(Single UUID,Single UUID,Single Text,Single Text,Single Text,Single UTCTime,Single UTCTime)]
        case inserted of
          [(Single reportId,Single reportReleaseId,Single storedReason,Single storedDescription,Single storedStatus,Single createdAt,Single updatedAt)] -> do
            rawExecute
              "INSERT INTO music_release_audit_event(release_id,actor_party_id,event_type,idempotency_key,data) VALUES(?::uuid,?,'infringement_received',?,jsonb_build_object('report_id',?::uuid,'reason_code',?::text))"
              [toPersistValue reportReleaseId,PersistInt64 actor,PersistText idempotencyKey,toPersistValue reportId,PersistText storedReason]
            pure (Just (object ["id" .= reportId,"releaseId" .= reportReleaseId,"reasonCode" .= storedReason,"description" .= storedDescription,"status" .= storedStatus,"createdAt" .= createdAt,"updatedAt" .= updatedAt]))
          _ -> pure Nothing
  maybe (throwError err404) pure outcome

listInfringementReports :: AuthedUser -> Maybe UUID -> AppM [Value]
listInfringementReports user releaseId = do
  requireFeature "music_releases.authoring"
  unless (hasStrictAdminAccess user) (throwError err403)
  jsonRows
    "SELECT jsonb_build_object('id',report.id,'releaseId',report.release_id,'reasonCode',report.reason_code,'description',report.description,'status',report.status,'reporterPartyId',report.reporter_party_id,'assignedTo',report.assigned_to,'resolutionNotes',report.resolution_notes,'createdAt',report.created_at,'updatedAt',report.updated_at,'resolvedAt',report.resolved_at,'releaseTitle',version.title,'publishedVersionId',release.published_version_id) FROM music_infringement_report report JOIN music_release release ON release.id=report.release_id LEFT JOIN music_release_version version ON version.id=release.published_version_id WHERE (?::uuid IS NULL OR report.release_id=?::uuid) ORDER BY report.created_at DESC,report.id"
    [optionalUuid releaseId,optionalUuid releaseId]

actionInfringementReport :: AuthedUser -> UUID -> MusicInfringementActionRequest -> AppM Value
actionInfringementReport user reportId MusicInfringementActionRequest{..} = do
  requireFeature "music_releases.authoring"
  unless (hasStrictAdminAccess user) (throwError err403)
  let target = T.toLower (T.strip musicReportStatus)
      notes = T.strip musicReportNotes
      actor = currentPartyId user
  unless (target `elem` ["triage","investigating","actioned","dismissed"]) $
    throwError (badRequest "Unsupported infringement status")
  unless (validRequiredText 10000 notes) $
    throwError (badRequest "Resolution notes are required and limited to 10000 characters")
  when (musicReportSuspendVersionId /= Nothing && target /= "actioned") $
    throwError (badRequest "A suspension can only accompany an actioned report")
  current <- runDB (rawSql "SELECT status FROM music_infringement_report WHERE id=?::uuid"
    [toPersistValue reportId] :: SqlPersistT IO [Single Text])
  prior <- case current of [Single value] -> pure value; _ -> throwError err404
  unless (validInfringementTransition prior target) $
    throwError (conflict "Invalid infringement status transition")
  jsonOne (conflict "The report changed or the selected release version cannot be suspended")
    "WITH report_target AS (SELECT id,release_id FROM music_infringement_report WHERE id=?::uuid AND status=? FOR UPDATE), suspended AS (UPDATE music_release_version version SET state='suspended',updated_at=NOW() FROM report_target WHERE ?::uuid IS NOT NULL AND version.id=?::uuid AND version.release_id=report_target.release_id AND version.state IN ('in_review','approved','scheduled','published') RETURNING version.id), updated AS (UPDATE music_infringement_report report SET status=?,assigned_to=?,resolution_notes=?,resolved_at=CASE WHEN ? IN ('actioned','dismissed') THEN NOW() ELSE NULL END FROM report_target WHERE report.id=report_target.id AND (?::uuid IS NULL OR EXISTS(SELECT 1 FROM suspended)) RETURNING report.*), audit AS (INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,data) SELECT updated.release_id,?::uuid,?,'infringement_' || updated.status,jsonb_build_object('report_id',updated.id,'notes',updated.resolution_notes) FROM updated RETURNING id) SELECT jsonb_build_object('id',id,'releaseId',release_id,'reasonCode',reason_code,'description',description,'status',status,'reporterPartyId',reporter_party_id,'assignedTo',assigned_to,'resolutionNotes',resolution_notes,'createdAt',created_at,'updatedAt',updated_at,'resolvedAt',resolved_at) FROM updated"
    [ toPersistValue reportId,PersistText prior,optionalUuid musicReportSuspendVersionId
    , optionalUuid musicReportSuspendVersionId,PersistText target,PersistInt64 actor,PersistText notes
    , PersistText target,optionalUuid musicReportSuspendVersionId,optionalUuid musicReportSuspendVersionId
    , PersistInt64 actor
    ]

validInfringementTransition :: Text -> Text -> Bool
validInfringementTransition prior target = (prior,target) `elem`
  [ ("received","triage"),("received","dismissed")
  , ("triage","investigating"),("triage","actioned"),("triage","dismissed")
  , ("investigating","actioned"),("investigating","dismissed")
  ]

releaseAnalytics :: AuthedUser -> UUID -> Maybe Day -> Maybe Day -> AppM Value
releaseAnalytics user releaseId fromDay toDay = do
  requireFeature "music_releases.authoring"
  requireOwnerPermission user releaseId "release.analytics"
  when (maybe False id ((>) <$> fromDay <*> toDay)) $
    throwError (badRequest "Analytics from date must not be after to date")
  jsonOne err404
    "WITH bounds AS (SELECT ?::date AS from_day,?::date AS to_day), metrics AS MATERIALIZED (SELECT metric.* FROM music_daily_metric metric JOIN music_release_version version ON version.id=metric.release_version_id CROSS JOIN bounds WHERE version.release_id=?::uuid AND (bounds.from_day IS NULL OR metric.metric_date>=bounds.from_day) AND (bounds.to_day IS NULL OR metric.metric_date<=bounds.to_day)) SELECT jsonb_build_object('releaseId',release.id,'disclaimer','Operational analytics only; not certified royalty accounting. Low-volume territories are suppressed below five aggregated listeners.','totals',jsonb_build_object('playStarts',COALESCE((SELECT SUM(play_starts) FROM metrics),0),'eligiblePlays',COALESCE((SELECT SUM(eligible_plays) FROM metrics),0),'completions',COALESCE((SELECT SUM(completions) FROM metrics),0),'skips',COALESCE((SELECT SUM(skips) FROM metrics),0),'listenedMs',COALESCE((SELECT SUM(listened_ms) FROM metrics),0),'uniqueListeners',COALESCE((SELECT SUM(unique_listeners) FROM metrics),0),'purchases',COALESCE((SELECT SUM(purchases) FROM metrics),0),'downloads',COALESCE((SELECT SUM(downloads) FROM metrics),0)),'daily',COALESCE((SELECT jsonb_agg(jsonb_build_object('date',daily.metric_date,'playStarts',daily.play_starts,'eligiblePlays',daily.eligible_plays,'completions',daily.completions,'skips',daily.skips,'listenedMs',daily.listened_ms,'uniqueListeners',daily.unique_listeners,'purchases',daily.purchases,'downloads',daily.downloads) ORDER BY daily.metric_date) FROM (SELECT metric_date,SUM(play_starts) AS play_starts,SUM(eligible_plays) AS eligible_plays,SUM(completions) AS completions,SUM(skips) AS skips,SUM(listened_ms) AS listened_ms,SUM(unique_listeners) AS unique_listeners,SUM(purchases) AS purchases,SUM(downloads) AS downloads FROM metrics GROUP BY metric_date) daily),'[]'::jsonb),'tracks',COALESCE((SELECT jsonb_agg(jsonb_build_object('recordingId',track.recording_id,'title',track.canonical_title,'playStarts',track.play_starts,'eligiblePlays',track.eligible_plays,'completions',track.completions,'skips',track.skips,'listenedMs',track.listened_ms,'uniqueListeners',track.unique_listeners,'purchases',track.purchases,'downloads',track.downloads) ORDER BY track.disc_number,track.track_number) FROM (SELECT recording.id AS recording_id,recording.canonical_title,MIN(release_track.disc_number) AS disc_number,MIN(release_track.track_number) AS track_number,SUM(metrics.play_starts) AS play_starts,SUM(metrics.eligible_plays) AS eligible_plays,SUM(metrics.completions) AS completions,SUM(metrics.skips) AS skips,SUM(metrics.listened_ms) AS listened_ms,SUM(metrics.unique_listeners) AS unique_listeners,SUM(metrics.purchases) AS purchases,SUM(metrics.downloads) AS downloads FROM metrics JOIN music_recording recording ON recording.id=metrics.recording_id JOIN music_release_track release_track ON release_track.recording_id=recording.id AND release_track.release_version_id=metrics.release_version_id GROUP BY recording.id,recording.canonical_title) track),'[]'::jsonb),'territories',COALESCE((SELECT jsonb_agg(jsonb_build_object('territoryCode',territory.territory_code,'playStarts',territory.play_starts,'eligiblePlays',territory.eligible_plays,'completions',territory.completions,'skips',territory.skips,'listenedMs',territory.listened_ms,'uniqueListeners',territory.unique_listeners,'purchases',territory.purchases,'downloads',territory.downloads) ORDER BY territory.eligible_plays DESC,territory.territory_code) FROM (SELECT territory_code,SUM(play_starts) AS play_starts,SUM(eligible_plays) AS eligible_plays,SUM(completions) AS completions,SUM(skips) AS skips,SUM(listened_ms) AS listened_ms,SUM(unique_listeners) AS unique_listeners,SUM(purchases) AS purchases,SUM(downloads) AS downloads FROM metrics GROUP BY territory_code HAVING territory_code='ZZ' OR SUM(unique_listeners)>=5) territory),'[]'::jsonb)) FROM music_release release WHERE release.id=?::uuid"
    [optionalDay fromDay, optionalDay toDay, toPersistValue releaseId, toPersistValue releaseId]

recordingIsPublic :: UUID -> Maybe Text -> AppM Bool
recordingIsPublic recordingId territory = booleanQuery
  "SELECT EXISTS(SELECT 1 FROM music_release_track track JOIN music_public_release public ON public.release_version_id=track.release_version_id WHERE track.recording_id=?::uuid AND EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=public.release_version_id AND (rule.release_track_id IS NULL OR rule.release_track_id=track.id) AND (rule.starts_at IS NULL OR rule.starts_at<=NOW()) AND (rule.ends_at IS NULL OR rule.ends_at>NOW()) AND rule.listening_policy<>'none' AND ((rule.territory_mode='include' AND ('Worldwide'=ANY(rule.territories) OR ?::text=ANY(rule.territories))) OR (rule.territory_mode='exclude' AND ?::text IS NOT NULL AND NOT (?::text=ANY(rule.territories))))))"
  [toPersistValue recordingId,optionalText territory,optionalText territory,optionalText territory]

releaseVersionJson :: UUID -> UUID -> AppM Value
releaseVersionJson releaseId versionId = jsonOne err404
  "SELECT jsonb_build_object('id',version.id,'releaseId',version.release_id,'versionNumber',version.version_number,'state',version.state,'title',version.title,'subtitle',version.subtitle,'versionTitle',version.version_title,'displayArtist',version.display_artist,'titleLanguage',version.title_language,'titleScript',version.title_script,'primaryGenreId',version.primary_genre_id,'secondaryGenreId',version.secondary_genre_id,'explicitContent',version.explicit_content,'originalReleaseDate',version.original_release_date,'releaseAtUtc',version.release_at_utc,'releaseTimezone',version.release_timezone,'embargoUntilUtc',version.embargo_until_utc,'takedownAtUtc',version.takedown_at_utc,'takedownTimezone',version.takedown_timezone,'labelName',version.label_name,'catalogNumber',version.catalog_number,'metadataValid',version.metadata_valid,'assetsValid',version.assets_valid,'rightsValid',version.rights_valid,'accessValid',version.access_valid,'approvedAt',version.approved_at,'publishedAt',version.published_at,'createdAt',version.created_at,'updatedAt',version.updated_at) FROM music_release_version version WHERE version.id=?::uuid AND version.release_id=?::uuid"
  [toPersistValue versionId, toPersistValue releaseId]

canonicalSnapshot :: UUID -> UUID -> SqlPersistT IO (Maybe Value)
canonicalSnapshot releaseId versionId = fmap (fmap (CMS.unAesonValue . unSingle) . listToMaybe) $ rawSql
  "SELECT jsonb_build_object('schemaVersion',2,'parties',music_version_parties(version.id),'release',to_jsonb(release),'version',to_jsonb(version),'tracks',COALESCE((SELECT jsonb_agg(to_jsonb(track) ORDER BY track.disc_number,track.track_number) FROM music_release_track track WHERE track.release_version_id=version.id),'[]'::jsonb),'recordings',COALESCE((SELECT jsonb_agg(DISTINCT to_jsonb(recording)) FROM music_release_track track JOIN music_recording recording ON recording.id=track.recording_id WHERE track.release_version_id=version.id),'[]'::jsonb),'credits',COALESCE((SELECT jsonb_agg(to_jsonb(credit) ORDER BY credit.display_order,credit.id) FROM music_credit credit WHERE credit.release_version_id=version.id),'[]'::jsonb),'rights',COALESCE((SELECT jsonb_agg(to_jsonb(rights)) FROM music_rights_declaration rights WHERE rights.release_version_id=version.id),'[]'::jsonb),'splits',COALESCE((SELECT jsonb_agg(to_jsonb(split)) FROM music_rights_declaration rights JOIN music_rights_split split ON split.declaration_id=rights.id WHERE rights.release_version_id=version.id),'[]'::jsonb),'availability',COALESCE((SELECT jsonb_agg(to_jsonb(rule)) FROM music_availability_rule rule WHERE rule.release_version_id=version.id),'[]'::jsonb),'identifiers',COALESCE((SELECT jsonb_agg(to_jsonb(identifier)) FROM music_identifier identifier WHERE identifier.release_version_id=version.id OR identifier.recording_id IN (SELECT track.recording_id FROM music_release_track track WHERE track.release_version_id=version.id)),'[]'::jsonb),'assets',COALESCE((SELECT jsonb_agg(to_jsonb(asset)-'object_key'-'bucket_name') FROM music_asset asset WHERE asset.release_version_id=version.id),'[]'::jsonb)) FROM music_release release JOIN music_release_version version ON version.release_id=release.id WHERE release.id=?::uuid AND version.id=?::uuid"
  [toPersistValue releaseId, toPersistValue versionId]

validateCanonicalGenre :: UUID -> AppM ()
validateCanonicalGenre genreId = do
  valid <- booleanQuery
    "SELECT EXISTS(SELECT 1 FROM genre item JOIN catalog_definition catalog ON catalog.id=item.catalog_id WHERE item.id=?::uuid AND catalog.code='genres' AND catalog.active AND item.active AND item.deprecated_at IS NULL AND item.workflow_state_id IN (SELECT state.id FROM workflow_state state WHERE state.code IN ('published','approved','active')))"
    [toPersistValue genreId]
  unless valid (throwError (badRequest "Genre IDs must reference active published items in the canonical genres catalog"))

ensureVersion :: UUID -> UUID -> AppM ()
ensureVersion releaseId versionId = do
  exists <- booleanQuery "SELECT EXISTS(SELECT 1 FROM music_release_version WHERE id=?::uuid AND release_id=?::uuid)"
    [toPersistValue versionId, toPersistValue releaseId]
  unless exists (throwError err404)

appendAudit :: UUID -> Maybe UUID -> Maybe Int64 -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AppM ()
appendAudit releaseId versionId actor eventType idempotencyKey priorState nextState reason =
  runDB $ rawExecute
    "INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,idempotency_key,prior_state,next_state,data) VALUES(?::uuid,?::uuid,?::bigint,?,?,?,?,jsonb_build_object('reason',?::text)) ON CONFLICT DO NOTHING"
    [ toPersistValue releaseId, optionalUuid versionId, optionalInt64 actor, PersistText eventType
    , optionalText idempotencyKey, optionalText priorState, optionalText nextState, optionalText reason
    ]

booleanQuery :: Text -> [PersistValue] -> AppM Bool
booleanQuery statement params = do
  rows <- runDB (rawSql statement params :: SqlPersistT IO [Single Bool])
  pure (rows == [Single True])

hashText :: Text -> Text
hashText value = T.pack (show (hash (TE.encodeUtf8 value) :: Digest SHA256))

hashValue :: Value -> Text
hashValue value = T.pack (show (hash (BL.toStrict (encode value)) :: Digest SHA256))

jsonText :: ToJSON value => value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode

optionalText :: Maybe Text -> PersistValue
optionalText = maybe PersistNull PersistText

optionalInt64 :: Maybe Int64 -> PersistValue
optionalInt64 = maybe PersistNull PersistInt64

optionalUuid :: Maybe UUID -> PersistValue
optionalUuid = maybe PersistNull toPersistValue

optionalTime :: Maybe UTCTime -> PersistValue
optionalTime = maybe PersistNull PersistUTCTime

optionalDay :: Maybe Day -> PersistValue
optionalDay = maybe PersistNull PersistDay

validSlug :: Text -> Bool
validSlug slug =
  not (T.null slug)
    && T.length slug <= 160
    && T.all (\character -> (character >= 'a' && character <= 'z') || (character >= '0' && character <= '9') || character == '-') slug
    && T.head slug /= '-'
    && T.last slug /= '-'
    && not ("--" `T.isInfixOf` slug)

validRequiredText :: Int -> Text -> Bool
validRequiredText maximumLength value = not (T.null value) && T.length value <= maximumLength

validatePlaybackEvent :: MusicPlaybackEventRequest -> AppM ()
validatePlaybackEvent MusicPlaybackEventRequest{..} = do
  unless (musicEventType `elem` ["play_start","progress","pause","seek","complete","skip","error","buffering","quality_selected"]) $
    throwError (badRequest "Unsupported playback event type")
  when (musicEventSequenceNumber < 0 || musicEventPositionMs < 0 || musicEventListenedDeltaMs < 0 || musicEventListenedDeltaMs > 30000) $
    throwError (badRequest "Playback event counters are outside the accepted range")

allStates :: [Text]
allStates =
  [ "draft","uploading","processing","validation_failed","ready_for_review","in_review"
  , "changes_requested","approved","scheduled","published","suspended","cancelled"
  , "replacement_pending","takedown_scheduled","withdrawn"
  ]
