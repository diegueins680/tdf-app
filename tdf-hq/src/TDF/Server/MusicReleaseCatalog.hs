{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.MusicReleaseCatalog
  ( getMusicReleaseVersion
  , replaceMusicReleaseContent
  , acceptMusicReleaseTerms
  ) where

import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (ToJSON, Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..), toPersistValue)
import Database.Persist.Sql (Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool)
import Servant
import System.Environment (lookupEnv)

import TDF.API.MusicRelease
import TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
import qualified TDF.CMS.Models as CMS
import TDF.DB (Env(..))
import TDF.MusicRelease.Domain
  ( IdentifierType(..)
  , validateIdentifier
  )
import qualified TDF.MusicRelease.ContentValidation as ContentValidation

type AppM = ReaderT Env Handler

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

getMusicReleaseVersion :: AuthedUser -> UUID -> UUID -> AppM Value
getMusicReleaseVersion user releaseId versionId = do
  requireFeature "music_releases.authoring"
  requireOwnerPermission user releaseId "release.read"
  contentJson releaseId versionId

replaceMusicReleaseContent
  :: AuthedUser -> UUID -> UUID -> MusicReleaseContentRequest -> AppM Value
replaceMusicReleaseContent user releaseId versionId request@MusicReleaseContentRequest{..} = do
  requireFeature "music_releases.authoring"
  requireOwnerPermission user releaseId "release.edit"
  either (throwError . badRequest) pure (ContentValidation.validateMusicReleaseContent request)
  validateExistingReferences user releaseId versionId request

  recordingIds <- forM musicContentTracks $ \track ->
    maybe (liftIO nextRandom) pure (musicTrackRecordingId track)
  candidatePartyIds <- forM musicContentParties $ \party ->
    maybe (liftIO nextRandom) pure (musicPartyId party)
  declarationIds <- mapM (const (liftIO nextRandom)) musicContentRightsDeclarations

  changed <- runDB (replaceContentDB
    (currentPartyId user) releaseId versionId request recordingIds candidatePartyIds declarationIds)
  unless changed $ throwError (conflict "Draft changed, is no longer editable, or expectedUpdatedAt is stale")
  contentJson releaseId versionId

acceptMusicReleaseTerms
  :: AuthedUser -> UUID -> UUID -> MusicTermsAcceptanceRequest -> AppM Value
acceptMusicReleaseTerms user releaseId versionId MusicTermsAcceptanceRequest{..} = do
  requireFeature "music_releases.authoring"
  requireOwnerPermission user releaseId "release.submit"
  let termsKind = T.toLower (T.strip musicTermsKind)
      termsVersion = T.strip musicTermsVersion
  unless musicTermsAccepted (throwError (badRequest "accepted must be true to record legal acceptance"))
  unless (termsKind `elem` ["publication_authority","distribution","privacy"]) $
    throwError (badRequest "termsKind must be publication_authority, distribution, or privacy")
  unless (validRequiredText 160 termsVersion) $
    throwError (badRequest "termsVersion is required and limited to 160 characters")
  when (BL.length (encode musicTermsEvidence) > 16384) $
    throwError (badRequest "terms evidence exceeds 16 KiB")
  rows <- runDB (rawSql
    "WITH locked AS (SELECT id FROM music_release_version WHERE id=?::uuid AND release_id=?::uuid AND state IN ('draft','uploading','processing','validation_failed','ready_for_review','changes_requested') FOR UPDATE), inserted AS (INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence) SELECT id,?,?,?,?::jsonb FROM locked ON CONFLICT(release_version_id,terms_kind,terms_version,accepted_by) DO UPDATE SET evidence=music_terms_acceptance.evidence RETURNING *) SELECT jsonb_build_object('id',id,'releaseVersionId',release_version_id,'termsKind',terms_kind,'termsVersion',terms_version,'acceptedBy',accepted_by,'acceptedAt',accepted_at,'evidence',evidence) FROM inserted"
    [ toPersistValue versionId, toPersistValue releaseId, PersistText termsKind
    , PersistText termsVersion, PersistInt64 (currentPartyId user), PersistText (jsonText musicTermsEvidence)
    ] :: SqlPersistT IO [Single CMS.AesonValue])
  value <- maybe (throwError (conflict "Release version is not editable"))
    (pure . CMS.unAesonValue . unSingle) (listToMaybe rows)
  runDB $ rawExecute
    "INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,data) VALUES(?::uuid,?::uuid,?,'terms_accepted',jsonb_build_object('terms_kind',?::text,'terms_version',?::text))"
    [toPersistValue releaseId,toPersistValue versionId,PersistInt64 (currentPartyId user),PersistText termsKind,PersistText termsVersion]
  pure value

replaceContentDB
  :: Int64
  -> UUID
  -> UUID
  -> MusicReleaseContentRequest
  -> [UUID]
  -> [UUID]
  -> [UUID]
  -> SqlPersistT IO Bool
replaceContentDB actor releaseId versionId MusicReleaseContentRequest{..} recordingIds candidatePartyIds declarationIds = do
  locked <- rawSql
    "UPDATE music_release_version SET metadata_valid=FALSE,rights_valid=FALSE,access_valid=FALSE,updated_at=NOW() WHERE id=?::uuid AND release_id=?::uuid AND updated_at=? AND state IN ('draft','uploading','processing','validation_failed','changes_requested') RETURNING id"
    [toPersistValue versionId,toPersistValue releaseId,PersistUTCTime musicContentExpectedUpdatedAt]
    :: SqlPersistT IO [Single UUID]
  case locked of
    [Single _] -> do
      partyMap <- foldM (upsertParty actor versionId) Map.empty (zip musicContentParties candidatePartyIds)
      rawExecute
        "DELETE FROM music_release_version_party WHERE release_version_id=?::uuid AND NOT (music_party_id=ANY(?::uuid[]))"
        [toPersistValue versionId,postgresUuidArray (Map.elems partyMap)]
      rawExecute
        "DELETE FROM music_identifier WHERE release_version_id=?::uuid OR recording_id IN (SELECT recording_id FROM music_release_track WHERE release_version_id=?::uuid)"
        [toPersistValue versionId,toPersistValue versionId]
      rawExecute "DELETE FROM music_credit WHERE release_version_id=?::uuid" [toPersistValue versionId]
      rawExecute "DELETE FROM music_rights_split WHERE declaration_id IN (SELECT id FROM music_rights_declaration WHERE release_version_id=?::uuid)" [toPersistValue versionId]
      rawExecute "DELETE FROM music_rights_declaration WHERE release_version_id=?::uuid" [toPersistValue versionId]
      rawExecute "DELETE FROM music_availability_rule WHERE release_version_id=?::uuid" [toPersistValue versionId]
      rawExecute "DELETE FROM music_release_track WHERE release_version_id=?::uuid" [toPersistValue versionId]

      forM_ (zip musicContentTracks recordingIds) $ \(track, recordingId) -> do
        case musicTrackRecordingId track of
          Just _ -> rawExecute
            "UPDATE music_recording SET canonical_title=?,subtitle=?,version_title=?,title_language=?,title_script=?,explicit_content=? WHERE id=?::uuid"
            [ PersistText (T.strip (musicTrackTitle track)), optionalText (musicTrackSubtitle track)
            , optionalText (musicTrackVersionTitle track), PersistText (T.strip (musicTrackTitleLanguage track))
            , optionalText (musicTrackTitleScript track), PersistText (musicTrackExplicitContent track)
            , toPersistValue recordingId
            ]
          Nothing -> rawExecute
            "INSERT INTO music_recording(id,canonical_title,subtitle,version_title,title_language,title_script,explicit_content,created_by) VALUES(?::uuid,?,?,?,?,?,?,?)"
            [ toPersistValue recordingId, PersistText (T.strip (musicTrackTitle track)), optionalText (musicTrackSubtitle track)
            , optionalText (musicTrackVersionTitle track), PersistText (T.strip (musicTrackTitleLanguage track))
            , optionalText (musicTrackTitleScript track), PersistText (musicTrackExplicitContent track), PersistInt64 actor
            ]
        rawExecute
          "INSERT INTO music_release_track(release_version_id,recording_id,disc_number,track_number,display_artist,is_primary_resource,preview_start_ms,preview_duration_ms) VALUES(?::uuid,?::uuid,?,?,?,?,?::bigint,?::bigint)"
          [ toPersistValue versionId,toPersistValue recordingId
          , PersistInt64 (fromIntegral (musicTrackDiscNumber track))
          , PersistInt64 (fromIntegral (musicTrackTrackNumber track))
          , PersistText (T.strip (musicTrackDisplayArtist track)),PersistBool (musicTrackIsPrimaryResource track)
          , optionalInt64 (musicTrackPreviewStartMs track),optionalInt64 (musicTrackPreviewDurationMs track)
          ]

      let trackMap = Map.fromList (zip (map musicTrackClientRef musicContentTracks) recordingIds)
      forM_ musicContentIdentifiers (insertIdentifier versionId trackMap)
      forM_ musicContentCredits (insertCredit versionId trackMap partyMap)
      forM_ (zip musicContentRightsDeclarations declarationIds) (insertRights actor versionId trackMap partyMap)
      forM_ musicContentAvailability (insertAvailability versionId trackMap)
      _ <- rawSql "SELECT metadata_valid,assets_valid,rights_valid,access_valid FROM music_refresh_validation_flags(?::uuid)"
        [toPersistValue versionId] :: SqlPersistT IO [(Single Bool,Single Bool,Single Bool,Single Bool)]
      rawExecute
        "INSERT INTO music_release_audit_event(release_id,release_version_id,actor_party_id,event_type,data) VALUES(?::uuid,?::uuid,?,'catalog_content_replaced',jsonb_build_object('parties_snapshot',music_version_parties(?::uuid),'tracks',?::integer,'parties',?::integer,'credits',?::integer,'rights_declarations',?::integer,'availability_rules',?::integer))"
        [ toPersistValue releaseId,toPersistValue versionId,PersistInt64 actor
        , toPersistValue versionId
        , PersistInt64 (fromIntegral (length musicContentTracks))
        , PersistInt64 (fromIntegral (length musicContentParties))
        , PersistInt64 (fromIntegral (length musicContentCredits))
        , PersistInt64 (fromIntegral (length musicContentRightsDeclarations))
        , PersistInt64 (fromIntegral (length musicContentAvailability))
        ]
      pure True
    _ -> pure False

upsertParty
  :: Int64 -> UUID -> Map.Map Text UUID -> (MusicPartyDraft,UUID)
  -> SqlPersistT IO (Map.Map Text UUID)
upsertParty actor versionId accumulated (party,candidateId) = do
  resolved <- case musicPartyId party of
    Just existing -> pure existing
    Nothing -> case musicPartyTdfPartyId party of
      Just tdfPartyId -> do
        rows <- rawSql
          "INSERT INTO music_party(id,tdf_party_id,display_name,legal_name,party_kind,created_by) VALUES(?::uuid,?,?,?,?,?) ON CONFLICT(tdf_party_id) WHERE tdf_party_id IS NOT NULL DO UPDATE SET display_name=music_party.display_name RETURNING id"
          [toPersistValue candidateId,PersistInt64 tdfPartyId,PersistText (T.strip (musicPartyDisplayName party)),optionalText (musicPartyLegalName party),PersistText (musicPartyKind party),PersistInt64 actor]
          :: SqlPersistT IO [Single UUID]
        singleUuid "party" rows
      Nothing -> do
        rows <- rawSql
          "INSERT INTO music_party(id,display_name,legal_name,party_kind,created_by) VALUES(?::uuid,?,?,?,?) RETURNING id"
          [toPersistValue candidateId,PersistText (T.strip (musicPartyDisplayName party)),optionalText (musicPartyLegalName party),PersistText (musicPartyKind party),PersistInt64 actor]
          :: SqlPersistT IO [Single UUID]
        singleUuid "party" rows
  let identifiers = map (partyIdentifierJson resolved) (musicPartyIdentifiers party)
  rawExecute
    "INSERT INTO music_release_version_party(release_version_id,music_party_id,party_details,details_source) VALUES(?::uuid,?::uuid,jsonb_build_object('displayName',?::text,'legalName',?::text,'partyKind',?::text,'identifiers',music_merge_party_identifiers((SELECT party_details->'identifiers' FROM music_release_version_party WHERE release_version_id=?::uuid AND music_party_id=?::uuid),?::jsonb)),'user_provided') ON CONFLICT(release_version_id,music_party_id) DO UPDATE SET party_details=EXCLUDED.party_details,details_source=EXCLUDED.details_source"
    [ toPersistValue versionId,toPersistValue resolved
    , PersistText (T.strip (musicPartyDisplayName party)),optionalText (musicPartyLegalName party)
    , PersistText (musicPartyKind party),toPersistValue versionId,toPersistValue resolved
    , PersistText (jsonText identifiers)
    ]
  pure (Map.insert (musicPartyClientRef party) resolved accumulated)

partyIdentifierJson :: UUID -> MusicPartyIdentifierDraft -> Value
partyIdentifierJson partyId MusicPartyIdentifierDraft{..} =
  let kind = T.toLower (T.strip musicPartyIdentifierType)
      verification = if kind == "proprietary" then "unvalidated" else "syntax_valid" :: Text
  in object
    [ "music_party_id" .= partyId, "identifier_type" .= kind
    , "identifier_value" .= ContentValidation.normalizeMusicIdentifier kind musicPartyIdentifierValue
    , "provenance" .= ("provided" :: Text), "verification_status" .= verification
    ]

insertIdentifier :: UUID -> Map.Map Text UUID -> MusicIdentifierDraft -> SqlPersistT IO ()
insertIdentifier versionId trackMap MusicIdentifierDraft{..} = do
  let targetRecording = musicIdentifierTrackRef >>= (`Map.lookup` trackMap)
      verification = if T.toLower musicIdentifierType == "proprietary" then "unvalidated" else "syntax_valid"
  rawExecute
    "INSERT INTO music_identifier(release_version_id,recording_id,identifier_type,identifier_value,provenance,verification_status) VALUES(?::uuid,?::uuid,?,?, 'provided',?)"
    [ maybe (toPersistValue versionId) (const PersistNull) targetRecording
    , optionalUuid targetRecording,PersistText (T.toLower musicIdentifierType)
    , PersistText (normalizedIdentifier musicIdentifierType musicIdentifierValue),PersistText verification
    ]

insertCredit :: UUID -> Map.Map Text UUID -> Map.Map Text UUID -> MusicCreditDraft -> SqlPersistT IO ()
insertCredit versionId trackMap partyMap MusicCreditDraft{..} =
  rawExecute
    "INSERT INTO music_credit(release_version_id,recording_id,music_party_id,credit_role,display_order,notes) VALUES(?::uuid,?::uuid,?::uuid,?,?,?)"
    [ toPersistValue versionId,optionalUuid (musicCreditTrackRef >>= (`Map.lookup` trackMap))
    , toPersistValue (requiredMap "partyRef" musicCreditPartyRef partyMap)
    , PersistText (T.toLower musicCreditRole),PersistInt64 (fromIntegral musicCreditDisplayOrder)
    , optionalText musicCreditNotes
    ]

insertRights
  :: Int64 -> UUID -> Map.Map Text UUID -> Map.Map Text UUID
  -> (MusicRightsDraft,UUID) -> SqlPersistT IO ()
insertRights actor versionId trackMap partyMap (MusicRightsDraft{..},declarationId) = do
  rawExecute
    "INSERT INTO music_rights_declaration(id,release_version_id,recording_id,rights_scope,authority_basis,territories,starts_on,ends_on,declared_by) VALUES(?::uuid,?::uuid,?::uuid,?,?,?,?,?,?)"
    [ toPersistValue declarationId,toPersistValue versionId
    , optionalUuid (musicRightsTrackRef >>= (`Map.lookup` trackMap)),PersistText (T.toLower musicRightsScope)
    , PersistText (T.strip musicRightsAuthorityBasis),postgresTextArray musicRightsTerritories
    , PersistDay musicRightsStartsOn,optionalDay musicRightsEndsOn,PersistInt64 actor
    ]
  forM_ musicRightsSplits $ \MusicSplitDraft{..} -> rawExecute
    "INSERT INTO music_rights_split(declaration_id,rights_holder_id,basis_points,territories,starts_on,ends_on) VALUES(?::uuid,?::uuid,?,?,?,?)"
    [ toPersistValue declarationId,toPersistValue (requiredMap "partyRef" musicSplitPartyRef partyMap)
    , PersistInt64 (fromIntegral musicSplitBasisPoints),postgresTextArray musicSplitTerritories
    , PersistDay musicSplitStartsOn,optionalDay musicSplitEndsOn
    ]

insertAvailability :: UUID -> Map.Map Text UUID -> MusicAvailabilityDraft -> SqlPersistT IO ()
insertAvailability versionId trackMap MusicAvailabilityDraft{..} =
  rawExecute
    "INSERT INTO music_availability_rule(release_version_id,release_track_id,territory_mode,territories,starts_at,ends_at,listening_policy,download_policy,purchasable,price_minor,currency,downloadable_asset_id) SELECT ?::uuid,track.id,?,?,?,?,?,?,?,?,?,?::uuid FROM (SELECT NULL::uuid AS id WHERE ?::text IS NULL UNION ALL SELECT release_track.id FROM music_release_track release_track WHERE release_track.release_version_id=?::uuid AND release_track.recording_id=?::uuid AND ?::text IS NOT NULL) track"
    [ toPersistValue versionId,PersistText (T.toLower musicAvailabilityTerritoryMode)
    , postgresTextArray musicAvailabilityTerritories,optionalTime musicAvailabilityStartsAt
    , optionalTime musicAvailabilityEndsAt,PersistText (T.toLower musicAvailabilityListeningPolicy)
    , PersistText (T.toLower musicAvailabilityDownloadPolicy),PersistBool musicAvailabilityPurchasable
    , optionalInt64 musicAvailabilityPriceMinor,optionalText (T.toUpper . T.strip <$> musicAvailabilityCurrency)
    , optionalUuid musicAvailabilityDownloadableAssetId,optionalText musicAvailabilityTrackRef
    , toPersistValue versionId,optionalUuid (musicAvailabilityTrackRef >>= (`Map.lookup` trackMap))
    , optionalText musicAvailabilityTrackRef
    ]

validateExistingReferences :: AuthedUser -> UUID -> UUID -> MusicReleaseContentRequest -> AppM ()
validateExistingReferences user releaseId versionId MusicReleaseContentRequest{..} = do
  let requestedExisting = [value | Just value <- map musicTrackRecordingId musicContentTracks]
  current <- runDB (rawSql "SELECT recording_id FROM music_release_track WHERE release_version_id=?::uuid"
    [toPersistValue versionId] :: SqlPersistT IO [Single UUID])
  let currentIds = Set.fromList (map unSingle current)
  unless (all (`Set.member` currentIds) requestedExisting) $
    throwError (badRequest "recordingId must already belong to this editable release version")
  shared <- runDB (rawSql
    "SELECT EXISTS(SELECT 1 FROM music_release_track track WHERE track.recording_id=ANY(?::uuid[]) AND track.release_version_id<>?::uuid)"
    [postgresUuidArray requestedExisting,toPersistValue versionId] :: SqlPersistT IO [Single Bool])
  when (shared == [Single True]) $
    throwError (conflict "A recording shared with another release version cannot be edited; create a correction copy")
  let removed = Set.toList (currentIds `Set.difference` Set.fromList requestedExisting)
  retainedAssets <- runDB (rawSql
    "SELECT EXISTS(SELECT 1 FROM music_asset WHERE release_version_id=?::uuid AND recording_id=ANY(?::uuid[]) AND processing_state<>'deleted')"
    [toPersistValue versionId,postgresUuidArray removed] :: SqlPersistT IO [Single Bool])
  when (retainedAssets == [Single True]) $
    throwError (conflict "A track with uploaded assets cannot be removed; create a new correction version")
  forM_ musicContentParties $ \party -> do
    forM_ (musicPartyTdfPartyId party) $ \tdfId -> do
      exists <- booleanQuery "SELECT EXISTS(SELECT 1 FROM party WHERE id=?)" [PersistInt64 tdfId]
      unless exists (throwError (badRequest "tdfPartyId does not identify a TDF party"))
    forM_ (musicPartyId party) $ \partyId -> do
      allowed <- booleanQuery
        "SELECT EXISTS(SELECT 1 FROM music_party party WHERE party.id=?::uuid AND (party.tdf_party_id IS NOT NULL OR party.created_by=? OR EXISTS(SELECT 1 FROM music_release_version_party member WHERE member.release_version_id=?::uuid AND member.music_party_id=party.id) OR EXISTS(SELECT 1 FROM music_credit credit WHERE credit.release_version_id=?::uuid AND credit.music_party_id=party.id)))"
        [toPersistValue partyId,PersistInt64 (currentPartyId user),toPersistValue versionId,toPersistValue versionId]
      unless allowed (throwError (badRequest "partyId is not available to this release"))
  assetsValid <- forM musicContentAvailability $ \availability -> case musicAvailabilityDownloadableAssetId availability of
    Nothing -> pure True
    Just assetId -> booleanQuery
      "SELECT EXISTS(SELECT 1 FROM music_asset WHERE id=?::uuid AND release_version_id=?::uuid AND processing_state IN ('valid','ready'))"
      [toPersistValue assetId,toPersistValue versionId]
  unless (and assetsValid) (throwError (badRequest "downloadableAssetId must be a validated asset from this release version"))
  exists <- booleanQuery "SELECT EXISTS(SELECT 1 FROM music_release_version WHERE id=?::uuid AND release_id=?::uuid)"
    [toPersistValue versionId,toPersistValue releaseId]
  unless exists (throwError err404)

contentJson :: UUID -> UUID -> AppM Value
contentJson releaseId versionId = jsonOne err404 (injectGenreFields
  "SELECT jsonb_build_object('id',version.id,'releaseId',version.release_id,'versionNumber',version.version_number,'state',version.state,'title',version.title,'subtitle',version.subtitle,'versionTitle',version.version_title,'displayArtist',version.display_artist,'titleLanguage',version.title_language,'titleScript',version.title_script,'explicitContent',version.explicit_content,'originalReleaseDate',version.original_release_date,'releaseAtUtc',version.release_at_utc,'releaseTimezone',version.release_timezone,'embargoUntilUtc',version.embargo_until_utc,'takedownAtUtc',version.takedown_at_utc,'takedownTimezone',version.takedown_timezone,'labelName',version.label_name,'catalogNumber',version.catalog_number,'recordingCopyrightText',version.recording_copyright_text,'workCopyrightText',version.work_copyright_text,'validation',jsonb_build_object('metadata',version.metadata_valid,'assets',version.assets_valid,'rights',version.rights_valid,'access',version.access_valid,'errors',COALESCE((SELECT jsonb_agg(jsonb_build_object('fieldPath',field_path,'code',error_code,'message',message)) FROM music_check_submission(version.id)),'[]'::jsonb)),'tracks',COALESCE((SELECT jsonb_agg(jsonb_build_object('trackId',track.id,'recordingId',recording.id,'discNumber',track.disc_number,'trackNumber',track.track_number,'displayArtist',track.display_artist,'isPrimaryResource',track.is_primary_resource,'previewStartMs',track.preview_start_ms,'previewDurationMs',track.preview_duration_ms,'title',recording.canonical_title,'subtitle',recording.subtitle,'versionTitle',recording.version_title,'titleLanguage',recording.title_language,'titleScript',recording.title_script,'durationMs',recording.duration_ms,'explicitContent',recording.explicit_content) ORDER BY track.disc_number,track.track_number) FROM music_release_track track JOIN music_recording recording ON recording.id=track.recording_id WHERE track.release_version_id=version.id),'[]'::jsonb),'parties',music_version_parties(version.id),'credits',COALESCE((SELECT jsonb_agg(to_jsonb(credit) ORDER BY credit.display_order,credit.id) FROM music_credit credit WHERE credit.release_version_id=version.id),'[]'::jsonb),'identifiers',COALESCE((SELECT jsonb_agg(to_jsonb(identifier) ORDER BY identifier.identifier_type,identifier.id) FROM music_identifier identifier WHERE identifier.release_version_id=version.id OR identifier.recording_id IN (SELECT track.recording_id FROM music_release_track track WHERE track.release_version_id=version.id)),'[]'::jsonb),'rights',COALESCE((SELECT jsonb_agg(to_jsonb(rights) || jsonb_build_object('splits',COALESCE((SELECT jsonb_agg(to_jsonb(split)) FROM music_rights_split split WHERE split.declaration_id=rights.id),'[]'::jsonb))) FROM music_rights_declaration rights WHERE rights.release_version_id=version.id),'[]'::jsonb),'availability',COALESCE((SELECT jsonb_agg(to_jsonb(rule)) FROM music_availability_rule rule WHERE rule.release_version_id=version.id),'[]'::jsonb),'terms',COALESCE((SELECT jsonb_agg(to_jsonb(terms)-'evidence') FROM music_terms_acceptance terms WHERE terms.release_version_id=version.id AND terms.terms_kind<>'download_sale'),'[]'::jsonb),'assets',COALESCE((SELECT jsonb_agg(jsonb_build_object('id',asset.id,'recordingId',asset.recording_id,'parentAssetId',asset.parent_asset_id,'role',asset.asset_role,'originalFilename',asset.original_filename,'mediaType',asset.media_type,'byteSize',asset.byte_size,'sha256',asset.sha256,'processingState',asset.processing_state,'technicalMetadata',asset.technical_metadata,'createdAt',asset.created_at,'readyAt',asset.ready_at)) FROM music_asset asset WHERE asset.release_version_id=version.id AND asset.processing_state<>'deleted'),'[]'::jsonb),'comments',COALESCE((SELECT jsonb_agg(jsonb_build_object('id',comment.id,'parentCommentId',comment.parent_comment_id,'fieldPath',comment.field_path,'body',comment.body,'visibility',comment.visibility,'resolutionState',comment.resolution_state,'createdBy',comment.created_by,'createdAt',comment.created_at,'resolvedBy',comment.resolved_by,'resolvedAt',comment.resolved_at) ORDER BY comment.created_at,comment.id) FROM music_editorial_comment comment WHERE comment.release_version_id=version.id),'[]'::jsonb),'updatedAt',version.updated_at) FROM music_release_version version WHERE version.id=?::uuid AND version.release_id=?::uuid"
  ) [toPersistValue versionId,toPersistValue releaseId]

injectGenreFields :: Text -> Text
injectGenreFields = T.replace
  "'titleScript',version.title_script,'explicitContent'"
  "'titleScript',version.title_script,'primaryGenreId',version.primary_genre_id,'secondaryGenreId',version.secondary_genre_id,'primaryGenre',(SELECT genre.name_es FROM genre WHERE genre.id=version.primary_genre_id),'secondaryGenre',(SELECT genre.name_es FROM genre WHERE genre.id=version.secondary_genre_id),'explicitContent'"

requireFeature :: Text -> AppM ()
requireFeature flag = do
  rawEnvironment <- liftIO (lookupEnv "APP_ENV")
  let environment = case fmap (T.toLower . T.strip . T.pack) rawEnvironment of
        Just "production" -> "production"
        Just "prod" -> "production"
        _ -> "sandbox"
  enabled <- booleanQuery "SELECT EXISTS(SELECT 1 FROM revenue_feature_flag WHERE flag_key=? AND environment=? AND enabled)"
    [PersistText flag,PersistText environment]
  unless enabled $ throwError err503 { errBody="Music release authoring is not enabled" }

requireOwnerPermission :: AuthedUser -> UUID -> Text -> AppM ()
requireOwnerPermission user releaseId permission = unless (hasStrictAdminAccess user) $ do
  allowed <- booleanQuery
    "SELECT EXISTS(SELECT 1 FROM music_release release WHERE release.id=?::uuid AND music_can(?,release.artist_party_id,?))"
    [toPersistValue releaseId,PersistInt64 (currentPartyId user),PersistText permission]
  unless allowed (throwError err403)

jsonOne :: ServerError -> Text -> [PersistValue] -> AppM Value
jsonOne missing statement params = do
  rows <- runDB (rawSql statement params :: SqlPersistT IO [Single CMS.AesonValue])
  maybe (throwError missing) (pure . CMS.unAesonValue . unSingle) (listToMaybe rows)

booleanQuery :: Text -> [PersistValue] -> AppM Bool
booleanQuery statement params = do
  rows <- runDB (rawSql statement params :: SqlPersistT IO [Single Bool])
  pure (rows == [Single True])

singleUuid :: String -> [Single UUID] -> SqlPersistT IO UUID
singleUuid _ [Single value] = pure value
singleUuid label _ = fail (label <> " insert did not return exactly one identifier")

requiredMap :: String -> Text -> Map.Map Text UUID -> UUID
requiredMap label key values = fromMaybe (error (label <> " was not validated")) (Map.lookup key values)

normalizedIdentifier :: Text -> Text -> Text
normalizedIdentifier rawType rawValue = case identifierKind rawType of
  Right kind -> case validateIdentifier kind rawValue of
    Right value -> value
    Left _ -> T.strip rawValue
  Left _ -> T.strip rawValue

identifierKind :: Text -> Either Text IdentifierType
identifierKind raw = case T.toLower (T.strip raw) of
  "isrc" -> Right ISRC
  "upc" -> Right UPC
  "ean" -> Right EAN
  "grid" -> Right GRid
  "proprietary" -> Right Proprietary
  _ -> Left "identifierType must be isrc, upc, ean, grid, or proprietary."

validRequiredText :: Int -> Text -> Bool
validRequiredText maxLength value = not (T.null (T.strip value)) && T.length value <= maxLength

jsonText :: ToJSON value => value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode

optionalText :: Maybe Text -> PersistValue
optionalText = maybe PersistNull (PersistText . T.strip)

optionalInt64 :: Maybe Int64 -> PersistValue
optionalInt64 = maybe PersistNull PersistInt64

optionalUuid :: Maybe UUID -> PersistValue
optionalUuid = maybe PersistNull toPersistValue

optionalTime :: Maybe UTCTime -> PersistValue
optionalTime = maybe PersistNull PersistUTCTime

optionalDay :: Maybe Day -> PersistValue
optionalDay = maybe PersistNull PersistDay

postgresTextArray :: [Text] -> PersistValue
postgresTextArray values = PersistText ("{" <> T.intercalate "," values <> "}")

postgresUuidArray :: [UUID] -> PersistValue
postgresUuidArray values = PersistText ("{" <> T.intercalate "," (map (T.pack . show) values) <> "}")
