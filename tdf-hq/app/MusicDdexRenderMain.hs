{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import System.Environment (getArgs, getEnv)
import System.Exit (die)

import TDF.MusicRelease.DDEX.ERN432

data HeaderRow = HeaderRow
  { hOperation :: Text
  , hMessageId :: Text
  , hCreatedAt :: UTCTime
  , hSenderDpid :: Text
  , hSenderName :: Text
  , hSenderAuthority :: Text
  , hSenderEvidence :: Text
  , hSenderVerifiedAt :: UTCTime
  , hRecipientDpid :: Text
  , hRecipientName :: Text
  , hRecipientAuthority :: Text
  , hRecipientEvidence :: Text
  , hRecipientVerifiedAt :: UTCTime
  , hReleaseId :: Text
  , hReleaseKind :: Text
  , hReleaseTitle :: Text
  , hDisplayArtist :: Text
  , hLanguage :: Text
  , hLabelName :: Text
  , hGenre :: Text
  , hDealStartsAt :: UTCTime
  , hReleaseIdentifierType :: Text
  , hReleaseIdentifierValue :: Text
  , hPLineText :: Text
  , hPLineYear :: Int
  , hDurationMs :: Int64
  , hTerritories :: [Text]
  , hDealEndsAt :: Maybe UTCTime
  }

instance FromRow HeaderRow where
  fromRow = HeaderRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> (fromPGArray <$> field) <*> field

data TrackRow = TrackRow Text Text Text Int64 Text Text Text Text Text
instance FromRow TrackRow where fromRow = TrackRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data CoverRow = CoverRow Text Text Text Text
instance FromRow CoverRow where fromRow = CoverRow <$> field <*> field <*> field <*> field

main :: IO ()
main = do
  arguments <- getArgs
  (exportId,xmlPath,resourceManifestPath) <- case arguments of
    [exportId,xmlPath,manifestPath] -> pure (exportId,xmlPath,manifestPath)
    _ -> die "Usage: tdf-ddex-render EXPORT_UUID OUTPUT_XML RESOURCE_MANIFEST_TSV"
  databaseUrl <- getEnv "DATABASE_URL"
  bracket (connectPostgreSQL (BS8.pack databaseUrl)) close $ \connection -> do
    -- Direct CLI use must enforce the same limited free-streaming contract as
    -- queued jobs; never infer subscription/payment terms from a full listen.
    issues <- query connection
      "SELECT issue.field_path,issue.message FROM music_ddex_export export CROSS JOIN LATERAL music_check_ddex_operation(export.release_version_id,export.sender_registry_id,export.recipient_registry_id,export.operation) issue WHERE export.id=?::uuid"
      (Only (fromString exportId :: Text)) :: IO [(Text,Text)]
    unless (null issues) (die (unlines [T.unpack (path <> ": " <> explanation) | (path,explanation) <- issues]))
    headers <- query connection headerSql (Only (fromString exportId :: Text))
    header <- case headers of [value] -> pure value; _ -> die "DDEX export was not found or has incomplete canonical metadata"
    trackRows <- query connection trackSql (Only (fromString exportId :: Text))
    covers <- query connection coverSql (Only (fromString exportId :: Text))
    coverRow <- case covers of [value] -> pure value; _ -> die "DDEX export requires exactly one ready cover delivery asset"
    unless (not (null (trackRows :: [TrackRow]))) (die "DDEX export requires at least one ready audio resource")
    snapshots <- query connection
      "SELECT version.immutable_snapshot FROM music_ddex_export export JOIN music_release_version version ON version.id=export.release_version_id WHERE export.id=?::uuid AND version.snapshot_sha256=export.canonical_snapshot_sha256"
      (Only (fromString exportId :: Text)) :: IO [Only Value]
    graph <- case snapshots of
      [Only snapshot] -> case parseErn432Credits snapshot of
        Right value -> pure value
        Left errors -> die (unlines [T.unpack (exportErrorField e <> ": " <> exportErrorMessage e) | e <- errors])
      _ -> die "Approved snapshot is missing or no longer matches the requested export hash"
    let message = toMessage header graph trackRows coverRow
    case renderErn432AudioRelease message of
      Left errors -> die (unlines [T.unpack (exportErrorField problem <> ": " <> exportErrorMessage problem) | problem <- errors])
      Right xml -> do
        BL.writeFile xmlPath xml
        TIO.writeFile resourceManifestPath (resourceManifest header trackRows coverRow)

toMessage :: HeaderRow -> Ern432Credits -> [TrackRow] -> CoverRow -> Ern432AudioRelease
toMessage header graph trackRows coverRow = Ern432AudioRelease
  { messagePurpose = purpose (hOperation header)
  , messageThreadId = "TDF-" <> hReleaseId header <> "-" <> hRecipientDpid header
  , messageId = hMessageId header
  , messageCreatedAt = hCreatedAt header
  , messageLanguageAndScript = hLanguage header
  , sender = counterparty (hSenderDpid header) (hSenderName header) (hSenderAuthority header) (hSenderEvidence header) (hSenderVerifiedAt header)
  , recipient = counterparty (hRecipientDpid header) (hRecipientName header) (hRecipientAuthority header) (hRecipientEvidence header) (hRecipientVerifiedAt header)
  , catalogCredits = graph
  , labelPartyReference = "PLabel"
  , labelName = hLabelName header
  , releaseReference = "R0"
  , releaseKind = toReleaseKind (hReleaseKind header)
  , releaseIdentifier = headerIdentifier header
  , releaseTitle = hReleaseTitle header
  , releaseDisplayArtist = hDisplayArtist header
  , releaseGenre = hGenre header
  , releaseDurationSeconds = max 1 (fromIntegral (hDurationMs header `div` 1000))
  , releaseTerritories = hTerritories header
  , dealStartsAt = hDealStartsAt header
  , dealEndsAt = hDealEndsAt header
  , tracks = zipWith (toTrack header) [1 :: Int ..] trackRows
  , cover = toCover header coverRow
  }

counterparty :: Text -> Text -> Text -> Text -> UTCTime -> Ern432Counterparty
counterparty dpid name authority evidence verified = Ern432Counterparty dpid name
  (AuthorityVerified authority evidence verified)

toTrack :: HeaderRow -> Int -> TrackRow -> Ern432Track
toTrack header sequenceNumber (TrackRow recordingId title artist duration explicit isrc _ _ _) = Ern432Track
  { trackRecordingId = recordingId
  , trackResourceReference = "A" <> T.pack (show sequenceNumber)
  , trackReleaseReference = "R" <> T.pack (show sequenceNumber)
  , trackTitle = title
  , trackDisplayArtist = artist
  , trackIsrc = isrc
  , trackDurationSeconds = max 1 (fromIntegral (duration `div` 1000))
  , trackPLineYear = hPLineYear header
  , trackPLineText = hPLineText header
  , trackAudioFileUri = ern432AudioResourcePath (headerIdentifier header) sequenceNumber
  , trackExplicit = explicit == "explicit"
  }

toCover :: HeaderRow -> CoverRow -> Ern432Cover
toCover header (CoverRow assetId _ _ _) = Ern432Cover
  { coverResourceReference = "AArtwork"
  , coverProprietaryId = assetId
  , coverFileUri = ern432CoverResourcePath (headerIdentifier header)
  , coverExplicit = False
  }

resourceManifest :: HeaderRow -> [TrackRow] -> CoverRow -> Text
resourceManifest header trackRows (CoverRow _ coverBucket coverKey coverSha256) = T.unlines $
  [T.intercalate "\t" [bucket,key,sha256,ern432AudioResourcePath identifier sequenceNumber]
    | (sequenceNumber,TrackRow _ _ _ _ _ _ bucket key sha256) <- zip [1 :: Int ..] trackRows]
  ++ [T.intercalate "\t" [coverBucket,coverKey,coverSha256,ern432CoverResourcePath identifier]]
  where identifier = headerIdentifier header

headerIdentifier :: HeaderRow -> Ern432ReleaseId
headerIdentifier header = (if hReleaseIdentifierType header == "grid" then ErnGRid else ErnIcpn)
  (hReleaseIdentifierValue header)

purpose :: Text -> Ern432MessagePurpose
purpose "update" = ErnUpdate
purpose "takedown" = ErnTakedown
purpose _ = ErnNewRelease

toReleaseKind :: Text -> Ern432ReleaseKind
toReleaseKind "album" = ErnAlbum
toReleaseKind "ep" = ErnEP
toReleaseKind _ = ErnSingle

headerSql :: Query
headerSql = "SELECT export.operation,export.message_id,export.created_at,sender.dpid,sender.party_name,sender.verification_authority,sender.verification_evidence::text,sender.verified_at,recipient.dpid,recipient.party_name,recipient.verification_authority,recipient.verification_evidence::text,recipient.verified_at,release.id::text,release.release_kind,version.title,version.display_artist,version.title_language,version.label_name,genre.name_en,COALESCE(GREATEST(rule.starts_at,version.release_at_utc,version.embargo_until_utc),version.approved_at,export.created_at),identifier.identifier_type,identifier.identifier_value,version.recording_copyright_text,EXTRACT(YEAR FROM COALESCE(version.original_release_date,version.release_at_utc::date,version.approved_at::date,export.created_at::date))::int,COALESCE((SELECT SUM(recording.duration_ms) FROM music_release_track track JOIN music_recording recording ON recording.id=track.recording_id WHERE track.release_version_id=version.id),0)::bigint,rule.territories,rule.ends_at FROM music_ddex_export export JOIN music_release_version version ON version.id=export.release_version_id JOIN music_release release ON release.id=version.release_id JOIN music_ddex_party_registry sender ON sender.id=export.sender_registry_id JOIN music_ddex_party_registry recipient ON recipient.id=export.recipient_registry_id JOIN genre ON genre.id=version.primary_genre_id JOIN music_availability_rule rule ON rule.release_version_id=version.id AND rule.release_track_id IS NULL AND rule.territory_mode='include' JOIN LATERAL (SELECT item.identifier_type,item.identifier_value FROM music_identifier item WHERE item.release_version_id=version.id AND item.identifier_type IN ('grid','upc','ean') AND item.verification_status IN ('syntax_valid','authority_verified') ORDER BY CASE item.identifier_type WHEN 'grid' THEN 0 WHEN 'upc' THEN 1 ELSE 2 END,item.created_at,item.id LIMIT 1) identifier ON TRUE WHERE export.id=?::uuid"

trackSql :: Query
trackSql = "SELECT recording.id::text,recording.canonical_title,track.display_artist,recording.duration_ms,recording.explicit_content,identifier.identifier_value,asset.bucket_name,asset.object_key,asset.sha256 FROM music_ddex_export export JOIN music_release_track track ON track.release_version_id=export.release_version_id JOIN music_recording recording ON recording.id=track.recording_id JOIN LATERAL (SELECT item.identifier_value FROM music_identifier item WHERE item.recording_id=recording.id AND item.identifier_type='isrc' AND item.verification_status IN ('syntax_valid','authority_verified') ORDER BY item.created_at LIMIT 1) identifier ON TRUE JOIN LATERAL (SELECT delivery.bucket_name,delivery.object_key,delivery.sha256 FROM music_asset delivery WHERE delivery.release_version_id=export.release_version_id AND delivery.recording_id=recording.id AND delivery.asset_role='stream_audio' AND delivery.processing_state='ready' AND delivery.media_type='audio/mp4' ORDER BY COALESCE((delivery.technical_metadata->>'bitrate_kbps')::int,0) DESC,delivery.created_at DESC LIMIT 1) asset ON TRUE WHERE export.id=?::uuid ORDER BY track.disc_number,track.track_number,track.id"

coverSql :: Query
coverSql = "SELECT asset.id::text,asset.bucket_name,asset.object_key,asset.sha256 FROM music_ddex_export export JOIN LATERAL (SELECT delivery.id,delivery.bucket_name,delivery.object_key,delivery.sha256 FROM music_asset delivery WHERE delivery.release_version_id=export.release_version_id AND delivery.asset_role='cover_display' AND delivery.processing_state='ready' AND delivery.media_type IN ('image/jpeg','image/jpg') ORDER BY delivery.created_at DESC LIMIT 1) asset ON TRUE WHERE export.id=?::uuid"
