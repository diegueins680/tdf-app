{-# LANGUAGE OverloadedStrings #-}

module TDF.Profiles.ArtistSpec (spec) where

import qualified Data.Aeson as A
import Data.Either (isLeft)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, fromSqlKey, rawExecute)
import Database.Persist.Sqlite (runSqlite)
import Test.Hspec

import TDF.DTO (ArtistProfileDTO (..), ArtistProfileUpsert (..))
import TDF.Models
import TDF.Profiles.Artist (
    loadOrCreateArtistProfileDTO,
    upsertArtistProfileRecord,
    validateArtistProfileUpsert,
 )

spec :: Spec
spec = do
    describe "ArtistProfileUpsert FromJSON" $ do
        it "accepts canonical artist profile write payloads" $
            case A.eitherDecode
                "{\"apuArtistId\":42,\"apuDisplayName\":\"Los Mentores\",\"apuSlug\":\"los-mentores\",\"apuBio\":\"Fusionando ritmos latinos con neo soul.\",\"apuCity\":\"Quito\"}" of
                Left err ->
                    expectationFailure ("Expected canonical artist profile payload to decode, got: " <> err)
                Right payload -> do
                    apuArtistId payload `shouldBe` 42
                    apuDisplayName payload `shouldBe` Just "Los Mentores"
                    apuSlug payload `shouldBe` Just "los-mentores"
                    apuBio payload `shouldBe` Just "Fusionando ritmos latinos con neo soul."
                    apuCity payload `shouldBe` Just "Quito"

        it "rejects unexpected artist profile keys so typoed writes fail explicitly" $ do
            (A.eitherDecode
                "{\"apuArtistId\":42,\"apuDisplayName\":\"Los Mentores\",\"displayName\":\"ignored by mistake\"}"
                    :: Either String ArtistProfileUpsert)
                `shouldSatisfy` isLeft
            (A.eitherDecode
                "{\"apuArtistId\":42,\"apuDisplayName\":\"Los Mentores\",\"unexpected\":true}"
                    :: Either String ArtistProfileUpsert)
                `shouldSatisfy` isLeft

    describe "Artist profile helpers" $ do
        it "normalizes path-safe profile slugs before artist profile writes persist them" $ do
            fmap apuSlug
                (validateArtistProfileUpsert
                    (baseProfileUpsert { apuSlug = Just "  Mentores-Del-Aire  " }))
                `shouldBe` Right (Just "mentores-del-aire")
            fmap apuSlug
                (validateArtistProfileUpsert (baseProfileUpsert { apuSlug = Just "   " }))
                `shouldBe` Right Nothing

        it "rejects path-ambiguous profile slugs before profile writes can miss public lookups" $ do
            let assertInvalid rawSlug expectedMessage =
                    case validateArtistProfileUpsert
                        (baseProfileUpsert { apuSlug = Just rawSlug }) of
                        Left err -> T.unpack err `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure ("Expected invalid profile slug error, got " <> show value)
            assertInvalid "mentores del aire" "only lowercase ASCII letters"
            assertInvalid "mentores/del-aire" "only lowercase ASCII letters"
            assertInvalid "mentores?draft=true" "only lowercase ASCII letters"
            assertInvalid "-mentores" "only lowercase ASCII letters"
            assertInvalid ("mentores" <> T.singleton '\x202E') "only lowercase ASCII letters"
            assertInvalid (T.replicate 97 "a") "96 characters or fewer"

        it "returns an initialized profile when none exists" $ do
            dto <- runInMemory $ do
                partyId <- insertParty "Aurora"
                loadOrCreateArtistProfileDTO partyId
            apDisplayName dto `shouldBe` "Aurora"
            apFollowerCount dto `shouldBe` 0
            apSlug dto `shouldBe` Nothing
            apHasUserAccount dto `shouldBe` False

        it "upserts profile data and reports follower counts" $ do
            dto <- runInMemory $ do
                now <- liftIO getCurrentTime
                artistId <- insertParty "Los Mentores"
                insertFanFollow artistId "Carla"
                insertFanFollow artistId "Edu"
                let payload =
                        ArtistProfileUpsert
                            { apuArtistId = fromSqlKey artistId
                            , apuDisplayName = Just "Los Mentores"
                            , apuSlug = Just "los-mentores"
                            , apuBio = Just "Fusionando ritmos latinos con neo soul."
                            , apuCity = Just "Quito"
                            , apuHeroImageUrl = Just "https://cdn.tdf/hero.jpg"
                            , apuSpotifyArtistId = Just "spotify-123"
                            , apuSpotifyUrl = Just "https://open.spotify.com/artist/spotify-123"
                            , apuYoutubeChannelId = Just "yt-chan"
                            , apuYoutubeUrl = Just "https://youtube.com/@tdf"
                            , apuWebsiteUrl = Just "https://tdfrecords.com/mentores"
                            , apuFeaturedVideoUrl = Just "https://youtube.com/watch?v=123"
                            , apuGenres = Just "Latin,Soul"
                            , apuHighlights = Just "Ganadores del IMAGINE 2024"
                            }
                upsertArtistProfileRecord artistId payload now
            apSlug dto `shouldBe` Just "los-mentores"
            apCity dto `shouldBe` Just "Quito"
            apFollowerCount dto `shouldBe` 2
            apSpotifyUrl dto `shouldBe` Just "https://open.spotify.com/artist/spotify-123"
            apHasUserAccount dto `shouldBe` False

        it "trims optional artist profile text fields and drops explicit blanks instead of storing whitespace-only data" $ do
            dto <- runInMemory $ do
                now <- liftIO getCurrentTime
                artistId <- insertParty "   Mentores del Aire   "
                let payload =
                        ArtistProfileUpsert
                            { apuArtistId = fromSqlKey artistId
                            , apuDisplayName = Just "  Mentores del Aire  "
                            , apuSlug = Just "  mentores-del-aire  "
                            , apuBio = Just "   "
                            , apuCity = Just "  Quito  "
                            , apuHeroImageUrl = Just "  https://cdn.tdf/hero.jpg  "
                            , apuSpotifyArtistId = Just "  spotify-456  "
                            , apuSpotifyUrl = Just "   "
                            , apuYoutubeChannelId = Just "  yt-mentores  "
                            , apuYoutubeUrl = Just " https://youtube.com/@mentores "
                            , apuWebsiteUrl = Just "   "
                            , apuFeaturedVideoUrl = Just "  https://youtube.com/watch?v=456  "
                            , apuGenres = Just "  Latin Pop  "
                            , apuHighlights = Just "   "
                            }
                upsertArtistProfileRecord artistId payload now

            apDisplayName dto `shouldBe` "Mentores del Aire"
            apSlug dto `shouldBe` Just "mentores-del-aire"
            apBio dto `shouldBe` Nothing
            apCity dto `shouldBe` Just "Quito"
            apHeroImageUrl dto `shouldBe` Just "https://cdn.tdf/hero.jpg"
            apSpotifyArtistId dto `shouldBe` Just "spotify-456"
            apSpotifyUrl dto `shouldBe` Nothing
            apYoutubeChannelId dto `shouldBe` Just "yt-mentores"
            apYoutubeUrl dto `shouldBe` Just "https://youtube.com/@mentores"
            apWebsiteUrl dto `shouldBe` Nothing
            apFeaturedVideoUrl dto `shouldBe` Just "https://youtube.com/watch?v=456"
            apGenres dto `shouldBe` Just "Latin Pop"
            apHighlights dto `shouldBe` Nothing

-- Helpers

baseProfileUpsert :: ArtistProfileUpsert
baseProfileUpsert =
    ArtistProfileUpsert
        { apuArtistId = 42
        , apuDisplayName = Just "Los Mentores"
        , apuSlug = Nothing
        , apuBio = Nothing
        , apuCity = Nothing
        , apuHeroImageUrl = Nothing
        , apuSpotifyArtistId = Nothing
        , apuSpotifyUrl = Nothing
        , apuYoutubeChannelId = Nothing
        , apuYoutubeUrl = Nothing
        , apuWebsiteUrl = Nothing
        , apuFeaturedVideoUrl = Nothing
        , apuGenres = Nothing
        , apuHighlights = Nothing
        }

runInMemory :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runInMemory action =
    runSqlite ":memory:" $ do
        initializeTestSchema
        action

initializeTestSchema :: (MonadIO m) => SqlPersistT m ()
initializeTestSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"party\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"legal_name\" VARCHAR NULL,\
        \\"display_name\" VARCHAR NOT NULL,\
        \\"is_org\" BOOLEAN NOT NULL,\
        \\"tax_id\" VARCHAR NULL,\
        \\"primary_email\" VARCHAR NULL,\
        \\"primary_phone\" VARCHAR NULL,\
        \\"whatsapp\" VARCHAR NULL,\
        \\"instagram\" VARCHAR NULL,\
        \\"emergency_contact\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"user_credential\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"party_id\" INTEGER NOT NULL,\
        \\"username\" VARCHAR NOT NULL,\
        \\"password_hash\" VARCHAR NOT NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \CONSTRAINT \"unique_credential_username\" UNIQUE (\"username\"),\
        \FOREIGN KEY(\"party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_profile\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"artist_party_id\" INTEGER NOT NULL,\
        \\"slug\" VARCHAR NULL,\
        \\"bio\" VARCHAR NULL,\
        \\"city\" VARCHAR NULL,\
        \\"hero_image_url\" VARCHAR NULL,\
        \\"spotify_artist_id\" VARCHAR NULL,\
        \\"spotify_url\" VARCHAR NULL,\
        \\"youtube_channel_id\" VARCHAR NULL,\
        \\"youtube_url\" VARCHAR NULL,\
        \\"website_url\" VARCHAR NULL,\
        \\"featured_video_url\" VARCHAR NULL,\
        \\"genres\" VARCHAR NULL,\
        \\"highlights\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NULL,\
        \CONSTRAINT \"unique_artist_profile\" UNIQUE (\"artist_party_id\"),\
        \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"fan_follow\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"fan_party_id\" INTEGER NOT NULL,\
        \\"artist_party_id\" INTEGER NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \CONSTRAINT \"unique_fan_follow\" UNIQUE (\"fan_party_id\", \"artist_party_id\"),\
        \FOREIGN KEY(\"fan_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE,\
        \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE\
        \)"
        []

insertParty :: (MonadIO m) => Text -> SqlPersistT m PartyId
insertParty name = do
    now <- liftIO getCurrentTime
    insert
        Party
            { partyLegalName = Nothing
            , partyDisplayName = name
            , partyIsOrg = False
            , partyTaxId = Nothing
            , partyPrimaryEmail = Nothing
            , partyPrimaryPhone = Nothing
            , partyWhatsapp = Nothing
            , partyInstagram = Nothing
            , partyEmergencyContact = Nothing
            , partyNotes = Nothing
            , partyCreatedAt = now
            }

insertFanFollow :: (MonadIO m) => PartyId -> Text -> SqlPersistT m ()
insertFanFollow artistId fanName = do
    fanId <- insertParty fanName
    now <- liftIO getCurrentTime
    _ <-
        insert
            FanFollow
                { fanFollowFanPartyId = fanId
                , fanFollowArtistPartyId = artistId
                , fanFollowCreatedAt = now
                }
    pure ()
