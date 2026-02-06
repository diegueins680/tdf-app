{-# LANGUAGE OverloadedStrings #-}

module TDF.Profiles.ArtistSpec (spec) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (NoLoggingT)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Text                  (Text)
import           Data.Time.Clock            (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql       (SqlBackend, SqlPersistT, fromSqlKey, rawExecute)
import           Database.Persist.Sqlite    (runSqlite)
import           Test.Hspec

import           TDF.DTO                    (ArtistProfileDTO(..), ArtistProfileUpsert(..))
import           TDF.Models
import           TDF.Profiles.Artist        ( loadOrCreateArtistProfileDTO
                                            , upsertArtistProfileRecord
                                            )

spec :: Spec
spec = describe "Artist profile helpers" $ do
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
      let payload = ArtistProfileUpsert
            { apuArtistId        = fromSqlKey artistId
            , apuDisplayName     = Just "Los Mentores"
            , apuSlug            = Just "los-mentores"
            , apuBio             = Just "Fusionando ritmos latinos con neo soul."
            , apuCity            = Just "Quito"
            , apuHeroImageUrl    = Just "https://cdn.tdf/hero.jpg"
            , apuSpotifyArtistId = Just "spotify-123"
            , apuSpotifyUrl      = Just "https://open.spotify.com/artist/spotify-123"
            , apuYoutubeChannelId = Just "yt-chan"
            , apuYoutubeUrl      = Just "https://youtube.com/@tdf"
            , apuWebsiteUrl      = Just "https://tdfrecords.com/mentores"
            , apuFeaturedVideoUrl = Just "https://youtube.com/watch?v=123"
            , apuGenres          = Just "Latin,Soul"
            , apuHighlights      = Just "Ganadores del IMAGINE 2024"
            }
      upsertArtistProfileRecord artistId payload now
    apSlug dto `shouldBe` Just "los-mentores"
    apCity dto `shouldBe` Just "Quito"
    apFollowerCount dto `shouldBe` 2
    apSpotifyUrl dto `shouldBe` Just "https://open.spotify.com/artist/spotify-123"
    apHasUserAccount dto `shouldBe` False

-- Helpers

runInMemory :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runInMemory action =
  runSqlite ":memory:" $ do
    initializeTestSchema
    action

initializeTestSchema :: MonadIO m => SqlPersistT m ()
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
    \)" []
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
    \)" []
  rawExecute
    "CREATE TABLE IF NOT EXISTS \"fan_follow\" (\
    \\"id\" INTEGER PRIMARY KEY,\
    \\"fan_party_id\" INTEGER NOT NULL,\
    \\"artist_party_id\" INTEGER NOT NULL,\
    \\"created_at\" TIMESTAMP NOT NULL,\
    \CONSTRAINT \"unique_fan_follow\" UNIQUE (\"fan_party_id\", \"artist_party_id\"),\
    \FOREIGN KEY(\"fan_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE,\
    \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\") ON DELETE CASCADE\
    \)" []

insertParty :: MonadIO m => Text -> SqlPersistT m PartyId
insertParty name = do
  now <- liftIO getCurrentTime
  insert Party
    { partyLegalName        = Nothing
    , partyDisplayName      = name
    , partyIsOrg            = False
    , partyTaxId            = Nothing
    , partyPrimaryEmail     = Nothing
    , partyPrimaryPhone     = Nothing
    , partyWhatsapp         = Nothing
    , partyInstagram        = Nothing
    , partyEmergencyContact = Nothing
    , partyNotes            = Nothing
    , partyCreatedAt        = now
    }

insertFanFollow :: MonadIO m => PartyId -> Text -> SqlPersistT m ()
insertFanFollow artistId fanName = do
  fanId <- insertParty fanName
  now <- liftIO getCurrentTime
  _ <- insert FanFollow
    { fanFollowFanPartyId    = fanId
    , fanFollowArtistPartyId = artistId
    , fanFollowCreatedAt     = now
    }
  pure ()
