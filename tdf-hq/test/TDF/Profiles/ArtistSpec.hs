{-# LANGUAGE OverloadedStrings #-}

module TDF.Profiles.ArtistSpec (spec) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (NoLoggingT)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Text                  (Text)
import           Data.Time.Clock            (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql       (SqlBackend, SqlPersistT, fromSqlKey, runMigrationSilent)
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

  it "upserts profile data and reports follower counts" $ do
    dto <- runInMemory $ do
      now <- liftIO getCurrentTime
      artistId <- insertParty "Los Mentores"
      insertFanFollow artistId "Carla"
      insertFanFollow artistId "Edu"
      let payload = ArtistProfileUpsert
            { apuArtistId        = fromSqlKey artistId
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

-- Helpers

runInMemory :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runInMemory action =
  runSqlite ":memory:" $ do
    _ <- runMigrationSilent migrateAll
    action

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
