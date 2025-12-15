{-# LANGUAGE OverloadedStrings #-}

module TDF.Social.FollowSpec (spec) where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (NoLoggingT)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Maybe                  (isJust, isNothing)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist            (insert, insertUnique)
import           Database.Persist.Sql        (runMigration, runSqlite, SqlBackend)
import           Test.Hspec

import           TDF.Models.SocialEventsModels

spec :: Spec
spec = describe "ArtistFollow insertUnique" $ do
  it "prevents duplicate follows (idempotent)" $ do
    (createdOnce, secondWasIgnored) <- runInMemory $ do
      now <- liftIO getCurrentTime
      -- create an artist
      artistId <- insert ArtistProfile
        { artistProfilePartyId = Nothing
        , artistProfileName = "Test Band"
        , artistProfileBio = Nothing
        , artistProfileAvatarUrl = Nothing
        , artistProfileGenres = Nothing
        , artistProfileSocialLinks = Nothing
        , artistProfileCreatedAt = now
        , artistProfileUpdatedAt = now
        }
      let follower = "carla"
      m1 <- insertUnique ArtistFollow { artistFollowArtistId = artistId, artistFollowFollowerPartyId = follower, artistFollowCreatedAt = now }
      m2 <- insertUnique ArtistFollow { artistFollowArtistId = artistId, artistFollowFollowerPartyId = follower, artistFollowCreatedAt = now }
      pure (isJust m1, isNothing m2)

    createdOnce `shouldBe` True
    secondWasIgnored `shouldBe` True

-- Helpers

runInMemory :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runInMemory action =
  runSqlite ":memory:" $ do
    runMigration migrateSocialEvents
    action
