{-# LANGUAGE OverloadedStrings #-}

module TDF.Social.FollowHandlerSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (insert)
import Database.Persist.Sql (SqlPersistT, rawExecute, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Test.Hspec

import TDF.DTO.SocialEventsDTO (ArtistFollowerDTO (..))
import TDF.Models.SocialEventsModels
import TDF.Server.SocialEventsHandlers (followArtistDb)

spec :: Spec
spec = describe "followArtistDb helper" $ do
    it "creates a follow and is idempotent" $ do
        pool <- runStdoutLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- liftIO getCurrentTime
        artistId <-
            runSqlPool
                ( insert
                    ArtistProfile
                        { artistProfilePartyId = Nothing
                        , artistProfileName = "Band X"
                        , artistProfileBio = Nothing
                        , artistProfileAvatarUrl = Nothing
                        , artistProfileGenres = Nothing
                        , artistProfileSocialLinks = Nothing
                        , artistProfileCreatedAt = now
                        , artistProfileUpdatedAt = now
                        }
                )
                pool

        first <- followArtistDb pool artistId "carla"
        second <- followArtistDb pool artistId "carla"
        liftIO $ do
            (afFollowId first) `shouldSatisfy` (/= Nothing)
            (afFollowId second) `shouldSatisfy` (/= Nothing)

initializeSocialSchema :: SqlPersistT IO ()
initializeSocialSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"social_artist_profile\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"party_id\" VARCHAR NULL,\
        \\"name\" VARCHAR NOT NULL,\
        \\"bio\" VARCHAR NULL,\
        \\"avatar_url\" VARCHAR NULL,\
        \\"genres\" VARCHAR NULL,\
        \\"social_links\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_follow\" (\
        \\"artist_id\" INTEGER NOT NULL,\
        \\"follower_party_id\" VARCHAR NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \PRIMARY KEY (\"artist_id\", \"follower_party_id\"),\
        \FOREIGN KEY(\"artist_id\") REFERENCES \"social_artist_profile\"(\"id\") ON DELETE CASCADE\
        \)"
        []
