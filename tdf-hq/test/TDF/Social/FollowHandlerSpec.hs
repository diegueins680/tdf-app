{-# LANGUAGE OverloadedStrings #-}

module TDF.Social.FollowHandlerSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (..), insert)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Servant (ServerError (errBody, errHTTPCode))
import Test.Hspec

import TDF.DTO.SocialEventsDTO (ArtistFollowerDTO (..))
import TDF.Models (Party (..))
import TDF.Models.SocialEventsModels
import TDF.Server.SocialEventsHandlers (followArtistDb, resolveExistingPartyIdText, resolveUniqueRsvpRow)

spec :: Spec
spec = describe "social event handler helpers" $ do
    it "rejects duplicate RSVP rows instead of updating an arbitrary existing match" $ do
        now <- getCurrentTime
        let rsvpRow rowId status =
                Entity
                    (toSqlKey rowId)
                    EventRsvp
                        { eventRsvpEventId = toSqlKey 7
                        , eventRsvpPartyId = "42"
                        , eventRsvpStatus = status
                        , eventRsvpMetadata = Nothing
                        , eventRsvpCreatedAt = now
                        , eventRsvpUpdatedAt = now
                        }

        case resolveUniqueRsvpRow [rsvpRow 1 "accepted", rsvpRow 2 "maybe"] of
            Left err -> do
                errHTTPCode err `shouldBe` 409
                BL8.unpack (errBody err) `shouldContain` "Multiple RSVP rows exist"
            Right value ->
                expectationFailure ("Expected duplicate RSVP rows to be rejected, got: " <> show value)

    it "rejects unknown follower party ids before the handler can create orphan follows or RSVPs" $ do
        pool <- runStdoutLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool

        unknownResult <- resolveExistingPartyIdText pool "followerPartyId" "42"
        case unknownResult of
            Left err -> do
                errHTTPCode err `shouldBe` 422
                BL8.unpack (errBody err) `shouldContain` "followerPartyId references an unknown party"
            Right value ->
                expectationFailure ("Expected missing follower party to be rejected, got: " <> show value)

        now <- liftIO getCurrentTime
        existingPartyId <-
            runSqlPool
                ( insert
                    Party
                        { partyLegalName = Nothing
                        , partyDisplayName = "Follower"
                        , partyIsOrg = False
                        , partyTaxId = Nothing
                        , partyPrimaryEmail = Just "follower@example.com"
                        , partyPrimaryPhone = Nothing
                        , partyWhatsapp = Nothing
                        , partyInstagram = Nothing
                        , partyEmergencyContact = Nothing
                        , partyNotes = Nothing
                        , partyCreatedAt = now
                        }
                )
                pool

        let existingPartyText = T.pack (show (fromSqlKey existingPartyId))
        resolveExistingPartyIdText pool "followerPartyId" (" 00" <> existingPartyText <> " ")
            `shouldReturn` Right existingPartyText

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
