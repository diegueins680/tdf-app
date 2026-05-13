{-# LANGUAGE OverloadedStrings #-}

module TDF.Social.FollowHandlerSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity (..), get, insert, insertKey)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Servant (Handler, ServerError (errBody, errHTTPCode), (:<|>) (..))
import Servant.Multipart
    ( FileData (..)
    , FromMultipart (fromMultipart)
    , Input (..)
    , MultipartData (..)
    , Tmp
    )
import Servant.Server.Internal.Handler (runHandler)
import Test.Hspec

import TDF.API.SocialEventsAPI
    ( EventImageUploadForm (..)
    , validateEventImageUploadForm
    )
import TDF.DTO.SocialEventsDTO
    ( ArtistDTO
    , ArtistFollowerDTO (..)
    , EventDTO (..)
    , EventMetadataUpdateDTO (..)
    , EventUpdateDTO (..)
    , InvitationDTO (..)
    , NullableFieldUpdate (..)
    )
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.DB (Env (..))
import TDF.Models (Party (..), RoleEnum (Fan))
import TDF.Models.SocialEventsModels
import TDF.Server.SocialEventsHandlers
    ( followArtistDb
    , resolveExistingPartyIdText
    , resolveUniqueRsvpRow
    , socialEventsServer
    , validateEventImageUploadSize
    , validateEventMetadataUpdate
    , validateEventMetadataUrlField
    , validateInvitationFromPartyId
    , validateStoredEventFinanceMetadata
    )

mkEventImageUploadMultipart :: [(T.Text, T.Text)] -> [FileData Tmp] -> MultipartData Tmp
mkEventImageUploadMultipart fields uploads =
    MultipartData
        { inputs = map (uncurry Input) fields
        , files = uploads
        }

mkEventImageUploadFile :: T.Text -> FileData Tmp
mkEventImageUploadFile fileName =
    FileData
        { fdInputName = "file"
        , fdFileName = fileName
        , fdFileCType = "image/png"
        , fdPayload = "/tmp/mock-event-image-upload"
        }

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

    it "rejects empty or oversized event image uploads before copying files" $ do
        case validateEventImageUploadSize 1 of
            Right () -> pure ()
            Left err ->
                expectationFailure
                    ("Expected valid event image upload size, got: " <> show err)

        let assertInvalid expectedMessage size =
                case validateEventImageUploadSize size of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ("Expected invalid event image upload size to be rejected, got: " <> show value)

        assertInvalid "event image upload size is invalid" (-1)
        assertInvalid "event image upload must not be empty" 0
        assertInvalid "event image upload must be 10 MB or smaller" (10 * 1024 * 1024 + 1)

    it "revalidates event image upload names before handler filename sanitization" $ do
        case fromMultipart
            ( mkEventImageUploadMultipart
                [("name", "  Poster.png  ")]
                [mkEventImageUploadFile "camera.png"]
            ) :: Either String EventImageUploadForm of
            Right form ->
                eiuName form `shouldBe` Just "Poster.png"
            Left err ->
                expectationFailure
                    ("Expected valid event image upload multipart data, got: " <> err)

        let assertInvalid expectedMessage form =
                case validateEventImageUploadForm form of
                    Left err -> T.unpack err `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ( "Expected forged event image upload form to be rejected, got: "
                                <> show (eiuName value)
                            )

        assertInvalid
            "Uploaded browser file name must not contain path separators"
            EventImageUploadForm
                { eiuFile = mkEventImageUploadFile "events/poster.png"
                , eiuName = Just "poster.png"
                }
        assertInvalid
            "Uploaded image extension must match its MIME type"
            EventImageUploadForm
                { eiuFile = mkEventImageUploadFile "camera.jpg"
                , eiuName = Just "poster.png"
                }

    it "rejects unsafe social event metadata URLs before storing public links" $ do
        validateEventMetadataUrlField
            "eventTicketUrl"
            (Just " https://tickets.example.com/event?id=42 ")
            `shouldBe` Right (Just "https://tickets.example.com/event?id=42")
        validateEventMetadataUrlField "eventImageUrl" (Just "   ")
            `shouldBe` Right Nothing

        let assertInvalid field raw expectedMessage =
                case validateEventMetadataUrlField field (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ("Expected unsafe event metadata URL to be rejected, got: " <> show value)

        assertInvalid
            "eventTicketUrl"
            "http://tickets.example.com/event"
            "eventTicketUrl must be an absolute https URL"
        assertInvalid
            "eventImageUrl"
            "https://localhost/event.jpg"
            "eventImageUrl must be an absolute https URL"
        assertInvalid
            "eventTicketUrl"
            ("https://tickets.example.com/event/" <> T.replicate 2049 "a")
            "eventTicketUrl must be 2048 characters or fewer"

        case validateEventMetadataUpdate
            emptyEventMetadataUpdate
                { emuTicketUrl = FieldValue " https://tickets.example.com/event "
                , emuImageUrl = FieldValue "javascript:alert(1)"
                } of
            Left err -> do
                errHTTPCode err `shouldBe` 400
                BL8.unpack (errBody err)
                    `shouldContain` "eventImageUrl must be an absolute https URL"
            Right value ->
                expectationFailure
                    ("Expected metadata update with unsafe image URL to fail, got: " <> show value)

    it "rejects malformed stored event finance metadata instead of falling back to USD" $ do
        let eventWithMetadata rawMetadata =
                (seedSocialEvent "1" "Finance event" socialEventStartFixture)
                    { socialEventMetadata = rawMetadata
                    }
            assertInvalid rawMetadata expectedMessage =
                case validateStoredEventFinanceMetadata (eventWithMetadata rawMetadata) of
                    Left message ->
                        T.unpack message `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ("Expected invalid stored event metadata to be rejected, got: " <> show value)

        validateStoredEventFinanceMetadata (eventWithMetadata Nothing)
            `shouldBe` Right ("USD", Nothing)
        validateStoredEventFinanceMetadata
            (eventWithMetadata (Just "{\"currency\":\"eur\",\"budgetCents\":2500}"))
            `shouldBe` Right ("EUR", Just 2500)
        assertInvalid (Just "not-json") "Stored event metadata is invalid JSON"
        assertInvalid
            (Just "{\"currency\":\"usd\",\"budgetCents\":2500,\"curency\":\"eur\"}")
            "Stored event metadata contains unknown fields: curency"
        assertInvalid (Just "{\"currency\":\"USDT\"}") "Stored event currency is invalid"
        assertInvalid (Just "{\"budgetCents\":-1}") "Stored event budget is invalid"

    it "rejects malformed stored event metadata before publishing event DTO fallbacks" $ do
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- getCurrentTime
        let eventKey :: SocialEventId
            eventKey = toSqlKey 11
        runSqlPool
            ( insertKey
                eventKey
                ( (seedSocialEvent "1" "Corrupt metadata event" now)
                    { socialEventMetadata = Just "not-json"
                    }
                )
            )
            pool

        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused by social event get tests"
                    }
        result <-
            runHandler $
                runReaderT
                    (socialEventGetHandlerFor (socialEventUser 1) "11")
                    env

        case result of
            Left err -> do
                errHTTPCode err `shouldBe` 500
                BL8.unpack (errBody err)
                    `shouldContain` "Stored event metadata is invalid JSON"
            Right value ->
                expectationFailure
                    ("Expected malformed stored event metadata to be rejected, got: " <> show value)

    it "rejects malformed stored artist social links before publishing artist DTO fallbacks" $ do
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- getCurrentTime
        let artistKey :: ArtistProfileId
            artistKey = toSqlKey 15
        runSqlPool
            ( insertKey
                artistKey
                ArtistProfile
                    { artistProfilePartyId = Nothing
                    , artistProfileName = "Corrupt Links"
                    , artistProfileBio = Nothing
                    , artistProfileAvatarUrl = Nothing
                    , artistProfileGenres = Nothing
                    , artistProfileSocialLinks = Just "{\"bandcamp\":\"https://artist.example\"}"
                    , artistProfileCreatedAt = now
                    , artistProfileUpdatedAt = now
                    }
            )
            pool

        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused by artist get tests"
                    }
        result <-
            runHandler $
                runReaderT
                    (artistGetHandlerFor (socialEventUser 1) "15")
                    env

        case result of
            Left err -> do
                errHTTPCode err `shouldBe` 500
                BL8.unpack (errBody err)
                    `shouldContain` "Stored artist social links are invalid"
            Right value ->
                expectationFailure
                    ("Expected malformed stored artist social links to be rejected, got: " <> show value)

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

    it "requires the event organizer before updating event details" $ do
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- getCurrentTime
        let eventKey :: SocialEventId
            eventKey = toSqlKey 7
        runSqlPool
            ( insertKey
                eventKey
                (seedSocialEvent "1" "Original event" now)
            )
            pool

        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused by social event update auth tests"
                    }
        result <-
            runHandler $
                runReaderT
                    ( socialEventUpdateHandlerFor
                        (socialEventUser 2)
                        "7"
                        (socialEventUpdatePayload "Hijacked event")
                    )
                    env

        case result of
            Left err -> do
                errHTTPCode err `shouldBe` 403
                BL8.unpack (errBody err)
                    `shouldContain` "Only the event organizer can manage this event"
            Right updated ->
                expectationFailure
                    ("Expected non-organizer event update to fail, got: " <> show updated)

        stored <- runSqlPool (get eventKey) pool
        fmap socialEventTitle stored `shouldBe` Just "Original event"

    it "rejects spoofed invitation senders before inserting social event invitations" $ do
        pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
        runSqlPool initializeSocialSchema pool
        now <- getCurrentTime
        let eventKey :: SocialEventId
            eventKey = toSqlKey 9
        runSqlPool
            ( insertKey
                eventKey
                (seedSocialEvent "5" "Invitation event" now)
            )
            pool

        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused by invitation auth tests"
                    }

        validateInvitationFromPartyId "5" Nothing `shouldBe` Right "5"
        validateInvitationFromPartyId "5" (Just " 005 ") `shouldBe` Right "5"

        spoofed <-
            runHandler $
                runReaderT
                    ( socialEventInvitationCreateHandlerFor
                        (socialEventUser 5)
                        "9"
                        (invitationCreatePayload (Just "999"))
                    )
                    env
        case spoofed of
            Left err -> do
                errHTTPCode err `shouldBe` 403
                BL8.unpack (errBody err)
                    `shouldContain` "invitationFromPartyId must match the authenticated party"
            Right value ->
                expectationFailure
                    ("Expected spoofed invitation sender to be rejected, got: " <> show value)

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

socialEventUpdateHandlerFor
    :: AuthedUser
    -> T.Text
    -> EventUpdateDTO
    -> ReaderT Env Handler EventDTO
socialEventUpdateHandlerFor user =
    case socialEventsServer user of
        eventsServer
            :<|> _venues
            :<|> _artists
            :<|> _rsvps
            :<|> _invitations
            :<|> _moments
            :<|> _tickets
            :<|> _budget
            :<|> _finance ->
            case eventsServer of
                _listEvents
                    :<|> _createEvent
                    :<|> _getEvent
                    :<|> updateEventHandler
                    :<|> _uploadEventImage
                    :<|> _deleteEvent ->
                    updateEventHandler

socialEventGetHandlerFor
    :: AuthedUser
    -> T.Text
    -> ReaderT Env Handler EventDTO
socialEventGetHandlerFor user =
    case socialEventsServer user of
        eventsServer
            :<|> _venues
            :<|> _artists
            :<|> _rsvps
            :<|> _invitations
            :<|> _moments
            :<|> _tickets
            :<|> _budget
            :<|> _finance ->
            case eventsServer of
                _listEvents
                    :<|> _createEvent
                    :<|> getEventHandler
                    :<|> _updateEvent
                    :<|> _uploadEventImage
                    :<|> _deleteEvent ->
                    getEventHandler

artistGetHandlerFor
    :: AuthedUser
    -> T.Text
    -> ReaderT Env Handler ArtistDTO
artistGetHandlerFor user =
    case socialEventsServer user of
        _events
            :<|> _venues
            :<|> artistsServer
            :<|> _rsvps
            :<|> _invitations
            :<|> _moments
            :<|> _tickets
            :<|> _budget
            :<|> _finance ->
            case artistsServer of
                _listArtists
                    :<|> _createArtist
                    :<|> getArtistHandler
                    :<|> _updateArtist
                    :<|> _listArtistFollowers
                    :<|> _followArtist
                    :<|> _unfollowArtist ->
                    getArtistHandler

socialEventInvitationCreateHandlerFor
    :: AuthedUser
    -> T.Text
    -> InvitationDTO
    -> ReaderT Env Handler InvitationDTO
socialEventInvitationCreateHandlerFor user eventIdText =
    case socialEventsServer user of
        _events
            :<|> _venues
            :<|> _artists
            :<|> _rsvps
            :<|> invitationsServer
            :<|> _moments
            :<|> _tickets
            :<|> _budget
            :<|> _finance ->
            case invitationsServer eventIdText of
                _listInvitations :<|> createInvitationHandler :<|> _updateInvitation ->
                    createInvitationHandler

socialEventUser :: Int64 -> AuthedUser
socialEventUser partyId =
    AuthedUser
        { auPartyId = toSqlKey partyId
        , auRoles = [Fan]
        , auModules = modulesForRoles [Fan]
        }

socialEventStartFixture :: UTCTime
socialEventStartFixture =
    UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

socialEventEndFixture :: UTCTime
socialEventEndFixture =
    UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 3600)

seedSocialEvent :: T.Text -> T.Text -> UTCTime -> SocialEvent
seedSocialEvent owner title now =
    SocialEvent
        { socialEventOrganizerPartyId = Just owner
        , socialEventTitle = title
        , socialEventDescription = Nothing
        , socialEventVenueId = Nothing
        , socialEventStartTime = socialEventStartFixture
        , socialEventEndTime = socialEventEndFixture
        , socialEventPriceCents = Nothing
        , socialEventCapacity = Nothing
        , socialEventMetadata = Nothing
        , socialEventCreatedAt = now
        , socialEventUpdatedAt = now
        }

socialEventUpdatePayload :: T.Text -> EventUpdateDTO
socialEventUpdatePayload title =
    EventUpdateDTO
        { eudEvent =
            EventDTO
                { eventId = Just "7"
                , eventOrganizerPartyId = Nothing
                , eventTitle = title
                , eventDescription = Nothing
                , eventStart = socialEventStartFixture
                , eventEnd = socialEventEndFixture
                , eventVenueId = Nothing
                , eventPriceCents = Nothing
                , eventCapacity = Nothing
                , eventTicketUrl = Nothing
                , eventImageUrl = Nothing
                , eventIsPublic = Nothing
                , eventType = Nothing
                , eventStatus = Nothing
                , eventCurrency = Nothing
                , eventBudgetCents = Nothing
                , eventCreatedAt = Nothing
                , eventUpdatedAt = Nothing
                , eventArtists = []
                }
        , eudMetadataUpdate = emptyEventMetadataUpdate
        }

invitationCreatePayload :: Maybe T.Text -> InvitationDTO
invitationCreatePayload mFromPartyId =
    InvitationDTO
        { invitationId = Nothing
        , invitationEventId = Nothing
        , invitationFromPartyId = mFromPartyId
        , invitationToPartyId = "2"
        , invitationStatus = Just "pending"
        , invitationMessage = Just "Join us"
        , invitationCreatedAt = Nothing
        , invitationUpdatedAt = Nothing
        }

emptyEventMetadataUpdate :: EventMetadataUpdateDTO
emptyEventMetadataUpdate =
    EventMetadataUpdateDTO
        { emuTicketUrl = FieldMissing
        , emuImageUrl = FieldMissing
        , emuIsPublic = FieldMissing
        , emuType = FieldMissing
        , emuStatus = FieldMissing
        , emuCurrency = FieldMissing
        , emuBudgetCents = FieldMissing
        }

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
        "CREATE TABLE IF NOT EXISTS \"artist_genre\" (\
        \\"artist_id\" INTEGER NOT NULL,\
        \\"genre\" VARCHAR NOT NULL,\
        \PRIMARY KEY (\"artist_id\", \"genre\"),\
        \FOREIGN KEY(\"artist_id\") REFERENCES \"social_artist_profile\"(\"id\") ON DELETE CASCADE\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"social_event\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"organizer_party_id\" VARCHAR NULL,\
        \\"title\" VARCHAR NOT NULL,\
        \\"description\" VARCHAR NULL,\
        \\"venue_id\" INTEGER NULL,\
        \\"start_time\" TIMESTAMP NOT NULL,\
        \\"end_time\" TIMESTAMP NOT NULL,\
        \\"price_cents\" INTEGER NULL,\
        \\"capacity\" INTEGER NULL,\
        \\"metadata\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"event_artist\" (\
        \\"event_id\" INTEGER NOT NULL,\
        \\"artist_id\" INTEGER NOT NULL,\
        \\"role\" VARCHAR NULL,\
        \PRIMARY KEY (\"event_id\", \"artist_id\"),\
        \FOREIGN KEY(\"event_id\") REFERENCES \"social_event\"(\"id\") ON DELETE CASCADE,\
        \FOREIGN KEY(\"artist_id\") REFERENCES \"social_artist_profile\"(\"id\") ON DELETE CASCADE\
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
