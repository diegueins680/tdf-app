{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerAdminSpec (spec) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (Value, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist (Entity (..), get, insert, selectList)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Servant (NoContent (..), ServerError (errBody, errHTTPCode), (:<|>) (..))
import Test.Hspec

import TDF.API.Admin
    ( AdminEmailBroadcastRequest
    , AdminEmailBroadcastResponse
    , BrainEntryCreate (..)
    , BrainEntryUpdate (..)
    , AdminWhatsAppResendRequest (..)
    , AdminWhatsAppSendRequest (..)
    , AdminWhatsAppSendResponse
    , EmailTestRequest (..)
    , SocialUnholdRequest (..)
    , UserCommunicationHistoryDTO
    )
import TDF.API.Types (UserAccountCreate (..), UserAccountDTO, UserAccountUpdate (..))
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.Config (loadConfig)
import TDF.DB (Env (..))
import TDF.DTO
    ( ArtistProfileDTO
    , ArtistProfileUpsert (..)
    , ArtistReleaseDTO
    , ArtistReleaseUpsert (..)
    , LogEntryDTO
    )
import TDF.Models (Party (..), RoleEnum (..), UserCredential (..))
import qualified TDF.ModelsExtra as ME
import TDF.ServerAdmin (
    adminServer,
    buildAdminUsernameCandidate,
    dedupeAdminEmailRecipients,
    normalizeAdminEmailAddress,
    normalizeAdminEmailBodyLines,
    normalizeAdminUsername,
    SocialUnholdLookup (..),
    validateSocialUnholdLookup,
    validateAdminWhatsAppSendMode,
    validateAdminEmailBroadcastLimit,
    validateAdminLogsLimit,
    validateUserCommunicationHistoryLimit,
    validateOptionalAdminUsername,
  )

spec :: Spec
spec = describe "TDF.ServerAdmin email broadcast helpers" $ do
    describe "normalizeAdminEmailAddress" $ do
        it "trims and lowercases valid email addresses" $ do
            normalizeAdminEmailAddress " ADA@example.com " `shouldBe` Just "ada@example.com"

        it "rejects blank or malformed email addresses" $ do
            normalizeAdminEmailAddress "   " `shouldBe` Nothing
            normalizeAdminEmailAddress "ada.example.com" `shouldBe` Nothing
            normalizeAdminEmailAddress "ada@example" `shouldBe` Nothing
            normalizeAdminEmailAddress "ada@ example.com" `shouldBe` Nothing
            normalizeAdminEmailAddress ".ada@example.com" `shouldBe` Nothing
            normalizeAdminEmailAddress "ada.@example.com" `shouldBe` Nothing
            normalizeAdminEmailAddress "ada..ops@example.com" `shouldBe` Nothing
            normalizeAdminEmailAddress "ada()@example.com" `shouldBe` Nothing
            normalizeAdminEmailAddress "ada@example..com" `shouldBe` Nothing
            normalizeAdminEmailAddress "ada@-example.com" `shouldBe` Nothing
            normalizeAdminEmailAddress "ada@example-.com" `shouldBe` Nothing

    describe "normalizeAdminEmailBodyLines" $ do
        it "trims lines and drops blanks" $
            normalizeAdminEmailBodyLines ["  Hola  ", "", "   ", " Link: https://example.com  "]
                `shouldBe` ["Hola", "Link: https://example.com"]

    describe "normalizeAdminUsername" $ do
        it "canonicalizes explicit usernames when they are already in the supported login shape" $ do
            normalizeAdminUsername " Ada.Example " `shouldBe` Just "ada.example"

        it "rejects usernames that would need lossy rewriting, have no identifier characters, or exceed the storage limit" $ do
            normalizeAdminUsername "   " `shouldBe` Nothing
            normalizeAdminUsername "._-" `shouldBe` Nothing
            normalizeAdminUsername "!!!" `shouldBe` Nothing
            normalizeAdminUsername " Team Lead! " `shouldBe` Nothing
            normalizeAdminUsername (T.replicate 61 "a") `shouldBe` Nothing

    describe "buildAdminUsernameCandidate" $ do
        it "preserves collision suffixes within the 60-character username budget" $ do
            let candidate = buildAdminUsernameCandidate (T.replicate 60 "a") 12
            T.length candidate `shouldBe` 60
            candidate `shouldBe` (T.replicate 57 "a" <> "-12")

    describe "validateOptionalAdminUsername" $ do
        it "keeps omitted usernames unset and normalizes explicit values" $ do
            validateOptionalAdminUsername Nothing `shouldBe` Right Nothing
            validateOptionalAdminUsername (Just " Ada.Example ")
                `shouldBe` Right (Just "ada.example")

        it "rejects blank, unsupported, or too-long usernames instead of mutating them implicitly" $ do
            let assertInvalid expectedMessage result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure ("Expected invalid admin username to be rejected, got " <> show value)
            assertInvalid
                "Username must contain at least one letter, number, dot, dash, or underscore"
                (validateOptionalAdminUsername (Just "   "))
            assertInvalid
                "Username must include at least one letter or number"
                (validateOptionalAdminUsername (Just "._-"))
            assertInvalid
                "Username may only contain letters, numbers, dots, dashes, or underscores"
                (validateOptionalAdminUsername (Just " Team Lead! "))
            assertInvalid
                "Username may only contain letters, numbers, dots, dashes, or underscores"
                (validateOptionalAdminUsername (Just "!!!"))
            assertInvalid
                "Username must be 60 characters or fewer"
                (validateOptionalAdminUsername (Just (T.replicate 61 "a")))

    describe "dedupeAdminEmailRecipients" $ do
        it "keeps the first valid recipient for duplicate emails and drops malformed addresses" $
            dedupeAdminEmailRecipients
                [ ("Ada", " ADA@example.com ")
                , ("Broken", "not-an-email")
                , ("Broken local", ".ada@example.com")
                , ("Broken domain", "ada@example..com")
                , ("Ignored duplicate", "ada@example.com")
                , ("Linus", "linus@example.com")
                , ("Missing dot", "linus@example")
                , ("Blank", "   ")
                ]
                `shouldBe`
                [ ("Ada", "ada@example.com")
                , ("Linus", "linus@example.com")
                ]

    describe "EmailTestRequest FromJSON" $ do
        it "accepts public wire keys instead of leaking internal record prefixes into the API contract" $
            case eitherDecode
                "{\"email\":\"ada@example.com\",\"name\":\"Ada\",\"subject\":\"Hola\",\"body\":\"Prueba\",\"ctaUrl\":\"https://example.com\"}" of
                Left err ->
                    expectationFailure ("Expected canonical email-test payload to decode, got: " <> err)
                Right payload -> do
                    etrEmail payload `shouldBe` "ada@example.com"
                    etrName payload `shouldBe` Just "Ada"
                    etrSubject payload `shouldBe` Just "Hola"
                    etrBody payload `shouldBe` Just "Prueba"
                    etrCtaUrl payload `shouldBe` Just "https://example.com"

        it "rejects prefixed or unexpected keys so malformed email-test bodies fail explicitly" $ do
            decodeEmailTest "{\"etrEmail\":\"ada@example.com\"}" `shouldSatisfy` isLeft
            decodeEmailTest "{\"email\":\"ada@example.com\",\"unexpected\":true}" `shouldSatisfy` isLeft

    describe "SocialUnholdRequest FromJSON" $ do
        it "accepts canonical admin wire keys for social unhold lookups" $
            case eitherDecode
                "{\"channel\":\"whatsapp\",\"senderId\":\"wa:+593999000111\",\"note\":\"retry latest reply\"}" of
                Left err ->
                    expectationFailure ("Expected canonical social unhold payload to decode, got: " <> err)
                Right payload -> do
                    surChannel payload `shouldBe` "whatsapp"
                    surExternalId payload `shouldBe` Nothing
                    surSenderId payload `shouldBe` Just "wa:+593999000111"
                    surNote payload `shouldBe` Just "retry latest reply"

        it "rejects prefixed or unexpected keys instead of silently accepting malformed admin payloads" $ do
            decodeSocialUnhold "{\"surChannel\":\"whatsapp\",\"surSenderId\":\"wa:+593999000111\"}" `shouldSatisfy` isLeft
            decodeSocialUnhold "{\"channel\":\"whatsapp\",\"senderId\":\"wa:+593999000111\",\"unexpected\":true}" `shouldSatisfy` isLeft

    describe "AdminWhatsAppSendRequest FromJSON" $ do
        it "accepts canonical admin wire keys for WhatsApp sends" $
            case eitherDecode
                "{\"message\":\"Hola\",\"mode\":\"reply\",\"replyToMessageId\":42}" of
                Left err ->
                    expectationFailure ("Expected canonical WhatsApp send payload to decode, got: " <> err)
                Right payload -> do
                    awsrMessage payload `shouldBe` "Hola"
                    awsrMode payload `shouldBe` "reply"
                    awsrReplyToMessageId payload `shouldBe` Just 42

        it "rejects prefixed or unexpected keys so malformed WhatsApp send bodies fail explicitly" $ do
            decodeWhatsAppSend
                "{\"awsrMessage\":\"Hola\",\"awsrMode\":\"reply\",\"awsrReplyToMessageId\":42}"
                `shouldSatisfy` isLeft
            decodeWhatsAppSend
                "{\"message\":\"Hola\",\"mode\":\"notify\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "AdminWhatsAppResendRequest FromJSON" $ do
        it "accepts canonical admin wire keys for WhatsApp resends" $
            case eitherDecode "{\"message\":\"Reintentando\"}" of
                Left err ->
                    expectationFailure ("Expected canonical WhatsApp resend payload to decode, got: " <> err)
                Right payload ->
                    awrrMessage payload `shouldBe` Just "Reintentando"

        it "rejects prefixed or unexpected keys so malformed WhatsApp resend bodies fail explicitly" $ do
            decodeWhatsAppResend "{\"awrrMessage\":\"Hola\"}" `shouldSatisfy` isLeft
            decodeWhatsAppResend "{\"message\":\"Hola\",\"unexpected\":true}" `shouldSatisfy` isLeft

    describe "UserAccount payload FromJSON" $ do
        it "accepts canonical admin wire keys for user create and update payloads" $ do
            case decodeUserAccountCreate
                "{\"uacPartyId\":42,\"uacUsername\":\"ada.example\",\"uacPassword\":\"TempPass123!\",\"uacActive\":true,\"uacRoles\":[\"Admin\",\"Teacher\"]}" of
                Left err ->
                    expectationFailure ("Expected canonical user create payload to decode, got: " <> err)
                Right payload -> do
                    uacPartyId payload `shouldBe` 42
                    uacUsername payload `shouldBe` Just "ada.example"
                    uacPassword payload `shouldBe` Just "TempPass123!"
                    uacActive payload `shouldBe` Just True
                    uacRoles payload `shouldBe` Just [Admin, Teacher]

            case decodeUserAccountUpdate
                "{\"uauUsername\":\"ada.ops\",\"uauPassword\":\"NextPass123!\",\"uauActive\":false,\"uauRoles\":[\"ReadOnly\"]}" of
                Left err ->
                    expectationFailure ("Expected canonical user update payload to decode, got: " <> err)
                Right payload -> do
                    uauUsername payload `shouldBe` Just "ada.ops"
                    uauPassword payload `shouldBe` Just "NextPass123!"
                    uauActive payload `shouldBe` Just False
                    uauRoles payload `shouldBe` Just [ReadOnly]

        it "rejects mixed-prefix or unexpected keys so admin user writes fail explicitly" $ do
            decodeUserAccountCreate
                "{\"uacPartyId\":42,\"uacUsername\":\"ada.example\",\"roles\":[\"Admin\"]}"
                `shouldSatisfy` isLeft
            decodeUserAccountCreate
                "{\"uacPartyId\":42,\"uacUsername\":\"ada.example\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodeUserAccountUpdate
                "{\"uauUsername\":\"ada.ops\",\"active\":false}"
                `shouldSatisfy` isLeft
            decodeUserAccountUpdate
                "{\"uauUsername\":\"ada.ops\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "BrainEntry payload FromJSON" $ do
        it "accepts canonical brain entry create and update keys while preserving explicit null clears" $ do
            case decodeBrainEntryCreate
                "{\"becTitle\":\"Runbook\",\"becBody\":\"Keep this handy\",\"becCategory\":\"ops\",\"becTags\":[\"docs\",\"incident\"],\"becActive\":true}" of
                Left err ->
                    expectationFailure ("Expected canonical brain entry create payload to decode, got: " <> err)
                Right payload -> do
                    becTitle payload `shouldBe` "Runbook"
                    becBody payload `shouldBe` "Keep this handy"
                    becCategory payload `shouldBe` Just "ops"
                    becTags payload `shouldBe` Just ["docs", "incident"]
                    becActive payload `shouldBe` Just True

            case decodeBrainEntryUpdate
                "{\"beuTitle\":\"Updated runbook\",\"beuCategory\":null,\"beuTags\":[\"ops\"],\"beuActive\":false}" of
                Left err ->
                    expectationFailure ("Expected canonical brain entry update payload to decode, got: " <> err)
                Right payload -> do
                    beuTitle payload `shouldBe` Just "Updated runbook"
                    beuCategory payload `shouldBe` Just Nothing
                    beuTags payload `shouldBe` Just ["ops"]
                    beuActive payload `shouldBe` Just False

        it "rejects unexpected brain entry keys so admin writes fail before turning into silent partial updates" $ do
            decodeBrainEntryCreate
                "{\"becTitle\":\"Runbook\",\"becBody\":\"Keep this handy\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodeBrainEntryUpdate
                "{\"beuTitle\":\"Updated runbook\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "validateAdminWhatsAppSendMode" $ do
        it "normalizes supported modes and preserves valid reply semantics" $ do
            validateAdminWhatsAppSendMode " reply " (Just 42) `shouldBe` Right "reply"
            validateAdminWhatsAppSendMode "NOTIFY" Nothing `shouldBe` Right "notify"

        it "rejects invalid or contradictory reply metadata instead of silently ignoring it" $ do
            let assertInvalid expectedMessage result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure ("Expected invalid WhatsApp send mode error, got " <> show value)
            assertInvalid "reply|notify" (validateAdminWhatsAppSendMode "broadcast" Nothing)
            assertInvalid "replyToMessageId requerido" (validateAdminWhatsAppSendMode "reply" Nothing)
            assertInvalid "entero positivo" (validateAdminWhatsAppSendMode "reply" (Just 0))
            assertInvalid "solo se permite en mode=reply" (validateAdminWhatsAppSendMode "notify" (Just 99))

    describe "validateUserCommunicationHistoryLimit" $ do
        it "defaults omitted limits and accepts explicit values inside the supported history window" $ do
            validateUserCommunicationHistoryLimit Nothing `shouldBe` Right 150
            validateUserCommunicationHistoryLimit (Just 1) `shouldBe` Right 1
            validateUserCommunicationHistoryLimit (Just 300) `shouldBe` Right 300

        it "rejects out-of-range limits instead of silently clamping admin history queries" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` "limit must be between 1 and 300"
                    Right value ->
                        expectationFailure ("Expected invalid user communication history limit, got " <> show value)
            assertInvalid (validateUserCommunicationHistoryLimit (Just 0))
            assertInvalid (validateUserCommunicationHistoryLimit (Just 301))
            assertInvalid (validateUserCommunicationHistoryLimit (Just (-5)))

    describe "validateAdminLogsLimit" $ do
        it "defaults omitted limits and accepts explicit values inside the retained log window" $ do
            validateAdminLogsLimit Nothing `shouldBe` Right 100
            validateAdminLogsLimit (Just 1) `shouldBe` Right 1
            validateAdminLogsLimit (Just 1000) `shouldBe` Right 1000

        it "rejects out-of-range limits instead of silently returning a partial or empty log slice" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` "limit must be between 1 and 1000"
                    Right value ->
                        expectationFailure ("Expected invalid admin logs limit, got " <> show value)
            assertInvalid (validateAdminLogsLimit (Just 0))
            assertInvalid (validateAdminLogsLimit (Just (-3)))
            assertInvalid (validateAdminLogsLimit (Just 1001))

    describe "validateAdminEmailBroadcastLimit" $ do
        it "keeps omitted limits unset and accepts explicit values inside the supported send window" $ do
            validateAdminEmailBroadcastLimit Nothing `shouldBe` Right Nothing
            validateAdminEmailBroadcastLimit (Just 1) `shouldBe` Right (Just 1)
            validateAdminEmailBroadcastLimit (Just 5000) `shouldBe` Right (Just 5000)

        it "rejects out-of-range limits instead of silently clamping broadcast batches" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` "limit must be between 1 and 5000"
                    Right value ->
                        expectationFailure ("Expected invalid email broadcast limit, got " <> show value)
            assertInvalid (validateAdminEmailBroadcastLimit (Just 0))
            assertInvalid (validateAdminEmailBroadcastLimit (Just (-3)))
            assertInvalid (validateAdminEmailBroadcastLimit (Just 5001))

    describe "admin lookup id validation" $ do
        it "rejects non-positive user ids before admin user lookups can degrade malformed input into 404s" $ do
            let _listUsers :<|> _createUser :<|> userById = usersHandlersFor (mkUser [Admin])
                getUserHandler :<|> _updateUserHandler = userById 0

            result <- runAdminTest getUserHandler
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "userId must be a positive integer"
                Right value ->
                    expectationFailure ("Expected invalid admin user lookup id to be rejected, got " <> show value)

        it "rejects non-positive communications ids before admin WhatsApp handlers touch the database" $ do
            let historyHandler :<|> _sendHandler :<|> resendHandler :<|> _broadcastHandler =
                    communicationsHandlersFor (mkUser [Admin])

            historyResult <- runAdminTest (historyHandler (-7) Nothing)
            case historyResult of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "userId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid communication history user id to be rejected, got " <> show value)

            resendResult <- runAdminTest (resendHandler 0 (AdminWhatsAppResendRequest Nothing))
            case resendResult of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "messageId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid WhatsApp resend message id to be rejected, got " <> show value)

        it "rejects non-positive artist and release ids before admin artist writes hit the database" $ do
            let artistProfiles :<|> artistReleases = artistsHandlersFor (mkUser [Admin])
                _listProfiles :<|> upsertArtistProfile = artistProfiles
                createArtistRelease :<|> updateArtistRelease = artistReleases
                invalidArtistProfilePayload =
                    ArtistProfileUpsert
                        { apuArtistId = 0
                        , apuDisplayName = Just "Ada Artist"
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
                invalidArtistReleasePayload =
                    ArtistReleaseUpsert
                        { aruArtistId = 0
                        , aruTitle = "Debut"
                        , aruReleaseDate = Nothing
                        , aruDescription = Nothing
                        , aruCoverImageUrl = Nothing
                        , aruSpotifyUrl = Nothing
                        , aruYoutubeUrl = Nothing
                        }
                validArtistReleasePayload =
                    invalidArtistReleasePayload { aruArtistId = 7 }

            profileResult <- runAdminTest (upsertArtistProfile invalidArtistProfilePayload)
            case profileResult of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "artistId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid artist profile artist id to be rejected, got " <> show value)

            createReleaseResult <- runAdminTest (createArtistRelease invalidArtistReleasePayload)
            case createReleaseResult of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "artistId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid artist release artist id to be rejected, got " <> show value)

            updateReleaseResult <- runAdminTest (updateArtistRelease 0 validArtistReleasePayload)
            case updateReleaseResult of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "releaseId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid artist release id to be rejected, got " <> show value)

    describe "admin user creation invariants" $ do
        it "rejects creating a second credential for the same party instead of forking login state" $ do
            pool <- runStdoutLoggingT $ createSqlitePool ":memory:" 1
            runSqlPool initializeAdminUsersSchema pool
            cfg <- loadConfig
            let env = dummyEnv { envPool = pool, envConfig = cfg }
                _listUsers :<|> createUserHandler :<|> _userById = usersHandlersFor (mkUser [Admin])
                now = UTCTime (fromGregorian 2026 4 13) (secondsToDiffTime 0)
            partyKey <- runSqlPool
                (insert
                    Party
                        { partyLegalName = Nothing
                        , partyDisplayName = "Ada Existing"
                        , partyIsOrg = False
                        , partyTaxId = Nothing
                        , partyPrimaryEmail = Just "ada@example.com"
                        , partyPrimaryPhone = Nothing
                        , partyWhatsapp = Nothing
                        , partyInstagram = Nothing
                        , partyEmergencyContact = Nothing
                        , partyNotes = Nothing
                        , partyCreatedAt = now
                        }
                )
                pool
            _ <-
                runSqlPool
                    (insert
                        UserCredential
                            { userCredentialPartyId = partyKey
                            , userCredentialUsername = "ada.existing"
                            , userCredentialPasswordHash = "hashed-password"
                            , userCredentialActive = True
                            }
                    )
                    pool

            result <-
                runAdminTestWith
                    env
                    (createUserHandler
                        UserAccountCreate
                            { uacPartyId = fromSqlKey partyKey
                            , uacUsername = Just "ada.new"
                            , uacPassword = Just "TempPass123!"
                            , uacActive = Just True
                            , uacRoles = Nothing
                            }
                    )
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 409
                    BL8.unpack (errBody err) `shouldContain` "Party already has a user account"
                Right _ ->
                    expectationFailure "Expected duplicate party credential creation to be rejected"

            credentials <- runSqlPool (selectList [] []) pool :: IO [Entity UserCredential]
            length credentials `shouldBe` 1
            map (userCredentialUsername . entityVal) credentials `shouldBe` ["ada.existing"]

        it "rejects blank explicit passwords instead of silently generating a different credential" $ do
            pool <- runStdoutLoggingT $ createSqlitePool ":memory:" 1
            runSqlPool initializeAdminUsersSchema pool
            cfg <- loadConfig
            let env = dummyEnv { envPool = pool, envConfig = cfg }
                _listUsers :<|> createUserHandler :<|> _userById = usersHandlersFor (mkUser [Admin])
                now = UTCTime (fromGregorian 2026 4 13) (secondsToDiffTime 0)
            partyKey <-
                runSqlPool
                    (insert
                        Party
                            { partyLegalName = Nothing
                            , partyDisplayName = "Blank Password"
                            , partyIsOrg = False
                            , partyTaxId = Nothing
                            , partyPrimaryEmail = Just "blank-password@example.com"
                            , partyPrimaryPhone = Nothing
                            , partyWhatsapp = Nothing
                            , partyInstagram = Nothing
                            , partyEmergencyContact = Nothing
                            , partyNotes = Nothing
                            , partyCreatedAt = now
                            }
                    )
                    pool

            result <-
                runAdminTestWith
                    env
                    (createUserHandler
                        UserAccountCreate
                            { uacPartyId = fromSqlKey partyKey
                            , uacUsername = Nothing
                            , uacPassword = Just "   "
                            , uacActive = Just True
                            , uacRoles = Nothing
                            }
                    )
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "Password must not be empty"
                Right _ ->
                    expectationFailure "Expected blank create-user password to be rejected"

            credentials <- runSqlPool (selectList [] []) pool :: IO [Entity UserCredential]
            length credentials `shouldBe` 0

    describe "validateSocialUnholdLookup" $ do
        it "accepts exactly one lookup key and trims the chosen identifier" $ do
            validateSocialUnholdLookup (Just "  ig-mid-1  ") Nothing
                `shouldBe` Right (SocialUnholdByExternalId "ig-mid-1")
            validateSocialUnholdLookup Nothing (Just "  wa:+593999000111  ")
                `shouldBe` Right (SocialUnholdBySenderId "wa:+593999000111")

        it "rejects missing or contradictory lookup keys instead of silently picking one" $ do
            let assertInvalid expectedMessage result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL8.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure ("Expected invalid social unhold lookup, got " <> show value)
            assertInvalid "Provide externalId or senderId" (validateSocialUnholdLookup Nothing Nothing)
            assertInvalid "Provide externalId or senderId" (validateSocialUnholdLookup (Just "   ") (Just ""))
            assertInvalid "Provide only one of externalId or senderId"
                (validateSocialUnholdLookup (Just "ext-1") (Just "sender-1"))

    describe "social unhold route validation" $ do
        it "rejects blank channels before surfacing lookup-shape errors" $ do
            let socialUnhold :<|> _socialStatus :<|> _socialErrors = socialHandlersFor (mkUser [Admin])
                req =
                    SocialUnholdRequest
                        { surChannel = "   "
                        , surExternalId = Nothing
                        , surSenderId = Nothing
                        , surNote = Nothing
                        }

            result <- runAdminTest (socialUnhold req)
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "channel requerido"
                    BL8.unpack (errBody err) `shouldNotContain` "Provide externalId or senderId"
                Right value ->
                    expectationFailure ("Expected blank channel social unhold to be rejected, got " <> show value)

        it "rejects unsupported channels before any unhold lookup work" $ do
            let socialUnhold :<|> _socialStatus :<|> _socialErrors = socialHandlersFor (mkUser [Admin])
                req =
                    SocialUnholdRequest
                        { surChannel = "telegram"
                        , surExternalId = Just "ext-1"
                        , surSenderId = Nothing
                        , surNote = Nothing
                        }

            result <- runAdminTest (socialUnhold req)
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "channel inválido"
                Right value ->
                    expectationFailure ("Expected unsupported channel social unhold to be rejected, got " <> show value)

        it "rejects external-id unholds that only match outgoing WhatsApp messages" $ do
            pool <- runStdoutLoggingT $ createSqlitePool ":memory:" 1
            runSqlPool initializeWhatsAppAdminSchema pool
            let env = dummyEnv { envPool = pool }
                now = UTCTime (fromGregorian 2026 4 13) (secondsToDiffTime 0)
                socialUnhold :<|> _socialStatus :<|> _socialErrors = socialHandlersFor (mkUser [Admin])
                req =
                    SocialUnholdRequest
                        { surChannel = "whatsapp"
                        , surExternalId = Just "wa-outgoing-1"
                        , surSenderId = Nothing
                        , surNote = Nothing
                        }
                outgoingMsg =
                    (seedWhatsAppAdminMessage now "wa-outgoing-1" "outgoing")
                        { ME.whatsAppMessageReplyStatus = "error"
                        , ME.whatsAppMessageHoldReason = Just "should stay outbound"
                        , ME.whatsAppMessageReplyError = Just "transport failure"
                        }

            msgKey <- runSqlPool (insert outgoingMsg) pool
            result <- runAdminTestWith env (socialUnhold req)
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 404
                    BL8.unpack (errBody err)
                        `shouldContain` "No incoming whatsapp message found for externalId"
                Right value ->
                    expectationFailure
                        ("Expected outgoing-only social unhold to be rejected, got " <> show value)

            mStored <- runSqlPool (get msgKey) pool
            case mStored of
                Nothing ->
                    expectationFailure "Expected outgoing WhatsApp message to remain readable"
                Just stored -> do
                    ME.whatsAppMessageReplyStatus stored `shouldBe` "error"
                    ME.whatsAppMessageHoldReason stored `shouldBe` Just "should stay outbound"
                    ME.whatsAppMessageReplyError stored `shouldBe` Just "transport failure"

        it "resets reply hold fields for matching incoming WhatsApp messages" $ do
            pool <- runStdoutLoggingT $ createSqlitePool ":memory:" 1
            runSqlPool initializeWhatsAppAdminSchema pool
            let env = dummyEnv { envPool = pool }
                now = UTCTime (fromGregorian 2026 4 13) (secondsToDiffTime 0)
                socialUnhold :<|> _socialStatus :<|> _socialErrors = socialHandlersFor (mkUser [Admin])
                req =
                    SocialUnholdRequest
                        { surChannel = "whatsapp"
                        , surExternalId = Just "wa-incoming-1"
                        , surSenderId = Nothing
                        , surNote = Nothing
                        }
                incomingMsg =
                    (seedWhatsAppAdminMessage now "wa-incoming-1" "incoming")
                        { ME.whatsAppMessageReplyStatus = "hold"
                        , ME.whatsAppMessageHoldReason = Just "missing data"
                        , ME.whatsAppMessageHoldRequiredFields = Just "[\"email\"]"
                        , ME.whatsAppMessageLastAttemptAt = Just now
                        , ME.whatsAppMessageReplyError = Just "validation failed"
                        , ME.whatsAppMessageAttemptCount = 2
                        }

            msgKey <- runSqlPool (insert incomingMsg) pool
            result <- runAdminTestWith env (socialUnhold req)
            case result of
                Left err ->
                    expectationFailure
                        ("Expected incoming social unhold to succeed, got " <> show err)
                Right _ -> pure ()

            mStored <- runSqlPool (get msgKey) pool
            case mStored of
                Nothing ->
                    expectationFailure "Expected incoming WhatsApp message to remain readable"
                Just stored -> do
                    ME.whatsAppMessageReplyStatus stored `shouldBe` "pending"
                    ME.whatsAppMessageHoldReason stored `shouldBe` Nothing
                    ME.whatsAppMessageHoldRequiredFields stored `shouldBe` Nothing
                    ME.whatsAppMessageLastAttemptAt stored `shouldBe` Nothing
                    ME.whatsAppMessageReplyError stored `shouldBe` Nothing

    describe "admin logs route authorization" $ do
        it "requires the Admin module before listing or clearing logs" $ do
            let listLogs :<|> clearLogs = logsHandlersFor (mkUser [Fan])

            listResult <- runAdminTest (listLogs Nothing)
            case listResult of
                Left err -> do
                    errHTTPCode err `shouldBe` 403
                    BL8.unpack (errBody err) `shouldContain` "Missing required module access"
                Right value ->
                    expectationFailure ("Expected unauthorized log listing to be rejected, got " <> show value)

            clearResult <- runAdminTest clearLogs
            case clearResult of
                Left err -> do
                    errHTTPCode err `shouldBe` 403
                    BL8.unpack (errBody err) `shouldContain` "Missing required module access"
                Right NoContent ->
                    expectationFailure "Expected unauthorized log clearing to be rejected"
  where
    decodeEmailTest :: BL8.ByteString -> Either String EmailTestRequest
    decodeEmailTest = eitherDecode
    decodeSocialUnhold :: BL8.ByteString -> Either String SocialUnholdRequest
    decodeSocialUnhold = eitherDecode
    decodeWhatsAppSend :: BL8.ByteString -> Either String AdminWhatsAppSendRequest
    decodeWhatsAppSend = eitherDecode
    decodeWhatsAppResend :: BL8.ByteString -> Either String AdminWhatsAppResendRequest
    decodeWhatsAppResend = eitherDecode
    decodeUserAccountCreate :: BL8.ByteString -> Either String UserAccountCreate
    decodeUserAccountCreate = eitherDecode
    decodeUserAccountUpdate :: BL8.ByteString -> Either String UserAccountUpdate
    decodeUserAccountUpdate = eitherDecode
    decodeBrainEntryCreate :: BL8.ByteString -> Either String BrainEntryCreate
    decodeBrainEntryCreate = eitherDecode
    decodeBrainEntryUpdate :: BL8.ByteString -> Either String BrainEntryUpdate
    decodeBrainEntryUpdate = eitherDecode
    isLeft (Left _) = True
    isLeft (Right _) = False

type AdminTestM = ReaderT Env (ExceptT ServerError IO)

mkUser :: [RoleEnum] -> AuthedUser
mkUser roles =
    AuthedUser
        { auPartyId = toSqlKey 1
        , auRoles = roles
        , auModules = modulesForRoles roles
        }

runAdminTest :: AdminTestM a -> IO (Either ServerError a)
runAdminTest action = runExceptT (runReaderT action dummyEnv)

runAdminTestWith :: Env -> AdminTestM a -> IO (Either ServerError a)
runAdminTestWith env action = runExceptT (runReaderT action env)

dummyEnv :: Env
dummyEnv =
    Env
        { envPool = error "envPool should be unused in ServerAdminSpec"
        , envConfig = error "envConfig should be unused in ServerAdminSpec"
        }

initializeWhatsAppAdminSchema :: SqlPersistT IO ()
initializeWhatsAppAdminSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"whats_app_message\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"external_id\" VARCHAR NOT NULL,\
        \\"sender_id\" VARCHAR NOT NULL,\
        \\"sender_name\" VARCHAR NULL,\
        \\"party_id\" INTEGER NULL,\
        \\"actor_party_id\" INTEGER NULL,\
        \\"phone_e164\" VARCHAR NULL,\
        \\"contact_email\" VARCHAR NULL,\
        \\"text\" VARCHAR NULL,\
        \\"direction\" VARCHAR NOT NULL,\
        \\"ad_external_id\" VARCHAR NULL,\
        \\"ad_name\" VARCHAR NULL,\
        \\"campaign_external_id\" VARCHAR NULL,\
        \\"campaign_name\" VARCHAR NULL,\
        \\"metadata\" VARCHAR NULL,\
        \\"reply_status\" VARCHAR NOT NULL,\
        \\"hold_reason\" VARCHAR NULL,\
        \\"hold_required_fields\" VARCHAR NULL,\
        \\"last_attempt_at\" TIMESTAMP NULL,\
        \\"attempt_count\" INTEGER NOT NULL,\
        \\"replied_at\" TIMESTAMP NULL,\
        \\"reply_text\" VARCHAR NULL,\
        \\"reply_error\" VARCHAR NULL,\
        \\"delivery_status\" VARCHAR NOT NULL,\
        \\"delivery_updated_at\" TIMESTAMP NULL,\
        \\"delivery_error\" VARCHAR NULL,\
        \\"transport_payload\" VARCHAR NULL,\
        \\"status_payload\" VARCHAR NULL,\
        \\"source\" VARCHAR NULL,\
        \\"resend_of_message_id\" INTEGER NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \CONSTRAINT \"unique_whats_app_message\" UNIQUE (\"external_id\")\
        \)"
        []

initializeAdminUsersSchema :: SqlPersistT IO ()
initializeAdminUsersSchema = do
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
        \FOREIGN KEY(\"party_id\") REFERENCES \"party\"(\"id\")\
        \)"
        []

seedWhatsAppAdminMessage :: UTCTime -> T.Text -> T.Text -> ME.WhatsAppMessage
seedWhatsAppAdminMessage now externalId direction =
    ME.WhatsAppMessage
        { ME.whatsAppMessageExternalId = externalId
        , ME.whatsAppMessageSenderId = "+593999000111"
        , ME.whatsAppMessageSenderName = Just "Ada"
        , ME.whatsAppMessagePartyId = Nothing
        , ME.whatsAppMessageActorPartyId = Nothing
        , ME.whatsAppMessagePhoneE164 = Just "+593999000111"
        , ME.whatsAppMessageContactEmail = Nothing
        , ME.whatsAppMessageText = Just "Original message"
        , ME.whatsAppMessageDirection = direction
        , ME.whatsAppMessageAdExternalId = Nothing
        , ME.whatsAppMessageAdName = Nothing
        , ME.whatsAppMessageCampaignExternalId = Nothing
        , ME.whatsAppMessageCampaignName = Nothing
        , ME.whatsAppMessageMetadata = Nothing
        , ME.whatsAppMessageReplyStatus =
            if direction == "incoming" then "pending" else "sent"
        , ME.whatsAppMessageHoldReason = Nothing
        , ME.whatsAppMessageHoldRequiredFields = Nothing
        , ME.whatsAppMessageLastAttemptAt = Nothing
        , ME.whatsAppMessageAttemptCount = 0
        , ME.whatsAppMessageRepliedAt = Nothing
        , ME.whatsAppMessageReplyText = Nothing
        , ME.whatsAppMessageReplyError = Nothing
        , ME.whatsAppMessageDeliveryStatus =
            if direction == "incoming" then "received" else "sent"
        , ME.whatsAppMessageDeliveryUpdatedAt = Nothing
        , ME.whatsAppMessageDeliveryError = Nothing
        , ME.whatsAppMessageTransportPayload = Nothing
        , ME.whatsAppMessageStatusPayload = Nothing
        , ME.whatsAppMessageSource = Just "server_admin_spec_seed"
        , ME.whatsAppMessageResendOfMessageId = Nothing
        , ME.whatsAppMessageCreatedAt = now
        }

logsHandlersFor :: AuthedUser -> (Maybe Int -> AdminTestM [LogEntryDTO]) :<|> AdminTestM NoContent
logsHandlersFor user =
    case adminServer user of
        _seed
            :<|> _dropdowns
            :<|> _users
            :<|> _communications
            :<|> _roles
            :<|> _artists
            :<|> logsRouter
            :<|> _emailTest
            :<|> _brain
            :<|> _rag
            :<|> _social ->
                logsRouter

usersHandlersFor
    :: AuthedUser
    -> (Maybe Bool -> AdminTestM [UserAccountDTO])
        :<|> (UserAccountCreate -> AdminTestM UserAccountDTO)
        :<|> (Int64 -> AdminTestM UserAccountDTO :<|> (UserAccountUpdate -> AdminTestM UserAccountDTO))
usersHandlersFor user =
    case adminServer user of
        _seed
            :<|> _dropdowns
            :<|> usersRouter
            :<|> _communications
            :<|> _roles
            :<|> _artists
            :<|> _logs
            :<|> _emailTest
            :<|> _brain
            :<|> _rag
            :<|> _social ->
                usersRouter

artistsHandlersFor
    :: AuthedUser
    -> ((AdminTestM [ArtistProfileDTO]
        :<|> (ArtistProfileUpsert -> AdminTestM ArtistProfileDTO))
        :<|> ((ArtistReleaseUpsert -> AdminTestM ArtistReleaseDTO)
        :<|> (Int64 -> ArtistReleaseUpsert -> AdminTestM ArtistReleaseDTO)))
artistsHandlersFor user =
    case adminServer user of
        _seed
            :<|> _dropdowns
            :<|> _users
            :<|> _communications
            :<|> _roles
            :<|> artistsRouter
            :<|> _logs
            :<|> _emailTest
            :<|> _brain
            :<|> _rag
            :<|> _social ->
                artistsRouter

communicationsHandlersFor
    :: AuthedUser
    -> (Int64 -> Maybe Int -> AdminTestM UserCommunicationHistoryDTO)
        :<|> (Int64 -> AdminWhatsAppSendRequest -> AdminTestM AdminWhatsAppSendResponse)
        :<|> (Int64 -> AdminWhatsAppResendRequest -> AdminTestM AdminWhatsAppSendResponse)
        :<|> (AdminEmailBroadcastRequest -> AdminTestM AdminEmailBroadcastResponse)
communicationsHandlersFor user =
    case adminServer user of
        _seed
            :<|> _dropdowns
            :<|> _users
            :<|> communicationsRouter
            :<|> _roles
            :<|> _artists
            :<|> _logs
            :<|> _emailTest
            :<|> _brain
            :<|> _rag
            :<|> _social ->
                communicationsRouter

socialHandlersFor
    :: AuthedUser
    -> (SocialUnholdRequest -> AdminTestM Value)
        :<|> AdminTestM Value
        :<|> (Maybe T.Text -> Maybe Int -> AdminTestM Value)
socialHandlersFor user =
    case adminServer user of
        _seed
            :<|> _dropdowns
            :<|> _users
            :<|> _communications
            :<|> _roles
            :<|> _artists
            :<|> _logs
            :<|> _emailTest
            :<|> _brain
            :<|> _rag
            :<|> socialRouter ->
                socialRouter
