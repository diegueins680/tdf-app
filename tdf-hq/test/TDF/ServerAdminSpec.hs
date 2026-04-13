{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerAdminSpec (spec) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (Value, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Database.Persist.Sql (toSqlKey)
import Servant (NoContent (..), ServerError (errBody, errHTTPCode), (:<|>) (..))
import Test.Hspec

import TDF.API.Admin
    ( AdminWhatsAppResendRequest (..)
    , AdminWhatsAppSendRequest (..)
    , EmailTestRequest (..)
    , SocialUnholdRequest (..)
    )
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.DB (Env (..))
import TDF.DTO (LogEntryDTO)
import TDF.Models (RoleEnum (..))
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

dummyEnv :: Env
dummyEnv =
    Env
        { envPool = error "envPool should be unused in ServerAdminSpec"
        , envConfig = error "envConfig should be unused in ServerAdminSpec"
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
