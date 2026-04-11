{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerAdminSpec (spec) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Servant (ServerError (errBody, errHTTPCode))
import Test.Hspec

import TDF.API.Admin (EmailTestRequest (..))
import TDF.ServerAdmin (
    buildAdminUsernameCandidate,
    dedupeAdminEmailRecipients,
    normalizeAdminEmailAddress,
    normalizeAdminEmailBodyLines,
    normalizeAdminUsername,
    SocialUnholdLookup (..),
    validateSocialUnholdLookup,
    validateAdminWhatsAppSendMode,
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

        it "rejects usernames that would need lossy rewriting or exceed the storage limit" $ do
            normalizeAdminUsername "   " `shouldBe` Nothing
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
  where
    decodeEmailTest :: BL8.ByteString -> Either String EmailTestRequest
    decodeEmailTest = eitherDecode
    isLeft (Left _) = True
    isLeft (Right _) = False
