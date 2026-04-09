{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerAdminSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Servant (ServerError (errBody, errHTTPCode))
import Test.Hspec

import TDF.ServerAdmin (
    dedupeAdminEmailRecipients,
    normalizeAdminEmailAddress,
    normalizeAdminEmailBodyLines,
    SocialUnholdLookup (..),
    validateSocialUnholdLookup,
    validateAdminWhatsAppSendMode,
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

    describe "normalizeAdminEmailBodyLines" $ do
        it "trims lines and drops blanks" $
            normalizeAdminEmailBodyLines ["  Hola  ", "", "   ", " Link: https://example.com  "]
                `shouldBe` ["Hola", "Link: https://example.com"]

    describe "dedupeAdminEmailRecipients" $ do
        it "keeps the first valid recipient for duplicate emails and drops malformed addresses" $
            dedupeAdminEmailRecipients
                [ ("Ada", " ADA@example.com ")
                , ("Broken", "not-an-email")
                , ("Ignored duplicate", "ada@example.com")
                , ("Linus", "linus@example.com")
                , ("Missing dot", "linus@example")
                , ("Blank", "   ")
                ]
                `shouldBe`
                [ ("Ada", "ada@example.com")
                , ("Linus", "linus@example.com")
                ]

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
