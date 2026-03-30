{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerAdminSpec (spec) where

import Test.Hspec

import TDF.ServerAdmin (dedupeAdminEmailRecipients, normalizeAdminEmailBodyLines)

spec :: Spec
spec = describe "TDF.ServerAdmin email broadcast helpers" $ do
    describe "normalizeAdminEmailBodyLines" $ do
        it "trims lines and drops blanks" $
            normalizeAdminEmailBodyLines ["  Hola  ", "", "   ", " Link: https://example.com  "]
                `shouldBe` ["Hola", "Link: https://example.com"]

    describe "dedupeAdminEmailRecipients" $ do
        it "keeps the first recipient for duplicate emails and normalizes casing" $
            dedupeAdminEmailRecipients
                [ ("Ada", " ADA@example.com ")
                , ("Ignored duplicate", "ada@example.com")
                , ("Linus", "linus@example.com")
                , ("Blank", "   ")
                ]
                `shouldBe`
                [ ("Ada", "ada@example.com")
                , ("Linus", "linus@example.com")
                ]
