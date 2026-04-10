{-# LANGUAGE OverloadedStrings #-}

module TDF.WhatsApp.HistorySpec (spec) where

import Test.Hspec

import TDF.WhatsApp.History (normalizeWhatsAppPhone)

spec :: Spec
spec = describe "TDF.WhatsApp.History.normalizeWhatsAppPhone" $ do
  it "normalizes plausible WhatsApp phone inputs to an E.164-style value" $ do
    normalizeWhatsAppPhone " +593 99 123 4567 " `shouldBe` Just "+593991234567"
    normalizeWhatsAppPhone "(02) 555-0123" `shouldBe` Just "+025550123"

  it "rejects mixed-text or implausible phone inputs instead of deriving misleading matches" $ do
    normalizeWhatsAppPhone "call me at 099 123 4567" `shouldBe` Nothing
    normalizeWhatsAppPhone "12345" `shouldBe` Nothing
    normalizeWhatsAppPhone "+1234567890123456" `shouldBe` Nothing
    normalizeWhatsAppPhone "593+991234567" `shouldBe` Nothing
