{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerSpec (spec) where

import           Test.Hspec
import           TDF.Server (normalizeOptionalInput, parseStatusWithDefault)
import           TDF.Models (BookingStatus(..))

spec :: Spec
spec = describe "TDF.Server helpers" $ do
  describe "normalizeOptionalInput" $ do
    it "returns Nothing when input is Nothing" $
      normalizeOptionalInput Nothing `shouldBe` Nothing

    it "trims whitespace and preserves meaningful content" $
      normalizeOptionalInput (Just "   Live Room  ") `shouldBe` Just "Live Room"

    it "drops strings that only contain whitespace" $
      normalizeOptionalInput (Just "   ") `shouldBe` Nothing

  describe "parseStatusWithDefault" $ do
    it "parses a known status and ignores surrounding whitespace" $
      parseStatusWithDefault Tentative "  InProgress   " `shouldBe` InProgress

    it "falls back to the provided status when parsing fails" $
      parseStatusWithDefault Confirmed "not-a-status" `shouldBe` Confirmed
