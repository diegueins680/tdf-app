{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either (isLeft)
import           Data.Time (UTCTime(..), addDays, addUTCTime, fromGregorian, secondsToDiffTime)
import           Database.Persist.Sql (toSqlKey)
import           Servant (ServerError(..))
import           Test.Hspec

import           TDF.Models.SocialEventsModels (EventInvitationId, SocialEventId)
import           TDF.RagStore (availabilityOverlaps, validateEmbeddingModelDimensions)
import           TDF.Server.SocialEventsHandlers (normalizeInvitationStatus, parseInvitationIdsEither)

main :: IO ()
main = hspec $ do
  describe "normalizeInvitationStatus" $ do
    it "falls back to pending when missing" $ do
      normalizeInvitationStatus Nothing `shouldBe` "pending"

    it "trims and lowercases non-empty statuses" $ do
      normalizeInvitationStatus (Just "  Accepted ") `shouldBe` "accepted"

    it "treats blank strings as pending" $ do
      normalizeInvitationStatus (Just "   ") `shouldBe` "pending"

  describe "parseInvitationIdsEither" $ do
    it "parses numeric ids into typed keys" $ do
      let expected :: (SocialEventId, EventInvitationId)
          expected = (toSqlKey 42, toSqlKey 77)
      parseInvitationIdsEither "42" "77" `shouldBe` Right expected

    it "returns a ServerError for invalid ids" $ do
      case parseInvitationIdsEither "x" "1" of
        Left err -> BL.unpack (errBody err) `shouldContain` "Invalid event or invitation id"
        Right _  -> expectationFailure "Expected an error for invalid ids"

  describe "availabilityOverlaps" $ do
    let day = fromGregorian 2025 1 1
        windowStart = UTCTime day (secondsToDiffTime 0)
        windowEnd = UTCTime (addDays 1 day) (secondsToDiffTime 0)

    it "includes events that overlap the start" $ do
      let eventStart = addUTCTime (-3600) windowStart
          eventEnd = addUTCTime 3600 windowStart
      availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` True

    it "includes events that overlap the end" $ do
      let eventStart = addUTCTime (-3600) windowEnd
          eventEnd = addUTCTime 3600 windowEnd
      availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` True

    it "excludes events entirely before the window" $ do
      let eventStart = addUTCTime (-7200) windowStart
          eventEnd = addUTCTime (-3600) windowStart
      availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` False

    it "excludes events entirely after the window" $ do
      let eventStart = addUTCTime 3600 windowEnd
          eventEnd = addUTCTime 7200 windowEnd
      availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` False

  describe "validateEmbeddingModelDimensions" $ do
    it "accepts known embedding models" $ do
      validateEmbeddingModelDimensions "text-embedding-3-small" `shouldBe` Right 1536
      validateEmbeddingModelDimensions "text-embedding-ada-002" `shouldBe` Right 1536
      validateEmbeddingModelDimensions "TEXT-EMBEDDING-3-LARGE" `shouldBe` Right 3072

    it "rejects unknown embedding models" $ do
      validateEmbeddingModelDimensions "mystery-embedder" `shouldSatisfy` isLeft
