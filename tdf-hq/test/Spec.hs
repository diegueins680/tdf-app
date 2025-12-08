{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Database.Persist.Sql (toSqlKey)
import           Servant (ServerError(..))
import           Test.Hspec

import           TDF.Models.SocialEventsModels (EventInvitationId, SocialEventId)
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
