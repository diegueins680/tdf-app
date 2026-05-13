{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerFanClubSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Int (Int64)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist (Entity (..))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Servant (ServerError (errBody, errHTTPCode))
import Test.Hspec

import TDF.Models
  ( ElectionStatus (Upcoming)
  , FanClubCandidacy (..)
  , FanClubElection (..)
  , FanClubOfficerRole (Coordinator, Secretary)
  , FanClubPost (..)
  )
import TDF.ServerFanClub
  ( validateFanClubArtistPathId
  , validateFanClubCandidacyPathId
  , validateFanClubElectionMutationTarget
  , validateFanClubElectionPathId
  , validateFanClubOfficerRoleInput
  , validateFanClubReplyParentTarget
  , validateFanClubVoteCandidacyTarget
  )

spec :: Spec
spec = do
  describe "fan club artist path validation" $
    it "rejects malformed artist ids before public club lookups can look like missing clubs" $ do
      case validateFanClubArtistPathId 42 of
        Right artistId -> fromSqlKey artistId `shouldBe` 42
        Left err -> expectationFailure (unexpectedRejection err)

      assertRejected 400 "Invalid fan club artist id" $
        validateFanClubArtistPathId 0
      assertRejected 400 "Invalid fan club artist id" $
        validateFanClubArtistPathId (-7)

  describe "fan club election path validation" $ do
    it "rejects malformed election and candidacy ids before DB fallback lookup" $ do
      case validateFanClubElectionPathId 17 of
        Right electionId -> fromSqlKey electionId `shouldBe` 17
        Left err -> expectationFailure (unexpectedRejection err)
      case validateFanClubCandidacyPathId 23 of
        Right candidacyId -> fromSqlKey candidacyId `shouldBe` 23
        Left err -> expectationFailure (unexpectedRejection err)

      assertRejected 400 "Invalid fan club election id" $
        validateFanClubElectionPathId 0
      assertRejected 400 "Invalid fan club candidacy id" $
        validateFanClubCandidacyPathId (-1)

  describe "validateFanClubElectionMutationTarget" $ do
    it "requires URL artist-club ownership before mutating an election" $ do
      case validateFanClubElectionMutationTarget
        (toSqlKey 10)
        (Entity (toSqlKey 20) (mkElection 10)) of
          Right electionId -> fromSqlKey electionId `shouldBe` 20
          Left err -> expectationFailure (unexpectedRejection err)

      assertRejected 404 "Fan club election not found" $
        validateFanClubElectionMutationTarget
          (toSqlKey 11)
          (Entity (toSqlKey 20) (mkElection 10))

  describe "validateFanClubVoteCandidacyTarget" $ do
    it "requires vote candidates to belong to the targeted election" $ do
      case validateFanClubVoteCandidacyTarget
        (toSqlKey 20)
        (Entity (toSqlKey 30) (mkCandidacy 20)) of
          Right candidacyId -> fromSqlKey candidacyId `shouldBe` 30
          Left err -> expectationFailure (unexpectedRejection err)

      assertRejected 404 "Fan club candidacy not found" $
        validateFanClubVoteCandidacyTarget
          (toSqlKey 21)
          (Entity (toSqlKey 30) (mkCandidacy 20))

  describe "validateFanClubReplyParentTarget" $ do
    it "requires reply parents to be top-level posts in the requested club" $ do
      case validateFanClubReplyParentTarget
        (toSqlKey 10)
        (Entity (toSqlKey 50) (mkPost 10 Nothing)) of
          Right postId -> fromSqlKey postId `shouldBe` 50
          Left err -> expectationFailure (unexpectedRejection err)

      assertRejected 404 "Fan club post not found" $
        validateFanClubReplyParentTarget
          (toSqlKey 10)
          (Entity (toSqlKey 51) (mkPost 11 Nothing))
      assertRejected 400 "top-level post" $
        validateFanClubReplyParentTarget
          (toSqlKey 10)
          (Entity (toSqlKey 52) (mkPost 10 (Just 50)))

  describe "validateFanClubOfficerRoleInput" $ do
    it "rejects typoed candidacy roles instead of falling back to coordinator" $ do
      case validateFanClubOfficerRoleInput "  Secretario  " of
        Right Secretary -> pure ()
        Right role -> expectationFailure ("Expected Secretary, got " <> show role)
        Left err -> expectationFailure (unexpectedRejection err)
      assertRejected 400 "role is required" $
        validateFanClubOfficerRoleInput "  "
      assertRejected 400 "role must be one of" $
        validateFanClubOfficerRoleInput "secretary"

mkElection :: Int64 -> FanClubElection
mkElection clubId =
  FanClubElection
    { fanClubElectionClubId = toSqlKey clubId
    , fanClubElectionYear = 2026
    , fanClubElectionCandidacyStartsAt = Nothing
    , fanClubElectionCandidacyEndsAt = Nothing
    , fanClubElectionVotingStartsAt = Nothing
    , fanClubElectionVotingEndsAt = Nothing
    , fanClubElectionStatus = Upcoming
    , fanClubElectionCreatedAt = testTime
    }

mkCandidacy :: Int64 -> FanClubCandidacy
mkCandidacy electionId =
  FanClubCandidacy
    { fanClubCandidacyElectionId = toSqlKey electionId
    , fanClubCandidacyFanPartyId = toSqlKey 40
    , fanClubCandidacyRole = Coordinator
    , fanClubCandidacyManifesto = Nothing
    , fanClubCandidacyCreatedAt = testTime
    }

mkPost :: Int64 -> Maybe Int64 -> FanClubPost
mkPost clubId parentId =
  FanClubPost
    { fanClubPostClubId = toSqlKey clubId
    , fanClubPostFanPartyId = toSqlKey 40
    , fanClubPostParentId = fmap toSqlKey parentId
    , fanClubPostTitle = Just "Club note"
    , fanClubPostContent = "Visible to club members"
    , fanClubPostIsPinned = False
    , fanClubPostIsHidden = False
    , fanClubPostCreatedAt = testTime
    , fanClubPostUpdatedAt = Nothing
    }

testTime :: UTCTime
testTime = UTCTime (fromGregorian 2026 5 12) (secondsToDiffTime 0)

assertRejected :: Int -> String -> Either ServerError a -> Expectation
assertRejected expectedStatus expectedBody result =
  case result of
    Left err -> do
      errHTTPCode err `shouldBe` expectedStatus
      BL8.unpack (errBody err) `shouldContain` expectedBody
    Right _ ->
      expectationFailure "Expected validation to reject the fan-club target"

unexpectedRejection :: ServerError -> String
unexpectedRejection err =
  "Unexpected rejection: "
    <> show (errHTTPCode err)
    <> " "
    <> BL8.unpack (errBody err)
