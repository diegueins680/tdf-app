{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerFanClubSpec (spec) where

import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist (Entity (..))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Servant (ServerError (errBody, errHTTPCode), (:<|>) (..))
import Servant.Server.Internal.Handler (runHandler)
import Test.Hspec

import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.Models
  ( ElectionStatus (Upcoming)
  , FanClubCandidacy (..)
  , FanClubElection (..)
  , FanClubOfficerRole (Coordinator, Secretary)
  , FanClubPost (..)
  , RoleEnum (Customer, Fan)
  )
import TDF.ServerFanClub
  ( fanClubSecureArtistHandlers
  , validateFanClubArtistPathId
  , validateFanClubCandidacyPathId
  , validateFanClubElectionMutationTarget
  , validateFanClubElectionPathId
  , validateFanClubInboxBodyInput
  , validateFanClubInboxReplyBodyInput
  , validateFanClubOfficerRoleInput
  , validateFanClubInboxSubjectInput
  , validateFanClubInboxStatusInput
  , validateFanClubReplyParentTarget
  , validateFanClubVoteCandidacyTargets
  , validateFanClubVoteCandidacyTarget
  , validateFanClubVoteSelectionIds
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

  describe "secure fan club artist routes" $
    it "rejects malformed artist ids before authorization or DB fallback" $ do
      let getClubDetail :<|> _ =
            fanClubSecureArtistHandlers fanClubUser 0
      result <-
        runHandler $
          runReaderT
            getClubDetail
            (error "fan club artist id validation should not read Env")
      case result of
        Left err -> do
          errHTTPCode err `shouldBe` 400
          BL8.unpack (errBody err) `shouldContain` "Invalid fan club artist id"
        Right _ ->
          expectationFailure "Expected malformed secure fan club artist id to be rejected"

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

  describe "fan club vote ballot validation" $ do
    it "requires a non-empty, deduplicated candidacy list before vote fallback inserts" $ do
      fmap (map fromSqlKey) (validateFanClubVoteSelectionIds [30, 31])
        `shouldBe` Right ([30, 31] :: [Int64])

      assertRejected 400 "requires at least one candidacy id" $
        validateFanClubVoteSelectionIds []
      assertRejected 400 "duplicate candidacy ids" $
        validateFanClubVoteSelectionIds [30, 30]
      assertRejected 400 "at most 5 candidacy ids" $
        validateFanClubVoteSelectionIds [1, 2, 3, 4, 5, 6]

    it "rejects same-role ballot collisions before insertUnique can hide intent" $ do
      let target candidacyId role =
            (toSqlKey candidacyId, mkCandidacyWithRole 20 role)
      case validateFanClubVoteCandidacyTargets
        [ target 30 Coordinator
        , target 31 Secretary
        ] of
          Right targets ->
            map (fromSqlKey . fst) targets `shouldBe` ([30, 31] :: [Int64])
          Left err ->
            expectationFailure (unexpectedRejection err)

      assertRejected 400 "at most one candidacy per officer role" $
        validateFanClubVoteCandidacyTargets
          [ target 30 Coordinator
          , target 31 Coordinator
          ]

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

  describe "fan club inbox text validation" $ do
    it "normalizes optional subjects and required bodies before inbox persistence" $ do
      validateFanClubInboxSubjectInput (Just "  Hola directiva  ")
        `shouldBe` Right (Just "Hola directiva")
      validateFanClubInboxSubjectInput (Just "   ")
        `shouldBe` Right Nothing
      validateFanClubInboxBodyInput "  Primera linea\nsegunda linea  "
        `shouldBe` Right "Primera linea\nsegunda linea"
      validateFanClubInboxReplyBodyInput "  Respondido  "
        `shouldBe` Right "Respondido"

    it "rejects blank, oversized, or unsafe inbox text before DB fallback writes" $ do
      assertRejected 400 "body is required" $
        validateFanClubInboxBodyInput "   "
      assertRejected 400 "subject must be 160 characters or fewer" $
        validateFanClubInboxSubjectInput (Just (T.replicate 161 "a"))
      assertRejected 400 "replyBody must be 4096 characters or fewer" $
        validateFanClubInboxReplyBodyInput (T.replicate 4097 "a")
      assertRejected 400 "hidden formatting" $
        validateFanClubInboxBodyInput ("Hola" <> "\x202E" <> "ops")
      assertRejected 400 "unsupported control" $
        validateFanClubInboxSubjectInput (Just "Hola\nDirectiva")
      assertRejected 400 "non-ASCII whitespace" $
        validateFanClubInboxSubjectInput (Just ("Hola" <> "\x00A0" <> "directiva"))
      assertRejected 400 "unsupported control" $
        validateFanClubInboxReplyBodyInput ("Hola" <> "\NUL" <> "ops")

  describe "validateFanClubInboxStatusInput" $ do
    it "normalizes supported inbox statuses before persistence" $ do
      validateFanClubInboxStatusInput " Archived "
        `shouldBe` Right "archived"
      validateFanClubInboxStatusInput "OPENED"
        `shouldBe` Right "opened"

    it "rejects blank, unknown, or unsafe inbox status tokens before persistence" $ do
      assertRejected 400 "status is required" $
        validateFanClubInboxStatusInput "   "
      assertRejected 400 "status must be one of" $
        validateFanClubInboxStatusInput "deleted"
      assertRejected 400 "status must not contain control" $
        validateFanClubInboxStatusInput "opened\n"
      assertRejected 400 "hidden formatting" $
        validateFanClubInboxStatusInput ("open" <> "\x202E" <> "ed")

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
  mkCandidacyWithRole electionId Coordinator

mkCandidacyWithRole :: Int64 -> FanClubOfficerRole -> FanClubCandidacy
mkCandidacyWithRole electionId role =
  FanClubCandidacy
    { fanClubCandidacyElectionId = toSqlKey electionId
    , fanClubCandidacyFanPartyId = toSqlKey 40
    , fanClubCandidacyRole = role
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

fanClubUser :: AuthedUser
fanClubUser =
  AuthedUser
    { auPartyId = toSqlKey 99
    , auRoles = [Fan, Customer]
    , auModules = modulesForRoles [Fan, Customer]
    }
