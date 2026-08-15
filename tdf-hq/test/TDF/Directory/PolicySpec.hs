{-# LANGUAGE OverloadedStrings #-}

module TDF.Directory.PolicySpec (spec) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import TDF.Directory.Policy

spec :: Spec
spec = describe "music directory formal invariants" $ do
  it "I01: public professions grant no internal directory capability" $
    property $ \labels ->
      permissionsFromProfessions
        (Set.fromList (map (PublicProfession . T.pack) (labels :: [String])))
        == Set.empty

  it "I03: repeated profession selections normalize to one membership" $
    property $ \labels ->
      let values = map (PublicProfession . T.pack) (labels :: [String])
      in Set.size (Set.fromList (values <> values)) == Set.size (Set.fromList values)

  it "I04: public search requires every publication predicate" $
    property $ \published public allowed current ->
      publicSearchEligible
        (if published then "published" else "draft")
        (if public then "public" else "private")
        (if allowed then "allowed" else "blocked")
        current
        == (published && public && allowed && current)

  it "I07: undeclared classified transitions are rejected" $ do
    allowedClassifiedTransition Draft Published `shouldBe` True
    allowedClassifiedTransition Published Filled `shouldBe` True
    allowedClassifiedTransition Filled Published `shouldBe` False
    allowedClassifiedTransition Withdrawn Draft `shouldBe` False

  it "profile lifecycle changes also use the published transition table" $ do
    allowedProfileTransition ProfileDraft ProfilePublished `shouldBe` True
    allowedProfileTransition ProfilePublished ProfilePaused `shouldBe` True
    allowedProfileTransition ProfileArchived ProfilePublished `shouldBe` False

  it "I07: application and invitation state changes use declared relations" $ do
    allowedApplicationTransition ApplicationSubmitted ApplicationShortlisted `shouldBe` True
    allowedApplicationTransition ApplicationConversationOpen ApplicationConverted `shouldBe` True
    allowedApplicationTransition ApplicationRejected ApplicationAccepted `shouldBe` False
    allowedInvitationTransition InvitationPending InvitationAccepted `shouldBe` True
    allowedInvitationTransition InvitationConversationOpen InvitationConverted `shouldBe` True
    allowedInvitationTransition InvitationDeclined InvitationAccepted `shouldBe` False

  it "I08: an application is visible only to participants or an administrator" $
    property $ \viewer applicant author adminIds ->
      applicationVisibleTo viewer applicant author (Set.fromList adminIds)
        == ((viewer :: Integer) == applicant || viewer == author || viewer `elem` adminIds)

  it "minor or unknown age assurance cannot publish or respond independently" $ do
    minorMayPublishOrRespond "unknown" `shouldBe` False
    minorMayPublishOrRespond "minor_restricted" `shouldBe` False
    minorMayPublishOrRespond "guardian_pending" `shouldBe` False
    minorMayPublishOrRespond "guardian_approved" `shouldBe` True
    minorMayPublishOrRespond "adult_attested" `shouldBe` True

  it "inactive grants never authorize a profile capability" $
    property $ \editGranted publishGranted requestPublish ->
      let capabilities = Set.fromList
            ([Edit | editGranted] <> [Publish | publishGranted])
          requested = if requestPublish then Publish else Edit
      in not (capabilityAllows False capabilities requested)
