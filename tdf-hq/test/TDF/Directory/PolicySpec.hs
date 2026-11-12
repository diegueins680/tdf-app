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
    -- I19: a released slot reopens a filled classified; nothing else leaves filled.
    allowedClassifiedTransition Filled Published `shouldBe` True
    allowedClassifiedTransition Filled Paused `shouldBe` False
    allowedClassifiedTransition Filled Withdrawn `shouldBe` False
    allowedClassifiedTransition Withdrawn Draft `shouldBe` False

  it "profile lifecycle changes also use the published transition table" $ do
    allowedProfileTransition ProfileDraft ProfilePublished `shouldBe` True
    allowedProfileTransition ProfilePublished ProfilePaused `shouldBe` True
    allowedProfileTransition ProfileArchived ProfilePublished `shouldBe` False

  it "I07: application and invitation state changes use declared relations" $ do
    allowedApplicationTransition ApplicationSubmitted ApplicationShortlisted `shouldBe` True
    allowedApplicationTransition ApplicationConversationOpen ApplicationConverted `shouldBe` True
    allowedApplicationTransition ApplicationSubmitted ApplicationConversationOpen `shouldBe` False
    allowedApplicationTransition ApplicationRejected ApplicationAccepted `shouldBe` False
    allowedInvitationTransition InvitationPending InvitationAccepted `shouldBe` True
    allowedInvitationTransition InvitationPending InvitationExpired `shouldBe` True
    allowedInvitationTransition InvitationConversationOpen InvitationConverted `shouldBe` True
    allowedInvitationTransition InvitationPending InvitationConversationOpen `shouldBe` False
    allowedInvitationTransition InvitationDeclined InvitationAccepted `shouldBe` False

  -- ADR 0134: negotiable offers.
  it "I18: counter states alternate and are reached only from open states" $ do
    allowedApplicationTransition ApplicationSubmitted ApplicationCounteredByAuthor `shouldBe` True
    allowedApplicationTransition ApplicationCounteredByAuthor ApplicationCounteredByApplicant `shouldBe` True
    allowedApplicationTransition ApplicationCounteredByApplicant ApplicationCounteredByAuthor `shouldBe` True
    allowedApplicationTransition ApplicationCounteredByAuthor ApplicationCounteredByAuthor `shouldBe` True -- same-state observation
    allowedApplicationTransition ApplicationSubmitted ApplicationCounteredByApplicant `shouldBe` False
    allowedApplicationTransition ApplicationAccepted ApplicationCounteredByAuthor `shouldBe` False
    allowedApplicationTransition ApplicationRejected ApplicationCounteredByAuthor `shouldBe` False
    allowedApplicationTransition ApplicationExpired ApplicationSubmitted `shouldBe` False

  it "I18: every open offer can be rejected, withdrawn or expired, and closed offers never reopen" $ do
    let closed = [ApplicationRejected, ApplicationWithdrawn, ApplicationExpired]
    and [allowedApplicationTransition from to | from <- Set.toList openApplicationStatuses, to <- closed]
      `shouldBe` True
    or [allowedApplicationTransition from to | from <- closed, to <- [minBound .. maxBound], to /= from]
      `shouldBe` False

  it "I18: whoever did not make the last move may accept or counter; the other may not" $ do
    acceptAllowedFor ApplicationCounteredByAuthor ActorApplicant `shouldBe` True
    acceptAllowedFor ApplicationCounteredByAuthor ActorAuthor `shouldBe` False
    acceptAllowedFor ApplicationCounteredByApplicant ActorAuthor `shouldBe` True
    acceptAllowedFor ApplicationSubmitted ActorApplicant `shouldBe` False
    counterAllowedFor ApplicationSubmitted ActorAuthor `shouldBe` True
    counterAllowedFor ApplicationSubmitted ActorApplicant `shouldBe` False
    counterAllowedFor ApplicationCounteredByAuthor ActorApplicant `shouldBe` True
    counterAllowedFor ApplicationAccepted ActorAuthor `shouldBe` False

  it "I18: both participants can never accept from the same state" $
    forAll (elements [minBound .. maxBound]) $ \status ->
      not (acceptAllowedFor status ActorAuthor && acceptAllowedFor status ActorApplicant)

  it "I18: counter states are never reachable through the generic status endpoint" $
    forAll ((,) <$> elements [minBound .. maxBound] <*> elements ["author", "applicant"]) $ \(from, participant) ->
      not (participantTransitionAllowed participant from ApplicationCounteredByAuthor
           || participantTransitionAllowed participant from ApplicationCounteredByApplicant)

  it "I18: a valid negotiation starts with the applicant, alternates, stays within the limit and accepts only last" $ do
    let chain n = [OfferRound i (if odd i then ActorApplicant else ActorAuthor) (if i == 1 then RoundInitial else RoundCounter) | i <- [1 .. n]]
    negotiationRoundsValid (chain 1) `shouldBe` True
    negotiationRoundsValid (chain 3) `shouldBe` True
    negotiationRoundsValid (chain maxNegotiationRounds) `shouldBe` True
    negotiationRoundsValid (chain (maxNegotiationRounds + 1)) `shouldBe` False
    negotiationRoundsValid [] `shouldBe` False
    negotiationRoundsValid [OfferRound 1 ActorAuthor RoundInitial] `shouldBe` False
    negotiationRoundsValid [OfferRound 1 ActorApplicant RoundCounter] `shouldBe` False
    negotiationRoundsValid [OfferRound 1 ActorApplicant RoundInitial, OfferRound 2 ActorApplicant RoundCounter] `shouldBe` False
    negotiationRoundsValid [OfferRound 1 ActorApplicant RoundInitial, OfferRound 3 ActorAuthor RoundCounter] `shouldBe` False
    negotiationRoundsValid [OfferRound 1 ActorApplicant RoundInitial, OfferRound 2 ActorAuthor RoundAccept] `shouldBe` True
    negotiationRoundsValid [OfferRound 1 ActorApplicant RoundInitial, OfferRound 2 ActorAuthor RoundAccept, OfferRound 3 ActorApplicant RoundCounter] `shouldBe` False

  it "I18: counters stop one round before the limit so the acceptance is always recorded" $ do
    counterRoundAllowed 1 `shouldBe` False
    counterRoundAllowed 2 `shouldBe` True
    counterRoundAllowed (maxNegotiationRounds - 1) `shouldBe` True
    counterRoundAllowed maxNegotiationRounds `shouldBe` False
    -- Nine counters after the initial round leave exactly the final round for the accept.
    let counters = [OfferRound i (if odd i then ActorApplicant else ActorAuthor) (if i == 1 then RoundInitial else RoundCounter) | i <- [1 .. maxNegotiationRounds - 1]]
    all (counterRoundAllowed . roundNumber) (drop 1 counters) `shouldBe` True
    negotiationRoundsValid (counters ++ [OfferRound maxNegotiationRounds ActorAuthor RoundAccept]) `shouldBe` True

  it "I19: accepting consumes exactly one slot and never goes below zero" $
    property $ \remaining ->
      case slotsAfterAccept remaining of
        Nothing -> remaining <= 0
        Just next -> next == remaining - 1 && next >= 0

  it "I19: releasing a slot never exceeds the total" $
    property $ \total remaining ->
      let released = slotsAfterRelease total remaining
      in released <= total && (remaining >= total || released == remaining + 1)

  it "fixed prices admit only the exact reference amount; open and suggested admit any non-negative amount" $ do
    offerAmountAcceptable PriceFixed (Just 40000) (Just 40000) `shouldBe` True
    offerAmountAcceptable PriceFixed (Just 40000) (Just 39999) `shouldBe` False
    offerAmountAcceptable PriceFixed Nothing (Just 1) `shouldBe` False
    offerAmountAcceptable PriceSuggested (Just 40000) (Just 1) `shouldBe` True
    offerAmountAcceptable PriceOpen Nothing Nothing `shouldBe` True
    offerAmountAcceptable PriceOpen Nothing (Just (-1)) `shouldBe` False
    parsePriceMode " Fixed " `shouldBe` Just PriceFixed
    parsePriceMode "auction" `shouldBe` Nothing

  it "closing a classified rejects open offers with the declared reason; pausing keeps them" $ do
    closureRejectionReason Filled `shouldBe` Just "slots_filled"
    closureRejectionReason Withdrawn `shouldBe` Just "classified_closed"
    closureRejectionReason Expired `shouldBe` Just "classified_closed"
    closureRejectionReason Paused `shouldBe` Nothing
    closureRejectionReason Published `shouldBe` Nothing

  it "I08: an application is visible only to participants or an administrator" $
    property $ \viewer applicant author adminIds ->
      applicationVisibleTo viewer applicant author (Set.fromList adminIds)
        == ((viewer :: Integer) == applicant || viewer == author || viewer `elem` adminIds)

  it "I09: a review requires an explicitly managed author and the exact verified completed pair" $
    property $ \verified managed author subject profileA profileB ->
      verifiedReviewEligible "completed" verified managed author subject profileA profileB
        == (verified && managed && author /= subject
          && ((author == profileA && subject == profileB) || (author == profileB && subject == profileA)))

  it "I09: pending, confirmed, cancelled, or disputed interactions never qualify" $ do
    map (\status -> verifiedReviewEligible status True True 1 2 1 2)
      ["pending","confirmed","cancelled","disputed"] `shouldBe` replicate 4 False

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

  it "I16: omitted profile fields preserve data and explicit replacements are exact" $
    property $ \current next ->
      applyProfileFieldUpdate (current :: String) PreserveProfileField == current
        && applyProfileFieldUpdate current (ReplaceProfileField next) == next

  it "I16: structured membership details accept one duplicate-free ID set" $
    property $ \ids ->
      detailIdsMatch (Set.toList (Set.fromList (ids :: [Int]))) (Set.toList (Set.fromList ids))

  it "I16: structured membership details reject duplicates and unequal sets" $ do
    detailIdsMatch [1 :: Int, 1] [1] `shouldBe` False
    detailIdsMatch [1 :: Int, 2] [1] `shouldBe` False

  it "I16: a nonempty explicit service-area set has exactly one primary" $
    property $ \flags ->
      serviceAreaPrimaryValid flags == (null flags || length (filter id flags) == 1)

  it "canonicalizes supported favorite targets before persistence" $ do
    canonicalFavoriteTarget " Event " "0042" `shouldBe` Right ("event", "42")
    canonicalFavoriteTarget "VENUE" " 7 " `shouldBe` Right ("venue", "7")
    canonicalFavoriteTarget
      "profile"
      "D1000000-0000-4000-8000-000000000001"
      `shouldBe` Right ("profile", "d1000000-0000-4000-8000-000000000001")

  it "rejects malformed, non-positive, and unsupported favorite targets" $ do
    canonicalFavoriteTarget "event" "abc" `shouldBe` Left "event targetId must be a positive integer"
    canonicalFavoriteTarget "event" "0" `shouldBe` Left "event targetId must be a positive integer"
    canonicalFavoriteTarget "profile" "not-a-uuid" `shouldBe` Left "profile targetId must be a UUID"
    canonicalFavoriteTarget "application" "1" `shouldBe` Left "invalid targetKind"
