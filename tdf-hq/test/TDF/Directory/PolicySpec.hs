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

  it "I21: agreement transitions follow the declared relation and terminal states stay terminal" $ do
    allowedAgreementTransition AgreementConfirmed AgreementInProgress `shouldBe` True
    allowedAgreementTransition AgreementConfirmed AgreementCompleted `shouldBe` True
    allowedAgreementTransition AgreementPendingDeposit AgreementConfirmed `shouldBe` True
    allowedAgreementTransition AgreementConfirmed AgreementPendingDeposit `shouldBe` False
    allowedAgreementTransition AgreementBalanceDue AgreementCancelled `shouldBe` False

  it "I21: completed, cancelled and expired agreements never move again" $
    forAll (elements [minBound .. maxBound]) $ \target ->
      all (\terminal -> terminal == target || not (allowedAgreementTransition terminal target))
        [AgreementCompleted, AgreementCancelled, AgreementExpired]

  it "I22: the provider starts, only the buyer completes, and settlement states are never requested by a participant" $ do
    agreementTransitionAllowedFor PartyProvider AgreementConfirmed AgreementInProgress `shouldBe` True
    agreementTransitionAllowedFor PartyBuyer AgreementConfirmed AgreementInProgress `shouldBe` False
    agreementTransitionAllowedFor PartyBuyer AgreementConfirmed AgreementCompleted `shouldBe` True
    agreementTransitionAllowedFor PartyBuyer AgreementInProgress AgreementCompleted `shouldBe` True
    agreementTransitionAllowedFor PartyProvider AgreementInProgress AgreementCompleted `shouldBe` False
    agreementTransitionAllowedFor PartyProvider AgreementConfirmed AgreementCancelled `shouldBe` True
    agreementTransitionAllowedFor PartyBuyer AgreementInProgress AgreementDisputed `shouldBe` True
    agreementTransitionAllowedFor PartyAdmin AgreementDisputed AgreementCompleted `shouldBe` True
    agreementTransitionAllowedFor PartyAdmin AgreementConfirmed AgreementCancelled `shouldBe` False
    agreementTransitionAllowedFor PartyBuyer AgreementDisputed AgreementCompleted `shouldBe` False

  it "I22: no participant ever requests a settlement state or leaves one" $
    forAll ((,,) <$> elements [PartyBuyer, PartyProvider] <*> elements [minBound .. maxBound] <*> elements [AgreementPendingDeposit, AgreementBalanceDue, AgreementExpired]) $ \(party, from, settlement) ->
      not (agreementTransitionAllowedFor party from settlement) && not (agreementTransitionAllowedFor party settlement AgreementConfirmed)

  it "I23: agreement amounts are frozen so that deposit plus balance is the total and the commission never exceeds it" $ do
    agreementAmounts 1000 3000 125000 `shouldBe` Just (12500, 37500, 87500)
    agreementAmounts 1000 3000 123456 `shouldBe` Just (12345, 37037, 86419)
    agreementAmounts 0 10000 999 `shouldBe` Just (0, 999, 0)
    agreementAmounts 1000 3000 0 `shouldBe` Nothing
    agreementAmounts 10001 3000 100 `shouldBe` Nothing
    agreementAmounts 1000 (-1) 100 `shouldBe` Nothing

  it "I23: for every rate and amount the split is exact and the commission is bounded" $
    forAll ((,,) <$> choose (0, 10000) <*> choose (0, 10000) <*> choose (1, 1000000000000)) $ \(commissionBps, depositBps, amount) ->
      case agreementAmounts commissionBps depositBps amount of
        Just (commission, deposit, balance) ->
          deposit + balance == amount && deposit >= 0 && balance >= 0 && commission >= 0 && commission <= amount
        Nothing -> False

  it "I24: a pending agreement payment is paid, expires or is cancelled, and never moves again" $ do
    allowedAgreementPaymentTransition AgreementPaymentPending AgreementPaymentPaid `shouldBe` True
    allowedAgreementPaymentTransition AgreementPaymentPending AgreementPaymentExpired `shouldBe` True
    allowedAgreementPaymentTransition AgreementPaymentPending AgreementPaymentCancelled `shouldBe` True
    allowedAgreementPaymentTransition AgreementPaymentPaid AgreementPaymentPending `shouldBe` False
    allowedAgreementPaymentTransition AgreementPaymentPaid AgreementPaymentCancelled `shouldBe` False
    allowedAgreementPaymentTransition AgreementPaymentExpired AgreementPaymentPaid `shouldBe` False

  it "I24: no settled agreement payment has an outgoing transition" $
    forAll ((,) <$> elements [AgreementPaymentPaid, AgreementPaymentExpired, AgreementPaymentCancelled] <*> elements [minBound .. maxBound]) $ \(from, to) ->
      from == to || not (allowedAgreementPaymentTransition from to)

  it "I25: the deposit carries the commission and the provider bears the processor fee by default" $ do
    -- 100.00 agreed, 10 % commission, 30 % deposit, 5.75 % processor fee.
    agreementCommissionDue AgreementDeposit 1000 3000 0 `shouldBe` 1000
    agreementCommissionDue AgreementBalance 1000 3000 1000 `shouldBe` 0
    agreementProcessingFee 575 0 3000 `shouldBe` Just 173
    agreementProcessingFee 575 0 7000 `shouldBe` Just 403
    agreementPaymentSplit FeePaidByProvider 173 1000 3000 `shouldBe` Just (AgreementPaymentSplit 1000 173 1827)
    agreementPaymentSplit FeePaidByProvider 403 0 7000 `shouldBe` Just (AgreementPaymentSplit 0 403 6597)
    agreementPaymentSplit FeePaidByPlatform 173 1000 3000 `shouldBe` Just (AgreementPaymentSplit 827 173 2000)
    -- A deposit smaller than the commission leaves the rest for the balance.
    agreementCommissionDue AgreementDeposit 1000 500 0 `shouldBe` 500
    agreementCommissionDue AgreementBalance 1000 500 500 `shouldBe` 500
    -- The processor fee ate into a deposit smaller than the commission: the
    -- platform collected 471 of it, so the balance carries the other 529.
    agreementProcessingFee 575 0 500 `shouldBe` Just 29
    agreementPaymentSplit FeePaidByProvider 29 500 500 `shouldBe` Just (AgreementPaymentSplit 471 29 0)
    agreementCommissionDue AgreementBalance 1000 500 471 `shouldBe` 529
    agreementCommissionDue AgreementBalance 1000 500 5000 `shouldBe` 0
    agreementPaymentSplit FeePaidByProvider 0 1000 0 `shouldBe` Nothing
    agreementProcessingFee 10001 0 100 `shouldBe` Nothing

  it "I25: every split is exact, non-negative and never gives the platform more than the commission due" $
    forAll ((,,,) <$> elements [FeePaidByProvider, FeePaidByPlatform] <*> choose (0, 2000000000000) <*> choose (0, 2000000000000) <*> choose (1, 1000000000000)) $ \(payer, processingFee, commissionDue, amount) ->
      case agreementPaymentSplit payer processingFee commissionDue amount of
        Just (AgreementPaymentSplit platform processing provider) ->
          platform + processing + provider == amount && platform >= 0 && processing >= 0 && provider >= 0 && platform <= commissionDue
        Nothing -> False

  it "I25: over the deposit and the balance the platform takes the frozen commission whenever the charges can carry it, and never more" $
    forAll ((,,,) <$> choose (0, 10000) <*> choose (0, 10000) <*> choose (0, 1000) <*> choose (1, 1000000000000)) $ \(commissionBps, depositBps, feeBps, amount) ->
      case agreementAmounts commissionBps depositBps amount of
        Just (commission, deposit, balance) ->
          let platformOf due charge
                | charge <= 0 = 0
                | otherwise = case agreementProcessingFee feeBps 0 charge >>= \fee -> agreementPaymentSplit FeePaidByProvider fee due charge of
                    Just parts -> splitPlatformFeeMinor parts
                    Nothing -> 0
              netOf charge
                | charge <= 0 = 0
                | otherwise = maybe 0 (charge -) (agreementProcessingFee feeBps 0 charge)
              fromDeposit = platformOf (agreementCommissionDue AgreementDeposit commission deposit 0) deposit
              balanceDue = agreementCommissionDue AgreementBalance commission deposit fromDeposit
              fromBalance = platformOf balanceDue balance
              total = fromDeposit + fromBalance
          in total <= commission && (total == commission || netOf balance < balanceDue)
        Nothing -> False

  it "I25: the processor fee is rounded up and never exceeds the charge" $
    forAll ((,,) <$> choose (0, 10000) <*> choose (0, 100000) <*> choose (1, 1000000000000)) $ \(feeBps, feeFixed, amount) ->
      case agreementProcessingFee feeBps feeFixed amount of
        Just fee -> fee >= 0 && fee <= amount && (toInteger fee * 10000 >= min (toInteger amount * 10000) (toInteger amount * toInteger feeBps))
        Nothing -> False

  it "I26: confirming delivery waits for the balance only on the platform, and a paid deposit blocks a direct cancellation" $ do
    agreementCompletionTarget True 7000 `shouldBe` AgreementBalanceDue
    agreementCompletionTarget True 0 `shouldBe` AgreementCompleted
    agreementCompletionTarget False 7000 `shouldBe` AgreementCompleted
    agreementParticipantCancelAllowed True True `shouldBe` False
    agreementParticipantCancelAllowed True False `shouldBe` True
    agreementParticipantCancelAllowed False False `shouldBe` True

  it "I25: a payout recipient is a cédula, a RUC or an E.164 phone" $ do
    payoutIdentifierValid "identification" "1712345678" `shouldBe` True
    payoutIdentifierValid "identification" "1712345678001" `shouldBe` True
    payoutIdentifierValid "identification" "17123456" `shouldBe` False
    payoutIdentifierValid "identification" "17123456AB" `shouldBe` False
    payoutIdentifierValid "phone" "+593991234567" `shouldBe` True
    payoutIdentifierValid "phone" "0991234567" `shouldBe` False
    payoutIdentifierValid "phone" "+0991234567" `shouldBe` False
    payoutIdentifierValid "account" "acct_1234567890" `shouldBe` False

  it "I22: either party may cancel before the deposit is paid, and nobody confirms it by hand" $ do
    agreementTransitionAllowedFor PartyBuyer AgreementPendingDeposit AgreementCancelled `shouldBe` True
    agreementTransitionAllowedFor PartyProvider AgreementPendingDeposit AgreementCancelled `shouldBe` True
    agreementTransitionAllowedFor PartyAdmin AgreementPendingDeposit AgreementConfirmed `shouldBe` False
    allowedAgreementTransition AgreementConfirmed AgreementBalanceDue `shouldBe` True
    agreementTransitionAllowedFor PartyBuyer AgreementConfirmed AgreementBalanceDue `shouldBe` False

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
