{-# LANGUAGE OverloadedStrings #-}

module TDF.Directory.Policy
  ( ProfileStatus(..)
  , ClassifiedStatus(..)
  , ApplicationStatus(..)
  , InvitationStatus(..)
  , DirectoryCapability(..)
  , PublicProfession(..)
  , ProfileFieldUpdate(..)
  , NegotiationActor(..)
  , OfferRoundKind(..)
  , OfferRound(..)
  , PriceMode(..)
  , allowedClaimTransition
  , allowedProfileTransition
  , allowedClassifiedTransition
  , allowedApplicationTransition
  , allowedInvitationTransition
  , AgreementStatus(..)
  , AgreementParty(..)
  , allowedAgreementTransition
  , agreementTransitionAllowedFor
  , agreementAmounts
  , AgreementPaymentStatus(..)
  , AgreementPaymentKind(..)
  , ProcessingFeePayer(..)
  , AgreementPaymentSplit(..)
  , allowedAgreementPaymentTransition
  , agreementCommissionDue
  , agreementProcessingFee
  , agreementPaymentSplit
  , agreementCompletionTarget
  , agreementParticipantCancelAllowed
  , payoutIdentifierValid
  , openApplicationStatuses
  , applicationIsOpen
  , counterAllowedFor
  , acceptAllowedFor
  , participantTransitionAllowed
  , negotiationRoundsValid
  , nextRoundActor
  , maxNegotiationRounds
  , counterRoundAllowed
  , parsePriceMode
  , priceModeText
  , offerAmountAcceptable
  , slotsAfterAccept
  , slotsAfterRelease
  , classifiedClosesOffers
  , closureRejectionReason
  , capabilityAllows
  , permissionsFromProfessions
  , publicSearchEligible
  , applicationVisibleTo
  , verifiedReviewEligible
  , minorMayPublishOrRespond
  , applyProfileFieldUpdate
  , detailIdsMatch
  , serviceAreaPrimaryValid
  , canonicalFavoriteKind
  , canonicalFavoriteTarget
  ) where

import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Text.Read (readMaybe)

data ProfileStatus
  = ProfileDraft | ProfilePendingReview | ProfilePublished | ProfilePaused
  | ProfileArchived | ProfileSuspended | ProfileMerged
  deriving (Eq, Ord, Show, Enum, Bounded)

data ClassifiedStatus
  = Draft | PendingModeration | Published | Paused | Filled | Expired
  | Withdrawn | Rejected | Moderated
  deriving (Eq, Ord, Show, Enum, Bounded)

-- ADR 0134: an application is an offer. Negotiation alternates between the
-- applicant (offerer) and the classified author through countered states.
data ApplicationStatus
  = ApplicationSubmitted | ApplicationViewed | ApplicationShortlisted
  | ApplicationCounteredByAuthor | ApplicationCounteredByApplicant
  | ApplicationAccepted | ApplicationRejected | ApplicationWithdrawn
  | ApplicationExpired | ApplicationConversationOpen | ApplicationConverted
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | ADR 0135: lifecycle of a marketplace agreement. @pending_deposit@,
-- @balance_due@ and @expired@ belong to platform settlement (checkout) and
-- are never entered by a participant request.
data AgreementStatus
  = AgreementPendingDeposit | AgreementConfirmed | AgreementInProgress
  | AgreementBalanceDue | AgreementCompleted | AgreementCancelled
  | AgreementDisputed | AgreementExpired
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Who asks for an agreement transition. The buyer is the party that seeks
-- and pays; the provider delivers; the admin resolves disputes.
data AgreementParty = PartyBuyer | PartyProvider | PartyAdmin
  deriving (Eq, Ord, Show, Enum, Bounded)

data NegotiationActor = ActorApplicant | ActorAuthor
  deriving (Eq, Ord, Show, Enum, Bounded)

data OfferRoundKind = RoundInitial | RoundCounter | RoundAccept
  deriving (Eq, Ord, Show, Enum, Bounded)

data OfferRound = OfferRound
  { roundNumber :: Int
  , roundActor :: NegotiationActor
  , roundKind :: OfferRoundKind
  } deriving (Eq, Ord, Show)

data PriceMode = PriceOpen | PriceSuggested | PriceFixed
  deriving (Eq, Ord, Show, Enum, Bounded)

data InvitationStatus
  = InvitationPending | InvitationAccepted | InvitationDeclined
  | InvitationWithdrawn | InvitationBlocked | InvitationConversationOpen
  | InvitationConverted | InvitationExpired
  deriving (Eq, Ord, Show, Enum, Bounded)

allowedProfileTransition :: ProfileStatus -> ProfileStatus -> Bool
allowedProfileTransition fromStatus toStatus =
  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions
  where
    transitions = Set.fromList
      [ (ProfileDraft, ProfilePendingReview), (ProfileDraft, ProfilePublished), (ProfileDraft, ProfileArchived)
      , (ProfilePendingReview, ProfilePublished), (ProfilePendingReview, ProfileDraft), (ProfilePendingReview, ProfileSuspended)
      , (ProfilePublished, ProfilePaused), (ProfilePublished, ProfileArchived), (ProfilePublished, ProfileSuspended), (ProfilePublished, ProfileMerged)
      , (ProfilePaused, ProfilePublished), (ProfilePaused, ProfileArchived), (ProfilePaused, ProfileSuspended)
      , (ProfileSuspended, ProfilePublished), (ProfileSuspended, ProfileArchived)
      ]

data DirectoryCapability = ViewPrivate | Edit | Publish | Contact | Manage
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Deliberately distinct from Auth.RoleEnum and security permissions.
newtype PublicProfession = PublicProfession Text
  deriving (Eq, Ord, Show)

data ProfileFieldUpdate a = PreserveProfileField | ReplaceProfileField a
  deriving (Eq, Ord, Show)

-- ID-CLAIM-REVIEW-001: docs/music-directory/formal-model.yaml is the
-- declared edge relation. HTTP conformance exhausts its Cartesian product.
-- Same-state requests observe existing state; unknown states never replay.
allowedClaimTransition :: Text -> Text -> Bool
allowedClaimTransition fromStatus toStatus =
  (fromStatus == toStatus && fromStatus `Set.member` states)
    || (fromStatus, toStatus) `Set.member` transitions
  where
    states = Set.fromList
      ["draft", "submitted", "under_review", "more_evidence_requested",
       "approved", "rejected", "withdrawn", "revoked"]
    transitions = Set.fromList
      [ ("draft", "submitted"), ("draft", "withdrawn")
      , ("submitted", "under_review"), ("submitted", "withdrawn")
      , ("under_review", "approved"), ("under_review", "rejected")
      , ("under_review", "more_evidence_requested"), ("under_review", "withdrawn")
      , ("more_evidence_requested", "submitted"), ("more_evidence_requested", "withdrawn")
      , ("rejected", "submitted")
      -- AUTHORITY-097: an approved claim can be revoked by a distinct
      -- administrator with a stated reason; revocation is terminal.
      , ("approved", "revoked")
      ]

allowedClassifiedTransition :: ClassifiedStatus -> ClassifiedStatus -> Bool
allowedClassifiedTransition fromStatus toStatus =
  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions
  where
    transitions = Set.fromList
      [ (Draft, PendingModeration), (Draft, Published), (Draft, Withdrawn)
      , (PendingModeration, Published), (PendingModeration, Rejected), (PendingModeration, Withdrawn)
      , (Published, Paused), (Published, Filled), (Published, Expired), (Published, Withdrawn), (Published, Moderated)
      , (Paused, Published), (Paused, Filled), (Paused, Expired), (Paused, Withdrawn), (Paused, Moderated)
      -- ADR 0134 I19: withdrawing an accepted offer releases a slot and reopens
      -- a filled classified that has not expired.
      , (Filled, Published)
      , (Expired, Published), (Expired, Withdrawn)
      , (Rejected, Draft), (Rejected, Withdrawn)
      , (Moderated, Draft), (Moderated, Withdrawn)
      ]

-- I21 (ADR 0135): every agreement state change belongs to this relation; it
-- mirrors marketplace_agreement_transition_allowed in the database and
-- docs/music-directory/formal-model.yaml#agreement.
allowedAgreementTransition :: AgreementStatus -> AgreementStatus -> Bool
allowedAgreementTransition fromStatus toStatus =
  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions
  where
    transitions = Set.fromList
      [ (AgreementPendingDeposit, AgreementConfirmed)
      , (AgreementPendingDeposit, AgreementCancelled)
      , (AgreementPendingDeposit, AgreementExpired)
      , (AgreementConfirmed, AgreementInProgress)
      -- ADR 0136: the buyer may confirm delivery before the provider marked
      -- the start; with a balance owed on the platform that waits for it.
      , (AgreementConfirmed, AgreementBalanceDue)
      , (AgreementConfirmed, AgreementCompleted)
      , (AgreementConfirmed, AgreementCancelled)
      , (AgreementConfirmed, AgreementDisputed)
      , (AgreementInProgress, AgreementBalanceDue)
      , (AgreementInProgress, AgreementCompleted)
      , (AgreementInProgress, AgreementCancelled)
      , (AgreementInProgress, AgreementDisputed)
      , (AgreementBalanceDue, AgreementCompleted)
      , (AgreementBalanceDue, AgreementDisputed)
      , (AgreementDisputed, AgreementCompleted)
      , (AgreementDisputed, AgreementCancelled)
      ]

-- | I22 (ADR 0135): which declared transition each party may request. The
-- provider marks the work as started; only the buyer confirms completion
-- (a payment state never implies the service was delivered, and the provider
-- cannot self-certify); either party may cancel or dispute while the
-- agreement is live; only an admin resolves a dispute. Settlement states are
-- never requested by a participant.
agreementTransitionAllowedFor :: AgreementParty -> AgreementStatus -> AgreementStatus -> Bool
agreementTransitionAllowedFor party fromStatus toStatus =
  fromStatus /= toStatus && allowedAgreementTransition fromStatus toStatus && case (party, fromStatus, toStatus) of
    (PartyProvider, AgreementConfirmed, AgreementInProgress) -> True
    (PartyBuyer, AgreementConfirmed, AgreementCompleted) -> True
    (PartyBuyer, AgreementInProgress, AgreementCompleted) -> True
    (PartyAdmin, AgreementDisputed, _) -> True
    (PartyAdmin, _, _) -> False
    -- ADR 0136: either party may walk away before the deposit is paid.
    (_, AgreementPendingDeposit, AgreementCancelled) -> True
    (_, AgreementConfirmed, AgreementCancelled) -> True
    (_, AgreementInProgress, AgreementCancelled) -> True
    (_, AgreementConfirmed, AgreementDisputed) -> True
    (_, AgreementInProgress, AgreementDisputed) -> True
    _ -> False

-- | I23 (ADR 0135): amounts frozen when an agreement is created, from the
-- agreed amount and the commission and deposit rates in basis points. The
-- commission rounds down (never above the rate), the deposit rounds half up,
-- and deposit + balance is exactly the amount. @Nothing@ for a non-positive
-- amount or a rate outside 0..10000.
agreementAmounts :: Int -> Int -> Int64 -> Maybe (Int64, Int64, Int64)
agreementAmounts commissionBps depositBps amount
  | amount <= 0 = Nothing
  | commissionBps < 0 || commissionBps > 10000 = Nothing
  | depositBps < 0 || depositBps > 10000 = Nothing
  | otherwise =
      let commission = fromInteger ((toInteger amount * toInteger commissionBps) `div` 10000)
          deposit = fromInteger ((toInteger amount * toInteger depositBps + 5000) `div` 10000)
      in Just (commission, deposit, amount - deposit)

-- | ADR 0136: state of one charge (deposit or balance) of an agreement.
data AgreementPaymentStatus
  = AgreementPaymentPending
  | AgreementPaymentPaid
  | AgreementPaymentExpired
  | AgreementPaymentCancelled
  deriving (Eq, Ord, Show, Enum, Bounded)

data AgreementPaymentKind = AgreementDeposit | AgreementBalance
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Who bears the payment processor's fee on a charge.
data ProcessingFeePayer = FeePaidByProvider | FeePaidByPlatform
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Frozen split of one charge. The three parts always add up to the charge.
data AgreementPaymentSplit = AgreementPaymentSplit
  { splitPlatformFeeMinor :: Int64
  , splitProcessingFeeMinor :: Int64
  , splitProviderAmountMinor :: Int64
  } deriving (Eq, Show)

-- I24 (ADR 0136): every agreement payment state change belongs to this
-- relation; it mirrors marketplace_agreement_payment_transition_allowed in the
-- database and docs/music-directory/formal-model.yaml#agreement_payment. A
-- paid, expired or cancelled payment never moves again.
allowedAgreementPaymentTransition :: AgreementPaymentStatus -> AgreementPaymentStatus -> Bool
allowedAgreementPaymentTransition fromStatus toStatus =
  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions
  where
    transitions = Set.fromList
      [ (AgreementPaymentPending, AgreementPaymentPaid)
      , (AgreementPaymentPending, AgreementPaymentExpired)
      , (AgreementPaymentPending, AgreementPaymentCancelled)
      ]

-- | I25 (ADR 0136): commission the platform takes from one charge. The
-- deposit carries as much of the frozen commission as fits in it; the balance
-- carries whatever the deposit could not. Over both charges the platform
-- takes exactly the frozen commission.
agreementCommissionDue :: AgreementPaymentKind -> Int64 -> Int64 -> Int64
agreementCommissionDue kind commission deposit =
  let fromDeposit = max 0 (min commission deposit)
  in case kind of
       AgreementDeposit -> fromDeposit
       AgreementBalance -> max 0 (commission - fromDeposit)

-- | Processor fee estimated for a charge: basis points rounded up plus a fixed
-- part, never above the charge. @Nothing@ for invalid inputs.
agreementProcessingFee :: Int -> Int64 -> Int64 -> Maybe Int64
agreementProcessingFee feeBps feeFixed amount
  | amount <= 0 = Nothing
  | feeBps < 0 || feeBps > 10000 = Nothing
  | feeFixed < 0 = Nothing
  | otherwise =
      let proportional = (toInteger amount * toInteger feeBps + 9999) `div` 10000
      in Just (fromInteger (min (toInteger amount) (proportional + toInteger feeFixed)))

-- | I25 (ADR 0136): split one charge between the platform, the processor and
-- the service provider. Arguments: who bears the processor fee, the processor
-- fee, the commission due on this charge and the charge amount. When the
-- provider bears the fee it comes out of the provider's part; when the
-- platform bears it, it comes out of the commission and only the excess, if
-- any, reaches the provider's part. No part is negative and the parts add up
-- to the charge. @Nothing@ for a non-positive charge or negative inputs.
agreementPaymentSplit :: ProcessingFeePayer -> Int64 -> Int64 -> Int64 -> Maybe AgreementPaymentSplit
agreementPaymentSplit payer processingFee commissionDue amount
  | amount <= 0 || processingFee < 0 || commissionDue < 0 = Nothing
  | otherwise =
      let processing = min amount processingFee
          afterProcessing = amount - processing
          platform = case payer of
            FeePaidByProvider -> min commissionDue afterProcessing
            FeePaidByPlatform -> min (max 0 (commissionDue - processing)) afterProcessing
      in Just AgreementPaymentSplit
           { splitPlatformFeeMinor = platform
           , splitProcessingFeeMinor = processing
           , splitProviderAmountMinor = afterProcessing - platform
           }

-- | I26 (ADR 0136): where the buyer's confirmation of delivery takes a live
-- agreement. Settled on the platform with a balance still owed, it waits for
-- the balance (the buyer never requests a settlement state directly);
-- otherwise it completes.
agreementCompletionTarget :: Bool -> Int64 -> AgreementStatus
agreementCompletionTarget settledOnPlatform balance
  | settledOnPlatform && balance > 0 = AgreementBalanceDue
  | otherwise = AgreementCompleted

-- | I26 (ADR 0136): a participant cancels an agreement directly only while no
-- money was collected on the platform. With a paid deposit the way out is a
-- dispute resolved by an administrator, until automatic refunds exist.
agreementParticipantCancelAllowed :: Bool -> Bool -> Bool
agreementParticipantCancelAllowed settledOnPlatform depositPaid =
  not (settledOnPlatform && depositPaid)

-- | ADR 0136: shape of a PayPhone payout recipient. An identification is an
-- Ecuadorian cédula (10 digits) or RUC (13 digits); a phone is E.164 with a
-- leading plus sign. Anything else, or another identifier kind, is rejected.
payoutIdentifierValid :: Text -> Text -> Bool
payoutIdentifierValid kind identifier = case kind of
  "identification" -> T.length identifier `elem` [10, 13] && T.all isAsciiDigit identifier
  "phone" -> case T.uncons identifier of
    Just ('+', digits) ->
      T.length digits >= 8 && T.length digits <= 15 && T.all isAsciiDigit digits && T.take 1 digits /= "0"
    _ -> False
  _ -> False
  where
    isAsciiDigit character = character >= '0' && character <= '9'

-- I18 (ADR 0134): every offer state change belongs to this relation. Open
-- states can always be closed by the system (rejected/expired) and by the
-- offerer (withdrawn); counter states alternate actors.
allowedApplicationTransition :: ApplicationStatus -> ApplicationStatus -> Bool
allowedApplicationTransition fromStatus toStatus =
  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions
  where
    transitions = Set.fromList
      [ (ApplicationSubmitted, ApplicationViewed)
      , (ApplicationSubmitted, ApplicationShortlisted)
      , (ApplicationSubmitted, ApplicationCounteredByAuthor)
      , (ApplicationSubmitted, ApplicationAccepted)
      , (ApplicationSubmitted, ApplicationRejected)
      , (ApplicationSubmitted, ApplicationWithdrawn)
      , (ApplicationSubmitted, ApplicationExpired)
      , (ApplicationViewed, ApplicationShortlisted)
      , (ApplicationViewed, ApplicationCounteredByAuthor)
      , (ApplicationViewed, ApplicationAccepted)
      , (ApplicationViewed, ApplicationRejected)
      , (ApplicationViewed, ApplicationWithdrawn)
      , (ApplicationViewed, ApplicationExpired)
      , (ApplicationShortlisted, ApplicationCounteredByAuthor)
      , (ApplicationShortlisted, ApplicationAccepted)
      , (ApplicationShortlisted, ApplicationRejected)
      , (ApplicationShortlisted, ApplicationWithdrawn)
      , (ApplicationShortlisted, ApplicationExpired)
      , (ApplicationCounteredByAuthor, ApplicationCounteredByApplicant)
      , (ApplicationCounteredByAuthor, ApplicationAccepted)
      , (ApplicationCounteredByAuthor, ApplicationRejected)
      , (ApplicationCounteredByAuthor, ApplicationWithdrawn)
      , (ApplicationCounteredByAuthor, ApplicationExpired)
      , (ApplicationCounteredByApplicant, ApplicationCounteredByAuthor)
      , (ApplicationCounteredByApplicant, ApplicationAccepted)
      , (ApplicationCounteredByApplicant, ApplicationRejected)
      , (ApplicationCounteredByApplicant, ApplicationWithdrawn)
      , (ApplicationCounteredByApplicant, ApplicationExpired)
      , (ApplicationAccepted, ApplicationWithdrawn)
      , (ApplicationAccepted, ApplicationConversationOpen)
      , (ApplicationAccepted, ApplicationConverted)
      , (ApplicationConversationOpen, ApplicationWithdrawn)
      , (ApplicationConversationOpen, ApplicationConverted)
      ]

-- | Offers still under negotiation: they consume no slot and can be countered.
openApplicationStatuses :: Set ApplicationStatus
openApplicationStatuses = Set.fromList
  [ ApplicationSubmitted, ApplicationViewed, ApplicationShortlisted
  , ApplicationCounteredByAuthor, ApplicationCounteredByApplicant ]

applicationIsOpen :: ApplicationStatus -> Bool
applicationIsOpen status = status `Set.member` openApplicationStatuses

-- | Who may counter from a given state. The author counters any offer that
-- is waiting on the author; the applicant only answers an author counter.
counterAllowedFor :: ApplicationStatus -> NegotiationActor -> Bool
counterAllowedFor status ActorAuthor =
  status `elem` [ApplicationSubmitted, ApplicationViewed, ApplicationShortlisted, ApplicationCounteredByApplicant]
counterAllowedFor status ActorApplicant = status == ApplicationCounteredByAuthor

-- | Who may accept from a given state: whoever did not make the last move.
acceptAllowedFor :: ApplicationStatus -> NegotiationActor -> Bool
acceptAllowedFor status ActorAuthor =
  status `elem` [ApplicationSubmitted, ApplicationViewed, ApplicationShortlisted, ApplicationCounteredByApplicant]
acceptAllowedFor status ActorApplicant = status == ApplicationCounteredByAuthor

-- | Which target states each participant may request through the generic
-- status endpoint. Counter states are reached only through the counter
-- endpoint (which records the round), never by a bare status change.
participantTransitionAllowed :: Text -> ApplicationStatus -> ApplicationStatus -> Bool
participantTransitionAllowed "admin" _ _ = True
participantTransitionAllowed "author" from to = case to of
  ApplicationViewed -> True
  ApplicationShortlisted -> True
  ApplicationRejected -> applicationIsOpen from
  ApplicationAccepted -> acceptAllowedFor from ActorAuthor
  ApplicationConversationOpen -> True
  ApplicationConverted -> True
  _ -> False
participantTransitionAllowed "applicant" from to = case to of
  ApplicationWithdrawn -> True
  ApplicationAccepted -> acceptAllowedFor from ActorApplicant
  ApplicationConversationOpen -> True
  _ -> False
participantTransitionAllowed _ _ _ = False

maxNegotiationRounds :: Int
maxNegotiationRounds = 10

-- | A counter-offer may occupy any round but the last: the final round is
-- reserved for the acceptance that closes the chain, so an acceptance is
-- always recorded (I18).
counterRoundAllowed :: Int -> Bool
counterRoundAllowed nextRound = nextRound >= 2 && nextRound < maxNegotiationRounds

-- | The actor expected to produce the next round given the last one.
nextRoundActor :: NegotiationActor -> NegotiationActor
nextRoundActor ActorApplicant = ActorAuthor
nextRoundActor ActorAuthor = ActorApplicant

-- | I18: rounds are numbered consecutively from 1, start with the applicant's
-- initial proposal, alternate actors, never exceed the maximum, and an
-- acceptance can only be the final round.
negotiationRoundsValid :: [OfferRound] -> Bool
negotiationRoundsValid [] = False
negotiationRoundsValid rounds@(first : rest) =
  length rounds <= maxNegotiationRounds
    && roundNumber first == 1
    && roundActor first == ActorApplicant
    && roundKind first == RoundInitial
    && all ((/= RoundInitial) . roundKind) rest
    && and (zipWith consecutive rounds rest)
    && acceptOnlyLast rounds
  where
    consecutive previous next =
      roundNumber next == roundNumber previous + 1
        && roundActor next == nextRoundActor (roundActor previous)
        && roundKind previous /= RoundAccept
    acceptOnlyLast [] = True
    acceptOnlyLast [_] = True
    acceptOnlyLast (x : xs) = roundKind x /= RoundAccept && acceptOnlyLast xs

parsePriceMode :: Text -> Maybe PriceMode
parsePriceMode value = lookup (T.toLower (T.strip value))
  [("open", PriceOpen), ("suggested", PriceSuggested), ("fixed", PriceFixed)]

priceModeText :: PriceMode -> Text
priceModeText PriceOpen = "open"
priceModeText PriceSuggested = "suggested"
priceModeText PriceFixed = "fixed"

-- | Whether an offered amount is admissible for a classified price mode and
-- its reference price (@budget_max_minor@). Fixed prices admit only the exact
-- amount; open and suggested prices admit any non-negative amount.
offerAmountAcceptable :: PriceMode -> Maybe Int64 -> Maybe Int64 -> Bool
offerAmountAcceptable PriceFixed reference offered = reference /= Nothing && offered == reference
offerAmountAcceptable _ _ (Just offered) = offered >= 0
offerAmountAcceptable _ _ Nothing = True

-- | I19: accepting consumes exactly one slot and never drives the count
-- below zero. @Nothing@ means the classified has no slot left.
slotsAfterAccept :: Int -> Maybe Int
slotsAfterAccept remaining
  | remaining <= 0 = Nothing
  | otherwise = Just (remaining - 1)

-- | Withdrawing an accepted offer releases one slot, capped at the total.
slotsAfterRelease :: Int -> Int -> Int
slotsAfterRelease total remaining = min total (remaining + 1)

-- | Classified states that close every open offer. Pausing keeps offers
-- open (no new offers are admitted while paused, because applying requires a
-- published classified).
classifiedClosesOffers :: ClassifiedStatus -> Bool
classifiedClosesOffers status =
  status `elem` [Filled, Expired, Withdrawn, Moderated, Rejected]

closureRejectionReason :: ClassifiedStatus -> Maybe Text
closureRejectionReason Filled = Just "slots_filled"
closureRejectionReason status
  | classifiedClosesOffers status = Just "classified_closed"
  | otherwise = Nothing

allowedInvitationTransition :: InvitationStatus -> InvitationStatus -> Bool
allowedInvitationTransition fromStatus toStatus =
  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions
  where
    transitions = Set.fromList
      [ (InvitationPending, InvitationAccepted)
      , (InvitationPending, InvitationDeclined)
      , (InvitationPending, InvitationWithdrawn)
      , (InvitationPending, InvitationBlocked)
      , (InvitationPending, InvitationExpired)
      , (InvitationAccepted, InvitationWithdrawn)
      , (InvitationAccepted, InvitationBlocked)
      , (InvitationAccepted, InvitationConversationOpen)
      , (InvitationAccepted, InvitationConverted)
      , (InvitationConversationOpen, InvitationWithdrawn)
      , (InvitationConversationOpen, InvitationBlocked)
      , (InvitationConversationOpen, InvitationConverted)
      ]

capabilityAllows :: Bool -> Set DirectoryCapability -> DirectoryCapability -> Bool
capabilityAllows active granted requested = active && requested `Set.member` granted

permissionsFromProfessions :: Set PublicProfession -> Set DirectoryCapability
permissionsFromProfessions _ = Set.empty

publicSearchEligible :: Text -> Text -> Text -> Bool -> Bool
publicSearchEligible lifecycle visibility moderation current =
  lifecycle == "published" && visibility == "public" && moderation == "allowed" && current

applicationVisibleTo :: Integer -> Integer -> Integer -> Set Integer -> Bool
applicationVisibleTo viewer applicant author administrators =
  viewer == applicant || viewer == author || viewer `Set.member` administrators

verifiedReviewEligible
  :: Text -> Bool -> Bool -> Integer -> Integer -> Integer -> Integer -> Bool
verifiedReviewEligible interactionStatus verified authorManaged author subject profileA profileB =
  interactionStatus == "completed"
    && verified
    && authorManaged
    && author /= subject
    && ((author == profileA && subject == profileB) || (author == profileB && subject == profileA))

minorMayPublishOrRespond :: Text -> Bool
minorMayPublishOrRespond assurance = assurance `elem` ["adult_attested", "adult_verified", "guardian_approved"]

applyProfileFieldUpdate :: a -> ProfileFieldUpdate a -> a
applyProfileFieldUpdate current PreserveProfileField = current
applyProfileFieldUpdate _ (ReplaceProfileField next) = next

detailIdsMatch :: Ord a => [a] -> [a] -> Bool
detailIdsMatch ids detailIds =
  length ids == Set.size (Set.fromList ids)
    && length detailIds == Set.size (Set.fromList detailIds)
    && Set.fromList ids == Set.fromList detailIds

serviceAreaPrimaryValid :: [Bool] -> Bool
serviceAreaPrimaryValid primaryFlags =
  null primaryFlags || length (filter id primaryFlags) == 1

canonicalFavoriteKind :: Text -> Either Text Text
canonicalFavoriteKind rawKind
  | kind `Set.member` Set.fromList ["profile", "classified", "event", "venue"] = Right kind
  | otherwise = Left "invalid targetKind"
  where
    kind = T.toLower (T.strip rawKind)

canonicalFavoriteTarget :: Text -> Text -> Either Text (Text, Text)
canonicalFavoriteTarget rawKind rawIdentifier = do
  kind <- canonicalFavoriteKind rawKind
  identifier <- case kind of
    "event" -> canonicalNumericIdentifier "event" rawIdentifier
    "venue" -> canonicalNumericIdentifier "venue" rawIdentifier
    "profile" -> canonicalUuidIdentifier "profile" rawIdentifier
    "classified" -> canonicalUuidIdentifier "classified" rawIdentifier
    _ -> Left "invalid targetKind"
  pure (kind, identifier)

canonicalNumericIdentifier :: Text -> Text -> Either Text Text
canonicalNumericIdentifier kind rawIdentifier =
  case readMaybe (T.unpack (T.strip rawIdentifier)) :: Maybe Int64 of
    Just identifier | identifier > 0 -> Right (T.pack (show identifier))
    _ -> Left (kind <> " targetId must be a positive integer")

canonicalUuidIdentifier :: Text -> Text -> Either Text Text
canonicalUuidIdentifier kind rawIdentifier =
  case UUID.fromText (T.strip rawIdentifier) of
    Just identifier -> Right (UUID.toText identifier)
    Nothing -> Left (kind <> " targetId must be a UUID")
