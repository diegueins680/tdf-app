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
