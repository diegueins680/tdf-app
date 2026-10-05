{-# LANGUAGE OverloadedStrings #-}

module TDF.Directory.Policy
  ( ProfileStatus(..)
  , ClassifiedStatus(..)
  , ApplicationStatus(..)
  , InvitationStatus(..)
  , DirectoryCapability(..)
  , PublicProfession(..)
  , ProfileFieldUpdate(..)
  , allowedClaimTransition
  , allowedProfileTransition
  , allowedClassifiedTransition
  , allowedApplicationTransition
  , allowedInvitationTransition
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

data ApplicationStatus
  = ApplicationSubmitted | ApplicationViewed | ApplicationShortlisted
  | ApplicationAccepted | ApplicationRejected | ApplicationWithdrawn
  | ApplicationConversationOpen | ApplicationConverted
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
       "approved", "rejected", "withdrawn"]
    transitions = Set.fromList
      [ ("draft", "submitted"), ("draft", "withdrawn")
      , ("submitted", "under_review"), ("submitted", "withdrawn")
      , ("under_review", "approved"), ("under_review", "rejected")
      , ("under_review", "more_evidence_requested"), ("under_review", "withdrawn")
      , ("more_evidence_requested", "submitted"), ("more_evidence_requested", "withdrawn")
      , ("rejected", "submitted")
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
      , (Expired, Published), (Expired, Withdrawn)
      , (Rejected, Draft), (Rejected, Withdrawn)
      , (Moderated, Draft), (Moderated, Withdrawn)
      ]

allowedApplicationTransition :: ApplicationStatus -> ApplicationStatus -> Bool
allowedApplicationTransition fromStatus toStatus =
  fromStatus == toStatus || (fromStatus, toStatus) `Set.member` transitions
  where
    transitions = Set.fromList
      [ (ApplicationSubmitted, ApplicationViewed)
      , (ApplicationSubmitted, ApplicationShortlisted)
      , (ApplicationSubmitted, ApplicationAccepted)
      , (ApplicationSubmitted, ApplicationRejected)
      , (ApplicationSubmitted, ApplicationWithdrawn)
      , (ApplicationViewed, ApplicationShortlisted)
      , (ApplicationViewed, ApplicationAccepted)
      , (ApplicationViewed, ApplicationRejected)
      , (ApplicationViewed, ApplicationWithdrawn)
      , (ApplicationShortlisted, ApplicationAccepted)
      , (ApplicationShortlisted, ApplicationRejected)
      , (ApplicationShortlisted, ApplicationWithdrawn)
      , (ApplicationAccepted, ApplicationWithdrawn)
      , (ApplicationAccepted, ApplicationConversationOpen)
      , (ApplicationAccepted, ApplicationConverted)
      , (ApplicationConversationOpen, ApplicationWithdrawn)
      , (ApplicationConversationOpen, ApplicationConverted)
      ]

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
