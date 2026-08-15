{-# LANGUAGE OverloadedStrings #-}

module TDF.Directory.Policy
  ( ProfileStatus(..)
  , ClassifiedStatus(..)
  , ApplicationStatus(..)
  , InvitationStatus(..)
  , DirectoryCapability(..)
  , PublicProfession(..)
  , allowedProfileTransition
  , allowedClassifiedTransition
  , allowedApplicationTransition
  , allowedInvitationTransition
  , capabilityAllows
  , permissionsFromProfessions
  , publicSearchEligible
  , applicationVisibleTo
  , minorMayPublishOrRespond
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

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
      , (ApplicationSubmitted, ApplicationConversationOpen)
      , (ApplicationViewed, ApplicationShortlisted)
      , (ApplicationViewed, ApplicationAccepted)
      , (ApplicationViewed, ApplicationRejected)
      , (ApplicationViewed, ApplicationWithdrawn)
      , (ApplicationViewed, ApplicationConversationOpen)
      , (ApplicationShortlisted, ApplicationAccepted)
      , (ApplicationShortlisted, ApplicationRejected)
      , (ApplicationShortlisted, ApplicationWithdrawn)
      , (ApplicationShortlisted, ApplicationConversationOpen)
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
      , (InvitationPending, InvitationConversationOpen)
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

minorMayPublishOrRespond :: Text -> Bool
minorMayPublishOrRespond assurance = assurance `elem` ["adult_attested", "adult_verified", "guardian_approved"]
