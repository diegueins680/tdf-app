{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.Operations.Types
  ( WorkStatus(..)
  , WorkPriority(..)
  , SlaState(..)
  , WorkItemDTO(..)
  , WorkItemEventDTO(..)
  , WorkItemNoteDTO(..)
  , WorkItemDetailDTO(..)
  , WorkItemPageDTO(..)
  , OperationsMetricsDTO(..)
  , StreamEventDTO(..)
  , StreamBatchDTO(..)
  , VersionedCommand(..)
  , TransitionCommand(..)
  , AssignmentCommand(..)
  , PriorityCommand(..)
  , NoteCreate(..)
  , ManualWorkItemCreate(..)
  , ApprovalCreate(..)
  , ApprovalDecision(..)
  , ApprovalDTO(..)
  , IntegrationFailureDTO(..)
  , ReplayCommand(..)
  , SavedViewDTO(..)
  , SavedViewCreate(..)
  , PushSubscriptionCreate(..)
  , PushSubscriptionDTO(..)
  , parseWorkStatus
  , parseWorkPriority
  , workStatusText
  , workPriorityText
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value
  , withText
  )
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data WorkStatus
  = WorkNew
  | WorkSeen
  | WorkAssigned
  | WorkInProgress
  | WorkWaiting
  | WorkResolved
  | WorkArchived
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

workStatusText :: WorkStatus -> Text
workStatusText WorkNew = "new"
workStatusText WorkSeen = "seen"
workStatusText WorkAssigned = "assigned"
workStatusText WorkInProgress = "in_progress"
workStatusText WorkWaiting = "waiting"
workStatusText WorkResolved = "resolved"
workStatusText WorkArchived = "archived"

parseWorkStatus :: Text -> Maybe WorkStatus
parseWorkStatus raw = case T.toLower (T.strip raw) of
  "new" -> Just WorkNew
  "seen" -> Just WorkSeen
  "assigned" -> Just WorkAssigned
  "in_progress" -> Just WorkInProgress
  "waiting" -> Just WorkWaiting
  "resolved" -> Just WorkResolved
  "archived" -> Just WorkArchived
  _ -> Nothing

instance ToJSON WorkStatus where
  toJSON = toJSON . workStatusText

instance FromJSON WorkStatus where
  parseJSON = withText "WorkStatus" $ \raw ->
    maybe (fail "invalid operational work status") pure (parseWorkStatus raw)

data WorkPriority = Urgent | High | Normal | Low
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

workPriorityText :: WorkPriority -> Text
workPriorityText Urgent = "urgent"
workPriorityText High = "high"
workPriorityText Normal = "normal"
workPriorityText Low = "low"

parseWorkPriority :: Text -> Maybe WorkPriority
parseWorkPriority raw = case T.toLower (T.strip raw) of
  "urgent" -> Just Urgent
  "high" -> Just High
  "normal" -> Just Normal
  "low" -> Just Low
  _ -> Nothing

instance ToJSON WorkPriority where
  toJSON = toJSON . workPriorityText

instance FromJSON WorkPriority where
  parseJSON = withText "WorkPriority" $ \raw ->
    maybe (fail "invalid operational priority") pure (parseWorkPriority raw)

data SlaState = SlaOnTrack | SlaAtRisk | SlaDue | SlaBreached | SlaPaused
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON SlaState where
  toJSON value = toJSON $ case value of
    SlaOnTrack -> ("on_track" :: Text)
    SlaAtRisk -> "at_risk"
    SlaDue -> "due"
    SlaBreached -> "breached"
    SlaPaused -> "paused"

instance FromJSON SlaState where
  parseJSON = withText "SlaState" $ \raw -> case T.toLower (T.strip raw) of
    "on_track" -> pure SlaOnTrack
    "at_risk" -> pure SlaAtRisk
    "due" -> pure SlaDue
    "breached" -> pure SlaBreached
    "paused" -> pure SlaPaused
    _ -> fail "invalid SLA state"

data WorkItemDTO = WorkItemDTO
  { id :: UUID
  , organizationId :: UUID
  , branchId :: Maybe UUID
  , sourceSystem :: Text
  , sourceChannel :: Text
  , entityType :: Text
  , entityId :: Maybe Text
  , uncorrelated :: Bool
  , correlationKey :: Text
  , titleEs :: Text
  , titleEn :: Text
  , descriptionEs :: Text
  , descriptionEn :: Text
  , status :: WorkStatus
  , priority :: WorkPriority
  , recommendedPriority :: WorkPriority
  , severity :: Text
  , seen :: Bool
  , firstSeenBy :: Maybe Int64
  , firstSeenAt :: Maybe UTCTime
  , assigneePartyId :: Maybe Int64
  , responsibleTeam :: Maybe Text
  , customerPartyId :: Maybe Int64
  , serviceKey :: Maybe Text
  , amountMinor :: Maybe Int64
  , currency :: Maybe Text
  , paymentState :: Maybe Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , dueAt :: Maybe UTCTime
  , snoozedUntil :: Maybe UTCTime
  , waitingReason :: Maybe Text
  , waitingExternalDependency :: Bool
  , resumeAt :: Maybe UTCTime
  , resolvedAt :: Maybe UTCTime
  , archivedAt :: Maybe UTCTime
  , slaState :: SlaState
  , version :: Int64
  , metadata :: Value
  } deriving (Show, Eq, Generic)

instance ToJSON WorkItemDTO
instance FromJSON WorkItemDTO

data WorkItemEventDTO = WorkItemEventDTO
  { id :: Int64
  , eventType :: Text
  , actorPartyId :: Maybe Int64
  , actorRole :: Maybe Text
  , bodyEs :: Text
  , bodyEn :: Text
  , metadata :: Value
  , occurredAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON WorkItemEventDTO
instance FromJSON WorkItemEventDTO

data WorkItemNoteDTO = WorkItemNoteDTO
  { id :: UUID
  , authorPartyId :: Int64
  , body :: Text
  , mentionedPartyIds :: [Int64]
  , createdAt :: UTCTime
  , editedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON WorkItemNoteDTO
instance FromJSON WorkItemNoteDTO

data WorkItemDetailDTO = WorkItemDetailDTO
  { workItem :: WorkItemDTO
  , events :: [WorkItemEventDTO]
  , notes :: [WorkItemNoteDTO]
  , allowedTransitions :: [WorkStatus]
  , sourceRecordUrl :: Maybe Text
  , quickActions :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON WorkItemDetailDTO
instance FromJSON WorkItemDetailDTO

data WorkItemPageDTO = WorkItemPageDTO
  { items :: [WorkItemDTO]
  , nextCursor :: Maybe Text
  , hasMore :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON WorkItemPageDTO
instance FromJSON WorkItemPageDTO

data OperationsMetricsDTO = OperationsMetricsDTO
  { newRegistrations :: Int64
  , registrationsRequiringAttention :: Int64
  , reservationsAwaitingConfirmation :: Int64
  , todaySessions :: Int64
  , schedulingConflicts :: Int64
  , unpaidInvoices :: Int64
  , overdueInvoices :: Int64
  , paymentsAwaitingVerification :: Int64
  , revenueReceivedTodayMinor :: Int64
  , unassignedWork :: Int64
  , slaBreaches :: Int64
  , averageFirstResponseSeconds :: Maybe Double
  , averageResolutionSeconds :: Maybe Double
  , integrationFailures :: Int64
  , currency :: Text
  , calculatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON OperationsMetricsDTO
instance FromJSON OperationsMetricsDTO

data StreamEventDTO = StreamEventDTO
  { id :: Int64
  , eventType :: Text
  , workItemId :: Maybe UUID
  , payload :: Value
  , createdAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON StreamEventDTO
instance FromJSON StreamEventDTO

data StreamBatchDTO = StreamBatchDTO
  { events :: [StreamEventDTO]
  , lastEventId :: Maybe Int64
  , retryAfterMs :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON StreamBatchDTO
instance FromJSON StreamBatchDTO

data VersionedCommand = VersionedCommand
  { expectedVersion :: Int64
  , reason :: Maybe Text
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON VersionedCommand
instance FromJSON VersionedCommand

data TransitionCommand = TransitionCommand
  { expectedVersion :: Int64
  , targetStatus :: WorkStatus
  , reason :: Maybe Text
  , waitingExternalDependency :: Maybe Bool
  , resumeAt :: Maybe UTCTime
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON TransitionCommand
instance FromJSON TransitionCommand

data AssignmentCommand = AssignmentCommand
  { expectedVersion :: Int64
  , assigneePartyId :: Maybe Int64
  , responsibleTeam :: Maybe Text
  , reason :: Maybe Text
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON AssignmentCommand
instance FromJSON AssignmentCommand

data PriorityCommand = PriorityCommand
  { expectedVersion :: Int64
  , priority :: WorkPriority
  , reason :: Text
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON PriorityCommand
instance FromJSON PriorityCommand

data NoteCreate = NoteCreate
  { body :: Text
  , mentionedPartyIds :: [Int64]
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON NoteCreate
instance FromJSON NoteCreate

data ManualWorkItemCreate = ManualWorkItemCreate
  { organizationId :: UUID
  , branchId :: Maybe UUID
  , entityType :: Text
  , entityId :: Maybe Text
  , uncorrelated :: Bool
  , correlationKey :: Text
  , titleEs :: Text
  , titleEn :: Text
  , descriptionEs :: Text
  , descriptionEn :: Text
  , priority :: WorkPriority
  , responsibleTeam :: Maybe Text
  , customerPartyId :: Maybe Int64
  , serviceKey :: Maybe Text
  , amountMinor :: Maybe Int64
  , currency :: Maybe Text
  , metadata :: Value
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ManualWorkItemCreate
instance FromJSON ManualWorkItemCreate

data ApprovalCreate = ApprovalCreate
  { organizationId :: UUID
  , branchId :: Maybe UUID
  , workItemId :: Maybe UUID
  , actionType :: Text
  , targetEntityType :: Text
  , targetEntityId :: Text
  , amountMinor :: Maybe Int64
  , currency :: Maybe Text
  , reason :: Text
  , idempotencyKey :: Text
  , expiresAt :: Maybe UTCTime
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ApprovalCreate
instance FromJSON ApprovalCreate

data ApprovalDecision = ApprovalDecision
  { decision :: Text
  , reason :: Text
  , expectedDecision :: Text
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ApprovalDecision
instance FromJSON ApprovalDecision

data ApprovalDTO = ApprovalDTO
  { id :: UUID
  , organizationId :: UUID
  , branchId :: Maybe UUID
  , workItemId :: Maybe UUID
  , actionType :: Text
  , targetEntityType :: Text
  , targetEntityId :: Text
  , amountMinor :: Maybe Int64
  , currency :: Maybe Text
  , requesterPartyId :: Int64
  , requesterRole :: Text
  , requestReason :: Text
  , requestedAt :: UTCTime
  , approverPartyId :: Maybe Int64
  , approverRole :: Maybe Text
  , decision :: Text
  , decisionReason :: Maybe Text
  , decidedAt :: Maybe UTCTime
  , expiresAt :: Maybe UTCTime
  , executionStatus :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ApprovalDTO
instance FromJSON ApprovalDTO

data IntegrationFailureDTO = IntegrationFailureDTO
  { id :: UUID
  , organizationId :: UUID
  , branchId :: Maybe UUID
  , provider :: Text
  , direction :: Text
  , sourceRecordType :: Text
  , sourceRecordId :: Text
  , failureCode :: Text
  , redactedSummary :: Text
  , retryable :: Bool
  , status :: Text
  , attemptCount :: Int
  , lastAttemptAt :: Maybe UTCTime
  , nextAttemptAt :: Maybe UTCTime
  , createdAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON IntegrationFailureDTO
instance FromJSON IntegrationFailureDTO

data ReplayCommand = ReplayCommand
  { reason :: Text
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ReplayCommand
instance FromJSON ReplayCommand

data SavedViewDTO = SavedViewDTO
  { id :: UUID
  , organizationId :: UUID
  , ownerPartyId :: Maybe Int64
  , name :: Text
  , shared :: Bool
  , filters :: Value
  , columns :: Value
  , widgets :: Value
  , subscribedEventTypes :: Value
  , updatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON SavedViewDTO
instance FromJSON SavedViewDTO

data SavedViewCreate = SavedViewCreate
  { organizationId :: UUID
  , name :: Text
  , shared :: Bool
  , filters :: Value
  , columns :: Value
  , widgets :: Value
  , subscribedEventTypes :: Value
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON SavedViewCreate
instance FromJSON SavedViewCreate

data PushSubscriptionCreate = PushSubscriptionCreate
  { organizationId :: UUID
  , platform :: Text
  , deviceToken :: Text
  , requestId :: Text
  , sourceClient :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON PushSubscriptionCreate
instance FromJSON PushSubscriptionCreate

data PushSubscriptionDTO = PushSubscriptionDTO
  { id :: UUID
  , organizationId :: UUID
  , platform :: Text
  , active :: Bool
  , updatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON PushSubscriptionDTO
instance FromJSON PushSubscriptionDTO
