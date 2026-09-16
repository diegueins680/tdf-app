{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.EventOperations.Types
  ( EventLifecycleState(..)
  , EventOperationSnapshotDTO(..)
  , EventTransitionCommand(..)
  , EventTransitionOutcomeDTO(..)
  , EventTaskStatus(..)
  , EventRaciRole(..)
  , EventRaciAssignmentDTO(..)
  , EventTaskPolicyDTO(..)
  , EventOperationTaskDTO(..)
  , EventTaskAggregateRevision
  , parseEventTaskAggregateRevision
  , EventOperationTaskWithRevisionDTO(..)
  , EventRaciReassignmentCommand(..)
  , EventRaciReassignmentOutcomeDTO(..)
  , EventRaciEditorContextDTO(..)
  , validRaciReassignmentCommand
  , EventTaskCompletionCommand(..)
  , EventTaskCompletionOutcomeDTO(..)
  , validTaskCompletionCommand
  , aggregateRevisionInteger
  , raciRoleText
  , isSafePositiveInteger
  , allEventLifecycleStates
  , eventLifecycleStateText
  , parseEventLifecycleState
  ) where

import Data.Aeson
  ( FromJSON(..)
  , Options
  , ToJSON(..)
  , Value(..)
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , omitNothingFields
  , rejectUnknownFields
  , withText
  )
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (toLower)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)

data EventLifecycleState
  = Draft
  | Planning
  | PendingApproval
  | Approved
  | Published
  | Staffing
  | Ready
  | InProgress
  | Completed
  | SettlementPending
  | Settled
  | Archived
  | Reprogrammed
  | Cancelled
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

allEventLifecycleStates :: [EventLifecycleState]
allEventLifecycleStates = [minBound .. maxBound]

eventLifecycleStateText :: EventLifecycleState -> Text
eventLifecycleStateText state = case state of
  Draft -> "draft"
  Planning -> "planning"
  PendingApproval -> "pending_approval"
  Approved -> "approved"
  Published -> "published"
  Staffing -> "staffing"
  Ready -> "ready"
  InProgress -> "in_progress"
  Completed -> "completed"
  SettlementPending -> "settlement_pending"
  Settled -> "settled"
  Archived -> "archived"
  Reprogrammed -> "reprogrammed"
  Cancelled -> "cancelled"

parseEventLifecycleState :: Text -> Maybe EventLifecycleState
parseEventLifecycleState raw = case T.toLower (T.strip raw) of
  "draft" -> Just Draft
  "planning" -> Just Planning
  "pending_approval" -> Just PendingApproval
  "approved" -> Just Approved
  "published" -> Just Published
  "staffing" -> Just Staffing
  "ready" -> Just Ready
  "in_progress" -> Just InProgress
  "completed" -> Just Completed
  "settlement_pending" -> Just SettlementPending
  "settled" -> Just Settled
  "archived" -> Just Archived
  "reprogrammed" -> Just Reprogrammed
  "cancelled" -> Just Cancelled
  _ -> Nothing

instance ToJSON EventLifecycleState where
  toJSON = toJSON . eventLifecycleStateText

instance FromJSON EventLifecycleState where
  parseJSON = withText "EventLifecycleState" $ \raw ->
    maybe (fail "invalid canonical event lifecycle state") pure
      (parseEventLifecycleState raw)

data EventOperationSnapshotDTO = EventOperationSnapshotDTO
  { eosEventId :: Int64
  , eosCanonicalState :: EventLifecycleState
  , eosVersion :: Int64
  , eosLegacyStateCode :: Maybe Text
  , eosCapabilities :: [Text]
  , eosAvailableTransitions :: [EventLifecycleState]
  } deriving (Eq, Generic, Show)

instance ToJSON EventOperationSnapshotDTO where
  toJSON = genericToJSON (prefixedJsonOptions 3)

instance FromJSON EventOperationSnapshotDTO where
  parseJSON = genericParseJSON (prefixedJsonOptions 3)

data EventTransitionCommand = EventTransitionCommand
  { etcExpectedVersion :: Int64
  , etcTargetState :: EventLifecycleState
  , etcReason :: Maybe Text
  , etcCorrelationId :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON EventTransitionCommand where
  toJSON = genericToJSON (prefixedJsonOptions 3)

instance FromJSON EventTransitionCommand where
  parseJSON raw@(Object fields) = do
    case KeyMap.lookup "reason" fields of
      Just Null -> fail "reason must be omitted rather than null"
      _ -> pure ()
    genericParseJSON (prefixedJsonOptions 3) raw
  parseJSON raw = genericParseJSON (prefixedJsonOptions 3) raw

data EventTransitionOutcomeDTO = EventTransitionOutcomeDTO
  { etoEventId :: Int64
  , etoCanonicalState :: EventLifecycleState
  , etoVersion :: Int64
  , etoCommandId :: UUID
  , etoAuthorityCode :: Text
  , etoReplayed :: Bool
  } deriving (Eq, Generic, Show)

instance ToJSON EventTransitionOutcomeDTO where
  toJSON = genericToJSON (prefixedJsonOptions 3)

instance FromJSON EventTransitionOutcomeDTO where
  parseJSON = genericParseJSON (prefixedJsonOptions 3)

data EventTaskStatus = TaskPlanned | TaskConfirmed | TaskInProgress | TaskCompleted | TaskCancelled
  deriving (Bounded, Enum, Eq, Ord, Show)

taskStatusText :: EventTaskStatus -> Text
taskStatusText status = case status of
  TaskPlanned -> "planned"
  TaskConfirmed -> "confirmed"
  TaskInProgress -> "in_progress"
  TaskCompleted -> "completed"
  TaskCancelled -> "cancelled"

instance ToJSON EventTaskStatus where
  toJSON = toJSON . taskStatusText
instance FromJSON EventTaskStatus where
  parseJSON = withText "EventTaskStatus" $ \raw ->
    maybe (fail "invalid task status") pure (lookup raw [(taskStatusText s, s) | s <- [minBound..maxBound]])

data EventRaciRole = RaciResponsible | RaciAccountable | RaciConsulted | RaciInformed
  deriving (Bounded, Enum, Eq, Ord, Show)

raciRoleText :: EventRaciRole -> Text
raciRoleText role = case role of
  RaciResponsible -> "responsible"
  RaciAccountable -> "accountable"
  RaciConsulted -> "consulted"
  RaciInformed -> "informed"

instance ToJSON EventRaciRole where
  toJSON = toJSON . raciRoleText
instance FromJSON EventRaciRole where
  parseJSON = withText "EventRaciRole" $ \raw ->
    maybe (fail "invalid RACI role") pure (lookup raw [(raciRoleText r, r) | r <- [minBound..maxBound]])

data EventRaciAssignmentDTO = EventRaciAssignmentDTO
  { eraPartyId :: Int64
  , eraRole :: EventRaciRole
  } deriving (Eq, Generic, Show)
instance ToJSON EventRaciAssignmentDTO where
  toJSON = genericToJSON (prefixedJsonOptions 3)
instance FromJSON EventRaciAssignmentDTO where
  parseJSON = genericParseJSON (prefixedJsonOptions 3)

data EventTaskPolicyDTO = EventTaskPolicyDTO
  { etpRequiresAccountability :: Bool
  , etpDependenciesGateCompletion :: Bool
  , etpVersion :: Int64
  } deriving (Eq, Generic, Show)
instance ToJSON EventTaskPolicyDTO where
  toJSON = genericToJSON (prefixedJsonOptions 3)
instance FromJSON EventTaskPolicyDTO where
  parseJSON = genericParseJSON (prefixedJsonOptions 3)

data EventOperationTaskDTO = EventOperationTaskDTO
  { eotEventId :: Int64
  , eotActivityId :: Int64
  , eotStatus :: EventTaskStatus
  , eotVersion :: Int64
  , eotPolicy :: Maybe EventTaskPolicyDTO
  , eotRaci :: [EventRaciAssignmentDTO]
  , eotAccountabilityNeedsAttention :: Bool
  } deriving (Eq, Generic, Show)
instance ToJSON EventOperationTaskDTO where
  toJSON = genericToJSON (prefixedJsonOptions 3)
instance FromJSON EventOperationTaskDTO where
  parseJSON raw@(Object fields) = do
    case KeyMap.lookup "policy" fields of
      Just Null -> fail "policy must be omitted rather than null"
      _ -> pure ()
    genericParseJSON (prefixedJsonOptions 3) raw
  parseJSON raw = genericParseJSON (prefixedJsonOptions 3) raw

-- Constructor intentionally private: revisions are canonical, bounded decimal TEXT.
newtype EventTaskAggregateRevision = EventTaskAggregateRevision Text
  deriving (Eq, Show)

parseEventTaskAggregateRevision :: Text -> Maybe EventTaskAggregateRevision
parseEventTaskAggregateRevision raw
  | not (T.null raw) && T.length raw <= 19
    && T.head raw /= '0' && T.all (\c -> c >= '0' && c <= '9') raw
    && (T.length raw < 19 || raw <= "9223372036854775807") = Just (EventTaskAggregateRevision raw)
  | otherwise = Nothing

instance ToJSON EventTaskAggregateRevision where
  toJSON (EventTaskAggregateRevision raw) = String raw
instance FromJSON EventTaskAggregateRevision where
  parseJSON = withText "EventTaskAggregateRevision" $ \raw ->
    maybe (fail "invalid aggregate revision") pure (parseEventTaskAggregateRevision raw)

data EventOperationTaskWithRevisionDTO = EventOperationTaskWithRevisionDTO
  { etrTask :: EventOperationTaskDTO
  , etrAggregateRevision :: EventTaskAggregateRevision
  } deriving (Eq, Generic, Show)
instance ToJSON EventOperationTaskWithRevisionDTO where
  toJSON = genericToJSON (prefixedJsonOptions 3)
instance FromJSON EventOperationTaskWithRevisionDTO where
  parseJSON = genericParseJSON (prefixedJsonOptions 3)

aggregateRevisionInteger :: EventTaskAggregateRevision -> Integer
aggregateRevisionInteger (EventTaskAggregateRevision raw) =
  T.foldl' (\n c -> n * 10 + toInteger (fromEnum c - fromEnum '0')) 0 raw

data EventTaskCompletionCommand = EventTaskCompletionCommand
  { etcpExpectedRevision :: EventTaskAggregateRevision
  , etcpReason :: Text
  , etcpCorrelationId :: Text
  } deriving (Eq, Generic, Show)
instance ToJSON EventTaskCompletionCommand where
  toJSON = genericToJSON (prefixedJsonOptions 4)
instance FromJSON EventTaskCompletionCommand where
  parseJSON raw = do
    command <- genericParseJSON (prefixedJsonOptions 4) raw
    if validTaskCompletionCommand command then pure command else fail "invalid completion command"

validTaskCompletionCommand :: EventTaskCompletionCommand -> Bool
validTaskCompletionCommand command =
  validText 2000 (etcpReason command) && validText 200 (etcpCorrelationId command)
  where validText limit raw = not (T.null (T.strip raw)) && T.length raw <= limit

data EventTaskCompletionOutcomeDTO = EventTaskCompletionOutcomeDTO
  { etcoEventId :: Int64
  , etcoActivityId :: Int64
  , etcoCommandId :: UUID
  , etcoStatus :: EventTaskStatus
  , etcoActivityVersion :: Int64
  , etcoAggregateRevision :: EventTaskAggregateRevision
  , etcoReplayed :: Bool
  } deriving (Eq, Generic, Show)
instance ToJSON EventTaskCompletionOutcomeDTO where
  toJSON = genericToJSON (prefixedJsonOptions 4)
instance FromJSON EventTaskCompletionOutcomeDTO where
  parseJSON = genericParseJSON (prefixedJsonOptions 4)

data EventRaciReassignmentCommand = EventRaciReassignmentCommand
  { ercExpectedRevision :: EventTaskAggregateRevision
  , ercRole :: EventRaciRole
  , ercFromPartyId :: Int64
  , ercToPartyId :: Int64
  , ercReason :: Text
  , ercCorrelationId :: Text
  } deriving (Eq, Generic, Show)
instance ToJSON EventRaciReassignmentCommand where
  toJSON = genericToJSON (prefixedJsonOptions 3)
instance FromJSON EventRaciReassignmentCommand where
  parseJSON raw = do
    command <- genericParseJSON (prefixedJsonOptions 3) raw
    if validRaciReassignmentCommand command then pure command else fail "invalid RACI command"

validRaciReassignmentCommand :: EventRaciReassignmentCommand -> Bool
validRaciReassignmentCommand command =
  all isSafePositiveInteger [ercFromPartyId command, ercToPartyId command]
  && ercFromPartyId command /= ercToPartyId command
  && validText 2000 (ercReason command) && validText 200 (ercCorrelationId command)
  where validText limit raw = not (T.null (T.strip raw)) && T.length raw <= limit

data EventRaciReassignmentOutcomeDTO = EventRaciReassignmentOutcomeDTO
  { eroEventId :: Int64
  , eroActivityId :: Int64
  , eroCommandId :: UUID
  , eroRole :: EventRaciRole
  , eroFromPartyId :: Int64
  , eroToPartyId :: Int64
  , eroAggregateRevision :: EventTaskAggregateRevision
  , eroReplayed :: Bool
  } deriving (Eq, Generic, Show)
instance ToJSON EventRaciReassignmentOutcomeDTO where
  toJSON = genericToJSON (prefixedJsonOptions 3)
instance FromJSON EventRaciReassignmentOutcomeDTO where
  parseJSON = genericParseJSON (prefixedJsonOptions 3)

-- Advisory editor context, never a command-authorization certificate.
data EventRaciEditorContextDTO = EventRaciEditorContextDTO
  { eccEventId :: Int64
  , eccActivityId :: Int64
  , eccAggregateRevision :: EventTaskAggregateRevision
  , eccCanManage :: Bool
  , eccOperationReady :: Bool
  , eccReplaceableAssignments :: [EventRaciAssignmentDTO]
  , eccEligiblePartyIds :: [Int64]
  , eccNextAfterPartyId :: Maybe Int64
  } deriving (Eq, Generic, Show)
instance ToJSON EventRaciEditorContextDTO where
  toJSON = genericToJSON (prefixedJsonOptions 3)
instance FromJSON EventRaciEditorContextDTO where
  parseJSON raw@(Object fields) = do
    case KeyMap.lookup "nextAfterPartyId" fields of
      Just Null -> fail "nextAfterPartyId must be omitted rather than null"
      _ -> pure ()
    genericParseJSON (prefixedJsonOptions 3) raw
  parseJSON raw = genericParseJSON (prefixedJsonOptions 3) raw

-- Existing JSON-number fields keep their original safe-integer restriction.
isSafePositiveInteger :: Int64 -> Bool
isSafePositiveInteger value = value > 0 && value <= 9007199254740991

prefixedJsonOptions :: Int -> Options
prefixedJsonOptions prefixLength =
  defaultOptions
    { fieldLabelModifier = lowerFirst . drop prefixLength
    , omitNothingFields = True
    , rejectUnknownFields = True
    }
  where
    lowerFirst [] = []
    lowerFirst (firstCharacter : rest) = toLower firstCharacter : rest
