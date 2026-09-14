{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.EventOperations.Types
  ( EventLifecycleState(..)
  , EventOperationSnapshotDTO(..)
  , EventTransitionCommand(..)
  , EventTransitionOutcomeDTO(..)
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
