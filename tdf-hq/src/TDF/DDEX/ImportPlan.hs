{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.ImportPlan
  ( -- * Import planning
    createImportPlan
  , ImportPlan(..)
  , ImportPlanConflict(..)
  , ConflictType(..)
  , ImportPlanChange(..)
  , ChangeOperation(..)
    -- * Conflict resolution
  , ConflictResolution(..)
  , ResolutionAction(..)
    -- * Dry run
  , dryRunImport
  , DryRunResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import Data.Time (UTCTime, getCurrentTime)
import Data.Int (Int64)
import TDF.DDEX.ERN.V432.Normalize

-- | Import plan with detected conflicts and proposed changes
data ImportPlan = ImportPlan
  { ipDocumentId      :: Int64
  , ipCanonicalImport :: CanonicalImport
  , ipConflicts       :: [ImportPlanConflict]
  , ipChanges         :: [ImportPlanChange]
  , ipWarnings        :: [Text]
  , ipCreatedAt       :: UTCTime
  } deriving (Show, Eq)

-- | Detected conflict during import planning
data ImportPlanConflict = ImportPlanConflict
  { ipcConflictType    :: ConflictType
  , ipcEntityType      :: Text
  , ipcIdentifier      :: Text
  , ipcDescription     :: Text
  , ipcExistingId      :: Maybe Int64
  , ipcSuggestedAction :: ResolutionAction
  } deriving (Show, Eq)

-- | Type of conflict detected
data ConflictType
  = ConflictDuplicateIsrc
  | ConflictDuplicateUpc
  | ConflictDuplicateParty
  | ConflictDataMismatch
  | ConflictMissingReference
  | ConflictAmbiguousMatch
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Proposed change to catalog
data ImportPlanChange = ImportPlanChange
  { ipchOperation     :: ChangeOperation
  , ipchEntityType    :: Text
  , ipchEntityRef     :: Text
  , ipchDescription   :: Text
  , ipchPreviousState :: Maybe Text
  , ipchNewState      :: Text
  } deriving (Show, Eq)

-- | Type of change operation
data ChangeOperation
  = OpCreateEntity
  | OpUpdateEntity
  | OpLinkEntity
  | OpSkipEntity
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Resolution action for a conflict
data ResolutionAction
  = ActionUseExisting
  | ActionCreateNew
  | ActionMerge
  | ActionIgnore
  | ActionManualReview
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Conflict resolution provided by user
data ConflictResolution = ConflictResolution
  { crConflictId     :: Int
  , crAction         :: ResolutionAction
  , crTargetId       :: Maybe Int64
  , crOverrideValues :: Map Text Text
  } deriving (Show, Eq)

-- | Result of a dry-run import
data DryRunResult = DryRunResult
  { drrIsValid   :: Bool
  , drrChanges   :: [ImportPlanChange]
  , drrConflicts :: [ImportPlanConflict]
  , drrWarnings  :: [Text]
  , drrSummary   :: Text
  } deriving (Show, Eq)

-- | Create an import plan from a canonical import
-- TODO: Implement conflict detection against existing catalog
createImportPlan :: Int64 -> CanonicalImport -> IO ImportPlan
createImportPlan docId ci = do
  now <- getCurrentTime
  return ImportPlan
    { ipDocumentId = docId
    , ipCanonicalImport = ci
    , ipConflicts = []
    , ipChanges = []
    , ipWarnings = []
    , ipCreatedAt = now
    }

-- | Perform a dry-run import without committing
-- TODO: Implement full dry-run logic
dryRunImport :: CanonicalImport -> IO DryRunResult
dryRunImport _ci =
  return DryRunResult
    { drrIsValid = True
    , drrChanges = []
    , drrConflicts = []
    , drrWarnings = []
    , drrSummary = "Dry-run not yet implemented"
    }
