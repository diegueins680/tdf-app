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
    -- * Transactional import
  , executeImport
  , ImportResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import Data.Time (UTCTime, getCurrentTime)
import Data.Int (Int64)
import Database.Persist.Sql (SqlPersistT)
import Control.Monad.IO.Class (liftIO)
import TDF.DDEX.ERN.V432.Normalize
import qualified TDF.DDEX.MatchEngine as ME

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

-- | Result of an actual import
data ImportResult = ImportResult
  { irSuccess       :: Bool
  , irEntitiesCreated :: Int
  , irEntitiesUpdated :: Int
  , irEntitiesSkipped :: Int
  , irErrors        :: [Text]
  } deriving (Show, Eq)

-- | Create an import plan from a canonical import
createImportPlan :: Int64 -> CanonicalImport -> SqlPersistT IO ImportPlan
createImportPlan docId ci = do
  -- Detect conflicts using match engine
  matchResult <- ME.detectConflicts ci
  now <- liftIO getCurrentTime

  -- Convert MatchEngine conflicts to ImportPlan conflicts
  let conflicts = map conflictToImportConflict (ME.matchConflicts matchResult)
      changes = generateChanges ci conflicts
      warnings = ME.matchWarnings matchResult

  return ImportPlan
    { ipDocumentId = docId
    , ipCanonicalImport = ci
    , ipConflicts = conflicts
    , ipChanges = changes
    , ipWarnings = warnings
    , ipCreatedAt = now
    }

-- | Convert MatchEngine Conflict to ImportPlanConflict
conflictToImportConflict :: ME.Conflict -> ImportPlanConflict
conflictToImportConflict c = ImportPlanConflict
  { ipcConflictType = convertConflictType (ME.conflictType c)
  , ipcEntityType = ME.conflictEntityType c
  , ipcIdentifier = ME.conflictIdentifier c
  , ipcDescription = ME.conflictDescription c
  , ipcExistingId = ME.conflictExistingId c
  , ipcSuggestedAction = convertSuggestedAction (ME.conflictSuggestedAction c)
  }

-- | Convert ConflictType
convertConflictType :: ME.ConflictType -> ConflictType
convertConflictType ME.ConflictDuplicateIsrc = ConflictDuplicateIsrc
convertConflictType ME.ConflictDuplicateUpc = ConflictDuplicateUpc
convertConflictType ME.ConflictDuplicateParty = ConflictDuplicateParty
convertConflictType ME.ConflictDataMismatch = ConflictDataMismatch
convertConflictType ME.ConflictMissingReference = ConflictMissingReference
convertConflictType ME.ConflictAmbiguousMatch = ConflictAmbiguousMatch

-- | Convert suggested action text to ResolutionAction
convertSuggestedAction :: Text -> ResolutionAction
convertSuggestedAction "UseExisting" = ActionUseExisting
convertSuggestedAction "CreateNew" = ActionCreateNew
convertSuggestedAction "Merge" = ActionMerge
convertSuggestedAction "Review" = ActionManualReview
convertSuggestedAction _ = ActionManualReview

-- | Generate proposed changes from canonical import and conflicts
generateChanges :: CanonicalImport -> [ImportPlanConflict] -> [ImportPlanChange]
generateChanges ci conflicts =
  let resourceChanges = map resourceToChange (ciResources ci)
      releaseChanges = map releaseToChange (ciReleases ci)
      partyChanges = map partyToChange (ciParties ci)
  in resourceChanges ++ releaseChanges ++ partyChanges
  where
    resourceToChange CanonicalResource{..} = ImportPlanChange
      { ipchOperation = OpCreateEntity
      , ipchEntityType = "Resource"
      , ipchEntityRef = cresSourcePartyRef
      , ipchDescription = "Create resource: " <> cresTitle
      , ipchPreviousState = Nothing
      , ipchNewState = "Resource: " <> cresTitle <> " (ISRC: " <> maybe "none" id cresIsrc <> ")"
      }

    releaseToChange CanonicalRelease{..} = ImportPlanChange
      { ipchOperation = OpCreateEntity
      , ipchEntityType = "Release"
      , ipchEntityRef = crSourcePartyRef
      , ipchDescription = "Create release: " <> crTitle
      , ipchPreviousState = Nothing
      , ipchNewState = "Release: " <> crTitle <> " (UPC: " <> maybe "none" id crUpc <> ")"
      }

    partyToChange CanonicalParty{..} = ImportPlanChange
      { ipchOperation = OpCreateEntity
      , ipchEntityType = "Party"
      , ipchEntityRef = cpSourcePartyRef
      , ipchDescription = "Create party: " <> cpName
      , ipchPreviousState = Nothing
      , ipchNewState = "Party: " <> cpName
      }

-- | Perform a dry-run import without committing
dryRunImport :: Int64 -> CanonicalImport -> SqlPersistT IO DryRunResult
dryRunImport docId ci = do
  plan <- createImportPlan docId ci
  let isValid = not $ any requiresManualReview (ipConflicts plan)
      summary = generateSummary (ipChanges plan) (ipConflicts plan)
  return DryRunResult
    { drrIsValid = isValid
    , drrChanges = ipChanges plan
    , drrConflicts = ipConflicts plan
    , drrWarnings = ipWarnings plan
    , drrSummary = summary
    }

-- | Check if a conflict requires manual review
requiresManualReview :: ImportPlanConflict -> Bool
requiresManualReview c = ipcSuggestedAction c == ActionManualReview

-- | Generate summary text for dry-run result
generateSummary :: [ImportPlanChange] -> [ImportPlanConflict] -> Text
generateSummary changes conflicts =
  let creates = length [c | c <- changes, ipchOperation c == OpCreateEntity]
      updates = length [c | c <- changes, ipchOperation c == OpUpdateEntity]
      links = length [c | c <- changes, ipchOperation c == OpLinkEntity]
      conflictCount = length conflicts
      manualReviewCount = length [c | c <- conflicts, requiresManualReview c]
  in T.unlines
      [ "Import Summary:"
      , "  Entities to create: " <> T.pack (show creates)
      , "  Entities to update: " <> T.pack (show updates)
      , "  Links to create: " <> T.pack (show links)
      , "  Conflicts detected: " <> T.pack (show conflictCount)
      , "  Requiring manual review: " <> T.pack (show manualReviewCount)
      ]

-- | Execute the actual import transactionally
executeImport :: ImportPlan -> [ConflictResolution] -> SqlPersistT IO ImportResult
executeImport plan resolutions = do
  -- TODO: Implement actual database insertion
  -- For now, return a stub result
  return ImportResult
    { irSuccess = True
    , irEntitiesCreated = length (ipChanges plan)
    , irEntitiesUpdated = 0
    , irEntitiesSkipped = 0
    , irErrors = []
    }
