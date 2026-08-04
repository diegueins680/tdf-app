{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DDEX.Types
  ( -- * Document States
    DdexDocumentStatus(..)
  , documentStatusToText
  , textToDocumentStatus
    -- * Document Family
  , DdexFamily(..)
  , familyToText
  , textToFamily
    -- * Detection
  , DdexDetection(..)
  , DetectionConfidence(..)
    -- * Validation
  , ValidationSeverity(..)
  , ValidationLayer(..)
  , ValidationIssue(..)
  , ValidationResult(..)
    -- * Jobs
  , DdexJobType(..)
  , DdexJobStatus(..)
    -- * Import
  , ImportPlanStatus(..)
  , ImportOperation(..)
  , ConflictAction(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | State machine for DDEX document lifecycle
data DdexDocumentStatus
  = StatusReceived
  | StatusQuarantined
  | StatusQueued
  | StatusValidating
  | StatusInvalid
  | StatusValid
  | StatusMappingRequired
  | StatusReadyToImport
  | StatusImporting
  | StatusImported
  | StatusImportFailed
  | StatusSuperseded
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

documentStatusToText :: DdexDocumentStatus -> Text
documentStatusToText StatusReceived        = "received"
documentStatusToText StatusQuarantined     = "quarantined"
documentStatusToText StatusQueued          = "queued"
documentStatusToText StatusValidating      = "validating"
documentStatusToText StatusInvalid         = "invalid"
documentStatusToText StatusValid           = "valid"
documentStatusToText StatusMappingRequired = "mapping_required"
documentStatusToText StatusReadyToImport   = "ready_to_import"
documentStatusToText StatusImporting       = "importing"
documentStatusToText StatusImported        = "imported"
documentStatusToText StatusImportFailed    = "import_failed"
documentStatusToText StatusSuperseded      = "superseded"

textToDocumentStatus :: Text -> Maybe DdexDocumentStatus
textToDocumentStatus "received"         = Just StatusReceived
textToDocumentStatus "quarantined"      = Just StatusQuarantined
textToDocumentStatus "queued"           = Just StatusQueued
textToDocumentStatus "validating"       = Just StatusValidating
textToDocumentStatus "invalid"          = Just StatusInvalid
textToDocumentStatus "valid"            = Just StatusValid
textToDocumentStatus "mapping_required" = Just StatusMappingRequired
textToDocumentStatus "ready_to_import"  = Just StatusReadyToImport
textToDocumentStatus "importing"        = Just StatusImporting
textToDocumentStatus "imported"         = Just StatusImported
textToDocumentStatus "import_failed"    = Just StatusImportFailed
textToDocumentStatus "superseded"       = Just StatusSuperseded
textToDocumentStatus _                  = Nothing

-- | DDEX message family
data DdexFamily
  = FamilyERN  -- Electronic Release Notification
  | FamilyRIN  -- Release Information Notification
  | FamilyDSR  -- Digital Sales Report
  | FamilyMEAD -- Metadata for Audio-Visual
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

familyToText :: DdexFamily -> Text
familyToText FamilyERN  = "ERN"
familyToText FamilyRIN  = "RIN"
familyToText FamilyDSR  = "DSR"
familyToText FamilyMEAD = "MEAD"

textToFamily :: Text -> Maybe DdexFamily
textToFamily "ERN"  = Just FamilyERN
textToFamily "RIN"  = Just FamilyRIN
textToFamily "DSR"  = Just FamilyDSR
textToFamily "MEAD" = Just FamilyMEAD
textToFamily _      = Nothing

-- | Result of detecting document type
data DdexDetection = DdexDetection
  { detectionFamily    :: DdexFamily
  , detectionVersion   :: Text
  , detectionNamespace :: Text
  , detectionRoot      :: Text
  , detectionConfidence :: DetectionConfidence
  } deriving (Show, Eq, Generic)

data DetectionConfidence
  = ConfidenceHigh   -- Clear namespace and root match
  | ConfidenceMedium -- Partial match, needs manual verification
  | ConfidenceLow    -- Ambiguous, requires human review
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Severity of a validation issue
data ValidationSeverity
  = SeverityError
  | SeverityWarning
  | SeverityInfo
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Layer where the issue was detected
data ValidationLayer
  = LayerXML      -- Well-formedness
  | LayerXSD      -- Schema validation
  | LayerAVS      -- Allowed Value Sets
  | LayerBusiness -- Semantic rules
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | A single validation issue
data ValidationIssue = ValidationIssue
  { issueSeverity  :: ValidationSeverity
  , issueLayer     :: ValidationLayer
  , issueCode      :: Maybe Text
  , issueMessage   :: Text
  , issueLine      :: Maybe Int
  , issueColumn    :: Maybe Int
  , issueXPath     :: Maybe Text
  , issueSuggestion :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Overall validation result
data ValidationResult = ValidationResult
  { validationResult :: !Bool  -- True if valid
  , resultErrors     :: ![ValidationIssue]
  , resultWarnings   :: ![ValidationIssue]
  , resultInfo       :: ![ValidationIssue]
  } deriving (Show, Eq, Generic)

-- | Type of background job
data DdexJobType
  = JobValidate
  | JobImport
  | JobExport
  | JobCleanup
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Status of a background job
data DdexJobStatus
  = JobPending
  | JobProcessing
  | JobCompleted
  | JobFailed
  | JobRetry
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Status of an import plan
data ImportPlanStatus
  = PlanDraft
  | PlanResolved
  | PlanCommitted
  | PlanAbandoned
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Operation performed during import
data ImportOperation
  = OpCreate
  | OpUpdate
  | OpSkip
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Action to resolve a conflict
data ConflictAction
  = ActionUseExisting
  | ActionCreateNew
  | ActionIgnore
  | ActionKeepInternal
  | ActionReplaceWithDdex
  | ActionMarkForReview
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
