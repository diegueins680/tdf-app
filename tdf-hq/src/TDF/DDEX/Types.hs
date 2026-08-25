{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DDEX.Types
  ( -- * Document Family
    DdexFamily(..)
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
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Parser discriminants needed for family detection. These are executable
-- protocol constants, not the authoritative list shown to users; governed
-- standards and their runtime support live in ddex_standard_version and
-- ddex_standard_support.
data DdexFamily
  = FamilyERN  -- Electronic Release Notification
  | FamilyRIN  -- Recording Information Notification
  | FamilyDSR  -- Digital Sales Report
  | FamilyMEAD -- Media Enrichment and Description
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
