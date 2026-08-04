{-# LANGUAGE OverloadedStrings #-}

module TDF.DDEX.ERN.V432.BusinessRules
  ( -- * Business rule validation
    validateBusinessRules
  , BusinessRuleViolation(..)
  , RuleSeverity(..)
  ) where

import Data.Text (Text)
import TDF.DDEX.ERN.V432.Normalize

-- | Severity of a business rule violation
data RuleSeverity
  = RuleError
  | RuleWarning
  | RuleInfo
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Business rule violation
data BusinessRuleViolation = BusinessRuleViolation
  { brvRule       :: Text
  , brvSeverity   :: RuleSeverity
  , brvMessage    :: Text
  , brvElement    :: Maybe Text
  , brvSuggestion :: Maybe Text
  } deriving (Show, Eq)

-- | Validate business rules on a canonical import
-- TODO: Implement full business rule validation
-- Rules to implement:
-- - BR-001: All sound recordings must have ISRC
-- - BR-002: ISRC format validation (12 alphanumeric chars)
-- - BR-003: UPC format validation (8, 12, or 13 digits)
-- - BR-004: All resource references in releases must exist
-- - BR-005: Deal territories must be valid ISO codes or "Worldwide"
-- - BR-006: Releases should have copyright lines
-- - BR-007: Duration consistency check
validateBusinessRules :: CanonicalImport -> [BusinessRuleViolation]
validateBusinessRules _ci = []
