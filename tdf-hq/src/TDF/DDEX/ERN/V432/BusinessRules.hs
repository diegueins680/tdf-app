{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.ERN.V432.BusinessRules
  ( -- * Business rule validation
    validateBusinessRules
  , BusinessRuleViolation(..)
  , RuleSeverity(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing, mapMaybe)
import TDF.DDEX.ERN.V432.Normalize
import qualified TDF.Catalog.Types as Catalog

-- | Severity of a business rule violation
data RuleSeverity
  = RuleError     -- Must be fixed before import
  | RuleWarning   -- Should be reviewed but can proceed
  | RuleInfo      -- Informational notice
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
validateBusinessRules :: CanonicalImport -> [BusinessRuleViolation]
validateBusinessRules ci = concat
  [ validateRequiredIdentifiers ci
  , validateIsrcFormat ci
  , validateUpcFormat ci
  , validateResourceReferences ci
  , validateDealTerritories ci
  , validateCopyrightLines ci
  , validateDurationConsistency ci
  ]

-- | Rule BR-001: All sound recordings must have ISRC
validateRequiredIdentifiers :: CanonicalImport -> [BusinessRuleViolation]
validateRequiredIdentifiers ci =
  let resourcesWithoutIsrc = filter isSoundRecordingWithoutIsrc (ciResources ci)
  in map mkViolation resourcesWithoutIsrc
  where
    isSoundRecordingWithoutIsrc CanonicalResource{..} =
      cresResourceType == Catalog.SoundRecording && isNothing cresIsrc
    mkViolation CanonicalResource{..} = BusinessRuleViolation
      { brvRule = "BR-001"
      , brvSeverity = RuleError
      , brvMessage = "Sound recording missing ISRC: " <> cresTitle
      , brvElement = Just cresSourcePartyRef
      , brvSuggestion = Just "Add ISRC before importing"
      }

-- | Rule BR-002: ISRC format validation (12 alphanumeric characters)
validateIsrcFormat :: CanonicalImport -> [BusinessRuleViolation]
validateIsrcFormat ci =
  let invalidIsrcs = filter (not . isValidIsrc) (mapMaybe cresIsrc (ciResources ci))
  in map mkViolation invalidIsrcs
  where
    isValidIsrc isrc = T.length isrc == 12 && T.all isAlphaNum isrc
    isAlphaNum c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    mkViolation isrc = BusinessRuleViolation
      { brvRule = "BR-002"
      , brvSeverity = RuleWarning
      , brvMessage = "ISRC format may be invalid: " <> isrc
      , brvElement = Nothing
      , brvSuggestion = Just "ISRC should be 12 alphanumeric characters (e.g., USRC17607839)"
      }

-- | Rule BR-003: UPC format validation (8, 12, or 13 digits)
validateUpcFormat :: CanonicalImport -> [BusinessRuleViolation]
validateUpcFormat ci =
  let invalidUpcs = filter (not . isValidUpc) (mapMaybe crUpc (ciReleases ci))
  in map mkViolation invalidUpcs
  where
    isValidUpc upc = T.length upc `elem` [8, 12, 13] && T.all isDigit upc
    isDigit c = c >= '0' && c <= '9'
    mkViolation upc = BusinessRuleViolation
      { brvRule = "BR-003"
      , brvSeverity = RuleWarning
      , brvMessage = "UPC format may be invalid: " <> upc
      , brvElement = Nothing
      , brvSuggestion = Just "UPC should be 8, 12, or 13 digits"
      }

-- | Rule BR-004: All resource references in releases must exist
validateResourceReferences :: CanonicalImport -> [BusinessRuleViolation]
validateResourceReferences ci =
  let validRefs = map cresSourcePartyRef (ciResources ci)
      invalidRefs = concatMap (checkRefs validRefs) (ciReleases ci)
  in map mkViolation invalidRefs
  where
    checkRefs validRefs CanonicalRelease{..} =
      filter (`notElem` validRefs) crResourceRefs
    mkViolation ref = BusinessRuleViolation
      { brvRule = "BR-004"
      , brvSeverity = RuleError
      , brvMessage = "Release references non-existent resource: " <> ref
      , brvElement = Just ref
      , brvSuggestion = Just "Ensure all resource references are valid"
      }

-- | Rule BR-005: Deal territories must be valid ISO codes or "Worldwide"
validateDealTerritories :: CanonicalImport -> [BusinessRuleViolation]
validateDealTerritories ci =
  let invalidTerritories = filter (not . isValidTerritory) (concatMap cdealTerritories (ciDeals ci))
  in map mkViolation invalidTerritories
  where
    isValidTerritory t = t == "Worldwide" || (T.length t == 2 && T.all isAlpha t)
    isAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
    mkViolation t = BusinessRuleViolation
      { brvRule = "BR-005"
      , brvSeverity = RuleWarning
      , brvMessage = "Invalid territory code: " <> t
      , brvElement = Nothing
      , brvSuggestion = Just "Use ISO 3166-1 alpha-2 codes (e.g., US, GB) or 'Worldwide'"
      }

-- | Rule BR-006: Releases should have copyright lines
validateCopyrightLines :: CanonicalImport -> [BusinessRuleViolation]
validateCopyrightLines ci =
  let releasesWithoutCopyright = filter isMissingCopyright (ciReleases ci)
  in map mkViolation releasesWithoutCopyright
  where
    isMissingCopyright CanonicalRelease{..} =
      isNothing crCopyrightLine && isNothing crPhonographicCopyrightLine
    mkViolation CanonicalRelease{..} = BusinessRuleViolation
      { brvRule = "BR-006"
      , brvSeverity = RuleWarning
      , brvMessage = "Release missing copyright lines: " <> crTitle
      , brvElement = Just crSourcePartyRef
      , brvSuggestion = Just "Add CLine (copyright) and/or PLine (phonographic copyright)"
      }

-- | Rule BR-007: Total duration should match sum of resource durations
validateDurationConsistency :: CanonicalImport -> [BusinessRuleViolation]
validateDurationConsistency ci =
  let mismatches = concatMap checkDuration (ciReleases ci)
  in map mkViolation mismatches
  where
    resourceDurationMap = [(cresSourcePartyRef, fromMaybe 0 cresDurationMs) | CanonicalResource{..} <- ciResources ci]
    fromMaybe def Nothing = def
    fromMaybe _ (Just x) = x

    checkDuration CanonicalRelease{..} =
      let totalResourceDuration = sum [maybe 0 id (lookup ref resourceDurationMap) | ref <- crResourceRefs]
      in []  -- Simplified: would need release duration to compare
    mkViolation ref = BusinessRuleViolation
      { brvRule = "BR-007"
      , brvSeverity = RuleInfo
      , brvMessage = "Duration mismatch for release: " <> ref
      , brvElement = Just ref
      , brvSuggestion = Just "Verify track durations match release duration"
      }
