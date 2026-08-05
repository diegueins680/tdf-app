{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.Partner
  ( -- * Partner Profile
    PartnerProfile(..)
  , PartnerId
  , defaultPartnerProfile
    -- * Completeness Rules
  , RequiredField(..)
  , checkCompleteness
  , CompletenessResult(..)
    -- * Validation
  , validateForPartner
  , ValidationError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Int (Int64)
import TDF.Catalog.Types

-- | Partner profile identifier
type PartnerId = Int64

-- | Partner profile configuration
data PartnerProfile = PartnerProfile
  { ppId                :: PartnerId
  , ppName              :: Text
  , ppDpid              :: Maybe Text
  , ppAllowedVersions   :: [Text]        -- ^ Allowed ERN versions
  , ppRequiredFields    :: Set RequiredField  -- ^ Required fields for export
  , ppNamingConvention  :: Maybe Text    -- ^ Naming pattern
  , ppTerritoryDefaults :: Map Text Text -- ^ Default territory mappings
  , ppIsActive          :: Bool
  } deriving (Show, Eq)

-- | Default partner profile
defaultPartnerProfile :: PartnerProfile
defaultPartnerProfile = PartnerProfile
  { ppId = 0
  , ppName = "Default Partner"
  , ppDpid = Nothing
  , ppAllowedVersions = ["4.3.2"]
  , ppRequiredFields = defaultRequiredFields
  , ppNamingConvention = Nothing
  , ppTerritoryDefaults = Map.empty
  , ppIsActive = True
  }

-- | Default required fields for most partners
defaultRequiredFields :: Set RequiredField
defaultRequiredFields = Set.fromList
  [ ReqReleaseTitle
  , ReqReleaseType
  , ReqResourceTitle
  , ReqISRC
  , ReqCopyrightLine
  ]

-- | Required field for export
data RequiredField
  -- Release fields
  = ReqReleaseTitle
  | ReqReleaseType
  | ReqReleaseDate
  | ReqUPC
  | ReqCopyrightLine
  | ReqPhonographicCopyrightLine
  | ReqGenre
  -- Resource fields
  | ReqResourceTitle
  | ReqISRC
  | ReqDuration
  | ReqExplicitContent
  -- Party fields
  | ReqMainArtist
  | ReqLabel
  | ReqProducer
  -- Deal fields
  | ReqTerritories
  | ReqStartDate
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Result of completeness check
data CompletenessResult = CompletenessResult
  { crIsComplete      :: Bool
  , crMissingFields   :: [RequiredField]
  , crWarnings        :: [Text]
  , crCompletionPercent :: Int  -- 0-100
  } deriving (Show, Eq)

-- | Validation error
data ValidationError = ValidationError
  { veField    :: Text
  , veMessage  :: Text
  , veSeverity :: Text  -- "error", "warning", "info"
  } deriving (Show, Eq)

-- | Check completeness of catalog entities for a partner
checkCompleteness :: PartnerProfile -> [CatalogRelease] -> [CatalogResource] -> [CatalogCredit] -> CompletenessResult
checkCompleteness partner releases resources credits =
  let releaseChecks = concatMap (checkReleaseCompleteness partner) releases
      resourceChecks = concatMap (checkResourceCompleteness partner) resources
      creditChecks = checkCreditCompleteness partner credits
      allMissing = releaseChecks ++ resourceChecks ++ creditChecks
      totalRequired = Set.size (ppRequiredFields partner)
      missingCount = length (Set.fromList allMissing)
      completionPercent = if totalRequired == 0
        then 100
        else ((totalRequired - missingCount) * 100) `div` totalRequired
      warnings = generateWarnings partner releases resources
  in CompletenessResult
    { crIsComplete = null allMissing
    , crMissingFields = allMissing
    , crWarnings = warnings
    , crCompletionPercent = completionPercent
    }

-- | Check release completeness
checkReleaseCompleteness :: PartnerProfile -> CatalogRelease -> [RequiredField]
checkReleaseCompleteness partner CatalogRelease{..} =
  let required = ppRequiredFields partner
      missing = filter (`Set.member` required) $ concat
        [ [ReqReleaseTitle | T.null catalogReleaseTitle]
        , [ReqReleaseDate | case catalogReleaseDate of { Nothing -> True; _ -> False }]
        , [ReqCopyrightLine | case catalogReleaseCopyrightLine of { Nothing -> True; _ -> False }]
        , [ReqPhonographicCopyrightLine | case catalogReleasePhonographicCopyrightLine of { Nothing -> True; _ -> False }]
        , [ReqGenre | case catalogReleaseGenre of { Nothing -> True; _ -> False }]
        ]
  in missing

-- | Check resource completeness
checkResourceCompleteness :: PartnerProfile -> CatalogResource -> [RequiredField]
checkResourceCompleteness partner CatalogResource{..} =
  let required = ppRequiredFields partner
      missing = filter (`Set.member` required) $ concat
        [ [ReqResourceTitle | T.null catalogResourceTitle]
        , [ReqDuration | case catalogResourceDurationMs of { Nothing -> True; _ -> False }]
        ]
  in missing

-- | Check credit completeness
checkCreditCompleteness :: PartnerProfile -> [CatalogCredit] -> [RequiredField]
checkCreditCompleteness partner credits =
  let required = ppRequiredFields partner
      hasMainArtist = any (\c -> catalogCreditRole c == MainArtist) credits
      hasProducer = any (\c -> catalogCreditRole c == Producer) credits
      missing = filter (`Set.member` required) $ concat
        [ [ReqMainArtist | not hasMainArtist]
        , [ReqProducer | not hasProducer]
        ]
  in missing

-- | Generate warnings for completeness check
generateWarnings :: PartnerProfile -> [CatalogRelease] -> [CatalogResource] -> [Text]
generateWarnings partner releases resources =
  let noIsrc = filter (\r -> case catalogResourceDurationMs r of { Nothing -> True; _ -> False }) resources
      warnings = concat
        [ [ "Some resources missing duration" | not (null noIsrc) ]
        ]
  in warnings

-- | Validate catalog entities for a specific partner
validateForPartner :: PartnerProfile -> [CatalogRelease] -> [CatalogResource] -> [CatalogCredit] -> [ValidationError]
validateForPartner partner releases resources credits =
  let completeness = checkCompleteness partner releases resources credits
      errors = map missingFieldToError (crMissingFields completeness)
      warnings = map (\w -> ValidationError "general" w "warning") (crWarnings completeness)
  in errors ++ warnings

-- | Convert missing field to validation error
missingFieldToError :: RequiredField -> ValidationError
missingFieldToError field = ValidationError
  { veField = fieldName field
  , veMessage = "Missing required field: " <> fieldName field
  , veSeverity = "error"
  }

-- | Get field name for display
fieldName :: RequiredField -> Text
fieldName ReqReleaseTitle = "Release Title"
fieldName ReqReleaseType = "Release Type"
fieldName ReqReleaseDate = "Release Date"
fieldName ReqUPC = "UPC"
fieldName ReqCopyrightLine = "Copyright Line"
fieldName ReqPhonographicCopyrightLine = "Phonographic Copyright Line"
fieldName ReqGenre = "Genre"
fieldName ReqResourceTitle = "Resource Title"
fieldName ReqISRC = "ISRC"
fieldName ReqDuration = "Duration"
fieldName ReqExplicitContent = "Explicit Content Flag"
fieldName ReqMainArtist = "Main Artist"
fieldName ReqLabel = "Label"
fieldName ReqProducer = "Producer"
fieldName ReqTerritories = "Territories"
fieldName ReqStartDate = "Start Date"
