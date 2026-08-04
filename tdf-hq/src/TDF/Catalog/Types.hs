{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Catalog.Types
  ( -- * Release Types
    ReleaseType(..)
  , CatalogRelease(..)
  , CatalogResource(..)
  , ResourceType(..)
  , CatalogReleaseResource(..)
    -- * Identifiers
  , IdentifierScheme(..)
  , CatalogIdentifier(..)
    -- * Credits
  , CreditRole(..)
  , CatalogCredit(..)
    -- * Deals
  , DealModel(..)
  , CatalogDeal(..)
  , CatalogDealTerritory(..)
  , TerritoryCode(..)
    -- * Assets
  , CatalogAsset(..)
  , AssetType(..)
  ) where

import Data.Time (UTCTime)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Type of release
data ReleaseType
  = Album
  | Single
  | EP
  | Compilation
  | LiveAlbum
  | RemixAlbum
  | Soundtrack
  | SpokenWord
  deriving (Show, Eq, Generic, Enum)

-- | Main Release entity in Canonical Catalog
data CatalogRelease = CatalogRelease
  { catalogReleaseId :: Int
  , catalogReleaseTitle :: Text
  , catalogReleaseSubTitle :: Maybe Text
  , catalogReleaseType :: ReleaseType
  , catalogReleaseDate :: Maybe UTCTime
  , catalogReleaseOriginalDate :: Maybe UTCTime
  , catalogReleaseLabel :: Maybe Text
  , catalogReleaseStatus :: Text -- 'Draft', 'Active', 'Takedown'
  , catalogReleaseCopyrightLine :: Maybe Text
  , catalogReleasePhonographicCopyrightLine :: Maybe Text
  , catalogReleaseGenre :: Maybe Text
  , catalogReleaseCoverArtAssetId :: Maybe Int
  , catalogReleaseCreatedAt :: UTCTime
  , catalogReleaseUpdatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

-- | Type of resource (track, video, image)
data ResourceType
  = SoundRecording
  | MusicVideo
  | Image
  | Text
  | Software
  deriving (Show, Eq, Generic, Enum)

-- | Individual Resource (Recording, Video, etc.)
data CatalogResource = CatalogResource
  { catalogResourceId :: Int
  , catalogResourceType :: ResourceType
  , catalogResourceTitle :: Text
  , catalogResourceVersion :: Maybe Text
  , catalogResourceDurationMs :: Maybe Int
  , catalogResourceLanguage :: Maybe Text
  , catalogResourceExplicitContent :: Bool
  , catalogResourceCreatedAt :: UTCTime
  , catalogResourceUpdatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

-- | Link between Release and Resource (Tracklist)
data CatalogReleaseResource = CatalogReleaseResource
  { catalogReleaseResourceId :: Int
  , catalogReleaseResourceReleaseId :: Int
  , catalogReleaseResourceResourceId :: Int
  , catalogReleaseResourceDiscNumber :: Int
  , catalogReleaseResourceSequence :: Int
  , catalogReleaseResourceIsPrimary :: Bool
  } deriving (Show, Eq, Generic)

-- | Scheme for identifiers
data IdentifierScheme
  = ISRC
  | UPC
  | EAN
  | GRid
  | IPI
  | ISNI
  | DPID
  | Proprietary
  deriving (Show, Eq, Generic, Enum)

-- | Generic Identifier table
data CatalogIdentifier = CatalogIdentifier
  { catalogIdentifierId :: Int
  , catalogIdentifierEntityId :: Int -- FK to Release, Resource, or Party
  , catalogIdentifierEntityType :: Text -- 'Release', 'Resource', 'Party'
  , catalogIdentifierScheme :: IdentifierScheme
  , catalogIdentifierValue :: Text
  , catalogIdentifierNamespace :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Role in a credit
data CreditRole
  = MainArtist
  | FeaturedArtist
  | Producer
  | Engineer
  | Mixer
  | MasteringEngineer
  | Composer
  | Lyricist
  | Arranger
  | Performer
  | StudioMusician
  deriving (Show, Eq, Generic, Enum)

-- | Credit linking a Party to an entity
data CatalogCredit = CatalogCredit
  { catalogCreditId :: Int
  , catalogCreditEntityId :: Int
  , catalogCreditEntityType :: Text
  , catalogCreditPartyId :: Int -- FK to Party
  , catalogCreditRole :: CreditRole
  , catalogCreditText :: Maybe Text
  , catalogCreditSequence :: Maybe Int
  } deriving (Show, Eq, Generic)

-- | Business Deal Model
data DealModel
  = ExclusiveLicense
  | DistributionAgreement
  | AdministrationDeal
  | PressAndDistribution
  deriving (Show, Eq, Generic, Enum)

-- | Commercial Deal
data CatalogDeal = CatalogDeal
  { catalogDealId :: Int
  , catalogDealReleaseId :: Maybe Int
  , catalogDealResourceId :: Maybe Int
  , catalogDealModel :: DealModel
  , catalogDealStartDate :: UTCTime
  , catalogDealEndDate :: Maybe UTCTime
  , catalogDealTakedownDate :: Maybe UTCTime
  , catalogDealPartnerName :: Text
  } deriving (Show, Eq, Generic)

-- | Territory included/excluded in a deal
data TerritoryCode
  = Worldwide
  | US
  | GB
  | DE
  | FR
  | JP
  | CA
  | AU
  | BR
  | MX
  | ES
  | IT
  | NL
  | SE
  | NO
  | DK
  | FI
  | PL
  | KR
  | CN
  deriving (Show, Eq, Generic, Enum)

data CatalogDealTerritory = CatalogDealTerritory
  { catalogDealTerritoryId :: Int
  , catalogDealTerritoryDealId :: Int
  , catalogDealTerritoryCode :: TerritoryCode
  , catalogDealTerritoryInclusion :: Bool -- True = Included, False = Excluded
  } deriving (Show, Eq, Generic)

-- | Type of asset file
data AssetType
  = AudioFile
  | ImageFile
  | DocumentFile
  | VideoFile
  deriving (Show, Eq, Generic, Enum)

-- | Physical/Digital Asset
data CatalogAsset = CatalogAsset
  { catalogAssetId :: Int
  , catalogAssetType :: AssetType
  , catalogAssetUri :: Text -- Private URI (S3/GCS)
  , catalogAssetLogicalName :: Text
  , catalogAssetMimeType :: Text
  , catalogAssetSizeBytes :: Int
  , catalogAssetSha256 :: Text
  , catalogAssetMetadataJson :: Maybe Text
  } deriving (Show, Eq, Generic)
