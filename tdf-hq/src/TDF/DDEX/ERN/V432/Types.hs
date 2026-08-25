{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DDEX.ERN.V432.Types
  ( -- * Main Message
    ErnMessage(..)
  , MessageHeader(..)
    -- * Party
  , Party(..)
  , PartyId(..)
  , PartyName(..)
    -- * Resource
  , Resource(..)
  , ResourceId(..)
  , ResourceType(..)
  , ResourceReference(..)
  , ResourceContributor(..)
    -- * Release
  , Release(..)
  , ReleaseId(..)
  , ReleaseType(..)
  , ReleaseReference(..)
  , ReleaseContributor(..)
    -- * Resource Group
  , ResourceGroup(..)
  , ResourceGroupContent(..)
    -- * Deal
  , Deal(..)
  , DealTerms(..)
  , TerritoryCode(..)
    -- * Common
  , ISRC(..)
  , GRid(..)
  , ISWC(..)
  , Duration(..)
  , Date(..)
  , Language(..)
  ) where

import Data.Text (Text)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)

-- | Complete ERN 4.3.2 NewReleaseMessage
data ErnMessage = ErnMessage
  { ernMessageHeader  :: MessageHeader
  , ernPartyList      :: [Party]
  , ernResourceList   :: [Resource]
  , ernReleaseList    :: [Release]
  , ernResourceGroups :: [ResourceGroup]
  , ernDealList       :: [Deal]
  } deriving (Show, Eq, Generic)

-- | Message header with metadata
data MessageHeader = MessageHeader
  { mhMessageId       :: Text
  , mhMessageThreadId :: Maybe Text
  , mhSenderPartyId   :: PartyId
  , mhRecipientPartyId :: PartyId
  , mhMessageCreatedDateTime :: UTCTime
  , mhMessageAuditTrail :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Party (artist, label, producer, etc.)
data Party = Party
  { partyPartyId      :: PartyId
  , partyPartyName    :: Maybe PartyName
  , partyIpiNumber    :: Maybe Text
  , partyIsniNumber   :: Maybe Text
  , partyDPID         :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Party identifier (can be DPID, IPI, ISNI, or proprietary)
data PartyId
  = PartyIdDPID Text
  | PartyIdIPI Text
  | PartyIdISNI Text
  | PartyIdProprietary Text Text  -- namespace, value
  deriving (Show, Eq, Generic)

-- | Party name with possible aliases
data PartyName = PartyName
  { pnFullName       :: Text
  , pnNamesToDisplay :: [Text]
  } deriving (Show, Eq, Generic)

-- | Resource (sound recording, video, image, etc.)
data Resource = Resource
  { resourceReference     :: ResourceReference
  , resourceType          :: ResourceType
  , resourceIds           :: [ResourceId]
  , resourceTitle         :: Text
  , resourceSubTitle      :: Maybe Text
  , resourceContributors  :: [ResourceContributor]
  , resourceDuration      :: Maybe Duration
  , resourceLanguage      :: Maybe Language
  , resourceExplicitContent :: Bool
  , resourceParentResourceRef :: Maybe ResourceReference
  } deriving (Show, Eq, Generic)

-- | Resource identifier (ISRC, GRid, or proprietary)
data ResourceId
  = ResourceIdISRC ISRC
  | ResourceIdGRid GRid
  | ResourceIdProprietary Text Text
  deriving (Show, Eq, Generic)

-- | Type of resource
data ResourceType
  = ResourceTypeSoundRecording
  | ResourceTypeMusicVideo
  | ResourceTypeImage
  | ResourceTypeText
  | ResourceTypeSoftware
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Reference to a resource within the message
data ResourceReference = ResourceReference Text
  deriving (Show, Eq, Ord, Generic)

-- | Resource contributor (artist, producer, etc.)
data ResourceContributor = ResourceContributor
  { rcPartyReference    :: Text  -- Reference to Party
  , rcRole              :: Text  -- Role code (MainArtist, Producer, etc.)
  , rcCreditText        :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Release (album, single, EP, etc.)
data Release = Release
  { releaseReference    :: ReleaseReference
  , releaseType         :: ReleaseType
  , releaseIds          :: [ReleaseId]
  , releaseTitle        :: Text
  , releaseSubTitle     :: Maybe Text
  , releaseContributors :: [ReleaseContributor]
  , releaseResourceRefs :: [ResourceReference]
  , releaseDuration     :: Maybe Duration
  , releaseDate         :: Maybe Date
  , releaseCopyrightLine :: Maybe Text
  , releasePhonographicCopyrightLine :: Maybe Text
  , releaseGenre        :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Release identifier (UPC/EAN, GRid, or proprietary)
data ReleaseId
  = ReleaseIdUPC Text      -- UPC or EAN
  | ReleaseIdGRid GRid
  | ReleaseIdCatalogNumber Text Text  -- label, catalog number
  | ReleaseIdProprietary Text Text
  deriving (Show, Eq, Generic)

-- | Type of release
data ReleaseType
  = ReleaseTypeAlbum
  | ReleaseTypeSingle
  | ReleaseTypeEP
  | ReleaseTypeCompilation
  | ReleaseTypeLiveAlbum
  | ReleaseTypeRemixAlbum
  | ReleaseTypeSoundtrack
  | ReleaseTypeSpokenWord
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Reference to a release within the message
data ReleaseReference = ReleaseReference Text
  deriving (Show, Eq, Ord, Generic)

-- | Release contributor
data ReleaseContributor = ReleaseContributor
  { relcPartyReference :: Text
  , relcRole           :: Text
  } deriving (Show, Eq, Generic)

-- | Resource group (tracklist organization)
data ResourceGroup = ResourceGroup
  { rgSequenceNumber   :: Maybe Int
  , rgTitle            :: Maybe Text
  , rgContent          :: [ResourceGroupContent]
  , rgSubGroups        :: [ResourceGroup]
  } deriving (Show, Eq, Generic)

-- | Content within a resource group
data ResourceGroupContent
  = RGCResource ResourceReference
  | RGCDisc (Maybe Int) [ResourceGroup]
  | RGCSequence Int ResourceReference
  deriving (Show, Eq, Generic)

-- | Commercial deal for a release
data Deal = Deal
  { dealDealTerms      :: DealTerms
  , dealReleaseRefs    :: [ReleaseReference]
  , dealResourceRefs   :: [ResourceReference]
  , dealEffectiveDate  :: Date
  } deriving (Show, Eq, Generic)

-- | Deal terms with territories and usage
data DealTerms = DealTerms
  { dtTerritoryCodes   :: [TerritoryCode]
  , dtUsageType        :: Text  -- PermanentDownload, OnDemandStream, etc.
  , dtPriceType        :: Maybe Text
  , dtWholesalePrice   :: Maybe Text
  , dtRetailPrice      :: Maybe Text
  , dtStartDate        :: Date
  , dtEndDate          :: Maybe Date
  , dtTakedownDate     :: Maybe Date
  } deriving (Show, Eq, Generic)

-- | ISO 3166 territory code or "Worldwide"
data TerritoryCode
  = TerritoryWorldwide
  | TerritoryCode Text  -- ISO 3166-1 alpha-2
  deriving (Show, Eq, Ord, Generic)

-- | International Standard Recording Code
data ISRC = ISRC
  { isrcCountryCode :: Text
  , isrcRegistrant  :: Text
  , isrcYear        :: Text
  , isrcDesignation :: Text
  } deriving (Show, Eq, Ord, Generic)

-- | Global Release Identifier
data GRid = GRid Text
  deriving (Show, Eq, Ord, Generic)

-- | International Standard Musical Work Code
data ISWC = ISWC Text
  deriving (Show, Eq, Ord, Generic)

-- | Duration in ISO 8601 format (PT##H##M##S)
data Duration = Duration
  { durationHours   :: Int
  , durationMinutes :: Int
  , durationSeconds :: Int
  } deriving (Show, Eq, Ord, Generic)

-- | Date (can be year-only, year-month, or full date)
data Date
  = DateYear Int
  | DateYearMonth Int Int
  | DateFull Day
  deriving (Show, Eq, Ord, Generic)

-- | ISO 639 language code
data Language = Language Text
  deriving (Show, Eq, Ord, Generic)
