{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.ERN.V432.Convert
  ( -- * Conversion
    catalogToErn
  , ConvertConfig(..)
  , defaultConvertConfig
  , ConvertError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Data.Maybe (mapMaybe, catMaybes)
import TDF.DDEX.ERN.V432.Types
import TDF.Catalog.Types

-- | Configuration for catalog-to-ERN conversion
data ConvertConfig = ConvertConfig
  { ccSenderDpid     :: Text      -- ^ Sender DPID (TDF's DPID)
  , ccRecipientDpid  :: Text      -- ^ Recipient DPID (DSP's DPID)
  , ccMessageId      :: Text      -- ^ Unique message ID
  , ccPartnerName    :: Text      -- ^ Partner name for deals
  } deriving (Show, Eq)

-- | Default conversion configuration
defaultConvertConfig :: ConvertConfig
defaultConvertConfig = ConvertConfig
  { ccSenderDpid = ""
  , ccRecipientDpid = ""
  , ccMessageId = ""
  , ccPartnerName = ""
  }

-- | Conversion error
data ConvertError = ConvertError
  { ceMessage :: Text
  , ceEntity  :: Maybe Text
  } deriving (Show, Eq)

-- | Convert catalog entities to ERN message
catalogToErn :: ConvertConfig -> UTCTime -> [CatalogRelease] -> [CatalogResource] -> [CatalogCredit] -> [CatalogDeal] -> Either [ConvertError] ErnMessage
catalogToErn config now releases resources credits deals = do
  validateConvertConfig config
  -- Build message header
  let header = buildMessageHeader config now

  -- Build parties from credits
  let parties = buildParties credits

  -- Build resources
  ernResources <- mapM catalogResourceToErn resources

  -- Build releases
  ernReleases <- mapM (catalogReleaseToErn resources) releases

  -- Build resource groups (tracklists)
  let resourceGroups = buildResourceGroups releases resources

  -- Build deals
  ernDeals <- mapM catalogDealToErn deals

  return ErnMessage
    { ernMessageHeader = header
    , ernPartyList = parties
    , ernResourceList = ernResources
    , ernReleaseList = ernReleases
    , ernResourceGroups = resourceGroups
    , ernDealList = ernDeals
    }

validateConvertConfig :: ConvertConfig -> Either [ConvertError] ()
validateConvertConfig ConvertConfig{..} =
  let required label value =
        [ConvertError (label <> " must be configured; generated placeholders are forbidden") Nothing | T.null (T.strip value)]
      errors = concat
        [ required "Sender DPID" ccSenderDpid
        , required "Recipient DPID" ccRecipientDpid
        , required "Message ID" ccMessageId
        , required "Partner name" ccPartnerName
        ]
  in if null errors then Right () else Left errors

-- | Build MessageHeader
buildMessageHeader :: ConvertConfig -> UTCTime -> MessageHeader
buildMessageHeader config now = MessageHeader
  { mhMessageId = ccMessageId config
  , mhMessageThreadId = Nothing
  , mhSenderPartyId = PartyIdDPID (ccSenderDpid config)
  , mhRecipientPartyId = PartyIdDPID (ccRecipientDpid config)
  , mhMessageCreatedDateTime = now
  , mhMessageAuditTrail = Nothing
  }

-- | Build parties from credits
buildParties :: [CatalogCredit] -> [Party]
buildParties credits =
  let partyIds = map catalogCreditPartyId credits
      uniquePartyIds = removeDuplicates partyIds
  in map buildParty uniquePartyIds
  where
    buildParty partyId = Party
      { partyPartyId = PartyIdProprietary "TDF" (T.pack $ show partyId)
      , partyPartyName = Just $ PartyName
          { pnFullName = "Party " <> T.pack (show partyId)
          , pnNamesToDisplay = ["Party " <> T.pack (show partyId)]
          }
      , partyIpiNumber = Nothing
      , partyIsniNumber = Nothing
      , partyDPID = Nothing
      }

    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- | Convert catalog resource to ERN resource
catalogResourceToErn :: CatalogResource -> Either [ConvertError] Resource
catalogResourceToErn CatalogResource{..} = Right Resource
  { resourceReference = ResourceReference $ "A" <> T.pack (show catalogResourceId)
  , resourceType = catalogResourceTypeToErn catalogResourceType
  , resourceIds = []  -- TODO: Get from catalog_identifier
  , resourceTitle = catalogResourceTitle
  , resourceSubTitle = catalogResourceVersion
  , resourceContributors = []  -- TODO: Get from catalog_credit
  , resourceDuration = fmap msToDuration catalogResourceDurationMs
  , resourceLanguage = fmap Language catalogResourceLanguage
  , resourceExplicitContent = catalogResourceExplicitContent
  , resourceParentResourceRef = Nothing
  }

-- | Convert catalog resource type to ERN resource type
catalogResourceTypeToErn :: TDF.Catalog.Types.ResourceType -> TDF.DDEX.ERN.V432.Types.ResourceType
catalogResourceTypeToErn TDF.Catalog.Types.SoundRecording = TDF.DDEX.ERN.V432.Types.ResourceTypeSoundRecording
catalogResourceTypeToErn TDF.Catalog.Types.MusicVideo = TDF.DDEX.ERN.V432.Types.ResourceTypeMusicVideo
catalogResourceTypeToErn TDF.Catalog.Types.Image = TDF.DDEX.ERN.V432.Types.ResourceTypeImage
catalogResourceTypeToErn TDF.Catalog.Types.Text = TDF.DDEX.ERN.V432.Types.ResourceTypeText
catalogResourceTypeToErn TDF.Catalog.Types.Software = TDF.DDEX.ERN.V432.Types.ResourceTypeSoftware

-- | Convert milliseconds to Duration
msToDuration :: Int -> Duration
msToDuration ms =
  let totalSeconds = ms `div` 1000
      hours = totalSeconds `div` 3600
      minutes = (totalSeconds `mod` 3600) `div` 60
      seconds = totalSeconds `mod` 60
  in Duration hours minutes seconds

-- | Convert catalog release to ERN release
catalogReleaseToErn :: [CatalogResource] -> CatalogRelease -> Either [ConvertError] Release
catalogReleaseToErn _resources CatalogRelease{..} = Right Release
  { releaseReference = ReleaseReference $ "R" <> T.pack (show catalogReleaseId)
  , releaseType = catalogReleaseTypeToErn catalogReleaseType
  , releaseIds = []  -- TODO: Get from catalog_identifier
  , releaseTitle = catalogReleaseTitle
  , releaseSubTitle = catalogReleaseSubTitle
  , releaseContributors = []  -- TODO: Get from catalog_credit
  , releaseResourceRefs = []  -- TODO: Get from catalog_release_resource
  , releaseDuration = Nothing  -- TODO: Calculate from resources
  , releaseDate = fmap utcToDate catalogReleaseDate
  , releaseCopyrightLine = catalogReleaseCopyrightLine
  , releasePhonographicCopyrightLine = catalogReleasePhonographicCopyrightLine
  , releaseGenre = catalogReleaseGenre
  }

-- | Convert catalog release type to ERN release type
catalogReleaseTypeToErn :: TDF.Catalog.Types.ReleaseType -> TDF.DDEX.ERN.V432.Types.ReleaseType
catalogReleaseTypeToErn TDF.Catalog.Types.Album = TDF.DDEX.ERN.V432.Types.ReleaseTypeAlbum
catalogReleaseTypeToErn TDF.Catalog.Types.Single = TDF.DDEX.ERN.V432.Types.ReleaseTypeSingle
catalogReleaseTypeToErn TDF.Catalog.Types.EP = TDF.DDEX.ERN.V432.Types.ReleaseTypeEP
catalogReleaseTypeToErn TDF.Catalog.Types.Compilation = TDF.DDEX.ERN.V432.Types.ReleaseTypeCompilation
catalogReleaseTypeToErn TDF.Catalog.Types.LiveAlbum = TDF.DDEX.ERN.V432.Types.ReleaseTypeLiveAlbum
catalogReleaseTypeToErn TDF.Catalog.Types.RemixAlbum = TDF.DDEX.ERN.V432.Types.ReleaseTypeRemixAlbum
catalogReleaseTypeToErn TDF.Catalog.Types.Soundtrack = TDF.DDEX.ERN.V432.Types.ReleaseTypeSoundtrack
catalogReleaseTypeToErn TDF.Catalog.Types.SpokenWord = TDF.DDEX.ERN.V432.Types.ReleaseTypeSpokenWord

-- | Build resource groups from releases and resources
buildResourceGroups :: [CatalogRelease] -> [CatalogResource] -> [ResourceGroup]
buildResourceGroups releases _resources =
  map buildReleaseResourceGroup releases
  where
    buildReleaseResourceGroup CatalogRelease{..} = ResourceGroup
      { rgSequenceNumber = Nothing
      , rgTitle = Just catalogReleaseTitle
      , rgContent = []  -- TODO: Get from catalog_release_resource
      , rgSubGroups = []
      }

-- | Convert catalog deal to ERN deal
catalogDealToErn :: CatalogDeal -> Either [ConvertError] Deal
catalogDealToErn CatalogDeal{..} = Right Deal
  { dealDealTerms = DealTerms
      { dtTerritoryCodes = [TerritoryWorldwide]  -- TODO: Get from catalog_deal_territory
      , dtUsageType = "OnDemandStream"
      , dtPriceType = Nothing
      , dtWholesalePrice = Nothing
      , dtRetailPrice = Nothing
      , dtStartDate = utcToDate catalogDealStartDate
      , dtEndDate = fmap utcToDate catalogDealEndDate
      , dtTakedownDate = fmap utcToDate catalogDealTakedownDate
      }
  , dealReleaseRefs = maybe [] (\rid -> [ReleaseReference $ "R" <> T.pack (show rid)]) catalogDealReleaseId
  , dealResourceRefs = maybe [] (\rid -> [ResourceReference $ "A" <> T.pack (show rid)]) catalogDealResourceId
  , dealEffectiveDate = utcToDate catalogDealStartDate
  }

-- | Convert UTCTime to Date (extracts year/month/day)
utcToDate :: UTCTime -> Date
utcToDate utc = DateFull (utctDay utc)
