{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.ERN.V432.Normalize
  ( -- * Normalization
    normalizeErnMessage
  , CanonicalImport(..)
  , CanonicalRelease(..)
  , CanonicalResource(..)
  , CanonicalParty(..)
  , CanonicalCredit(..)
  , CanonicalDeal(..)
  , NormalizationError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Maybe (mapMaybe, catMaybes)
import TDF.DDEX.ERN.V432.Types
import qualified TDF.Catalog.Types as Catalog

-- | Normalized import ready for catalog insertion
data CanonicalImport = CanonicalImport
  { ciReleases    :: [CanonicalRelease]
  , ciResources   :: [CanonicalResource]
  , ciParties     :: [CanonicalParty]
  , ciCredits     :: [CanonicalCredit]
  , ciDeals       :: [CanonicalDeal]
  , ciSourceDocumentId :: Int
  } deriving (Show, Eq)

-- | Normalized release for catalog
data CanonicalRelease = CanonicalRelease
  { crTitle           :: Text
  , crSubTitle        :: Maybe Text
  , crReleaseType     :: Catalog.ReleaseType
  , crReleaseDate     :: Maybe UTCTime
  , crOriginalDate    :: Maybe UTCTime
  , crLabel           :: Maybe Text
  , crCopyrightLine   :: Maybe Text
  , crPhonographicCopyrightLine :: Maybe Text
  , crGenre           :: Maybe Text
  , crUpc             :: Maybe Text
  , crCatalogNumber   :: Maybe Text
  , crResourceRefs    :: [Text]
  , crSourcePartyRef  :: Text
  } deriving (Show, Eq)

-- | Normalized resource for catalog
data CanonicalResource = CanonicalResource
  { cresTitle         :: Text
  , cresSubTitle      :: Maybe Text
  , cresResourceType  :: Catalog.ResourceType
  , cresDurationMs    :: Maybe Int
  , cresLanguage      :: Maybe Text
  , cresExplicitContent :: Bool
  , cresIsrc          :: Maybe Text
  , cresGRid          :: Maybe Text
  , cresSourcePartyRef :: Text
  } deriving (Show, Eq)

-- | Normalized party for catalog
data CanonicalParty = CanonicalParty
  { cpName            :: Text
  , cpDPID            :: Maybe Text
  , cpIPI             :: Maybe Text
  , cpISNI            :: Maybe Text
  , cpSourcePartyRef  :: Text
  } deriving (Show, Eq)

-- | Normalized credit linking party to resource/release
data CanonicalCredit = CanonicalCredit
  { ccredEntityRef    :: Text
  , ccredEntityType   :: Text
  , ccredPartyRef     :: Text
  , ccredRole         :: Catalog.CreditRole
  , ccredText         :: Maybe Text
  } deriving (Show, Eq)

-- | Normalized deal for catalog
data CanonicalDeal = CanonicalDeal
  { cdealReleaseRef   :: Maybe Text
  , cdealResourceRef  :: Maybe Text
  , cdealModel        :: Catalog.DealModel
  , cdealTerritories  :: [Text]
  , cdealStartDate    :: UTCTime
  , cdealEndDate      :: Maybe UTCTime
  , cdealPartnerName  :: Text
  } deriving (Show, Eq)

-- | Normalization error
data NormalizationError = NormalizationError
  { neMessage :: Text
  , neElement :: Maybe Text
  } deriving (Show, Eq)

-- | Normalize ERN message to canonical import
normalizeErnMessage :: Int -> ErnMessage -> Either [NormalizationError] CanonicalImport
normalizeErnMessage docId ern =
  let parties = normalizeParties (ernPartyList ern)
      resources = normalizeResources (ernResourceList ern)
      releases = normalizeReleases (ernReleaseList ern)
      credits = normalizeCredits (ernResourceList ern)
      deals = normalizeDeals (ernDealList ern)
      errors = []
  in if null errors
     then Right CanonicalImport
       { ciReleases = releases
       , ciResources = resources
       , ciParties = parties
       , ciCredits = credits
       , ciDeals = deals
       , ciSourceDocumentId = docId
       }
     else Left errors

-- | Normalize parties from ERN to canonical
normalizeParties :: [Party] -> [CanonicalParty]
normalizeParties = map normalizeParty
  where
    normalizeParty Party{..} = CanonicalParty
      { cpName = maybe "Unknown" pnFullName partyPartyName
      , cpDPID = case partyPartyId of
          PartyIdDPID d -> Just d
          _ -> partyDPID
      , cpIPI = partyIpiNumber
      , cpISNI = partyIsniNumber
      , cpSourcePartyRef = partyIdToText partyPartyId
      }

-- | Convert PartyId to text reference
partyIdToText :: PartyId -> Text
partyIdToText (PartyIdDPID d) = "DPID:" <> d
partyIdToText (PartyIdIPI i) = "IPI:" <> i
partyIdToText (PartyIdISNI i) = "ISNI:" <> i
partyIdToText (PartyIdProprietary ns v) = ns <> ":" <> v

-- | Normalize resources from ERN to canonical
normalizeResources :: [Resource] -> [CanonicalResource]
normalizeResources = map normalizeResource
  where
    normalizeResource Resource{..} = CanonicalResource
      { cresTitle = resourceTitle
      , cresSubTitle = resourceSubTitle
      , cresResourceType = convertResourceType resourceType
      , cresDurationMs = fmap durationToMs resourceDuration
      , cresLanguage = fmap (\(Language l) -> l) resourceLanguage
      , cresExplicitContent = resourceExplicitContent
      , cresIsrc = extractIsrc resourceIds
      , cresGRid = extractGRid resourceIds
      , cresSourcePartyRef = unResourceRef resourceReference
      }

-- | Convert ERN ResourceType to Catalog ResourceType
convertResourceType :: TDF.DDEX.ERN.V432.Types.ResourceType -> Catalog.ResourceType
convertResourceType ResourceTypeSoundRecording = Catalog.SoundRecording
convertResourceType ResourceTypeMusicVideo = Catalog.MusicVideo
convertResourceType ResourceTypeImage = Catalog.Image
convertResourceType ResourceTypeText = Catalog.Text
convertResourceType ResourceTypeSoftware = Catalog.Software

-- | Extract ISRC from resource IDs
extractIsrc :: [ResourceId] -> Maybe Text
extractIsrc [] = Nothing
extractIsrc (ResourceIdISRC isrc:_) = Just (formatISRC isrc)
extractIsrc (_:rest) = extractIsrc rest

-- | Format ISRC as full string
formatISRC :: ISRC -> Text
formatISRC ISRC{..} = isrcCountryCode <> isrcRegistrant <> isrcYear <> isrcDesignation

-- | Extract GRid from resource IDs
extractGRid :: [ResourceId] -> Maybe Text
extractGRid [] = Nothing
extractGRid (ResourceIdGRid (GRid g):_) = Just g
extractGRid (_:rest) = extractGRid rest

-- | Convert Duration to milliseconds
durationToMs :: Duration -> Int
durationToMs Duration{..} =
  (durationHours * 3600 + durationMinutes * 60 + durationSeconds) * 1000

-- | Unwrap ResourceReference
unResourceRef :: ResourceReference -> Text
unResourceRef (ResourceReference r) = r

-- | Normalize releases from ERN to canonical
normalizeReleases :: [Release] -> [CanonicalRelease]
normalizeReleases = map normalizeRelease
  where
    normalizeRelease Release{..} = CanonicalRelease
      { crTitle = releaseTitle
      , crSubTitle = releaseSubTitle
      , crReleaseType = convertReleaseType releaseType
      , crReleaseDate = fmap dateToUTCTime releaseDate
      , crOriginalDate = Nothing
      , crLabel = Nothing  -- Would need to look up from PartyList
      , crCopyrightLine = releaseCopyrightLine
      , crPhonographicCopyrightLine = releasePhonographicCopyrightLine
      , crGenre = releaseGenre
      , crUpc = extractUpc releaseIds
      , crCatalogNumber = extractCatalogNumber releaseIds
      , crResourceRefs = map unResourceRef releaseResourceRefs
      , crSourcePartyRef = unReleaseRef releaseReference
      }

-- | Convert ERN ReleaseType to Catalog ReleaseType
convertReleaseType :: TDF.DDEX.ERN.V432.Types.ReleaseType -> Catalog.ReleaseType
convertReleaseType ReleaseTypeAlbum = Catalog.Album
convertReleaseType ReleaseTypeSingle = Catalog.Single
convertReleaseType ReleaseTypeEP = Catalog.EP
convertReleaseType ReleaseTypeCompilation = Catalog.Compilation
convertReleaseType ReleaseTypeLiveAlbum = Catalog.LiveAlbum
convertReleaseType ReleaseTypeRemixAlbum = Catalog.RemixAlbum
convertReleaseType ReleaseTypeSoundtrack = Catalog.Soundtrack
convertReleaseType ReleaseTypeSpokenWord = Catalog.SpokenWord

-- | Extract UPC from release IDs
extractUpc :: [ReleaseId] -> Maybe Text
extractUpc [] = Nothing
extractUpc (ReleaseIdUPC u:_) = Just u
extractUpc (_:rest) = extractUpc rest

-- | Extract catalog number from release IDs
extractCatalogNumber :: [ReleaseId] -> Maybe Text
extractCatalogNumber [] = Nothing
extractCatalogNumber (ReleaseIdCatalogNumber _ cn:_) = Just cn
extractCatalogNumber (_:rest) = extractCatalogNumber rest

-- | Unwrap ReleaseReference
unReleaseRef :: ReleaseReference -> Text
unReleaseRef (ReleaseReference r) = r

-- | Convert Date to UTCTime (simplified - assumes midnight UTC)
dateToUTCTime :: Date -> UTCTime
dateToUTCTime (DateYear y) = read $ show y ++ "-01-01 00:00:00 UTC"
dateToUTCTime (DateYearMonth y m) = read $ show y ++ "-" ++ padMonth m ++ "-01 00:00:00 UTC"
  where padMonth n = if n < 10 then "0" ++ show n else show n
dateToUTCTime (DateFull d) = read $ show d ++ " 00:00:00 UTC"

-- | Normalize credits from ERN resources
normalizeCredits :: [Resource] -> [CanonicalCredit]
normalizeCredits resources = concatMap normalizeResourceCredits resources
  where
    normalizeResourceCredits Resource{..} =
      map (normalizeContributor (unResourceRef resourceReference)) resourceContributors

    normalizeContributor resourceRef ResourceContributor{..} = CanonicalCredit
      { ccredEntityRef = resourceRef
      , ccredEntityType = "Resource"
      , ccredPartyRef = rcPartyReference
      , ccredRole = convertRole rcRole
      , ccredText = rcCreditText
      }

-- | Convert role text to CreditRole
convertRole :: Text -> Catalog.CreditRole
convertRole "MainArtist" = Catalog.MainArtist
convertRole "FeaturedArtist" = Catalog.FeaturedArtist
convertRole "Producer" = Catalog.Producer
convertRole "Engineer" = Catalog.Engineer
convertRole "Mixer" = Catalog.Mixer
convertRole "MasteringEngineer" = Catalog.MasteringEngineer
convertRole "Composer" = Catalog.Composer
convertRole "Lyricist" = Catalog.Lyricist
convertRole "Arranger" = Catalog.Arranger
convertRole "Performer" = Catalog.Performer
convertRole "StudioMusician" = Catalog.StudioMusician
convertRole _ = Catalog.Performer  -- Default fallback

-- | Normalize deals from ERN to canonical
normalizeDeals :: [Deal] -> [CanonicalDeal]
normalizeDeals = map normalizeDeal
  where
    normalizeDeal Deal{..} = CanonicalDeal
      { cdealReleaseRef = case dealReleaseRefs of
          (r:_) -> Just (unReleaseRef r)
          [] -> Nothing
      , cdealResourceRef = case dealResourceRefs of
          (r:_) -> Just (unResourceRef r)
          [] -> Nothing
      , cdealModel = Catalog.DistributionAgreement  -- Default model
      , cdealTerritories = map territoryToText (dtTerritoryCodes dealDealTerms)
      , cdealStartDate = dateToUTCTime (dtStartDate dealDealTerms)
      , cdealEndDate = fmap dateToUTCTime (dtEndDate dealDealTerms)
      , cdealPartnerName = "Unknown Partner"  -- Would need to look up from MessageHeader
      }

-- | Convert TerritoryCode to text
territoryToText :: TerritoryCode -> Text
territoryToText TerritoryWorldwide = "Worldwide"
territoryToText (TerritoryCode c) = c
