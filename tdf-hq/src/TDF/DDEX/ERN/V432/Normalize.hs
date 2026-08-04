{-# LANGUAGE OverloadedStrings #-}

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
import Data.Time (UTCTime)
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
-- TODO: Implement full normalization logic
normalizeErnMessage :: Int -> ErnMessage -> Either [NormalizationError] CanonicalImport
normalizeErnMessage docId _ern =
  Right CanonicalImport
    { ciReleases = []
    , ciResources = []
    , ciParties = []
    , ciCredits = []
    , ciDeals = []
    , ciSourceDocumentId = docId
    }
