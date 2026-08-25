{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.ServiceStorefrontTypes where

import           Data.Aeson   (FromJSON(..), ToJSON(..), genericParseJSON)
import           Data.Int     (Int64)
import           Data.Text    (Text)
import           Data.Time    (UTCTime, Day)
import           GHC.Generics (Generic)

import           TDF.API.Types (strictObjectOptions)

-- | A storefront package (pricing tier for a service).
data ServiceStorefrontPackageDTO = ServiceStorefrontPackageDTO
  { sspId              :: Text
  , sspServiceKind     :: Text       -- ^ "Mixing", "Mastering", "Bundle"
  , sspTier            :: Text       -- ^ "Basic", "Pro", "Premium"
  , sspName            :: Text
  , sspDescription     :: Maybe Text
  , sspPriceUsdCents   :: Int
  , sspCurrency        :: Text
  , sspMinSongCount    :: Int
  , sspMaxSongCount    :: Int
  , sspTurnaroundDays  :: Int
  , sspRevisionCount   :: Int
  , sspDeliverables    :: Maybe [Text]
  , sspFeatures        :: Maybe [Text]
  , sspActive          :: Bool
  , sspSortOrder       :: Int
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontPackageDTO
instance FromJSON ServiceStorefrontPackageDTO

-- | Create a new storefront package (admin).
data ServiceStorefrontPackageCreate = ServiceStorefrontPackageCreate
  { sspcServiceKind     :: Text
  , sspcTier            :: Text
  , sspcName            :: Text
  , sspcDescription     :: Maybe Text
  , sspcPriceUsdCents   :: Int
  , sspcCurrency        :: Maybe Text
  , sspcMinSongCount    :: Maybe Int
  , sspcMaxSongCount    :: Maybe Int
  , sspcTurnaroundDays  :: Maybe Int
  , sspcRevisionCount   :: Maybe Int
  , sspcDeliverables    :: Maybe [Text]
  , sspcFeatures        :: Maybe [Text]
  , sspcSortOrder       :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontPackageCreate
instance FromJSON ServiceStorefrontPackageCreate where
  parseJSON = genericParseJSON strictObjectOptions

-- | Update a storefront package (admin).
data ServiceStorefrontPackageUpdate = ServiceStorefrontPackageUpdate
  { sspuName            :: Maybe Text
  , sspuDescription     :: Maybe Text
  , sspuPriceUsdCents   :: Maybe Int
  , sspuMinSongCount    :: Maybe Int
  , sspuMaxSongCount    :: Maybe Int
  , sspuTurnaroundDays  :: Maybe Int
  , sspuRevisionCount   :: Maybe Int
  , sspuDeliverables    :: Maybe [Text]
  , sspuFeatures        :: Maybe [Text]
  , sspuActive          :: Maybe Bool
  , sspuSortOrder       :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontPackageUpdate
instance FromJSON ServiceStorefrontPackageUpdate where
  parseJSON = genericParseJSON strictObjectOptions

-- | Create a new service order (public).
data ServiceStorefrontOrderCreate = ServiceStorefrontOrderCreate
  { ssocPackageId      :: Text
  , ssocBuyerName      :: Text
  , ssocBuyerEmail     :: Text
  , ssocBuyerPhone     :: Maybe Text
  , ssocArtistName     :: Maybe Text
  , ssocGenre          :: Maybe Text
  , ssocSongCount      :: Maybe Int
  , ssocNotes          :: Maybe Text
  , ssocReferenceTrackUrl :: Maybe Text
  , ssocDeadline       :: Maybe Text  -- ^ ISO date string
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontOrderCreate
instance FromJSON ServiceStorefrontOrderCreate where
  parseJSON = genericParseJSON strictObjectOptions

-- | A service order (public view).
data ServiceStorefrontOrderDTO = ServiceStorefrontOrderDTO
  { ssoId                  :: Text
  , ssoOrderNumber         :: Text
  , ssoBuyerName           :: Text
  , ssoBuyerEmail          :: Text
  , ssoBuyerPhone          :: Maybe Text
  , ssoArtistName          :: Maybe Text
  , ssoPackageId           :: Text
  , ssoServiceKind         :: Text
  , ssoTier                :: Text
  , ssoPriceUsdCents       :: Int
  , ssoCurrency            :: Text
  , ssoStatus              :: Text
  , ssoPaymentProvider     :: Maybe Text
  , ssoLookupToken         :: Maybe Text
  , ssoPaidAt              :: Maybe UTCTime
  , ssoGenre               :: Maybe Text
  , ssoSongCount           :: Int
  , ssoNotes               :: Maybe Text
  , ssoReferenceTrackUrl   :: Maybe Text
  , ssoDeadline            :: Maybe Day
  , ssoDeliverablesUrl     :: Maybe Text
  , ssoCreatedAt           :: UTCTime
  , ssoUpdatedAt           :: UTCTime
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontOrderDTO
instance FromJSON ServiceStorefrontOrderDTO

-- | Update a service order (admin).
data ServiceStorefrontOrderUpdate = ServiceStorefrontOrderUpdate
  { ssouStatus           :: Maybe Text
  , ssouDeliverablesUrl  :: Maybe Text
  , ssouNotes            :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontOrderUpdate
instance FromJSON ServiceStorefrontOrderUpdate where
  parseJSON = genericParseJSON strictObjectOptions

-- | Create a revision request (public).
data ServiceStorefrontRevisionCreate = ServiceStorefrontRevisionCreate
  { ssrcFeedback :: Text
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontRevisionCreate
instance FromJSON ServiceStorefrontRevisionCreate where
  parseJSON = genericParseJSON strictObjectOptions

-- | Select an offline payment rail. Selection is not payment confirmation.
data ServiceStorefrontManualPaymentCreate = ServiceStorefrontManualPaymentCreate
  { ssmPaymentMethod :: Text
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontManualPaymentCreate
instance FromJSON ServiceStorefrontManualPaymentCreate where
  parseJSON = genericParseJSON strictObjectOptions

data ServiceStorefrontPaypalCaptureReq = ServiceStorefrontPaypalCaptureReq
  { pcCaptureOrderId  :: Text
  , pcCapturePaypalId :: Text
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontPaypalCaptureReq
instance FromJSON ServiceStorefrontPaypalCaptureReq where
  parseJSON = genericParseJSON strictObjectOptions

-- | Request a full or partial refund. When omitted, the amount is the remaining
-- unreserved captured balance calculated by the server.
data ServiceStorefrontRefundCreate = ServiceStorefrontRefundCreate
  { ssrfcAmountUsdCents :: Maybe Int
  , ssrfcReasonCode     :: Text
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontRefundCreate
instance FromJSON ServiceStorefrontRefundCreate where
  parseJSON = genericParseJSON strictObjectOptions

data ServiceStorefrontRefundDTO = ServiceStorefrontRefundDTO
  { ssrfId               :: Text
  , ssrfOrderId          :: Text
  , ssrfProvider         :: Text
  , ssrfProviderRefundId :: Maybe Text
  , ssrfStatus           :: Text
  , ssrfAmountUsdCents   :: Int
  , ssrfCurrency         :: Text
  , ssrfReasonCode       :: Text
  , ssrfRequestedBy      :: Int64
  , ssrfApprovedBy       :: Maybe Int64
  , ssrfCreatedAt        :: UTCTime
  , ssrfCompletedAt      :: Maybe UTCTime
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontRefundDTO
instance FromJSON ServiceStorefrontRefundDTO

data ServiceStorefrontReconciliationDTO = ServiceStorefrontReconciliationDTO
  { ssrecOrderId           :: Text
  , ssrecProvider          :: Text
  , ssrecProviderReference :: Text
  , ssrecExpectedAmount    :: Int
  , ssrecActualAmount      :: Maybe Int
  , ssrecCurrency          :: Text
  , ssrecMatched           :: Bool
  , ssrecCheckedAt         :: UTCTime
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontReconciliationDTO
instance FromJSON ServiceStorefrontReconciliationDTO

-- | A revision request (public view).
data ServiceStorefrontRevisionDTO = ServiceStorefrontRevisionDTO
  { ssrId             :: Text
  , ssrOrderId        :: Text
  , ssrRevisionNumber :: Int
  , ssrFeedback       :: Text
  , ssrStatus         :: Text
  , ssrCreatedAt      :: UTCTime
  , ssrCompletedAt    :: Maybe UTCTime
  } deriving (Show, Generic)

instance ToJSON ServiceStorefrontRevisionDTO
instance FromJSON ServiceStorefrontRevisionDTO
