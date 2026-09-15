{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.CommerceOperations
  ( CommerceOperationsAPI
  , CommercePaymentOverviewDTO(..)
  , CommerceProviderAccountDTO(..)
  , CommerceProviderCapabilityDTO(..)
  , CommercePaymentIntentSummaryDTO(..)
  , CommerceAmountComponentSummaryDTO(..)
  , CommerceCommissionSummaryDTO(..)
  , CommerceRefundSummaryDTO(..)
  , CommerceDisputeSummaryDTO(..)
  , CommerceReconciliationSummaryDTO(..)
  , CommerceSettlementSummaryDTO(..)
  , CommerceSellerBalanceSummaryDTO(..)
  , CommercePayoutSummaryDTO(..)
  , CommerceProviderEventDTO(..)
  , CommerceProviderEventReplayCreate(..)
  ) where

import           Data.Aeson (FromJSON(..), ToJSON, genericParseJSON)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Servant

import           TDF.API.Types (strictObjectOptions)

-- | Redacted provider readiness. Merchant account references, feature-flag
-- keys, secrets and capability source payloads are intentionally absent.
data CommerceProviderAccountDTO = CommerceProviderAccountDTO
  { cpaProvider         :: Text
  , cpaEnvironment      :: Text
  , cpaStatus           :: Text
  , cpaContractStatus   :: Text
  , cpaCredentialStatus :: Text
  , cpaSettlementCurrency :: Text
  , cpaEnabled          :: Bool
  , cpaFeatureEnabled   :: Bool
  , cpaVerifiedAt       :: Maybe UTCTime
  , cpaDisabledReason   :: Maybe Text
  , cpaCapabilities     :: [CommerceProviderCapabilityDTO]
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceProviderAccountDTO
instance FromJSON CommerceProviderAccountDTO

data CommerceProviderCapabilityDTO = CommerceProviderCapabilityDTO
  { cpcPaymentMethod      :: Text
  , cpcCapability         :: Text
  , cpcVerificationStatus :: Text
  , cpcVerifiedAt         :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceProviderCapabilityDTO
instance FromJSON CommerceProviderCapabilityDTO

data CommercePaymentIntentSummaryDTO = CommercePaymentIntentSummaryDTO
  { cpiStatus          :: Text
  , cpiCurrency        :: Text
  , cpiCount           :: Int64
  , cpiAmountMinor     :: Int64
  , cpiAuthorizedMinor :: Int64
  , cpiCapturedMinor   :: Int64
  , cpiRefundedMinor   :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON CommercePaymentIntentSummaryDTO
instance FromJSON CommercePaymentIntentSummaryDTO

data CommerceAmountComponentSummaryDTO = CommerceAmountComponentSummaryDTO
  { cacComponentType :: Text
  , cacSource        :: Text
  , cacCurrency      :: Text
  , cacCount         :: Int64
  , cacAmountMinor   :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceAmountComponentSummaryDTO
instance FromJSON CommerceAmountComponentSummaryDTO

data CommerceCommissionSummaryDTO = CommerceCommissionSummaryDTO
  { ccmProvider         :: Text
  , ccmEnvironment      :: Text
  , ccmCurrency         :: Text
  , ccmCount            :: Int64
  , ccmBasisAmountMinor :: Int64
  , ccmCommissionMinor  :: Int64
  , ccmProviderFeeMinor :: Int64
  , ccmTaxMinor         :: Int64
  , ccmSellerNetMinor   :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceCommissionSummaryDTO
instance FromJSON CommerceCommissionSummaryDTO

data CommerceRefundSummaryDTO = CommerceRefundSummaryDTO
  { crfProvider    :: Text
  , crfEnvironment :: Text
  , crfStatus      :: Text
  , crfCurrency    :: Text
  , crfCount       :: Int64
  , crfAmountMinor :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceRefundSummaryDTO
instance FromJSON CommerceRefundSummaryDTO

data CommerceDisputeSummaryDTO = CommerceDisputeSummaryDTO
  { cdsProvider    :: Text
  , cdsEnvironment :: Text
  , cdsKind        :: Text
  , cdsStatus      :: Text
  , cdsCurrency    :: Text
  , cdsCount       :: Int64
  , cdsAmountMinor :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceDisputeSummaryDTO
instance FromJSON CommerceDisputeSummaryDTO

data CommerceReconciliationSummaryDTO = CommerceReconciliationSummaryDTO
  { crsProvider      :: Text
  , crsEnvironment   :: Text
  , crsStatus        :: Text
  , crsCurrency      :: Maybe Text
  , crsCount         :: Int64
  , crsExpectedMinor :: Int64
  , crsActualMinor   :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceReconciliationSummaryDTO
instance FromJSON CommerceReconciliationSummaryDTO

data CommerceSettlementSummaryDTO = CommerceSettlementSummaryDTO
  { cssProvider         :: Text
  , cssEnvironment      :: Text
  , cssStatus           :: Text
  , cssCurrency         :: Text
  , cssCount            :: Int64
  , cssGrossMinor       :: Int64
  , cssFeeMinor         :: Int64
  , cssWithholdingMinor :: Int64
  , cssRefundMinor      :: Int64
  , cssChargebackMinor  :: Int64
  , cssNetMinor         :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceSettlementSummaryDTO
instance FromJSON CommerceSettlementSummaryDTO

data CommerceSellerBalanceSummaryDTO = CommerceSellerBalanceSummaryDTO
  { csbProvider       :: Text
  , csbEnvironment    :: Text
  , csbAvailability   :: Text
  , csbCurrency       :: Text
  , csbEntryCount     :: Int64
  , csbNetAmountMinor :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceSellerBalanceSummaryDTO
instance FromJSON CommerceSellerBalanceSummaryDTO

data CommercePayoutSummaryDTO = CommercePayoutSummaryDTO
  { cpsProvider    :: Text
  , cpsEnvironment :: Text
  , cpsStatus      :: Text
  , cpsCurrency    :: Text
  , cpsCount       :: Int64
  , cpsAmountMinor :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON CommercePayoutSummaryDTO
instance FromJSON CommercePayoutSummaryDTO

data CommercePaymentOverviewDTO = CommercePaymentOverviewDTO
  { cpoGeneratedAt              :: UTCTime
  , cpoProviderAccounts         :: [CommerceProviderAccountDTO]
  , cpoPaymentIntents           :: [CommercePaymentIntentSummaryDTO]
  , cpoAmountComponents         :: [CommerceAmountComponentSummaryDTO]
  , cpoCommissions              :: [CommerceCommissionSummaryDTO]
  , cpoRefunds                  :: [CommerceRefundSummaryDTO]
  , cpoDisputes                 :: [CommerceDisputeSummaryDTO]
  , cpoReconciliationExceptions :: [CommerceReconciliationSummaryDTO]
  , cpoSettlements              :: [CommerceSettlementSummaryDTO]
  , cpoSellerBalances           :: [CommerceSellerBalanceSummaryDTO]
  , cpoPayouts                  :: [CommercePayoutSummaryDTO]
  } deriving (Eq, Show, Generic)

instance ToJSON CommercePaymentOverviewDTO
instance FromJSON CommercePaymentOverviewDTO

-- | Sensitive fields are intentionally excluded from this operator DTO. In
-- particular, the encrypted provider payload and merchant account reference
-- never leave the backend.
data CommerceProviderEventDTO = CommerceProviderEventDTO
  { cpeId                 :: Text
  , cpeProvider           :: Text
  , cpeEnvironment        :: Text
  , cpeProviderEventId    :: Text
  , cpeEventType          :: Text
  , cpeProviderResourceId :: Maybe Text
  , cpeStatus             :: Text
  , cpeAttemptCount       :: Int
  , cpeCheckoutId         :: Maybe Text
  , cpePaymentAttemptId   :: Maybe Text
  , cpeRefundId           :: Maybe Text
  , cpeReceivedAt         :: UTCTime
  , cpeProviderCreatedAt  :: Maybe UTCTime
  , cpeProcessingStartedAt :: Maybe UTCTime
  , cpeLastAttemptAt      :: Maybe UTCTime
  , cpeNextAttemptAt      :: Maybe UTCTime
  , cpeProcessedAt        :: Maybe UTCTime
  , cpeErrorSummary       :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceProviderEventDTO
instance FromJSON CommerceProviderEventDTO

data CommerceProviderEventReplayCreate = CommerceProviderEventReplayCreate
  { cperReason :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceProviderEventReplayCreate
instance FromJSON CommerceProviderEventReplayCreate where
  parseJSON = genericParseJSON strictObjectOptions

type CommerceOperationsAPI =
       "admin" :> "commerce" :> "overview"
         :> Get '[JSON] CommercePaymentOverviewDTO
  :<|> "admin" :> "commerce" :> "provider-events"
         :> QueryParam "status" Text
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Get '[JSON] [CommerceProviderEventDTO]
  :<|> "admin" :> "commerce" :> "provider-events"
         :> Capture "eventId" Text
         :> "replay"
         :> ReqBody '[JSON] CommerceProviderEventReplayCreate
         :> Post '[JSON] CommerceProviderEventDTO
