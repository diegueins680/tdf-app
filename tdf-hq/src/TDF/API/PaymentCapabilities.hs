{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.PaymentCapabilities
  ( PaymentCapabilitiesAPI
  , PaymentRouteDTO(..)
  , PaymentCapabilityResponseDTO(..)
  ) where

import           Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import           Data.Int (Int64)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant

import           TDF.API.Types (prefixedStrictObjectOptions)

data PaymentRouteDTO = PaymentRouteDTO
  { prdProvider     :: Text
  , prdPaymentMethod :: Text
  , prdCapabilities :: [Text]
  , prdPriority     :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON PaymentRouteDTO where
  toJSON = genericToJSON (prefixedStrictObjectOptions 3)
instance FromJSON PaymentRouteDTO where
  parseJSON = genericParseJSON (prefixedStrictObjectOptions 3)

data PaymentCapabilityResponseDTO = PaymentCapabilityResponseDTO
  { pcrEnvironment    :: Text
  , pcrBuyerCountry   :: Text
  , pcrCurrency       :: Text
  , pcrAmountMinor    :: Int64
  , pcrPaymentMethod  :: Text
  , pcrProductFlow    :: Text
  , pcrRoutes         :: [PaymentRouteDTO]
  , pcrFallbackPolicy :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON PaymentCapabilityResponseDTO where
  toJSON = genericToJSON (prefixedStrictObjectOptions 3)
instance FromJSON PaymentCapabilityResponseDTO where
  parseJSON = genericParseJSON (prefixedStrictObjectOptions 3)

type PaymentCapabilitiesAPI =
  "commerce" :> "payment-capabilities"
    :> QueryParam' '[Required] "buyerCountry" Text
    :> QueryParam' '[Required] "currency" Text
    :> QueryParam' '[Required] "amountMinor" Int64
    :> QueryParam' '[Required] "paymentMethod" Text
    :> QueryParam' '[Required] "productFlow" Text
    :> QueryParams "requires" Text
    :> Get '[JSON] PaymentCapabilityResponseDTO
