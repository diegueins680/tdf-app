{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Routes.DomoQuotes
  ( PublicDomoStorefrontDTO(..)
  , PublicDomoQuoteCreateRequest(..)
  , PublicDomoQuoteAcceptRequest(..)
  , PublicDomoQuoteLineDTO(..)
  , PublicDomoQuoteDTO(..)
  , PublicDomoPaypalCaptureRequest(..)
  , PublicDomoQuotesAPI
  ) where

import           Data.Aeson
  ( FromJSON(..), Options(..), ToJSON, defaultOptions, genericParseJSON )
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Servant

import qualified TDF.API.Types as APITypes
import           TDF.API.Types (rejectNullOptionalFields)

data PublicDomoStorefrontDTO = PublicDomoStorefrontDTO
  { checkoutAvailable    :: Bool
  , unavailableReason    :: Maybe Text
  , rateCardVersion      :: Maybe Text
  , currency             :: Maybe Text
  , eventTypes           :: [Text]
  , maximumGuests        :: Maybe Int
  , maximumDurationHours :: Maybe Int
  , maximumSetupHours    :: Maybe Int
  , quoteHoldMinutes     :: Maybe Int
  , timezone             :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicDomoStorefrontDTO

data PublicDomoQuoteCreateRequest = PublicDomoQuoteCreateRequest
  { customerName  :: Text
  , customerEmail :: Text
  , customerPhone :: Maybe Text
  , eventType     :: Text
  , guests        :: Int
  , startsAt      :: UTCTime
  , durationHours :: Int
  , setupHours    :: Int
  , catering      :: Bool
  , production    :: Bool
  , transport     :: Bool
  , notes         :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicDomoQuoteCreateRequest
instance FromJSON PublicDomoQuoteCreateRequest where
  parseJSON value = do
    rejectNullOptionalFields
      "PublicDomoQuoteCreateRequest"
      ["customerPhone", "notes"]
      value
    genericParseJSON defaultOptions { rejectUnknownFields = True } value

data PublicDomoQuoteAcceptRequest = PublicDomoQuoteAcceptRequest
  { termsAccepted :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON PublicDomoQuoteAcceptRequest
instance FromJSON PublicDomoQuoteAcceptRequest where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

data PublicDomoQuoteLineDTO = PublicDomoQuoteLineDTO
  { code            :: Text
  , description     :: Text
  , quantity        :: Int
  , unitAmountMinor :: Int64
  , subtotalMinor   :: Int64
  } deriving (Eq, Show, Generic)

instance ToJSON PublicDomoQuoteLineDTO

data PublicDomoQuoteDTO = PublicDomoQuoteDTO
  { quoteId           :: Text
  , checkoutId        :: Text
  , lookupToken       :: Maybe Text
  , quoteStatus       :: Text
  , paymentStatus     :: Text
  , fulfillmentStatus :: Text
  , rateCardVersion   :: Text
  , currency          :: Text
  , eventType         :: Text
  , guests            :: Int
  , startsAt          :: UTCTime
  , endsAt            :: UTCTime
  , setupStartsAt     :: UTCTime
  , lines             :: [PublicDomoQuoteLineDTO]
  , subtotalMinor     :: Int64
  , taxMinor          :: Int64
  , totalMinor        :: Int64
  , depositMinor      :: Int64
  , balanceMinor      :: Int64
  , timezone          :: Text
  , termsVersion      :: Text
  , holdExpiresAt     :: UTCTime
  , termsAcceptedAt   :: Maybe UTCTime
  , depositPaidAt     :: Maybe UTCTime
  , paymentMethods    :: [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON PublicDomoQuoteDTO

data PublicDomoPaypalCaptureRequest = PublicDomoPaypalCaptureRequest
  { paypalOrderId :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicDomoPaypalCaptureRequest
instance FromJSON PublicDomoPaypalCaptureRequest where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

type PublicDomoQuotesAPI =
       "public" :> "domo" :> Get '[JSON] PublicDomoStorefrontDTO
  :<|> "public" :> "domo" :> "quotes"
         :> Header "Idempotency-Key" Text
         :> ReqBody '[JSON] PublicDomoQuoteCreateRequest
         :> PostCreated '[JSON] PublicDomoQuoteDTO
  :<|> "public" :> "domo" :> "quotes" :> Capture "quoteId" Text
         :> Header "X-Order-Lookup-Token" Text
         :> Get '[JSON] PublicDomoQuoteDTO
  :<|> "public" :> "domo" :> "quotes" :> Capture "quoteId" Text :> "accept"
         :> Header "X-Order-Lookup-Token" Text
         :> ReqBody '[JSON] PublicDomoQuoteAcceptRequest
         :> Post '[JSON] PublicDomoQuoteDTO
  :<|> "public" :> "domo" :> "quotes" :> Capture "quoteId" Text
         :> "datafast" :> "checkout"
         :> Header "X-Order-Lookup-Token" Text
         :> Post '[JSON] APITypes.DatafastCheckoutDTO
  :<|> "public" :> "domo" :> "quotes" :> Capture "quoteId" Text
         :> "datafast" :> "status"
         :> Header "X-Order-Lookup-Token" Text
         :> QueryParam' '[Required] "resourcePath" Text
         :> Get '[JSON] PublicDomoQuoteDTO
  :<|> "public" :> "domo" :> "quotes" :> Capture "quoteId" Text
         :> "paypal" :> "create"
         :> Header "X-Order-Lookup-Token" Text
         :> Post '[JSON] APITypes.PaypalCreateDTO
  :<|> "public" :> "domo" :> "quotes" :> Capture "quoteId" Text
         :> "paypal" :> "capture"
         :> Header "X-Order-Lookup-Token" Text
         :> ReqBody '[JSON] PublicDomoPaypalCaptureRequest
         :> Post '[JSON] PublicDomoQuoteDTO
