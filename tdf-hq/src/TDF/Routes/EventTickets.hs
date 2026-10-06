{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Routes.EventTickets
  ( PublicEventTicketTierDTO(..)
  , PublicEventTicketPolicyDTO(..)
  , PublicEventTicketStorefrontDTO(..)
  , PublicEventTicketCheckoutRequest(..)
  , PublicEventTicketQuoteDTO(..)
  , PublicEventTicketDTO(..)
  , PublicEventTicketCheckoutResponse(..)
  , PublicEventTicketPaypalCaptureRequest(..)
  , PublicEventTicketBankTransferDTO(..)
  , PublicEventTicketBankTransferEvidenceRequest(..)
  , PublicEventTicketsAPI
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

data PublicEventTicketTierDTO = PublicEventTicketTierDTO
  { tierId          :: Int64
  , code            :: Text
  , name            :: Text
  , description     :: Maybe Text
  , unitPriceMinor  :: Int64
  , currency        :: Text
  , remaining       :: Int
  , salesStart      :: Maybe UTCTime
  , salesEnd        :: Maybe UTCTime
  , transfersAllowed :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketTierDTO

data PublicEventTicketPolicyDTO = PublicEventTicketPolicyDTO
  { policyVersion     :: Text
  , currency          :: Text
  , buyerFeeBps       :: Int
  , organizerFeeBps   :: Int
  , taxIncluded       :: Bool
  , taxBps            :: Int
  , holdMinutes       :: Int
  , termsVersion      :: Text
  , termsSummary      :: Text
  , refundPolicy      :: Text
  , transferAllowed   :: Bool
  , maxTicketsPerOrder :: Int
  , bankTransferAvailableUntil :: Maybe UTCTime
  , taxInvoiceIssued  :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketPolicyDTO

data PublicEventTicketStorefrontDTO = PublicEventTicketStorefrontDTO
  { eventId           :: Int64
  , title             :: Text
  , description       :: Maybe Text
  , startsAt          :: UTCTime
  , endsAt            :: Maybe UTCTime
  , timezone          :: Maybe Text
  , venueName         :: Maybe Text
  , venueAddress      :: Maybe Text
  , imageUrl          :: Maybe Text
  , tiers             :: [PublicEventTicketTierDTO]
  , policy            :: Maybe PublicEventTicketPolicyDTO
  , checkoutAvailable :: Bool
  , unavailableReason :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketStorefrontDTO

data PublicEventTicketCheckoutRequest = PublicEventTicketCheckoutRequest
  { tierId        :: Int64
  , quantity      :: Int
  , buyerName     :: Text
  , buyerEmail    :: Text
  , buyerPhone    :: Maybe Text
  , promoCode     :: Maybe Text
  , termsAccepted :: Bool
  , billingIdType :: Maybe Text
  , billingIdNumber :: Maybe Text
  , billingName   :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketCheckoutRequest
instance FromJSON PublicEventTicketCheckoutRequest where
  parseJSON value = do
    rejectNullOptionalFields
      "PublicEventTicketCheckoutRequest"
      ["buyerPhone", "promoCode", "billingIdType", "billingIdNumber", "billingName"]
      value
    genericParseJSON defaultOptions { rejectUnknownFields = True } value

data PublicEventTicketQuoteDTO = PublicEventTicketQuoteDTO
  { policyVersion          :: Text
  , currency               :: Text
  , quantity               :: Int
  , unitPriceMinor         :: Int64
  , grossFaceValueMinor    :: Int64
  , discountMinor          :: Int64
  , netFaceValueMinor      :: Int64
  , buyerPlatformFeeMinor  :: Int64
  , organizerPlatformFeeMinor :: Int64
  , taxIncluded            :: Bool
  , taxMinor               :: Int64
  , checkoutTotalMinor     :: Int64
  , organizerPayableMinor  :: Int64
  , platformFeeMinor       :: Int64
  , termsVersion           :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketQuoteDTO

data PublicEventTicketDTO = PublicEventTicketDTO
  { ticketId   :: Int64
  , ticketCode :: Text
  , status     :: Text
  , holderName :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketDTO

data PublicEventTicketCheckoutResponse = PublicEventTicketCheckoutResponse
  { orderId           :: Int64
  , eventId           :: Int64
  , checkoutId        :: Text
  , lookupToken       :: Maybe Text
  , paymentStatus     :: Text
  , fulfillmentStatus :: Text
  , holdExpiresAt     :: UTCTime
  , quote             :: PublicEventTicketQuoteDTO
  , paymentMethods    :: [Text]
  , tickets           :: [PublicEventTicketDTO]
  , bankTransfer      :: Maybe PublicEventTicketBankTransferDTO
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketCheckoutResponse

-- | Present only after the buyer selected bank transfer. The reference is the
-- public order number; account details come from server configuration.
data PublicEventTicketBankTransferDTO = PublicEventTicketBankTransferDTO
  { instructions      :: Text
  , paymentReference  :: Text
  , amountMinor       :: Int64
  , currency          :: Text
  , evidenceStatus    :: Text
  , customerReference :: Maybe Text
  , reviewNotes       :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketBankTransferDTO

data PublicEventTicketBankTransferEvidenceRequest = PublicEventTicketBankTransferEvidenceRequest
  { customerReference :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketBankTransferEvidenceRequest
instance FromJSON PublicEventTicketBankTransferEvidenceRequest where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

data PublicEventTicketPaypalCaptureRequest = PublicEventTicketPaypalCaptureRequest
  { paypalOrderId :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON PublicEventTicketPaypalCaptureRequest
instance FromJSON PublicEventTicketPaypalCaptureRequest where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

type PublicEventTicketsAPI =
       "public" :> "events" :> Capture "eventId" Int64 :> "tickets"
         :> Get '[JSON] PublicEventTicketStorefrontDTO
  :<|> "public" :> "events" :> Capture "eventId" Int64 :> "ticket-orders"
         :> Header "Idempotency-Key" Text
         :> ReqBody '[JSON] PublicEventTicketCheckoutRequest
         :> PostCreated '[JSON] PublicEventTicketCheckoutResponse
  :<|> "public" :> "events" :> Capture "eventId" Int64 :> "ticket-orders"
         :> Capture "orderId" Int64
         :> Header "X-Order-Lookup-Token" Text
         :> Get '[JSON] PublicEventTicketCheckoutResponse
  :<|> "public" :> "events" :> Capture "eventId" Int64 :> "ticket-orders"
         :> Capture "orderId" Int64 :> "datafast" :> "checkout"
         :> Header "X-Order-Lookup-Token" Text
         :> Post '[JSON] APITypes.DatafastCheckoutDTO
  :<|> "public" :> "events" :> Capture "eventId" Int64 :> "ticket-orders"
         :> Capture "orderId" Int64 :> "datafast" :> "status"
         :> Header "X-Order-Lookup-Token" Text
         :> QueryParam' '[Required] "resourcePath" Text
         :> Get '[JSON] PublicEventTicketCheckoutResponse
  :<|> "public" :> "events" :> Capture "eventId" Int64 :> "ticket-orders"
         :> Capture "orderId" Int64 :> "paypal" :> "create"
         :> Header "X-Order-Lookup-Token" Text
         :> Post '[JSON] APITypes.PaypalCreateDTO
  :<|> "public" :> "events" :> Capture "eventId" Int64 :> "ticket-orders"
         :> Capture "orderId" Int64 :> "paypal" :> "capture"
         :> Header "X-Order-Lookup-Token" Text
         :> ReqBody '[JSON] PublicEventTicketPaypalCaptureRequest
         :> Post '[JSON] PublicEventTicketCheckoutResponse
  :<|> "public" :> "events" :> Capture "eventId" Int64 :> "ticket-orders"
         :> Capture "orderId" Int64 :> "bank-transfer"
         :> Header "X-Order-Lookup-Token" Text
         :> Post '[JSON] PublicEventTicketCheckoutResponse
  :<|> "public" :> "events" :> Capture "eventId" Int64 :> "ticket-orders"
         :> Capture "orderId" Int64 :> "bank-transfer" :> "evidence"
         :> Header "X-Order-Lookup-Token" Text
         :> ReqBody '[JSON] PublicEventTicketBankTransferEvidenceRequest
         :> Post '[JSON] PublicEventTicketCheckoutResponse
