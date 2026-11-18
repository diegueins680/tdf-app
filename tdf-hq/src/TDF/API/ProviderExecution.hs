{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.ProviderExecution
  ( ProviderExecutionAPI
  , PaymentSessionCreateDTO(..)
  , PaymentSessionDTO(..)
  , PayPhoneNotificationAck(..)
  , PayPhoneConfirmationDTO(..)
  ) where

import           Data.Aeson
  ( FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, object, (.=) )
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant

import           TDF.API.Types (prefixedStrictObjectOptions)
import           TDF.API.Types (RawJSON)

data PaymentSessionCreateDTO = PaymentSessionCreateDTO
  { pscProvider         :: Text
  , pscPaymentMethod    :: Text
  , pscBuyerPhone       :: Maybe Text
  , pscBuyerCountryCode :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON PaymentSessionCreateDTO where
  toJSON = genericToJSON (prefixedStrictObjectOptions 3)
instance FromJSON PaymentSessionCreateDTO where
  parseJSON = genericParseJSON (prefixedStrictObjectOptions 3)

data PaymentSessionDTO = PaymentSessionDTO
  { pssCheckoutId      :: Text
  , pssAttemptId       :: Text
  , pssOperationId     :: Text
  , pssProvider        :: Text
  , pssState           :: Text
  , pssExternalId      :: Maybe Text
  , pssRedirectUrl     :: Maybe Text
  , pssOutcomeCertainty :: Text
  , pssCanRetryOrFallback :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON PaymentSessionDTO where
  toJSON = genericToJSON (prefixedStrictObjectOptions 3)
instance FromJSON PaymentSessionDTO where
  parseJSON = genericParseJSON (prefixedStrictObjectOptions 3)

-- | What PayPhone appends to the response URL when the buyer returns. Both
-- values are untrusted; the server confirms them with PayPhone.
data PayPhoneConfirmationDTO = PayPhoneConfirmationDTO
  { ppcTransactionId       :: Int64
  , ppcClientTransactionId :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON PayPhoneConfirmationDTO where
  toJSON = genericToJSON (prefixedStrictObjectOptions 3)
instance FromJSON PayPhoneConfirmationDTO where
  parseJSON = genericParseJSON (prefixedStrictObjectOptions 3)

data PayPhoneNotificationAck = PayPhoneNotificationAck
  { ppnAccepted  :: Bool
  , ppnErrorCode :: Text
  } deriving (Eq, Show)

-- PayPhone requires these response keys with their documented capitalization.
instance ToJSON PayPhoneNotificationAck where
  toJSON acknowledgment = object
    [ "Response" .= ppnAccepted acknowledgment
    , "ErrorCode" .= ppnErrorCode acknowledgment
    ]

type ProviderExecutionAPI =
       "commerce" :> "checkouts" :> Capture "checkoutId" Text
         :> "payment-sessions"
         :> Header "X-Checkout-Lookup-Token" Text
         :> Header "Idempotency-Key" Text
         :> Header "User-Agent" Text
         :> RemoteHost
         :> ReqBody '[JSON] PaymentSessionCreateDTO
         :> PostAccepted '[JSON] PaymentSessionDTO
  :<|> "commerce" :> "checkouts" :> Capture "checkoutId" Text
         :> "payment-sessions" :> Capture "attemptId" Text
         :> Header "X-Checkout-Lookup-Token" Text
         :> Get '[JSON] PaymentSessionDTO
  :<|> "commerce" :> "checkouts" :> Capture "checkoutId" Text
         :> "payment-sessions" :> Capture "attemptId" Text
         :> "payphone-confirmation"
         :> Header "X-Checkout-Lookup-Token" Text
         :> ReqBody '[JSON] PayPhoneConfirmationDTO
         :> Post '[JSON] PaymentSessionDTO
  :<|> "commerce" :> "provider-notifications" :> "placetopay"
         :> ReqBody '[RawJSON] BL.ByteString
         :> Post '[JSON] NoContent
  :<|> "commerce" :> "provider-notifications" :> "payphone"
         :> ReqBody '[RawJSON] BL.ByteString
         :> Post '[JSON] PayPhoneNotificationAck
  :<|> "NotificacionPago"
         :> ReqBody '[RawJSON] BL.ByteString
         :> Post '[JSON] PayPhoneNotificationAck
