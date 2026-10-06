{-# LANGUAGE OverloadedStrings #-}

-- | Shared provider transport and PayPal credentials/refund adapter. Domain
-- authorization, immutable financial binding and execution claims stay in callers.
module TDF.Server.ProviderTransport
  ( PaypalRefundOutcome(..), parsePaypalRefundOutcome, loadPaypalEnvForService
  , loadRequiredSafeEnv, providerRequest, providerResponse, paypalAccessTokenForService
  , issuePaypalRefundRemote, issuePaypalRefundWithTokenRemote, isProviderReference ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Aeson (FromJSON, Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Manager, Request(..), RequestBody(..))
import Servant
import System.Environment (lookupEnv)
import TDF.DB (Env)
import TDF.Internationalization (formatMinorUnitsDecimal)
import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.RefundStore as Refund
import qualified TDF.Commerce.ProviderAdapter.Http as ProviderHttp

type AppM = ReaderT Env Handler

configurationError, providerValidationError :: Text -> ServerError
configurationError message = err500 { errBody=BL.fromStrict (TE.encodeUtf8 message) }
providerValidationError message = err502 { errBody=BL.fromStrict (TE.encodeUtf8 message) }

lookupObject :: Text -> Aeson.Object -> Maybe Aeson.Object
lookupObject key obj = case KM.lookup (AesonKey.fromText key) obj of
  Just (Object value) -> Just value
  _ -> Nothing

requiredObjectText :: Text -> Aeson.Object -> Either Text Text
requiredObjectText key obj = case KM.lookup (AesonKey.fromText key) obj of
  Just (String value) | not (T.null (T.strip value)) -> Right (T.strip value)
  _ -> Left ("PayPal payload omitted " <> key)

data PaypalRefundOutcome = PaypalRefundOutcome
  { proRefundId :: Text
  , proStatus   :: Text
  , proAmount   :: Text
  , proCurrency :: Text
  } deriving (Eq, Show)


parsePaypalRefundOutcome :: Value -> Either Text PaypalRefundOutcome
parsePaypalRefundOutcome (Object obj) = do
  refundId <- requiredObjectText "id" obj
  status <- requiredObjectText "status" obj
  amountObject <- maybe (Left "PayPal refund response omitted amount") Right
    (lookupObject "amount" obj)
  amount <- requiredObjectText "value" amountObject
  currency <- requiredObjectText "currency_code" amountObject
  unless (isProviderReference refundId) $
    Left "PayPal refund response contains an invalid refund ID"
  pure PaypalRefundOutcome
    { proRefundId = refundId
    , proStatus = T.toUpper (T.strip status)
    , proAmount = T.strip amount
    , proCurrency = T.toUpper (T.strip currency)
    }
parsePaypalRefundOutcome _ = Left "PayPal refund response must be an object"

loadPaypalEnvForService
  :: AppM (Text, Text, String, Checkout.CheckoutEnvironment, Text)
loadPaypalEnvForService = do
  mEnv <- liftIO $ lookupEnv "PAYPAL_ENV"
  mMerchant <- liftIO $ lookupEnv "PAYPAL_MERCHANT_ID"
  cid <- loadRequiredSafeEnv "PAYPAL_CLIENT_ID" 256
  secret <- loadRequiredSafeEnv "PAYPAL_CLIENT_SECRET" 512
  environment <- either (throwError . configurationError) pure
    (Checkout.resolveCheckoutEnvironment mEnv)
  merchantRef <- case T.strip . T.pack <$> mMerchant of
    Just value | isProviderReference value -> pure value
    _ -> throwError err500
      { errBody = "PAYPAL_MERCHANT_ID must be configured with the provider merchant account ID" }
  let baseUrl = case environment of
        Checkout.CheckoutSandbox -> "https://api-m.sandbox.paypal.com"
        Checkout.CheckoutProduction -> "https://api-m.paypal.com"
  pure (cid, secret, baseUrl, environment, merchantRef)

loadRequiredSafeEnv :: String -> Int -> AppM Text
loadRequiredSafeEnv variableName maxLength = do
  rawValue <- liftIO (lookupEnv variableName)
  case T.strip . T.pack <$> rawValue of
    Just value
      | not (T.null value)
      , T.length value <= maxLength
      , T.all (\character -> character >= '!' && character <= '~') value -> pure value
    _ -> throwError err500
      { errBody = BL.fromStrict (TE.encodeUtf8
          (T.pack variableName <> " must be configured with safe visible ASCII")) }


providerRequest :: Checkout.PaymentProvider -> String -> AppM Request
providerRequest provider url = do
  result <- liftIO (ProviderHttp.parseProviderRequest provider url)
  either (throwError . providerTransportError) pure result

providerResponse :: FromJSON a => Manager -> Checkout.PaymentProvider -> Request -> AppM a
providerResponse manager provider request = do
  result <- liftIO (ProviderHttp.executeProviderRequest manager provider request)
  either (throwError . providerTransportError) pure result

providerTransportError :: ProviderHttp.AdapterTransportError -> ServerError
providerTransportError failure = err502
  { errBody = BL.fromStrict (TE.encodeUtf8
      (ProviderHttp.adapterTransportPublicMessage failure)) }


paypalAccessTokenForService :: Manager -> Text -> Text -> String -> AppM Text
paypalAccessTokenForService manager cid sec baseUrl = do
  req0 <- providerRequest Checkout.ProviderPayPal (baseUrl ++ "/v1/oauth2/token")
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyBS "grant_type=client_credentials"
        , requestHeaders =
            [ ("Authorization", "Basic " <> encodeBasicAuth cid sec)
            , ("Content-Type", "application/x-www-form-urlencoded")
            ]
        }
  response <- providerResponse manager Checkout.ProviderPayPal req
  case response of
    Object obj -> case (KM.lookup "access_token" obj, KM.lookup "token_type" obj) of
      (Just (String token), Just (String tokenType))
        | not (T.null token) && T.length token <= 4096
        , T.all (\c -> c >= '!' && c <= '~') token
        , T.toLower tokenType == "bearer" -> pure token
      _ -> throwError err502 { errBody = "Invalid PayPal access token or token type" }
    _ -> throwError err502 { errBody = "Invalid PayPal token response format" }


issuePaypalRefundRemote
  :: Manager
  -> Text
  -> Text
  -> String
  -> Text
  -> Refund.RefundRecord
  -> AppM PaypalRefundOutcome
issuePaypalRefundRemote manager cid sec baseUrl captureId refundRecord = do
  token <- paypalAccessTokenForService manager cid sec baseUrl
  issuePaypalRefundWithTokenRemote manager token baseUrl captureId refundRecord

-- OAuth happens before a caller consumes the durable, single-POST permit.
issuePaypalRefundWithTokenRemote
  :: Manager -> Text -> String -> Text -> Refund.RefundRecord -> AppM PaypalRefundOutcome
issuePaypalRefundWithTokenRemote manager token baseUrl captureId refundRecord = do
  req0 <- providerRequest Checkout.ProviderPayPal
    (baseUrl ++ "/v2/payments/captures/" ++ T.unpack captureId ++ "/refund")
  let body = object
        [ "custom_id" .= Refund.refundReferenceId (Refund.rrReference refundRecord)
        , "amount" .= object
            [ "currency_code" .= Refund.rrCurrency refundRecord
            , "value" .= formatMinorUnitsDecimal
                (Refund.rrCurrency refundRecord)
                (fromIntegral (Refund.rrAmountMinor refundRecord))
            ]
        ]
      requestId = Refund.refundReferenceId (Refund.rrReference refundRecord)
      req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS (Aeson.encode body)
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 token)
            , ("PayPal-Request-Id", TE.encodeUtf8 requestId)
            , ("Prefer", "return=representation")
            ]
        }
  value <- providerResponse manager Checkout.ProviderPayPal req
  either (throwError . providerValidationError) pure (parsePaypalRefundOutcome value)


isProviderReference :: Text -> Bool
isProviderReference value =
  not (T.null value)
    && T.length value <= 256
    && T.any (\c -> isAsciiLower c || isAsciiUpper c || isDigit c) value
    && T.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_." :: String)) value

-- | Encode Basic auth header.
encodeBasicAuth :: Text -> Text -> ByteString
encodeBasicAuth cid sec =
  let credentials = TE.encodeUtf8 (cid <> ":" <> sec)
  in TE.encodeUtf8 (T.pack (encodeBase64 credentials))

-- | Simple Base64 encoding.
encodeBase64 :: ByteString -> String
encodeBase64 bs =
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
      toChar n = chars !! (n `mod` 64)
      bytes = map fromIntegral (BS.unpack bs) :: [Int]
      triples = splitInto 3 bytes
      encodeTriple [a] = [toChar (a `div` 4), toChar ((a `mod` 4) * 16)]
      encodeTriple [a, b] = [toChar (a `div` 4), toChar ((a `mod` 4) * 16 + b `div` 16), toChar ((b `mod` 16) * 4)]
      encodeTriple [a, b, c] = [toChar (a `div` 4), toChar ((a `mod` 4) * 16 + b `div` 16), toChar ((b `mod` 16) * 4 + c `div` 64), toChar (c `mod` 64)]
      encodeTriple _ = ""
      splitInto _ [] = []
      splitInto n xs = take n xs : splitInto n (drop n xs)
      pad = let r = BS.length bs `mod` 3 in if r == 0 then "" else replicate (3 - r) '='
  in concatMap encodeTriple triples ++ pad
