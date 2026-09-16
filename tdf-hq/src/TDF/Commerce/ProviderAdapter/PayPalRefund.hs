{-# LANGUAGE OverloadedStrings #-}

-- | Read-only lookup of an already bound refund. Never follow response links,
-- infer no refund from an error, or build a refund/capture POST here.
module TDF.Commerce.ProviderAdapter.PayPalRefund
  ( RefundQueryBinding(..)
  , RefundQueryOutcome(..)
  , buildRefundQuery
  , parseRefundQuery
  , validateRefundQueryBinding
  ) where

import Control.Monad (unless)
import Data.Aeson ((.:), (.:?), Value)
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, parseEither)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import TDF.Commerce.CheckoutStore (CheckoutEnvironment(..), PaymentProvider(..))
import TDF.Commerce.ProviderAdapter

data RefundQueryBinding = RefundQueryBinding
  { rqbEnvironment :: CheckoutEnvironment
  , rqbRefundId :: Text
  , rqbCaptureId :: Text
  , rqbMerchantId :: Text
  , rqbAmountMinor :: Int64
  , rqbCurrency :: Text
  } deriving (Eq, Show)

-- Failure/cancellation are intentionally held too: releasing money requires a
-- separately qualified finality policy and an audited resolution command.
data RefundQueryOutcome = RefundQueryCompleted | RefundQueryHeld
  deriving (Eq, Show)

buildRefundQuery :: Text -> RefundQueryBinding -> Either AdapterError AdapterRequest
buildRefundQuery token binding = do
  validateRefundQueryBinding binding
  unless (validVisibleCredential token) (Left invalidQuery)
  pure AdapterRequest
    { arProvider = ProviderPayPal, arOperation = AdapterQuery, arMethod = AdapterGet
    , arUrl = refundUrl binding
    , arHeaders = [("Authorization", sensitiveText ("Bearer " <> token))]
    , arBody = Nothing, arRetryPolicy = SafeReadRetry
    }

parseRefundQuery :: RefundQueryBinding -> Value -> Either AdapterError RefundQueryOutcome
parseRefundQuery binding value = do
  validateRefundQueryBinding binding
  either (const (Left invalidEvidence)) Right (parseEither parse value)
  where
    parse = A.withObject "refund" $ \obj -> do
      refundId <- obj .: "id"
      status <- obj .: "status" :: Parser Text
      amount <- obj .: "amount" >>= A.withObject "amount" (\money -> do
        currency <- money .: "currency_code"
        decimal <- money .: "value"
        unless (currency == rqbCurrency binding) (fail "currency")
        maybe (fail "amount") pure (parseMinor decimal))
      links <- obj .: "links" :: Parser [Value]
      parsedLinks <- mapM (A.withObject "link" $ \link ->
        (,,) <$> link .: "rel" <*> link .: "method" <*> link .: "href") links
      let upstream = [ (method, href) | (rel, method, href) <- parsedLinks, rel == ("up" :: Text) ]
          self = [ (method, href) | (rel, method, href) <- parsedLinks, rel == ("self" :: Text) ]
      unless (refundId == rqbRefundId binding && amount == rqbAmountMinor binding
        && upstream == [("GET" :: Text, captureUrl binding)]
        && self == [("GET" :: Text, refundUrl binding)]) (fail "binding")
      -- Some represented responses include the refund payer (the merchant).
      -- Its absence is allowed by the official schema; any provided ID must match.
      payer <- obj .:? "payer" :: Parser (Maybe Value)
      case payer of
        Nothing -> pure ()
        Just details -> A.withObject "payer" (\p -> do
          merchant <- p .:? "merchant_id"
          unless (maybe True (== rqbMerchantId binding) merchant) (fail "merchant")) details
      pure $ if status == "COMPLETED" then RefundQueryCompleted else RefundQueryHeld

validateRefundQueryBinding :: RefundQueryBinding -> Either AdapterError ()
validateRefundQueryBinding binding =
  unless (validProviderIdentifier (rqbRefundId binding)
    && validProviderIdentifier (rqbCaptureId binding)
    && validProviderIdentifier (rqbMerchantId binding)
    && rqbAmountMinor binding > 0 && rqbCurrency binding == "USD") (Left invalidQuery)

baseUrl :: RefundQueryBinding -> Text
baseUrl binding = case rqbEnvironment binding of
  CheckoutSandbox -> "https://api-m.sandbox.paypal.com"
  CheckoutProduction -> "https://api-m.paypal.com"

refundUrl :: RefundQueryBinding -> Text
refundUrl binding = baseUrl binding <> "/v2/payments/refunds/" <> rqbRefundId binding

captureUrl :: RefundQueryBinding -> Text
captureUrl binding = baseUrl binding <> "/v2/payments/captures/" <> rqbCaptureId binding

-- PayPal money is a decimal string. Bound work and reject rounding, signs,
-- exponents, whitespace and fractions of a cent; Integer prevents overflow.
parseMinor :: Text -> Maybe Int64
parseMinor value
  | T.length value > 22 = Nothing
  | otherwise = do
      (whole, fraction) <- case T.splitOn "." value of
        [w] -> Just (w, "00")
        [w, f] | T.length f == 1 -> Just (w, f <> "0")
        [w, f] | T.length f == 2 -> Just (w, f)
        _ -> Nothing
      unless (digits whole && digits fraction) Nothing
      (integer, rest) <- either (const Nothing) Just (TR.decimal (whole <> fraction))
      unless (T.null rest && integer > 0 && integer <= toInteger (maxBound :: Int64)) Nothing
      pure (fromInteger integer)
  where
    digits text = not (T.null text) && T.all (\c -> c >= '0' && c <= '9') text

invalidQuery, invalidEvidence :: AdapterError
invalidQuery = AdapterError "Refund query configuration is unavailable."
invalidEvidence = AdapterError "Refund query evidence does not match the original refund."
