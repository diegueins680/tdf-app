{-# LANGUAGE OverloadedStrings #-}

-- | PlaceToPay WebCheckout adapter built from the official session and
-- notification contracts. It performs no network I/O; the shared executor is
-- responsible for timeouts, TLS, inbox persistence, and redacted logging.
module TDF.Commerce.ProviderAdapter.PlaceToPay
  ( PlaceToPayConfig(..)
  , placeToPayAdapter
  , placeToPayReference
  , verifyPlaceToPayNotification
  ) where

import           Control.Monad (unless, when)
import           Crypto.Hash (Digest, SHA256, hash)
import           Data.Aeson ((.:), (.:?), (.=), Value)
import qualified Data.Aeson as A
import           Data.Aeson.Types (Pair, Parser, parseEither)
import           Data.ByteArray (constEq, convert)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (FormatTime, addUTCTime, defaultTimeLocale, formatTime)

import           TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment(..), PaymentProvider(..) )
import           TDF.Commerce.ProviderAdapter
import           TDF.Commerce.ProviderCapabilities
  ( ProviderOutcomeCertainty(..) )

data PlaceToPayConfig = PlaceToPayConfig
  { ptpEnvironment :: CheckoutEnvironment
  , ptpLogin       :: Text
  , ptpSecretKey   :: Text
  }

placeToPayAdapter :: PlaceToPayConfig -> Either AdapterError ProviderAdapter
placeToPayAdapter config = do
  unless (validVisibleCredential (ptpLogin config))
    (Left (AdapterError "PlaceToPay login is missing or invalid."))
  unless (validVisibleCredential (ptpSecretKey config))
    (Left (AdapterError "PlaceToPay secret key is missing or invalid."))
  pure ProviderAdapter
    { adapterProvider = ProviderPlaceToPay
    , adapterBuildCreate = buildCreate config
    , adapterBuildQuery = buildQuery config
    , adapterBuildCancel = buildCancel config
    , adapterBuildCapture = \_ _ -> unsupportedOperation
    , adapterBuildVoid = \_ _ -> unsupportedOperation
    , adapterBuildRefund = \_ _ -> unsupportedOperation
    , adapterBuildSameDayReverse = \_ _ -> unsupportedOperation
    , adapterParseResponse = parseResponse config
    , adapterAssessNotification = verifyPlaceToPayNotification config
    }

placeToPayReference :: Text -> Text
placeToPayReference internalReference =
  "TDF-" <> T.take 28 (sha256Hex internalReference)

buildCreate
  :: PlaceToPayConfig
  -> AdapterContext
  -> CreatePayment
  -> Either AdapterError AdapterRequest
buildCreate config context payment = do
  validateContext context
  validateCreate payment
  validateUsdMoney (cpMoney payment)
  pure AdapterRequest
    { arProvider = ProviderPlaceToPay
    , arOperation = AdapterCreate
    , arMethod = AdapterPost
    , arUrl = placeToPayBaseUrl config <> "/api/session"
    , arHeaders = jsonHeaders
    , arBody = Just (A.object fields)
    , arRetryPolicy = ReuseStableReference
    }
  where
    fields =
      [ "auth" .= authentication config context
      , "payment" .= paymentObject payment
      , "expiration" .= iso8601 (addUTCTime (20 * 60) (acNow context))
      , "returnUrl" .= cpReturnUrl payment
      , "ipAddress" .= cpIpAddress payment
      , "userAgent" .= cpUserAgent payment
      ] <> maybe [] (\url -> ["notificationUrl" .= url]) (cpNotificationUrl payment)

buildQuery
  :: PlaceToPayConfig
  -> AdapterContext
  -> PaymentLocator
  -> Either AdapterError AdapterRequest
buildQuery config context locator = do
  validateContext context
  validateRequestId (plExternalId locator)
  pure (authenticatedRequest config context AdapterQuery
    ("/api/session/" <> plExternalId locator) SafeReadRetry)

buildCancel
  :: PlaceToPayConfig
  -> AdapterContext
  -> PaymentLocator
  -> Either AdapterError AdapterRequest
buildCancel config context locator = do
  validateContext context
  validateRequestId (plExternalId locator)
  pure (authenticatedRequest config context AdapterCancel
    ("/api/session/" <> plExternalId locator <> "/cancel") QueryBeforeRetry)

authenticatedRequest
  :: PlaceToPayConfig
  -> AdapterContext
  -> AdapterOperation
  -> Text
  -> AdapterRetryPolicy
  -> AdapterRequest
authenticatedRequest config context operation path retryPolicy = AdapterRequest
  { arProvider = ProviderPlaceToPay
  , arOperation = operation
  , arMethod = AdapterPost
  , arUrl = placeToPayBaseUrl config <> path
  , arHeaders = jsonHeaders
  , arBody = Just (A.object ["auth" .= authentication config context])
  , arRetryPolicy = retryPolicy
  }

authentication :: PlaceToPayConfig -> AdapterContext -> Value
authentication config context = A.object
  [ "login" .= ptpLogin config
  , "tranKey" .= tranKey config context
  , "nonce" .= TE.decodeUtf8 (B64.encode (acRawNonce context))
  , "seed" .= iso8601 (acNow context)
  ]

tranKey :: PlaceToPayConfig -> AdapterContext -> Text
tranKey config context = TE.decodeUtf8 (B64.encode digestBytes)
  where
    seed = TE.encodeUtf8 (iso8601 (acNow context))
    secret = TE.encodeUtf8 (ptpSecretKey config)
    digestBytes = convert
      (hash (acRawNonce context <> seed <> secret) :: Digest SHA256)

paymentObject :: CreatePayment -> Value
paymentObject payment = A.object
  [ "reference" .= cpReference payment
  , "description" .= cpDescription payment
  , "amount" .= A.object amountFields
  ]
  where
    money = cpMoney payment
    amountFields =
      [ "currency" .= normalizedCurrency (mbCurrency money)
      , "total" .= minorToDecimal (mbTotalMinor money)
      ] <> taxFields money

taxFields :: MoneyBreakdown -> [Pair]
taxFields money
  | mbTaxMinor money == 0 = []
  | otherwise =
      [ "taxes" .=
          [ A.object
              [ "kind" .= ("valueAddedTax" :: Text)
              , "amount" .= minorToDecimal (mbTaxMinor money)
              , "base" .= minorToDecimal (mbTaxableBaseMinor money)
              ]
          ]
      ]

parseResponse
  :: PlaceToPayConfig
  -> AdapterOperation
  -> PaymentLocator
  -> Value
  -> Either AdapterError AdapterResult
parseResponse config operation locator value = case operation of
  AdapterCreate -> parseProviderValue (parseCreate config) value
  AdapterQuery -> parseProviderValue (parseQuery locator) value
  AdapterCancel -> parseProviderValue (parseCancel locator) value
  _ -> unsupportedOperation

parseCreate :: PlaceToPayConfig -> Value -> Parser AdapterResult
parseCreate config = A.withObject "PlaceToPay create response" $ \object -> do
  status <- parseStatus object
  unless (status == "OK") (fail "PlaceToPay did not create the session")
  requestId <- object .: "requestId" :: Parser Int64
  when (requestId <= 0) (fail "PlaceToPay returned an invalid request ID")
  processUrl <- object .: "processUrl"
  unless (validProcessUrl config processUrl)
    (fail "PlaceToPay returned a process URL outside the configured host")
  pure AdapterResult
    { adapterResultState = AdapterRequiresCustomerAction
    , adapterResultExternalId = T.pack (show requestId)
    , adapterResultRedirectUrl = Just processUrl
    , adapterResultAmountMinor = Nothing
    , adapterResultCurrency = Nothing
    , adapterResultCertainty = ProviderAmbiguous
    }

parseQuery :: PaymentLocator -> Value -> Parser AdapterResult
parseQuery locator = A.withObject "PlaceToPay query response" $ \object -> do
  requestId <- object .: "requestId" :: Parser Int64
  let requestIdText = T.pack (show requestId)
  unless (requestIdText == plExternalId locator)
    (fail "PlaceToPay request ID does not match the stored session")
  status <- parseStatus object
  request <- object .: "request"
  (reference, amountMinor, currency) <- parseRequestPayment request
  validateBindingParser (plExpected locator) reference amountMinor currency
  payments <- fromMaybe [] <$> object .:? "payment"
  approvedPayment <- anyM paymentApproved payments
  let (resultState, certainty) = placeToPayStatus status approvedPayment
  when (status == "APPROVED" && not approvedPayment)
    (fail "PlaceToPay approval has no approved payment transaction")
  pure AdapterResult
    { adapterResultState = resultState
    , adapterResultExternalId = requestIdText
    , adapterResultRedirectUrl = Nothing
    , adapterResultAmountMinor = Just amountMinor
    , adapterResultCurrency = Just currency
    , adapterResultCertainty = certainty
    }

parseCancel :: PaymentLocator -> Value -> Parser AdapterResult
parseCancel locator = A.withObject "PlaceToPay cancel response" $ \object -> do
  status <- parseStatus object
  unless (status == "OK") (fail "PlaceToPay did not confirm session cancellation")
  pure AdapterResult
    { adapterResultState = AdapterCancelled
    , adapterResultExternalId = plExternalId locator
    , adapterResultRedirectUrl = Nothing
    , adapterResultAmountMinor = Nothing
    , adapterResultCurrency = Nothing
    , adapterResultCertainty = ProviderConfirmedNoCharge
    }

parseRequestPayment :: Value -> Parser (Text, Int64, Text)
parseRequestPayment = A.withObject "PlaceToPay request" $ \request -> do
  payment <- request .: "payment"
  A.withObject "PlaceToPay payment" (\paymentObject' -> do
    reference <- paymentObject' .: "reference"
    amount <- paymentObject' .: "amount"
    A.withObject "PlaceToPay amount" (\amountObject -> do
      currency <- amountObject .: "currency"
      total <- amountObject .: "total" :: Parser Scientific
      amountMinor <- maybe
        (fail "PlaceToPay returned an amount with unsupported precision")
        pure
        (decimalToMinor total)
      pure (reference, amountMinor, normalizedCurrency currency)) amount) payment

paymentApproved :: Value -> Parser Bool
paymentApproved = A.withObject "PlaceToPay payment transaction" $ \payment -> do
  status <- payment .: "status"
  A.withObject "PlaceToPay transaction status"
    (\statusObject -> (== ("APPROVED" :: Text)) <$> statusObject .: "status")
    status

parseStatus :: A.Object -> Parser Text
parseStatus object = do
  status <- object .: "status"
  A.withObject "PlaceToPay status" (\statusObject -> statusObject .: "status") status

placeToPayStatus
  :: Text
  -> Bool
  -> (AdapterResultState, ProviderOutcomeCertainty)
placeToPayStatus status approvedPayment
  | status == "APPROVED" && approvedPayment =
      (AdapterSucceeded, ProviderSucceeded)
  | status == "REJECTED" =
      (AdapterDeclined, ProviderConfirmedNoCharge)
  | status == "EXPIRED" || status == "CANCELLED" =
      (AdapterCancelled, ProviderConfirmedNoCharge)
  | status == "PENDING" || status == "PARTIAL" =
      (AdapterPending, ProviderAmbiguous)
  | otherwise = (AdapterUnknown, ProviderAmbiguous)

verifyPlaceToPayNotification
  :: PlaceToPayConfig
  -> Value
  -> Either AdapterError NotificationAssessment
verifyPlaceToPayNotification config = parseProviderValue parser
  where
    parser = A.withObject "PlaceToPay notification" $ \object -> do
      requestId <- object .: "requestId" :: Parser Int64
      statusValue <- object .: "status"
      (status, date) <- A.withObject "PlaceToPay notification status"
        (\statusObject -> (,) <$> statusObject .: "status" <*> statusObject .: "date")
        statusValue
      signature <- object .: "signature"
      unless ("sha256:" `T.isPrefixOf` T.toLower signature)
        (fail "PlaceToPay notification does not use SHA-256")
      let supplied = TE.encodeUtf8 (T.toLower (T.drop 7 signature))
          expected = TE.encodeUtf8 (sha256Hex
            (T.pack (show requestId) <> status <> date <> ptpSecretKey config))
      unless (expected `constEq` supplied)
        (fail "PlaceToPay notification signature is invalid")
      pure NotificationAssessment
        { notificationExternalId = T.pack (show requestId)
        , notificationMerchantReference = Nothing
        , notificationProviderStatus = Just status
        , notificationAuthenticated = True
        , notificationRequiresQuery = True
        }

validateBindingParser :: ExpectedPayment -> Text -> Int64 -> Text -> Parser ()
validateBindingParser expected reference amountMinor currency = do
  unless (reference == epReference expected)
    (fail "Provider payment reference does not match the order")
  unless (amountMinor == epAmountMinor expected)
    (fail "Provider payment amount does not match the order")
  unless (normalizedCurrency currency == normalizedCurrency (epCurrency expected))
    (fail "Provider payment currency does not match the order")

validateContext :: AdapterContext -> Either AdapterError ()
validateContext context
  | BS.null (acRawNonce context) = Left (AdapterError "A fresh PlaceToPay nonce is required.")
  | BS.length (acRawNonce context) < 16 = Left (AdapterError "PlaceToPay nonce is too short.")
  | otherwise = Right ()

validateCreate :: CreatePayment -> Either AdapterError ()
validateCreate payment
  | T.null (cpReference payment) || T.length (cpReference payment) > 32 =
      Left (AdapterError "PlaceToPay reference must contain 1 to 32 characters.")
  | T.null (T.strip (cpDescription payment)) =
      Left (AdapterError "Payment description is required.")
  | not (validHttpsUrl (cpReturnUrl payment)) =
      Left (AdapterError "PlaceToPay return URL must use HTTPS.")
  | maybe False (not . validHttpsUrl) (cpNotificationUrl payment) =
      Left (AdapterError "PlaceToPay notification URL must use HTTPS.")
  | T.null (T.strip (cpIpAddress payment)) || T.null (T.strip (cpUserAgent payment)) =
      Left (AdapterError "PlaceToPay requires the buyer IP address and user agent.")
  | otherwise = Right ()

validateRequestId :: Text -> Either AdapterError ()
validateRequestId requestId
  | T.null requestId || T.length requestId > 32 || not (T.all (`elem` ['0'..'9']) requestId) =
      Left (AdapterError "PlaceToPay request ID is invalid.")
  | otherwise = Right ()

validHttpsUrl :: Text -> Bool
validHttpsUrl url = "https://" `T.isPrefixOf` T.toLower url

validProcessUrl :: PlaceToPayConfig -> Text -> Bool
validProcessUrl config url = (placeToPayBaseUrl config <> "/") `T.isPrefixOf` url

placeToPayBaseUrl :: PlaceToPayConfig -> Text
placeToPayBaseUrl config = case ptpEnvironment config of
  CheckoutSandbox -> "https://checkout-test.placetopay.ec"
  CheckoutProduction -> "https://checkout.placetopay.ec"

jsonHeaders :: [(Text, SensitiveText)]
jsonHeaders = [("Content-Type", sensitiveText "application/json")]

iso8601 :: FormatTime time => time -> Text
iso8601 = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

sha256Hex :: Text -> Text
sha256Hex value = TE.decodeUtf8
  (BAE.convertToBase BAE.Base16 (hash (TE.encodeUtf8 value) :: Digest SHA256))

parseProviderValue :: (Value -> Parser result) -> Value -> Either AdapterError result
parseProviderValue parser value = case parseEither parser value of
  Left _ -> Left (AdapterError "PlaceToPay returned an invalid or mismatched response.")
  Right result -> Right result

anyM :: Monad monad => (value -> monad Bool) -> [value] -> monad Bool
anyM predicate values = or <$> mapM predicate values
