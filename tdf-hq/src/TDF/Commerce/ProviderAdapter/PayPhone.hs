{-# LANGUAGE OverloadedStrings #-}

-- | PayPhone API Sale adapter. PayPhone's response URL is not signed, so every
-- callback is deliberately classified as untrusted until the authenticated
-- transaction-status endpoint confirms merchant reference, amount, currency,
-- and status.
module TDF.Commerce.ProviderAdapter.PayPhone
  ( PayPhoneConfig(..)
  , payPhoneAdapter
  ) where

import           Control.Monad (unless, when)
import           Control.Applicative ((<|>))
import           Data.Aeson ((.:), (.:?), (.=), Value)
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Char (isDigit)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
  ( TimeOfDay(..), hoursToTimeZone, localDay, localTimeOfDay, utcToLocalTime )

import           TDF.Commerce.CheckoutStore (PaymentProvider(..))
import           TDF.Commerce.ProviderAdapter
import           TDF.Commerce.ProviderCapabilities
  ( PaymentMethod(..), ProviderOutcomeCertainty(..) )

data PayPhoneConfig = PayPhoneConfig
  { payPhoneToken   :: Text
  , payPhoneStoreId :: Text
  }

payPhoneAdapter :: PayPhoneConfig -> Either AdapterError ProviderAdapter
payPhoneAdapter config = do
  unless (validVisibleCredential (payPhoneToken config))
    (Left (AdapterError "PayPhone token is missing or invalid."))
  unless (validProviderIdentifier (payPhoneStoreId config))
    (Left (AdapterError "PayPhone store ID is missing or invalid."))
  pure ProviderAdapter
    { adapterProvider = ProviderPayPhone
    , adapterBuildCreate = buildCreate config
    , adapterBuildQuery = buildQuery config
    , adapterBuildCancel = buildCancel config
    , adapterBuildCapture = \_ _ -> unsupportedOperation
    , adapterBuildVoid = \_ _ -> unsupportedOperation
    , adapterBuildRefund = \_ _ -> unsupportedOperation
    , adapterBuildSameDayReverse = buildSameDayReverse config
    , adapterParseResponse = parseResponse
    , adapterAssessNotification = assessCallback config
    }

buildCreate
  :: PayPhoneConfig
  -> AdapterContext
  -> CreatePayment
  -> Either AdapterError AdapterRequest
buildCreate config _ payment = do
  validateCreate payment
  unless (cpPaymentMethod payment == MethodPayPhoneWallet)
    (Left (AdapterError "PayPhone API Sale supports only the PayPhone wallet flow."))
  validateUsdMoney (cpMoney payment)
  phone <- maybe
    (Left (AdapterError "PayPhone buyer phone number is required."))
    Right
    (cpBuyerPhone payment)
  countryCode <- maybe
    (Left (AdapterError "PayPhone E.164 country code is required."))
    Right
    (cpBuyerCountryCode payment)
  unless (digitsBetween 6 15 phone)
    (Left (AdapterError "PayPhone buyer phone number is invalid."))
  unless (digitsBetween 1 3 countryCode)
    (Left (AdapterError "PayPhone country code is invalid."))
  let money = cpMoney payment
  pure AdapterRequest
    { arProvider = ProviderPayPhone
    , arOperation = AdapterCreate
    , arMethod = AdapterPost
    , arUrl = payPhoneBaseUrl <> "/api/Sale"
    , arHeaders = payPhoneHeaders config
    , arBody = Just (A.object
        [ "phoneNumber" .= phone
        , "countryCode" .= countryCode
        , "amount" .= mbTotalMinor money
        , "amountWithoutTax" .= mbWithoutTaxMinor money
        , "amountWithTax" .= mbTaxableBaseMinor money
        , "tax" .= mbTaxMinor money
        , "service" .= mbServiceMinor money
        , "tip" .= mbTipMinor money
        , "storeId" .= payPhoneStoreId config
        , "clientTransactionId" .= cpReference payment
        , "currency" .= normalizedCurrency (mbCurrency money)
        , "reference" .= cpDescription payment
        , "responseUrl" .= cpReturnUrl payment
        ])
    , arRetryPolicy = QueryBeforeRetry
    }

buildQuery
  :: PayPhoneConfig
  -> AdapterContext
  -> PaymentLocator
  -> Either AdapterError AdapterRequest
buildQuery config _ locator = do
  validateClientReference (epReference (plExpected locator))
  pure AdapterRequest
    { arProvider = ProviderPayPhone
    , arOperation = AdapterQuery
    , arMethod = AdapterGet
    , arUrl = payPhoneBaseUrl <> "/api/Sale/client/" <> epReference (plExpected locator)
    , arHeaders = payPhoneHeaders config
    , arBody = Nothing
    , arRetryPolicy = SafeReadRetry
    }

buildCancel
  :: PayPhoneConfig
  -> AdapterContext
  -> PaymentLocator
  -> Either AdapterError AdapterRequest
buildCancel config _ locator = do
  validateClientReference (epReference (plExpected locator))
  pure (clientMutationRequest config AdapterCancel "/api/Cancel/Client" locator)

buildSameDayReverse
  :: PayPhoneConfig
  -> AdapterContext
  -> PaymentMutation
  -> Either AdapterError AdapterRequest
buildSameDayReverse config context mutation = do
  let locator = pmLocator mutation
      expected = plExpected locator
  validateClientReference (epReference expected)
  unless (pmAmountMinor mutation == epAmountMinor expected)
    (Left (AdapterError "PayPhone supports only a full same-day reversal."))
  unless (eligibleForSameDayReverse context mutation)
    (Left (AdapterError "PayPhone reversal window has closed."))
  pure (clientMutationRequest config AdapterSameDayReverse "/api/Reverse/Client" locator)

clientMutationRequest
  :: PayPhoneConfig
  -> AdapterOperation
  -> Text
  -> PaymentLocator
  -> AdapterRequest
clientMutationRequest config operation path locator = AdapterRequest
  { arProvider = ProviderPayPhone
  , arOperation = operation
  , arMethod = AdapterPost
  , arUrl = payPhoneBaseUrl <> path
  , arHeaders = payPhoneHeaders config
  , arBody = Just (A.object ["clientId" .= epReference (plExpected locator)])
  , arRetryPolicy = QueryBeforeRetry
  }

parseResponse
  :: AdapterOperation
  -> PaymentLocator
  -> Value
  -> Either AdapterError AdapterResult
parseResponse operation locator value = case operation of
  AdapterCreate -> parsePayPhoneValue parseCreate value
  AdapterQuery -> parsePayPhoneValue (parseQuery locator) value
  AdapterCancel -> parsePayPhoneValue
    (parseBooleanMutation AdapterCancelled ProviderConfirmedNoCharge locator) value
  AdapterSameDayReverse -> parsePayPhoneValue
    (parseBooleanMutation AdapterReversed ProviderSucceeded locator) value
  _ -> unsupportedOperation

parseCreate :: Value -> Parser AdapterResult
parseCreate = A.withObject "PayPhone create response" $ \object -> do
  transactionId <- object .: "transactionId" :: Parser Int64
  when (transactionId <= 0) (fail "PayPhone returned an invalid transaction ID")
  pure AdapterResult
    { adapterResultState = AdapterPending
    , adapterResultExternalId = T.pack (show transactionId)
    , adapterResultRedirectUrl = Nothing
    , adapterResultAmountMinor = Nothing
    , adapterResultCurrency = Nothing
    , adapterResultCertainty = ProviderAmbiguous
    }

parseQuery :: PaymentLocator -> Value -> Parser AdapterResult
parseQuery locator = A.withObject "PayPhone query response" $ \object -> do
  clientReference <- object .: "clientTransactionId"
  transactionId <- object .: "transactionId" :: Parser Int64
  amountMinor <- object .: "amount" :: Parser Int64
  currency <- normalizedCurrency <$> object .: "currency"
  statusCode <- object .: "statusCode" :: Parser Int
  let transactionIdText = T.pack (show transactionId)
  unless (transactionIdText == plExternalId locator)
    (fail "PayPhone transaction ID does not match the stored sale")
  validateBindingParser (plExpected locator) clientReference amountMinor currency
  let (state, certainty) = payPhoneStatus statusCode
  pure AdapterResult
    { adapterResultState = state
    , adapterResultExternalId = transactionIdText
    , adapterResultRedirectUrl = Nothing
    , adapterResultAmountMinor = Just amountMinor
    , adapterResultCurrency = Just currency
    , adapterResultCertainty = certainty
    }

parseBooleanMutation
  :: AdapterResultState
  -> ProviderOutcomeCertainty
  -> PaymentLocator
  -> Value
  -> Parser AdapterResult
parseBooleanMutation state certainty locator = A.withBool "PayPhone mutation response" $ \confirmed -> do
  unless confirmed (fail "PayPhone did not confirm the requested operation")
  pure AdapterResult
    { adapterResultState = state
    , adapterResultExternalId = plExternalId locator
    , adapterResultRedirectUrl = Nothing
    , adapterResultAmountMinor = Nothing
    , adapterResultCurrency = Nothing
    , adapterResultCertainty = certainty
    }

assessCallback :: PayPhoneConfig -> Value -> Either AdapterError NotificationAssessment
assessCallback config = parsePayPhoneValue $ A.withObject "PayPhone callback" $ \object -> do
  transactionId <-
    (object .: "TransactionId" :: Parser Int64)
      <|> object .: "id"
  clientReference <-
    (object .: "ClientTransactionId" :: Parser Text)
      <|> object .: "clientTransactionID"
  statusCode <- object .:? "StatusCode" :: Parser (Maybe Int)
  callbackStoreId <- object .:? "StoreId" :: Parser (Maybe Text)
  when (transactionId <= 0) (fail "PayPhone callback transaction ID is invalid")
  unless (validProviderIdentifier clientReference)
    (fail "PayPhone callback client transaction ID is invalid")
  when (maybe False (/= payPhoneStoreId config) callbackStoreId)
    (fail "PayPhone callback store ID does not match this merchant")
  pure NotificationAssessment
    { notificationExternalId = T.pack (show transactionId)
    , notificationMerchantReference = Just clientReference
    , notificationProviderStatus = T.pack . show <$> statusCode
    , notificationAuthenticated = False
    , notificationRequiresQuery = True
    }

payPhoneStatus :: Int -> (AdapterResultState, ProviderOutcomeCertainty)
payPhoneStatus statusCode = case statusCode of
  1 -> (AdapterPending, ProviderAmbiguous)
  -- The documented terminal label is Canceled. The numeric code does not
  -- establish whether the buyer, provider or issuer caused the outcome.
  2 -> (AdapterCancelled, ProviderConfirmedNoCharge)
  3 -> (AdapterSucceeded, ProviderSucceeded)
  _ -> (AdapterUnknown, ProviderAmbiguous)

validateBindingParser :: ExpectedPayment -> Text -> Int64 -> Text -> Parser ()
validateBindingParser expected reference amountMinor currency = do
  unless (reference == epReference expected)
    (fail "Provider payment reference does not match the order")
  unless (amountMinor == epAmountMinor expected)
    (fail "Provider payment amount does not match the order")
  unless (normalizedCurrency currency == normalizedCurrency (epCurrency expected))
    (fail "Provider payment currency does not match the order")

validateCreate :: CreatePayment -> Either AdapterError ()
validateCreate payment
  | not (validProviderIdentifier (cpReference payment)) =
      Left (AdapterError "PayPhone client transaction ID is invalid.")
  | T.null (T.strip (cpDescription payment)) =
      Left (AdapterError "PayPhone payment reference is required.")
  | not ("https://" `T.isPrefixOf` T.toLower (cpReturnUrl payment)) =
      Left (AdapterError "PayPhone response URL must use HTTPS.")
  | otherwise = Right ()

validateClientReference :: Text -> Either AdapterError ()
validateClientReference reference
  | validProviderIdentifier reference = Right ()
  | otherwise = Left (AdapterError "PayPhone client transaction ID is invalid.")

digitsBetween :: Int -> Int -> Text -> Bool
digitsBetween minimumLength maximumLength value =
  T.length value >= minimumLength
    && T.length value <= maximumLength
    && T.all isDigit value

eligibleForSameDayReverse :: AdapterContext -> PaymentMutation -> Bool
eligibleForSameDayReverse context mutation =
  localDay actionLocal == localDay nowLocal
    && localTimeOfDay nowLocal < TimeOfDay 20 0 0
    && pmProviderActionAt mutation <= acNow context
  where
    ecuadorTimeZone = hoursToTimeZone (-5)
    actionLocal = utcToLocalTime ecuadorTimeZone (pmProviderActionAt mutation)
    nowLocal = utcToLocalTime ecuadorTimeZone (acNow context)

payPhoneHeaders :: PayPhoneConfig -> [(Text, SensitiveText)]
payPhoneHeaders config =
  [ ("Authorization", sensitiveText ("Bearer " <> payPhoneToken config))
  , ("Content-Type", sensitiveText "application/json")
  ]

payPhoneBaseUrl :: Text
payPhoneBaseUrl = "https://pay.payphonetodoesposible.com"

parsePayPhoneValue :: (Value -> Parser result) -> Value -> Either AdapterError result
parsePayPhoneValue parser value = case parseEither parser value of
  Left _ -> Left (AdapterError "PayPhone returned an invalid or mismatched response.")
  Right result -> Right result
