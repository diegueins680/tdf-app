{-# LANGUAGE OverloadedStrings #-}

-- | PayPhone payment button ("Botón de Pagos") adapter.
--
-- A checkout is prepared server-side (@/api/button/Prepare@) and the buyer
-- pays on PayPhone's hosted page, either with a card or with the PayPhone
-- app. PayPhone then redirects the buyer to the response URL with the
-- PayPhone transaction ID and our client transaction ID.
--
-- Sandbox observation (2026-11-17): an approved but unconfirmed payment is
-- reported by the transaction query as @statusCode 3 / Approved@, with no
-- field distinguishing it from a confirmed one, and PayPhone reverses it to
-- @statusCode 2 / Canceled@ about five minutes later. Therefore:
--
-- * only the authenticated @/api/button/V2/Confirm@ response (modelled as
--   'AdapterCapture', keyed by the PayPhone transaction ID) may establish a
--   successful charge;
-- * a plain query never reports success. Approved stays pending until it is
--   confirmed, and a canceled transaction is a confirmed no-charge outcome.
--
-- PayPhone's response URL and notifications are not signed, so every
-- callback stays untrusted until an authenticated server-to-server response
-- confirms merchant reference, amount, currency and status.
--
-- Bindings created by the retired API Sale flow use a numeric transaction ID
-- as the provider resource; their query, cancel and reversal behaviour is kept.
module TDF.Commerce.ProviderAdapter.PayPhone
  ( PayPhoneConfig(..)
  , payPhoneAdapter
  , payPhoneCreateLocatorId
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (unless, when)
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
  ( PaymentMethod(..), ProviderOutcomeCertainty(..), paymentMethodText )

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
    , adapterBuildCapture = buildConfirm config
    , adapterBuildVoid = \_ _ -> unsupportedOperation
    , adapterBuildRefund = \_ _ -> unsupportedOperation
    , adapterBuildSameDayReverse = buildSameDayReverse config
    , adapterParseResponse = parseResponse
    , adapterAssessNotification = assessCallback config
    }

-- | The placeholder external ID the executor passes while parsing a create
-- response. It carries the requested method so the matching hosted page
-- (card form or PayPhone app) is returned as the redirect.
payPhoneCreateLocatorId :: PaymentMethod -> Text
payPhoneCreateLocatorId method = "pending:" <> paymentMethodText method

buildCreate
  :: PayPhoneConfig
  -> AdapterContext
  -> CreatePayment
  -> Either AdapterError AdapterRequest
buildCreate config _ payment = do
  validateCreate payment
  unless (cpPaymentMethod payment `elem` [MethodCard, MethodPayPhoneWallet])
    (Left (AdapterError "PayPhone supports only card and PayPhone wallet payments."))
  validateUsdMoney (cpMoney payment)
  let money = cpMoney payment
  pure AdapterRequest
    { arProvider = ProviderPayPhone
    , arOperation = AdapterCreate
    , arMethod = AdapterPost
    , arUrl = payPhoneBaseUrl <> "/api/button/Prepare"
    , arHeaders = payPhoneHeaders config
    , arBody = Just (A.object
        [ "amount" .= mbTotalMinor money
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
        , "cancellationUrl" .= cpReturnUrl payment
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

-- | Confirms a button payment. The locator's external ID is the PayPhone
-- transaction ID received on the buyer's return or in a notification; it is
-- untrusted until PayPhone answers for the same client transaction ID with
-- the expected amount and currency. Confirming again is idempotent.
buildConfirm
  :: PayPhoneConfig
  -> AdapterContext
  -> PaymentMutation
  -> Either AdapterError AdapterRequest
buildConfirm config _ mutation = do
  let locator = pmLocator mutation
      expected = plExpected locator
  validateClientReference (epReference expected)
  transactionId <- maybe
    (Left (AdapterError "PayPhone transaction ID is invalid."))
    Right
    (positiveTransactionId (plExternalId locator))
  unless (pmAmountMinor mutation == epAmountMinor expected)
    (Left (AdapterError "PayPhone confirms only the full payment amount."))
  pure AdapterRequest
    { arProvider = ProviderPayPhone
    , arOperation = AdapterCapture
    , arMethod = AdapterPost
    , arUrl = payPhoneBaseUrl <> "/api/button/V2/Confirm"
    , arHeaders = payPhoneHeaders config
    , arBody = Just (A.object
        [ "id" .= transactionId
        , "clientTxId" .= epReference expected
        ])
    , arRetryPolicy = QueryBeforeRetry
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
  AdapterCreate -> parsePayPhoneValue (parseCreate locator) value
  AdapterQuery
    | legacySaleBinding locator -> parsePayPhoneValue (parseLegacyQuery locator) value
    | otherwise -> parsePayPhoneValue (parseButtonQuery locator) value
  AdapterCapture -> parsePayPhoneValue (parseConfirm locator) value
  AdapterCancel -> parsePayPhoneValue
    (parseBooleanMutation AdapterCancelled ProviderConfirmedNoCharge locator) value
  AdapterSameDayReverse -> parsePayPhoneValue
    (parseBooleanMutation AdapterReversed ProviderSucceeded locator) value
  _ -> unsupportedOperation

parseCreate :: PaymentLocator -> Value -> Parser AdapterResult
parseCreate locator = A.withObject "PayPhone prepare response" $ \object -> do
  paymentId <- object .: "paymentId"
  unless (validProviderIdentifier paymentId)
    (fail "PayPhone returned an invalid payment ID")
  appUrl <- object .: "payWithPayPhone"
  cardUrl <- object .: "payWithCard"
  let wanted
        | plExternalId locator == payPhoneCreateLocatorId MethodCard = cardUrl
        | otherwise = appUrl
  unless (hostedCheckoutUrl paymentId wanted)
    (fail "PayPhone returned an unexpected checkout URL")
  pure AdapterResult
    { adapterResultState = AdapterRequiresCustomerAction
    , adapterResultExternalId = paymentId
    , adapterResultRedirectUrl = Just wanted
    , adapterResultAmountMinor = Nothing
    , adapterResultCurrency = Nothing
    , adapterResultCertainty = ProviderAmbiguous
    }

-- | Query result for a button binding. The binding's own resource ID (the
-- prepared payment ID) is reported back, because PayPhone's transaction ID
-- only exists once the buyer starts paying.
parseButtonQuery :: PaymentLocator -> Value -> Parser AdapterResult
parseButtonQuery locator value = do
  entries <- case value of
    A.Array _ -> A.parseJSON value
    A.Object _ -> pure [value]
    _ -> fail "PayPhone query response must be an object or a list"
  transactions <- traverse transactionParser entries
  let matching = filter ((== epReference expected) . txReference) transactions
  when (null matching) (fail "PayPhone query has no transaction for this order")
  mapM_ (validateTransaction expected) matching
  let statuses = map txStatusCode matching
      (state, certainty)
        | all (== 2) statuses = (AdapterCancelled, ProviderConfirmedNoCharge)
        | all (`elem` [1, 2, 3]) statuses = (AdapterPending, ProviderAmbiguous)
        | otherwise = (AdapterUnknown, ProviderAmbiguous)
  pure AdapterResult
    { adapterResultState = state
    , adapterResultExternalId = plExternalId locator
    , adapterResultRedirectUrl = Nothing
    , adapterResultAmountMinor = Just (epAmountMinor expected)
    , adapterResultCurrency = Just (normalizedCurrency (epCurrency expected))
    , adapterResultCertainty = certainty
    }
  where
    expected = plExpected locator

parseConfirm :: PaymentLocator -> Value -> Parser AdapterResult
parseConfirm locator value = do
  transaction <- transactionParser value
  unless (T.pack (show (txId transaction)) == plExternalId locator)
    (fail "PayPhone confirmed a different transaction")
  validateTransaction (plExpected locator) transaction
  let (state, certainty) = case txStatusCode transaction of
        3 -> (AdapterSucceeded, ProviderSucceeded)
        2 -> (AdapterCancelled, ProviderConfirmedNoCharge)
        1 -> (AdapterPending, ProviderAmbiguous)
        _ -> (AdapterUnknown, ProviderAmbiguous)
  pure AdapterResult
    { adapterResultState = state
    , adapterResultExternalId = plExternalId locator
    , adapterResultRedirectUrl = Nothing
    , adapterResultAmountMinor = Just (txAmountMinor transaction)
    , adapterResultCurrency = Just (txCurrency transaction)
    , adapterResultCertainty = certainty
    }

parseLegacyQuery :: PaymentLocator -> Value -> Parser AdapterResult
parseLegacyQuery locator value = do
  transaction <- transactionParser value
  unless (T.pack (show (txId transaction)) == plExternalId locator)
    (fail "PayPhone transaction ID does not match the stored sale")
  validateTransaction (plExpected locator) transaction
  let (state, certainty) = legacySaleStatus (txStatusCode transaction)
  pure AdapterResult
    { adapterResultState = state
    , adapterResultExternalId = plExternalId locator
    , adapterResultRedirectUrl = Nothing
    , adapterResultAmountMinor = Just (txAmountMinor transaction)
    , adapterResultCurrency = Just (txCurrency transaction)
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

data PayPhoneTransaction = PayPhoneTransaction
  { txId          :: Int64
  , txReference   :: Text
  , txAmountMinor :: Int64
  , txCurrency    :: Text
  , txStatusCode  :: Int
  }

transactionParser :: Value -> Parser PayPhoneTransaction
transactionParser = A.withObject "PayPhone transaction" $ \object -> do
  transactionId <- object .: "transactionId"
  when (transactionId <= 0) (fail "PayPhone returned an invalid transaction ID")
  PayPhoneTransaction transactionId
    <$> object .: "clientTransactionId"
    <*> object .: "amount"
    <*> (normalizedCurrency <$> object .: "currency")
    <*> object .: "statusCode"

validateTransaction :: ExpectedPayment -> PayPhoneTransaction -> Parser ()
validateTransaction expected transaction = do
  unless (txReference transaction == epReference expected)
    (fail "Provider payment reference does not match the order")
  unless (txAmountMinor transaction == epAmountMinor expected)
    (fail "Provider payment amount does not match the order")
  unless (txCurrency transaction == normalizedCurrency (epCurrency expected))
    (fail "Provider payment currency does not match the order")

legacySaleStatus :: Int -> (AdapterResultState, ProviderOutcomeCertainty)
legacySaleStatus statusCode = case statusCode of
  1 -> (AdapterPending, ProviderAmbiguous)
  -- The documented terminal label is Canceled. The numeric code does not
  -- establish whether the buyer, provider or issuer caused the outcome.
  2 -> (AdapterCancelled, ProviderConfirmedNoCharge)
  3 -> (AdapterSucceeded, ProviderSucceeded)
  _ -> (AdapterUnknown, ProviderAmbiguous)

-- | API Sale bindings stored PayPhone's numeric transaction ID; button
-- bindings store the alphanumeric prepared payment ID.
legacySaleBinding :: PaymentLocator -> Bool
legacySaleBinding = maybe False (const True) . positiveTransactionId . plExternalId

positiveTransactionId :: Text -> Maybe Int64
positiveTransactionId value
  | T.null value || T.length value > 18 || not (T.all isDigit value) = Nothing
  | otherwise = case reads (T.unpack value) of
      [(transactionId, "")] | transactionId > 0 -> Just transactionId
      _ -> Nothing

-- | Accepts only PayPhone's own hosted checkout pages for this payment.
hostedCheckoutUrl :: Text -> Text -> Bool
hostedCheckoutUrl paymentId url =
  any (\path -> url == payPhoneBaseUrl <> path <> "?paymentId=" <> paymentId)
    ["/PayPhone/Index", "/Anonymous/Index"]

validateCreate :: CreatePayment -> Either AdapterError ()
validateCreate payment
  | not (validProviderIdentifier (cpReference payment)) =
      Left (AdapterError "PayPhone client transaction ID is invalid.")
  | T.length (cpReference payment) > 50 =
      Left (AdapterError "PayPhone client transaction ID is too long.")
  | T.null (T.strip (cpDescription payment)) =
      Left (AdapterError "PayPhone payment reference is required.")
  | not ("https://" `T.isPrefixOf` T.toLower (cpReturnUrl payment)) =
      Left (AdapterError "PayPhone response URL must use HTTPS.")
  | otherwise = Right ()

validateClientReference :: Text -> Either AdapterError ()
validateClientReference reference
  | validProviderIdentifier reference = Right ()
  | otherwise = Left (AdapterError "PayPhone client transaction ID is invalid.")

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
