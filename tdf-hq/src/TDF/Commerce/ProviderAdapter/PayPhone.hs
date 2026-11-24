{-# LANGUAGE OverloadedStrings #-}

-- | PayPhone adapter for two flows. API Sale (the PayPhone wallet) pushes the
-- charge to the buyer's app. The Button ("Botón de Pagos", card method)
-- prepares a hosted payment page and, for a split charge (ADR 0136), carries
-- the encrypted @transferTo@ recipients so PayPhone pays them out directly.
--
-- PayPhone's response URL is not signed, so every callback is deliberately
-- classified as untrusted until an authenticated call confirms merchant
-- reference, amount, currency and status: the transaction-status endpoint for
-- API Sale, and the Button confirmation for Button charges. PayPhone reverses
-- a Button charge that is not confirmed within five minutes of payment, so
-- the Button confirmation is also the query that reconciliation repeats.
-- Button charges carry the 'payPhoneButtonReferencePrefix' in their merchant
-- reference, which is immutable and bound, so a query always takes the right
-- endpoint for the flow that created the charge.
module TDF.Commerce.ProviderAdapter.PayPhone
  ( PayPhoneConfig(..)
  , payPhoneAdapter
  , payPhoneButtonReferencePrefix
  , encryptPayPhoneTransfers
  , safePayPhoneRedirect
  ) where

import           Control.Monad (forM_, unless, when)
import           Control.Applicative ((<|>))
import qualified Crypto.Cipher.AES as AES
import           Crypto.Cipher.Types (cbcEncrypt, cipherInit, nullIV)
import           Crypto.Data.Padding (Format(PKCS7), pad)
import           Crypto.Error (CryptoFailable(..))
import           Data.Aeson ((.:), (.:?), (.=), Value)
import qualified Data.Aeson as A
import           Data.Aeson.Types (Parser, parseEither, parseMaybe)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isDigit)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
  ( TimeOfDay(..), hoursToTimeZone, localDay, localTimeOfDay, utcToLocalTime )

import           TDF.Commerce.CheckoutStore (PaymentProvider(..))
import           TDF.Commerce.ProviderAdapter
import           TDF.Commerce.ProviderCapabilities
  ( PaymentMethod(..), ProviderOutcomeCertainty(..) )

data PayPhoneConfig = PayPhoneConfig
  { payPhoneToken   :: Text
  , payPhoneStoreId :: Text
  , payPhoneCodingPassword :: Maybe Text
    -- ^ The store's coding password from PayPhone Developer. Required only to
    -- send split recipients; its UTF-8 bytes are the AES-256 key, so it must
    -- be exactly 32 bytes long.
  }

payPhoneAdapter :: PayPhoneConfig -> Either AdapterError ProviderAdapter
payPhoneAdapter config = do
  unless (validVisibleCredential (payPhoneToken config))
    (Left (AdapterError "PayPhone token is missing or invalid."))
  unless (validProviderIdentifier (payPhoneStoreId config))
    (Left (AdapterError "PayPhone store ID is missing or invalid."))
  forM_ (payPhoneCodingPassword config) $ \password ->
    unless (BS.length (TE.encodeUtf8 password) == 32)
      (Left (AdapterError "PayPhone coding password must be exactly 32 bytes."))
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
    , adapterQueryFollowUp = buttonConfirmation config
    }

buildCreate
  :: PayPhoneConfig
  -> AdapterContext
  -> CreatePayment
  -> Either AdapterError AdapterRequest
buildCreate config context payment = case cpPaymentMethod payment of
  MethodPayPhoneWallet -> buildSale config context payment
  MethodCard -> buildButtonPrepare config payment
  _ -> Left (AdapterError "PayPhone supports only its wallet and its payment button.")

buildSale
  :: PayPhoneConfig
  -> AdapterContext
  -> CreatePayment
  -> Either AdapterError AdapterRequest
buildSale config _ payment = do
  validateCreate payment
  -- API Sale does not document a split; its charge stays whole.
  refuseSplitTransfers payment
  when (isButtonReference (cpReference payment))
    (Left (AdapterError "PayPhone wallet charges cannot use a button reference."))
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

-- | Button flow ("Botón de Pagos"): PayPhone prepares a hosted page where the
-- buyer pays by card or with the PayPhone app, then returns the buyer to the
-- response URL. Split recipients travel encrypted in @transferTo@; their total
-- never exceeds the charge (PayPhone deducts its own fee before paying them,
-- and the caller has already discounted it from their part).
buildButtonPrepare
  :: PayPhoneConfig
  -> CreatePayment
  -> Either AdapterError AdapterRequest
buildButtonPrepare config payment = do
  validateCreate payment
  unless (isButtonReference (cpReference payment))
    (Left (AdapterError "PayPhone button charges need a button reference."))
  validateUsdMoney (cpMoney payment)
  transferTo <- buttonTransfers config payment
  let money = cpMoney payment
  pure AdapterRequest
    { arProvider = ProviderPayPhone
    , arOperation = AdapterCreate
    , arMethod = AdapterPost
    , arUrl = payPhoneBaseUrl <> "/api/button/Prepare"
    , arHeaders = payPhoneHeaders config
    , arBody = Just (A.object (
        [ "amount" .= mbTotalMinor money
        , "amountWithoutTax" .= mbWithoutTaxMinor money
        , "amountWithTax" .= mbTaxableBaseMinor money
        , "tax" .= mbTaxMinor money
        , "service" .= mbServiceMinor money
        , "tip" .= mbTipMinor money
        , "currency" .= normalizedCurrency (mbCurrency money)
        , "clientTransactionId" .= cpReference payment
        , "storeId" .= payPhoneStoreId config
        , "reference" .= cpDescription payment
        , "responseUrl" .= cpReturnUrl payment
        , "cancellationUrl" .= cpReturnUrl payment
        ] <> maybe [] (\encrypted -> ["transferTo" .= encrypted]) transferTo))
    , arRetryPolicy = QueryBeforeRetry
    }

buttonTransfers :: PayPhoneConfig -> CreatePayment -> Either AdapterError (Maybe Text)
buttonTransfers config payment
  | null transfers = Right Nothing
  | otherwise = do
      password <- maybe
        (Left (AdapterError "PayPhone split needs the store coding password."))
        Right
        (payPhoneCodingPassword config)
      forM_ transfers $ \transfer -> do
        unless (stAmountMinor transfer > 0)
          (Left (AdapterError "PayPhone split amounts must be greater than zero."))
        unless (validRecipient transfer)
          (Left (AdapterError "PayPhone split recipient is invalid."))
      unless (sum (map (toInteger . stAmountMinor) transfers)
          <= toInteger (mbTotalMinor (cpMoney payment)))
        (Left (AdapterError "PayPhone split exceeds the charge."))
      Just <$> encryptPayPhoneTransfers password transfers
  where
    transfers = cpTransfers payment
    validRecipient transfer
      | stByPhone transfer = case T.uncons (stIdentifier transfer) of
          Just ('+', digits) ->
            digitsBetween 8 15 digits && T.take 1 digits /= "0"
          _ -> False
      | otherwise = T.length (stIdentifier transfer) `elem` [10, 13]
          && T.all isDigit (stIdentifier transfer)

-- | PayPhone's @transferTo@ encoding: the recipients as a JSON array of
-- @Identifier@, @Type@ (3 for an identification, 4 for a phone) and @Amount@
-- in cents, encrypted with AES-256-CBC under the store's coding password (its
-- UTF-8 bytes are the key) with no initialization vector (all zero bytes) and
-- PKCS#7 padding, as Base64 text.
encryptPayPhoneTransfers :: Text -> [SplitTransfer] -> Either AdapterError Text
encryptPayPhoneTransfers password transfers =
  case cipherInit (TE.encodeUtf8 password) :: CryptoFailable AES.AES256 of
    CryptoFailed _ -> Left (AdapterError "PayPhone coding password must be exactly 32 bytes.")
    CryptoPassed cipher ->
      let plaintext = BL.toStrict (A.encode (map transferJson transfers))
          ciphertext = cbcEncrypt cipher nullIV (pad (PKCS7 16) plaintext) :: BS.ByteString
      in Right (TE.decodeUtf8 (BAE.convertToBase BAE.Base64 ciphertext))
  where
    transferJson transfer = A.object
      [ "Identifier" .= stIdentifier transfer
      , "Type" .= (if stByPhone transfer then 4 else 3 :: Int)
      , "Amount" .= stAmountMinor transfer
      ]

-- | Merchant references of Button charges start with this prefix; API Sale
-- references never do. The reference is bound to the charge, so reconciliation
-- picks the Button confirmation from it alone.
payPhoneButtonReferencePrefix :: Text
payPhoneButtonReferencePrefix = "TDFC-"

isButtonReference :: Text -> Bool
isButtonReference = T.isPrefixOf payPhoneButtonReferencePrefix

-- | The hosted Button page the buyer is sent to: HTTPS on a PayPhone host,
-- with no credentials, port or unusual characters in the authority.
safePayPhoneRedirect :: Text -> Maybe Text
safePayPhoneRedirect url = do
  rest <- T.stripPrefix "https://" url
  let authority = T.toLower (T.takeWhile (`notElem` ("/?#" :: String)) rest)
      payPhoneHost = authority == "payphonetodoesposible.com"
        || ".payphonetodoesposible.com" `T.isSuffixOf` authority
        || authority == "payphone.app"
        || ".payphone.app" `T.isSuffixOf` authority
  if T.length url <= 2048 && not (T.null authority)
      && T.all (\character -> character == '.' || character == '-' || isDigit character
        || (character >= 'a' && character <= 'z')) authority
      && payPhoneHost
    then Just url
    else Nothing

-- | The authenticated status read by the merchant's client reference. A
-- Button charge has no transaction ID until the buyer pays (Prepare answers
-- only an opaque preparation ID), so it is found by its @TDFC-@ reference too;
-- until the buyer pays, PayPhone answers with an error status, which
-- reconciliation treats as unavailable and retries. An approved Button answer
-- is not final: 'buttonConfirmation' confirms it first.
buildQuery
  :: PayPhoneConfig
  -> AdapterContext
  -> PaymentLocator
  -> Either AdapterError AdapterRequest
buildQuery config _ = buildSaleQuery config

buildSaleQuery
  :: PayPhoneConfig
  -> PaymentLocator
  -> Either AdapterError AdapterRequest
buildSaleQuery config locator = do
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

-- | PayPhone reverses a Button payment that is not confirmed within five
-- minutes (ADR 0136). When the status read shows an approved Button payment
-- whose reference, amount and currency match the binding, the confirmation
-- with its transaction ID is the request that must answer before anything is
-- applied. Any other answer is final as it is.
buttonConfirmation
  :: PayPhoneConfig
  -> AdapterContext
  -> PaymentLocator
  -> Value
  -> Either AdapterError (Maybe QueryFollowUp)
buttonConfirmation config _ locator value
  | not (isButtonReference reference) = Right Nothing
  | otherwise = do
      validateClientReference reference
      (transactionId, result) <- parsePayPhoneValue (parseTransaction locator) value
      pure $ if adapterResultState result == AdapterSucceeded
        then Just (confirmation transactionId)
        else Nothing
  where
    reference = epReference (plExpected locator)
    confirmation transactionId = QueryFollowUp
      { followUpRequest = AdapterRequest
          { arProvider = ProviderPayPhone
          , arOperation = AdapterQuery
          , arMethod = AdapterPost
          , arUrl = payPhoneBaseUrl <> "/api/button/V2/Confirm"
          , arHeaders = payPhoneHeaders config
          , arBody = Just (A.object ["id" .= transactionId, "clientTxId" .= reference])
          , arRetryPolicy = SafeReadRetry
          }
      , parseFollowUpResponse = parsePayPhoneValue $ \answer -> do
          (confirmedId, result) <- parseTransaction locator answer
          unless (confirmedId == transactionId)
            (fail "PayPhone confirmation returned a different transaction ID")
          pure result
      }

buildCancel
  :: PayPhoneConfig
  -> AdapterContext
  -> PaymentLocator
  -> Either AdapterError AdapterRequest
buildCancel config _ locator = do
  validateClientReference (epReference (plExpected locator))
  -- Cancelling applies to a pending API Sale push; a Button charge the buyer
  -- has not paid simply lapses, and one never confirmed is reversed by PayPhone.
  when (isButtonReference (epReference (plExpected locator)))
    (Left (AdapterError "PayPhone button charges cannot be cancelled."))
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
  AdapterQuery -> parsePayPhoneValue (parseQuery locator) value
  AdapterCancel -> parsePayPhoneValue
    (parseBooleanMutation AdapterCancelled ProviderConfirmedNoCharge locator) value
  AdapterSameDayReverse -> parsePayPhoneValue
    (parseBooleanMutation AdapterReversed ProviderSucceeded locator) value
  _ -> unsupportedOperation

parseCreate :: PaymentLocator -> Value -> Parser AdapterResult
parseCreate locator = A.withObject "PayPhone create response" $ \object ->
  if isButtonReference (epReference (plExpected locator))
    then object .: "payWithCard" >>= parsePrepared object
    else parseSaleCreate object

-- | Button Prepare answer: the payment id and the hosted page. The charge is
-- unknown until the buyer pays and the confirmation answers.
parsePrepared :: A.Object -> Text -> Parser AdapterResult
parsePrepared object page = do
  -- An opaque preparation ID (the documentation shows text such as
  -- "GSizecyUkIAkxTj3SQ"); it identifies the binding, never the transaction.
  paymentId <- (object .: "paymentId" :: Parser Text)
    <|> (T.pack . show <$> (object .: "paymentId" :: Parser Int64))
  unless (validProviderIdentifier paymentId)
    (fail "PayPhone returned an invalid payment ID")
  redirect <- maybe (fail "PayPhone payment page is not an allowed PayPhone URL") pure
    (safePayPhoneRedirect page)
  pure AdapterResult
    { adapterResultState = AdapterRequiresCustomerAction
    , adapterResultExternalId = paymentId
    , adapterResultRedirectUrl = Just redirect
    , adapterResultAmountMinor = Nothing
    , adapterResultCurrency = Nothing
    , adapterResultCertainty = ProviderAmbiguous
    }

parseSaleCreate :: A.Object -> Parser AdapterResult
parseSaleCreate object = do
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
parseQuery locator value = do
  (_, result) <- parseTransaction locator value
  when (isButtonReference (epReference (plExpected locator))
      && adapterResultState result == AdapterSucceeded)
    (fail "PayPhone Button approval requires confirmation")
  pure result

-- | Check the financial binding and keep the transaction identity distinct
-- from the stored resource (the preparation ID for Button payments).
--
-- Observed in the PayPhone sandbox (2026-11-17): @GET /api/Sale/client/{ref}@
-- answers a Button payment with a JSON array of transactions, not an object.
-- Only an array holding exactly one transaction for this reference is read;
-- an empty array, or several transactions for one reference, is ambiguous and
-- rejected, so nothing is applied from it. The Button confirmation answers
-- with a single object.
parseTransaction :: PaymentLocator -> Value -> Parser (Int64, AdapterResult)
parseTransaction locator value = case value of
  A.Array transactions ->
    case filter forThisReference (foldr (:) [] transactions) of
      [single] -> parseTransactionObject locator single
      _ -> fail "PayPhone query must list exactly one transaction for this reference"
  _ -> parseTransactionObject locator value
  where
    forThisReference transaction =
      parseMaybe (A.withObject "PayPhone transaction" (.: "clientTransactionId")) transaction
        == Just (epReference (plExpected locator))

parseTransactionObject :: PaymentLocator -> Value -> Parser (Int64, AdapterResult)
parseTransactionObject locator = A.withObject "PayPhone query response" $ \object -> do
  clientReference <- object .: "clientTransactionId"
  transactionId <- object .: "transactionId" :: Parser Int64
  amountMinor <- object .: "amount" :: Parser Int64
  currency <- normalizedCurrency <$> object .: "currency"
  statusCode <- object .: "statusCode" :: Parser Int
  when (transactionId <= 0) (fail "PayPhone returned an invalid transaction ID")
  let transactionIdText = T.pack (show transactionId)
      button = isButtonReference (epReference (plExpected locator))
  -- A Button binding holds the preparation ID; its transaction is bound by the
  -- merchant reference, amount and currency checked below.
  unless (button || transactionIdText == plExternalId locator)
    (fail "PayPhone transaction ID does not match the stored sale")
  validateBindingParser (plExpected locator) clientReference amountMinor currency
  let (state, certainty) = payPhoneStatus statusCode
  pure (transactionId, AdapterResult
    { adapterResultState = state
    , adapterResultExternalId = plExternalId locator
    , adapterResultRedirectUrl = Nothing
    , adapterResultAmountMinor = Just amountMinor
    , adapterResultCurrency = Just currency
    , adapterResultCertainty = certainty
    })

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
