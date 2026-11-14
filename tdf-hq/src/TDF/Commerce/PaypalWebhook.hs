{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | Processing of verified PayPal webhook events: envelope and capture
-- parsing, binding a capture to the canonical checkout, late-capture
-- reconciliation and external refund or reversal handling. Signature
-- verification stays with the HTTP handler; this module is shared by that
-- handler and the provider event worker and must not depend on either.
module TDF.Commerce.PaypalWebhook
  ( PaypalWebhookEnvelope(..)
  , PaypalWebhookCapture(..)
  , PaypalEventProcessResult(..)
  , BoundPaypalCapture(..)
  , parsePaypalWebhookEnvelope
  , parsePaypalWebhookCapture
  , parsePaypalExternalCaptureChange
  , paypalWebhookResourceId
  , validatePaypalWebhookCaptureBinding
  , paypalWebhookDomainSupported
  , paypalCaptureRequiresReconciliation
  , processPaypalWebhookEventIO
  , parseDatafastCents
  , parseAesonUtc
  , requiredObjectText
  , validPaypalEventType
  , lookupObject
  , lookupObjectText
  , isProviderReference
  ) where

import           Control.Monad (when, unless)
import           Control.Exception.Safe (tryAny)
import           Data.Aeson (Result(..), eitherDecode, Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID (fromText, toText)
import           Data.Time (UTCTime)
import           Database.Persist (PersistValue(..), toPersistValue)
import           Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import           Web.PathPieces (fromPathPiece)

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.ProviderAdapter.PayPalRefund as RefundQuery
import qualified TDF.Commerce.RefundStore as Refund
import           TDF.DB (Env(..))
import qualified TDF.Models.SocialEventsModels as SM
import qualified TDF.Ticketing.Issuance as Issuance
import qualified TDF.Ticketing.Refund as TicketRefund

processPaypalWebhookEventIO
  :: Env
  -> Checkout.CheckoutEnvironment
  -> Text
  -> PaypalWebhookEnvelope
  -> UTCTime
  -> IO PaypalEventProcessResult
processPaypalWebhookEventIO Env{envPool = pool} environment merchantRef envelope now =
  case pweEventType envelope of
    "PAYMENT.CAPTURE.COMPLETED" -> processCompletedCapture
    "PAYMENT.CAPTURE.REFUNDED" -> processExternalCaptureChange
      "external_refund_detected"
    "PAYMENT.CAPTURE.REVERSED" -> processExternalCaptureChange
      "external_reversal_detected"
    _ -> pure PaypalEventIgnored
  where
    processCompletedCapture = case parsePaypalWebhookCapture envelope of
      Left message -> pure (PaypalEventPermanentFailure message Nothing Nothing Nothing)
      Right capture -> do
        result <- tryAny $ flip runSqlPool pool $ do
          mCaptureBound <- loadBoundPaypalCapture
            environment merchantRef (pwcCaptureId capture)
          mBound <- case mCaptureBound of
            Just existing -> pure (Just existing)
            Nothing -> loadBoundPaypalOrder
              environment merchantRef (pwcPaypalOrderId capture)
          case mBound of
            Nothing -> pure (PaypalEventIgnored, Nothing)
            Just bound -> case validatePaypalWebhookCaptureBinding merchantRef bound capture of
              Left message -> do
                recordPaypalWebhookMismatch environment (pweCreatedAt envelope) bound capture message
                pure (PaypalEventPermanentFailure message
                  (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing, Nothing)
              Right () -> do
                reconciled <- reconcileLatePaypalCapture environment merchantRef now bound capture
                if reconciled
                  then pure (PaypalEventProcessed
                    (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing, Nothing)
                  else do
                    let checkout = Checkout.CheckoutReference (bpcCheckoutId bound)
                        attempt = Checkout.PaymentAttemptReference (bpcAttemptId bound)
                        correlationId = "paypal-webhook:" <> pweEventId envelope
                    binding <- Checkout.bindProviderResource Checkout.ProviderBindingCreation
                      { Checkout.pbcAttempt = attempt
                      , Checkout.pbcCheckout = checkout
                      , Checkout.pbcProvider = Checkout.ProviderPayPal
                      , Checkout.pbcEnvironment = environment
                      , Checkout.pbcMerchantRef = merchantRef
                      , Checkout.pbcResourceType = "capture"
                      , Checkout.pbcProviderResource = pwcCaptureId capture
                      , Checkout.pbcResourcePath = Just
                          ("/v2/checkout/orders/" <> pwcPaypalOrderId capture <> "/capture")
                      , Checkout.pbcOrderReference = bpcDomainOrderId bound
                      , Checkout.pbcAmountMinor = bpcExpectedAmount bound
                      , Checkout.pbcCurrency = bpcCurrency bound
                      , Checkout.pbcStage = Checkout.AttemptProcessing
                      , Checkout.pbcOccurredAt = pweCreatedAt envelope
                      , Checkout.pbcCorrelationId = correlationId
                      }
                    case binding of
                      Left message -> do
                        recordPaypalWebhookMismatch environment (pweCreatedAt envelope) bound capture message
                        pure (PaypalEventPermanentFailure message
                          (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing, Nothing)
                      Right () -> do
                        verified <- Checkout.recordVerifiedPayment Checkout.VerifiedPayment
                          { Checkout.vpAttempt = attempt
                          , Checkout.vpCheckout = checkout
                          , Checkout.vpProvider = Checkout.ProviderPayPal
                          , Checkout.vpEnvironment = environment
                          , Checkout.vpMerchantRef = merchantRef
                          , Checkout.vpResourceType = "capture"
                          , Checkout.vpProviderResource = pwcCaptureId capture
                          , Checkout.vpProviderResourcePath = Just
                              ("/v2/checkout/orders/" <> pwcPaypalOrderId capture <> "/capture")
                          , Checkout.vpOrderReference = bpcDomainOrderId bound
                          , Checkout.vpProviderReference = bpcDomainOrderId bound
                          , Checkout.vpAmountMinor = bpcExpectedAmount bound
                          , Checkout.vpCurrency = bpcCurrency bound
                          , Checkout.vpEvidence = "signature_verified_webhook"
                          , Checkout.vpOccurredAt = pweCreatedAt envelope
                          , Checkout.vpCorrelationId = correlationId
                          }
                        case verified of
                          Left message -> do
                            recordPaypalWebhookMismatch environment (pweCreatedAt envelope) bound capture message
                            pure (PaypalEventPermanentFailure message
                              (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing, Nothing)
                          Right newlyPaid -> do
                            when (newlyPaid && bpcDomainType bound == "mixing_mastering") $ do
                              rawExecute
                                "UPDATE service_storefront_order SET status = 'paid',\
                                \ paypal_capture_id = ?, paid_at = COALESCE(paid_at, ?), updated_at = ?\
                                \ WHERE id = ?::uuid AND checkout_id = ?::uuid\
                                \ AND paypal_order_id = ?"
                                [ PersistText (pwcCaptureId capture)
                                , PersistUTCTime (pweCreatedAt envelope)
                                , PersistUTCTime now
                                , PersistText (bpcDomainOrderId bound)
                                , PersistText (bpcCheckoutId bound)
                                , PersistText (pwcPaypalOrderId capture)
                                ]
                              rawExecute
                                "INSERT INTO service_storefront_order_status_change\
                                \ (order_id, status, notes, changed_by, created_at)\
                                \ VALUES (?::uuid, 'paid',\
                                \ 'PayPal capture completed through a signature-verified webhook',\
                                \ 'paypal_signature_verified_webhook', ?)"
                                [ PersistText (bpcDomainOrderId bound)
                                , PersistUTCTime now
                                ]
                            confirmation <- if bpcDomainType bound == "event_ticket_order"
                              then do
                                orderKey <- maybe
                                  (fail "Event ticket PayPal binding has an invalid order reference")
                                  pure
                                  (fromPathPiece (bpcDomainOrderId bound)
                                    :: Maybe SM.EventTicketOrderId)
                                (order, ticketCodes, newlyIssued) <-
                                  Issuance.finalizePaidTicketOrder now orderKey
                                pure $ if newlyIssued
                                  then Just (order, ticketCodes)
                                  else Nothing
                              else pure Nothing
                            pure (PaypalEventProcessed
                              (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing,
                              confirmation)
        case result of
          Left _ -> pure (PaypalEventRetry "PayPal capture event database processing failed")
          -- finalizePaidTicketOrder records a durable confirmation in the same
          -- transaction. A webhook retry must not invoke SMTP a second time.
          Right (outcome, _mConfirmation) -> pure outcome

    processExternalCaptureChange exceptionType =
      case parsePaypalExternalCaptureChange environment envelope of
        Left message -> pure (PaypalEventPermanentFailure message Nothing Nothing Nothing)
        Right (captureId, actualAmount, currency) -> do
          result <- tryAny $ flip runSqlPool pool $ do
            mBound <- loadBoundPaypalCapture environment merchantRef captureId
            case mBound of
              -- Retry exhaustion must not discard the admission fence. Persist
              -- verified capture-scoped evidence before requesting another retry;
              -- future binding/admission will join it without another webhook.
              Nothing -> do
                Checkout.recordUnmatchedCaptureException Checkout.ProviderPayPal
                  environment merchantRef exceptionType captureId actualAmount currency now
                pure (PaypalEventRetry "PayPal capture binding is not available yet")
              Just bound
                | currency /= bpcCurrency bound || actualAmount > bpcExpectedAmount bound ->
                    pure (PaypalEventPermanentFailure
                      "PayPal external change does not match the bound capture"
                      (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing)
                | otherwise -> do
                    -- Serialize the admission fence with scanning. Do not turn an
                    -- unallocated external refund into an invented ticket refund.
                    when (bpcDomainType bound == "event_ticket_order") $ do
                      lockedEvents <- (rawSql
                        "SELECT event.id FROM social_event event JOIN event_ticket_order orders\
                        \ ON orders.event_id=event.id WHERE orders.id::text=? FOR UPDATE OF event"
                        [PersistText (bpcDomainOrderId bound)] :: SqlPersistT IO [Single Int64])
                      lockedOrders <- (rawSql
                        "SELECT id FROM event_ticket_order WHERE id::text=? FOR UPDATE"
                        [PersistText (bpcDomainOrderId bound)] :: SqlPersistT IO [Single Int64])
                      unless (length lockedEvents == 1 && length lockedOrders == 1) $
                        fail "External ticket refund is missing its bound order"
                    known <- if bpcDomainType bound == "event_ticket_order" &&
                        pweEventType envelope == "PAYMENT.CAPTURE.REFUNDED" then do
                      let refundId = paypalWebhookResourceId envelope
                          customId = case pweResource envelope of
                            Object fields -> lookupObjectText "custom_id" fields >>= (fmap toText . fromText)
                            _ -> Nothing
                      refs <- rawSql
                        "SELECT refund.id::text FROM commerce_refund refund\
                        \ WHERE refund.checkout_id=?::uuid AND refund.payment_attempt_id=?::uuid\
                        \ AND refund.provider='paypal' AND refund.environment=? AND refund.merchant_account_ref=?\
                        \ AND refund.amount_minor=? AND refund.currency=? AND refund.status IN ('processing','succeeded')\
                        \ AND refund.approved_by>0 AND refund.approved_by<>refund.requested_by\
                        \ AND (refund.provider_refund_id=? OR (refund.provider_refund_id IS NULL AND refund.id::text=?))\
                        \ AND (?::text IS NULL OR refund.id::text=?)\
                        \ AND EXISTS (SELECT 1 FROM event_ticket_refund_allocation allocation WHERE allocation.refund_id=refund.id)"
                        [PersistText (bpcCheckoutId bound),PersistText (bpcAttemptId bound),
                         PersistText (Checkout.checkoutEnvironmentText environment),PersistText merchantRef,
                         PersistInt64 actualAmount,PersistText currency,toPersistValue refundId,toPersistValue customId,
                         toPersistValue customId,toPersistValue customId]
                      case (refs,refundId) of
                        ([Single ref],Just providerId) -> do
                          _ <- TicketRefund.completeTicketRefund Refund.VerifiedRefund
                            { Refund.vrRefund=Refund.RefundReference ref,Refund.vrProviderRefund=providerId
                            , Refund.vrAmountMinor=actualAmount,Refund.vrCurrency=currency
                            , Refund.vrOccurredAt=pweCreatedAt envelope
                            , Refund.vrCorrelationId="paypal-ticket-refund:" <> pweEventId envelope }
                          pure (Just ref)
                        _ -> pure Nothing
                    else pure Nothing
                    case known of
                      Just ref -> pure (PaypalEventProcessed
                        (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) (Just ref))
                      Nothing -> do
                        Checkout.recordReconciliationException
                          Checkout.ProviderPayPal environment merchantRef exceptionType
                          (bpcDomainOrderId bound) captureId
                          (bpcExpectedAmount bound) (Just actualAmount) currency now
                        pure (PaypalEventProcessed
                          (Just (bpcCheckoutId bound)) (Just (bpcAttemptId bound)) Nothing)

          pure $ case result of
            Left _ -> PaypalEventRetry "PayPal refund or reversal event database processing failed"
            Right outcome -> outcome

paypalWebhookDomainTypes :: [Text]
paypalWebhookDomainTypes =
  [ "mixing_mastering"
  , "marketplace_sale"
  , "marketplace_rental"
  , "service_booking"
  , "course_registration"
  , "event_ticket_order"
  ]

paypalWebhookDomainSupported :: Text -> Bool
paypalWebhookDomainSupported domainType = domainType `elem` paypalWebhookDomainTypes

paypalCaptureRequiresReconciliation
  :: UTCTime
  -> Text
  -> Maybe UTCTime
  -> Bool
paypalCaptureRequiresReconciliation now checkoutStatus expiresAt =
  checkoutStatus `notElem`
    ["holding", "awaiting_payment", "processing", "failed", "paid"]
  || (checkoutStatus /= "paid" && maybe False (<= now) expiresAt)

reconcileLatePaypalCapture
  :: Checkout.CheckoutEnvironment
  -> Text
  -> UTCTime
  -> BoundPaypalCapture
  -> PaypalWebhookCapture
  -> SqlPersistT IO Bool
reconcileLatePaypalCapture environment merchantRef now bound capture = do
  rows <- (rawSql
    "SELECT status, expires_at FROM commerce_checkout_session\
    \ WHERE id = ?::uuid FOR UPDATE"
    [PersistText (bpcCheckoutId bound)]
    :: SqlPersistT IO [(Single Text, Single (Maybe UTCTime))])
  (checkoutStatus, expiresAt) <- case rows of
    [(Single status, Single deadline)] -> pure (status, deadline)
    _ -> fail "PayPal capture binding has no unique canonical checkout"
  if not (paypalCaptureRequiresReconciliation now checkoutStatus expiresAt)
    then pure False
    else do
      let holdExpired = checkoutStatus /= "paid" && maybe False (<= now) expiresAt
          exceptionType
            | holdExpired && bpcDomainType bound == "event_ticket_order" =
                "payment_after_ticket_hold_expiry"
            | holdExpired && bpcDomainType bound == "course_registration" =
                "payment_after_course_hold_expiry"
            | holdExpired = "payment_after_checkout_expiry"
            | otherwise = "payment_after_terminal_checkout"
      when holdExpired $ rawExecute
        "UPDATE commerce_checkout_session SET status = 'expired', updated_at = ?\
        \ WHERE id = ?::uuid\
        \ AND status IN ('holding','awaiting_payment','processing','failed')"
        [PersistUTCTime now, PersistText (bpcCheckoutId bound)]
      Checkout.recordReconciliationException
        Checkout.ProviderPayPal environment merchantRef exceptionType
        (bpcDomainOrderId bound) (pwcCaptureId capture)
        (bpcExpectedAmount bound) (Just (bpcExpectedAmount bound))
        (bpcCurrency bound) now
      pure True

loadBoundPaypalOrder
  :: Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> SqlPersistT IO (Maybe BoundPaypalCapture)
loadBoundPaypalOrder environment merchantRef paypalOrderId =
  loadBoundPaypalCaptureBy
    "binding.resource_type = 'order' AND binding.provider_resource_id = ?"
    environment merchantRef paypalOrderId

loadBoundPaypalCapture
  :: Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> SqlPersistT IO (Maybe BoundPaypalCapture)
loadBoundPaypalCapture environment merchantRef captureId =
  loadBoundPaypalCaptureBy
    "binding.resource_type = 'capture' AND binding.provider_resource_id = ?"
    environment merchantRef captureId

loadBoundPaypalCaptureBy
  :: Text
  -> Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> SqlPersistT IO (Maybe BoundPaypalCapture)
loadBoundPaypalCaptureBy bindingPredicate environment merchantRef providerResource = do
  let domainPlaceholders = T.intercalate "," (replicate (length paypalWebhookDomainTypes) "?")
  rows <- (rawSql
    ("SELECT checkout.id::text, attempt.id::text, checkout.domain_type, checkout.domain_order_id,\
     \ checkout.total_minor, checkout.currency, attempt.merchant_account_ref,\
     \ order_binding.provider_resource_id\
     \ FROM commerce_checkout_session checkout\
     \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id = checkout.id\
     \ JOIN commerce_provider_binding binding ON binding.payment_attempt_id = attempt.id\
     \ JOIN commerce_provider_binding order_binding\
     \   ON order_binding.provider = attempt.provider\
     \  AND order_binding.environment = attempt.environment\
     \  AND order_binding.merchant_account_ref = attempt.merchant_account_ref\
     \  AND order_binding.merchant_reference = checkout.domain_order_id\
     \  AND order_binding.resource_type = 'order'\
     \ WHERE checkout.domain_type IN (" <> domainPlaceholders <> ")\
     \ AND attempt.provider = 'paypal' AND attempt.environment = ?\
     \ AND attempt.merchant_account_ref = ? AND " <> bindingPredicate)
    (map PersistText paypalWebhookDomainTypes <>
      [ PersistText (Checkout.checkoutEnvironmentText environment)
      , PersistText merchantRef
      , PersistText providerResource
      ]) :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Int64
       , Single Text, Single Text, Single Text
       )])
  pure $ case rows of
    [( Single checkoutId, Single attemptId, Single domainType, Single domainOrderId, Single amount
     , Single currency, Single storedMerchant, Single paypalOrderId
     )] -> Just BoundPaypalCapture
        { bpcCheckoutId = checkoutId
        , bpcAttemptId = attemptId
        , bpcDomainType = domainType
        , bpcDomainOrderId = domainOrderId
        , bpcExpectedAmount = amount
        , bpcCurrency = currency
        , bpcMerchantRef = storedMerchant
        , bpcPaypalOrderId = paypalOrderId
        }
    _ -> Nothing

validatePaypalWebhookCaptureBinding
  :: Text
  -> BoundPaypalCapture
  -> PaypalWebhookCapture
  -> Either Text ()
validatePaypalWebhookCaptureBinding configuredMerchant bound capture = do
  capturedMinor <- fromIntegral <$> parseDatafastCents (pwcAmount capture)
  unless (pwcStatus capture == "COMPLETED") $
    Left "PayPal webhook capture is not completed"
  unless (pwcPaypalOrderId capture == bpcPaypalOrderId bound) $
    Left "PayPal webhook order ID does not match the immutable provider binding"
  unless (pwcMerchantId capture == configuredMerchant
      && pwcMerchantId capture == bpcMerchantRef bound) $
    Left "PayPal webhook merchant does not match the configured merchant binding"
  unless (capturedMinor == bpcExpectedAmount bound) $
    Left "PayPal webhook amount does not match the immutable checkout"
  unless (pwcCurrency capture == T.toUpper (T.strip (bpcCurrency bound))) $
    Left "PayPal webhook currency does not match the immutable checkout"

recordPaypalWebhookMismatch
  :: Checkout.CheckoutEnvironment
  -> UTCTime
  -> BoundPaypalCapture
  -> PaypalWebhookCapture
  -> Text
  -> SqlPersistT IO ()
recordPaypalWebhookMismatch environment occurredAt bound capture _ =
  Checkout.recordReconciliationException
    Checkout.ProviderPayPal
    environment
    (bpcMerchantRef bound)
    "provider_verification_mismatch"
    (bpcDomainOrderId bound)
    (pwcCaptureId capture)
    (bpcExpectedAmount bound)
    (fromIntegral <$> either (const Nothing) Just (parseDatafastCents (pwcAmount capture)))
    (bpcCurrency bound)
    occurredAt

parseDatafastCents :: Text -> Either Text Int
parseDatafastCents raw =
  case T.splitOn "." (T.strip raw) of
    [whole] -> parseParts whole "0"
    [whole, fraction] | T.length fraction <= 2 -> parseParts whole fraction
    _ -> Left "Datafast amount is invalid"
  where
    parseParts whole fraction
      | T.null whole || T.null fraction = Left "Datafast amount is invalid"
      | not (T.all isDigit whole && T.all isDigit fraction) = Left "Datafast amount is invalid"
      | otherwise =
          let wholeValue = read (T.unpack whole) :: Integer
              fractionValue = read (T.unpack (T.take 2 (fraction <> "00"))) :: Integer
              cents = wholeValue * 100 + fractionValue
          in if cents > 0 && cents <= fromIntegral (maxBound :: Int)
               then Right (fromIntegral cents)
               else Left "Datafast amount is invalid"

data PaypalWebhookEnvelope = PaypalWebhookEnvelope
  { pweEventId   :: Text
  , pweEventType :: Text
  , pweCreatedAt :: UTCTime
  , pweResource  :: Value
  } deriving (Show)

data PaypalWebhookCapture = PaypalWebhookCapture
  { pwcCaptureId    :: Text
  , pwcStatus       :: Text
  , pwcAmount       :: Text
  , pwcCurrency     :: Text
  , pwcMerchantId   :: Text
  , pwcPaypalOrderId :: Text
  } deriving (Eq, Show)

data PaypalEventProcessResult
  = PaypalEventProcessed (Maybe Text) (Maybe Text) (Maybe Text)
  | PaypalEventIgnored
  | PaypalEventPermanentFailure Text (Maybe Text) (Maybe Text) (Maybe Text)
  | PaypalEventRetry Text
  deriving (Eq, Show)

data BoundPaypalCapture = BoundPaypalCapture
  { bpcCheckoutId     :: Text
  , bpcAttemptId      :: Text
  , bpcDomainType     :: Text
  , bpcDomainOrderId  :: Text
  , bpcExpectedAmount :: Int64
  , bpcCurrency       :: Text
  , bpcMerchantRef    :: Text
  , bpcPaypalOrderId  :: Text
  }

parsePaypalWebhookEnvelope :: BL.ByteString -> Either Text PaypalWebhookEnvelope
parsePaypalWebhookEnvelope rawBody
  | BL.null rawBody = Left "PayPal webhook body is empty"
  | BL.length rawBody > 1024 * 1024 = Left "PayPal webhook body exceeds 1048576 bytes"
  | otherwise = do
      value <- either (const (Left "Invalid PayPal webhook JSON")) Right
        (eitherDecode rawBody :: Either String Value)
      case value of
        Object obj -> do
          eventId <- requiredObjectText "id" obj
          eventType <- requiredObjectText "event_type" obj
          createTime <- requiredObjectText "create_time" obj
          createdAt <- parseAesonUtc "PayPal event create_time" createTime
          resource <- maybe (Left "PayPal webhook omitted resource") Right
            (KM.lookup "resource" obj)
          unless (isProviderReference eventId && T.length eventId <= 128) $
            Left "PayPal webhook event ID is invalid"
          unless (validPaypalEventType eventType) $
            Left "PayPal webhook event type is invalid"
          pure PaypalWebhookEnvelope
            { pweEventId = eventId
            , pweEventType = eventType
            , pweCreatedAt = createdAt
            , pweResource = resource
            }
        _ -> Left "PayPal webhook must be a JSON object"

parsePaypalWebhookCapture :: PaypalWebhookEnvelope -> Either Text PaypalWebhookCapture
parsePaypalWebhookCapture PaypalWebhookEnvelope{pweResource = Object resource} = do
  captureId <- requiredObjectText "id" resource
  status <- requiredObjectText "status" resource
  amountObject <- maybe (Left "PayPal webhook omitted capture amount") Right
    (lookupObject "amount" resource)
  amount <- requiredObjectText "value" amountObject
  currency <- requiredObjectText "currency_code" amountObject
  payee <- maybe (Left "PayPal webhook omitted capture payee") Right
    (lookupObject "payee" resource)
  merchantId <- requiredObjectText "merchant_id" payee
  supplementary <- maybe (Left "PayPal webhook omitted supplementary_data") Right
    (lookupObject "supplementary_data" resource)
  related <- maybe (Left "PayPal webhook omitted related_ids") Right
    (lookupObject "related_ids" supplementary)
  paypalOrderId <- requiredObjectText "order_id" related
  unless (all isProviderReference [captureId, merchantId, paypalOrderId]) $
    Left "PayPal webhook contains an invalid provider reference"
  pure PaypalWebhookCapture
    { pwcCaptureId = captureId
    , pwcStatus = T.toUpper (T.strip status)
    , pwcAmount = T.strip amount
    , pwcCurrency = T.toUpper (T.strip currency)
    , pwcMerchantId = merchantId
    , pwcPaypalOrderId = paypalOrderId
    }
parsePaypalWebhookCapture _ = Left "PayPal webhook capture resource must be an object"

-- Signature verification and merchant binding remain the caller's authority.
parsePaypalExternalCaptureChange
  :: Checkout.CheckoutEnvironment -> PaypalWebhookEnvelope
  -> Either Text (Text, Int64, Text)
parsePaypalExternalCaptureChange environment PaypalWebhookEnvelope{pweEventType, pweResource} =
  RefundQuery.parseExternalCaptureChange environment pweEventType pweResource


paypalWebhookResourceId :: PaypalWebhookEnvelope -> Maybe Text
paypalWebhookResourceId PaypalWebhookEnvelope{pweResource = Object resource} =
  lookupObjectText "id" resource
paypalWebhookResourceId _ = Nothing

requiredObjectText :: Text -> Aeson.Object -> Either Text Text
requiredObjectText key obj =
  case lookupObjectText key obj of
    Just value | not (T.null (T.strip value)) -> Right (T.strip value)
    _ -> Left ("PayPal payload omitted " <> key)

parseAesonUtc :: Text -> Text -> Either Text UTCTime
parseAesonUtc label rawTimestamp =
  case Aeson.fromJSON (String (T.strip rawTimestamp)) of
    Success value -> Right value
    Error _ -> Left (label <> " is not a valid RFC 3339 timestamp")

validPaypalEventType :: Text -> Bool
validPaypalEventType value =
  not (T.null value)
    && T.length value <= 100
    && T.all (\character ->
      (character >= 'A' && character <= 'Z')
        || (character >= '0' && character <= '9')
        || character `elem` ("._-" :: String)) value

lookupObject :: Text -> Aeson.Object -> Maybe Aeson.Object
lookupObject key obj = case KM.lookup (AesonKey.fromText key) obj of
  Just (Object value) -> Just value
  _ -> Nothing

lookupObjectText :: Text -> Aeson.Object -> Maybe Text
lookupObjectText key obj = case KM.lookup (AesonKey.fromText key) obj of
  Just (String value) -> Just value
  _ -> Nothing

-- | Opaque provider identifiers: bounded ASCII letters, digits and "-_.".
isProviderReference :: Text -> Bool
isProviderReference value =
  not (T.null value)
    && T.length value <= 256
    && T.any (\c -> isAsciiLower c || isAsciiUpper c || isDigit c) value
    && T.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_." :: String)) value
