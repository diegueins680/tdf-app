{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.Commerce.PaymentIntentStore
  ( CaptureMethod(..)
  , PaymentIntentReference(..)
  , PaymentIntentCreation(..)
  , PaymentIntentStart(..)
  , createPaymentIntent
  , bindPaymentAttemptToIntent
  , transitionPaymentIntent
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isAscii, isPrint)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql)

import           TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment, CheckoutReference(..), PaymentAttemptReference(..)
  , PaymentProvider, checkoutEnvironmentText, paymentProviderText )
import           TDF.Commerce.ProviderCapabilities
  ( PaymentMethod, paymentMethodText )
import           TDF.Commerce.StateMachine
  ( PaymentEvent, PaymentLifecycle(..), PaymentState(..), transitionPayment )

data CaptureMethod = CaptureAutomatic | CaptureManual
  deriving (Eq, Show)

newtype PaymentIntentReference = PaymentIntentReference
  { paymentIntentReferenceId :: Text
  } deriving (Eq, Show)

data PaymentIntentCreation = PaymentIntentCreation
  { picCheckout       :: CheckoutReference
  , picEnvironment    :: CheckoutEnvironment
  , picProvider       :: PaymentProvider
  , picPaymentMethod  :: PaymentMethod
  , picCaptureMethod  :: CaptureMethod
  , picAmountMinor    :: Int64
  , picCurrency       :: Text
  , picIdempotencyKey :: Text
  , picOccurredAt     :: UTCTime
  , picCorrelationId  :: Text
  }

data PaymentIntentStart = PaymentIntentStart
  { pisReference :: PaymentIntentReference
  , pisLifecycle :: PaymentLifecycle
  , pisCreated   :: Bool
  } deriving (Eq, Show)

createPaymentIntent
  :: PaymentIntentCreation
  -> SqlPersistT IO (Either Text PaymentIntentStart)
createPaymentIntent creation@PaymentIntentCreation{..}
  | picAmountMinor <= 0 = pure (Left "Payment intent amount must be positive")
  | normalizedCurrency picCurrency /= "USD" =
      pure (Left "Payment intent settlement currency must be USD")
  | not (validIdempotencyKey picIdempotencyKey) =
      pure (Left "Payment intent idempotency key is invalid")
  | not (validCorrelationId picCorrelationId) =
      pure (Left "Payment intent correlation ID is invalid")
  | otherwise = do
      checkoutRows <- rawSql
        "SELECT status, environment, total_minor, currency, expires_at > ?\
        \ FROM commerce_checkout_session WHERE id = ?::uuid FOR UPDATE"
        [ PersistUTCTime picOccurredAt
        , PersistText (checkoutReferenceId picCheckout)
        ] :: SqlPersistT IO
          [(Single Text, Single Text, Single Int64, Single Text, Single Bool)]
      case checkoutRows of
        [] -> pure (Left "Canonical checkout was not found")
        [(Single checkoutStatus, Single environment, Single amountMinor,
          Single currency, Single unexpired)]
          | checkoutStatus `notElem` ["awaiting_payment", "processing", "failed"] ->
              pure (Left "Canonical checkout cannot accept a payment intent")
          | not unexpired -> pure (Left "Canonical checkout has expired")
          | environment /= checkoutEnvironmentText picEnvironment ->
              pure (Left "Payment intent environment does not match the checkout")
          | amountMinor /= picAmountMinor ->
              pure (Left "Payment intent amount does not match the checkout")
          | normalizedCurrency currency /= normalizedCurrency picCurrency ->
              pure (Left "Payment intent currency does not match the checkout")
          | otherwise -> createOrLoad creation
        _ -> pure (Left "Canonical checkout lookup was ambiguous")

createOrLoad
  :: PaymentIntentCreation
  -> SqlPersistT IO (Either Text PaymentIntentStart)
createOrLoad creation@PaymentIntentCreation{..} = do
  existing <- loadByIdempotency creation
  case existing of
    Left problem -> pure (Left problem)
    Right (Just started) -> pure (Right started)
    Right Nothing -> do
      active <- rawSql
        "SELECT id::text FROM commerce_payment_intent\
        \ WHERE checkout_id = ?::uuid\
        \ AND status NOT IN ('voided','failed','cancelled','refunded','chargeback')\
        \ FOR UPDATE"
        [PersistText (checkoutReferenceId picCheckout)]
        :: SqlPersistT IO [Single Text]
      if not (null active)
        then pure (Left "Checkout already has a different active payment intent")
        else insertIntent creation

loadByIdempotency
  :: PaymentIntentCreation
  -> SqlPersistT IO (Either Text (Maybe PaymentIntentStart))
loadByIdempotency PaymentIntentCreation{..} = do
  rows <- rawSql
    "SELECT id::text, status, authorized_minor, captured_minor, refunded_minor,\
    \ capture_method, provider, payment_method, amount_minor, currency\
    \ FROM commerce_payment_intent\
    \ WHERE checkout_id = ?::uuid AND idempotency_key = ? FOR UPDATE"
    [ PersistText (checkoutReferenceId picCheckout)
    , PersistText picIdempotencyKey
    ] :: SqlPersistT IO
      [(Single Text, Single Text, Single Int64, Single Int64, Single Int64,
        Single Text, Single Text, Single Text, Single Int64, Single Text)]
  pure $ case rows of
    [] -> Right Nothing
    [(Single intentId, Single status, Single authorizedMinor,
      Single capturedMinor, Single refundedMinor, Single captureMethod,
      Single provider, Single paymentMethod, Single amountMinor, Single currency)]
        | captureMethod /= captureMethodText picCaptureMethod
          || provider /= paymentProviderText picProvider
          || paymentMethod /= paymentMethodText picPaymentMethod
          || amountMinor /= picAmountMinor
          || normalizedCurrency currency /= normalizedCurrency picCurrency ->
            Left "Payment intent idempotency key conflicts with immutable fields"
        | otherwise -> do
            parsedState <- paymentStateFromText status
            Right (Just PaymentIntentStart
              { pisReference = PaymentIntentReference intentId
              , pisLifecycle = PaymentLifecycle
                  { paymentState = parsedState
                  , paymentAmountMinor = amountMinor
                  , paymentAuthorizedMinor = authorizedMinor
                  , paymentCapturedMinor = capturedMinor
                  , paymentRefundedMinor = refundedMinor
                  }
              , pisCreated = False
              })
    _ -> Left "Payment intent idempotency lookup was ambiguous"

insertIntent
  :: PaymentIntentCreation
  -> SqlPersistT IO (Either Text PaymentIntentStart)
insertIntent PaymentIntentCreation{..} = do
  intentId <- liftIO (toText <$> nextRandom)
  rawExecute
    "INSERT INTO commerce_payment_intent (\
    \ id, checkout_id, status, capture_method, provider, payment_method,\
    \ amount_minor, currency, idempotency_key, created_at, updated_at\
    \) VALUES (?::uuid, ?::uuid, 'requires_payment_method', ?, ?, ?, ?, ?, ?, ?, ?)"
    [ PersistText intentId
    , PersistText (checkoutReferenceId picCheckout)
    , PersistText (captureMethodText picCaptureMethod)
    , PersistText (paymentProviderText picProvider)
    , PersistText (paymentMethodText picPaymentMethod)
    , PersistInt64 picAmountMinor
    , PersistText (normalizedCurrency picCurrency)
    , PersistText picIdempotencyKey
    , PersistUTCTime picOccurredAt
    , PersistUTCTime picOccurredAt
    ]
  rawExecute
    "INSERT INTO commerce_payment_amount_component (\
    \ payment_intent_id, component_type, amount_minor, currency, source, occurred_at\
    \) VALUES (?::uuid, 'subtotal', ?, ?, 'quote', ?)"
    [ PersistText intentId
    , PersistInt64 picAmountMinor
    , PersistText (normalizedCurrency picCurrency)
    , PersistUTCTime picOccurredAt
    ]
  insertHistory
    (PaymentIntentReference intentId)
    Nothing
    PaymentRequiresMethod
    "payment_intent_created"
    "customer"
    picCorrelationId
    picOccurredAt
  pure (Right PaymentIntentStart
    { pisReference = PaymentIntentReference intentId
    , pisLifecycle = PaymentLifecycle PaymentRequiresMethod picAmountMinor 0 0 0
    , pisCreated = True
    })

bindPaymentAttemptToIntent
  :: PaymentIntentReference
  -> PaymentAttemptReference
  -> SqlPersistT IO (Either Text ())
bindPaymentAttemptToIntent intent attempt = do
  updated <- rawSql
    "UPDATE commerce_payment_attempt attempt\
    \ SET payment_intent_id = intent.id\
    \ FROM commerce_payment_intent intent\
    \ WHERE attempt.id = ?::uuid AND intent.id = ?::uuid\
    \ AND attempt.checkout_id = intent.checkout_id\
    \ AND attempt.provider = intent.provider\
    \ AND attempt.amount_minor = intent.amount_minor\
    \ AND attempt.currency = intent.currency\
    \ AND (attempt.payment_intent_id IS NULL OR attempt.payment_intent_id = intent.id)\
    \ RETURNING attempt.id::text"
    [ PersistText (paymentAttemptReferenceId attempt)
    , PersistText (paymentIntentReferenceId intent)
    ] :: SqlPersistT IO [Single Text]
  pure $ case updated of
    [_] -> Right ()
    [] -> Left "Payment attempt does not match the canonical intent"
    _ -> Left "Payment attempt binding was ambiguous"

transitionPaymentIntent
  :: PaymentIntentReference
  -> PaymentEvent
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text PaymentLifecycle)
transitionPaymentIntent intent event actorType correlationId occurredAt
  | actorType `notElem` ["system", "customer", "provider"] =
      pure (Left "Payment transition actor type is invalid")
  | not (validCorrelationId correlationId) =
      pure (Left "Payment transition correlation ID is invalid")
  | otherwise = do
      current <- loadLifecycleForUpdate intent
      case current of
        Left problem -> pure (Left problem)
        Right lifecycle -> case transitionPayment lifecycle event of
          Left problem -> pure (Left problem)
          Right next -> do
            rawExecute
              "UPDATE commerce_payment_intent SET status = ?, authorized_minor = ?,\
              \ captured_minor = ?, refunded_minor = ?, updated_at = ?\
              \ WHERE id = ?::uuid AND status = ?"
              [ PersistText (paymentStateText (paymentState next))
              , PersistInt64 (paymentAuthorizedMinor next)
              , PersistInt64 (paymentCapturedMinor next)
              , PersistInt64 (paymentRefundedMinor next)
              , PersistUTCTime occurredAt
              , PersistText (paymentIntentReferenceId intent)
              , PersistText (paymentStateText (paymentState lifecycle))
              ]
            insertHistory intent (Just (paymentState lifecycle))
              (paymentState next) (paymentEventText event) actorType
              correlationId occurredAt
            pure (Right next)

loadLifecycleForUpdate
  :: PaymentIntentReference
  -> SqlPersistT IO (Either Text PaymentLifecycle)
loadLifecycleForUpdate intent = do
  rows <- rawSql
    "SELECT status, amount_minor, authorized_minor, captured_minor, refunded_minor\
    \ FROM commerce_payment_intent WHERE id = ?::uuid FOR UPDATE"
    [PersistText (paymentIntentReferenceId intent)]
    :: SqlPersistT IO
      [(Single Text, Single Int64, Single Int64, Single Int64, Single Int64)]
  pure $ case rows of
    [(Single status, Single amountMinor, Single authorizedMinor,
      Single capturedMinor, Single refundedMinor)] -> do
        parsedState <- paymentStateFromText status
        Right PaymentLifecycle
          { paymentState = parsedState
          , paymentAmountMinor = amountMinor
          , paymentAuthorizedMinor = authorizedMinor
          , paymentCapturedMinor = capturedMinor
          , paymentRefundedMinor = refundedMinor
          }
    [] -> Left "Payment intent was not found"
    _ -> Left "Payment intent lookup was ambiguous"

insertHistory
  :: PaymentIntentReference
  -> Maybe PaymentState
  -> PaymentState
  -> Text
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
insertHistory intent fromState toState eventType actorType correlationId occurredAt = do
  historyId <- liftIO (toText <$> nextRandom)
  rawExecute
    "INSERT INTO commerce_payment_state_history (\
    \ id, payment_intent_id, from_status, to_status, event_type, actor_type,\
    \ correlation_id, occurred_at\
    \) VALUES (?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?)"
    [ PersistText historyId
    , PersistText (paymentIntentReferenceId intent)
    , maybe PersistNull (PersistText . paymentStateText) fromState
    , PersistText (paymentStateText toState)
    , PersistText eventType
    , PersistText actorType
    , PersistText correlationId
    , PersistUTCTime occurredAt
    ]

captureMethodText :: CaptureMethod -> Text
captureMethodText CaptureAutomatic = "automatic"
captureMethodText CaptureManual = "manual"

paymentStateText :: PaymentState -> Text
paymentStateText state = case state of
  PaymentRequiresMethod -> "requires_payment_method"
  PaymentRequiresCustomerAction -> "requires_customer_action"
  PaymentProcessing -> "processing"
  PaymentAuthorized -> "authorized"
  PaymentPartiallyCaptured -> "partially_captured"
  PaymentCaptured -> "captured"
  PaymentVoided -> "voided"
  PaymentFailed -> "failed"
  PaymentCancelled -> "cancelled"
  PaymentPartiallyRefunded -> "partially_refunded"
  PaymentRefunded -> "refunded"
  PaymentDisputed -> "disputed"
  PaymentChargeback -> "chargeback"

paymentStateFromText :: Text -> Either Text PaymentState
paymentStateFromText value = case value of
  "requires_payment_method" -> Right PaymentRequiresMethod
  "requires_customer_action" -> Right PaymentRequiresCustomerAction
  "processing" -> Right PaymentProcessing
  "authorized" -> Right PaymentAuthorized
  "partially_captured" -> Right PaymentPartiallyCaptured
  "captured" -> Right PaymentCaptured
  "voided" -> Right PaymentVoided
  "failed" -> Right PaymentFailed
  "cancelled" -> Right PaymentCancelled
  "partially_refunded" -> Right PaymentPartiallyRefunded
  "refunded" -> Right PaymentRefunded
  "disputed" -> Right PaymentDisputed
  "chargeback" -> Right PaymentChargeback
  _ -> Left "Stored payment intent state is invalid"

paymentEventText :: PaymentEvent -> Text
paymentEventText = T.pack . show

normalizedCurrency :: Text -> Text
normalizedCurrency = T.toUpper . T.strip

validIdempotencyKey :: Text -> Bool
validIdempotencyKey value =
  T.length value >= 16
    && T.length value <= 128
    && T.all (\character -> isAscii character && isPrint character && character > ' ') value

validCorrelationId :: Text -> Bool
validCorrelationId value =
  not (T.null value)
    && T.length value <= 256
    && T.all (\character -> isAscii character && isPrint character) value
