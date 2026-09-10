{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.Commerce.StateMachine
  ( CheckoutState(..)
  , CheckoutEvent(..)
  , PaymentState(..)
  , PaymentLifecycle(..)
  , PaymentEvent(..)
  , ProviderEnvironment(..)
  , VerificationEvidence(..)
  , PaymentVerification(..)
  , transitionCheckout
  , transitionPayment
  , verifyPaymentBinding
  , ledgerBalances
  ) where

import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T

data CheckoutState
  = CheckoutDraft
  | CheckoutValidated
  | CheckoutHolding
  | CheckoutAwaitingPayment
  | CheckoutProcessing
  | CheckoutPaid
  | CheckoutFailed
  | CheckoutCancelled
  | CheckoutExpired
  | CheckoutPartiallyRefunded
  | CheckoutRefunded
  | CheckoutDisputed
  | CheckoutChargeback
  deriving (Eq, Ord, Show, Enum, Bounded)

data ProviderEnvironment = ProviderSandbox | ProviderProduction
  deriving (Eq, Ord, Show)

data VerificationEvidence
  = SignatureVerifiedWebhook
  | ServerToServerVerification
  | MockedEvidence
  | BrowserReturnOnly
  deriving (Eq, Ord, Show)

data PaymentVerification = PaymentVerification
  { pvCheckoutEnvironment :: ProviderEnvironment
  , pvEventEnvironment    :: ProviderEnvironment
  , pvEvidence            :: VerificationEvidence
  , pvExpectedAmountMinor :: Int64
  , pvActualAmountMinor   :: Int64
  , pvExpectedCurrency    :: Text
  , pvActualCurrency      :: Text
  , pvExpectedMerchant    :: Text
  , pvActualMerchant      :: Text
  , pvExpectedOrder       :: Text
  , pvActualOrder         :: Text
  , pvExpectedResource    :: Text
  , pvActualResource      :: Text
  } deriving (Eq, Show)

data CheckoutEvent
  = CheckoutValidationPassed
  | CheckoutHoldAcquired
  | CheckoutPaymentRequested
  | CheckoutProviderProcessing
  | CheckoutPaymentVerified PaymentVerification
  | CheckoutPaymentFailed
  | CheckoutCancelRequested
  | CheckoutExpiryObserved
  | CheckoutPartialRefundSucceeded
  | CheckoutFullRefundSucceeded
  | CheckoutDisputeOpened
  | CheckoutChargebackConfirmed
  deriving (Eq, Show)

-- | The payment lifecycle is separate from the checkout lifecycle. An
-- authorization is not a paid checkout and a void is not a refund. Amounts
-- are always represented in integer minor units.
data PaymentState
  = PaymentRequiresMethod
  | PaymentRequiresCustomerAction
  | PaymentProcessing
  | PaymentAuthorized
  | PaymentPartiallyCaptured
  | PaymentCaptured
  | PaymentVoided
  | PaymentFailed
  | PaymentCancelled
  | PaymentPartiallyRefunded
  | PaymentRefunded
  | PaymentDisputed
  | PaymentChargeback
  deriving (Eq, Ord, Show, Enum, Bounded)

data PaymentLifecycle = PaymentLifecycle
  { paymentState           :: PaymentState
  , paymentAmountMinor     :: Int64
  , paymentAuthorizedMinor :: Int64
  , paymentCapturedMinor   :: Int64
  , paymentRefundedMinor   :: Int64
  } deriving (Eq, Show)

data PaymentEvent
  = PaymentCustomerActionRequired
  | PaymentProcessingObserved
  | PaymentAuthorizationVerified Int64
  | PaymentCaptureVerified Int64
  | PaymentVoidVerified Int64
  | PaymentFailureConfirmed
  | PaymentCancellationRequested
  | PaymentRefundVerified Int64
  | PaymentDisputeObserved
  | PaymentChargebackObserved
  deriving (Eq, Show)

transitionCheckout :: CheckoutState -> CheckoutEvent -> Either Text CheckoutState
transitionCheckout current event = case (current, event) of
  (CheckoutDraft, CheckoutValidationPassed) -> Right CheckoutValidated
  (CheckoutValidated, CheckoutHoldAcquired) -> Right CheckoutHolding
  (CheckoutHolding, CheckoutPaymentRequested) -> Right CheckoutAwaitingPayment
  (CheckoutAwaitingPayment, CheckoutProviderProcessing) -> Right CheckoutProcessing
  (CheckoutAwaitingPayment, CheckoutPaymentVerified verification) -> CheckoutPaid <$ verifyPaymentBinding verification
  (CheckoutProcessing, CheckoutPaymentVerified verification) -> CheckoutPaid <$ verifyPaymentBinding verification
  (CheckoutAwaitingPayment, CheckoutPaymentFailed) -> Right CheckoutFailed
  (CheckoutProcessing, CheckoutPaymentFailed) -> Right CheckoutFailed
  (CheckoutFailed, CheckoutPaymentRequested) -> Right CheckoutAwaitingPayment
  (CheckoutHolding, CheckoutCancelRequested) -> Right CheckoutCancelled
  (CheckoutAwaitingPayment, CheckoutCancelRequested) -> Right CheckoutCancelled
  (CheckoutValidated, CheckoutCancelRequested) -> Right CheckoutCancelled
  (CheckoutHolding, CheckoutExpiryObserved) -> Right CheckoutExpired
  (CheckoutAwaitingPayment, CheckoutExpiryObserved) -> Right CheckoutExpired
  (CheckoutProcessing, CheckoutExpiryObserved) -> Right CheckoutExpired
  (CheckoutPaid, CheckoutPartialRefundSucceeded) -> Right CheckoutPartiallyRefunded
  (CheckoutPaid, CheckoutFullRefundSucceeded) -> Right CheckoutRefunded
  (CheckoutPartiallyRefunded, CheckoutPartialRefundSucceeded) -> Right CheckoutPartiallyRefunded
  (CheckoutPartiallyRefunded, CheckoutFullRefundSucceeded) -> Right CheckoutRefunded
  (CheckoutPaid, CheckoutDisputeOpened) -> Right CheckoutDisputed
  (CheckoutPartiallyRefunded, CheckoutDisputeOpened) -> Right CheckoutDisputed
  (CheckoutDisputed, CheckoutChargebackConfirmed) -> Right CheckoutChargeback
  _ -> Left ("Invalid checkout transition from " <> T.pack (show current) <> " using " <> eventName event)

transitionPayment
  :: PaymentLifecycle
  -> PaymentEvent
  -> Either Text PaymentLifecycle
transitionPayment lifecycle event
  | paymentAmountMinor lifecycle <= 0 = Left "Payment amount must be positive"
  | any (< 0)
      [ paymentAuthorizedMinor lifecycle
      , paymentCapturedMinor lifecycle
      , paymentRefundedMinor lifecycle
      ] = Left "Payment lifecycle amounts cannot be negative"
  | paymentAuthorizedMinor lifecycle > paymentAmountMinor lifecycle =
      Left "Authorized amount exceeds the payment amount"
  | paymentCapturedMinor lifecycle > paymentAmountMinor lifecycle =
      Left "Captured amount exceeds the payment amount"
  | paymentRefundedMinor lifecycle > paymentCapturedMinor lifecycle =
      Left "Refunded amount exceeds the captured amount"
  | otherwise = applyPaymentEvent lifecycle event

applyPaymentEvent
  :: PaymentLifecycle
  -> PaymentEvent
  -> Either Text PaymentLifecycle
applyPaymentEvent lifecycle event = case (paymentState lifecycle, event) of
  (PaymentRequiresMethod, PaymentCustomerActionRequired) ->
    Right lifecycle { paymentState = PaymentRequiresCustomerAction }
  (PaymentRequiresMethod, PaymentProcessingObserved) ->
    Right lifecycle { paymentState = PaymentProcessing }
  (PaymentRequiresCustomerAction, PaymentProcessingObserved) ->
    Right lifecycle { paymentState = PaymentProcessing }
  (PaymentProcessing, PaymentAuthorizationVerified amount) ->
    authorize lifecycle amount
  (PaymentRequiresCustomerAction, PaymentAuthorizationVerified amount) ->
    authorize lifecycle amount
  (PaymentRequiresMethod, PaymentAuthorizationVerified amount) ->
    authorize lifecycle amount
  (PaymentProcessing, PaymentCaptureVerified amount) ->
    capture lifecycle amount
  (PaymentRequiresCustomerAction, PaymentCaptureVerified amount) ->
    capture lifecycle amount
  (PaymentRequiresMethod, PaymentCaptureVerified amount) ->
    capture lifecycle amount
  (PaymentAuthorized, PaymentCaptureVerified amount) ->
    capture lifecycle amount
  (PaymentPartiallyCaptured, PaymentCaptureVerified amount) ->
    capture lifecycle amount
  (PaymentAuthorized, PaymentVoidVerified amount) ->
    voidAuthorization lifecycle amount
  (PaymentPartiallyCaptured, PaymentVoidVerified amount) ->
    voidAuthorization lifecycle amount
  (state, PaymentFailureConfirmed)
    | state `elem` [PaymentRequiresMethod, PaymentRequiresCustomerAction, PaymentProcessing] ->
        Right lifecycle { paymentState = PaymentFailed }
  (state, PaymentCancellationRequested)
    | state `elem` [PaymentRequiresMethod, PaymentRequiresCustomerAction, PaymentProcessing, PaymentFailed] ->
        Right lifecycle { paymentState = PaymentCancelled }
  (PaymentCaptured, PaymentRefundVerified amount) -> refund lifecycle amount
  (PaymentPartiallyRefunded, PaymentRefundVerified amount) -> refund lifecycle amount
  (state, PaymentDisputeObserved)
    | state `elem` [PaymentCaptured, PaymentPartiallyRefunded] ->
        Right lifecycle { paymentState = PaymentDisputed }
  (PaymentDisputed, PaymentChargebackObserved) ->
    Right lifecycle { paymentState = PaymentChargeback }
  _ -> invalidPaymentTransition lifecycle event

authorize :: PaymentLifecycle -> Int64 -> Either Text PaymentLifecycle
authorize lifecycle amount
  | amount <= 0 = Left "Authorization amount must be positive"
  | amount > paymentAmountMinor lifecycle = Left "Authorization exceeds the payment amount"
  | otherwise = Right lifecycle
      { paymentState = PaymentAuthorized
      , paymentAuthorizedMinor = amount
      }

capture :: PaymentLifecycle -> Int64 -> Either Text PaymentLifecycle
capture lifecycle amount
  | amount <= 0 = Left "Capture amount must be positive"
  | newCaptured > maximumCapture = Left "Capture exceeds the authorized payment balance"
  | otherwise = Right lifecycle
      { paymentState = if newCaptured == paymentAmountMinor lifecycle
          then PaymentCaptured
          else PaymentPartiallyCaptured
      , paymentAuthorizedMinor = max
          (paymentAuthorizedMinor lifecycle)
          maximumCapture
      , paymentCapturedMinor = newCaptured
      }
  where
    newCaptured = paymentCapturedMinor lifecycle + amount
    maximumCapture = case paymentState lifecycle of
      PaymentAuthorized -> paymentAuthorizedMinor lifecycle
      PaymentPartiallyCaptured -> paymentAuthorizedMinor lifecycle
      _ -> paymentAmountMinor lifecycle

voidAuthorization :: PaymentLifecycle -> Int64 -> Either Text PaymentLifecycle
voidAuthorization lifecycle amount
  | amount <= 0 = Left "Void amount must be positive"
  | amount /= remainingAuthorization = Left "Void must release the exact uncaptured authorization balance"
  | otherwise = Right lifecycle
      { paymentState = if paymentCapturedMinor lifecycle == 0
          then PaymentVoided
          else PaymentCaptured
      , paymentAuthorizedMinor = paymentCapturedMinor lifecycle
      }
  where
    remainingAuthorization =
      paymentAuthorizedMinor lifecycle - paymentCapturedMinor lifecycle

refund :: PaymentLifecycle -> Int64 -> Either Text PaymentLifecycle
refund lifecycle amount
  | amount <= 0 = Left "Refund amount must be positive"
  | newRefunded > paymentCapturedMinor lifecycle = Left "Refund exceeds the captured balance"
  | otherwise = Right lifecycle
      { paymentState = if newRefunded == paymentCapturedMinor lifecycle
          then PaymentRefunded
          else PaymentPartiallyRefunded
      , paymentRefundedMinor = newRefunded
      }
  where
    newRefunded = paymentRefundedMinor lifecycle + amount

invalidPaymentTransition
  :: PaymentLifecycle
  -> PaymentEvent
  -> Either Text PaymentLifecycle
invalidPaymentTransition lifecycle event =
  Left
    ( "Invalid payment transition from "
        <> T.pack (show (paymentState lifecycle))
        <> " using "
        <> T.pack (show event)
    )

verifyPaymentBinding :: PaymentVerification -> Either Text ()
verifyPaymentBinding verification
  | pvCheckoutEnvironment verification /= pvEventEnvironment verification = Left "Provider event environment does not match the checkout environment"
  | pvEvidence verification `notElem` [SignatureVerifiedWebhook, ServerToServerVerification] = Left "Payment evidence is not authoritative"
  | pvExpectedAmountMinor verification <= 0 = Left "Expected amount must be positive"
  | pvExpectedAmountMinor verification /= pvActualAmountMinor verification = Left "Provider amount mismatch"
  | normalizeCurrency (pvExpectedCurrency verification) /= normalizeCurrency (pvActualCurrency verification) = Left "Provider currency mismatch"
  | pairMismatch pvExpectedMerchant pvActualMerchant = Left "Provider merchant mismatch"
  | pairMismatch pvExpectedOrder pvActualOrder = Left "Provider order mismatch"
  | pairMismatch pvExpectedResource pvActualResource = Left "Provider resource mismatch"
  | otherwise = Right ()
  where
    pairMismatch expected actual =
      T.null (T.strip (expected verification)) || T.strip (expected verification) /= T.strip (actual verification)
    normalizeCurrency = T.toUpper . T.strip

eventName :: CheckoutEvent -> Text
eventName event = case event of
  CheckoutValidationPassed -> "validation_passed"
  CheckoutHoldAcquired -> "hold_acquired"
  CheckoutPaymentRequested -> "payment_requested"
  CheckoutProviderProcessing -> "provider_processing"
  CheckoutPaymentVerified _ -> "payment_verified"
  CheckoutPaymentFailed -> "payment_failed"
  CheckoutCancelRequested -> "cancel_requested"
  CheckoutExpiryObserved -> "expiry_observed"
  CheckoutPartialRefundSucceeded -> "partial_refund_succeeded"
  CheckoutFullRefundSucceeded -> "full_refund_succeeded"
  CheckoutDisputeOpened -> "dispute_opened"
  CheckoutChargebackConfirmed -> "chargeback_confirmed"

ledgerBalances :: [(Text, Int64)] -> Bool
ledgerBalances entries = not (null entries) && all (== 0) (Map.elems balances)
  where
    balances :: Map Text Int64
    balances = Map.fromListWith (+) [(T.toUpper (T.strip currency), amount) | (currency, amount) <- entries]
