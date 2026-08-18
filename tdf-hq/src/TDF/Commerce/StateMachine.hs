{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.StateMachine
  ( CheckoutState(..)
  , CheckoutEvent(..)
  , ProviderEnvironment(..)
  , VerificationEvidence(..)
  , PaymentVerification(..)
  , transitionCheckout
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
