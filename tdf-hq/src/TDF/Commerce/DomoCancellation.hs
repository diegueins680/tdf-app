{-# LANGUAGE OverloadedStrings #-}

-- | Refund policy for cancelling a Domo quote after money was collected
-- (PAY-DOMO-CANCEL-001, AUTHORITY-093).
--
-- Pure computation over a versioned policy record. The policy rows live in
-- the database (versioned, approved); this module only computes the refund
-- from a policy value, the canceller, the lead time and the collected amounts.
-- The cancellation transaction creates the refund request for exactly this
-- amount so no collected money is left unaccounted.
module TDF.Commerce.DomoCancellation
  ( Canceller (..)
  , DomoCancellationPolicy (..)
  , defaultDomoCancellationPolicy
  , validateDomoCancellationPolicy
  , depositRetainedBasisPoints
  , domoCancellationRefund
  ) where

import           Data.Int (Int64)
import           Data.Text (Text)

data Canceller = CancelledByCustomer | CancelledByTdf
  deriving (Eq, Show)

-- | Deposit retention by lead time, in whole days before the event start.
data DomoCancellationPolicy = DomoCancellationPolicy
  { dcpFullRefundMinDays      :: !Int
    -- ^ at or above this many days the deposit is refunded in full
  , dcpPartialRefundMinDays   :: !Int
    -- ^ at or above this many days (and below the full threshold) the partial rate applies
  , dcpPartialRetainedBps     :: !Int
    -- ^ basis points of the deposit retained in the partial window
  , dcpBalanceRefundedInFull  :: !Bool
    -- ^ collected balance is always refunded (the service was not delivered)
  } deriving (Eq, Show)

-- | AUTHORITY-093: more than 30 days full refund; 7 to 30 days 50% retained;
-- under 7 days fully retained; collected balance always refunded.
defaultDomoCancellationPolicy :: DomoCancellationPolicy
defaultDomoCancellationPolicy = DomoCancellationPolicy
  { dcpFullRefundMinDays = 31
  , dcpPartialRefundMinDays = 7
  , dcpPartialRetainedBps = 5000
  , dcpBalanceRefundedInFull = True
  }

validateDomoCancellationPolicy :: DomoCancellationPolicy -> Either Text DomoCancellationPolicy
validateDomoCancellationPolicy policy
  | dcpPartialRefundMinDays policy < 0 = Left "Partial refund threshold must not be negative"
  | dcpFullRefundMinDays policy <= dcpPartialRefundMinDays policy =
      Left "Full refund threshold must exceed the partial refund threshold"
  | dcpPartialRetainedBps policy < 0 || dcpPartialRetainedBps policy > 10000 =
      Left "Partial retention must be between 0 and 10000 basis points"
  | otherwise = Right policy

-- | Basis points of the deposit retained for a customer cancellation with the
-- given lead time (days before the event; negative means after the start).
depositRetainedBasisPoints :: DomoCancellationPolicy -> Bool -> Int -> Int
depositRetainedBasisPoints policy inProgress leadDays
  | inProgress = 10000
  | leadDays >= dcpFullRefundMinDays policy = 0
  | leadDays >= dcpPartialRefundMinDays policy = dcpPartialRetainedBps policy
  | otherwise = 10000

-- | Refund in minor units for a cancellation.
--
-- Arguments: policy, who cancels, whether the event is already in progress,
-- lead days, collected deposit and collected balance. TDF cancellations
-- refund everything. The retained part of the deposit rounds half up, so the
-- refund is the exact complement and @0 <= refund <= collected@.
domoCancellationRefund
  :: DomoCancellationPolicy -> Canceller -> Bool -> Int -> Int64 -> Int64 -> Int64
domoCancellationRefund policy canceller inProgress leadDays depositMinor balanceMinor =
  case canceller of
    CancelledByTdf -> deposit + balance
    CancelledByCustomer ->
      let retainedBps = toInteger (depositRetainedBasisPoints policy inProgress leadDays)
          retained = (toInteger deposit * retainedBps + 5000) `div` 10000
          refundedDeposit = toInteger deposit - retained
          refundedBalance = if dcpBalanceRefundedInFull policy then toInteger balance else 0
      in fromInteger (refundedDeposit + refundedBalance)
  where
    deposit = max 0 depositMinor
    balance = max 0 balanceMinor
