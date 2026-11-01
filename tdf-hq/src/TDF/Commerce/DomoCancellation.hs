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
  , leadDaysBefore
  , CancellationAssessment (..)
  , assessDomoCancellation
  ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime, diffUTCTime)

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

-- | Whole days from the cancellation instant to the event start; negative once
-- the start has passed. The day boundary is 24 hours before the start instant
-- (not a calendar day in the venue time zone), so the rule is the same for
-- every venue and does not depend on daylight-saving transitions.
leadDaysBefore :: UTCTime -> UTCTime -> Int
leadDaysBefore now startsAt =
  let seconds = toRational (diffUTCTime startsAt now)
      days = floor (seconds / 86400) :: Integer
  in fromInteger (max (toInteger (minBound :: Int)) (min (toInteger (maxBound :: Int)) days))

-- | The money outcome of one cancellation, recorded with the cancellation so
-- the refund obligation is auditable without recomputing it later.
data CancellationAssessment = CancellationAssessment
  { caLeadDays               :: !Int
  , caInProgress             :: !Bool
  , caRetainedBasisPoints    :: !Int
  , caDepositCollectedMinor  :: !Int64
  , caBalanceCollectedMinor  :: !Int64
  , caRetainedMinor          :: !Int64
  , caRefundDueMinor         :: !Int64
  } deriving (Eq, Show)

-- | Assess a cancellation at @now@ for an event starting at @startsAt@.
--
-- @statusInProgress@ is the quote lifecycle flag; the event is also treated as
-- in progress once @now >= startsAt@. Collected amounts below zero count as
-- zero. The invariants @retained + refundDue = collected@ and
-- @retained <= deposit collected@ hold for every input.
assessDomoCancellation
  :: DomoCancellationPolicy -> Canceller -> Bool -> UTCTime -> UTCTime -> Int64 -> Int64
  -> CancellationAssessment
assessDomoCancellation policy canceller statusInProgress now startsAt depositMinor balanceMinor =
  CancellationAssessment
    { caLeadDays = leadDays
    , caInProgress = inProgress
    , caRetainedBasisPoints = case canceller of
        CancelledByTdf -> 0
        CancelledByCustomer -> depositRetainedBasisPoints policy inProgress leadDays
    , caDepositCollectedMinor = deposit
    , caBalanceCollectedMinor = balance
    , caRetainedMinor = deposit + balance - refund
    , caRefundDueMinor = refund
    }
  where
    leadDays = leadDaysBefore now startsAt
    inProgress = statusInProgress || now >= startsAt
    deposit = max 0 depositMinor
    balance = max 0 balanceMinor
    refund = domoCancellationRefund policy canceller inProgress leadDays deposit balance
