{-# LANGUAGE OverloadedStrings #-}

-- | Deposit admission policy shared by Domo quotes and course checkouts
-- (PAY-DEPOSIT-POLICY-001, AUTHORITY-095).
--
-- A basis-point deposit can round to nothing or to almost the whole total.
-- The policy applies an absolute minimum and switches to full payment when the
-- deposit would exceed a share of the total, so no quote is rejected because
-- of rounding alone and no "deposit" is the whole price in disguise.
module TDF.Commerce.DepositPolicy
  ( DepositPolicy (..)
  , DepositOutcome (..)
  , defaultDepositPolicy
  , validateDepositPolicy
  , applyDepositPolicy
  ) where

import           Data.Int (Int64)
import           Data.Text (Text)

data DepositPolicy = DepositPolicy
  { dpMinimumDepositMinor :: !Int64
    -- ^ absolute minimum deposit (default 5.00)
  , dpFullPaymentAboveBps :: !Int
    -- ^ when the deposit exceeds this share of the total, charge the total
  } deriving (Eq, Show)

defaultDepositPolicy :: DepositPolicy
defaultDepositPolicy = DepositPolicy { dpMinimumDepositMinor = 500, dpFullPaymentAboveBps = 9000 }

validateDepositPolicy :: DepositPolicy -> Either Text DepositPolicy
validateDepositPolicy policy
  | dpMinimumDepositMinor policy < 0 = Left "Minimum deposit must not be negative"
  | dpFullPaymentAboveBps policy < 0 || dpFullPaymentAboveBps policy > 10000 =
      Left "Full-payment threshold must be between 0 and 10000 basis points"
  | otherwise = Right policy

data DepositOutcome
  = DepositOf !Int64
    -- ^ charge this deposit now; the balance is total minus it
  | FullPaymentNow
    -- ^ the deposit would be the whole price in disguise: charge the total
  deriving (Eq, Show)

-- | Apply the policy to a computed deposit for a positive total.
--
-- The result is always within @1..total@ (as a deposit) or the full total, so
-- @deposit + balance = total@ and @balance >= 0@ hold for callers.
applyDepositPolicy :: DepositPolicy -> Int64 -> Int64 -> DepositOutcome
applyDepositPolicy policy totalMinor computedDepositMinor
  | totalMinor <= 0 = FullPaymentNow
  | raised >= totalMinor = FullPaymentNow
  | toInteger raised * 10000 > toInteger totalMinor * toInteger (dpFullPaymentAboveBps policy) = FullPaymentNow
  | otherwise = DepositOf raised
  where
    raised = max 1 (max (dpMinimumDepositMinor policy) computedDepositMinor)
