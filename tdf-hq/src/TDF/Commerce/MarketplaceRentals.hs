{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.MarketplaceRentals
  ( MarketplaceRentalState(..)
  , MarketplaceDepositState(..)
  , RentalPriceBreakdown(..)
  , parseMarketplaceRentalState
  , marketplaceRentalStateText
  , marketplaceDepositStateText
  , rentalDurationDays
  , calculateRentalPrice
  , validateMarketplaceRentalTransition
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, diffDays)

data MarketplaceRentalState
  = RentalOnHold
  | RentalConfirmed
  | RentalReadyForHandoff
  | RentalCheckedOut
  | RentalReturnDue
  | RentalReturnedPendingInspection
  | RentalDamageReview
  | RentalDepositRefundDue
  | RentalClosed
  | RentalCancellationRequested
  | RentalCancelled
  | RentalNoShow
  | RentalLost
  | RentalDisputed
  | RentalExpired
  deriving (Eq, Show)

data MarketplaceDepositState
  = DepositAwaitingPayment
  | DepositCollected
  | DepositInspectionPending
  | DepositDeductionProposed
  | DepositRefundDue
  | DepositPartialRefundDue
  | DepositRefunded
  | DepositPartiallyRefunded
  | DepositForfeited
  | DepositDisputed
  deriving (Eq, Show)

data RentalPriceBreakdown = RentalPriceBreakdown
  { rpbDurationDays :: Int
  , rpbRentalChargeMinor :: Int
  , rpbSecurityDepositMinor :: Int
  , rpbCheckoutTotalMinor :: Int
  } deriving (Eq, Show)

parseMarketplaceRentalState :: Text -> Either Text MarketplaceRentalState
parseMarketplaceRentalState rawState =
  case T.toLower (T.strip rawState) of
    "on_hold" -> Right RentalOnHold
    "confirmed" -> Right RentalConfirmed
    "ready_for_handoff" -> Right RentalReadyForHandoff
    "checked_out" -> Right RentalCheckedOut
    "return_due" -> Right RentalReturnDue
    "returned_pending_inspection" -> Right RentalReturnedPendingInspection
    "damage_review" -> Right RentalDamageReview
    "deposit_refund_due" -> Right RentalDepositRefundDue
    "closed" -> Right RentalClosed
    "cancellation_requested" -> Right RentalCancellationRequested
    "cancelled" -> Right RentalCancelled
    "no_show" -> Right RentalNoShow
    "lost" -> Right RentalLost
    "disputed" -> Right RentalDisputed
    "expired" -> Right RentalExpired
    _ -> Left "Unknown marketplace rental state"

marketplaceRentalStateText :: MarketplaceRentalState -> Text
marketplaceRentalStateText state = case state of
  RentalOnHold -> "on_hold"
  RentalConfirmed -> "confirmed"
  RentalReadyForHandoff -> "ready_for_handoff"
  RentalCheckedOut -> "checked_out"
  RentalReturnDue -> "return_due"
  RentalReturnedPendingInspection -> "returned_pending_inspection"
  RentalDamageReview -> "damage_review"
  RentalDepositRefundDue -> "deposit_refund_due"
  RentalClosed -> "closed"
  RentalCancellationRequested -> "cancellation_requested"
  RentalCancelled -> "cancelled"
  RentalNoShow -> "no_show"
  RentalLost -> "lost"
  RentalDisputed -> "disputed"
  RentalExpired -> "expired"

marketplaceDepositStateText :: MarketplaceDepositState -> Text
marketplaceDepositStateText state = case state of
  DepositAwaitingPayment -> "awaiting_payment"
  DepositCollected -> "collected"
  DepositInspectionPending -> "inspection_pending"
  DepositDeductionProposed -> "deduction_proposed"
  DepositRefundDue -> "refund_due"
  DepositPartialRefundDue -> "partial_refund_due"
  DepositRefunded -> "refunded"
  DepositPartiallyRefunded -> "partially_refunded"
  DepositForfeited -> "forfeited"
  DepositDisputed -> "disputed"

rentalDurationDays :: Day -> Day -> Either Text Int
rentalDurationDays startDate endDate
  | endDate < startDate = Left "Rental end date must be on or after the start date"
  | duration > fromIntegral (maxBound :: Int) = Left "Rental duration is too large"
  | otherwise = Right (fromIntegral duration)
  where
    duration = diffDays endDate startDate + 1

calculateRentalPrice
  :: Int
  -> Maybe Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Either Text RentalPriceBreakdown
calculateRentalPrice dailyRateMinor weeklyRateMinor securityDepositMinor minDays maxDays durationDays
  | dailyRateMinor <= 0 = Left "Rental daily rate must be greater than zero"
  | maybe False (<= 0) weeklyRateMinor = Left "Rental weekly rate must be greater than zero"
  | maybe False ((> toInteger dailyRateMinor * 7) . toInteger) weeklyRateMinor =
      Left "Rental weekly rate cannot exceed seven daily rates"
  | securityDepositMinor < 0 = Left "Rental security deposit cannot be negative"
  | minDays < 1 = Left "Rental minimum duration must be at least one day"
  | maxDays < minDays = Left "Rental maximum duration cannot be below its minimum"
  | durationDays < minDays || durationDays > maxDays =
      Left "Rental duration is outside the configured listing limits"
  | rentalChargeInteger + toInteger securityDepositMinor > toInteger (maxBound :: Int) =
      Left "Rental total is too large"
  | otherwise = Right RentalPriceBreakdown
      { rpbDurationDays = durationDays
      , rpbRentalChargeMinor = fromInteger rentalChargeInteger
      , rpbSecurityDepositMinor = securityDepositMinor
      , rpbCheckoutTotalMinor = fromInteger rentalChargeInteger + securityDepositMinor
      }
  where
    rentalChargeInteger = case weeklyRateMinor of
      Nothing -> toInteger durationDays * toInteger dailyRateMinor
      Just weeklyRate ->
        let (wholeWeeks, remainingDays) = durationDays `divMod` 7
        in toInteger wholeWeeks * toInteger weeklyRate
            + toInteger remainingDays * toInteger dailyRateMinor

validateMarketplaceRentalTransition
  :: MarketplaceRentalState
  -> MarketplaceRentalState
  -> Either Text ()
validateMarketplaceRentalTransition fromState toState
  | fromState == toState = Right ()
  | (fromState, toState) `elem` allowedTransitions = Right ()
  | otherwise = Left "Marketplace rental transition is not allowed"
  where
    allowedTransitions =
      [ (RentalOnHold, RentalConfirmed)
      , (RentalOnHold, RentalCancelled)
      , (RentalOnHold, RentalExpired)
      , (RentalConfirmed, RentalReadyForHandoff)
      , (RentalConfirmed, RentalCancellationRequested)
      , (RentalConfirmed, RentalNoShow)
      , (RentalReadyForHandoff, RentalCheckedOut)
      , (RentalReadyForHandoff, RentalCancellationRequested)
      , (RentalReadyForHandoff, RentalNoShow)
      , (RentalCheckedOut, RentalReturnDue)
      , (RentalCheckedOut, RentalReturnedPendingInspection)
      , (RentalCheckedOut, RentalLost)
      , (RentalCheckedOut, RentalDisputed)
      , (RentalReturnDue, RentalReturnedPendingInspection)
      , (RentalReturnDue, RentalLost)
      , (RentalReturnDue, RentalDisputed)
      , (RentalReturnedPendingInspection, RentalDepositRefundDue)
      , (RentalReturnedPendingInspection, RentalDamageReview)
      , (RentalDamageReview, RentalDepositRefundDue)
      , (RentalDamageReview, RentalDisputed)
      , (RentalDepositRefundDue, RentalClosed)
      , (RentalCancellationRequested, RentalCancelled)
      , (RentalNoShow, RentalCancelled)
      , (RentalLost, RentalDisputed)
      , (RentalDisputed, RentalDamageReview)
      , (RentalDisputed, RentalDepositRefundDue)
      , (RentalDisputed, RentalClosed)
      ]
