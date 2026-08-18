{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.ServiceBookings
  ( ServiceBookingState(..)
  , BookingPriceBreakdown(..)
  , calculateBookingPrice
  , parseServiceBookingState
  , serviceBookingStateText
  , validateServiceBookingTransition
  ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T

-- Payment state deliberately does not appear here. A paid deposit may unlock a
-- transition to Confirmed, but it never means that the booked service happened.
data ServiceBookingState
  = BookingOnHold
  | BookingConfirmed
  | BookingScheduled
  | BookingInProgress
  | BookingBalanceDue
  | BookingCompleted
  | BookingRescheduleRequested
  | BookingCancellationRequested
  | BookingCancelled
  | BookingNoShow
  | BookingOvertimeReview
  | BookingDisputed
  | BookingExpired
  deriving (Eq, Show)

data BookingPriceBreakdown = BookingPriceBreakdown
  { bpbDurationMinutes :: Int
  , bpbBillingUnits :: Int
  , bpbSubtotalMinor :: Int64
  , bpbTaxMinor :: Int64
  , bpbTotalMinor :: Int64
  , bpbDepositMinor :: Int64
  , bpbBalanceMinor :: Int64
  } deriving (Eq, Show)

-- | Calculate an immutable booking quote using integer minor units only.
-- Durations must align exactly to the approved billing unit and policy step;
-- silently rounding a customer-selected duration would change the price.
calculateBookingPrice
  :: Int64 -- ^ rate per billing unit
  -> Int   -- ^ billing unit minutes
  -> Int   -- ^ tax basis points
  -> Int   -- ^ deposit basis points
  -> Int   -- ^ minimum duration minutes
  -> Int   -- ^ maximum duration minutes
  -> Int   -- ^ duration step minutes
  -> Int   -- ^ requested duration minutes
  -> Either Text BookingPriceBreakdown
calculateBookingPrice rateMinor unitMinutes taxBps depositBps minMinutes maxMinutes stepMinutes durationMinutes
  | rateMinor <= 0 = Left "Booking rate must be greater than zero"
  | unitMinutes <= 0 = Left "Booking billing unit must be greater than zero"
  | taxBps < 0 || taxBps > 10000 = Left "Booking tax must be between 0 and 10000 basis points"
  | depositBps <= 0 || depositBps > 10000 = Left "Booking deposit must be between 1 and 10000 basis points"
  | minMinutes <= 0 = Left "Booking minimum duration must be greater than zero"
  | maxMinutes < minMinutes = Left "Booking maximum duration cannot be below its minimum"
  | stepMinutes <= 0 = Left "Booking duration step must be greater than zero"
  | durationMinutes < minMinutes || durationMinutes > maxMinutes =
      Left "Booking duration is outside the approved policy limits"
  | durationMinutes `mod` stepMinutes /= 0 =
      Left "Booking duration does not align to the approved duration step"
  | durationMinutes `mod` unitMinutes /= 0 =
      Left "Booking duration does not align to the approved billing unit"
  | any (> maxInt64) [subtotal, tax, total, deposit] = Left "Booking total is too large"
  | otherwise = Right BookingPriceBreakdown
      { bpbDurationMinutes = durationMinutes
      , bpbBillingUnits = billingUnits
      , bpbSubtotalMinor = fromInteger subtotal
      , bpbTaxMinor = fromInteger tax
      , bpbTotalMinor = fromInteger total
      , bpbDepositMinor = fromInteger deposit
      , bpbBalanceMinor = fromInteger (total - deposit)
      }
  where
    billingUnits = durationMinutes `div` unitMinutes
    subtotal = toInteger rateMinor * toInteger billingUnits
    tax = roundBasisPoints subtotal taxBps
    total = subtotal + tax
    deposit = roundBasisPoints total depositBps
    maxInt64 = toInteger (maxBound :: Int64)

roundBasisPoints :: Integer -> Int -> Integer
roundBasisPoints amount basisPoints =
  (amount * toInteger basisPoints + 5000) `div` 10000

parseServiceBookingState :: Text -> Either Text ServiceBookingState
parseServiceBookingState raw =
  case T.toLower (T.strip raw) of
    "on_hold" -> Right BookingOnHold
    "confirmed" -> Right BookingConfirmed
    "scheduled" -> Right BookingScheduled
    "in_progress" -> Right BookingInProgress
    "balance_due" -> Right BookingBalanceDue
    "completed" -> Right BookingCompleted
    "reschedule_requested" -> Right BookingRescheduleRequested
    "cancellation_requested" -> Right BookingCancellationRequested
    "cancelled" -> Right BookingCancelled
    "no_show" -> Right BookingNoShow
    "overtime_review" -> Right BookingOvertimeReview
    "disputed" -> Right BookingDisputed
    "expired" -> Right BookingExpired
    _ -> Left "Unknown service booking state"

serviceBookingStateText :: ServiceBookingState -> Text
serviceBookingStateText state = case state of
  BookingOnHold -> "on_hold"
  BookingConfirmed -> "confirmed"
  BookingScheduled -> "scheduled"
  BookingInProgress -> "in_progress"
  BookingBalanceDue -> "balance_due"
  BookingCompleted -> "completed"
  BookingRescheduleRequested -> "reschedule_requested"
  BookingCancellationRequested -> "cancellation_requested"
  BookingCancelled -> "cancelled"
  BookingNoShow -> "no_show"
  BookingOvertimeReview -> "overtime_review"
  BookingDisputed -> "disputed"
  BookingExpired -> "expired"

validateServiceBookingTransition
  :: ServiceBookingState
  -> ServiceBookingState
  -> Either Text ()
validateServiceBookingTransition fromState toState
  | fromState == toState = Right ()
  | (fromState, toState) `elem` allowedTransitions = Right ()
  | otherwise = Left "Service booking transition is not allowed"
  where
    allowedTransitions =
      [ (BookingOnHold, BookingConfirmed)
      , (BookingOnHold, BookingCancelled)
      , (BookingOnHold, BookingExpired)
      , (BookingConfirmed, BookingScheduled)
      , (BookingConfirmed, BookingRescheduleRequested)
      , (BookingConfirmed, BookingCancellationRequested)
      , (BookingConfirmed, BookingNoShow)
      , (BookingScheduled, BookingInProgress)
      , (BookingScheduled, BookingRescheduleRequested)
      , (BookingScheduled, BookingCancellationRequested)
      , (BookingScheduled, BookingNoShow)
      , (BookingInProgress, BookingBalanceDue)
      , (BookingInProgress, BookingOvertimeReview)
      , (BookingInProgress, BookingDisputed)
      , (BookingOvertimeReview, BookingBalanceDue)
      , (BookingOvertimeReview, BookingDisputed)
      , (BookingBalanceDue, BookingCompleted)
      , (BookingBalanceDue, BookingDisputed)
      , (BookingRescheduleRequested, BookingConfirmed)
      , (BookingRescheduleRequested, BookingCancellationRequested)
      , (BookingCancellationRequested, BookingCancelled)
      , (BookingNoShow, BookingBalanceDue)
      , (BookingNoShow, BookingCancelled)
      , (BookingDisputed, BookingBalanceDue)
      , (BookingDisputed, BookingCancelled)
      ]
