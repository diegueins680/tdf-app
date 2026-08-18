{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.CourseCheckout
  ( CourseEnrollmentState(..)
  , CoursePaymentMode(..)
  , CoursePriceBreakdown(..)
  , calculateCoursePrice
  , parseCourseEnrollmentState
  , courseEnrollmentStateText
  , validateCourseEnrollmentTransition
  ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T

-- Payment is deliberately not part of this state machine. A verified payment
-- can release a seat hold into Enrolled, but it cannot prove attendance,
-- completion, or any later fulfillment event.
data CourseEnrollmentState
  = EnrollmentSeatHeld
  | EnrollmentEnrolled
  | EnrollmentWaitlisted
  | EnrollmentTransferRequested
  | EnrollmentTransferred
  | EnrollmentCancelled
  | EnrollmentCompleted
  | EnrollmentExpired
  deriving (Eq, Show)

data CoursePaymentMode
  = CourseFullPayment
  | CourseDeposit
  deriving (Eq, Show)

data CoursePriceBreakdown = CoursePriceBreakdown
  { cpbSubtotalMinor :: Int64
  , cpbTaxMinor      :: Int64
  , cpbTotalMinor    :: Int64
  , cpbDueNowMinor   :: Int64
  , cpbBalanceMinor  :: Int64
  } deriving (Eq, Show)

-- | Calculate an immutable course checkout snapshot in integer minor units.
-- Ordinary cohorts use 'CourseFullPayment'. A deposit is accepted only when an
-- approved policy explicitly selects it and supplies a valid basis-point rate.
calculateCoursePrice
  :: Int64
  -> Int
  -> CoursePaymentMode
  -> Int
  -> Either Text CoursePriceBreakdown
calculateCoursePrice priceMinor taxBps paymentMode depositBps
  | priceMinor <= 0 = Left "Course price must be greater than zero"
  | taxBps < 0 || taxBps > 10000 =
      Left "Course tax must be between 0 and 10000 basis points"
  | paymentMode == CourseDeposit && (depositBps <= 0 || depositBps >= 10000) =
      Left "Course deposit must be between 1 and 9999 basis points"
  | paymentMode == CourseFullPayment && depositBps /= 10000 =
      Left "Full-payment courses must charge 10000 basis points at checkout"
  | any (> maxInt64) [tax, total, dueNow] = Left "Course total is too large"
  | otherwise = Right CoursePriceBreakdown
      { cpbSubtotalMinor = priceMinor
      , cpbTaxMinor = fromInteger tax
      , cpbTotalMinor = fromInteger total
      , cpbDueNowMinor = fromInteger dueNow
      , cpbBalanceMinor = fromInteger (total - dueNow)
      }
  where
    subtotal = toInteger priceMinor
    tax = roundBasisPoints subtotal taxBps
    total = subtotal + tax
    dueNow = case paymentMode of
      CourseFullPayment -> total
      CourseDeposit -> roundBasisPoints total depositBps
    maxInt64 = toInteger (maxBound :: Int64)

roundBasisPoints :: Integer -> Int -> Integer
roundBasisPoints amount basisPoints =
  (amount * toInteger basisPoints + 5000) `div` 10000

parseCourseEnrollmentState :: Text -> Either Text CourseEnrollmentState
parseCourseEnrollmentState raw =
  case T.toLower (T.strip raw) of
    "seat_held" -> Right EnrollmentSeatHeld
    "enrolled" -> Right EnrollmentEnrolled
    "waitlisted" -> Right EnrollmentWaitlisted
    "transfer_requested" -> Right EnrollmentTransferRequested
    "transferred" -> Right EnrollmentTransferred
    "cancelled" -> Right EnrollmentCancelled
    "completed" -> Right EnrollmentCompleted
    "expired" -> Right EnrollmentExpired
    _ -> Left "Unknown course enrollment state"

courseEnrollmentStateText :: CourseEnrollmentState -> Text
courseEnrollmentStateText state = case state of
  EnrollmentSeatHeld -> "seat_held"
  EnrollmentEnrolled -> "enrolled"
  EnrollmentWaitlisted -> "waitlisted"
  EnrollmentTransferRequested -> "transfer_requested"
  EnrollmentTransferred -> "transferred"
  EnrollmentCancelled -> "cancelled"
  EnrollmentCompleted -> "completed"
  EnrollmentExpired -> "expired"

validateCourseEnrollmentTransition
  :: CourseEnrollmentState
  -> CourseEnrollmentState
  -> Either Text ()
validateCourseEnrollmentTransition fromState toState
  | fromState == toState = Right ()
  | (fromState, toState) `elem` allowedTransitions = Right ()
  | otherwise = Left "Course enrollment transition is not allowed"
  where
    allowedTransitions =
      [ (EnrollmentSeatHeld, EnrollmentEnrolled)
      , (EnrollmentSeatHeld, EnrollmentCancelled)
      , (EnrollmentSeatHeld, EnrollmentExpired)
      , (EnrollmentWaitlisted, EnrollmentSeatHeld)
      , (EnrollmentWaitlisted, EnrollmentCancelled)
      , (EnrollmentEnrolled, EnrollmentTransferRequested)
      , (EnrollmentEnrolled, EnrollmentCancelled)
      , (EnrollmentEnrolled, EnrollmentCompleted)
      , (EnrollmentTransferRequested, EnrollmentEnrolled)
      , (EnrollmentTransferRequested, EnrollmentTransferred)
      , (EnrollmentTransferRequested, EnrollmentCancelled)
      ]
