{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.EventTickets
  ( TicketFulfillmentState(..)
  , TicketPaymentGate(..)
  , TicketPriceBreakdown(..)
  , calculateTicketPrice
  , calculateTicketPriceWithTaxMode
  , parseTicketFulfillmentState
  , ticketFulfillmentStateText
  , validateTicketFulfillmentTransition
  ) where

import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T

data TicketFulfillmentState
  = TicketSeatHeld
  | TicketIssued
  | TicketTransferRequested
  | TicketTransferred
  | TicketCheckedIn
  | TicketCancelled
  | TicketRefunded
  | TicketExpired
  deriving (Eq, Show)

-- | Payment is an independent aggregate. It is consulted only as a gate when
-- fulfillment issues a ticket; a verified payment never performs issuance by
-- itself.
data TicketPaymentGate
  = TicketPaymentPending
  | TicketPaymentVerified
  | TicketPaymentNotRequired
  deriving (Eq, Show)

data TicketPriceBreakdown = TicketPriceBreakdown
  { tpbGrossFaceValueMinor :: Int64
  , tpbDiscountMinor :: Int64
  , tpbNetFaceValueMinor :: Int64
  , tpbBuyerFeeMinor :: Int64
  , tpbOrganizerFeeMinor :: Int64
  , tpbTaxMinor :: Int64
  , tpbCheckoutTotalMinor :: Int64
  , tpbOrganizerPayableMinor :: Int64
  , tpbPlatformFeeMinor :: Int64
  } deriving (Eq, Show)

calculateTicketPrice
  :: Int64
  -> Int
  -> Int64
  -> Int
  -> Int
  -> Int
  -> Either Text TicketPriceBreakdown
calculateTicketPrice = calculateTicketPriceWithTaxMode False

-- | An inclusive policy keeps the advertised face value (and any buyer fee)
-- inclusive of tax. Discounts are in the same advertised units. Round the tax
-- component once per order; never reduce the per-ticket price to an approximate
-- tax-exclusive amount. Legacy policies remain additive.
calculateTicketPriceWithTaxMode
  :: Bool -> Int64 -> Int -> Int64 -> Int -> Int -> Int
  -> Either Text TicketPriceBreakdown
calculateTicketPriceWithTaxMode taxIncluded unitPrice quantity discount buyerFeeBps organizerFeeBps taxBps
  | unitPrice <= 0 = Left "Ticket unit price must be greater than zero"
  | quantity <= 0 || quantity > 100 = Left "Ticket quantity must be between 1 and 100"
  | discount < 0 = Left "Ticket discount must not be negative"
  | any invalidBasisPoints [buyerFeeBps, organizerFeeBps, taxBps] =
      Left "Ticket fee and tax rates must be between 0 and 10000 basis points"
  | gross > maxInt64 = Left "Ticket face value is too large"
  | toInteger discount > gross = Left "Ticket discount exceeds face value"
  | any (> maxInt64) [buyerFee, organizerFee, tax, checkoutTotal, platformFee] =
      Left "Ticket checkout total is too large"
  | organizerFee + includedTax > netFace = Left "Organizer fee and included tax exceed net face value"
  | otherwise = Right TicketPriceBreakdown
      { tpbGrossFaceValueMinor = fromInteger gross
      , tpbDiscountMinor = discount
      , tpbNetFaceValueMinor = fromInteger netFace
      , tpbBuyerFeeMinor = fromInteger buyerFee
      , tpbOrganizerFeeMinor = fromInteger organizerFee
      , tpbTaxMinor = fromInteger tax
      , tpbCheckoutTotalMinor = fromInteger checkoutTotal
      , tpbOrganizerPayableMinor = fromInteger (netFace - organizerFee - includedTax)
      , tpbPlatformFeeMinor = fromInteger platformFee
      }
  where
    gross = toInteger unitPrice * toInteger quantity
    netFace = gross - toInteger discount
    buyerFee = roundBasisPoints netFace buyerFeeBps
    organizerFee = roundBasisPoints netFace organizerFeeBps
    tax = if taxIncluded
      then ((netFace + buyerFee) * toInteger taxBps + divisor `div` 2) `div` divisor
      else roundBasisPoints (netFace + buyerFee) taxBps
    divisor = 10000 + toInteger taxBps
    includedTax = if taxIncluded then tax else 0
    checkoutTotal = netFace + buyerFee + tax - includedTax
    platformFee = buyerFee + organizerFee
    maxInt64 = toInteger (maxBound :: Int64)
    invalidBasisPoints value = value < 0 || value > 10000

roundBasisPoints :: Integer -> Int -> Integer
roundBasisPoints amount basisPoints =
  (amount * toInteger basisPoints + 5000) `div` 10000

parseTicketFulfillmentState :: Text -> Either Text TicketFulfillmentState
parseTicketFulfillmentState raw =
  case T.toLower (T.strip raw) of
    "seat_held" -> Right TicketSeatHeld
    "issued" -> Right TicketIssued
    "transfer_requested" -> Right TicketTransferRequested
    "transferred" -> Right TicketTransferred
    "checked_in" -> Right TicketCheckedIn
    "cancelled" -> Right TicketCancelled
    "refunded" -> Right TicketRefunded
    "expired" -> Right TicketExpired
    _ -> Left "Unknown ticket fulfillment state"

ticketFulfillmentStateText :: TicketFulfillmentState -> Text
ticketFulfillmentStateText state = case state of
  TicketSeatHeld -> "seat_held"
  TicketIssued -> "issued"
  TicketTransferRequested -> "transfer_requested"
  TicketTransferred -> "transferred"
  TicketCheckedIn -> "checked_in"
  TicketCancelled -> "cancelled"
  TicketRefunded -> "refunded"
  TicketExpired -> "expired"

validateTicketFulfillmentTransition
  :: TicketPaymentGate
  -> TicketFulfillmentState
  -> TicketFulfillmentState
  -> Either Text ()
validateTicketFulfillmentTransition paymentGate fromState toState
  | fromState == toState = Right ()
  | fromState == TicketSeatHeld && toState == TicketIssued
  , paymentGate `notElem` [TicketPaymentVerified, TicketPaymentNotRequired] =
      Left "Ticket issuance requires verified payment or an explicit no-payment entitlement"
  | (fromState, toState) `elem` allowedTransitions = Right ()
  | otherwise = Left "Ticket fulfillment transition is not allowed"
  where
    allowedTransitions =
      [ (TicketSeatHeld, TicketIssued)
      , (TicketSeatHeld, TicketCancelled)
      , (TicketSeatHeld, TicketExpired)
      , (TicketIssued, TicketTransferRequested)
      , (TicketIssued, TicketCheckedIn)
      , (TicketIssued, TicketCancelled)
      , (TicketIssued, TicketRefunded)
      , (TicketTransferRequested, TicketIssued)
      , (TicketTransferRequested, TicketTransferred)
      , (TicketTransferRequested, TicketCancelled)
      , (TicketTransferred, TicketTransferRequested)
      , (TicketTransferred, TicketCheckedIn)
      , (TicketTransferred, TicketRefunded)
      ]
