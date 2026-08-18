{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.DomoQuotes
  ( DomoQuoteState(..)
  , DomoQuoteLine(..)
  , DomoEventRate(..)
  , DomoRateCard(..)
  , DomoQuoteInput(..)
  , DomoQuoteBreakdown(..)
  , calculateDomoQuote
  , parseDomoQuoteState
  , domoQuoteStateText
  , validateDomoQuoteTransition
  ) where

import           Data.Int (Int64)
import           Data.Aeson (FromJSON(..), withObject, (.:))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T

-- Payment is intentionally not part of this state machine. A verified deposit
-- is an explicit gate for DepositDue -> DepositPaid; it never completes the
-- event or proves that a customer received any service.
data DomoQuoteState
  = DomoDraft
  | DomoSent
  | DomoViewed
  | DomoAccepted
  | DomoDepositDue
  | DomoDepositPaid
  | DomoInProgress
  | DomoBalanceDue
  | DomoCompleted
  | DomoCancelled
  | DomoExpired
  deriving (Eq, Show)

data DomoQuoteLine = DomoQuoteLine
  { dqlCode            :: Text
  , dqlDescription     :: Text
  , dqlQuantity        :: Int
  , dqlUnitAmountMinor :: Int64
  , dqlSubtotalMinor   :: Int64
  } deriving (Eq, Show)

data DomoEventRate = DomoEventRate
  { derBaseMinor       :: Int64
  , derPerGuestMinor   :: Int64
  , derMinimumHours    :: Int
  , derIncludedGuests  :: Int
  } deriving (Eq, Show)

instance FromJSON DomoEventRate where
  parseJSON = withObject "DomoEventRate" $ \value -> DomoEventRate
    <$> value .: "base_minor"
    <*> value .: "per_guest_minor"
    <*> value .: "minimum_hours"
    <*> value .: "included_guests"

data DomoRateCard = DomoRateCard
  { drcEventRates            :: Map Text DomoEventRate
  , drcHourMinor             :: Int64
  , drcSetupHourMinor        :: Int64
  , drcCateringMinimumMinor  :: Int64
  , drcCateringPerGuestMinor :: Int64
  , drcProductionMinor       :: Int64
  , drcTransportMinor        :: Int64
  , drcTaxBasisPoints        :: Int
  , drcDepositBasisPoints    :: Int
  , drcMaximumGuests         :: Int
  , drcMaximumDurationHours  :: Int
  , drcMaximumSetupHours     :: Int
  } deriving (Eq, Show)

data DomoQuoteInput = DomoQuoteInput
  { dqiEventType    :: Text
  , dqiGuests       :: Int
  , dqiDurationHours :: Int
  , dqiSetupHours   :: Int
  , dqiCatering     :: Bool
  , dqiProduction   :: Bool
  , dqiTransport    :: Bool
  } deriving (Eq, Show)

data DomoQuoteBreakdown = DomoQuoteBreakdown
  { dqbLines          :: [DomoQuoteLine]
  , dqbBillableHours  :: Int
  , dqbSubtotalMinor  :: Int64
  , dqbTaxMinor       :: Int64
  , dqbTotalMinor     :: Int64
  , dqbDepositMinor   :: Int64
  , dqbBalanceMinor   :: Int64
  } deriving (Eq, Show)

calculateDomoQuote
  :: DomoRateCard
  -> DomoQuoteInput
  -> Either Text DomoQuoteBreakdown
calculateDomoQuote rateCard input
  | Map.null (drcEventRates rateCard) = Left "Domo rate card has no event types"
  | any (< 0) monetaryRates = Left "Domo rate card contains a negative amount"
  | drcHourMinor rateCard <= 0 = Left "Domo hourly rate must be greater than zero"
  | drcTaxBasisPoints rateCard < 0 || drcTaxBasisPoints rateCard > 10000 =
      Left "Domo tax must be between 0 and 10000 basis points"
  | drcDepositBasisPoints rateCard <= 0 || drcDepositBasisPoints rateCard > 10000 =
      Left "Domo deposit must be between 1 and 10000 basis points"
  | drcMaximumGuests rateCard <= 0 = Left "Domo maximum guests must be greater than zero"
  | drcMaximumDurationHours rateCard <= 0 =
      Left "Domo maximum duration must be greater than zero"
  | drcMaximumSetupHours rateCard < 0 = Left "Domo maximum setup duration is invalid"
  | dqiGuests input <= 0 || dqiGuests input > drcMaximumGuests rateCard =
      Left "Domo guest count is outside the approved rate card limits"
  | dqiDurationHours input <= 0 || dqiDurationHours input > drcMaximumDurationHours rateCard =
      Left "Domo duration is outside the approved rate card limits"
  | dqiSetupHours input < 0 || dqiSetupHours input > drcMaximumSetupHours rateCard =
      Left "Domo setup duration is outside the approved rate card limits"
  | otherwise = case Map.lookup normalizedEventType (drcEventRates rateCard) of
      Nothing -> Left "Domo event type is not included in the approved rate card"
      Just eventRate -> calculateFor eventRate
  where
    normalizedEventType = T.toLower (T.strip (dqiEventType input))
    monetaryRates =
      [ drcHourMinor rateCard
      , drcSetupHourMinor rateCard
      , drcCateringMinimumMinor rateCard
      , drcCateringPerGuestMinor rateCard
      , drcProductionMinor rateCard
      , drcTransportMinor rateCard
      ]
    calculateFor eventRate
      | derBaseMinor eventRate <= 0 = Left "Domo event base rate must be greater than zero"
      | derPerGuestMinor eventRate < 0 = Left "Domo per-guest rate must not be negative"
      | derMinimumHours eventRate <= 0
          || derMinimumHours eventRate > drcMaximumDurationHours rateCard =
          Left "Domo event minimum duration is outside the approved rate card limits"
      | derIncludedGuests eventRate < 0
          || derIncludedGuests eventRate > drcMaximumGuests rateCard =
          Left "Domo included guest count is outside the approved rate card limits"
      | any (> maxInt64) allAmounts = Left "Domo quote total is too large"
      | total <= 0 || deposit <= 0 = Left "Domo quote and deposit must be greater than zero"
      | otherwise = Right DomoQuoteBreakdown
          { dqbLines = quoteLines
          , dqbBillableHours = billableHours
          , dqbSubtotalMinor = fromInteger subtotal
          , dqbTaxMinor = fromInteger tax
          , dqbTotalMinor = fromInteger total
          , dqbDepositMinor = fromInteger deposit
          , dqbBalanceMinor = fromInteger (total - deposit)
          }
      where
        billableHours = max (derMinimumHours eventRate) (dqiDurationHours input)
        extraGuests = max 0 (dqiGuests input - derIncludedGuests eventRate)
        rawLines =
          [ ("venue_base", "Domo event base", 1, toInteger (derBaseMinor eventRate))
          , ("venue_hours", "Domo venue hours", billableHours,
              toInteger (drcHourMinor rateCard))
          ]
          <> optionalLine (dqiSetupHours input > 0)
              ("setup_hours", "Setup and teardown hours", dqiSetupHours input,
                toInteger (drcSetupHourMinor rateCard))
          <> optionalLine (extraGuests > 0)
              ("additional_guests", "Additional guests", extraGuests,
                toInteger (derPerGuestMinor eventRate))
          <> optionalLine (dqiCatering input)
              ("catering", "Catering and bar", 1,
                max (toInteger (drcCateringMinimumMinor rateCard))
                  (toInteger (dqiGuests input) * toInteger (drcCateringPerGuestMinor rateCard)))
          <> optionalLine (dqiProduction input)
              ("production", "Sound and lighting", 1,
                toInteger (drcProductionMinor rateCard))
          <> optionalLine (dqiTransport input)
              ("transport", "Quito to Pululahua transport coordination", 1,
                toInteger (drcTransportMinor rateCard))
        lineAmounts = [toInteger quantity * unit | (_, _, quantity, unit) <- rawLines]
        subtotal = sum lineAmounts
        tax = roundBasisPoints subtotal (drcTaxBasisPoints rateCard)
        total = subtotal + tax
        deposit = roundBasisPoints total (drcDepositBasisPoints rateCard)
        allAmounts = lineAmounts <> [subtotal, tax, total, deposit]
        quoteLines = zipWith toLine rawLines lineAmounts
        toLine (code, description, quantity, unit) lineSubtotal = DomoQuoteLine
          { dqlCode = code
          , dqlDescription = description
          , dqlQuantity = quantity
          , dqlUnitAmountMinor = fromInteger unit
          , dqlSubtotalMinor = fromInteger lineSubtotal
          }
    maxInt64 = toInteger (maxBound :: Int64)

optionalLine :: Bool -> a -> [a]
optionalLine include value = [value | include]

roundBasisPoints :: Integer -> Int -> Integer
roundBasisPoints amount basisPoints =
  (amount * toInteger basisPoints + 5000) `div` 10000

parseDomoQuoteState :: Text -> Either Text DomoQuoteState
parseDomoQuoteState raw = case T.toLower (T.strip raw) of
  "draft" -> Right DomoDraft
  "sent" -> Right DomoSent
  "viewed" -> Right DomoViewed
  "accepted" -> Right DomoAccepted
  "deposit_due" -> Right DomoDepositDue
  "deposit_paid" -> Right DomoDepositPaid
  "in_progress" -> Right DomoInProgress
  "balance_due" -> Right DomoBalanceDue
  "completed" -> Right DomoCompleted
  "cancelled" -> Right DomoCancelled
  "expired" -> Right DomoExpired
  _ -> Left "Unknown Domo quote state"

domoQuoteStateText :: DomoQuoteState -> Text
domoQuoteStateText state = case state of
  DomoDraft -> "draft"
  DomoSent -> "sent"
  DomoViewed -> "viewed"
  DomoAccepted -> "accepted"
  DomoDepositDue -> "deposit_due"
  DomoDepositPaid -> "deposit_paid"
  DomoInProgress -> "in_progress"
  DomoBalanceDue -> "balance_due"
  DomoCompleted -> "completed"
  DomoCancelled -> "cancelled"
  DomoExpired -> "expired"

validateDomoQuoteTransition :: DomoQuoteState -> DomoQuoteState -> Either Text ()
validateDomoQuoteTransition fromState toState
  | fromState == toState = Right ()
  | (fromState, toState) `elem` allowedTransitions = Right ()
  | otherwise = Left "Domo quote transition is not allowed"
  where
    allowedTransitions =
      [ (DomoDraft, DomoSent)
      , (DomoDraft, DomoCancelled)
      , (DomoSent, DomoViewed)
      , (DomoSent, DomoAccepted)
      , (DomoSent, DomoExpired)
      , (DomoSent, DomoCancelled)
      , (DomoViewed, DomoAccepted)
      , (DomoViewed, DomoExpired)
      , (DomoViewed, DomoCancelled)
      , (DomoAccepted, DomoDepositDue)
      , (DomoAccepted, DomoCancelled)
      , (DomoAccepted, DomoExpired)
      , (DomoDepositDue, DomoDepositPaid)
      , (DomoDepositDue, DomoCancelled)
      , (DomoDepositDue, DomoExpired)
      , (DomoDepositPaid, DomoInProgress)
      , (DomoDepositPaid, DomoCancelled)
      , (DomoInProgress, DomoBalanceDue)
      , (DomoInProgress, DomoCancelled)
      , (DomoBalanceDue, DomoCompleted)
      , (DomoBalanceDue, DomoCancelled)
      ]
