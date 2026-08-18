{-# LANGUAGE OverloadedStrings #-}

module TDF.Commerce.MarketplaceSales
  ( MarketplaceFulfillmentMethod(..)
  , MarketplaceFulfillmentState(..)
  , parseMarketplaceFulfillmentMethod
  , parseMarketplaceFulfillmentState
  , marketplaceFulfillmentMethodText
  , marketplaceFulfillmentStateText
  , validateMarketplaceFulfillmentTransition
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

data MarketplaceFulfillmentMethod
  = MarketplacePickup
  | MarketplaceLocalDelivery
  | MarketplaceShipping
  deriving (Eq, Show)

data MarketplaceFulfillmentState
  = MarketplaceOnHold
  | MarketplaceReadyToFulfill
  | MarketplacePicking
  | MarketplaceReadyForPickup
  | MarketplaceShipped
  | MarketplaceDelivered
  | MarketplaceCancellationRequested
  | MarketplaceCancelled
  | MarketplaceReturnRequested
  | MarketplaceReturnAuthorized
  | MarketplaceReturnInTransit
  | MarketplaceReturned
  | MarketplaceClosed
  | MarketplaceExpired
  deriving (Eq, Show)

parseMarketplaceFulfillmentMethod :: Text -> Either Text MarketplaceFulfillmentMethod
parseMarketplaceFulfillmentMethod rawMethod =
  case T.toLower (T.strip rawMethod) of
    "pickup" -> Right MarketplacePickup
    "local_delivery" -> Right MarketplaceLocalDelivery
    "shipping" -> Right MarketplaceShipping
    _ -> Left "Fulfillment method must be pickup, local_delivery, or shipping"

marketplaceFulfillmentMethodText :: MarketplaceFulfillmentMethod -> Text
marketplaceFulfillmentMethodText method = case method of
  MarketplacePickup -> "pickup"
  MarketplaceLocalDelivery -> "local_delivery"
  MarketplaceShipping -> "shipping"

parseMarketplaceFulfillmentState :: Text -> Either Text MarketplaceFulfillmentState
parseMarketplaceFulfillmentState rawState =
  case T.toLower (T.strip rawState) of
    "on_hold" -> Right MarketplaceOnHold
    "ready_to_fulfill" -> Right MarketplaceReadyToFulfill
    "picking" -> Right MarketplacePicking
    "ready_for_pickup" -> Right MarketplaceReadyForPickup
    "shipped" -> Right MarketplaceShipped
    "delivered" -> Right MarketplaceDelivered
    "cancellation_requested" -> Right MarketplaceCancellationRequested
    "cancelled" -> Right MarketplaceCancelled
    "return_requested" -> Right MarketplaceReturnRequested
    "return_authorized" -> Right MarketplaceReturnAuthorized
    "return_in_transit" -> Right MarketplaceReturnInTransit
    "returned" -> Right MarketplaceReturned
    "closed" -> Right MarketplaceClosed
    "expired" -> Right MarketplaceExpired
    _ -> Left "Unknown marketplace fulfillment state"

marketplaceFulfillmentStateText :: MarketplaceFulfillmentState -> Text
marketplaceFulfillmentStateText state = case state of
  MarketplaceOnHold -> "on_hold"
  MarketplaceReadyToFulfill -> "ready_to_fulfill"
  MarketplacePicking -> "picking"
  MarketplaceReadyForPickup -> "ready_for_pickup"
  MarketplaceShipped -> "shipped"
  MarketplaceDelivered -> "delivered"
  MarketplaceCancellationRequested -> "cancellation_requested"
  MarketplaceCancelled -> "cancelled"
  MarketplaceReturnRequested -> "return_requested"
  MarketplaceReturnAuthorized -> "return_authorized"
  MarketplaceReturnInTransit -> "return_in_transit"
  MarketplaceReturned -> "returned"
  MarketplaceClosed -> "closed"
  MarketplaceExpired -> "expired"

validateMarketplaceFulfillmentTransition
  :: MarketplaceFulfillmentMethod
  -> MarketplaceFulfillmentState
  -> MarketplaceFulfillmentState
  -> Either Text ()
validateMarketplaceFulfillmentTransition method fromState toState
  | fromState == toState = Right ()
  | (fromState, toState) `elem` commonTransitions = Right ()
  | method == MarketplacePickup
      && (fromState, toState) == (MarketplacePicking, MarketplaceReadyForPickup) = Right ()
  | method /= MarketplacePickup
      && (fromState, toState) == (MarketplacePicking, MarketplaceShipped) = Right ()
  | otherwise = Left "Marketplace fulfillment transition is not allowed"
  where
    commonTransitions =
      [ (MarketplaceOnHold, MarketplaceReadyToFulfill)
      , (MarketplaceOnHold, MarketplaceCancelled)
      , (MarketplaceOnHold, MarketplaceExpired)
      , (MarketplaceReadyToFulfill, MarketplacePicking)
      , (MarketplaceReadyToFulfill, MarketplaceCancellationRequested)
      , (MarketplacePicking, MarketplaceCancellationRequested)
      , (MarketplaceReadyForPickup, MarketplaceDelivered)
      , (MarketplaceReadyForPickup, MarketplaceCancellationRequested)
      , (MarketplaceShipped, MarketplaceDelivered)
      , (MarketplaceCancellationRequested, MarketplaceCancelled)
      , (MarketplaceDelivered, MarketplaceReturnRequested)
      , (MarketplaceDelivered, MarketplaceClosed)
      , (MarketplaceReturnRequested, MarketplaceReturnAuthorized)
      , (MarketplaceReturnRequested, MarketplaceClosed)
      , (MarketplaceReturnAuthorized, MarketplaceReturnInTransit)
      , (MarketplaceReturnAuthorized, MarketplaceReturned)
      , (MarketplaceReturnInTransit, MarketplaceReturned)
      , (MarketplaceReturned, MarketplaceClosed)
      ]
