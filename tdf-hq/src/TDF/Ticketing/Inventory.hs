{-# LANGUAGE OverloadedStrings #-}

module TDF.Ticketing.Inventory (reserveTicketInventory) where

import Data.Int (Int64)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql
import qualified TDF.Models.SocialEventsModels as M

-- | Call in the transaction that creates the hold/order. Locks serialize tiers
-- sharing an event capacity. A failed order must roll back this reservation.
-- This is inventory authority, not payment or event-publication authorization.
reserveTicketInventory :: M.SocialEventId -> M.EventTicketTierId -> Int -> UTCTime
  -> SqlPersistT IO Bool
reserveTicketInventory eventKey tierKey quantity now
  | quantity <= 0 || quantity > 100 = pure False
  | otherwise = do
      events <- rawSql "SELECT ?? FROM social_event WHERE id=? FOR UPDATE" [toPersistValue eventKey]
      tiers <- rawSql "SELECT ?? FROM event_ticket_tier WHERE id=? AND event_id=? FOR UPDATE"
        [toPersistValue tierKey, toPersistValue eventKey]
      case (events, tiers) of
        ([Entity _ event], [Entity _ tier])
          | M.eventTicketTierIsActive tier
          , maybe True (<= now) (M.eventTicketTierSalesStart tier)
          , maybe True (now <=) (M.eventTicketTierSalesEnd tier) -> do
              soldRows <- rawSql
                "SELECT COALESCE(sum(quantity_sold),0)::bigint FROM event_ticket_tier WHERE event_id=?"
                [toPersistValue eventKey]
              let capacityAvailable = case soldRows of
                    [Single sold] -> maybe True
                      (\capacity -> toInteger (sold :: Int64) + toInteger quantity <= toInteger capacity)
                      (M.socialEventCapacity event)
                    _ -> False
              if not capacityAvailable then pure False else do
                changed <- updateWhereCount
                  [ M.EventTicketTierId ==. tierKey
                  , M.EventTicketTierIsActive ==. True
                  , M.EventTicketTierQuantitySold <=. M.eventTicketTierQuantityTotal tier - quantity
                  ]
                  [ M.EventTicketTierQuantitySold +=. quantity
                  , M.EventTicketTierUpdatedAt =. now
                  ]
                pure (changed == 1)
        _ -> pure False
