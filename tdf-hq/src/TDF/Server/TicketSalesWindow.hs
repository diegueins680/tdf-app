{-# LANGUAGE OverloadedStrings #-}

-- | When an event may sell tickets: its workflow state must allow purchases
-- and the event must not have ended.
module TDF.Server.TicketSalesWindow (
    eventTicketPurchaseEnabledFor,
    ticketSalesOpenBeforeEventEnd,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Sql (SqlPersistT)

import TDF.Models.SocialEventsModels
import qualified TDF.SocialEventLifecycle as EventLifecycle

eventTicketPurchaseEnabledFor :: SocialEvent -> SqlPersistT IO Bool
eventTicketPurchaseEnabledFor eventRow =
    case socialEventWorkflowStateId eventRow of
        Nothing -> pure False
        Just stateId -> do
            now <- liftIO getCurrentTime
            if ticketSalesOpenBeforeEventEnd now (socialEventEndTime eventRow)
                then EventLifecycle.socialEventStateHasCapability stateId "ticket-purchase"
                else pure False

-- | An event that has already ended sells no more tickets, whatever workflow
-- state it was left in. An event without an end time is governed by its
-- workflow state alone.
ticketSalesOpenBeforeEventEnd :: UTCTime -> Maybe UTCTime -> Bool
ticketSalesOpenBeforeEventEnd now = maybe True (now <)
