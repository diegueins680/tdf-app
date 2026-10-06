{-# LANGUAGE OverloadedStrings #-}

-- | Transfer capabilities are single-use invitations, not proof of email ownership.
-- All mutations must run in one runSqlPool transaction. Lock order matches admission.
module TDF.Ticketing.Transfer
  ( createTransfer, acceptTransfer, cancelTransfer, retainedByBuyer ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql
import qualified TDF.Models.SocialEventsModels as M

-- Order lookup capabilities authorize the buyer, never a subsequent holder's QR.
retainedByBuyer :: M.EventTicket -> Bool
retainedByBuyer ticket =
  M.eventTicketCurrentHolderPartyId ticket == M.eventTicketOriginalHolderPartyId ticket

withTicket :: M.EventTicketId
  -> (M.SocialEvent -> M.EventTicketOrder -> M.EventTicket -> SqlPersistT IO (Either Text a))
  -> SqlPersistT IO (Either Text a)
withTicket key action = do
  initial <- get key
  case initial of
    Nothing -> pure (Left "Ticket not found")
    Just snapshot -> do
      events <- rawSql "SELECT ?? FROM social_event WHERE id=? FOR UPDATE"
        [toPersistValue (M.eventTicketEventId snapshot)]
      orders <- rawSql "SELECT ?? FROM event_ticket_order WHERE id=? FOR UPDATE"
        [toPersistValue (M.eventTicketOrderRefId snapshot)]
      tickets <- rawSql "SELECT ?? FROM event_ticket WHERE id=? FOR UPDATE" [toPersistValue key]
      case (events, orders, tickets) of
        ([Entity eventKey event], [Entity orderKey order], [Entity _ ticket])
          | M.eventTicketEventId ticket == eventKey
          , M.eventTicketOrderRefId ticket == orderKey
          , M.eventTicketOrderEventId order == eventKey
          , M.eventTicketOrderTierId order == M.eventTicketTierRefId ticket -> action event order ticket
        _ -> pure (Left "Ticket relationships changed")

mayTransfer :: Text -> M.SocialEvent -> M.EventTicket -> Bool
mayTransfer actor event ticket = not (T.null (T.strip actor)) &&
  (M.eventTicketCurrentHolderPartyId ticket == Just actor ||
    fmap T.strip (M.socialEventOrganizerPartyId event) == Just actor)

eligible :: UTCTime -> M.SocialEvent -> M.EventTicketOrder -> M.EventTicket
  -> SqlPersistT IO Bool
eligible now event order ticket = do
  tier <- get (M.eventTicketTierRefId ticket)
  policies <- rawSql
    "SELECT p.transfer_allowed,p.transfer_deadline,r.payment_status FROM event_ticket_checkout_runtime r JOIN event_ticket_checkout_policy p ON p.id=r.policy_id WHERE r.order_id=? AND r.event_id=? AND p.event_id=r.event_id"
    [toPersistValue (M.eventTicketOrderRefId ticket), toPersistValue (M.eventTicketEventId ticket)]
  runtimes <- rawSql "SELECT count(*) FROM event_ticket_checkout_runtime WHERE order_id=?"
    [toPersistValue (M.eventTicketOrderRefId ticket)] :: SqlPersistT IO [Single Int]
  let policyAllows = case (runtimes, policies) of
        ([Single 0], []) -> True -- Existing orders without the public checkout runtime.
        ([Single 1], [(Single allowed, Single deadline, Single paymentStatus)]) ->
          allowed && paymentStatus `elem` (["paid", "partially_refunded"] :: [Text])
            && maybe True (now <) deadline
        _ -> False
  pure $ M.eventTicketOrderStatus order == "paid" && M.eventTicketStatus ticket == "issued"
    && M.eventTicketCheckedInAt ticket == Nothing && now < M.socialEventStartTime event
    && policyAllows
    && maybe False (\value -> M.eventTicketTierAllowTransfers value &&
      M.eventTicketTierEventId value == M.eventTicketEventId ticket) tier

createTransfer :: Text -> M.SocialEventId -> M.TicketTransfer -> UTCTime
  -> SqlPersistT IO (Either Text (Entity M.TicketTransfer))
createTransfer actor eventKey proposal now = withTicket (M.ticketTransferTicketId proposal) $
  \event order ticket -> do
    allowed <- eligible now event order ticket
    if not allowed || M.eventTicketEventId ticket /= eventKey || not (mayTransfer actor event ticket)
      then pure (Left "Ticket is not eligible for this transfer")
      else do
        updateWhere [M.TicketTransferTicketId ==. M.ticketTransferTicketId proposal,
          M.TicketTransferStatus ==. "pending", M.TicketTransferExpiresAt <=. Just now]
          [M.TicketTransferStatus =. "expired", M.TicketTransferUpdatedAt =. now]
        pending <- selectFirst [M.TicketTransferTicketId ==. M.ticketTransferTicketId proposal,
          M.TicketTransferStatus ==. "pending"] []
        case pending of
          Just _ -> pure (Left "A pending transfer already exists for this ticket")
          Nothing -> do
            let value = proposal { M.ticketTransferFromPartyId = Just actor,
                  M.ticketTransferToPartyId = Nothing, M.ticketTransferStatus = "pending",
                  M.ticketTransferAcceptedAt = Nothing,
                  M.ticketTransferExpiresAt = Just (min (M.socialEventStartTime event)
                    (maybe (M.socialEventStartTime event) id (M.ticketTransferExpiresAt proposal))) }
            key <- insert value
            pure (Right (Entity key value))

acceptTransfer :: Text -> Text -> Text -> UTCTime
  -> SqlPersistT IO (Either Text (Entity M.EventTicket))
acceptTransfer actor invitation replacement now = do
  initial <- getBy (M.UniqueTicketTransferCode invitation)
  case initial of
    Nothing -> pure (Left "Transfer not found")
    Just (Entity transferKey snapshot) -> withTicket (M.ticketTransferTicketId snapshot) $
      \event order ticket -> do
        transfers <- rawSql "SELECT ?? FROM ticket_transfer WHERE id=? FOR UPDATE"
          [toPersistValue transferKey]
        allowed <- eligible now event order ticket
        case transfers of
          [Entity _ transfer]
            | allowed, not (T.null (T.strip actor))
            , M.ticketTransferStatus transfer == "pending"
            , M.ticketTransferTransferCode transfer == invitation
            , maybe False (> now) (M.ticketTransferExpiresAt transfer)
            , maybe False (\creator -> mayTransfer creator event ticket)
                (M.ticketTransferFromPartyId transfer) -> do
                let ticketKey = M.ticketTransferTicketId transfer
                update transferKey [M.TicketTransferStatus =. "completed",
                  M.TicketTransferToPartyId =. Just actor, M.TicketTransferAcceptedAt =. Just now,
                  M.TicketTransferUpdatedAt =. now]
                updateWhere [M.TicketTransferTicketId ==. ticketKey,
                  M.TicketTransferStatus ==. "pending"]
                  [M.TicketTransferStatus =. "cancelled", M.TicketTransferUpdatedAt =. now]
                update ticketKey [M.EventTicketCurrentHolderPartyId =. Just actor,
                  M.EventTicketCurrentHolderEmail =. M.ticketTransferToEmail transfer,
                  M.EventTicketCurrentHolderName =. M.ticketTransferToName transfer,
                  M.EventTicketCode =. replacement, M.EventTicketUpdatedAt =. now]
                stored <- getEntity ticketKey
                maybe (fail "Transferred ticket disappeared") (pure . Right) stored
          _ -> pure (Left "Transfer is no longer available")

cancelTransfer :: Text -> M.TicketTransferId -> UTCTime
  -> SqlPersistT IO (Either Text (Entity M.TicketTransfer))
cancelTransfer actor key now = do
  initial <- get key
  case initial of
    Nothing -> pure (Left "Transfer not found")
    Just snapshot -> withTicket (M.ticketTransferTicketId snapshot) $ \_ _ _ -> do
      transfers <- rawSql "SELECT ?? FROM ticket_transfer WHERE id=? FOR UPDATE" [toPersistValue key]
      case transfers of
        [Entity _ transfer]
          | M.ticketTransferFromPartyId transfer == Just actor
          , M.ticketTransferStatus transfer == "pending" -> do
              update key [M.TicketTransferStatus =. "cancelled", M.TicketTransferUpdatedAt =. now]
              stored <- getEntity key
              maybe (fail "Cancelled transfer disappeared") (pure . Right) stored
        _ -> pure (Left "Transfer is no longer cancellable")
