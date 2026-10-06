{-# LANGUAGE OverloadedStrings #-}

-- | Per-ticket projection around the existing canonical financial refund store.
-- Call each operation in ONE transaction, before any provider HTTP request.
-- SQL failures intentionally escape: no partial financial/ticket commit is safe.
module TDF.Ticketing.Refund
  ( requestTicketRefund, completeTicketRefund, cancelTicketRefund, ticketRefundAmounts ) where

import Control.Monad (forM_, unless, when)
import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql
import qualified TDF.Models.SocialEventsModels as M
import qualified TDF.Commerce.RefundStore as R
import qualified TDF.Commerce.CheckoutStore as C

-- Stable ticket-id order assigns the remainder cents once. Integer arithmetic
-- avoids overflow; splitting a refund cannot create or lose a cent.
ticketRefundAmounts :: Int64 -> [Int64] -> Either Text [(Int64,Int64)]
ticketRefundAmounts total keys
  | null keys || length keys > 100 || any (<= 0) keys || length (nub keys) /= length keys =
      Left "Ticket identities must be unique, positive and bounded"
  | total < fromIntegral (length keys) = Left "Ticket total must fund every ticket"
  | otherwise = Right $ zipWith (\index key ->
      (key, fromInteger (base + if index < remainder then 1 else 0))) [0..] (sort keys)
  where (base,remainder) = toInteger total `divMod` toInteger (length keys)

-- Event -> order -> tickets is the same lock order as admission and transfer.
withOrder :: M.EventTicketOrderId
  -> (M.SocialEvent -> M.EventTicketOrder -> [Entity M.EventTicket] -> SqlPersistT IO a)
  -> SqlPersistT IO a
withOrder key action = do
  initial <- get key >>= maybe (fail "Ticket order not found") pure
  events <- rawSql "SELECT ?? FROM social_event WHERE id=? FOR UPDATE"
    [toPersistValue (M.eventTicketOrderEventId initial)]
  orders <- rawSql "SELECT ?? FROM event_ticket_order WHERE id=? FOR UPDATE" [toPersistValue key]
  tickets <- rawSql "SELECT ?? FROM event_ticket WHERE order_ref_id=? ORDER BY id FOR UPDATE"
    [toPersistValue key]
  case (events,orders) of
    ([Entity eventKey event],[Entity _ order])
      | M.eventTicketOrderEventId order == eventKey
      , length tickets == M.eventTicketOrderQuantity order
      , all (\(Entity _ ticket) -> M.eventTicketEventId ticket == eventKey &&
          M.eventTicketTierRefId ticket == M.eventTicketOrderTierId order) tickets ->
          action event order tickets
    _ -> fail "Ticket refund order relationships are inconsistent"

orderForRefund :: R.RefundReference -> SqlPersistT IO M.EventTicketOrderId
orderForRefund ref = do
  rows <- rawSql
    "SELECT runtime.order_id FROM event_ticket_checkout_runtime runtime\
    \ JOIN commerce_refund refund ON refund.checkout_id=runtime.checkout_id\
    \ WHERE refund.id=?::uuid" [PersistText (R.refundReferenceId ref)]
  case rows of
    [Single key] -> pure key
    _ -> fail "Refund must bind exactly one canonical ticket order"

loadRecord :: R.RefundReference -> SqlPersistT IO R.RefundRecord
loadRecord ref = R.loadRefund ref >>= maybe (fail "Refund not found") pure

allocationRows :: R.RefundReference -> SqlPersistT IO [(Single M.EventTicketId,Single Int64,Single Text)]
allocationRows ref = rawSql
  "SELECT ticket_id,amount_minor,state FROM event_ticket_refund_allocation\
  \ WHERE refund_id=?::uuid ORDER BY ticket_id FOR UPDATE"
  [PersistText (R.refundReferenceId ref)]

-- Acquire admission locks BEFORE the canonical checkout/refund locks. The
-- canonical request and ticket reservation must never be separate transactions.
requestTicketRefund :: Text -> M.SocialEventId -> M.EventTicketOrderId
  -> R.RefundCreation -> [M.EventTicketId] -> SqlPersistT IO (Either Text R.RefundRecord)
requestTicketRefund actor eventKey orderKey creation requested = withOrder orderKey $ \_ _ _ -> do
  result <- R.requestSingleLineRefund creation
  case result of
    Left problem -> pure (Left problem)
    Right record -> do
      reserveTicketRefund actor eventKey orderKey (R.rrReference record) requested (R.rcCreatedAt creation)
      pure (Right record)

-- The authenticated requester must own the order or manage the event. A
-- transferred ticket cannot be reclaimed by its former buyer. Manager-initiated
-- refunds still require a different approver in RefundStore.
reserveTicketRefund :: Text -> M.SocialEventId -> M.EventTicketOrderId
  -> R.RefundReference -> [M.EventTicketId] -> UTCTime -> SqlPersistT IO ()
reserveTicketRefund actor eventKey orderKey ref requested now = withOrder orderKey $
  \event order tickets -> do
    record <- loadRecord ref
    unless (not (T.null actor) && R.rrRequestedBy record > 0 &&
        actor == T.pack (show (R.rrRequestedBy record)) &&
        M.eventTicketOrderEventId order == eventKey &&
        (M.socialEventOrganizerPartyId event == Just actor ||
          M.eventTicketOrderBuyerPartyId order == Just actor)) $
      fail "Ticket refund requester is not authorized"
    boundOrder <- orderForRefund ref
    unless (boundOrder == orderKey) $ fail "Refund belongs to a different ticket order"
    totals <- rawSql
      "SELECT checkout_total_minor FROM event_ticket_checkout_runtime WHERE order_id=?\
      \ AND checkout_id=?::uuid AND event_id=? AND currency=?"
      [toPersistValue orderKey, PersistText (C.checkoutReferenceId (R.rrCheckout record)),
       toPersistValue eventKey, PersistText (R.rrCurrency record)]
    total <- case totals of
      [Single value] -> pure value
      _ -> fail "Refund checkout snapshot is inconsistent"
    unless (M.eventTicketOrderCurrency order == R.rrCurrency record &&
        toInteger (M.eventTicketOrderAmountCents order) == toInteger total) $
      fail "Legacy ticket amount differs from the immutable checkout"
    amounts <- either (fail . T.unpack) pure $
      ticketRefundAmounts total (map (fromSqlKey . entityKey) tickets)
    let selected = filter (\(key,_) -> toSqlKey key `elem` requested) amounts
    unless (not (null requested) && length requested == length (nub requested) &&
        length selected == length requested &&
        sum (map (toInteger . snd) selected) == toInteger (R.rrAmountMinor record)) $
      fail "Refund amount must exactly match the selected whole tickets"
    existing <- allocationRows ref
    if not (null existing) then
      unless (map (\(Single key,Single amount,_) -> (fromSqlKey key,amount)) existing == selected) $
        fail "Refund replay changed the ticket selection"
    else do
      unless (R.rrStatus record == "requested" && M.eventTicketOrderStatus order == "paid") $
        fail "Only a newly requested paid ticket refund may reserve tickets"
      forM_ tickets $ \(Entity key ticket) -> when (key `elem` requested) $ do
        unless (M.eventTicketStatus ticket == "issued" && M.eventTicketCheckedInAt ticket == Nothing &&
            M.eventTicketCurrentHolderPartyId ticket == M.eventTicketOriginalHolderPartyId ticket) $
          fail "Used, transferred or unavailable ticket cannot be refunded"
        pending <- selectFirst [M.TicketTransferTicketId ==. key,M.TicketTransferStatus ==. "pending"] []
        unless (maybe True (const False) pending) $ fail "Cancel the pending transfer before requesting a refund"
      forM_ selected $ \(key,amount) -> do
        rawExecute
          "INSERT INTO event_ticket_refund_allocation(refund_id,ticket_id,order_id,amount_minor,created_at,updated_at)\
          \ VALUES (?::uuid,?,?,?,?,?)"
          [PersistText (R.refundReferenceId ref),PersistInt64 key,toPersistValue orderKey,
           PersistInt64 amount,PersistUTCTime now,PersistUTCTime now]
        update (toSqlKey key) [M.EventTicketStatus =. "refund_pending",M.EventTicketUpdatedAt =. now]

-- Financial verification and all ticket/inventory projections commit together.
-- This accepts ONLY an already allocated refund, never infers tickets from an
-- unallocated external refund. Such events retain the admission review fence.
completeTicketRefund :: R.VerifiedRefund -> SqlPersistT IO Bool
completeTicketRefund verified = do
  let ref = R.vrRefund verified
      now = R.vrOccurredAt verified
  key <- orderForRefund ref
  withOrder key $ \_ order tickets -> do
    rows <- allocationRows ref
    record <- loadRecord ref
    unless (not (null rows) && sum [toInteger amount | (_,Single amount,_) <- rows] ==
        toInteger (R.rrAmountMinor record)) $ fail "Refund allocation is incomplete"
    let allocated = [ticketKey | (Single ticketKey,_,_) <- rows]
        selected = filter (\(Entity ticketKey _) -> ticketKey `elem` allocated) tickets
    unless (length selected == length rows) $ fail "Refund ticket allocation changed orders"
    if R.rrStatus record == "succeeded" then do
      unless (all (\(_,_,Single state) -> state == "completed") rows &&
          all (\(Entity _ ticket) -> M.eventTicketStatus ticket == "refunded") selected) $
        fail "Succeeded refund requires ticket reconciliation"
      either (fail . T.unpack) pure =<< R.recordVerifiedRefund verified
    else do
      unless (all (\(_,_,Single state) -> state == "reserved") rows &&
          all (\(Entity _ ticket) -> M.eventTicketStatus ticket == "refund_pending" &&
            M.eventTicketCheckedInAt ticket == Nothing) selected) $
        fail "Refund tickets are not safely reserved"
      changed <- either (fail . T.unpack) pure =<< R.recordVerifiedRefund verified
      unless changed $ fail "Refund completion was not singular"
      forM_ allocated $ \ticketKey -> update ticketKey
        [M.EventTicketStatus =. "refunded",M.EventTicketUpdatedAt =. now]
      released <- rawSql
        "UPDATE event_ticket_tier SET quantity_sold=quantity_sold-?,updated_at=?\
        \ WHERE id=? AND event_id=? AND quantity_sold>=? RETURNING id"
        [PersistInt64 (fromIntegral (length rows)),PersistUTCTime now,
         toPersistValue (M.eventTicketOrderTierId order),toPersistValue (M.eventTicketOrderEventId order),
         PersistInt64 (fromIntegral (length rows))] :: SqlPersistT IO [Single Int64]
      unless (length released == 1) $ fail "Refund inventory release would underflow"
      let allRefunded = all (\(Entity ticketKey ticket) -> ticketKey `elem` allocated ||
            M.eventTicketStatus ticket == "refunded") tickets
      update key [M.EventTicketOrderStatus =. (if allRefunded then "refunded" else "paid"),
        M.EventTicketOrderUpdatedAt =. now]
      rawExecute
        "UPDATE event_ticket_refund_allocation SET state='completed',updated_at=?\
        \ WHERE refund_id=?::uuid AND state='reserved'"
        [PersistUTCTime now,PersistText (R.refundReferenceId ref)]
      pure True

cancelTicketRefund :: Text -> R.RefundReference -> UTCTime -> SqlPersistT IO ()
cancelTicketRefund actor ref now = do
  key <- orderForRefund ref
  withOrder key $ \event _ tickets -> do
    record <- loadRecord ref
    unless (not (T.null actor) && (M.socialEventOrganizerPartyId event == Just actor ||
        actor == T.pack (show (R.rrRequestedBy record)))) $ fail "Refund cancellation is not authorized"
    rows <- allocationRows ref
    unless (not (null rows)) $ fail "Refund allocation is absent"
    let allocated = [ticketKey | (Single ticketKey,_,_) <- rows]
    if R.rrStatus record == "cancelled" then
      unless (all (\(_,_,Single state) -> state == "cancelled") rows) $
        fail "Cancelled refund requires ticket reconciliation"
    else do
      unless (all (\(_,_,Single state) -> state == "reserved") rows &&
        all (\(Entity ticketKey ticket) -> ticketKey `notElem` allocated ||
          M.eventTicketStatus ticket == "refund_pending") tickets) $
        fail "Refund tickets cannot be released"
      actorId <- case reads (T.unpack actor) of
        [(value,"")] | value > 0 -> pure value
        _ -> fail "Refund cancellation requires an authenticated party"
      _ <- either (fail . T.unpack) pure =<< R.cancelRefundRequest ref actorId now
      forM_ allocated $ \ticketKey -> update ticketKey
        [M.EventTicketStatus =. "issued",M.EventTicketUpdatedAt =. now]
      rawExecute
        "UPDATE event_ticket_refund_allocation SET state='cancelled',updated_at=?\
        \ WHERE refund_id=?::uuid AND state='reserved'"
        [PersistUTCTime now,PersistText (R.refundReferenceId ref)]
