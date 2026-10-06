{-# LANGUAGE OverloadedStrings #-}

-- | Per-ticket projection around the existing canonical financial refund store.
-- Call each operation in ONE transaction, before any provider HTTP request.
-- SQL failures intentionally escape: no partial financial/ticket commit is safe.
module TDF.Ticketing.Refund
  ( requestTicketRefund, completeTicketRefund, cancelTicketRefund, ticketRefundAmounts
  , requestTicketRefundForOrder, loadTicketRefundReference, withOrder, cancelTicketRefundAs, capturedTicketRevenue ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql
import qualified TDF.Models.SocialEventsModels as M
import TDF.Auth (AuthedUser(..), hasStrictAdminAccess)
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
      rawExecute
        "UPDATE ticket_refund_request request SET status='approved',processed_at=?,updated_at=?\
        \ FROM event_ticket_refund_request_binding binding WHERE binding.request_id=request.id\
        \ AND binding.refund_id=?::uuid"
        [PersistUTCTime now,PersistUTCTime now,PersistText (R.refundReferenceId ref)]
      pure True

cancelTicketRefund :: Text -> R.RefundReference -> UTCTime -> SqlPersistT IO ()
cancelTicketRefund = cancelTicketRefundWithAuthority False

cancelTicketRefundAs :: AuthedUser -> R.RefundReference -> UTCTime -> SqlPersistT IO ()
cancelTicketRefundAs user = cancelTicketRefundWithAuthority (hasStrictAdminAccess user)
  (T.pack (show (fromSqlKey (auPartyId user))))

cancelTicketRefundWithAuthority :: Bool -> Text -> R.RefundReference -> UTCTime -> SqlPersistT IO ()
cancelTicketRefundWithAuthority strictAdmin actor ref now = do
  key <- orderForRefund ref
  withOrder key $ \event _ tickets -> do
    record <- loadRecord ref
    unless (not (T.null actor) && (strictAdmin || M.socialEventOrganizerPartyId event == Just actor ||
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

-- Reuses the legacy organizer request identity; canonical refund UUID is a
-- separate immutable binding, never stored in the Stripe-specific column.
loadTicketRefundReference :: M.TicketRefundRequestId -> SqlPersistT IO (Maybe R.RefundReference)
loadTicketRefundReference key = do
  rows <- rawSql "SELECT CAST(refund_id AS TEXT) FROM event_ticket_refund_request_binding WHERE request_id=?"
    [toPersistValue key]
  case rows of
    [] -> pure Nothing
    [Single ref] -> pure (Just (R.RefundReference ref))
    _ -> fail "Ticket refund request binding is ambiguous"

requestTicketRefundForOrder :: Text -> M.SocialEventId -> M.EventTicketOrderId
  -> Maybe Int -> Maybe Text -> UTCTime -> SqlPersistT IO (Entity M.TicketRefundRequest)
requestTicketRefundForOrder actor eventKey orderKey requestedAmount reason now = withOrder orderKey $
  \event order tickets -> do
    unless (M.eventTicketOrderEventId order == eventKey && not (T.null actor) &&
      (M.socialEventOrganizerPartyId event == Just actor || M.eventTicketOrderBuyerPartyId order == Just actor)) $
      fail "Refund requester is not authorized for this order"
    actorId <- case reads (T.unpack actor) of
      [(value,"")] | value > 0 -> pure value
      _ -> fail "Refund requester must be an authenticated party"
    existing <- selectFirst [M.TicketRefundRequestOrderId ==. orderKey,
      M.TicketRefundRequestStatus <-. ["pending","processing"]] [Asc M.TicketRefundRequestId]
    case existing of
      Just entity@(Entity priorKey prior) -> do
        priorBinding <- loadTicketRefundReference priorKey
        unless (maybe False (const True) priorBinding) $ fail "Legacy refund requires separate reconciliation"
        unless (M.ticketRefundRequestRequestedByPartyId prior == Just actor &&
          M.ticketRefundRequestReason prior == reason &&
          maybe True (== M.ticketRefundRequestAmountCents prior) requestedAmount) $
          fail "A different refund request is already active for this order"
        pure entity
      Nothing -> do
        bindings <- rawSql
          "SELECT runtime.checkout_id::text,attempt.id::text,attempt.environment,attempt.merchant_account_ref,\
          \ runtime.checkout_total_minor,runtime.currency FROM event_ticket_checkout_runtime runtime\
          \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id=runtime.checkout_id\
          \ WHERE runtime.order_id=? AND runtime.event_id=? AND attempt.provider='paypal'\
          \ AND attempt.status='succeeded' AND attempt.currency=runtime.currency\
          \ AND attempt.amount_minor=runtime.checkout_total_minor"
          [toPersistValue orderKey,toPersistValue eventKey]
        (checkout,attempt,environment,merchant,total,currency) <- case bindings of
          [(Single checkout,Single attempt,Single environment,Single merchant,Single total,Single currency)] ->
            pure (checkout,attempt,environment,merchant,total,currency)
          _ -> fail "Refund requires one verified PayPal ticket payment"
        parsedEnvironment <- case (environment :: Text) of
          "sandbox" -> pure C.CheckoutSandbox
          "production" -> pure C.CheckoutProduction
          _ -> fail "Unknown immutable payment environment"
        portions <- either (fail . T.unpack) pure $
          ticketRefundAmounts total (map (fromSqlKey . entityKey) tickets)
        let available = [ (key,amount) | (key,amount) <- portions,
              Entity ticketKey ticket <- tickets,fromSqlKey ticketKey == key,
              M.eventTicketStatus ticket == "issued",M.eventTicketCheckedInAt ticket == Nothing,
              M.eventTicketCurrentHolderPartyId ticket == M.eventTicketOriginalHolderPartyId ticket ]
            target = maybe (sum (map snd available)) fromIntegral requestedAmount
            selection = takeWhile (\(_,cumulative) -> cumulative <= target) $
              zip available (drop 1 (scanl (\acc (_,amount) -> acc+amount) 0 available))
            selected = map fst selection
        unless (target > 0 && sum (map snd selected) == target &&
            target <= fromIntegral (maxBound :: Int)) $
          fail "Refund must select a whole number of available tickets"
        let request = M.TicketRefundRequest orderKey (Just actor) reason (fromIntegral target)
              "pending" Nothing Nothing Nothing Nothing Nothing now now
        requestKey <- insert request
        result <- requestTicketRefund actor eventKey orderKey R.RefundCreation
          { R.rcCheckout=C.CheckoutReference checkout,R.rcPaymentAttempt=C.PaymentAttemptReference attempt
          , R.rcProvider=C.ProviderPayPal,R.rcEnvironment=parsedEnvironment,R.rcMerchantRef=merchant
          , R.rcAmountMinor=target,R.rcCurrency=currency,R.rcReasonCode="customer_request"
          , R.rcIdempotencyKey="event-ticket-refund-request-" <> T.pack (show (fromSqlKey requestKey))
          , R.rcRequestedBy=actorId,R.rcCreatedAt=now } (map (toSqlKey . fst) selected)
        record <- either (fail . T.unpack) pure result
        rawExecute "INSERT INTO event_ticket_refund_request_binding(request_id,refund_id,created_at) VALUES (?,?::uuid,?)"
          [toPersistValue requestKey,PersistText (R.refundReferenceId (R.rrReference record)),PersistUTCTime now]
        pure (Entity requestKey request)

-- Captured gross and completed refunds are independent of legacy order status.
-- A fully refunded order still had gross revenue; partial refunds must reduce net.
capturedTicketRevenue :: M.SocialEventId -> Text
  -> SqlPersistT IO [(M.EventTicketOrderId,Int64,Int64)]
capturedTicketRevenue eventKey currency = do
  rows <- rawSql
    "SELECT runtime.order_id,checkout.paid_minor,checkout.refunded_minor,checkout.currency\
    \ FROM event_ticket_checkout_runtime runtime\
    \ JOIN commerce_checkout_session checkout ON checkout.id=runtime.checkout_id\
    \ WHERE runtime.event_id=? AND checkout.paid_minor>0"
    [toPersistValue eventKey]
  forM rows $ \(Single order,Single paid,Single refunded,Single storedCurrency) -> do
    unless (storedCurrency == currency && paid > 0 && refunded >= 0 && refunded <= paid) $
      fail "Ticket revenue currency or monetary evidence is inconsistent"
    pure (order,paid,refunded)
