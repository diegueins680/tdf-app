{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | Ticket issuance for a paid order: the single place that turns a verified
-- canonical payment into tickets. Runs inside the caller's transaction and is
-- shared by HTTP handlers and the payment event worker.
module TDF.Ticketing.Issuance
    ( finalizePaidTicketOrder
    , issueMissingTicketsForOrder
    , generateUniqueTicketCode
    ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Int (Int64)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Sql

import TDF.Models.SocialEventsModels
import qualified TDF.Ticketing.Admission as Admission
import qualified TDF.Ticketing.Transfer as Transfer

issueMissingTicketsForOrder ::
    UTCTime ->
    EventTicketOrderId ->
    EventTicketOrder ->
    SqlPersistT IO [T.Text]
issueMissingTicketsForOrder now orderKey order = do
    existingTickets <-
        selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId]
    let missingCount =
            max 0 (eventTicketOrderQuantity order - length existingTickets)
    forM_ [1 .. missingCount] $ \_ -> do
        ticketCodeValue <- generateUniqueTicketCode
        insert_
            EventTicket
                { eventTicketEventId = eventTicketOrderEventId order
                , eventTicketTierRefId = eventTicketOrderTierId order
                , eventTicketOrderRefId = orderKey
                , eventTicketHolderName = eventTicketOrderBuyerName order
                , eventTicketHolderEmail = eventTicketOrderBuyerEmail order
                , eventTicketCode = ticketCodeValue
                , eventTicketStatus = "issued"
                , eventTicketCheckedInAt = Nothing
                , eventTicketCurrentHolderPartyId = eventTicketOrderBuyerPartyId order
                , eventTicketCurrentHolderEmail = eventTicketOrderBuyerEmail order
                , eventTicketCurrentHolderName = eventTicketOrderBuyerName order
                , eventTicketOriginalHolderPartyId = eventTicketOrderBuyerPartyId order
                , eventTicketTransferHistory = Nothing
                , eventTicketCreatedAt = now
                , eventTicketUpdatedAt = now
                }
    allTickets <- selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId]
    pure (map (eventTicketCode . entityVal) (filter (Transfer.retainedByBuyer . entityVal) allTickets))

finalizePaidTicketOrder ::
    UTCTime ->
    EventTicketOrderId ->
    SqlPersistT IO (EventTicketOrder, [T.Text], Bool)
finalizePaidTicketOrder now orderKey = do
    runtimeRows <-
        ( rawSql
            "SELECT runtime.fulfillment_status, runtime.payment_status,\
            \ runtime.promo_code_id, runtime.discount_minor\
            \ FROM event_ticket_checkout_runtime runtime\
            \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
            \ WHERE runtime.order_id = ? AND checkout.status = 'paid' FOR UPDATE OF runtime"
            [toPersistValue orderKey]
            :: SqlPersistT IO
                [(Single T.Text, Single T.Text, Single (Maybe Int64), Single Int64)]
        )
    (fulfillmentStatus, paymentStatus, promoKey, discountMinor) <-
        case runtimeRows of
            [(Single status, Single payment, Single promoId, Single discount)] ->
                pure (status, payment, toSqlKey <$> promoId, discount)
            _ -> fail "Ticket issuance requires one locked canonical checkout runtime"
    unless (paymentStatus == "paid") $
        fail "Ticket issuance requires a paid canonical checkout"
    order <- getJust orderKey
    when (eventTicketOrderStatus order `notElem` ["pending", "paid"]) $
        fail "Closed ticket order cannot be issued"
    updateWhere
        [ EventTicketOrderId ==. orderKey
        , EventTicketOrderStatus ==. "pending"
        ]
        [ EventTicketOrderStatus =. "paid"
        , EventTicketOrderUpdatedAt =. now
        ]
    case fulfillmentStatus of
        "seat_held" -> do
            ticketCodes <- issueMissingTicketsForOrder now orderKey order
            forM_ promoKey $ \key -> do
                existingRedemption <-
                    selectFirst [PromoCodeRedemptionOrderId ==. orderKey] []
                when (not (isJust existingRedemption)) $
                    insert_
                        PromoCodeRedemption
                            { promoCodeRedemptionPromoCodeId = key
                            , promoCodeRedemptionOrderId = orderKey
                            , promoCodeRedemptionDiscountAmountCents =
                                fromIntegral discountMinor
                            , promoCodeRedemptionRedeemedAt = now
                            }
            rawExecute
                "UPDATE event_ticket_checkout_runtime\
                \ SET fulfillment_status='issued', issued_at=?, updated_at=? WHERE order_id=?"
                [ PersistUTCTime now
                , PersistUTCTime now
                , toPersistValue orderKey
                ]
            rawExecute
                "INSERT INTO event_ticket_fulfillment_event(\
                \ order_id, from_status, to_status, actor_type, reason_code, notes\
                \) VALUES (?, 'seat_held', 'issued', 'provider', 'verified_payment',\
                \ 'Tickets issued only after canonical provider verification')"
                [toPersistValue orderKey]
            -- Persist delivery intent in the issuance transaction. Network SMTP
            -- happens in the leased worker only after this transaction commits.
            _ <- (rawSql "SELECT event_ticket_queue_confirmation(?) IS NULL"
                [toPersistValue orderKey] :: SqlPersistT IO [Single Bool])
            pure (order, ticketCodes, True)
        "issued" -> do
            existingTickets <-
                selectList [EventTicketOrderRefId ==. orderKey] [Asc EventTicketId]
            pure
                ( order
                , map (eventTicketCode . entityVal) (filter (Transfer.retainedByBuyer . entityVal) existingTickets)
                , False
                )
        _ -> fail "Paid ticket checkout is not in an issuable fulfillment state"

generateUniqueTicketCode :: (MonadIO m) => ReaderT SqlBackend m T.Text
generateUniqueTicketCode = do
    code <- liftIO Admission.newTicketCode
    mExisting <- getBy (UniqueEventTicketCode code)
    case mExisting of
        Nothing -> pure code
        Just _ -> generateUniqueTicketCode
