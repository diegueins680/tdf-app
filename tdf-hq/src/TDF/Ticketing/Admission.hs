{-# LANGUAGE OverloadedStrings #-}

-- | Server-owned admission. Call inside one runSqlPool transaction. The event,
-- order and ticket locks are held until the state and audit row commit together.
module TDF.Ticketing.Admission
  ( AdmissionLookup(..)
  , AdmissionError(..)
  , admitTicket
  , normalizeTicketCode
  , newTicketCode
  ) where

import Control.Monad (unless)
import Data.Char (isAscii, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Database.Persist
import Database.Persist.Sql
import qualified TDF.Models.SocialEventsModels as M

data AdmissionLookup = AdmissionById M.EventTicketId | AdmissionByCode Text
  deriving (Eq, Show)

data AdmissionError
  = AdmissionNotFound | AdmissionForbidden | AdmissionUnpaid
  | AdmissionCancelled | AdmissionRefunded | AdmissionAlreadyUsed
  | AdmissionInvalidState | AdmissionPaymentReview
  deriving (Eq, Show)

-- Legacy codes stay readable; new credentials retain the full UUID randomness.
normalizeTicketCode :: Text -> Maybe Text
normalizeTicketCode raw = do
  suffix <- T.stripPrefix "TDF-" normalized
  if T.length suffix `elem` [12, 32] && T.all (\c -> isAscii c && isHexDigit c) suffix
    then Just normalized else Nothing
  where normalized = T.toUpper (T.strip raw)

newTicketCode :: IO Text
newTicketCode = ("TDF-" <>) . T.toUpper . T.replace "-" "" . UUID.toText <$> UUIDV4.nextRandom

-- PostgreSQL-only authority: no ownership claim, no client-provided price or
-- payment assertion. Unknown/unpaid/used states are denied without mutation.
admitTicket :: Text -> M.SocialEventId -> AdmissionLookup -> UTCTime
  -> SqlPersistT IO (Either AdmissionError (Entity M.EventTicket))
admitTicket actor eventKey lookupValue now = do
  events <- rawSql "SELECT ?? FROM social_event WHERE id=? FOR UPDATE" [toPersistValue eventKey]
  case events of
    [Entity _ event]
      | maybe False (\owner -> not (T.null (T.strip actor)) && T.strip owner == actor)
          (M.socialEventOrganizerPartyId event) -> findAndLock
      | otherwise -> pure (Left AdmissionForbidden)
    _ -> pure (Left AdmissionNotFound)
  where
    filters = [M.EventTicketEventId ==. eventKey] ++ case lookupValue of
      AdmissionById ticketKey -> [M.EventTicketId ==. ticketKey]
      AdmissionByCode code -> [M.EventTicketCode ==. code]
    findAndLock = do
      found <- selectFirst filters []
      case found of
        Nothing -> pure (Left AdmissionNotFound)
        Just (Entity ticketKey initial) -> do
          orders <- rawSql "SELECT ?? FROM event_ticket_order WHERE id=? FOR UPDATE"
            [toPersistValue (M.eventTicketOrderRefId initial)]
          tickets <- rawSql "SELECT ?? FROM event_ticket WHERE id=? AND event_id=? FOR UPDATE"
            [toPersistValue ticketKey, toPersistValue eventKey]
          case (orders, tickets) of
            ([Entity orderKey order], [Entity _ ticket])
              | M.eventTicketOrderRefId ticket == orderKey
              , M.eventTicketOrderEventId order == eventKey
              , M.eventTicketOrderTierId order == M.eventTicketTierRefId ticket
              , lookupMatches ticket -> validateAndWrite ticketKey ticket order
            _ -> pure (Left AdmissionNotFound)
    lookupMatches ticket = case lookupValue of
      AdmissionById _ -> True
      AdmissionByCode code -> M.eventTicketCode ticket == code
    normalized = T.toLower . T.strip
    validateAndWrite :: M.EventTicketId -> M.EventTicket -> M.EventTicketOrder
      -> SqlPersistT IO (Either AdmissionError (Entity M.EventTicket))
    validateAndWrite ticketKey ticket order =
      case (normalized (M.eventTicketOrderStatus order), normalized (M.eventTicketStatus ticket)) of
        ("refunded", _) -> pure (Left AdmissionRefunded)
        ("cancelled", _) -> pure (Left AdmissionCancelled)
        ("canceled", _) -> pure (Left AdmissionCancelled)
        (_, "refunded") -> pure (Left AdmissionRefunded)
        (_, "cancelled") -> pure (Left AdmissionCancelled)
        (_, "canceled") -> pure (Left AdmissionCancelled)
        (_, "checked_in") -> pure (Left AdmissionAlreadyUsed)
        (_, "checkedin") -> pure (Left AdmissionAlreadyUsed)
        ("paid", "issued") | M.eventTicketCheckedInAt ticket == Nothing -> do
          runtimes <- rawSql
            "SELECT event_id,payment_status FROM event_ticket_checkout_runtime WHERE order_id=?"
            [toPersistValue (M.eventTicketOrderRefId ticket)]
          let paymentValid = case runtimes of
                [] -> True -- Legacy orders predate the canonical public checkout runtime.
                [(Single runtimeEvent, Single paymentStatus)] -> runtimeEvent == eventKey &&
                  paymentStatus `elem` (["paid", "partially_refunded"] :: [Text])
                _ -> False
          reviews <- rawSql
            "SELECT EXISTS(SELECT 1 FROM event_ticket_checkout_runtime runtime\
            \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id=runtime.checkout_id\
            \ JOIN commerce_provider_binding binding ON binding.payment_attempt_id=attempt.id\
            \ JOIN commerce_reconciliation_exception exception\
            \ ON exception.provider=binding.provider AND exception.environment=binding.environment\
            \ AND exception.merchant_account_ref=binding.merchant_account_ref\
            \ AND exception.provider_reference=binding.provider_resource_id\
            \ AND exception.internal_reference=runtime.order_id::text\
            \ WHERE runtime.order_id=? AND binding.resource_type='capture'\
            \ AND exception.exception_type IN ('external_refund_detected','external_reversal_detected'))"
            [toPersistValue (M.eventTicketOrderRefId ticket)]
          let reviewRequired = reviews /= [Single False]
          if not paymentValid then pure (Left AdmissionUnpaid)
          else if reviewRequired then pure (Left AdmissionPaymentReview) else do
              changed <- updateWhereCount
                [ M.EventTicketId ==. ticketKey
                , M.EventTicketStatus ==. M.eventTicketStatus ticket
                , M.EventTicketCheckedInAt ==. Nothing
                ]
                [ M.EventTicketStatus =. "checked_in"
                , M.EventTicketCheckedInAt =. Just now
                , M.EventTicketUpdatedAt =. now
                ]
              unless (changed == 1) $ fail "Admission mutation was not singular"
              rawExecute
                "INSERT INTO event_ticket_admission_audit(ticket_id,event_id,order_id,actor_party_id,admitted_at) VALUES (?,?,?,?,?)"
                [toPersistValue ticketKey, toPersistValue eventKey, toPersistValue (M.eventTicketOrderRefId ticket), PersistText actor, PersistUTCTime now]
              stored <- getEntity ticketKey
              case stored of
                Just entity@(Entity _ value)
                  | M.eventTicketStatus value == "checked_in"
                  , M.eventTicketCheckedInAt value /= Nothing -> pure (Right entity)
                _ -> fail "Admission stored state did not match mutation"
        ("paid", _) -> pure (Left AdmissionInvalidState)
        _ -> pure (Left AdmissionUnpaid)
