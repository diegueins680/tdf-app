{-# LANGUAGE OverloadedStrings #-}
module TDF.EventOperations.CrmPayments (manualCrmCommand) where

import Control.Monad (void,when)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (Digest,SHA256,hash)
import Data.Aeson (Value(..),encode,object,(.=),(.:),withObject)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime,getCurrentTime,addUTCTime)
import Data.UUID (UUID,toText)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..),SqlPersistT,rawSql,rawExecute,toSqlKey)
import qualified TDF.Commerce.CheckoutStore as C
import qualified TDF.Server.TicketManualPayments as Manual
import TDF.Server.SocialEventsHandlers (finalizePaidTicketOrder)

-- No new payment/sale ledger. The claim is existing manual evidence for one
-- canonical order; confirmation uses the existing verified-payment and issuance.
manualCrmCommand :: Int64 -> Int64 -> UUID -> Value -> SqlPersistT IO (Either Text Value)
manualCrmCommand event actor key body = do
  roles <- rawSql "SELECT event_crm_role(?,?)" [PersistInt64 event,PersistInt64 actor]
    :: SqlPersistT IO [Single (Maybe Text)]
  let role = case roles of [Single (Just r)] -> r; _ -> ""
      requestHash = T.pack (show (hash (BL.toStrict (encode body)) :: Digest SHA256))
  prior <- rawSql "SELECT request_hash,outcome::text FROM event_crm_command_receipt WHERE event_id=? AND actor_id=? AND command_id=?"
    [PersistInt64 event,PersistInt64 actor,PersistText (toText key)] :: SqlPersistT IO [(Single Text,Single Text)]
  case parseEither parseCommand body of
    Left _ -> pure (Left "invalid_request")
    Right (operation,orderId,providerText,details) ->
      if role == "" then pure (Left "not_found")
      else if (operation=="manual.review" && role `notElem` ["owner","payment_verifier"])
        || (operation=="manual.submit" && role `notElem` ["owner","coordinator","sales"])
        then pure (Left "forbidden")
      else case prior of
        [(Single oldHash,Single _)] | oldHash/=requestHash -> pure (Left "idempotency_conflict")
        [_] -> pure (Right (object ["replayed" .= True,"orderId" .= orderId]))
        [] -> do
          rawExecute "SAVEPOINT event_crm_manual" []
          result <- if operation=="manual.submit" then submit role orderId providerText details
            else review orderId providerText details
          case result of
            Left code -> do
              -- A refused review already undoes its own payment-state changes
              -- and deliberately keeps the reconciliation exception it records
              -- (expired hold, order paid by another attempt). Rolling back
              -- here would erase that record, so only a failed submission is
              -- rolled back.
              when (operation=="manual.submit") $
                rawExecute "ROLLBACK TO SAVEPOINT event_crm_manual" []
              rawExecute "RELEASE SAVEPOINT event_crm_manual" []
              pure (Left code)
            Right outcome -> do
              rawExecute "INSERT INTO event_crm_command_receipt(event_id,actor_id,command_id,request_hash,outcome) VALUES(?,?,?::uuid,?,?::jsonb)"
                [PersistInt64 event,PersistInt64 actor,PersistText (toText key),PersistText requestHash,jsonParam outcome]
              rawExecute "INSERT INTO event_crm_audit(event_id,actor_id,operation,subject_id) VALUES(?,?,?,?)"
                [PersistInt64 event,PersistInt64 actor,PersistText operation,PersistText (T.pack (show orderId))]
              rawExecute "RELEASE SAVEPOINT event_crm_manual" []
              pure (Right outcome)
        _ -> pure (Left "database_unavailable")
  where
    parseCommand = withObject "ManualCommand" $ \o -> do
      operation <- o .: "operation"
      orderId <- o .: "orderId"
      provider <- o .: "provider"
      if operation `notElem` ["manual.submit","manual.review"] || (orderId :: Int64)<=0
        || orderId>9007199254740991 || (provider :: Text) `notElem` ["cash","bank_transfer"]
        || any (`notElem` ["operation","orderId","provider","reference","occurredAt","amountMinor","notes","action"]) (KM.keys o)
        then fail "invalid manual command" else pure (operation,orderId,provider,Object o)
    provider "cash" = C.ProviderCash
    provider _ = C.ProviderBankTransfer
    submit role orderId method details = case parseEither (withObject "Claim" $ \o ->
      (,,) <$> o .: "reference" <*> o .: "occurredAt" <*> o .: "amountMinor") details of
      Left _ -> pure (Left "invalid_request")
      Right (reference,occurred,amount) -> do
        now <- liftIO getCurrentTime
        if T.length reference<3 || T.length reference>120 || T.any isControl reference || occurred>addUTCTime 300 now
          || (amount :: Int64)<=0 then pure (Left "invalid_request") else do
          rows <- rawSql
            "SELECT r.checkout_id::text,c.environment,r.checkout_total_minor,r.currency,c.status,\
            \ greatest(r.hold_expires_at,r.manual_hold_expires_at),r.fulfillment_status\
            \ FROM event_ticket_checkout_runtime r JOIN commerce_checkout_session c ON c.id=r.checkout_id\
            \ JOIN event_ticket_order o ON o.id=r.order_id WHERE r.event_id=? AND r.order_id=?\
            \ AND c.domain_type='event_ticket_order' AND c.domain_order_id=r.order_id::text\
            \ AND (? <> 'sales' OR EXISTS(SELECT 1 FROM event_crm_lead l WHERE l.order_id=o.id AND l.responsible_party_id=?))\
            \ FOR UPDATE OF c,r"
            [PersistInt64 event,PersistInt64 orderId,PersistText role,PersistInt64 actor]
            :: SqlPersistT IO [(Single Text,Single Text,Single Int64,Single Text,Single Text,Single UTCTime,Single Text)]
          case rows of
            [(Single checkoutId,Single environment,Single expected,Single currency,Single status,Single expiry,Single fulfillment)]
              | amount==expected && expiry>now && fulfillment=="seat_held" && status `elem` ["awaiting_payment","failed"] -> do
                let checkout=C.CheckoutReference checkoutId
                    methodProvider=provider method
                    correlation="event-crm-manual:"<>T.pack(show orderId)
                existing <- rawSql
                  "SELECT id::text FROM commerce_payment_attempt WHERE checkout_id=?::uuid AND operation='manual_verify' AND provider=? ORDER BY created_at DESC"
                  [PersistText checkoutId,PersistText method] :: SqlPersistT IO [Single Text]
                attemptResult <- case existing of
                  [Single attempt] -> pure (Right (C.PaymentAttemptReference attempt))
                  [] | method=="cash" -> case C.resolveCheckoutEnvironment (Just (T.unpack environment)) of
                    Left _ -> pure (Left "invalid_environment")
                    Right env -> C.beginPaymentAttempt C.PaymentAttemptCreation
                      { C.pacCheckout=checkout,C.pacProvider=methodProvider,C.pacEnvironment=env
                      , C.pacOperation=C.OperationManualVerify,C.pacAmountMinor=amount,C.pacCurrency=currency
                      , C.pacMerchantRef="tdf-manual-settlement",C.pacIdempotencyKey="crm-manual-"<>T.pack(show orderId)
                      , C.pacCreatedAt=now,C.pacCorrelationId=correlation }
                  _ -> pure (Left "Select bank transfer through the existing checkout first")
                case attemptResult of
                  Left _ -> pure (Left "manual_method_not_ready")
                  Right attempt -> do
                    -- Only an unsubmitted/rejected claim may be changed. A pending
                    -- review or verified capture must first be reconciled explicitly.
                    existingEvidence <- rawSql "SELECT status FROM commerce_manual_payment_evidence WHERE payment_attempt_id=?::uuid FOR UPDATE"
                      [PersistText (C.paymentAttemptReferenceId attempt)] :: SqlPersistT IO [Single Text]
                    if any (\(Single s) -> s `notElem` ["awaiting_evidence","rejected"]) existingEvidence then pure (Left "evidence_already_submitted") else do
                      C.recordManualPaymentSelection checkout attempt methodProvider correlation now
                      rawExecute "UPDATE commerce_manual_payment_evidence SET status='submitted',customer_reference=?,submitted_amount_minor=?,currency=?,submitted_at=?,submitted_by=?,sale_occurred_at=?,reviewed_by=NULL,reviewed_at=NULL,review_notes=NULL WHERE payment_attempt_id=?::uuid"
                        [PersistText reference,PersistInt64 amount,PersistText currency,PersistUTCTime now,PersistInt64 actor,PersistUTCTime occurred,PersistText(C.paymentAttemptReferenceId attempt)]
                      pure (Right (object ["orderId" .= orderId,"submitted" .= True,"paid" .= False]))
            _ -> pure (Left "order_evidence_required")
    review orderId method details = case parseEither (withObject "Review" $ \o -> (,) <$> o .: "action" <*> o .: "notes") details of
      Left _ -> pure (Left "invalid_request")
      Right (rawAction,rawNotes) -> case (Manual.parseManualReviewAction rawAction,Manual.validateManualReviewNotes rawNotes) of
        (Right action,Right notes) -> do
          now <- liftIO getCurrentTime
          result <- Manual.reviewCrmTicketManualPayment (provider method) (Manual.ManualReviewer actor False)
            (toSqlKey event) (toSqlKey orderId) action notes now (void (finalizePaidTicketOrder now (toSqlKey orderId)))
          pure $ case result of
            Left _ -> Left "manual_review_refused"
            Right paid -> Right (object ["orderId" .= orderId,"paid" .= paid])
        _ -> pure (Left "invalid_request")
    jsonParam=PersistText . TE.decodeUtf8 . BL.toStrict . encode
