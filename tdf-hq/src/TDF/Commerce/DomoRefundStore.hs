{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Domain binding and independent approval on top of the canonical refund
-- store. This module never calls a provider and never treats a request as cash.
module TDF.Commerce.DomoRefundStore
  ( loadDomoRefund, approveDomoRefund, claimDomoRefund
  , loadDomoRefundCapture, recordManualEvidence, confirmManualEvidence
  , ManualEvidence(..), bindDomoRefundReference, refundAudit
  ) where

import Control.Monad (unless, when)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql)
import qualified TDF.Commerce.CheckoutStore as C
import qualified TDF.Commerce.RefundStore as R

-- The cancellation itself is immutable, including its one refund link.
-- A refund ID from another domain or another quote never grants authority.
loadDomoRefund :: Text -> SqlPersistT IO (Either Text R.RefundRecord)
loadDomoRefund quoteId = do
  refs <- rawSql
    "SELECT r.id::text FROM domo_quote_cancellation c\
    \ JOIN domo_event_quote_runtime q ON q.id=c.domo_quote_id\
    \ JOIN commerce_checkout_session s ON s.id=q.checkout_id\
    \ JOIN commerce_refund r ON r.id=c.refund_id AND r.checkout_id=s.id\
    \ JOIN commerce_payment_attempt a ON a.id=r.payment_attempt_id AND a.checkout_id=s.id\
    \ WHERE c.domo_quote_id=?::uuid AND q.quote_status='cancelled'\
    \ AND s.domain_type='domo_event_quote' AND s.domain_order_id='domo-quote:'||q.id::text\
    \ AND r.amount_minor=c.refund_due_minor AND r.currency=c.currency AND s.currency=r.currency\
    \ AND a.status='succeeded' AND a.provider=r.provider AND a.environment=r.environment\
    \ AND s.environment=r.environment AND a.merchant_account_ref=r.merchant_account_ref\
    \ AND a.currency=r.currency AND a.amount_minor=s.paid_minor"
    [PersistText quoteId] :: SqlPersistT IO [Single Text]
  case refs of
    [Single ref] -> maybe (Left "Refund not found") Right <$> R.loadRefund (R.RefundReference ref)
    _ -> pure (Left "Domo cancellation has no exactly bound refund request")

locked :: Text -> SqlPersistT IO R.RefundRecord
locked quoteId = do
  initial <- either (fail . T.unpack) pure =<< loadDomoRefund quoteId
  _ <- rawSql "SELECT id::text FROM commerce_refund WHERE id=?::uuid FOR UPDATE"
    [PersistText (R.refundReferenceId (R.rrReference initial))] :: SqlPersistT IO [Single Text]
  either (fail . T.unpack) pure =<< loadDomoRefund quoteId

approveDomoRefund :: Text -> Int64 -> UTCTime -> SqlPersistT IO R.RefundRecord
approveDomoRefund quoteId actor now = do
  record <- locked quoteId
  unless (actor>0 && actor/=R.rrRequestedBy record) $
    fail "Refund approval requires a different party"
  (approved, changed) <- either (fail . T.unpack) pure =<<
    R.approveRefundRequest (R.rrReference record) actor now
  when changed $ refundAudit approved actor "approved"
  pure approved

-- Requires a prior separate approval. The existing durable processing fence
-- yields exactly one execution permit, including concurrent callers/retries.
claimDomoRefund :: Text -> Int64 -> UTCTime -> SqlPersistT IO (R.RefundRecord, Bool)
claimDomoRefund quoteId actor now = do
  record <- locked quoteId
  unless (actor>0 && actor/=R.rrRequestedBy record
      && maybe False (\p -> p>0 && p/=R.rrRequestedBy record) (R.rrApprovedBy record)
      && R.rrStatus record `elem` ["approved","processing","succeeded"]) $
    fail "Independent approval must precede execution"
  result <- either (fail . T.unpack) pure =<<
    R.approveRefundForProcessing (R.rrReference record) actor now
  when (snd result) $ refundAudit (fst result) actor
    (if R.rrProvider record=="paypal" then "execution_claimed" else "manual_execution_claimed")
  pure result

-- An operator-discovered refund ID is only a query candidate, never proof of
-- completion. It cannot change an already recorded provider identity or permit
-- a second POST after an ambiguous network outcome.
bindDomoRefundReference :: Text -> Int64 -> UTCTime -> Text -> SqlPersistT IO ()
bindDomoRefundReference quoteId actor now providerId = do
  record <- locked quoteId
  unless (R.rrProvider record=="paypal" && R.rrStatus record=="processing"
      && actor>0 && actor/=R.rrRequestedBy record
      && maybe False (/=R.rrRequestedBy record) (R.rrApprovedBy record)) $
    fail "Only an independently approved processing PayPal refund can bind a query candidate"
  _ <- either (fail . T.unpack) pure =<< R.recordRefundPending (R.rrReference record) providerId now
  refundAudit record actor "provider_query_reference_recorded"

loadDomoRefundCapture :: R.RefundRecord -> SqlPersistT IO Text
loadDomoRefundCapture record = do
  rows <- rawSql
    "SELECT b.provider_resource_id FROM commerce_provider_binding b\
    \ JOIN commerce_payment_attempt a ON a.id=b.payment_attempt_id\
    \ WHERE a.id=?::uuid AND a.checkout_id=?::uuid AND a.status='succeeded'\
    \ AND b.resource_type='capture' AND b.provider='paypal' AND b.provider=a.provider\
    \ AND b.environment=a.environment AND b.merchant_account_ref=a.merchant_account_ref\
    \ AND b.amount_minor=a.amount_minor AND b.currency=a.currency\
    \ AND a.environment=? AND a.merchant_account_ref=? AND a.currency=?"
    [ PersistText (C.paymentAttemptReferenceId (R.rrPaymentAttempt record))
    , PersistText (C.checkoutReferenceId (R.rrCheckout record))
    , PersistText (R.rrEnvironment record),PersistText (R.rrMerchantRef record)
    , PersistText (R.rrCurrency record) ] :: SqlPersistT IO [Single Text]
  case rows of
    [Single capture] -> pure capture
    _ -> fail "Refund requires one exactly bound original PayPal capture"

data ManualEvidence = ManualEvidence
  { meExternalReference :: Text, meEvidenceReference :: Text
  , meAmountMinor :: Int64, meCurrency :: Text, meOccurredAt :: UTCTime
  } deriving (Eq, Show)

recordManualEvidence :: Text -> Int64 -> UTCTime -> ManualEvidence -> SqlPersistT IO ()
recordManualEvidence quoteId actor now ManualEvidence{..} = do
  record <- locked quoteId
  unless (R.rrProvider record/="paypal" && R.rrStatus record=="processing"
    && actor>0 && actor/=R.rrRequestedBy record && meAmountMinor==R.rrAmountMinor record
    && meCurrency==R.rrCurrency record && meOccurredAt<=now
    && meOccurredAt>=R.rrCreatedAt record
    && validReference meExternalReference
    && T.length meEvidenceReference>=16 && T.length meEvidenceReference<=256
    && T.all (\c -> c>='!' && c<='~') meEvidenceReference) $
    fail "Manual evidence must match an independently approved processing refund"
  rawExecute
    "INSERT INTO domo_manual_refund_evidence(refund_id,provider,environment,merchant_account_ref,\
    \ external_reference,evidence_reference,amount_minor,currency,occurred_at,recorded_by,recorded_at)\
    \ VALUES (?::uuid,?,?,?,?,?,?,?,?,?,?) ON CONFLICT (refund_id) DO NOTHING"
    [ PersistText (R.refundReferenceId (R.rrReference record)),PersistText (R.rrProvider record)
    , PersistText (R.rrEnvironment record),PersistText (R.rrMerchantRef record)
    , PersistText meExternalReference,PersistText meEvidenceReference,PersistInt64 meAmountMinor
    , PersistText meCurrency,PersistUTCTime meOccurredAt,PersistInt64 actor,PersistUTCTime now ]
  matches <- rawSql
    "SELECT EXISTS(SELECT 1 FROM domo_manual_refund_evidence WHERE refund_id=?::uuid\
    \ AND external_reference=? AND evidence_reference=? AND amount_minor=? AND currency=?\
    \ AND occurred_at=? AND recorded_by=?)"
    [ PersistText (R.refundReferenceId (R.rrReference record)),PersistText meExternalReference
    , PersistText meEvidenceReference,PersistInt64 meAmountMinor,PersistText meCurrency
    , PersistUTCTime meOccurredAt,PersistInt64 actor ] :: SqlPersistT IO [Single Bool]
  unless (matches==[Single True]) $ fail "Manual evidence conflicts with immutable receipt"
  refundAudit record actor "manual_evidence_recorded"

confirmManualEvidence :: Text -> Int64 -> UTCTime -> SqlPersistT IO R.RefundRecord
confirmManualEvidence quoteId actor now = do
  record <- locked quoteId
  rows <- rawSql
    "SELECT external_reference,recorded_by,confirmed_by FROM domo_manual_refund_evidence\
    \ WHERE refund_id=?::uuid FOR UPDATE"
    [PersistText (R.refundReferenceId (R.rrReference record))]
    :: SqlPersistT IO [(Single Text,Single Int64,Single (Maybe Int64))]
  case rows of
    [(Single externalRef,Single recorder,Single confirmed)] -> do
      unless (actor>0 && actor/=recorder && actor/=R.rrRequestedBy record) $
        fail "Receipt confirmation requires independent review"
      case confirmed of
        Just _ | R.rrStatus record=="succeeded" -> pure record
        Nothing | R.rrStatus record=="processing" -> do
          -- Financial posting and evidence approval commit or roll back together.
          _ <- either (fail . T.unpack) pure =<< R.recordVerifiedRefund R.VerifiedRefund
            { R.vrRefund=R.rrReference record,R.vrProviderRefund=externalRef
            , R.vrAmountMinor=R.rrAmountMinor record,R.vrCurrency=R.rrCurrency record
            , R.vrOccurredAt=now,R.vrCorrelationId="domo-manual:"<>R.refundReferenceId (R.rrReference record) }
          rawExecute "UPDATE domo_manual_refund_evidence SET confirmed_by=?,confirmed_at=? WHERE refund_id=?::uuid"
            [PersistInt64 actor,PersistUTCTime now,PersistText (R.refundReferenceId (R.rrReference record))]
          refundAudit record actor "manual_evidence_confirmed"
          either (fail . T.unpack) pure =<< loadDomoRefund quoteId
        _ -> fail "Receipt is not awaiting confirmation"
    _ -> fail "No manual refund receipt to verify"

validReference :: Text -> Bool
validReference value = not (T.null value) && T.length value<=256 &&
  T.all (\c -> (c>='a' && c<='z') || (c>='A' && c<='Z') || (c>='0' && c<='9')
    || c `elem` ("-_." :: String)) value

refundAudit :: R.RefundRecord -> Int64 -> Text -> SqlPersistT IO ()
refundAudit record actor outcome = rawExecute
  "INSERT INTO commerce_checkout_audit_event(checkout_id,event_type,actor_type,actor_id,correlation_id,metadata)\
  \ VALUES (?::uuid,'domo_refund','admin',?,?,jsonb_build_object('refund_id',?::text,'outcome',?::text))"
  [ PersistText (C.checkoutReferenceId (R.rrCheckout record)),PersistText (T.pack (show actor))
  , PersistText ("domo-refund:"<>R.refundReferenceId (R.rrReference record))
  , PersistText (R.refundReferenceId (R.rrReference record)),PersistText outcome ]
