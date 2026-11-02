{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Cancelling a Domo quote (PAY-DOMO-CANCEL-001, AUTHORITY-093).
--
-- One transaction: lock the quote and its deposit checkout, price the refund
-- with the single approved policy version, record the immutable cancellation
-- (lead time, collected money, retained and refundable amounts), move the quote
-- and its reservation to @cancelled@ and, when an authenticated operator acts,
-- create the refund request for exactly the obligation. A customer acting with
-- the lookup token is not a party, so the obligation is recorded and surfaced
-- as an open reconciliation exception until the exact refund is verified;
-- the refund store then requires a different party to approve it.
--
-- Nothing is written before every validation that can fail has passed, so a
-- 'Left' result leaves the database untouched.
module TDF.Commerce.DomoCancellationStore
  ( CancellationActor (..)
  , CancellationRequest (..)
  , CancellationError (..)
  , DomoCancellationRecord (..)
  , approvedCancellationPolicy
  , cancelDomoQuote
  , loadRecord
  , domoPaymentAdmission
  , cancellationRefundIdempotencyKey
  , cancellationReasonCode
  , refundObligationExceptionType
  ) where

import           Control.Monad (unless)
import           Data.Maybe (fromMaybe)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.Persist (PersistValue (..))
import           Database.Persist.Sql (Single (..), SqlPersistT, rawExecute, rawSql)

import qualified TDF.Commerce.CheckoutStore as Checkout
import           TDF.Commerce.DomoCancellation
import qualified TDF.Commerce.RefundStore as Refund

data CancellationActor
  = CustomerActor
    -- ^ the quote holder, authenticated by the lookup token only
  | OperatorActor !Int64
    -- ^ an authenticated staff party
  deriving (Eq, Show)

data CancellationRequest = CancellationRequest
  { crQuoteId   :: !Text
  , crActor     :: !CancellationActor
  , crCanceller :: !Canceller
    -- ^ whose decision the cancellation is; drives the refund ladder
  , crNote      :: !(Maybe Text)
  , crNow       :: !UTCTime
  } deriving (Eq, Show)

data CancellationError
  = CancellationNotFound
  | CancellationNotAllowed !Text
    -- ^ terminal quote, payment in flight or missing cancellation record
  | CancellationPolicyUnavailable !Text
    -- ^ money was collected but no approved policy version exists
  | CancellationRefundRejected !Text
    -- ^ the refund store refused the obligation request; nothing was written
  deriving (Eq, Show)

data DomoCancellationRecord = DomoCancellationRecord
  { dcrQuoteId               :: !Text
  , dcrQuoteStatus           :: !Text
  , dcrCanceller             :: !Text
  , dcrActorType             :: !Text
  , dcrPolicyVersion         :: !(Maybe Int)
  , dcrCancelledAt           :: !UTCTime
  , dcrLeadDays              :: !Int
  , dcrInProgress            :: !Bool
  , dcrRetainedBasisPoints   :: !Int
  , dcrDepositCollectedMinor :: !Int64
  , dcrBalanceCollectedMinor :: !Int64
  , dcrRetainedMinor         :: !Int64
  , dcrRefundDueMinor        :: !Int64
  , dcrCurrency              :: !Text
  , dcrRefundStatus          :: !Text
    -- ^ none | awaiting_operator, or the linked canonical refund status
  , dcrRefundId              :: !(Maybe Text)
  , dcrReplayed              :: !Bool
    -- ^ the quote was already cancelled; the stored record is returned
  } deriving (Eq, Show)

-- | Refund reason code recorded against the collected deposit.
cancellationReasonCode :: Canceller -> Text
cancellationReasonCode CancelledByCustomer = "customer_request"
cancellationReasonCode CancelledByTdf = "service_cancelled"

-- | Deterministic refund idempotency key: one refund per cancellation.
cancellationRefundIdempotencyKey :: Text -> Text
cancellationRefundIdempotencyKey quoteId = "domo-cancel:" <> quoteId

-- | Reconciliation exception type carrying a customer cancellation's refund
-- obligation until verified completion settles it.
refundObligationExceptionType :: Text
refundObligationExceptionType = "domo_cancellation_refund_due"

cancellerText :: Canceller -> Text
cancellerText CancelledByCustomer = "customer"
cancellerText CancelledByTdf = "tdf"

-- | The single approved policy version, or why none is usable.
approvedCancellationPolicy :: SqlPersistT IO (Either Text (Int, DomoCancellationPolicy))
approvedCancellationPolicy = do
  rows <- rawSql
    "SELECT version, full_refund_min_days, partial_refund_min_days, partial_retained_bps,\
    \ balance_refunded_in_full FROM domo_cancellation_policy WHERE status = 'approved'"
    [] :: SqlPersistT IO [(Single Int, Single Int, Single Int, Single Int, Single Bool)]
  pure $ case rows of
    [(Single version, Single fullDays, Single partialDays, Single retainedBps, Single balanceInFull)] ->
      case validateDomoCancellationPolicy DomoCancellationPolicy
             { dcpFullRefundMinDays = fullDays
             , dcpPartialRefundMinDays = partialDays
             , dcpPartialRetainedBps = retainedBps
             , dcpBalanceRefundedInFull = balanceInFull
             } of
        Left message -> Left ("Approved Domo cancellation policy is invalid: " <> message)
        Right policy -> Right (version, policy)
    [] -> Left "No approved Domo cancellation policy version exists"
    _ -> Left "More than one approved Domo cancellation policy version exists"

data LockedQuote = LockedQuote
  { lqCommerceQuoteId :: !Text
  , lqCheckoutId      :: !Text
  , lqQuoteStatus     :: !Text
  , lqCheckoutStatus  :: !Text
  , lqEnvironment     :: !Text
  , lqStartsAt        :: !UTCTime
  , lqDepositMinor    :: !Int64
  , lqPaidMinor       :: !Int64
  , lqRefundedMinor   :: !Int64
  , lqCurrency        :: !Text
  }

lockQuote :: Text -> SqlPersistT IO (Maybe LockedQuote)
lockQuote quoteId = do
  rows <- rawSql
    "SELECT runtime.quote_id::text, runtime.checkout_id::text, runtime.quote_status,\
    \ checkout.status, checkout.environment, runtime.starts_at, runtime.deposit_minor,\
    \ checkout.paid_minor, checkout.refunded_minor, runtime.currency\
    \ FROM domo_event_quote_runtime runtime\
    \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
    \ WHERE runtime.id = ?::uuid AND checkout.domain_type = 'domo_event_quote'\
    \ AND checkout.domain_order_id = 'domo-quote:' || runtime.id::text\
    \ AND checkout.currency = runtime.currency\
    \ FOR UPDATE OF runtime, checkout"
    [PersistText quoteId]
    :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single Text, Single Text, Single UTCTime
       , Single Int64, Single Int64, Single Int64, Single Text )]
  pure $ case rows of
    [( Single lqCommerceQuoteId, Single lqCheckoutId, Single lqQuoteStatus, Single lqCheckoutStatus
     , Single lqEnvironment, Single lqStartsAt, Single lqDepositMinor, Single lqPaidMinor
     , Single lqRefundedMinor, Single lqCurrency )] -> Just LockedQuote{..}
    _ -> Nothing

loadRecord :: Text -> SqlPersistT IO (Maybe DomoCancellationRecord)
loadRecord quoteId = do
  rows <- rawSql
    "SELECT runtime.quote_status, c.canceller, c.actor_type, c.policy_version, c.cancelled_at,\
    \ c.lead_days, c.in_progress, c.retained_basis_points, c.deposit_collected_minor,\
    \ c.balance_collected_minor, c.retained_minor, c.refund_due_minor, c.currency,\
    \ COALESCE(refund.status, c.refund_status), c.refund_id::text\
    \ FROM domo_quote_cancellation c\
    \ JOIN domo_event_quote_runtime runtime ON runtime.id = c.domo_quote_id\
    \ LEFT JOIN commerce_refund refund ON refund.id = c.refund_id\
    \ WHERE c.domo_quote_id = ?::uuid"
    [PersistText quoteId]
    :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single (Maybe Int), Single UTCTime
       , Single Int, Single Bool, Single Int, Single Int64, Single Int64, Single Int64
       , Single Int64, Single Text, Single Text, Single (Maybe Text) )]
  pure $ case rows of
    [( Single quoteStatus, Single canceller, Single actorType, Single policyVersion, Single cancelledAt
     , Single leadDays, Single inProgress, Single retainedBps, Single depositCollected
     , Single balanceCollected, Single retained, Single refundDue, Single currency
     , Single refundStatus, Single refundId )] -> Just DomoCancellationRecord
        { dcrQuoteId = quoteId
        , dcrQuoteStatus = quoteStatus
        , dcrCanceller = canceller
        , dcrActorType = actorType
        , dcrPolicyVersion = policyVersion
        , dcrCancelledAt = cancelledAt
        , dcrLeadDays = leadDays
        , dcrInProgress = inProgress
        , dcrRetainedBasisPoints = retainedBps
        , dcrDepositCollectedMinor = depositCollected
        , dcrBalanceCollectedMinor = balanceCollected
        , dcrRetainedMinor = retained
        , dcrRefundDueMinor = refundDue
        , dcrCurrency = currency
        , dcrRefundStatus = refundStatus
        , dcrRefundId = refundId
        , dcrReplayed = True
        }
    _ -> Nothing

data PaidAttempt = PaidAttempt
  { paAttemptId   :: !Text
  , paProvider    :: !Checkout.PaymentProvider
  , paProviderRef :: !Text
  , paMerchantRef :: !Text
  , paEnvironment :: !Checkout.CheckoutEnvironment
  }

providerFromText :: Text -> Either Text Checkout.PaymentProvider
providerFromText raw = case raw of
  "datafast" -> Right Checkout.ProviderDatafast
  "paypal" -> Right Checkout.ProviderPayPal
  "placetopay" -> Right Checkout.ProviderPlaceToPay
  "payphone" -> Right Checkout.ProviderPayPhone
  "stripe" -> Right Checkout.ProviderStripe
  "bank_transfer" -> Right Checkout.ProviderBankTransfer
  "cash" -> Right Checkout.ProviderCash
  "pos" -> Right Checkout.ProviderPos
  "cardano" -> Right Checkout.ProviderCardano
  _ -> Left ("Stored payment provider is invalid: " <> raw)

-- | The one succeeded payment attempt that paid the deposit checkout.
loadPaidAttempt :: LockedQuote -> SqlPersistT IO (Either Text PaidAttempt)
loadPaidAttempt locked = do
  rows <- rawSql
    "SELECT attempt.id::text, attempt.provider, attempt.merchant_account_ref, attempt.environment,\
    \ COALESCE((SELECT binding.provider_resource_id FROM commerce_provider_binding binding\
    \   WHERE binding.payment_attempt_id = attempt.id ORDER BY binding.created_at DESC LIMIT 1), attempt.id::text)\
    \ FROM commerce_payment_attempt attempt\
    \ WHERE attempt.checkout_id = ?::uuid AND attempt.status = 'succeeded'"
    [PersistText (lqCheckoutId locked)]
    :: SqlPersistT IO [(Single Text, Single Text, Single Text, Single Text, Single Text)]
  pure $ case rows of
    [(Single attemptId, Single provider, Single merchantRef, Single environment, Single providerRef)] -> do
      paProvider <- providerFromText provider
      paEnvironment <- Checkout.resolveCheckoutEnvironment (Just (T.unpack environment))
      Right PaidAttempt
        { paAttemptId = attemptId, paProvider, paProviderRef = providerRef
        , paMerchantRef = merchantRef, paEnvironment }
    [] -> Left "Paid Domo checkout has no succeeded payment attempt"
    _ -> Left "Paid Domo checkout has more than one succeeded payment attempt"

-- | Cancel a quote, or settle the open refund obligation of an already
-- cancelled one when an operator calls again. Idempotent for the customer.
cancelDomoQuote :: CancellationRequest -> SqlPersistT IO (Either CancellationError DomoCancellationRecord)
cancelDomoQuote CancellationRequest{..} = do
  mLocked <- lockQuote crQuoteId
  case mLocked of
    Nothing -> pure (Left CancellationNotFound)
    Just locked -> do
      inFlight <- hasUnverifiedCharge locked
      existing <- loadRecord crQuoteId
      case existing of
        Just record
          | dcrRefundStatus record == "awaiting_operator", OperatorActor party <- crActor ->
              settleObligation locked record party
          | otherwise -> pure (Right record)
        Nothing
          | lqQuoteStatus locked == "cancelled" ->
              pure (Left (CancellationNotAllowed "Quote was cancelled before cancellation records existed"))
          | lqQuoteStatus locked `elem` ["completed", "expired"] ->
              pure (Left (CancellationNotAllowed ("Quote is " <> lqQuoteStatus locked <> " and cannot be cancelled")))
          | lqCheckoutStatus locked == "processing" || inFlight ->
              pure (Left (CancellationNotAllowed "A deposit payment is being verified; retry once it settles"))
          | otherwise -> cancelFresh locked
  where
    collectedMinor locked = max 0 (lqPaidMinor locked - lqRefundedMinor locked)

    cancelFresh locked
      | collectedMinor locked <= 0 = do
          writeCancellation locked Nothing assessmentNoMoney "none" Nothing
          finish
      | otherwise = do
          policyResult <- approvedCancellationPolicy
          case policyResult of
            Left message -> pure (Left (CancellationPolicyUnavailable message))
            Right (version, policy) -> do
              let collected = collectedMinor locked
                  depositCollected = min collected (lqDepositMinor locked)
                  balanceCollected = collected - depositCollected
                  assessment = assessDomoCancellation policy crCanceller
                    (lqQuoteStatus locked `elem` ["in_progress", "balance_due"])
                    crNow (lqStartsAt locked) depositCollected balanceCollected
              if caRefundDueMinor assessment <= 0
                then do
                  writeCancellation locked (Just version) assessment "none" Nothing
                  finish
                else do
                  attemptResult <- loadPaidAttempt locked
                  case attemptResult of
                    Left message -> pure (Left (CancellationRefundRejected message))
                    Right attempt -> case crActor of
                      OperatorActor party -> do
                        refundResult <- requestObligation locked attempt party (caRefundDueMinor assessment) (cancellationReasonCode crCanceller)
                        case refundResult of
                          Left message -> pure (Left (CancellationRefundRejected message))
                          Right refund -> do
                            writeCancellation locked (Just version) assessment "requested" (Just refund)
                            recordObligation locked attempt (caRefundDueMinor assessment)
                            finish
                      CustomerActor -> do
                        writeCancellation locked (Just version) assessment "awaiting_operator" Nothing
                        recordObligation locked attempt (caRefundDueMinor assessment)
                        finish
      where
        assessmentNoMoney = CancellationAssessment
          { caLeadDays = leadDaysBefore crNow (lqStartsAt locked)
          , caInProgress = lqQuoteStatus locked `elem` ["in_progress", "balance_due"] || crNow >= lqStartsAt locked
          , caRetainedBasisPoints = 0
          , caDepositCollectedMinor = 0
          , caBalanceCollectedMinor = 0
          , caRetainedMinor = 0
          , caRefundDueMinor = 0
          }

    finish = do
      record <- loadRecord crQuoteId
      pure $ maybe (Left (CancellationNotAllowed "Cancellation record could not be reloaded"))
        (\r -> Right r { dcrReplayed = False }) record

    requestObligation locked attempt party amount reasonCode =
      Refund.requestSingleLineRefund Refund.RefundCreation
        { Refund.rcCheckout = Checkout.CheckoutReference (lqCheckoutId locked)
        , Refund.rcPaymentAttempt = Checkout.PaymentAttemptReference (paAttemptId attempt)
        , Refund.rcProvider = paProvider attempt
        , Refund.rcEnvironment = paEnvironment attempt
        , Refund.rcMerchantRef = paMerchantRef attempt
        , Refund.rcAmountMinor = amount
        , Refund.rcCurrency = lqCurrency locked
        , Refund.rcReasonCode = reasonCode
        , Refund.rcIdempotencyKey = cancellationRefundIdempotencyKey crQuoteId
        , Refund.rcRequestedBy = party
        , Refund.rcCreatedAt = crNow
        }

    recordObligation locked attempt amount =
      Checkout.recordReconciliationException
        (paProvider attempt) (paEnvironment attempt) (paMerchantRef attempt)
        refundObligationExceptionType ("domo-quote:" <> crQuoteId) (paProviderRef attempt)
        amount Nothing (lqCurrency locked) crNow

    -- Linking a request does not settle a financial obligation. The exception
    -- stays open until canonical verified completion in the same transaction
    -- as the ledger and credit note (domo refund lifecycle migration).
    settleObligation locked record party = do
      attemptResult <- loadPaidAttempt locked
      case attemptResult of
        Left message -> pure (Left (CancellationRefundRejected message))
        Right attempt -> do
          refundResult <- requestObligation locked attempt party (dcrRefundDueMinor record)
            (if dcrCanceller record == "tdf" then "service_cancelled" else "customer_request")
          case refundResult of
            Left message -> pure (Left (CancellationRefundRejected message))
            Right refund -> do
              rawExecute
                "UPDATE domo_quote_cancellation SET refund_id = ?::uuid, refund_status = 'requested',\
                \ refund_linked_at = ? WHERE domo_quote_id = ?::uuid AND refund_id IS NULL"
                [ PersistText (Refund.refundReferenceId (Refund.rrReference refund))
                , PersistUTCTime crNow, PersistText crQuoteId ]
              rawExecute
                "INSERT INTO domo_quote_state_event(domo_quote_id, from_status, to_status, actor_type,\
                \ reason_code, notes) VALUES (?::uuid, 'cancelled', 'cancelled', 'operator',\
                \ 'cancellation_refund_requested', ?)"
                [PersistText crQuoteId, PersistText ("Refund " <> Refund.refundReferenceId (Refund.rrReference refund))]
              finish

    writeCancellation locked policyVersion assessment refundStatus mRefund = do
      let (actorType, actorParty) = case crActor of
            CustomerActor -> ("customer" :: Text, PersistNull)
            OperatorActor party -> ("operator", PersistInt64 party)
          fromStatus = lqQuoteStatus locked
      rawExecute
        "INSERT INTO domo_quote_cancellation(domo_quote_id, policy_version, canceller, actor_type,\
        \ actor_party_id, cancelled_at, event_starts_at, lead_days, in_progress, retained_basis_points,\
        \ deposit_collected_minor, balance_collected_minor, retained_minor, refund_due_minor, currency,\
        \ reason_code, note, refund_status, refund_id, refund_linked_at)\
        \ VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?::uuid, ?)"
        [ PersistText crQuoteId
        , maybe PersistNull (PersistInt64 . fromIntegral) policyVersion
        , PersistText (cancellerText crCanceller), PersistText actorType, actorParty
        , PersistUTCTime crNow, PersistUTCTime (lqStartsAt locked)
        , PersistInt64 (fromIntegral (caLeadDays assessment)), PersistBool (caInProgress assessment)
        , PersistInt64 (fromIntegral (caRetainedBasisPoints assessment))
        , PersistInt64 (caDepositCollectedMinor assessment), PersistInt64 (caBalanceCollectedMinor assessment)
        , PersistInt64 (caRetainedMinor assessment), PersistInt64 (caRefundDueMinor assessment)
        , PersistText (lqCurrency locked), PersistText (cancellationReasonCode crCanceller)
        , maybe PersistNull PersistText crNote, PersistText refundStatus
        , maybe PersistNull (PersistText . Refund.refundReferenceId . Refund.rrReference) mRefund
        , maybe PersistNull (const (PersistUTCTime crNow)) mRefund
        ]
      rawExecute
        "UPDATE domo_event_quote_runtime SET quote_status = 'cancelled', fulfillment_status = 'cancelled',\
        \ updated_at = ? WHERE id = ?::uuid"
        [PersistUTCTime crNow, PersistText crQuoteId]
      rawExecute
        "UPDATE commerce_quote SET status = 'cancelled' WHERE id = ?::uuid\
        \ AND status IN ('draft','sent','viewed','accepted')"
        [PersistText (lqCommerceQuoteId locked)]
      unless (lqCheckoutStatus locked `elem` ["paid", "partially_refunded", "refunded", "disputed", "chargeback"]) $
        rawExecute
          "UPDATE commerce_checkout_session SET status = 'cancelled', updated_at = ? WHERE id = ?::uuid\
          \ AND status IN ('draft','validated','holding','awaiting_payment','failed')"
          [PersistUTCTime crNow, PersistText (lqCheckoutId locked)]
      rawExecute
        "UPDATE commerce_reservation_hold SET status = 'released' WHERE checkout_id = ?::uuid AND status = 'active'"
        [PersistText (lqCheckoutId locked)]
      rawExecute
        "INSERT INTO domo_quote_state_event(domo_quote_id, from_status, to_status, actor_type, reason_code, notes)\
        \ VALUES (?::uuid, ?, 'cancelled', ?, ?, ?)"
        [ PersistText crQuoteId, PersistText fromStatus, PersistText actorType
        , PersistText (case crCanceller of
            CancelledByCustomer -> "customer_cancelled"
            CancelledByTdf -> "tdf_cancelled")
        , PersistText (T.intercalate "; " (filter (not . T.null)
            [ "refund_due=" <> T.pack (show (caRefundDueMinor assessment))
            , "retained=" <> T.pack (show (caRetainedMinor assessment))
            , "lead_days=" <> T.pack (show (caLeadDays assessment))
            , fromMaybe "" crNote ]))
        ]

-- Called under the same quote/checkout locks as cancellation. A created
-- capture is already in flight: HTTP has not necessarily returned yet.
hasUnverifiedCharge :: LockedQuote -> SqlPersistT IO Bool
hasUnverifiedCharge locked = do
  rows <- rawSql
    "SELECT EXISTS (SELECT 1 FROM commerce_payment_attempt WHERE checkout_id=?::uuid\
    \ AND (operation='capture' OR provider='datafast')\
    \ AND status NOT IN ('succeeded','cancelled'))"
    [PersistText (lqCheckoutId locked)] :: SqlPersistT IO [Single Bool]
  pure (rows /= [Single False])

-- PaymentRuntime runs this guard after its save boundary, before inserting
-- the attempt, and commits the attempt before releasing these locks. Checking
-- the persisted attempt in cancellation closes the post-commit HTTP window.
domoPaymentAdmission
  :: Text -> Checkout.PaymentOperation -> SqlPersistT IO (Maybe Text)
domoPaymentAdmission quoteId operation = do
  target <- lockQuote quoteId
  case target of
    Just locked | lqQuoteStatus locked == "deposit_due"
                , lqCheckoutStatus locked `elem` ["awaiting_payment", "processing", "failed"] ->
      if operation /= Checkout.OperationCapture then pure Nothing else do
        previous <- rawSql
          "SELECT EXISTS (SELECT 1 FROM commerce_payment_attempt WHERE checkout_id=?::uuid\
          \ AND operation='capture')"
          [PersistText (lqCheckoutId locked)] :: SqlPersistT IO [Single Bool]
        pure $ if previous == [Single False] then Nothing
          else Just "Deposit capture already admitted; reconcile its outcome before retrying"
    _ -> pure (Just "Domo quote is no longer payable")
