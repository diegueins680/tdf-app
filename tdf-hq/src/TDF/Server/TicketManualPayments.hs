{-# LANGUAGE OverloadedStrings #-}

-- | Staff review of public ticket orders paid by manual bank transfer. The
-- customer's evidence never settles an order by itself: an authorized reviewer
-- who is neither the submitting party nor the buyer's email approves it, and the
-- canonical checkout store records the staff-verified payment exactly once.
module TDF.Server.TicketManualPayments
  ( ManualReviewAction(..)
  , ManualReviewer(..)
  , parseManualReviewAction
  , validateManualReviewNotes
  , listTicketManualPayments
  , reviewTicketManualPayment
  , validateBankRefundReference
  , completeBankTransferTicketRefund
  ) where

import           Control.Monad (when)
import           Data.Char (isControl)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.Persist.Sql
  ( PersistValue(..), Single(..), SqlPersistT, fromSqlKey, rawExecute, rawSql
  , toPersistValue
  )

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.RefundStore as Refund
import qualified TDF.Ticketing.Refund as TicketRefund
import           TDF.DTO.SocialEventsDTO (TicketManualPaymentDTO(..))
import qualified TDF.Models.SocialEventsModels as SM

data ManualReviewAction = ManualApprove | ManualReject
  deriving (Eq, Show)

data ManualReviewer = ManualReviewer
  { mrPartyId     :: Int64
  , mrStrictAdmin :: Bool
  } deriving (Eq, Show)

parseManualReviewAction :: Text -> Either Text ManualReviewAction
parseManualReviewAction raw = case T.toLower (T.strip raw) of
  "approve" -> Right ManualApprove
  "reject" -> Right ManualReject
  _ -> Left "Review action must be approve or reject"

-- Mirrors the database bounds on commerce_manual_payment_evidence.review_notes.
validateManualReviewNotes :: Text -> Either Text Text
validateManualReviewNotes raw
  | T.length clean < 3 || T.length clean > 2000 =
      Left "Review notes must contain 3 to 2000 characters"
  | T.any isControl clean = Left "Review notes contain unsupported characters"
  | otherwise = Right clean
  where
    clean = T.strip raw

listTicketManualPayments :: SM.SocialEventId -> SqlPersistT IO [TicketManualPaymentDTO]
listTicketManualPayments eventKey = do
  rows <- rawSql
    "SELECT runtime.order_id, ticket_order.buyer_name, ticket_order.buyer_email,\
    \ runtime.quantity, runtime.checkout_total_minor, runtime.currency, evidence.status,\
    \ evidence.customer_reference, evidence.submitted_at, evidence.reviewed_at,\
    \ evidence.review_notes, checkout.status,\
    \ GREATEST(runtime.hold_expires_at, runtime.manual_hold_expires_at)\
    \ FROM event_ticket_checkout_runtime runtime\
    \ JOIN event_ticket_order ticket_order ON ticket_order.id = runtime.order_id\
    \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
    \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id = checkout.id\
    \ JOIN commerce_manual_payment_evidence evidence\
    \   ON evidence.payment_attempt_id = attempt.id AND evidence.checkout_id = checkout.id\
    \ WHERE runtime.event_id = ? AND attempt.provider = 'bank_transfer'\
    \ AND attempt.operation = 'manual_verify'\
    \ ORDER BY (evidence.status IN ('submitted','under_review')) DESC,\
    \ evidence.submitted_at NULLS LAST, runtime.order_id"
    [toPersistValue eventKey]
    :: SqlPersistT IO
      [( Single Int64, Single (Maybe Text), Single (Maybe Text), Single Int, Single Int64
       , Single Text, Single Text, Single (Maybe Text), Single (Maybe UTCTime)
       , Single (Maybe UTCTime), Single (Maybe Text), Single Text, Single UTCTime
       )]
  pure (map toDTO rows)
  where
    toDTO ( Single orderId, Single buyerName, Single buyerEmail, Single quantity
          , Single amountMinor, Single currency, Single evidenceStatus
          , Single customerReference, Single submittedAt, Single reviewedAt
          , Single reviewNotes, Single checkoutStatus, Single holdExpiresAt
          ) = TicketManualPaymentDTO
      { tmpOrderId = T.pack (show orderId)
      , tmpPaymentReference = "TDF-" <> T.pack (show orderId)
      , tmpBuyerName = buyerName
      , tmpBuyerEmail = buyerEmail
      , tmpQuantity = quantity
      , tmpAmountMinor = fromIntegral amountMinor
      , tmpCurrency = currency
      , tmpEvidenceStatus = evidenceStatus
      , tmpCustomerReference = customerReference
      , tmpSubmittedAt = submittedAt
      , tmpReviewedAt = reviewedAt
      , tmpReviewNotes = reviewNotes
      , tmpCheckoutStatus = checkoutStatus
      , tmpHoldExpiresAt = holdExpiresAt
      }

-- | Returns 'True' when this call (or an identical earlier approval) left the
-- checkout paid. The idempotent ticket issuance then runs in the same
-- transaction: a manual approval has no external effect to preserve, so an
-- issuance failure rolls the approval back and the evidence stays reviewable,
-- instead of leaving a paid order without tickets and no review action left.
reviewTicketManualPayment
  :: ManualReviewer
  -> SM.SocialEventId
  -> SM.EventTicketOrderId
  -> ManualReviewAction
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
  -> SqlPersistT IO (Either Text Bool)
reviewTicketManualPayment reviewer eventKey orderKey action notes now issueTickets = do
  result <- decideTicketManualPayment reviewer eventKey orderKey action notes now
  when (result == Right True) issueTickets
  pure result

decideTicketManualPayment
  :: ManualReviewer
  -> SM.SocialEventId
  -> SM.EventTicketOrderId
  -> ManualReviewAction
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text Bool)
decideTicketManualPayment reviewer eventKey orderKey action notes now = do
  rows <- rawSql
    "SELECT checkout.id::text, checkout.status, checkout.environment,\
    \ GREATEST(runtime.hold_expires_at, runtime.manual_hold_expires_at),\
    \ attempt.id::text, attempt.merchant_account_ref, attempt.status,\
    \ evidence.id::text, evidence.status, evidence.submitted_by, evidence.reviewed_by,\
    \ runtime.checkout_total_minor, runtime.currency, ticket_order.buyer_email,\
    \ event.organizer_party_id, reviewer.primary_email\
    \ FROM event_ticket_checkout_runtime runtime\
    \ JOIN event_ticket_order ticket_order ON ticket_order.id = runtime.order_id\
    \ JOIN social_event event ON event.id = runtime.event_id\
    \ JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id\
    \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id = checkout.id\
    \ JOIN commerce_manual_payment_evidence evidence\
    \   ON evidence.payment_attempt_id = attempt.id AND evidence.checkout_id = checkout.id\
    \ LEFT JOIN party reviewer ON reviewer.id = ?\
    \ WHERE runtime.order_id = ? AND runtime.event_id = ?\
    \ AND checkout.domain_type = 'event_ticket_order'\
    \ AND checkout.domain_order_id = runtime.order_id::text\
    \ AND attempt.provider = 'bank_transfer' AND attempt.operation = 'manual_verify'\
    \ FOR UPDATE OF checkout, attempt, evidence"
    [PersistInt64 reviewerId, toPersistValue orderKey, toPersistValue eventKey]
    :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single UTCTime, Single Text, Single Text
       , Single Text, Single Text, Single Text, Single (Maybe Int64), Single (Maybe Int64)
       , Single Int64, Single Text, Single (Maybe Text), Single (Maybe Text)
       , Single (Maybe Text)
       )]
  case rows of
    [] -> pure (Left "No bank transfer evidence exists for this ticket order")
    (_:_:_) -> pure (Left "Bank transfer evidence is ambiguous and requires reconciliation")
    [( Single checkoutId, Single checkoutStatus, Single environmentText, Single holdExpiresAt
     , Single attemptId, Single merchantRef, Single attemptStatus, Single evidenceId
     , Single evidenceStatus, Single mSubmittedBy, Single mReviewedBy, Single amountMinor
     , Single currency, Single mBuyerEmail, Single mOrganizer, Single mReviewerEmail
     )] -> case Checkout.resolveCheckoutEnvironment (Just (T.unpack environmentText)) of
      Left problem -> pure (Left problem)
      Right environment -> decide Decision
        { dCheckout = Checkout.CheckoutReference checkoutId
        , dCheckoutStatus = checkoutStatus
        , dEnvironment = environment
        , dHoldExpiresAt = holdExpiresAt
        , dAttempt = attemptId
        , dMerchantRef = merchantRef
        , dAttemptStatus = attemptStatus
        , dEvidence = evidenceId
        , dEvidenceStatus = evidenceStatus
        , dSubmittedBy = mSubmittedBy
        , dReviewedBy = mReviewedBy
        , dAmountMinor = amountMinor
        , dCurrency = currency
        , dBuyerEmail = mBuyerEmail
        , dOrganizer = mOrganizer
        , dReviewerEmail = mReviewerEmail
        }
  where
    reviewerId = mrPartyId reviewer
    orderReference = T.pack (show (fromSqlKey orderKey))
    correlation = "event-ticket:" <> orderReference <> ":bank_transfer:manual-review"
    sameEmail a b = T.toCaseFold (T.strip a) == T.toCaseFold (T.strip b)

    decide :: Decision -> SqlPersistT IO (Either Text Bool)
    decide d
      | not (mrStrictAdmin reviewer || dOrganizer d == Just (T.pack (show reviewerId))) =
          pure (Left "Only the event organizer or an administrator may review this payment")
      | dSubmittedBy d == Nothing =
          pure (Left "Bank transfer evidence has no recorded submitter")
      | dSubmittedBy d == Just reviewerId =
          pure (Left "Bank transfer evidence requires an independent reviewer")
      | Just buyer <- dBuyerEmail d, Just mine <- dReviewerEmail d, sameEmail buyer mine =
          pure (Left "Staff cannot review a transfer for an order bought with their own email")
      | dEvidenceStatus d == "approved" && action == ManualApprove
          && dCheckoutStatus d == "paid" && dAttemptStatus d == "succeeded" = pure (Right True)
      | dEvidenceStatus d == "approved" =
          pure (Left "Approved bank transfer evidence cannot be changed")
      | dEvidenceStatus d == "rejected" && action == ManualReject = pure (Right False)
      | dEvidenceStatus d == "rejected" =
          pure (Left "Rejected evidence must be resubmitted by the buyer before approval")
      | dEvidenceStatus d `notElem` ["submitted", "under_review"] =
          pure (Left "The buyer has not submitted a transfer reference yet")
      | dEvidenceStatus d == "under_review" && dReviewedBy d /= Just reviewerId =
          pure (Left "This transfer is already under review by another staff member")
      | dCheckoutStatus d == "paid" = do
          exception d "manual_evidence_after_other_payment"
          pure (Left "This ticket order is already paid by another payment attempt")
      | action == ManualApprove && dHoldExpiresAt d <= now = do
          exception d "manual_payment_after_ticket_hold_expiry"
          pure (Left "The seat hold expired; the transfer requires reconciliation or refund and no ticket was issued")
      | action == ManualApprove
          && dCheckoutStatus d `notElem` ["awaiting_payment", "failed", "processing"] =
          pure (Left "This ticket checkout no longer accepts manual payment approval")
      | otherwise = do
          -- A savepoint, not a commit: the row locks taken above must cover the
          -- decision, or a concurrent reviewer acts on the same pre-image.
          rawExecute "SAVEPOINT tdf_manual_review" []
          when (dEvidenceStatus d == "submitted") $
            rawExecute
              "UPDATE commerce_manual_payment_evidence\
              \ SET status = 'under_review', reviewed_by = ?, review_notes = ?\
              \ WHERE id = ?::uuid"
              [PersistInt64 reviewerId, PersistText notes, PersistText (dEvidence d)]
          case action of
            ManualReject -> do
              rawExecute
                "UPDATE commerce_manual_payment_evidence\
                \ SET status = 'rejected', reviewed_at = ?, review_notes = ?\
                \ WHERE id = ?::uuid"
                [PersistUTCTime now, PersistText notes, PersistText (dEvidence d)]
              Checkout.recordPaymentFailure (dCheckout d)
                (Checkout.PaymentAttemptReference (dAttempt d))
                Checkout.ProviderBankTransfer "manual_evidence_rejected" correlation now
              audit d "manual_payment_rejected"
              keepDecision
              pure (Right False)
            ManualApprove -> do
              rawExecute
                "UPDATE commerce_manual_payment_evidence\
                \ SET status = 'approved', reviewed_at = ?, review_notes = ?\
                \ WHERE id = ?::uuid"
                [PersistUTCTime now, PersistText notes, PersistText (dEvidence d)]
              binding <- Checkout.bindProviderResource Checkout.ProviderBindingCreation
                { Checkout.pbcAttempt = Checkout.PaymentAttemptReference (dAttempt d)
                , Checkout.pbcCheckout = dCheckout d
                , Checkout.pbcProvider = Checkout.ProviderBankTransfer
                , Checkout.pbcEnvironment = dEnvironment d
                , Checkout.pbcMerchantRef = dMerchantRef d
                , Checkout.pbcResourceType = "manual_evidence"
                , Checkout.pbcProviderResource = dEvidence d
                , Checkout.pbcResourcePath = Nothing
                , Checkout.pbcOrderReference = orderReference
                , Checkout.pbcAmountMinor = dAmountMinor d
                , Checkout.pbcCurrency = dCurrency d
                , Checkout.pbcStage = Checkout.AttemptProcessing
                , Checkout.pbcOccurredAt = now
                , Checkout.pbcCorrelationId = correlation
                }
              case binding of
                Left problem -> undoDecision >> pure (Left problem)
                Right () -> do
                  verified <- Checkout.recordApprovedManualPayment Checkout.VerifiedPayment
                    { Checkout.vpAttempt = Checkout.PaymentAttemptReference (dAttempt d)
                    , Checkout.vpCheckout = dCheckout d
                    , Checkout.vpProvider = Checkout.ProviderBankTransfer
                    , Checkout.vpEnvironment = dEnvironment d
                    , Checkout.vpMerchantRef = dMerchantRef d
                    , Checkout.vpResourceType = "manual_evidence"
                    , Checkout.vpProviderResource = dEvidence d
                    , Checkout.vpProviderResourcePath = Nothing
                    , Checkout.vpOrderReference = orderReference
                    , Checkout.vpProviderReference = orderReference
                    , Checkout.vpAmountMinor = dAmountMinor d
                    , Checkout.vpCurrency = dCurrency d
                    , Checkout.vpEvidence = "staff_verified_manual"
                    , Checkout.vpOccurredAt = now
                    , Checkout.vpCorrelationId = correlation
                    }
                  case verified of
                    Left problem -> undoDecision >> pure (Left problem)
                    Right _ -> do
                      audit d "manual_payment_approved"
                      keepDecision
                      pure (Right True)

    keepDecision, undoDecision :: SqlPersistT IO ()
    keepDecision = rawExecute "RELEASE SAVEPOINT tdf_manual_review" []
    undoDecision = do
      rawExecute "ROLLBACK TO SAVEPOINT tdf_manual_review" []
      keepDecision

    exception :: Decision -> Text -> SqlPersistT IO ()
    exception d code = Checkout.recordReconciliationException
      Checkout.ProviderBankTransfer (dEnvironment d) (dMerchantRef d) code
      orderReference (dEvidence d) (dAmountMinor d) (Just (dAmountMinor d)) (dCurrency d) now

    audit :: Decision -> Text -> SqlPersistT IO ()
    audit d eventType = rawExecute
      "INSERT INTO commerce_checkout_audit_event(\
      \ checkout_id, event_type, actor_type, actor_id, correlation_id, metadata\
      \) VALUES (?::uuid, ?, 'staff', ?, ?,\
      \ jsonb_build_object('attempt_id', ?, 'evidence_id', ?))"
      [ PersistText (Checkout.checkoutReferenceId (dCheckout d))
      , PersistText eventType
      , PersistText (T.pack (show reviewerId))
      , PersistText correlation
      , PersistText (dAttempt d)
      , PersistText (dEvidence d)
      ]

data Decision = Decision
  { dCheckout       :: Checkout.CheckoutReference
  , dCheckoutStatus :: Text
  , dEnvironment    :: Checkout.CheckoutEnvironment
  , dHoldExpiresAt  :: UTCTime
  , dAttempt        :: Text
  , dMerchantRef    :: Text
  , dAttemptStatus  :: Text
  , dEvidence       :: Text
  , dEvidenceStatus :: Text
  , dSubmittedBy    :: Maybe Int64
  , dReviewedBy     :: Maybe Int64
  , dAmountMinor    :: Int64
  , dCurrency       :: Text
  , dBuyerEmail     :: Maybe Text
  , dOrganizer      :: Maybe Text
  , dReviewerEmail  :: Maybe Text
  }

-- | The reference of the bank transfer staff made back to the buyer. It becomes
-- the canonical provider refund ID, so it must be stable and unique per refund.
validateBankRefundReference :: Text -> Either Text Text
validateBankRefundReference raw
  | T.length clean < 3 || T.length clean > 60 =
      Left "Bank refund reference must contain 3 to 60 characters"
  | T.any (\c -> not (isAsciiAlphaNum c || c `elem` ("-_." :: String))) clean =
      Left "Bank refund reference may contain letters, digits, '-', '_' and '.'"
  | otherwise = Right ("BT-" <> clean)
  where
    clean = T.strip raw
    isAsciiAlphaNum c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

-- | Complete a requested refund of a bank-transfer ticket order after staff
-- returned the money. The completing party must differ from the requester
-- (RefundStore), and only the event organizer or an administrator may act.
-- Ticket invalidation and inventory release follow the canonical projection.
completeBankTransferTicketRefund
  :: ManualReviewer
  -> SM.SocialEventId
  -> SM.TicketRefundRequestId
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text ())
completeBankTransferTicketRefund reviewer eventKey requestKey providerRefund now = do
  binding <- TicketRefund.loadTicketRefundReference requestKey
  case binding of
    Nothing -> pure (Left "This refund request has no canonical refund")
    Just ref -> do
      authority <- rawSql
        "SELECT event.organizer_party_id FROM ticket_refund_request request\
        \ JOIN event_ticket_order ticket_order ON ticket_order.id = request.order_id\
        \ JOIN social_event event ON event.id = ticket_order.event_id\
        \ WHERE request.id = ? AND ticket_order.event_id = ?"
        [toPersistValue requestKey, toPersistValue eventKey]
        :: SqlPersistT IO [Single (Maybe Text)]
      record <- Refund.loadRefund ref
      case (authority, record) of
        ([Single organizer], Just existing)
          | not (mrStrictAdmin reviewer || organizer == Just (T.pack (show (mrPartyId reviewer)))) ->
              pure (Left "Only the event organizer or an administrator may complete this refund")
          | Refund.rrProvider existing /= "bank_transfer" ->
              pure (Left "Only bank-transfer payments are refunded manually; use refund approval")
          | otherwise -> do
              approved <- Refund.approveRefundForProcessing ref (mrPartyId reviewer) now
              case approved of
                Left problem -> pure (Left problem)
                Right _ -> do
                  _ <- TicketRefund.completeTicketRefund Refund.VerifiedRefund
                    { Refund.vrRefund = ref
                    , Refund.vrProviderRefund = providerRefund
                    , Refund.vrAmountMinor = Refund.rrAmountMinor existing
                    , Refund.vrCurrency = Refund.rrCurrency existing
                    , Refund.vrOccurredAt = now
                    , Refund.vrCorrelationId = "event-ticket-refund:" <> Refund.refundReferenceId ref <> ":bank_transfer:manual"
                    }
                  pure (Right ())
        _ -> pure (Left "Refund request does not belong to this event")
