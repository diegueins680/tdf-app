{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Commerce.RefundStore
  ( RefundCreation(..)
  , RefundReference(..)
  , RefundRecord(..)
  , VerifiedRefund(..)
  , requestSingleLineRefund
  , approveRefundForProcessing
  , recordRefundPending
  , recordRefundFailure
  , recordVerifiedRefund
  , loadRefund
  , listCheckoutRefunds
  , validateRefundAmount
  , validateRefundReason
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isAsciiLower, isDigit)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql)

import           TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment
  , CheckoutReference(..)
  , PaymentAttemptReference(..)
  , PaymentProvider
  , checkoutEnvironmentText
  , paymentProviderText
  )

newtype RefundReference = RefundReference
  { refundReferenceId :: Text
  } deriving (Eq, Show)

data RefundCreation = RefundCreation
  { rcCheckout       :: CheckoutReference
  , rcPaymentAttempt :: PaymentAttemptReference
  , rcProvider       :: PaymentProvider
  , rcEnvironment    :: CheckoutEnvironment
  , rcMerchantRef    :: Text
  , rcAmountMinor    :: Int64
  , rcCurrency       :: Text
  , rcReasonCode     :: Text
  , rcIdempotencyKey :: Text
  , rcRequestedBy    :: Int64
  , rcCreatedAt      :: UTCTime
  }

data RefundRecord = RefundRecord
  { rrReference        :: RefundReference
  , rrCheckout         :: CheckoutReference
  , rrPaymentAttempt   :: PaymentAttemptReference
  , rrProvider         :: Text
  , rrEnvironment      :: Text
  , rrMerchantRef      :: Text
  , rrProviderRefundId :: Maybe Text
  , rrStatus           :: Text
  , rrAmountMinor      :: Int64
  , rrCurrency         :: Text
  , rrReasonCode       :: Text
  , rrIdempotencyKey   :: Text
  , rrRequestedBy      :: Int64
  , rrApprovedBy       :: Maybe Int64
  , rrCreatedAt        :: UTCTime
  , rrCompletedAt      :: Maybe UTCTime
  } deriving (Eq, Show)

data VerifiedRefund = VerifiedRefund
  { vrRefund          :: RefundReference
  , vrProviderRefund  :: Text
  , vrAmountMinor     :: Int64
  , vrCurrency        :: Text
  , vrOccurredAt      :: UTCTime
  , vrCorrelationId   :: Text
  }

requestSingleLineRefund
  :: RefundCreation
  -> SqlPersistT IO (Either Text RefundRecord)
requestSingleLineRefund creation@RefundCreation{..}
  | rcRequestedBy <= 0 = pure (Left "Refund requester must be an authenticated party")
  | not (validIdempotencyKey rcIdempotencyKey) =
      pure (Left "Refund Idempotency-Key must contain 16 to 128 visible ASCII characters")
  | otherwise = case validateRefundReason rcReasonCode of
      Left message -> pure (Left message)
      Right reason -> do
        activeRows <- (rawSql
          "SELECT EXISTS (SELECT 1 FROM commerce_refund_reason_code\
          \ WHERE reason_code = ? AND active)"
          [PersistText reason] :: SqlPersistT IO [Single Bool])
        case activeRows of
          [Single True] -> requestAgainstPayment reason
          _ -> pure (Left "Refund reason code is not active")
  where
    requestAgainstPayment reason = do
      paymentRows <- (rawSql
        "SELECT checkout.paid_minor, checkout.refunded_minor, checkout.currency\
        \ FROM commerce_checkout_session checkout\
        \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id = checkout.id\
        \ WHERE checkout.id = ?::uuid AND attempt.id = ?::uuid\
        \ AND attempt.status = 'succeeded' AND attempt.provider = ?\
        \ AND attempt.environment = ? AND attempt.merchant_account_ref = ?\
        \ FOR UPDATE OF checkout"
        [ PersistText (checkoutReferenceId rcCheckout)
        , PersistText (paymentAttemptReferenceId rcPaymentAttempt)
        , PersistText (paymentProviderText rcProvider)
        , PersistText (checkoutEnvironmentText rcEnvironment)
        , PersistText rcMerchantRef
        ] :: SqlPersistT IO [(Single Int64, Single Int64, Single Text)])
      case paymentRows of
        [(Single paidMinor, Single refundedMinor, Single storedCurrency)] -> do
          reservedRows <- (rawSql
            "SELECT COALESCE(SUM(amount_minor), 0) FROM commerce_refund\
            \ WHERE checkout_id = ?::uuid\
            \ AND status IN ('requested','approved','processing')"
            [PersistText (checkoutReferenceId rcCheckout)] :: SqlPersistT IO [Single Int64])
          let reservedMinor = case reservedRows of
                [Single amount] -> amount
                _ -> paidMinor
              requestedCurrency = normalizeCurrency rcCurrency
          case validateRefundAmount
                paidMinor refundedMinor reservedMinor rcAmountMinor
                storedCurrency requestedCurrency of
            Left message -> pure (Left message)
            Right () -> do
              lineItems <- (rawSql
                "SELECT id::text FROM commerce_checkout_line_item\
                \ WHERE checkout_id = ?::uuid ORDER BY line_number"
                [PersistText (checkoutReferenceId rcCheckout)]
                :: SqlPersistT IO [Single Text])
              case lineItems of
                [Single lineItemId] ->
                  insertOrReplay lineItemId creation { rcReasonCode = reason }
                _ -> pure (Left
                  "Service refund requires exactly one immutable checkout line")
        [] -> pure (Left "Refund does not match a succeeded canonical payment")
        _ -> pure (Left "Refund payment binding is ambiguous")

approveRefundForProcessing
  :: RefundReference
  -> Int64
  -> UTCTime
  -> SqlPersistT IO (Either Text (RefundRecord, Bool))
approveRefundForProcessing refundRef approver now
  | approver <= 0 = pure (Left "Refund approver must be an authenticated party")
  | otherwise = do
      records <- loadRefundForUpdate refundRef
      case records of
        [record]
          | rrStatus record == "succeeded" -> pure (Right (record, False))
          | rrStatus record == "cancelled" -> pure (Left "Cancelled refund cannot be approved")
          | rrRequestedBy record == approver ->
              pure (Left "Refund approval requires a different authenticated party")
          | rrStatus record `elem` ["requested", "approved", "failed", "processing"] -> do
              let approvedBy = case rrApprovedBy record of
                    Just existing -> existing
                    Nothing -> approver
              when (rrStatus record == "requested") $
                rawExecute
                  "UPDATE commerce_refund SET status = 'approved', approved_by = ?,\
                  \ updated_at = ? WHERE id = ?::uuid AND status = 'requested'"
                  [ PersistInt64 approvedBy
                  , PersistUTCTime now
                  , PersistText (refundReferenceId refundRef)
                  ]
              rawExecute
                "UPDATE commerce_refund SET status = 'processing', approved_by = ?,\
                \ failure_code = NULL, failure_summary = NULL, updated_at = ?\
                \ WHERE id = ?::uuid AND status IN ('approved','failed','processing')"
                [ PersistInt64 approvedBy
                , PersistUTCTime now
                , PersistText (refundReferenceId refundRef)
                ]
              updated <- loadRefund refundRef
              maybe
                (pure (Left "Approved refund could not be reloaded"))
                (\value -> pure (Right (value, True)))
                updated
          | otherwise -> pure (Left "Refund is not in an approvable state")
        [] -> pure (Left "Refund was not found")
        _ -> pure (Left "Refund lookup was ambiguous")

recordRefundPending
  :: RefundReference
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text ())
recordRefundPending refundRef providerRefundId now
  | not (validProviderReference providerRefundId) =
      pure (Left "Provider refund ID is invalid")
  | otherwise = do
      rawExecute
        "UPDATE commerce_refund SET status = 'processing', provider_refund_id = ?,\
        \ updated_at = ? WHERE id = ?::uuid\
        \ AND status IN ('approved','processing','failed')\
        \ AND (provider_refund_id IS NULL OR provider_refund_id = ?)"
        [ PersistText providerRefundId
        , PersistUTCTime now
        , PersistText (refundReferenceId refundRef)
        , PersistText providerRefundId
        ]
      compatible <- providerRefundIsCompatible refundRef providerRefundId
      pure $ if compatible
        then Right ()
        else Left "Provider refund ID conflicts with immutable refund evidence"

recordRefundFailure
  :: RefundReference
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
recordRefundFailure refundRef failureCode now =
  rawExecute
    "UPDATE commerce_refund SET status = 'failed', failure_code = ?,\
    \ failure_summary = 'Provider refund failed; inspect redacted operational logs.',\
    \ updated_at = ? WHERE id = ?::uuid AND status = 'processing'"
    [ PersistText (T.take 120 failureCode)
    , PersistUTCTime now
    , PersistText (refundReferenceId refundRef)
    ]

recordVerifiedRefund
  :: VerifiedRefund
  -> SqlPersistT IO (Either Text Bool)
recordVerifiedRefund VerifiedRefund{..}
  | not (validProviderReference vrProviderRefund) =
      pure (Left "Verified provider refund ID is invalid")
  | vrAmountMinor <= 0 = pure (Left "Verified refund amount must be positive")
  | otherwise = do
      records <- loadRefundForUpdate vrRefund
      case records of
        [record]
          | rrAmountMinor record /= vrAmountMinor ->
              pure (Left "Provider refund amount does not match the immutable request")
          | rrCurrency record /= normalizeCurrency vrCurrency ->
              pure (Left "Provider refund currency does not match the immutable request")
          | rrStatus record == "succeeded" ->
              pure $ if rrProviderRefundId record == Just vrProviderRefund
                then Right False
                else Left "Succeeded refund conflicts with another provider refund ID"
          | rrStatus record /= "processing" ->
              pure (Left "Refund is not awaiting verified provider completion")
          | maybe False (/= vrProviderRefund) (rrProviderRefundId record) ->
              pure (Left "Provider refund ID conflicts with existing evidence")
          | otherwise -> do
              rawExecute
                "UPDATE commerce_refund SET status = 'succeeded', provider_refund_id = ?,\
                \ failure_code = NULL, failure_summary = NULL, completed_at = ?, updated_at = ?\
                \ WHERE id = ?::uuid"
                [ PersistText vrProviderRefund
                , PersistUTCTime vrOccurredAt
                , PersistUTCTime vrOccurredAt
                , PersistText (refundReferenceId vrRefund)
                ]
              rawExecute
                "UPDATE commerce_checkout_session\
                \ SET refunded_minor = refunded_minor + ?,\
                \ status = CASE WHEN refunded_minor + ? = paid_minor\
                \   THEN 'refunded' ELSE 'partially_refunded' END,\
                \ updated_at = ? WHERE id = ?::uuid"
                [ PersistInt64 vrAmountMinor
                , PersistInt64 vrAmountMinor
                , PersistUTCTime vrOccurredAt
                , PersistText (checkoutReferenceId (rrCheckout record))
                ]
              postRefundLedger record vrProviderRefund vrOccurredAt vrCorrelationId
              ensureCreditNote record vrProviderRefund vrOccurredAt
              insertRefundAudit record vrOccurredAt vrCorrelationId
              pure (Right True)
        [] -> pure (Left "Refund was not found")
        _ -> pure (Left "Refund lookup was ambiguous")

loadRefund
  :: RefundReference
  -> SqlPersistT IO (Maybe RefundRecord)
loadRefund refundRef = do
  records <- refundRows
    "WHERE refund.id = ?::uuid"
    [PersistText (refundReferenceId refundRef)]
  pure $ case records of
    [record] -> Just record
    _ -> Nothing

listCheckoutRefunds
  :: CheckoutReference
  -> SqlPersistT IO [RefundRecord]
listCheckoutRefunds checkout =
  refundRows
    "WHERE refund.checkout_id = ?::uuid ORDER BY refund.created_at DESC, refund.id DESC"
    [PersistText (checkoutReferenceId checkout)]

validateRefundAmount
  :: Int64
  -> Int64
  -> Int64
  -> Int64
  -> Text
  -> Text
  -> Either Text ()
validateRefundAmount paidMinor refundedMinor reservedMinor requestedMinor storedCurrency requestedCurrency
  | requestedMinor <= 0 = Left "Refund amount must be positive"
  | normalizeCurrency storedCurrency /= normalizeCurrency requestedCurrency =
      Left "Refund currency does not match the captured payment"
  | refundedMinor < 0 || reservedMinor < 0 || paidMinor <= 0 =
      Left "Stored refund balance is invalid"
  | requestedMinor > paidMinor - refundedMinor - reservedMinor =
      Left "Refund amount exceeds the unreserved captured balance"
  | otherwise = Right ()

validateRefundReason :: Text -> Either Text Text
validateRefundReason rawReason
  | T.length reason < 2 || T.length reason > 64 =
      Left "Refund reason code must contain 2 to 64 characters"
  | not (isAsciiLower (T.head reason)) =
      Left "Refund reason code must begin with a lowercase letter"
  | not (T.all (\character -> isAsciiLower character || isDigit character || character == '_') reason) =
      Left "Refund reason code contains unsupported characters"
  | otherwise = Right reason
  where
    reason = T.toLower (T.strip rawReason)

insertOrReplay
  :: Text
  -> RefundCreation
  -> SqlPersistT IO (Either Text RefundRecord)
insertOrReplay lineItemId RefundCreation{..} = do
  refundId <- liftIO (toText <$> nextRandom)
  inserted <- (rawSql
    "INSERT INTO commerce_refund (\
    \ id, checkout_id, payment_attempt_id, provider, environment, merchant_account_ref,\
    \ status, amount_minor, currency, reason_code, idempotency_key, requested_by,\
    \ created_at, updated_at\
    \) VALUES (?::uuid, ?::uuid, ?::uuid, ?, ?, ?, 'requested', ?, ?, ?, ?, ?, ?, ?)\
    \ ON CONFLICT (payment_attempt_id, idempotency_key) DO NOTHING RETURNING id::text"
    [ PersistText refundId
    , PersistText (checkoutReferenceId rcCheckout)
    , PersistText (paymentAttemptReferenceId rcPaymentAttempt)
    , PersistText (paymentProviderText rcProvider)
    , PersistText (checkoutEnvironmentText rcEnvironment)
    , PersistText rcMerchantRef
    , PersistInt64 rcAmountMinor
    , PersistText (normalizeCurrency rcCurrency)
    , PersistText rcReasonCode
    , PersistText rcIdempotencyKey
    , PersistInt64 rcRequestedBy
    , PersistUTCTime rcCreatedAt
    , PersistUTCTime rcCreatedAt
    ] :: SqlPersistT IO [Single Text])
  resolvedId <- case inserted of
    [Single newId] -> do
      rawExecute
        "INSERT INTO commerce_refund_allocation (refund_id, line_item_id, amount_minor)\
        \ VALUES (?::uuid, ?::uuid, ?)"
        [PersistText newId, PersistText lineItemId, PersistInt64 rcAmountMinor]
      pure (Right newId)
    [] -> do
      existing <- (rawSql
        "SELECT id::text FROM commerce_refund\
        \ WHERE payment_attempt_id = ?::uuid AND idempotency_key = ?\
        \ AND checkout_id = ?::uuid AND provider = ? AND environment = ?\
        \ AND merchant_account_ref = ? AND amount_minor = ? AND currency = ?\
        \ AND reason_code = ? AND requested_by = ?"
        [ PersistText (paymentAttemptReferenceId rcPaymentAttempt)
        , PersistText rcIdempotencyKey
        , PersistText (checkoutReferenceId rcCheckout)
        , PersistText (paymentProviderText rcProvider)
        , PersistText (checkoutEnvironmentText rcEnvironment)
        , PersistText rcMerchantRef
        , PersistInt64 rcAmountMinor
        , PersistText (normalizeCurrency rcCurrency)
        , PersistText rcReasonCode
        , PersistInt64 rcRequestedBy
        ] :: SqlPersistT IO [Single Text])
      pure $ case existing of
        [Single existingId] -> Right existingId
        _ -> Left "Refund idempotency key conflicts with another immutable request"
    _ -> pure (Left "Refund insert returned an ambiguous result")
  case resolvedId of
    Left message -> pure (Left message)
    Right value -> do
      record <- loadRefund (RefundReference value)
      maybe (pure (Left "Refund could not be loaded")) (pure . Right) record

loadRefundForUpdate :: RefundReference -> SqlPersistT IO [RefundRecord]
loadRefundForUpdate refundRef =
  refundRows
    "WHERE refund.id = ?::uuid FOR UPDATE OF refund"
    [PersistText (refundReferenceId refundRef)]

refundRows :: Text -> [PersistValue] -> SqlPersistT IO [RefundRecord]
refundRows suffix params = do
  rows <- (rawSql
    ("SELECT refund.id::text, refund.checkout_id::text, refund.payment_attempt_id::text,\
     \ refund.provider, refund.environment, refund.merchant_account_ref,\
     \ refund.provider_refund_id, refund.status, refund.amount_minor, refund.currency,\
     \ refund.reason_code, refund.idempotency_key, refund.requested_by, refund.approved_by,\
     \ refund.created_at, refund.completed_at FROM commerce_refund refund " <> suffix)
    params :: SqlPersistT IO
      [( Single Text, Single Text, Single Text, Single (Maybe Text)
       , Single (Maybe Text), Single (Maybe Text), Single (Maybe Text), Single Text
       , Single Int64, Single Text, Single Text, Single Text, Single Int64
       , Single (Maybe Int64), Single UTCTime, Single (Maybe UTCTime)
       )])
  pure (map toRecord rows)
  where
    toRecord
      ( Single refundId, Single checkoutId, Single attemptId, Single mProvider
      , Single mEnvironment, Single mMerchantRef, Single providerRefundId, Single status
      , Single amount, Single currency, Single reason, Single idempotencyKey
      , Single requestedBy, Single approvedBy, Single createdAt, Single completedAt
      ) = RefundRecord
          { rrReference = RefundReference refundId
          , rrCheckout = CheckoutReference checkoutId
          , rrPaymentAttempt = PaymentAttemptReference attemptId
          , rrProvider = maybe "legacy" id mProvider
          , rrEnvironment = maybe "legacy" id mEnvironment
          , rrMerchantRef = maybe "legacy" id mMerchantRef
          , rrProviderRefundId = providerRefundId
          , rrStatus = status
          , rrAmountMinor = amount
          , rrCurrency = currency
          , rrReasonCode = reason
          , rrIdempotencyKey = idempotencyKey
          , rrRequestedBy = requestedBy
          , rrApprovedBy = approvedBy
          , rrCreatedAt = createdAt
          , rrCompletedAt = completedAt
          }

providerRefundIsCompatible :: RefundReference -> Text -> SqlPersistT IO Bool
providerRefundIsCompatible refundRef providerRefundId = do
  rows <- (rawSql
    "SELECT provider_refund_id = ? FROM commerce_refund WHERE id = ?::uuid"
    [ PersistText providerRefundId
    , PersistText (refundReferenceId refundRef)
    ] :: SqlPersistT IO [Single (Maybe Bool)])
  pure (rows == [Single (Just True)])

postRefundLedger :: RefundRecord -> Text -> UTCTime -> Text -> SqlPersistT IO ()
postRefundLedger record providerRefundId occurredAt correlationId = do
  ledgerId <- liftIO (toText <$> nextRandom)
  created <- (rawSql
    "INSERT INTO commerce_ledger_transaction (\
    \ id, transaction_type, source_type, source_id, status, effective_at,\
    \ correlation_id, created_by\
    \) VALUES (?::uuid, 'payment_refund', 'refund', ?, 'draft', ?, ?, ?)\
    \ ON CONFLICT (source_type, source_id, transaction_type) DO NOTHING\
    \ RETURNING id::text"
    [ PersistText ledgerId
    , PersistText (refundReferenceId (rrReference record))
    , PersistUTCTime occurredAt
    , PersistText correlationId
    , PersistText (rrProvider record <> "_verified_refund")
    ] :: SqlPersistT IO [Single Text])
  case created of
    [Single newLedgerId] -> do
      rawExecute
        "INSERT INTO commerce_ledger_entry (\
        \ transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo\
        \) SELECT ?::uuid, 'revenue.service_storefront', domain_type, domain_order_id,\
        \ ?, ?, 'Verified provider refund' FROM commerce_checkout_session\
        \ WHERE id = ?::uuid"
        [ PersistText newLedgerId
        , PersistText (rrCurrency record)
        , PersistInt64 (rrAmountMinor record)
        , PersistText (checkoutReferenceId (rrCheckout record))
        ]
      rawExecute
        "INSERT INTO commerce_ledger_entry (\
        \ transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo\
        \) SELECT ?::uuid, ?, domain_type, domain_order_id, ?, ?, ?\
        \ FROM commerce_checkout_session WHERE id = ?::uuid"
        [ PersistText newLedgerId
        , PersistText ("cash." <> rrProvider record)
        , PersistText (rrCurrency record)
        , PersistInt64 (negate (rrAmountMinor record))
        , PersistText ("Provider refund " <> providerRefundId)
        , PersistText (checkoutReferenceId (rrCheckout record))
        ]
      rawExecute
        "UPDATE commerce_ledger_transaction SET status = 'posted' WHERE id = ?::uuid"
        [PersistText newLedgerId]
    _ -> pure ()

ensureCreditNote :: RefundRecord -> Text -> UTCTime -> SqlPersistT IO ()
ensureCreditNote record providerRefundId occurredAt = do
  receiptId <- liftIO (toText <$> nextRandom)
  rawExecute
    "INSERT INTO commerce_receipt (\
    \ id, checkout_id, refund_id, receipt_number, kind, adapter, external_reference,\
    \ amount_minor, currency, issued_at\
    \) VALUES (?::uuid, ?::uuid, ?::uuid, ?, 'credit_note', ?, ?, ?, ?, ?)\
    \ ON CONFLICT DO NOTHING"
    [ PersistText receiptId
    , PersistText (checkoutReferenceId (rrCheckout record))
    , PersistText (refundReferenceId (rrReference record))
    , PersistText (creditNoteNumber (rrReference record))
    , PersistText (rrProvider record)
    , PersistText providerRefundId
    , PersistInt64 (rrAmountMinor record)
    , PersistText (rrCurrency record)
    , PersistUTCTime occurredAt
    ]

insertRefundAudit :: RefundRecord -> UTCTime -> Text -> SqlPersistT IO ()
insertRefundAudit record _occurredAt correlationId =
  rawExecute
    "INSERT INTO commerce_checkout_audit_event (\
    \ checkout_id, event_type, from_status, to_status, actor_type, correlation_id, metadata\
    \) VALUES (?::uuid, 'refund_verified', NULL,\
    \ CASE WHEN (SELECT refunded_minor = paid_minor FROM commerce_checkout_session\
    \   WHERE id = ?::uuid) THEN 'refunded' ELSE 'partially_refunded' END,\
    \ ?, ?, jsonb_build_object('refund_id', ?, 'amount_minor', ?))"
    [ PersistText (checkoutReferenceId (rrCheckout record))
    , PersistText (checkoutReferenceId (rrCheckout record))
    , PersistText (rrProvider record)
    , PersistText correlationId
    , PersistText (refundReferenceId (rrReference record))
    , PersistInt64 (rrAmountMinor record)
    ]

creditNoteNumber :: RefundReference -> Text
creditNoteNumber =
  ("TDF-CN-" <>) . T.toUpper . T.filter (/= '-') . refundReferenceId

normalizeCurrency :: Text -> Text
normalizeCurrency = T.toUpper . T.strip

validIdempotencyKey :: Text -> Bool
validIdempotencyKey value =
  let normalized = T.strip value
  in T.length normalized >= 16
      && T.length normalized <= 128
      && T.all (\character -> character >= '!' && character <= '~') normalized

validProviderReference :: Text -> Bool
validProviderReference value =
  let normalized = T.strip value
  in not (T.null normalized)
      && T.length normalized <= 256
      && T.all (\character ->
        (character >= '0' && character <= '9')
          || (character >= 'A' && character <= 'Z')
          || (character >= 'a' && character <= 'z')
          || character `elem` ("-_." :: String)) normalized
