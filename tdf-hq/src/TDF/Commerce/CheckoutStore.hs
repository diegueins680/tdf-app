{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment(..)
  , CheckoutCreation(..)
  , CheckoutLineCreation(..)
  , CheckoutReference(..)
  , PaymentProvider(..)
  , PaymentOperation(..)
  , PaymentAttemptStage(..)
  , PaymentAttemptCreation(..)
  , PaymentAttemptReference(..)
  , ProviderBindingCreation(..)
  , VerifiedPayment(..)
  , resolveCheckoutEnvironment
  , checkoutEnvironmentText
  , paymentProviderText
  , createCheckout
  , createCheckoutWithLines
  , loadCheckoutEnvironment
  , beginPaymentAttempt
  , bindProviderResource
  , recordPaymentFailure
  , recordPaymentProcessing
  , recordManualPaymentSelection
  , recordVerifiedPayment
  , recordApprovedManualPayment
  , validateApprovedManualPayment
  , recordReconciliationException
  , providerEnabledForEnvironment
  , domainEnabledForEnvironment
  , capabilityEnabledForEnvironment
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Value, encode, object, (.=))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql)

data CheckoutEnvironment
  = CheckoutSandbox
  | CheckoutProduction
  deriving (Eq, Show)

newtype CheckoutReference = CheckoutReference
  { checkoutReferenceId :: Text
  } deriving (Eq, Show)

data CheckoutCreation = CheckoutCreation
  { ccDomainType       :: Text
  , ccDomainOrderId    :: Text
  , ccEnvironment      :: CheckoutEnvironment
  , ccCurrency         :: Text
  , ccAmountMinor      :: Int64
  , ccCustomerEmail    :: Text
  , ccLookupTokenHash  :: Text
  , ccIdempotencyKey   :: Text
  , ccExpiresAt        :: UTCTime
  , ccProductType      :: Text
  , ccProductId        :: Text
  , ccProductVersion   :: Text
  , ccDescription      :: Text
  , ccSnapshot         :: Value
  , ccCorrelationId    :: Text
  }

data CheckoutLineCreation = CheckoutLineCreation
  { clProductType    :: Text
  , clProductId      :: Text
  , clProductVersion :: Text
  , clDescription    :: Text
  , clQuantity       :: Int
  , clUnitAmountMinor :: Int64
  , clSnapshot       :: Value
  }

data PaymentProvider
  = ProviderDatafast
  | ProviderPayPal
  | ProviderStripe
  | ProviderBankTransfer
  | ProviderCash
  | ProviderPos
  | ProviderCardano
  deriving (Eq, Show)

data PaymentOperation
  = OperationCreate
  | OperationAuthorize
  | OperationCapture
  | OperationManualVerify
  deriving (Eq, Show)

data PaymentAttemptStage
  = AttemptCreated
  | AttemptRequiresCustomerAction
  | AttemptProcessing
  | AttemptSucceeded
  | AttemptFailed
  | AttemptRequiresReview
  deriving (Eq, Show)

data PaymentAttemptCreation = PaymentAttemptCreation
  { pacCheckout       :: CheckoutReference
  , pacProvider       :: PaymentProvider
  , pacEnvironment    :: CheckoutEnvironment
  , pacOperation      :: PaymentOperation
  , pacAmountMinor    :: Int64
  , pacCurrency       :: Text
  , pacMerchantRef    :: Text
  , pacIdempotencyKey :: Text
  , pacCreatedAt      :: UTCTime
  , pacCorrelationId  :: Text
  }

newtype PaymentAttemptReference = PaymentAttemptReference
  { paymentAttemptReferenceId :: Text
  } deriving (Eq, Show)

data ProviderBindingCreation = ProviderBindingCreation
  { pbcAttempt          :: PaymentAttemptReference
  , pbcCheckout         :: CheckoutReference
  , pbcProvider         :: PaymentProvider
  , pbcEnvironment      :: CheckoutEnvironment
  , pbcMerchantRef      :: Text
  , pbcResourceType     :: Text
  , pbcProviderResource :: Text
  , pbcResourcePath     :: Maybe Text
  , pbcOrderReference   :: Text
  , pbcAmountMinor      :: Int64
  , pbcCurrency         :: Text
  , pbcStage            :: PaymentAttemptStage
  , pbcOccurredAt       :: UTCTime
  , pbcCorrelationId    :: Text
  }

data VerifiedPayment = VerifiedPayment
  { vpAttempt          :: PaymentAttemptReference
  , vpCheckout         :: CheckoutReference
  , vpProvider         :: PaymentProvider
  , vpEnvironment      :: CheckoutEnvironment
  , vpMerchantRef      :: Text
  , vpResourceType     :: Text
  , vpProviderResource :: Text
  , vpProviderResourcePath :: Maybe Text
  , vpOrderReference   :: Text
  , vpAmountMinor      :: Int64
  , vpCurrency         :: Text
  , vpEvidence         :: Text
  , vpOccurredAt       :: UTCTime
  , vpCorrelationId    :: Text
  }

resolveCheckoutEnvironment :: Maybe String -> Either Text CheckoutEnvironment
resolveCheckoutEnvironment rawEnvironment =
  case fmap (T.toLower . T.strip . T.pack) rawEnvironment of
    Nothing -> Right CheckoutSandbox
    Just value
      | value `elem` ["sandbox", "test"] -> Right CheckoutSandbox
      | value `elem` ["production", "prod", "live"] -> Right CheckoutProduction
      | otherwise -> Left "COMMERCE_CHECKOUT_ENV must be sandbox or production"

checkoutEnvironmentText :: CheckoutEnvironment -> Text
checkoutEnvironmentText CheckoutSandbox = "sandbox"
checkoutEnvironmentText CheckoutProduction = "production"

paymentProviderText :: PaymentProvider -> Text
paymentProviderText provider = case provider of
  ProviderDatafast -> "datafast"
  ProviderPayPal -> "paypal"
  ProviderStripe -> "stripe"
  ProviderBankTransfer -> "bank_transfer"
  ProviderCash -> "cash"
  ProviderPos -> "pos"
  ProviderCardano -> "cardano"

paymentOperationText :: PaymentOperation -> Text
paymentOperationText operation = case operation of
  OperationCreate -> "create"
  OperationAuthorize -> "authorize"
  OperationCapture -> "capture"
  OperationManualVerify -> "manual_verify"

paymentAttemptStageText :: PaymentAttemptStage -> Text
paymentAttemptStageText stage = case stage of
  AttemptCreated -> "created"
  AttemptRequiresCustomerAction -> "requires_customer_action"
  AttemptProcessing -> "processing"
  AttemptSucceeded -> "succeeded"
  AttemptFailed -> "failed"
  AttemptRequiresReview -> "requires_review"

createCheckout :: CheckoutCreation -> SqlPersistT IO CheckoutReference
createCheckout creation@CheckoutCreation{..} =
  createCheckoutWithLines creation
    [ CheckoutLineCreation
        { clProductType = ccProductType
        , clProductId = ccProductId
        , clProductVersion = ccProductVersion
        , clDescription = ccDescription
        , clQuantity = 1
        , clUnitAmountMinor = ccAmountMinor
        , clSnapshot = ccSnapshot
        }
    ]

createCheckoutWithLines
  :: CheckoutCreation
  -> [CheckoutLineCreation]
  -> SqlPersistT IO CheckoutReference
createCheckoutWithLines CheckoutCreation{..} checkoutLines = do
  when (null checkoutLines) $
    fail "Canonical checkout requires at least one immutable line item"
  when (any invalidLine checkoutLines) $
    fail "Canonical checkout line quantity and unit amount must be positive"
  when (lineTotal /= ccAmountMinor) $
    fail "Canonical checkout line totals do not match the checkout total"
  checkoutId <- liftIO (toText <$> nextRandom)
  rawExecute
    "INSERT INTO commerce_checkout_session (\
    \ id, domain_type, domain_order_id, status, environment, currency,\
    \ subtotal_minor, total_minor, customer_email, lookup_token_hash,\
    \ idempotency_key, expires_at, created_at, updated_at\
    \) VALUES (?::uuid, ?, ?, 'awaiting_payment', ?, ?, ?, ?, ?, ?, ?, ?, NOW(), NOW())"
    [ PersistText checkoutId
    , PersistText ccDomainType
    , PersistText ccDomainOrderId
    , PersistText (checkoutEnvironmentText ccEnvironment)
    , PersistText (normalizeCurrency ccCurrency)
    , PersistInt64 ccAmountMinor
    , PersistInt64 ccAmountMinor
    , PersistText (T.toLower (T.strip ccCustomerEmail))
    , PersistText ccLookupTokenHash
    , PersistText ccIdempotencyKey
    , PersistUTCTime ccExpiresAt
    ]
  mapM_ (insertCheckoutLine checkoutId) (zip [1 :: Int64 ..] checkoutLines)
  insertAudit
    checkoutId
    "checkout_created"
    Nothing
    (Just "awaiting_payment")
    "system"
    ccCorrelationId
    ccSnapshot
  pure (CheckoutReference checkoutId)
  where
    invalidLine CheckoutLineCreation{..} =
      clQuantity <= 0 || clUnitAmountMinor <= 0
    lineTotal = sum
      [ fromIntegral clQuantity * clUnitAmountMinor
      | CheckoutLineCreation{..} <- checkoutLines
      ]

insertCheckoutLine
  :: Text
  -> (Int64, CheckoutLineCreation)
  -> SqlPersistT IO ()
insertCheckoutLine checkoutId (lineNumber, CheckoutLineCreation{..}) = do
  let subtotal = fromIntegral clQuantity * clUnitAmountMinor
  rawExecute
    "INSERT INTO commerce_checkout_line_item (\
    \ checkout_id, line_number, product_type, product_id, product_version,\
    \ description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot\
    \) VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?::jsonb)"
    [ PersistText checkoutId
    , PersistInt64 lineNumber
    , PersistText clProductType
    , PersistText clProductId
    , PersistText clProductVersion
    , PersistText clDescription
    , PersistInt64 (fromIntegral clQuantity)
    , PersistInt64 clUnitAmountMinor
    , PersistInt64 subtotal
    , PersistInt64 subtotal
    , PersistText (jsonText clSnapshot)
    ]

loadCheckoutEnvironment
  :: CheckoutReference
  -> SqlPersistT IO (Either Text CheckoutEnvironment)
loadCheckoutEnvironment checkout = do
  rows <- rawSql
    "SELECT environment FROM commerce_checkout_session WHERE id = ?::uuid"
    [PersistText (checkoutReferenceId checkout)]
  pure $ case rows of
    [Single environment]
      | (environment :: Text) == "sandbox" -> Right CheckoutSandbox
      | environment == "production" -> Right CheckoutProduction
      | otherwise -> Left "Canonical checkout environment is invalid"
    [] -> Left "Canonical checkout was not found"
    _ -> Left "Canonical checkout environment is invalid or ambiguous"

beginPaymentAttempt
  :: PaymentAttemptCreation
  -> SqlPersistT IO (Either Text PaymentAttemptReference)
beginPaymentAttempt PaymentAttemptCreation{..}
  | pacAmountMinor <= 0 = pure (Left "Payment attempt amount must be positive")
  | otherwise = do
      attemptId <- liftIO (toText <$> nextRandom)
      created <- rawSql
        "INSERT INTO commerce_payment_attempt (\
        \ id, checkout_id, provider, environment, operation, status, amount_minor,\
        \ currency, merchant_account_ref, idempotency_key, created_at, updated_at\
        \) VALUES (?::uuid, ?::uuid, ?, ?, ?, 'created', ?, ?, ?, ?, ?, ?)\
        \ ON CONFLICT (provider, merchant_account_ref, operation, idempotency_key)\
        \ DO NOTHING RETURNING id::text"
        [ PersistText attemptId
        , PersistText (checkoutReferenceId pacCheckout)
        , PersistText (paymentProviderText pacProvider)
        , PersistText (checkoutEnvironmentText pacEnvironment)
        , PersistText (paymentOperationText pacOperation)
        , PersistInt64 pacAmountMinor
        , PersistText (normalizeCurrency pacCurrency)
        , PersistText pacMerchantRef
        , PersistText pacIdempotencyKey
        , PersistUTCTime pacCreatedAt
        , PersistUTCTime pacCreatedAt
        ]
      case created of
        [Single newAttemptId] -> do
          insertAudit
            (checkoutReferenceId pacCheckout)
            "payment_attempt_created"
            Nothing
            Nothing
            (paymentProviderText pacProvider)
            pacCorrelationId
            (attemptMetadata newAttemptId pacProvider pacOperation)
          pure (Right (PaymentAttemptReference newAttemptId))
        [] -> do
          existing <- rawSql
            "SELECT id::text FROM commerce_payment_attempt\
            \ WHERE checkout_id = ?::uuid AND provider = ? AND environment = ?\
            \ AND operation = ? AND amount_minor = ? AND currency = ?\
            \ AND merchant_account_ref = ? AND idempotency_key = ?"
            [ PersistText (checkoutReferenceId pacCheckout)
            , PersistText (paymentProviderText pacProvider)
            , PersistText (checkoutEnvironmentText pacEnvironment)
            , PersistText (paymentOperationText pacOperation)
            , PersistInt64 pacAmountMinor
            , PersistText (normalizeCurrency pacCurrency)
            , PersistText pacMerchantRef
            , PersistText pacIdempotencyKey
            ]
          case existing of
            [Single existingAttemptId] ->
              pure (Right (PaymentAttemptReference existingAttemptId))
            _ -> pure (Left "Payment idempotency key conflicts with another immutable attempt")
        _ -> pure (Left "Payment attempt idempotency lookup was ambiguous")

bindProviderResource
  :: ProviderBindingCreation
  -> SqlPersistT IO (Either Text ())
bindProviderResource ProviderBindingCreation{..}
  | not (validProviderReference pbcProviderResource) =
      pure (Left "Provider resource reference is invalid")
  | T.null (T.strip pbcOrderReference) =
      pure (Left "Provider merchant order reference is required")
  | pbcStage `notElem` [AttemptRequiresCustomerAction, AttemptProcessing] =
      pure (Left "Provider resource binding requires a pending payment stage")
  | otherwise = do
      bindingId <- liftIO (toText <$> nextRandom)
      created <- (rawSql
        "INSERT INTO commerce_provider_binding (\
        \ id, payment_attempt_id, provider, environment, merchant_account_ref,\
        \ resource_type, provider_resource_id, provider_resource_path, merchant_reference,\
        \ amount_minor, currency, created_at\
        \) VALUES (?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\
        \ ON CONFLICT (provider, environment, merchant_account_ref, resource_type, provider_resource_id)\
        \ DO NOTHING RETURNING id::text"
        [ PersistText bindingId
        , PersistText (paymentAttemptReferenceId pbcAttempt)
        , PersistText (paymentProviderText pbcProvider)
        , PersistText (checkoutEnvironmentText pbcEnvironment)
        , PersistText pbcMerchantRef
        , PersistText pbcResourceType
        , PersistText pbcProviderResource
        , maybe PersistNull PersistText pbcResourcePath
        , PersistText pbcOrderReference
        , PersistInt64 pbcAmountMinor
        , PersistText (normalizeCurrency pbcCurrency)
        , PersistUTCTime pbcOccurredAt
        ] :: SqlPersistT IO [Single Text])
      bindingMatches <- case created of
        [_] -> pure True
        [] -> do
          rows <- rawSql
            "SELECT EXISTS (\
            \ SELECT 1 FROM commerce_provider_binding binding\
            \ WHERE binding.payment_attempt_id = ?::uuid AND binding.provider = ?\
            \ AND binding.environment = ? AND binding.merchant_account_ref = ?\
            \ AND binding.resource_type = ? AND binding.provider_resource_id = ?\
            \ AND binding.provider_resource_path IS NOT DISTINCT FROM ?\
            \ AND binding.merchant_reference = ? AND binding.amount_minor = ?\
            \ AND binding.currency = ?)"
            [ PersistText (paymentAttemptReferenceId pbcAttempt)
            , PersistText (paymentProviderText pbcProvider)
            , PersistText (checkoutEnvironmentText pbcEnvironment)
            , PersistText pbcMerchantRef
            , PersistText pbcResourceType
            , PersistText pbcProviderResource
            , maybe PersistNull PersistText pbcResourcePath
            , PersistText pbcOrderReference
            , PersistInt64 pbcAmountMinor
            , PersistText (normalizeCurrency pbcCurrency)
            ]
          pure (rows == [Single True])
        _ -> pure False
      if not bindingMatches
        then pure (Left "Provider resource conflicts with an immutable binding")
        else do
          rawExecute
            "UPDATE commerce_payment_attempt SET status = ?, updated_at = ?\
            \ WHERE id = ?::uuid AND checkout_id = ?::uuid"
            [ PersistText (paymentAttemptStageText pbcStage)
            , PersistUTCTime pbcOccurredAt
            , PersistText (paymentAttemptReferenceId pbcAttempt)
            , PersistText (checkoutReferenceId pbcCheckout)
            ]
          let checkoutStatus = case pbcStage of
                AttemptProcessing -> "processing"
                _ -> "awaiting_payment"
          rawExecute
            "UPDATE commerce_checkout_session SET status = ?, updated_at = ?\
            \ WHERE id = ?::uuid AND status NOT IN (\
            \ 'paid','partially_refunded','refunded','disputed','chargeback')"
            [ PersistText checkoutStatus
            , PersistUTCTime pbcOccurredAt
            , PersistText (checkoutReferenceId pbcCheckout)
            ]
          when (not (null created)) $
            insertAudit
              (checkoutReferenceId pbcCheckout)
              "provider_resource_bound"
              Nothing
              (Just checkoutStatus)
              (paymentProviderText pbcProvider)
              pbcCorrelationId
              (bindingMetadata pbcResourceType pbcProviderResource)
          pure (Right ())

recordPaymentFailure
  :: CheckoutReference
  -> PaymentAttemptReference
  -> PaymentProvider
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
recordPaymentFailure checkout attempt provider failureCode correlationId occurredAt = do
  rawExecute
    "UPDATE commerce_payment_attempt\
    \ SET status = 'failed', failure_code = ?, failure_summary = ?, updated_at = ?\
    \ WHERE id = ?::uuid AND checkout_id = ?::uuid AND status <> 'succeeded'"
    [ PersistText (T.take 120 failureCode)
    , PersistText "Provider operation failed; inspect redacted operational logs."
    , PersistUTCTime occurredAt
    , PersistText (paymentAttemptReferenceId attempt)
    , PersistText (checkoutReferenceId checkout)
    ]
  rawExecute
    "UPDATE commerce_checkout_session SET status = 'failed', updated_at = ?\
    \ WHERE id = ?::uuid AND status IN ('awaiting_payment','processing')"
    [PersistUTCTime occurredAt, PersistText (checkoutReferenceId checkout)]
  insertAudit
    (checkoutReferenceId checkout)
    "payment_attempt_failed"
    Nothing
    (Just "failed")
    (paymentProviderText provider)
    correlationId
    (objectText "failure_code" (T.take 120 failureCode))

recordPaymentProcessing
  :: CheckoutReference
  -> PaymentAttemptReference
  -> PaymentProvider
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
recordPaymentProcessing checkout attempt provider correlationId occurredAt = do
  rawExecute
    "UPDATE commerce_payment_attempt SET status = 'processing', updated_at = ?\
    \ WHERE id = ?::uuid AND checkout_id = ?::uuid AND status <> 'succeeded'"
    [ PersistUTCTime occurredAt
    , PersistText (paymentAttemptReferenceId attempt)
    , PersistText (checkoutReferenceId checkout)
    ]
  rawExecute
    "UPDATE commerce_checkout_session SET status = 'processing', updated_at = ?\
    \ WHERE id = ?::uuid AND status IN ('awaiting_payment','processing','failed')"
    [PersistUTCTime occurredAt, PersistText (checkoutReferenceId checkout)]
  insertAudit
    (checkoutReferenceId checkout)
    "payment_processing"
    Nothing
    (Just "processing")
    (paymentProviderText provider)
    correlationId
    (objectText "attempt_id" (paymentAttemptReferenceId attempt))

recordManualPaymentSelection
  :: CheckoutReference
  -> PaymentAttemptReference
  -> PaymentProvider
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
recordManualPaymentSelection checkout attempt provider correlationId occurredAt = do
  rawExecute
    "UPDATE commerce_payment_attempt SET status = 'requires_review', updated_at = ?\
    \ WHERE id = ?::uuid AND checkout_id = ?::uuid AND status <> 'succeeded'"
    [ PersistUTCTime occurredAt
    , PersistText (paymentAttemptReferenceId attempt)
    , PersistText (checkoutReferenceId checkout)
    ]
  rawExecute
    "INSERT INTO commerce_manual_payment_evidence (\
    \ checkout_id, payment_attempt_id, status\
    \) VALUES (?::uuid, ?::uuid, 'awaiting_evidence')\
    \ ON CONFLICT (payment_attempt_id) DO NOTHING"
    [ PersistText (checkoutReferenceId checkout)
    , PersistText (paymentAttemptReferenceId attempt)
    ]
  rawExecute
    "UPDATE commerce_checkout_session SET status = 'awaiting_payment', updated_at = ?\
    \ WHERE id = ?::uuid AND status IN ('awaiting_payment','failed')"
    [PersistUTCTime occurredAt, PersistText (checkoutReferenceId checkout)]
  insertAudit
    (checkoutReferenceId checkout)
    "manual_payment_selected"
    Nothing
    (Just "awaiting_payment")
    "customer"
    correlationId
    (objectText "provider" (paymentProviderText provider))

recordVerifiedPayment :: VerifiedPayment -> SqlPersistT IO (Either Text Bool)
recordVerifiedPayment payment@VerifiedPayment{..}
  | vpEvidence `notElem` ["server_to_server", "signature_verified_webhook"] =
      pure (Left "Payment evidence is not authoritative")
  | vpAmountMinor <= 0 = pure (Left "Verified amount must be positive")
  | otherwise = do
      paymentStates <- (rawSql
        "SELECT checkout.status, attempt.status FROM commerce_checkout_session checkout\
        \ JOIN commerce_payment_attempt attempt\
        \   ON attempt.checkout_id = checkout.id\
        \ JOIN commerce_provider_binding binding\
        \   ON binding.payment_attempt_id = attempt.id\
        \ WHERE checkout.id = ?::uuid AND attempt.id = ?::uuid\
        \ AND checkout.domain_order_id = ?\
        \ AND checkout.environment = ? AND attempt.environment = checkout.environment\
        \ AND checkout.total_minor = ? AND attempt.amount_minor = checkout.total_minor\
        \ AND checkout.currency = ? AND attempt.currency = checkout.currency\
        \ AND attempt.provider = ? AND attempt.merchant_account_ref = ?\
        \ AND binding.provider = attempt.provider\
        \ AND binding.environment = attempt.environment\
        \ AND binding.merchant_account_ref = attempt.merchant_account_ref\
        \ AND binding.resource_type = ? AND binding.provider_resource_id = ?\
        \ AND binding.provider_resource_path IS NOT DISTINCT FROM ?\
        \ AND binding.merchant_reference = checkout.domain_order_id\
        \ AND binding.amount_minor = checkout.total_minor\
        \ AND binding.currency = checkout.currency\
        \ AND checkout.status IN ('awaiting_payment','processing','failed','paid')\
        \ FOR UPDATE OF checkout, attempt"
        [ PersistText (checkoutReferenceId vpCheckout)
        , PersistText (paymentAttemptReferenceId vpAttempt)
        , PersistText vpOrderReference
        , PersistText (checkoutEnvironmentText vpEnvironment)
        , PersistInt64 vpAmountMinor
        , PersistText (normalizeCurrency vpCurrency)
        , PersistText (paymentProviderText vpProvider)
        , PersistText vpMerchantRef
        , PersistText vpResourceType
        , PersistText vpProviderResource
        , maybe PersistNull PersistText vpProviderResourcePath
        ] :: SqlPersistT IO [(Single Text, Single Text)])
      case paymentStates of
        [(Single currentStatus, Single attemptStatus)] ->
          completeVerifiedPayment payment currentStatus attemptStatus
        [] -> pure (Left "Verified payment does not match the stored checkout and provider binding")
        _ -> pure (Left "Verified payment matched multiple immutable bindings")

-- | Finalize a manual settlement only after an independently reviewed evidence
-- row and its immutable provider binding agree with the checkout. Customer
-- submission alone can never reach this function's success path.
recordApprovedManualPayment :: VerifiedPayment -> SqlPersistT IO (Either Text Bool)
recordApprovedManualPayment payment@VerifiedPayment{..}
  | Left validationError <- validateApprovedManualPayment payment =
      pure (Left validationError)
  | otherwise = do
      paymentStates <- (rawSql
        "SELECT checkout.status, attempt.status FROM commerce_checkout_session checkout\
        \ JOIN commerce_payment_attempt attempt ON attempt.checkout_id = checkout.id\
        \ JOIN commerce_provider_binding binding ON binding.payment_attempt_id = attempt.id\
        \ JOIN commerce_manual_payment_evidence evidence\
        \   ON evidence.checkout_id = checkout.id\
        \  AND evidence.payment_attempt_id = attempt.id\
        \ WHERE checkout.id = ?::uuid AND attempt.id = ?::uuid\
        \ AND checkout.domain_order_id = ?\
        \ AND checkout.environment = ? AND attempt.environment = checkout.environment\
        \ AND checkout.total_minor = ? AND attempt.amount_minor = checkout.total_minor\
        \ AND checkout.currency = ? AND attempt.currency = checkout.currency\
        \ AND attempt.provider = ? AND attempt.operation = 'manual_verify'\
        \ AND attempt.merchant_account_ref = ?\
        \ AND binding.provider = attempt.provider\
        \ AND binding.environment = attempt.environment\
        \ AND binding.merchant_account_ref = attempt.merchant_account_ref\
        \ AND binding.resource_type = 'manual_evidence'\
        \ AND binding.provider_resource_id = evidence.id::text\
        \ AND binding.provider_resource_id = ?\
        \ AND binding.provider_resource_path IS NOT DISTINCT FROM ?\
        \ AND binding.merchant_reference = checkout.domain_order_id\
        \ AND binding.amount_minor = checkout.total_minor\
        \ AND binding.currency = checkout.currency\
        \ AND evidence.status = 'approved'\
        \ AND evidence.submitted_amount_minor = checkout.total_minor\
        \ AND evidence.currency = checkout.currency\
        \ AND evidence.submitted_by IS NOT NULL\
        \ AND evidence.reviewed_by IS NOT NULL\
        \ AND evidence.reviewed_by <> evidence.submitted_by\
        \ AND evidence.reviewed_at IS NOT NULL\
        \ AND checkout.status IN ('awaiting_payment','processing','failed','paid')\
        \ FOR UPDATE OF checkout, attempt, evidence"
        [ PersistText (checkoutReferenceId vpCheckout)
        , PersistText (paymentAttemptReferenceId vpAttempt)
        , PersistText vpOrderReference
        , PersistText (checkoutEnvironmentText vpEnvironment)
        , PersistInt64 vpAmountMinor
        , PersistText (normalizeCurrency vpCurrency)
        , PersistText (paymentProviderText vpProvider)
        , PersistText vpMerchantRef
        , PersistText vpProviderResource
        , maybe PersistNull PersistText vpProviderResourcePath
        ] :: SqlPersistT IO [(Single Text, Single Text)])
      case paymentStates of
        [(Single currentStatus, Single attemptStatus)] ->
          completeVerifiedPayment payment currentStatus attemptStatus
        [] -> pure (Left "Approved manual payment does not match immutable evidence and binding")
        _ -> pure (Left "Approved manual payment matched multiple immutable evidence rows")

validateApprovedManualPayment :: VerifiedPayment -> Either Text ()
validateApprovedManualPayment VerifiedPayment{..}
  | vpEvidence /= "staff_verified_manual" =
      Left "Manual payment evidence is not staff verified"
  | vpProvider `notElem` [ProviderBankTransfer, ProviderCash, ProviderPos] =
      Left "Manual payment provider is invalid"
  | vpResourceType /= "manual_evidence" =
      Left "Manual payment binding type is invalid"
  | vpAmountMinor <= 0 = Left "Verified amount must be positive"
  | otherwise = Right ()

completeVerifiedPayment
  :: VerifiedPayment
  -> Text
  -> Text
  -> SqlPersistT IO (Either Text Bool)
completeVerifiedPayment payment@VerifiedPayment{..} currentStatus attemptStatus = do
  ledgerStatus <- existingLedgerStatus vpAttempt
  receiptValid <- existingReceiptIsCompatible vpCheckout vpAmountMinor vpCurrency
  case ledgerStatus of
    Just status | status /= "posted" ->
      pure (Left "Existing payment ledger transaction is not posted")
    _ | currentStatus == "paid" && attemptStatus /= "succeeded" ->
      pure (Left "Checkout is already paid by another payment attempt")
    _ | not receiptValid ->
      pure (Left "Existing payment receipt conflicts with verified payment")
    _ -> do
      rawExecute
        "UPDATE commerce_payment_attempt\
        \ SET status = 'succeeded', failure_code = NULL, failure_summary = NULL, updated_at = ?\
        \ WHERE id = ?::uuid"
        [PersistUTCTime vpOccurredAt, PersistText (paymentAttemptReferenceId vpAttempt)]
      rawExecute
        "UPDATE commerce_checkout_session\
        \ SET status = 'paid', paid_minor = total_minor, paid_at = COALESCE(paid_at, ?), updated_at = ?\
        \ WHERE id = ?::uuid"
        [ PersistUTCTime vpOccurredAt
        , PersistUTCTime vpOccurredAt
        , PersistText (checkoutReferenceId vpCheckout)
        ]
      when (ledgerStatus == Nothing) (postPaymentLedger payment)
      ensurePaymentReceipt payment
      let newlyPaid = currentStatus /= "paid"
      when newlyPaid $
        insertAudit
          (checkoutReferenceId vpCheckout)
          "payment_verified"
          (Just currentStatus)
          (Just "paid")
          (paymentProviderText vpProvider)
          vpCorrelationId
          (objectText "evidence" vpEvidence)
      pure (Right newlyPaid)

recordReconciliationException
  :: PaymentProvider
  -> CheckoutEnvironment
  -> Text
  -> Text
  -> Text
  -> Text
  -> Int64
  -> Maybe Int64
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
recordReconciliationException provider environment merchantRef exceptionType internalRef providerRef expectedAmount actualAmount currency detectedAt = do
  exceptionId <- liftIO (toText <$> nextRandom)
  rawExecute
    "INSERT INTO commerce_reconciliation_exception (\
    \ id, provider, environment, merchant_account_ref, exception_type,\
    \ internal_reference, provider_reference, expected_amount_minor,\
    \ actual_amount_minor, currency, status, detected_at\
    \) VALUES (?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'open', ?)\
    \ ON CONFLICT DO NOTHING"
    [ PersistText exceptionId
    , PersistText (paymentProviderText provider)
    , PersistText (checkoutEnvironmentText environment)
    , PersistText merchantRef
    , PersistText (T.take 120 exceptionType)
    , PersistText internalRef
    , PersistText providerRef
    , PersistInt64 expectedAmount
    , maybe PersistNull PersistInt64 actualAmount
    , PersistText (normalizeCurrency currency)
    , PersistUTCTime detectedAt
    ]

providerEnabledForEnvironment
  :: CheckoutEnvironment
  -> PaymentProvider
  -> SqlPersistT IO Bool
providerEnabledForEnvironment CheckoutSandbox _ = pure True
providerEnabledForEnvironment CheckoutProduction provider = do
  rows <- rawSql
    "SELECT enabled FROM revenue_feature_flag\
    \ WHERE flag_key = ? AND environment = 'production'"
    [PersistText ("checkout." <> paymentProviderText provider)]
  pure (rows == [Single True])

domainEnabledForEnvironment
  :: CheckoutEnvironment
  -> Text
  -> SqlPersistT IO Bool
domainEnabledForEnvironment CheckoutSandbox _ = pure True
domainEnabledForEnvironment CheckoutProduction domainKey = do
  rows <- rawSql
    "SELECT enabled FROM revenue_feature_flag\
    \ WHERE flag_key = ? AND environment = 'production'"
    [PersistText ("commerce." <> domainKey)]
  pure (rows == [Single True])

capabilityEnabledForEnvironment
  :: CheckoutEnvironment
  -> Text
  -> SqlPersistT IO Bool
capabilityEnabledForEnvironment CheckoutSandbox _ = pure True
capabilityEnabledForEnvironment CheckoutProduction flagKey = do
  rows <- rawSql
    "SELECT enabled FROM revenue_feature_flag\
    \ WHERE flag_key = ? AND environment = 'production'"
    [PersistText flagKey]
  pure (rows == [Single True])

postPaymentLedger :: VerifiedPayment -> SqlPersistT IO ()
postPaymentLedger VerifiedPayment{..} = do
  ledgerId <- liftIO (toText <$> nextRandom)
  domainRows <- (rawSql
    "SELECT domain_type FROM commerce_checkout_session WHERE id = ?::uuid"
    [PersistText (checkoutReferenceId vpCheckout)]
    :: SqlPersistT IO [Single Text])
  let eventTicketCheckout = domainRows == [Single "event_ticket_order"]
  rawExecute
    "INSERT INTO commerce_ledger_transaction (\
    \ id, transaction_type, source_type, source_id, status, effective_at,\
    \ correlation_id, created_by\
    \) VALUES (?::uuid, 'payment_capture', 'payment_attempt', ?, 'draft', ?, ?, ?)"
    [ PersistText ledgerId
    , PersistText (paymentAttemptReferenceId vpAttempt)
    , PersistUTCTime vpOccurredAt
    , PersistText vpCorrelationId
    , PersistText (paymentProviderText vpProvider <> "_server_verification")
    ]
  rawExecute
    "INSERT INTO commerce_ledger_entry (\
    \ transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo\
    \) SELECT ?::uuid, ?, domain_type, domain_order_id, currency, total_minor,\
    \ 'Verified provider payment' FROM commerce_checkout_session WHERE id = ?::uuid"
    [ PersistText ledgerId
    , PersistText ("cash." <> paymentProviderText vpProvider)
    , PersistText (checkoutReferenceId vpCheckout)
    ]
  if eventTicketCheckout
    then rawExecute
      "INSERT INTO commerce_ledger_entry (\
      \ transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo\
      \) SELECT ?::uuid, 'revenue.event_ticket_order', checkout.domain_type,\
      \ checkout.domain_order_id, checkout.currency, -runtime.platform_fee_minor,\
      \ 'Ticket platform fees recognized on verified payment'\
      \ FROM commerce_checkout_session checkout\
      \ JOIN event_ticket_checkout_runtime runtime ON runtime.checkout_id = checkout.id\
      \ WHERE checkout.id = ?::uuid"
      [PersistText ledgerId, PersistText (checkoutReferenceId vpCheckout)]
    else rawExecute
      "INSERT INTO commerce_ledger_entry (\
      \ transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo\
      \) SELECT ?::uuid, 'revenue.' || domain_type, domain_type, domain_order_id,\
      \ currency,\
      \ -CASE WHEN domain_type = 'marketplace_rental' THEN COALESCE((\
      \   SELECT rental_charge_usd_cents FROM marketplace_rental_order_runtime\
      \   WHERE order_id::text = domain_order_id\
      \ ), total_minor) ELSE total_minor END,\
      \ 'Revenue recognized on verified payment'\
      \ FROM commerce_checkout_session WHERE id = ?::uuid"
      [PersistText ledgerId, PersistText (checkoutReferenceId vpCheckout)]
  rawExecute
    "INSERT INTO commerce_ledger_entry (\
    \ transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo\
    \) SELECT ?::uuid, 'liability.marketplace_rental_deposit', checkout.domain_type,\
    \ checkout.domain_order_id, checkout.currency, -runtime.security_deposit_usd_cents,\
    \ 'Refundable rental deposit collected'\
    \ FROM commerce_checkout_session checkout\
    \ JOIN marketplace_rental_order_runtime runtime\
    \   ON runtime.checkout_id = checkout.id\
    \ WHERE checkout.id = ?::uuid AND runtime.security_deposit_usd_cents > 0"
    [PersistText ledgerId, PersistText (checkoutReferenceId vpCheckout)]
  when eventTicketCheckout $ do
    rawExecute
      "INSERT INTO commerce_ledger_entry (\
      \ transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo\
      \) SELECT ?::uuid, 'liability.event_organizer_payable', checkout.domain_type,\
      \ checkout.domain_order_id, checkout.currency, -runtime.organizer_payable_minor,\
      \ 'Organizer proceeds accrued; not yet settled'\
      \ FROM commerce_checkout_session checkout\
      \ JOIN event_ticket_checkout_runtime runtime\
      \   ON runtime.checkout_id = checkout.id\
      \ WHERE checkout.id = ?::uuid AND runtime.organizer_payable_minor > 0"
      [PersistText ledgerId, PersistText (checkoutReferenceId vpCheckout)]
    rawExecute
      "INSERT INTO commerce_ledger_entry (\
      \ transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo\
      \) SELECT ?::uuid, 'liability.sales_tax', checkout.domain_type,\
      \ checkout.domain_order_id, checkout.currency, -runtime.tax_minor,\
      \ 'Ticket tax collected pending configured invoicing and remittance'\
      \ FROM commerce_checkout_session checkout\
      \ JOIN event_ticket_checkout_runtime runtime\
      \   ON runtime.checkout_id = checkout.id\
      \ WHERE checkout.id = ?::uuid AND runtime.tax_minor > 0"
      [PersistText ledgerId, PersistText (checkoutReferenceId vpCheckout)]
  rawExecute
    "UPDATE commerce_ledger_transaction SET status = 'posted' WHERE id = ?::uuid"
    [PersistText ledgerId]

ensurePaymentReceipt :: VerifiedPayment -> SqlPersistT IO ()
ensurePaymentReceipt VerifiedPayment{..} = do
  receiptId <- liftIO (toText <$> nextRandom)
  rawExecute
    "INSERT INTO commerce_receipt (\
    \ id, checkout_id, receipt_number, kind, adapter, external_reference,\
    \ amount_minor, currency, issued_at\
    \) VALUES (?::uuid, ?::uuid, ?, 'payment_receipt', ?, ?, ?, ?, ?)\
    \ ON CONFLICT DO NOTHING"
    [ PersistText receiptId
    , PersistText (checkoutReferenceId vpCheckout)
    , PersistText (receiptNumber vpCheckout)
    , PersistText (paymentProviderText vpProvider)
    , PersistText vpProviderResource
    , PersistInt64 vpAmountMinor
    , PersistText (normalizeCurrency vpCurrency)
    , PersistUTCTime vpOccurredAt
    ]

existingLedgerStatus
  :: PaymentAttemptReference
  -> SqlPersistT IO (Maybe Text)
existingLedgerStatus attempt = do
  rows <- rawSql
    "SELECT status FROM commerce_ledger_transaction\
    \ WHERE source_type = 'payment_attempt' AND source_id = ?\
    \ AND transaction_type = 'payment_capture'"
    [PersistText (paymentAttemptReferenceId attempt)]
  case rows of
    [] -> pure Nothing
    [Single status] -> pure (Just status)
    _ -> pure (Just "ambiguous")

existingReceiptIsCompatible
  :: CheckoutReference
  -> Int64
  -> Text
  -> SqlPersistT IO Bool
existingReceiptIsCompatible checkout amount currency = do
  rows <- rawSql
    "SELECT amount_minor = ? AND currency = ?\
    \ FROM commerce_receipt\
    \ WHERE checkout_id = ?::uuid AND kind = 'payment_receipt' AND voided_at IS NULL"
    [ PersistInt64 amount
    , PersistText (normalizeCurrency currency)
    , PersistText (checkoutReferenceId checkout)
    ]
  pure (null rows || rows == [Single True])

insertAudit
  :: Text
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Text
  -> Text
  -> Value
  -> SqlPersistT IO ()
insertAudit checkoutId eventType fromStatus toStatus actorType correlationId metadata =
  rawExecute
    "INSERT INTO commerce_checkout_audit_event (\
    \ checkout_id, event_type, from_status, to_status, actor_type, correlation_id, metadata\
    \) VALUES (?::uuid, ?, ?, ?, ?, ?, ?::jsonb)"
    [ PersistText checkoutId
    , PersistText eventType
    , maybe PersistNull PersistText fromStatus
    , maybe PersistNull PersistText toStatus
    , PersistText actorType
    , PersistText correlationId
    , PersistText (jsonText metadata)
    ]

attemptMetadata :: Text -> PaymentProvider -> PaymentOperation -> Value
attemptMetadata attemptId provider operation =
  objectPairs
    [ ("attempt_id", attemptId)
    , ("provider", paymentProviderText provider)
    , ("operation", paymentOperationText operation)
    ]

bindingMetadata :: Text -> Text -> Value
bindingMetadata resourceType resourceId =
  objectPairs
    [ ("resource_type", resourceType)
    , ("provider_resource_id", resourceId)
    ]

objectText :: Text -> Text -> Value
objectText key value = objectPairs [(key, value)]

objectPairs :: [(Text, Text)] -> Value
objectPairs pairs = object
  [ AesonKey.fromText key .= value
  | (key, value) <- pairs
  ]

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode

receiptNumber :: CheckoutReference -> Text
receiptNumber = ("TDF-RCPT-" <>) . T.toUpper . T.filter (/= '-') . checkoutReferenceId

normalizeCurrency :: Text -> Text
normalizeCurrency = T.toUpper . T.strip

validProviderReference :: Text -> Bool
validProviderReference value =
  let normalized = T.strip value
  in not (T.null normalized)
      && T.length normalized <= 256
      && T.all (\character ->
        character >= '!' && character <= '~' && character /= '/' && character /= '\\') normalized
