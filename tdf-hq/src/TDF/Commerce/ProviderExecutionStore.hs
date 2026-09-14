{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Durable state around remote provider calls. The store intentionally keeps
-- only non-sensitive identifiers and an encrypted hosted-checkout URL; raw
-- request/response payloads and credentials never cross this boundary.
module TDF.Commerce.ProviderExecutionStore
  ( CheckoutExecution(..)
  , BoundProviderPayment(..)
  , ProviderOperationReference(..)
  , ProviderOperationRecord(..)
  , ProviderOperationPreparation(..)
  , ProviderOperationClaim(..)
  , loadAuthorizedCheckout
  , loadReadyMerchantAccount
  , loadAttemptProviderReference
  , prepareProviderOperation
  , claimProviderOperation
  , recordCreateResult
  , recordAmbiguousOperation
  , loadAuthorizedCreateOperation
  , loadAuthorizedCreateReplay
  , providerCreateRequestFingerprint
  , loadBoundProviderPayment
  , recordReconciledCreateResult
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray.Encoding as BAE
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql
  ( Single(..), SqlPersistT, rawExecute, rawSql, transactionSave, transactionUndo )

import qualified TDF.Commerce.CheckoutStore as Checkout
import           TDF.Commerce.ProviderAdapter
  ( AdapterOperation(..), AdapterResult(..), AdapterResultState(..) )
import           TDF.Commerce.ProviderCapabilities
  ( PaymentMethod, paymentMethodText, ProviderOutcomeCertainty(..) )

data CheckoutExecution = CheckoutExecution
  { ceCheckout       :: Checkout.CheckoutReference
  , ceDomainOrderId  :: Text
  , ceDomainType     :: Text
  , ceEnvironment    :: Checkout.CheckoutEnvironment
  , ceCurrency       :: Text
  , ceSubtotalMinor  :: Int64
  , ceDiscountMinor  :: Int64
  , ceTaxMinor       :: Int64
  , ceFeeMinor       :: Int64
  , ceTotalMinor     :: Int64
  , ceCustomerEmail  :: Text
  } deriving (Eq, Show)

-- | Immutable identifiers and expected money loaded from the provider
-- binding. This is the only input accepted by authoritative reconciliation;
-- callback-supplied amounts, currencies, and order references are ignored.
data BoundProviderPayment = BoundProviderPayment
  { bppCheckout            :: Checkout.CheckoutReference
  , bppDomainOrderId       :: Text
  , bppAttempt             :: Checkout.PaymentAttemptReference
  , bppPaymentIntentId     :: Text
  , bppProvider            :: Checkout.PaymentProvider
  , bppEnvironment         :: Checkout.CheckoutEnvironment
  , bppMerchantRef         :: Text
  , bppResourceType        :: Text
  , bppProviderResourceId  :: Text
  , bppProviderResourcePath :: Maybe Text
  , bppProviderReference   :: Text
  , bppAmountMinor         :: Int64
  , bppCurrency            :: Text
  } deriving (Eq, Show)

newtype ProviderOperationReference = ProviderOperationReference
  { providerOperationReferenceId :: Text
  } deriving (Eq, Show)

data ProviderOperationRecord = ProviderOperationRecord
  { porReference          :: ProviderOperationReference
  , porAttempt            :: Checkout.PaymentAttemptReference
  , porProvider           :: Checkout.PaymentProvider
  , porStatus             :: Text
  , porOutcomeCertainty   :: ProviderOutcomeCertainty
  , porProviderResourceId :: Maybe Text
  , porRedirectUrl        :: Maybe Text
  } deriving (Eq, Show)

data ProviderOperationPreparation = ProviderOperationPreparation
  { popAttempt         :: Checkout.PaymentAttemptReference
  , popProvider        :: Checkout.PaymentProvider
  , popEnvironment     :: Checkout.CheckoutEnvironment
  , popMerchantRef     :: Text
  , popProviderReference :: Text
  , popOperation       :: AdapterOperation
  , popIdempotencyKey  :: Text
  , popRequestSha256   :: Text
  , popOccurredAt      :: UTCTime
  }

data ProviderOperationClaim
  = ProviderOperationClaimed ProviderOperationReference
  | ProviderOperationKnown ProviderOperationRecord
  | ProviderOperationBusy
  deriving (Eq, Show)

loadAuthorizedCheckout
  :: Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text CheckoutExecution)
loadAuthorizedCheckout checkoutId lookupTokenHash now = do
  rows <- rawSql
    "SELECT domain_order_id, domain_type, environment, currency, subtotal_minor,\
    \ discount_minor, tax_minor, fee_minor, total_minor, customer_email\
    \ FROM commerce_checkout_session\
    \ WHERE id = ?::uuid AND lookup_token_hash = ?\
    \ AND status IN ('awaiting_payment','processing','failed') AND expires_at > ?"
    [ PersistText checkoutId
    , PersistText lookupTokenHash
    , PersistUTCTime now
    ] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single Text, Single Int64,
        Single Int64, Single Int64, Single Int64, Single Int64, Single Text)]
  pure $ case rows of
    [(Single orderId, Single domainType, Single environment, Single currency,
      Single subtotalMinor, Single discountMinor, Single taxMinor,
      Single feeMinor, Single totalMinor, Single customerEmail)] -> do
        parsedEnvironment <- checkoutEnvironmentFromText environment
        Right CheckoutExecution
          { ceCheckout = Checkout.CheckoutReference checkoutId
          , ceDomainOrderId = orderId
          , ceDomainType = domainType
          , ceEnvironment = parsedEnvironment
          , ceCurrency = currency
          , ceSubtotalMinor = subtotalMinor
          , ceDiscountMinor = discountMinor
          , ceTaxMinor = taxMinor
          , ceFeeMinor = feeMinor
          , ceTotalMinor = totalMinor
          , ceCustomerEmail = customerEmail
          }
    [] -> Left "Checkout was not found, is expired, or cannot accept payment"
    _ -> Left "Checkout authorization lookup was ambiguous"

loadReadyMerchantAccount
  :: Checkout.CheckoutEnvironment
  -> Checkout.PaymentProvider
  -> SqlPersistT IO (Either Text Text)
loadReadyMerchantAccount environment provider = do
  rows <- rawSql
    "SELECT account.merchant_account_ref FROM commerce_provider_account account\
    \ LEFT JOIN revenue_feature_flag flag\
    \   ON flag.flag_key = account.feature_flag_key\
    \  AND flag.environment = account.environment\
    \ WHERE account.environment = ? AND account.provider = ?\
    \ AND account.enabled = TRUE AND account.status = 'ready'\
    \ AND account.contract_status = 'approved'\
    \ AND account.credential_status = 'validated'\
    \ AND account.settlement_currency = 'USD'\
    \ AND account.merchant_account_ref IS NOT NULL\
    \ AND (? = 'sandbox' OR COALESCE(flag.enabled, FALSE))"
    [ PersistText (Checkout.checkoutEnvironmentText environment)
    , PersistText (Checkout.paymentProviderText provider)
    , PersistText (Checkout.checkoutEnvironmentText environment)
    ] :: SqlPersistT IO [Single Text]
  pure $ case rows of
    [Single merchantRef]
      | not (T.null (T.strip merchantRef)) -> Right merchantRef
    [] -> Left "Payment provider account is not operationally ready"
    _ -> Left "Payment provider account readiness is ambiguous"

prepareProviderOperation
  :: ProviderOperationPreparation
  -> SqlPersistT IO (Either Text ProviderOperationRecord)
prepareProviderOperation preparation@ProviderOperationPreparation{..}
  | not (validIdempotencyKey popIdempotencyKey) =
      pure (Left "Provider operation idempotency key is invalid")
  | not (validSha256 popRequestSha256) =
      pure (Left "Provider operation request fingerprint is invalid")
  | not (validProviderReference popProviderReference) =
      pure (Left "Provider operation reference is invalid")
  | popProvider `notElem` [Checkout.ProviderPlaceToPay, Checkout.ProviderPayPhone] =
      pure (Left "Provider executor does not support this provider")
  | popOperation /= AdapterCreate =
      pure (Left "Only create operations can be prepared by this checkout endpoint")
  | otherwise = do
      operationId <- liftIO (toText <$> nextRandom)
      inserted <- rawSql
        "INSERT INTO commerce_provider_operation (\
        \ id, payment_attempt_id, provider, environment, merchant_account_ref,\
        \ provider_reference,\
        \ operation, idempotency_key, request_sha256, status, outcome_certainty,\
        \ created_at, updated_at\
        \) SELECT ?::uuid, attempt.id, ?, ?, ?, ?, ?, ?, ?, 'prepared',\
        \ 'not_contacted', ?, ? FROM commerce_payment_attempt attempt\
        \ WHERE attempt.id = ?::uuid AND attempt.provider = ?\
        \ AND attempt.environment = ? AND attempt.merchant_account_ref = ?\
        \ ON CONFLICT DO NOTHING RETURNING id::text"
        [ PersistText operationId
        , PersistText (Checkout.paymentProviderText popProvider)
        , PersistText (Checkout.checkoutEnvironmentText popEnvironment)
        , PersistText popMerchantRef
        , PersistText popProviderReference
        , PersistText (adapterOperationText popOperation)
        , PersistText popIdempotencyKey
        , PersistText popRequestSha256
        , PersistUTCTime popOccurredAt
        , PersistUTCTime popOccurredAt
        , PersistText (Checkout.paymentAttemptReferenceId popAttempt)
        , PersistText (Checkout.paymentProviderText popProvider)
        , PersistText (Checkout.checkoutEnvironmentText popEnvironment)
        , PersistText popMerchantRef
        ] :: SqlPersistT IO [Single Text]
      case inserted of
        [Single _] -> loadOperationByPreparation preparation
        [] -> loadOperationByPreparation preparation
        _ -> pure (Left "Provider operation insert was ambiguous")

-- Preserve the provider reference of an existing operation across upgrades.
-- This is called only after the canonical attempt's immutable fields match.
loadAttemptProviderReference
  :: Checkout.PaymentAttemptReference
  -> SqlPersistT IO (Either Text (Maybe Text))
loadAttemptProviderReference attempt = do
  rows <- rawSql
    "SELECT provider_reference FROM commerce_provider_operation\
    \ WHERE payment_attempt_id = ?::uuid AND operation = 'create'"
    [PersistText (Checkout.paymentAttemptReferenceId attempt)]
    :: SqlPersistT IO [Single Text]
  pure $ case rows of
    [] -> Right Nothing
    [Single reference] -> Right (Just reference)
    _ -> Left "Provider operation reference lookup was ambiguous"

claimProviderOperation
  :: ProviderOperationReference
  -> UTCTime
  -> Text
  -> SqlPersistT IO (Either Text ProviderOperationClaim)
claimProviderOperation operationRef now encryptionKey
  | not (validEncryptionKey encryptionKey) =
      pure (Left "Provider operation encryption key is invalid")
  | otherwise = do
      claimed <- rawSql
        "UPDATE commerce_provider_operation SET status = 'in_flight',\
        \ outcome_certainty = 'ambiguous', started_at = ?, updated_at = ?\
        \ WHERE id = ?::uuid AND status = 'prepared' RETURNING id::text"
        [ PersistUTCTime now
        , PersistUTCTime now
        , PersistText (providerOperationReferenceId operationRef)
        ] :: SqlPersistT IO [Single Text]
      case claimed of
        [_] -> pure (Right (ProviderOperationClaimed operationRef))
        [] -> do
          loaded <- loadOperation operationRef encryptionKey
          pure $ case loaded of
            Left problem -> Left problem
            Right record
              | porStatus record == "in_flight" -> Right ProviderOperationBusy
              | otherwise -> Right (ProviderOperationKnown record)
        _ -> pure (Left "Provider operation claim was ambiguous")

recordCreateResult
  :: ProviderOperationReference
  -> CheckoutExecution
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Text
  -> Text
  -> Text
  -> AdapterResult
  -> Text
  -> UTCTime
  -> SqlPersistT IO (Either Text ProviderOperationRecord)
recordCreateResult operationRef checkout attempt provider merchantRef providerReference
    encryptionKey result correlationId now
  | adapterResultState result `notElem`
      [AdapterRequiresCustomerAction, AdapterPending] =
      pure (Left "Provider create returned an unsupported terminal state")
  | not (validEncryptionKey encryptionKey) =
      pure (Left "Provider operation encryption key is invalid")
  | otherwise = do
      transactionSave
      let stage = case adapterResultState result of
            AdapterRequiresCustomerAction -> Checkout.AttemptRequiresCustomerAction
            _ -> Checkout.AttemptProcessing
          operationStatus = case adapterResultState result of
            AdapterRequiresCustomerAction -> "requires_customer_action"
            _ -> "processing"
          resourceType = case provider of
            Checkout.ProviderPlaceToPay -> "session"
            Checkout.ProviderPayPhone -> "sale"
            _ -> "payment"
          resourcePath = case provider of
            Checkout.ProviderPlaceToPay -> Just ("/api/session/" <> adapterResultExternalId result)
            Checkout.ProviderPayPhone -> Just ("/api/Sale/" <> adapterResultExternalId result)
            _ -> Nothing
      binding <- Checkout.bindProviderResource Checkout.ProviderBindingCreation
        { Checkout.pbcAttempt = attempt
        , Checkout.pbcCheckout = ceCheckout checkout
        , Checkout.pbcProvider = provider
        , Checkout.pbcEnvironment = ceEnvironment checkout
        , Checkout.pbcMerchantRef = merchantRef
        , Checkout.pbcResourceType = resourceType
        , Checkout.pbcProviderResource = adapterResultExternalId result
        , Checkout.pbcResourcePath = resourcePath
        , Checkout.pbcOrderReference = providerReference
        , Checkout.pbcAmountMinor = ceTotalMinor checkout
        , Checkout.pbcCurrency = ceCurrency checkout
        , Checkout.pbcStage = stage
        , Checkout.pbcOccurredAt = now
        , Checkout.pbcCorrelationId = correlationId
        }
      case binding of
        Left problem -> transactionUndo >> pure (Left problem)
        Right () -> do
          updated <- rawSql
            "UPDATE commerce_provider_operation SET status = ?, outcome_certainty = ?,\
            \ provider_resource_id = COALESCE(provider_resource_id, ?),\
            \ redirect_url_ciphertext = CASE WHEN ?::text IS NULL THEN NULL\
            \   ELSE pgp_sym_encrypt(?::text, ?, 'cipher-algo=aes256,compress-algo=1') END,\
            \ completed_at = ?, updated_at = ?, last_error_code = NULL\
            \ WHERE id = ?::uuid AND status = 'in_flight'\
            \ AND (provider_resource_id IS NULL OR provider_resource_id = ?)\
            \ RETURNING id::text"
            [ PersistText operationStatus
            , PersistText (outcomeCertaintyText (adapterResultCertainty result))
            , PersistText (adapterResultExternalId result)
            , maybe PersistNull PersistText (adapterResultRedirectUrl result)
            , maybe PersistNull PersistText (adapterResultRedirectUrl result)
            , PersistText encryptionKey
            , PersistUTCTime now
            , PersistUTCTime now
            , PersistText (providerOperationReferenceId operationRef)
            , PersistText (adapterResultExternalId result)
            ] :: SqlPersistT IO [Single Text]
          case updated of
            [_] -> do
              loaded <- loadOperation operationRef encryptionKey
              case loaded of
                Left problem -> transactionUndo >> pure (Left problem)
                Right completed -> transactionSave >> pure (Right completed)
            [] -> transactionUndo >>
              pure (Left "Provider operation changed concurrently")
            _ -> transactionUndo >>
              pure (Left "Provider operation completion was ambiguous")

recordAmbiguousOperation
  :: ProviderOperationReference
  -> CheckoutExecution
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
recordAmbiguousOperation operationRef checkout attempt provider errorCode correlationId now = do
  rawExecute
    "UPDATE commerce_provider_operation SET status = 'ambiguous',\
    \ outcome_certainty = 'ambiguous', last_error_code = ?, completed_at = ?, updated_at = ?\
    \ WHERE id = ?::uuid AND status = 'in_flight'"
    [ PersistText (safeErrorCode errorCode)
    , PersistUTCTime now
    , PersistUTCTime now
    , PersistText (providerOperationReferenceId operationRef)
    ]
  rawExecute
    "UPDATE commerce_payment_attempt SET status = 'requires_review',\
    \ failure_code = ?, failure_summary = 'Provider outcome is ambiguous; reconcile before retry or fallback.',\
    \ updated_at = ? WHERE id = ?::uuid AND status <> 'succeeded'"
    [ PersistText (safeErrorCode errorCode)
    , PersistUTCTime now
    , PersistText (Checkout.paymentAttemptReferenceId attempt)
    ]
  rawExecute
    "UPDATE commerce_checkout_session SET status = 'processing', updated_at = ?\
    \ WHERE id = ?::uuid AND status IN ('awaiting_payment','processing','failed')"
    [ PersistUTCTime now
    , PersistText (Checkout.checkoutReferenceId (ceCheckout checkout))
    ]
  rawExecute
    "INSERT INTO commerce_checkout_audit_event (\
    \ checkout_id, event_type, from_status, to_status, actor_type, correlation_id, metadata\
    \) VALUES (?::uuid, 'provider_outcome_ambiguous', NULL, 'processing', 'system', ?,\
    \ jsonb_build_object('provider', ?, 'operation_id', ?))"
    [ PersistText (Checkout.checkoutReferenceId (ceCheckout checkout))
    , PersistText correlationId
    , PersistText (Checkout.paymentProviderText provider)
    , PersistText (providerOperationReferenceId operationRef)
    ]

loadAuthorizedCreateOperation
  :: Text
  -> Text
  -> Text
  -> Text
  -> SqlPersistT IO (Either Text ProviderOperationRecord)
loadAuthorizedCreateOperation checkoutId attemptId lookupTokenHash encryptionKey
  | not (validEncryptionKey encryptionKey) =
      pure (Left "Provider operation encryption key is invalid")
  | otherwise = do
      refs <- rawSql
        "SELECT operation.id::text FROM commerce_provider_operation operation\
        \ JOIN commerce_payment_attempt attempt ON attempt.id = operation.payment_attempt_id\
        \ JOIN commerce_checkout_session checkout ON checkout.id = attempt.checkout_id\
        \ WHERE checkout.id = ?::uuid AND attempt.id = ?::uuid\
        \ AND checkout.lookup_token_hash = ? AND operation.operation = 'create'"
        [ PersistText checkoutId
        , PersistText attemptId
        , PersistText lookupTokenHash
        ] :: SqlPersistT IO [Single Text]
      case refs of
        [Single operationId] -> loadOperation
          (ProviderOperationReference operationId) encryptionKey
        [] -> pure (Left "Provider operation was not found")
        _ -> pure (Left "Provider operation lookup was ambiguous")

-- | Authenticate and validate an exact request replay without checking whether
-- a NEW charge may be started. Account suspension and checkout expiry must not
-- hide an already contacted provider's durable outcome. This function never
-- claims an operation, mutates payment state, or contacts a provider.
loadAuthorizedCreateReplay
  :: Text -> Text -> Checkout.PaymentProvider -> PaymentMethod -> Text
  -> Maybe Text -> Maybe Text -> Text
  -> SqlPersistT IO (Either Text (Maybe ProviderOperationRecord))
loadAuthorizedCreateReplay checkoutId lookupTokenHash provider method idempotencyKey
    buyerPhone buyerCountryCode encryptionKey = do
  rows <- rawSql
    "SELECT operation.id::text, operation.provider_reference, operation.request_sha256,\
    \ checkout.total_minor, checkout.currency, COALESCE(\
    \ operation.provider = attempt.provider AND operation.environment = attempt.environment\
    \ AND operation.merchant_account_ref = attempt.merchant_account_ref\
    \ AND operation.idempotency_key = attempt.idempotency_key\
    \ AND attempt.operation = 'create' AND attempt.environment = checkout.environment\
    \ AND attempt.amount_minor = checkout.total_minor AND attempt.currency = checkout.currency\
    \ AND intent.checkout_id = checkout.id\
    \ AND intent.provider = attempt.provider AND intent.amount_minor = attempt.amount_minor\
    \ AND intent.currency = attempt.currency AND intent.payment_method = ?\
    \ AND intent.capture_method = 'automatic', FALSE)\
    \ FROM commerce_provider_operation operation\
    \ JOIN commerce_payment_attempt attempt ON attempt.id = operation.payment_attempt_id\
    \ JOIN commerce_checkout_session checkout ON checkout.id = attempt.checkout_id\
    \ LEFT JOIN commerce_payment_intent intent ON intent.id = attempt.payment_intent_id\
    \ WHERE checkout.id = ?::uuid AND checkout.lookup_token_hash = ?\
    \ AND operation.provider = ? AND operation.operation = 'create'\
    \ AND operation.idempotency_key = ?"
    [ PersistText (paymentMethodText method), PersistText checkoutId
    , PersistText lookupTokenHash, PersistText (Checkout.paymentProviderText provider)
    , PersistText idempotencyKey
    ] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single Int64, Single Text, Single Bool)]
  case rows of
    [] -> pure (Right Nothing)
    [(Single operationId, Single reference, Single fingerprint, Single amount,
        Single currency, Single bindingsMatch)]
      | not bindingsMatch || fingerprint /= providerCreateRequestFingerprint
          (Checkout.CheckoutReference checkoutId) provider method reference amount currency
          buyerPhone buyerCountryCode ->
          pure (Left "Provider operation replay conflicts with immutable fields")
      | not (validEncryptionKey encryptionKey) ->
          pure (Left "Provider operation encryption key is invalid")
      | otherwise -> fmap Just <$> loadOperation
          (ProviderOperationReference operationId) encryptionKey
    _ -> pure (Left "Provider operation replay lookup was ambiguous")

-- Keep the original fingerprint byte format for already persisted operations.
-- Both creation and recovery normalize contact fields identically; neither
-- credentials, mutable account configuration, IP nor User-Agent are identity.
providerCreateRequestFingerprint
  :: Checkout.CheckoutReference -> Checkout.PaymentProvider -> PaymentMethod
  -> Text -> Int64 -> Text -> Maybe Text -> Maybe Text -> Text
providerCreateRequestFingerprint checkout provider method reference amount currency phone country =
  TE.decodeUtf8 (BAE.convertToBase BAE.Base16 (hash (TE.encodeUtf8 identity) :: Digest SHA256))
  where
    identity = T.intercalate "|"
      [ Checkout.checkoutReferenceId checkout, Checkout.paymentProviderText provider
      , paymentMethodText method, paymentMethodText method, reference, T.pack (show amount)
      , T.toUpper currency, fromMaybe "" (T.strip <$> phone), fromMaybe "" (T.strip <$> country)
      ]

loadBoundProviderPayment
  :: Checkout.PaymentProvider
  -> Checkout.CheckoutEnvironment
  -> Text
  -> Text
  -> Maybe Text
  -> SqlPersistT IO (Either Text BoundProviderPayment)
loadBoundProviderPayment provider environment merchantRef providerResource
    callbackReference = do
  rows <- rawSql
    "SELECT checkout.id::text, checkout.domain_order_id, attempt.id::text,\
    \ attempt.payment_intent_id::text, binding.resource_type,\
    \ binding.provider_resource_path, binding.merchant_reference,\
    \ binding.amount_minor, binding.currency\
    \ FROM commerce_provider_binding binding\
    \ JOIN commerce_payment_attempt attempt ON attempt.id = binding.payment_attempt_id\
    \ JOIN commerce_payment_intent intent ON intent.id = attempt.payment_intent_id\
    \ JOIN commerce_checkout_session checkout ON checkout.id = attempt.checkout_id\
    \ WHERE binding.provider = ? AND binding.environment = ?\
    \ AND binding.merchant_account_ref = ? AND binding.provider_resource_id = ?\
    \ AND (?::text IS NULL OR binding.merchant_reference = ?)\
    \ AND ((binding.provider = 'placetopay' AND binding.resource_type = 'session')\
    \   OR (binding.provider = 'payphone' AND binding.resource_type = 'sale'))\
    \ AND attempt.provider = binding.provider\
    \ AND attempt.environment = binding.environment\
    \ AND attempt.merchant_account_ref = binding.merchant_account_ref\
    \ AND checkout.environment = binding.environment\
    \ AND attempt.amount_minor = checkout.total_minor\
    \ AND binding.amount_minor = checkout.total_minor\
    \ AND attempt.currency = checkout.currency\
    \ AND binding.currency = checkout.currency\
    \ AND intent.checkout_id = checkout.id\
    \ AND intent.provider = binding.provider\
    \ AND intent.amount_minor = checkout.total_minor\
    \ AND intent.currency = checkout.currency"
    [ PersistText (Checkout.paymentProviderText provider)
    , PersistText (Checkout.checkoutEnvironmentText environment)
    , PersistText merchantRef
    , PersistText providerResource
    , maybe PersistNull PersistText callbackReference
    , maybe PersistNull PersistText callbackReference
    ] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single Text, Single Text,
        Single (Maybe Text), Single Text, Single Int64, Single Text)]
  pure $ case rows of
    [(Single checkoutId, Single domainOrderId, Single attemptId,
      Single intentId, Single resourceType, Single resourcePath,
      Single providerReference, Single amountMinor, Single currency)] ->
        Right BoundProviderPayment
          { bppCheckout = Checkout.CheckoutReference checkoutId
          , bppDomainOrderId = domainOrderId
          , bppAttempt = Checkout.PaymentAttemptReference attemptId
          , bppPaymentIntentId = intentId
          , bppProvider = provider
          , bppEnvironment = environment
          , bppMerchantRef = merchantRef
          , bppResourceType = resourceType
          , bppProviderResourceId = providerResource
          , bppProviderResourcePath = resourcePath
          , bppProviderReference = providerReference
          , bppAmountMinor = amountMinor
          , bppCurrency = currency
          }
    [] -> Left "Provider callback does not match an immutable payment binding"
    _ -> Left "Provider callback matched multiple immutable payment bindings"

recordReconciledCreateResult
  :: BoundProviderPayment
  -> AdapterResult
  -> UTCTime
  -> SqlPersistT IO (Either Text ())
recordReconciledCreateResult payment result now =
  case reconciledOperationState result of
    Left problem -> pure (Left problem)
    Right (nextStatus, certainty) -> do
      current <- rawSql
        "SELECT status, outcome_certainty FROM commerce_provider_operation\
        \ WHERE payment_attempt_id = ?::uuid AND operation = 'create'\
        \ AND provider_resource_id = ? FOR UPDATE"
        [ PersistText (Checkout.paymentAttemptReferenceId (bppAttempt payment))
        , PersistText (bppProviderResourceId payment)
        ] :: SqlPersistT IO [(Single Text, Single Text)]
      case current of
        [] -> pure (Left "Provider create operation was not found for reconciliation")
        [(Single currentStatus, Single currentCertainty)]
          | currentStatus `elem` ["succeeded", "confirmed_no_charge"] ->
              if currentStatus == nextStatus
                  && currentCertainty == outcomeCertaintyText certainty
                then pure (Right ())
                else pure (Left "Provider reconciliation conflicts with a terminal operation")
          | otherwise -> do
              updated <- rawSql
                "UPDATE commerce_provider_operation SET status = ?, outcome_certainty = ?,\
                \ last_error_code = NULL, completed_at = ?, updated_at = ?\
                \ WHERE payment_attempt_id = ?::uuid AND operation = 'create'\
                \ AND provider_resource_id = ? AND status = ? RETURNING id::text"
                [ PersistText nextStatus
                , PersistText (outcomeCertaintyText certainty)
                , PersistUTCTime now
                , PersistUTCTime now
                , PersistText (Checkout.paymentAttemptReferenceId (bppAttempt payment))
                , PersistText (bppProviderResourceId payment)
                , PersistText currentStatus
                ] :: SqlPersistT IO [Single Text]
              pure $ case updated of
                [_] -> Right ()
                [] -> Left "Provider create operation changed concurrently"
                _ -> Left "Provider create operation reconciliation was ambiguous"
        _ -> pure (Left "Provider create operation lookup was ambiguous")

reconciledOperationState
  :: AdapterResult
  -> Either Text (Text, ProviderOutcomeCertainty)
reconciledOperationState result = case adapterResultState result of
  AdapterSucceeded -> Right ("succeeded", ProviderSucceeded)
  AdapterDeclined -> Right ("confirmed_no_charge", ProviderConfirmedNoCharge)
  AdapterCancelled -> Right ("confirmed_no_charge", ProviderConfirmedNoCharge)
  AdapterPending -> Right ("processing", ProviderAmbiguous)
  AdapterRequiresCustomerAction -> Right ("requires_customer_action", ProviderAmbiguous)
  AdapterUnknown -> Right ("ambiguous", ProviderAmbiguous)
  AdapterAuthorized -> Left "Provider query returned an unsupported authorization state"
  AdapterReversed -> Left "Provider query returned an unsupported reversal state"

loadOperationByPreparation
  :: ProviderOperationPreparation
  -> SqlPersistT IO (Either Text ProviderOperationRecord)
loadOperationByPreparation ProviderOperationPreparation{..} = do
  rows <- rawSql
    "SELECT id::text, status, outcome_certainty, provider_resource_id\
    \ FROM commerce_provider_operation WHERE payment_attempt_id = ?::uuid\
    \ AND operation = ? AND idempotency_key = ?\
    \ AND provider = ? AND environment = ? AND merchant_account_ref = ?\
    \ AND provider_reference = ?\
    \ AND request_sha256 = ?"
    [ PersistText (Checkout.paymentAttemptReferenceId popAttempt)
    , PersistText (adapterOperationText popOperation)
    , PersistText popIdempotencyKey
    , PersistText (Checkout.paymentProviderText popProvider)
    , PersistText (Checkout.checkoutEnvironmentText popEnvironment)
    , PersistText popMerchantRef
    , PersistText popProviderReference
    , PersistText popRequestSha256
    ] :: SqlPersistT IO [(Single Text, Single Text, Single Text, Single (Maybe Text))]
  pure $ case rows of
    [(Single operationId, Single status, Single certainty, Single resourceId)] -> do
      parsedCertainty <- outcomeCertaintyFromText certainty
      Right ProviderOperationRecord
        { porReference = ProviderOperationReference operationId
        , porAttempt = popAttempt
        , porProvider = popProvider
        , porStatus = status
        , porOutcomeCertainty = parsedCertainty
        , porProviderResourceId = resourceId
        , porRedirectUrl = Nothing
        }
    [] -> Left "Provider operation idempotency key conflicts with immutable fields"
    _ -> Left "Provider operation idempotency lookup was ambiguous"

loadOperation
  :: ProviderOperationReference
  -> Text
  -> SqlPersistT IO (Either Text ProviderOperationRecord)
loadOperation operationRef encryptionKey = do
  rows <- rawSql
    "SELECT operation.payment_attempt_id::text, operation.provider, operation.status,\
    \ operation.outcome_certainty, operation.provider_resource_id,\
    \ CASE WHEN operation.redirect_url_ciphertext IS NULL THEN NULL\
    \   ELSE pgp_sym_decrypt(operation.redirect_url_ciphertext, ?) END\
    \ FROM commerce_provider_operation operation WHERE operation.id = ?::uuid"
    [ PersistText encryptionKey
    , PersistText (providerOperationReferenceId operationRef)
    ] :: SqlPersistT IO
      [(Single Text, Single Text, Single Text, Single Text,
        Single (Maybe Text), Single (Maybe Text))]
  pure $ case rows of
    [(Single attemptId, Single provider, Single status, Single certainty,
      Single resourceId, Single redirectUrl)] -> do
        parsedProvider <- paymentProviderFromText provider
        parsedCertainty <- outcomeCertaintyFromText certainty
        Right ProviderOperationRecord
          { porReference = operationRef
          , porAttempt = Checkout.PaymentAttemptReference attemptId
          , porProvider = parsedProvider
          , porStatus = status
          , porOutcomeCertainty = parsedCertainty
          , porProviderResourceId = resourceId
          , porRedirectUrl = redirectUrl
          }
    [] -> Left "Provider operation was not found"
    _ -> Left "Provider operation lookup was ambiguous"

adapterOperationText :: AdapterOperation -> Text
adapterOperationText operation = case operation of
  AdapterCreate -> "create"
  AdapterQuery -> "query"
  AdapterCancel -> "cancel"
  AdapterCapture -> "capture"
  AdapterVoid -> "void"
  AdapterRefund -> "refund"
  AdapterSameDayReverse -> "same_day_reverse"

outcomeCertaintyText :: ProviderOutcomeCertainty -> Text
outcomeCertaintyText certainty = case certainty of
  ProviderNotContacted -> "not_contacted"
  ProviderRejectedBeforeCreation -> "rejected_before_creation"
  ProviderConfirmedNoCharge -> "confirmed_no_charge"
  ProviderAmbiguous -> "ambiguous"
  ProviderSucceeded -> "succeeded"

outcomeCertaintyFromText :: Text -> Either Text ProviderOutcomeCertainty
outcomeCertaintyFromText certainty = case certainty of
  "not_contacted" -> Right ProviderNotContacted
  "rejected_before_creation" -> Right ProviderRejectedBeforeCreation
  "confirmed_no_charge" -> Right ProviderConfirmedNoCharge
  "ambiguous" -> Right ProviderAmbiguous
  "succeeded" -> Right ProviderSucceeded
  _ -> Left "Stored provider outcome certainty is invalid"

checkoutEnvironmentFromText :: Text -> Either Text Checkout.CheckoutEnvironment
checkoutEnvironmentFromText environment = case environment of
  "sandbox" -> Right Checkout.CheckoutSandbox
  "production" -> Right Checkout.CheckoutProduction
  _ -> Left "Stored checkout environment is invalid"

paymentProviderFromText :: Text -> Either Text Checkout.PaymentProvider
paymentProviderFromText provider = case provider of
  "placetopay" -> Right Checkout.ProviderPlaceToPay
  "payphone" -> Right Checkout.ProviderPayPhone
  _ -> Left "Stored provider operation provider is invalid"

validIdempotencyKey :: Text -> Bool
validIdempotencyKey value =
  T.length value >= 16 && T.length value <= 128
    && T.all (\character -> character >= '!' && character <= '~') value

validSha256 :: Text -> Bool
validSha256 value =
  T.length value == 64
    && T.all (\character -> character `elem` (['0'..'9'] <> ['a'..'f'])) value

validProviderReference :: Text -> Bool
validProviderReference value =
  not (T.null value)
    && T.length value <= 128
    && T.all (\character ->
      (character >= '0' && character <= '9')
        || (character >= 'A' && character <= 'Z')
        || (character >= 'a' && character <= 'z')
        || character `elem` ("._-" :: String)) value

validEncryptionKey :: Text -> Bool
validEncryptionKey value =
  let normalized = T.strip value
  in T.length normalized >= 32 && T.length normalized <= 256
      && T.all (\character -> character >= '!' && character <= '~') normalized

safeErrorCode :: Text -> Text
safeErrorCode = T.take 120 . T.filter (\character ->
  (character >= '0' && character <= '9')
    || (character >= 'A' && character <= 'Z')
    || (character >= 'a' && character <= 'z')
    || character `elem` ("-_.:" :: String))
