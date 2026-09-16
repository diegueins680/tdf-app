{-# LANGUAGE OverloadedStrings #-}

-- | Transactional compatibility bridge between the existing checkout runtime
-- and the canonical payment-intent lifecycle.
--
-- Existing product handlers still own their order/fulfilment rules, but every
-- viable online payment attempt (and staff-verified bank transfer) is bound to
-- one canonical intent before a provider can be contacted. Providers outside
-- the selected Ecuador online portfolio retain their historical attempt
-- records without being presented as canonical online methods.
module TDF.Commerce.PaymentRuntimeStore
  ( beginPaymentAttempt
  , beginPaymentAttemptForMethod
  , canonicalPaymentMethodForProvider
  , operationCapabilities
  , providerOperationCapabilities
  , validateCanonicalRoute
  , productFlowForDomain
  ) where

import           Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql
  ( Single(..), SqlPersistT, rawSql, transactionSave, transactionUndo )

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentIntentStore as Intent
import           TDF.Commerce.ProviderCapabilities
  ( ProviderActivation(..), PaymentCapability(..), PaymentMethod(..), PaymentRoute(..)
  , PaymentRouteRequest(..), ProductFlow(..), paymentMethodText, routePayments )
import           TDF.Commerce.ProviderCapabilityStore (loadProviderActivations)

beginPaymentAttempt
  :: Checkout.PaymentAttemptCreation
  -> SqlPersistT IO (Either Text Checkout.PaymentAttemptReference)
beginPaymentAttempt creation =
  case canonicalPaymentMethodForProvider (Checkout.pacProvider creation) of
    Nothing -> Checkout.beginPaymentAttempt creation
    Just paymentMethod -> beginPaymentAttemptForMethod paymentMethod creation

-- | Start an attempt when the checkout surface selected an explicit method.
-- This is mandatory for multi-method providers such as PlaceToPay: inferring a
-- method from the provider would let a client bypass method-level capability
-- verification (for example by requesting a bank redirect through a card-only
-- account).  The existing provider-only entry point remains for legacy,
-- unambiguous integrations.
beginPaymentAttemptForMethod
  :: PaymentMethod
  -> Checkout.PaymentAttemptCreation
  -> SqlPersistT IO (Either Text Checkout.PaymentAttemptReference)
beginPaymentAttemptForMethod paymentMethod creation = do
  routeResult <- validateCanonicalRoute creation paymentMethod
  case routeResult of
    Left problem -> pure (Left problem)
    Right () -> beginCanonical
  where
    beginCanonical = do
      -- Returning Left alone would commit an orphan intent or attempt under
      -- Persistent's outer transaction, so this bridge owns a save boundary.
      transactionSave
      -- Serialize same-checkout requests, including different idempotency keys.
      -- Replaying an existing attempt preserves pre-v2 intent/reference bindings.
      _ <- rawSql "SELECT id::text FROM commerce_checkout_session WHERE id = ?::uuid FOR UPDATE"
        [PersistText (Checkout.checkoutReferenceId (Checkout.pacCheckout creation))]
        :: SqlPersistT IO [Single Text]
      replay <- loadExistingAttempt paymentMethod creation
      case replay of
        Left problem -> transactionUndo >> pure (Left problem)
        Right (Just attempt) -> transactionSave >> pure (Right attempt)
        Right Nothing -> createCanonical
    createCanonical = do
      keyResult <- continuationIntentKey paymentMethod creation
      case keyResult of
        Left problem -> transactionUndo >> pure (Left problem)
        Right key -> createWithKey key
    createWithKey key = do
      intentResult <- Intent.createPaymentIntent Intent.PaymentIntentCreation
        { Intent.picCheckout = Checkout.pacCheckout creation
        , Intent.picEnvironment = Checkout.pacEnvironment creation
        , Intent.picProvider = Checkout.pacProvider creation
        , Intent.picPaymentMethod = paymentMethod
        , Intent.picCaptureMethod = Intent.CaptureAutomatic
        , Intent.picAmountMinor = Checkout.pacAmountMinor creation
        , Intent.picCurrency = Checkout.pacCurrency creation
        , Intent.picIdempotencyKey = key
        , Intent.picOccurredAt = Checkout.pacCreatedAt creation
        , Intent.picCorrelationId = Checkout.pacCorrelationId creation
        }
      case intentResult of
        Left problem -> transactionUndo >> pure (Left problem)
        Right intent -> do
          attemptResult <- Checkout.beginPaymentAttempt creation
          case attemptResult of
            Left problem -> transactionUndo >> pure (Left problem)
            Right attempt -> do
              bindingResult <- Intent.bindPaymentAttemptToIntent
                (Intent.pisReference intent)
                attempt
              case bindingResult of
                Left problem -> transactionUndo >> pure (Left problem)
                Right () -> transactionSave >> pure (Right attempt)

-- An idempotency key is never permission to attach a new attempt to an old
-- active/terminal intent. Only the exact persisted attempt may be replayed.
loadExistingAttempt
  :: PaymentMethod
  -> Checkout.PaymentAttemptCreation
  -> SqlPersistT IO (Either Text (Maybe Checkout.PaymentAttemptReference))
loadExistingAttempt method creation = do
  rows <- rawSql
    "SELECT attempt.id::text, (attempt.checkout_id = ?::uuid\
    \ AND attempt.environment = ? AND attempt.amount_minor = ? AND attempt.currency = ?\
    \ AND intent.checkout_id = attempt.checkout_id AND intent.provider = attempt.provider\
    \ AND intent.amount_minor = attempt.amount_minor AND intent.currency = attempt.currency\
    \ AND intent.payment_method = ? AND intent.capture_method = 'automatic') IS TRUE\
    \ FROM commerce_payment_attempt attempt\
    \ LEFT JOIN commerce_payment_intent intent ON intent.id = attempt.payment_intent_id\
    \ WHERE attempt.provider = ? AND attempt.merchant_account_ref = ?\
    \ AND attempt.operation = ? AND attempt.idempotency_key = ?"
    [ PersistText (Checkout.checkoutReferenceId (Checkout.pacCheckout creation))
    , PersistText (Checkout.checkoutEnvironmentText (Checkout.pacEnvironment creation))
    , PersistInt64 (Checkout.pacAmountMinor creation)
    , PersistText (T.toUpper (T.strip (Checkout.pacCurrency creation)))
    , PersistText (paymentMethodText method)
    , PersistText (Checkout.paymentProviderText (Checkout.pacProvider creation))
    , PersistText (Checkout.pacMerchantRef creation)
    , PersistText (Checkout.paymentOperationText (Checkout.pacOperation creation))
    , PersistText (Checkout.pacIdempotencyKey creation)
    ] :: SqlPersistT IO [(Single Text, Single Bool)]
  pure $ case rows of
    [] -> Right Nothing
    [(Single attemptId, Single True)] ->
      Right (Just (Checkout.PaymentAttemptReference attemptId))
    _ -> Left "Payment idempotency key conflicts with an immutable or unbound attempt"

-- Legacy Datafast/PayPal capture endpoints use a separate attempt to continue
-- the original create. Preserve that lifecycle instead of starting a second
-- intent. A different key cannot start a second capture of that same intent.
continuationIntentKey
  :: PaymentMethod
  -> Checkout.PaymentAttemptCreation
  -> SqlPersistT IO (Either Text Text)
continuationIntentKey method creation
  | Checkout.pacOperation creation /= Checkout.OperationCapture =
      pure (Right (canonicalIntentKey creation))
  | otherwise = do
      rows <- rawSql
        "SELECT intent.idempotency_key, EXISTS (SELECT 1 FROM commerce_payment_attempt prior\
        \ WHERE prior.payment_intent_id=intent.id AND prior.operation='capture')\
        \ FROM commerce_payment_intent intent\
        \ WHERE intent.checkout_id=?::uuid AND intent.provider=? AND intent.payment_method=?\
        \ AND intent.status IN ('requires_payment_method','requires_customer_action','processing',\
        \ 'authorized','partially_captured')\
        \ AND EXISTS (SELECT 1 FROM commerce_payment_attempt origin\
        \ JOIN commerce_provider_binding binding ON binding.payment_attempt_id=origin.id\
        \ WHERE origin.payment_intent_id=intent.id AND origin.operation IN ('create','authorize')\
        \ AND origin.environment=? AND origin.merchant_account_ref=?)"
        [ PersistText (Checkout.checkoutReferenceId (Checkout.pacCheckout creation))
        , PersistText (Checkout.paymentProviderText (Checkout.pacProvider creation))
        , PersistText (paymentMethodText method)
        , PersistText (Checkout.checkoutEnvironmentText (Checkout.pacEnvironment creation))
        , PersistText (Checkout.pacMerchantRef creation)
        ] :: SqlPersistT IO [(Single Text, Single Bool)]
      pure $ case rows of
        [] -> Right (canonicalIntentKey creation)
        [(Single key, Single False)] -> Right key
        _ -> Left "Capture already has an attempt; reconcile or replay its exact idempotency key"

validateCanonicalRoute
  :: Checkout.PaymentAttemptCreation
  -> PaymentMethod
  -> SqlPersistT IO (Either Text ())
validateCanonicalRoute creation paymentMethod = do
  domainRows <- rawSql
    "SELECT domain_type FROM commerce_checkout_session WHERE id = ?::uuid"
    [PersistText (Checkout.checkoutReferenceId (Checkout.pacCheckout creation))]
    :: SqlPersistT IO [Single Text]
  case domainRows of
    [Single domainType] -> case productFlowForDomain domainType of
      Nothing -> pure (Left "Checkout domain is not eligible for canonical payment routing")
      Just flow -> do
        activations <- loadProviderActivations (Checkout.pacEnvironment creation)
        let matchingAccounts = filter
              (\activation -> paMerchantRef activation == Just (Checkout.pacMerchantRef creation)
                && not (T.null (T.strip (Checkout.pacMerchantRef creation)))) activations
            request = PaymentRouteRequest
              { prEnvironment = Checkout.pacEnvironment creation
              , prBuyerCountry = "ZZ"
              , prCurrency = Checkout.pacCurrency creation
              , prAmountMinor = Checkout.pacAmountMinor creation
              , prMethod = paymentMethod
              , prFlow = flow
              , prRequiredCapabilities = providerOperationCapabilities
                  (Checkout.pacProvider creation) flow (Checkout.pacOperation creation)
              }
            selected = Checkout.pacProvider creation
        pure $ if any ((== selected) . routeProvider) (routePayments matchingAccounts request)
          then Right ()
          else Left "Payment provider is not verified for this merchant, method, operation and environment"
    [] -> pure (Left "Canonical checkout was not found")
    _ -> pure (Left "Canonical checkout lookup was ambiguous")

-- Datafast's historical "capture" attempt records the result of server-side
-- verification of an automatic debit. It does not call a separate capture API.
-- Keep the persisted operation/idempotency identity, while requiring the actual
-- one-time and verification capabilities. Other providers retain capture gates.
providerOperationCapabilities
  :: Checkout.PaymentProvider
  -> ProductFlow
  -> Checkout.PaymentOperation
  -> [PaymentCapability]
providerOperationCapabilities provider flow operation =
  case (provider, operation) of
    (Checkout.ProviderDatafast, Checkout.OperationCreate) ->
      CapabilityServerVerification : operationCapabilities flow operation
    (Checkout.ProviderDatafast, Checkout.OperationAuthorize) ->
      CapabilityServerVerification : operationCapabilities flow operation
    (Checkout.ProviderPayPal, Checkout.OperationCreate) ->
      CapabilityCapture : operationCapabilities flow operation
    (Checkout.ProviderPayPal, Checkout.OperationAuthorize) ->
      CapabilityCapture : operationCapabilities flow operation
    (Checkout.ProviderDatafast, Checkout.OperationCapture) ->
      [CapabilityOneTime, CapabilityServerVerification]
        <> filter (/= CapabilityCapture) (operationCapabilities flow operation)
    _ -> operationCapabilities flow operation

operationCapabilities
  :: ProductFlow
  -> Checkout.PaymentOperation
  -> [PaymentCapability]
operationCapabilities flow operation = base <> marketplace
  where
    base = case operation of
      Checkout.OperationCreate -> [CapabilityOneTime]
      Checkout.OperationAuthorize -> [CapabilityAuthorize]
      Checkout.OperationCapture -> [CapabilityCapture]
      Checkout.OperationManualVerify -> [CapabilityOneTime]
    marketplace
      | flow == FlowMarketplace =
          [ CapabilityConnectedAccounts
          , CapabilitySplitSettlement
          , CapabilitySellerPayouts
          ]
      | otherwise = []

productFlowForDomain :: Text -> Maybe ProductFlow
productFlowForDomain domainType = case domainType of
  "mixing_mastering" -> Just FlowProfessionalService
  "service_booking" -> Just FlowBooking
  "course_registration" -> Just FlowCourse
  "event_ticket_order" -> Just FlowEventTicket
  "domo_event_quote" -> Just FlowBooking
  "marketplace_sale" -> Just FlowMarketplace
  "marketplace_rental" -> Just FlowMarketplace
  "merch_order" -> Just FlowMerchandise
  _ -> Nothing

-- | Provider-only inference is intentionally limited to unambiguous methods.
-- PlaceToPay can represent cards, bank redirects, DeUna and links, so its
-- future runtime must supply the method explicitly instead of guessing.
canonicalPaymentMethodForProvider
  :: Checkout.PaymentProvider
  -> Maybe PaymentMethod
canonicalPaymentMethodForProvider provider = case provider of
  Checkout.ProviderDatafast -> Just MethodCard
  Checkout.ProviderPayPal -> Just MethodPayPalWallet
  Checkout.ProviderPayPhone -> Just MethodPayPhoneWallet
  Checkout.ProviderBankTransfer -> Just MethodManualBankTransfer
  Checkout.ProviderPlaceToPay -> Nothing
  Checkout.ProviderStripe -> Nothing
  Checkout.ProviderCash -> Nothing
  Checkout.ProviderPos -> Nothing
  Checkout.ProviderCardano -> Nothing

canonicalIntentKey :: Checkout.PaymentAttemptCreation -> Text
canonicalIntentKey creation =
  "payment-intent:v2:" <> T.pack (show digest)
  where
    digest = hash (BL.toStrict (A.encode
      ( Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
      , Checkout.checkoutEnvironmentText (Checkout.pacEnvironment creation)
      , Checkout.paymentProviderText (Checkout.pacProvider creation)
      , Checkout.paymentOperationText (Checkout.pacOperation creation)
      , Checkout.pacIdempotencyKey creation
      ))) :: Digest SHA256
