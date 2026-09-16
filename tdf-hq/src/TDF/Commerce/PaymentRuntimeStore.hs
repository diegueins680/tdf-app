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
  , canonicalPaymentMethodForProvider
  , operationCapabilities
  , providerOperationCapabilities
  , productFlowForDomain
  , recordProviderPaymentFailure
  , providerConfirmsNoCharge
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Either (isRight)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql
  ( Single(..), SqlPersistT, rawExecute, rawSql, transactionSave, transactionUndo )

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentIntentStore as Intent
import           TDF.Commerce.ProviderCapabilities
  ( PaymentCapability(..), PaymentMethod(..), PaymentRoute(..)
  , PaymentRouteRequest(..), ProductFlow(..), routePayments )
import           TDF.Commerce.ProviderCapabilityStore (loadProviderActivations)
import           TDF.Commerce.StateMachine (PaymentEvent(..), PaymentLifecycle(..), PaymentState(..))

beginPaymentAttempt
  :: Checkout.PaymentAttemptCreation
  -> SqlPersistT IO (Either Text Checkout.PaymentAttemptReference)
beginPaymentAttempt creation =
  case canonicalPaymentMethodForProvider (Checkout.pacProvider creation) of
    Nothing -> Checkout.beginPaymentAttempt creation
    Just paymentMethod -> do
      routeResult <- validateCanonicalRoute creation paymentMethod
      case routeResult of
        Left problem -> pure (Left problem)
        Right () -> beginCanonical paymentMethod
  where
    beginCanonical paymentMethod = do
      -- Returning Left alone would commit an orphan intent or attempt under
      -- Persistent's outer transaction, so this bridge owns a save boundary.
      transactionSave
      intentResult <- Intent.createPaymentIntent Intent.PaymentIntentCreation
        { Intent.picCheckout = Checkout.pacCheckout creation
        , Intent.picEnvironment = Checkout.pacEnvironment creation
        , Intent.picProvider = Checkout.pacProvider creation
        , Intent.picPaymentMethod = paymentMethod
        , Intent.picCaptureMethod = Intent.CaptureAutomatic
        , Intent.picAmountMinor = Checkout.pacAmountMinor creation
        , Intent.picCurrency = Checkout.pacCurrency creation
        , Intent.picIdempotencyKey = canonicalIntentKey creation
        , Intent.picOccurredAt = Checkout.pacCreatedAt creation
        , Intent.picCorrelationId = Checkout.pacCorrelationId creation
        }
      case intentResult of
        Left problem -> transactionUndo >> pure (Left problem)
        Right intent | paymentState (Intent.pisLifecycle intent) == PaymentFailed ->
          transactionUndo >> pure (Left "This provider definitively declined; use another verified provider")
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

-- Only authenticated status/capture responses for an already-bound resource
-- may enter here. Transport/authentication errors use recordPaymentFailure
-- directly and keep the canonical intent active. Binding validation is the
-- same amount/currency/order/merchant validation used by the success path.
recordProviderPaymentFailure
  :: Checkout.CheckoutReference
  -> Checkout.PaymentAttemptReference
  -> Checkout.PaymentProvider
  -> Text
  -> Either Text ()
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
recordProviderPaymentFailure checkout attempt provider code binding correlation now
  | not (providerConfirmsNoCharge provider code && isRight binding) =
      Checkout.recordPaymentFailure checkout attempt provider code correlation now
  | otherwise = do
    -- Serialize against fallback creation, which also locks the checkout first.
    _ <- rawSql "SELECT id::text FROM commerce_checkout_session WHERE id = ?::uuid FOR UPDATE"
      [PersistText (Checkout.checkoutReferenceId checkout)] :: SqlPersistT IO [Single Text]
    rows <- rawSql
      "SELECT intent.id::text, intent.status FROM commerce_payment_intent intent\
      \ JOIN commerce_payment_attempt attempt ON attempt.payment_intent_id = intent.id\
      \ JOIN commerce_checkout_session checkout ON checkout.id = intent.checkout_id\
      \ WHERE attempt.id = ?::uuid AND attempt.checkout_id = ?::uuid\
      \ AND intent.checkout_id = attempt.checkout_id AND intent.provider = attempt.provider\
      \ AND attempt.provider = ? AND attempt.environment = checkout.environment\
      \ AND intent.amount_minor = attempt.amount_minor AND intent.currency = attempt.currency\
      \ AND intent.authorized_minor = 0 AND intent.captured_minor = 0\
      \ AND attempt.status <> 'succeeded' FOR UPDATE OF intent, attempt"
      [ PersistText (Checkout.paymentAttemptReferenceId attempt)
      , PersistText (Checkout.checkoutReferenceId checkout)
      , PersistText (Checkout.paymentProviderText provider)
      ] :: SqlPersistT IO [(Single Text, Single Text)]
    case rows of
      [(Single _, Single "failed")] -> pure ()
      [(Single intentId, Single _)] -> do
        result <- Intent.transitionPaymentIntent (Intent.PaymentIntentReference intentId)
          PaymentFailureConfirmed "provider" correlation now
        either (const reject) (const (pure ())) result
        retireAttempts
        Checkout.recordPaymentFailure checkout attempt provider code correlation now
      -- Never retire a captured/authorized or mismatched intent on a decline.
      _ -> reject
  where
    reject = liftIO (ioError (userError "Confirmed decline does not match an unpaid canonical intent"))
    -- A PayPal capture and its earlier create attempt share this intent.
    -- Retire both so legacy "other active provider" guards see the same result.
    retireAttempts = rawExecute
      "UPDATE commerce_payment_attempt SET status = 'failed', failure_code = ?, updated_at = ?\
      \ WHERE payment_intent_id = (SELECT payment_intent_id FROM commerce_payment_attempt\
      \ WHERE id = ?::uuid) AND checkout_id = ?::uuid AND provider = ?\
      \ AND status <> 'succeeded'"
      [ PersistText code, PersistUTCTime now
      , PersistText (Checkout.paymentAttemptReferenceId attempt)
      , PersistText (Checkout.checkoutReferenceId checkout)
      , PersistText (Checkout.paymentProviderText provider)
      ]

-- Explicit documented declines only, not broad error prefixes or generic FAILED.
-- Sources: PayPal capture_status; OPPWA result codes (see review repair runbook).
providerConfirmsNoCharge :: Checkout.PaymentProvider -> Text -> Bool
providerConfirmsNoCharge provider code = case (provider, code) of
  (Checkout.ProviderPayPal, "paypal_declined") -> True
  (Checkout.ProviderDatafast, "800.100.151") -> True -- invalid card
  (Checkout.ProviderDatafast, "800.100.153") -> True -- invalid CVV
  (Checkout.ProviderDatafast, "800.100.155") -> True -- insufficient credit
  _ -> False

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
        let request = PaymentRouteRequest
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
        pure $ if any ((== selected) . routeProvider) (routePayments activations request)
          then Right ()
          else Left "Payment provider is not verified for this method, operation and environment"
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
  "payment-intent:"
    <> Checkout.checkoutReferenceId (Checkout.pacCheckout creation)
    <> ":"
    <> Checkout.paymentProviderText (Checkout.pacProvider creation)
