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
  , productFlowForDomain
  ) where

import           Data.Text (Text)
import           Database.Persist (PersistValue(..))
import           Database.Persist.Sql
  ( Single(..), SqlPersistT, rawSql, transactionSave, transactionUndo )

import qualified TDF.Commerce.CheckoutStore as Checkout
import qualified TDF.Commerce.PaymentIntentStore as Intent
import           TDF.Commerce.ProviderCapabilities
  ( PaymentCapability(..), PaymentMethod(..), PaymentRoute(..)
  , PaymentRouteRequest(..), ProductFlow(..), routePayments )
import           TDF.Commerce.ProviderCapabilityStore (loadProviderActivations)

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
              , prRequiredCapabilities = operationCapabilities
                  flow (Checkout.pacOperation creation)
              }
            selected = Checkout.pacProvider creation
        pure $ if any ((== selected) . routeProvider) (routePayments activations request)
          then Right ()
          else Left "Payment provider is not verified for this method, operation and environment"
    [] -> pure (Left "Canonical checkout was not found")
    _ -> pure (Left "Canonical checkout lookup was ambiguous")

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
