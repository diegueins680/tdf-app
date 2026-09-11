{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- | Pure, provider-neutral capability and routing policy.
--
-- Provider documentation describes what an integration can support. Runtime
-- activation is deliberately separate: a provider is never routable until its
-- contract, credentials and feature flag are all confirmed for the requested
-- environment. This keeps documentation claims from becoming production
-- availability claims.
module TDF.Commerce.ProviderCapabilities
  ( PaymentMethod(..)
  , ProductFlow(..)
  , PaymentCapability(..)
  , ProviderActivation(..)
  , PaymentRouteRequest(..)
  , PaymentRoute(..)
  , ProviderOutcomeCertainty(..)
  , providerCapabilities
  , routePayments
  , safeToFallback
  , paymentMethodText
  , paymentMethodFromText
  , paymentCapabilityText
  , paymentCapabilityFromText
  , productFlowText
  , productFlowFromText
  ) where

import           Data.Int (Int64)
import           Data.List (nub, sortOn)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T

import           TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment(..)
  , PaymentProvider(..)
  )

data PaymentMethod
  = MethodCard
  | MethodPayPalWallet
  | MethodBankRedirect
  | MethodDeunaQr
  | MethodPayPhoneWallet
  | MethodPaymentLink
  | MethodManualBankTransfer
  deriving (Eq, Ord, Show, Enum, Bounded)

data ProductFlow
  = FlowMerchandise
  | FlowBooking
  | FlowProfessionalService
  | FlowCourse
  | FlowEventTicket
  | FlowDigitalProduct
  | FlowSubscription
  | FlowMarketplace
  deriving (Eq, Ord, Show, Enum, Bounded)

data PaymentCapability
  = CapabilityOneTime
  | CapabilityRecurring
  | CapabilityTokenization
  | CapabilityThreeDS
  | CapabilityInstallments
  | CapabilityAuthorize
  | CapabilityCapture
  | CapabilityVoid
  | CapabilityFullRefund
  | CapabilityPartialRefund
  | CapabilityDisputes
  | CapabilityChargebacks
  | CapabilityPaymentLink
  | CapabilitySignedWebhook
  | CapabilityServerVerification
  | CapabilityConnectedAccounts
  | CapabilitySplitSettlement
  | CapabilitySellerPayouts
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Runtime facts. None of these values may be inferred from the presence of
-- environment-variable names or from a mock test.
data ProviderActivation = ProviderActivation
  { paProvider             :: PaymentProvider
  , paEnvironment          :: CheckoutEnvironment
  , paFeatureEnabled       :: Bool
  , paCredentialsValidated :: Bool
  , paContractApproved     :: Bool
  , paVerifiedMethods      :: [PaymentMethod]
  , paVerifiedCapabilities :: [PaymentCapability]
  , paVerifiedMethodCapabilities :: [(PaymentMethod, PaymentCapability)]
  } deriving (Eq, Show)

data PaymentRouteRequest = PaymentRouteRequest
  { prEnvironment          :: CheckoutEnvironment
  , prBuyerCountry         :: Text
  , prCurrency             :: Text
  , prAmountMinor          :: Int64
  , prMethod               :: PaymentMethod
  , prFlow                 :: ProductFlow
  , prRequiredCapabilities :: [PaymentCapability]
  } deriving (Eq, Show)

data PaymentRoute = PaymentRoute
  { routeProvider     :: PaymentProvider
  , routeMethod       :: PaymentMethod
  , routeCapabilities :: [PaymentCapability]
  , routePriority     :: Int
  } deriving (Eq, Show)

-- | Cross-provider fallback is allowed only when there is authoritative
-- evidence that the previous provider did not and will not create a charge.
-- A timeout, transport error or unverified browser return is ambiguous.
data ProviderOutcomeCertainty
  = ProviderNotContacted
  | ProviderRejectedBeforeCreation
  | ProviderConfirmedNoCharge
  | ProviderAmbiguous
  | ProviderSucceeded
  deriving (Eq, Ord, Show)

data ProviderProfile = ProviderProfile
  { ppProvider     :: PaymentProvider
  , ppFlows        :: [ProductFlow]
  , ppMethodCapabilities :: [(PaymentMethod, [PaymentCapability])]
  }

providerCapabilities :: PaymentProvider -> [PaymentCapability]
providerCapabilities provider =
  maybe [] (nub . concatMap snd . ppMethodCapabilities) (providerProfile provider)

routePayments
  :: [ProviderActivation]
  -> PaymentRouteRequest
  -> [PaymentRoute]
routePayments activations request
  | prAmountMinor request <= 0 = []
  | normalizeCurrency (prCurrency request) /= "USD" = []
  | not (validCountryCode (prBuyerCountry request)) = []
  | otherwise = sortOn routePriority (mapMaybe candidate activations)
  where
    candidate activation = do
      profile <- providerProfile (paProvider activation)
      documentedCapabilities <- lookup
        (prMethod request)
        (ppMethodCapabilities profile)
      let verifiedCapabilities =
            [ capability
            | (method, capability) <- paVerifiedMethodCapabilities activation
            , method == prMethod request
            ]
      if activationReady activation
          && paEnvironment activation == prEnvironment request
          && prFlow request `elem` ppFlows profile
          && all (`elem` documentedCapabilities) (prRequiredCapabilities request)
          && all (`elem` verifiedCapabilities) (prRequiredCapabilities request)
        then Just PaymentRoute
          { routeProvider = ppProvider profile
          , routeMethod = prMethod request
          , routeCapabilities = filter
              (`elem` verifiedCapabilities)
              documentedCapabilities
          , routePriority = providerPriority request (ppProvider profile)
          }
        else Nothing

safeToFallback :: ProviderOutcomeCertainty -> Bool
safeToFallback outcome = case outcome of
  ProviderNotContacted -> True
  ProviderRejectedBeforeCreation -> True
  ProviderConfirmedNoCharge -> True
  ProviderAmbiguous -> False
  ProviderSucceeded -> False

paymentMethodText :: PaymentMethod -> Text
paymentMethodText method = case method of
  MethodCard -> "card"
  MethodPayPalWallet -> "paypal_wallet"
  MethodBankRedirect -> "bank_redirect"
  MethodDeunaQr -> "deuna_qr"
  MethodPayPhoneWallet -> "payphone_wallet"
  MethodPaymentLink -> "payment_link"
  MethodManualBankTransfer -> "manual_bank_transfer"

paymentMethodFromText :: Text -> Maybe PaymentMethod
paymentMethodFromText rawMethod = case normalized rawMethod of
  "card" -> Just MethodCard
  "paypal_wallet" -> Just MethodPayPalWallet
  "bank_redirect" -> Just MethodBankRedirect
  "deuna_qr" -> Just MethodDeunaQr
  "payphone_wallet" -> Just MethodPayPhoneWallet
  "payment_link" -> Just MethodPaymentLink
  "manual_bank_transfer" -> Just MethodManualBankTransfer
  _ -> Nothing

paymentCapabilityText :: PaymentCapability -> Text
paymentCapabilityText capability = case capability of
  CapabilityOneTime -> "one_time"
  CapabilityRecurring -> "recurring"
  CapabilityTokenization -> "tokenization"
  CapabilityThreeDS -> "three_ds"
  CapabilityInstallments -> "installments"
  CapabilityAuthorize -> "authorize"
  CapabilityCapture -> "capture"
  CapabilityVoid -> "void"
  CapabilityFullRefund -> "full_refund"
  CapabilityPartialRefund -> "partial_refund"
  CapabilityDisputes -> "disputes"
  CapabilityChargebacks -> "chargebacks"
  CapabilityPaymentLink -> "payment_link"
  CapabilitySignedWebhook -> "signed_webhook"
  CapabilityServerVerification -> "server_verification"
  CapabilityConnectedAccounts -> "connected_accounts"
  CapabilitySplitSettlement -> "split_settlement"
  CapabilitySellerPayouts -> "seller_payouts"

paymentCapabilityFromText :: Text -> Maybe PaymentCapability
paymentCapabilityFromText rawCapability = case normalized rawCapability of
  "one_time" -> Just CapabilityOneTime
  "recurring" -> Just CapabilityRecurring
  "tokenization" -> Just CapabilityTokenization
  "three_ds" -> Just CapabilityThreeDS
  "installments" -> Just CapabilityInstallments
  "authorize" -> Just CapabilityAuthorize
  "capture" -> Just CapabilityCapture
  "void" -> Just CapabilityVoid
  "full_refund" -> Just CapabilityFullRefund
  "partial_refund" -> Just CapabilityPartialRefund
  "disputes" -> Just CapabilityDisputes
  "chargebacks" -> Just CapabilityChargebacks
  "payment_link" -> Just CapabilityPaymentLink
  "signed_webhook" -> Just CapabilitySignedWebhook
  "server_verification" -> Just CapabilityServerVerification
  "connected_accounts" -> Just CapabilityConnectedAccounts
  "split_settlement" -> Just CapabilitySplitSettlement
  "seller_payouts" -> Just CapabilitySellerPayouts
  _ -> Nothing

productFlowText :: ProductFlow -> Text
productFlowText flow = case flow of
  FlowMerchandise -> "merchandise"
  FlowBooking -> "booking"
  FlowProfessionalService -> "professional_service"
  FlowCourse -> "course"
  FlowEventTicket -> "event_ticket"
  FlowDigitalProduct -> "digital_product"
  FlowSubscription -> "subscription"
  FlowMarketplace -> "marketplace"

productFlowFromText :: Text -> Maybe ProductFlow
productFlowFromText rawFlow = case normalized rawFlow of
  "merchandise" -> Just FlowMerchandise
  "booking" -> Just FlowBooking
  "professional_service" -> Just FlowProfessionalService
  "course" -> Just FlowCourse
  "event_ticket" -> Just FlowEventTicket
  "digital_product" -> Just FlowDigitalProduct
  "subscription" -> Just FlowSubscription
  "marketplace" -> Just FlowMarketplace
  _ -> Nothing

activationReady :: ProviderActivation -> Bool
activationReady activation =
  paFeatureEnabled activation
    && paCredentialsValidated activation
    && paContractApproved activation

providerProfile :: PaymentProvider -> Maybe ProviderProfile
providerProfile provider = case provider of
  ProviderDatafast -> Just ProviderProfile
    { ppProvider = provider
    , ppFlows = directFlows <> [FlowSubscription]
    , ppMethodCapabilities = [(MethodCard,
        [ CapabilityOneTime
        , CapabilityRecurring
        , CapabilityTokenization
        , CapabilityThreeDS
        , CapabilityInstallments
        , CapabilityVoid
        , CapabilityFullRefund
        , CapabilityDisputes
        , CapabilityChargebacks
        , CapabilityServerVerification
        ])]
    }
  ProviderPayPal -> Just ProviderProfile
    { ppProvider = provider
    , ppFlows = directFlows <> [FlowSubscription, FlowMarketplace]
    , ppMethodCapabilities = [(MethodPayPalWallet,
        [ CapabilityOneTime
        , CapabilityRecurring
        , CapabilityAuthorize
        , CapabilityCapture
        , CapabilityVoid
        , CapabilityFullRefund
        , CapabilityPartialRefund
        , CapabilityDisputes
        , CapabilityChargebacks
        , CapabilitySignedWebhook
        , CapabilityServerVerification
        , CapabilityConnectedAccounts
        , CapabilitySplitSettlement
        , CapabilitySellerPayouts
        ])]
    }
  ProviderPlaceToPay -> Just ProviderProfile
    { ppProvider = provider
    , ppFlows = directFlows <> [FlowSubscription]
    , ppMethodCapabilities =
        [ (MethodCard,
            [ CapabilityOneTime
            , CapabilityThreeDS
            , CapabilityInstallments
            , CapabilityPaymentLink
            , CapabilitySignedWebhook
            , CapabilityServerVerification
            ])
        , (MethodBankRedirect,
            [ CapabilityOneTime
            , CapabilitySignedWebhook
            , CapabilityServerVerification
            ])
        , (MethodDeunaQr,
            [ CapabilityOneTime
            , CapabilitySignedWebhook
            , CapabilityServerVerification
            ])
        , (MethodPaymentLink,
            [ CapabilityOneTime
            , CapabilityPaymentLink
            , CapabilitySignedWebhook
            , CapabilityServerVerification
            ])
        ]
    }
  ProviderPayPhone -> Just ProviderProfile
    { ppProvider = provider
    , ppFlows = directFlows
    , ppMethodCapabilities =
        [ (MethodPayPhoneWallet,
            [ CapabilityOneTime
            , CapabilityServerVerification
            ])
        ]
    }
  ProviderBankTransfer -> Just ProviderProfile
    { ppProvider = provider
    , ppFlows = directFlows
    , ppMethodCapabilities = [(MethodManualBankTransfer, [])]
    }
  ProviderStripe -> Nothing
  ProviderCash -> Nothing
  ProviderPos -> Nothing
  ProviderCardano -> Nothing

directFlows :: [ProductFlow]
directFlows =
  [ FlowMerchandise
  , FlowBooking
  , FlowProfessionalService
  , FlowCourse
  , FlowEventTicket
  , FlowDigitalProduct
  ]

providerPriority :: PaymentRouteRequest -> PaymentProvider -> Int
providerPriority request provider = case (prMethod request, prFlow request, provider) of
  (MethodPayPalWallet, _, ProviderPayPal) -> 10
  (MethodDeunaQr, _, ProviderPlaceToPay) -> 10
  (MethodBankRedirect, _, ProviderPlaceToPay) -> 10
  (MethodPayPhoneWallet, _, ProviderPayPhone) -> 10
  (MethodManualBankTransfer, _, ProviderBankTransfer) -> 10
  (_, FlowMarketplace, ProviderPayPal) -> 10
  (MethodCard, _, ProviderDatafast) -> 10
  (MethodCard, _, ProviderPlaceToPay) -> 20
  (MethodCard, _, ProviderPayPhone) -> 30
  (MethodPaymentLink, _, ProviderPlaceToPay) -> 10
  (MethodPaymentLink, _, ProviderPayPhone) -> 20
  _ -> 100

normalizeCurrency :: Text -> Text
normalizeCurrency = T.toUpper . T.strip

normalized :: Text -> Text
normalized = T.toLower . T.strip

validCountryCode :: Text -> Bool
validCountryCode rawCountry =
  let country = T.toUpper (T.strip rawCountry)
  in T.length country == 2 && T.all (`elem` ['A' .. 'Z']) country
