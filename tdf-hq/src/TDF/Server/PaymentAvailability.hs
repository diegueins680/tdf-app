{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Runtime availability for payment methods that have complete public
-- checkout handlers today. Database verification remains authoritative, while
-- this layer also detects removed or environment-mismatched runtime secrets.
module TDF.Server.PaymentAvailability
  ( ProductFlow(..)
  , availableImplementedPaymentMethods
  , loadRuntimeReadyRoutes
  , publicPaymentRouteLabel
  , bankTransferInstructionsReady
  , manualTransferInstructionsConfigured
  ) where

import           Control.Monad (filterM)
import           Control.Monad.Except (catchError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import           Data.Int (Int64)
import           Data.List (nub)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist.Sql (runSqlPool)
import           Servant (Handler)
import           System.Environment (lookupEnv)

import qualified TDF.Commerce.CheckoutStore as Checkout
import           TDF.Commerce.ProviderCapabilities
import           TDF.Commerce.ProviderCapabilityStore (loadProviderActivations)
import           TDF.Commerce.ProviderRuntimeConfig
  ( runtimeProviderConfigured, runtimeProviderMethodConfigured )
import           TDF.DB (Env(..))
import qualified TDF.Server.ServiceStorefront as ServiceStorefront

type AppM = ReaderT Env Handler

-- | Return only labels backed by an enabled database route, complete runtime
-- configuration, and a public checkout executor. Hosted PlaceToPay and
-- PayPhone labels are intentionally provider-specific so clients cannot
-- silently substitute a different rail after an ambiguous attempt.
availableImplementedPaymentMethods
  :: Checkout.CheckoutEnvironment
  -> ProductFlow
  -> Int64
  -> Text
  -> Bool
  -> AppM [Text]
availableImplementedPaymentMethods environment flow amountMinor currency allowManual = do
  routes <- concat <$> mapM (loadRuntimeReadyRoutes . requestFor)
    ( [ MethodCard
      , MethodPayPalWallet
      , MethodBankRedirect
      , MethodDeunaQr
      , MethodPayPhoneWallet
      ]
      <> [MethodManualBankTransfer | allowManual]
    )
  pure . nub $ [label | route <- routes, Just label <- [publicPaymentRouteLabel route]]
  where
    requestFor method = PaymentRouteRequest
      { prEnvironment = environment
      -- The hosted provider step collects the billing country. "ZZ" is the
      -- ISO user-assigned unknown-country code and cannot grant a country-
      -- specific capability in the current provider profiles.
      , prBuyerCountry = "ZZ"
      , prCurrency = currency
      , prAmountMinor = amountMinor
      , prMethod = method
      , prFlow = flow
      , prRequiredCapabilities = requiredCapabilities flow
      }

loadRuntimeReadyRoutes :: PaymentRouteRequest -> AppM [PaymentRoute]
loadRuntimeReadyRoutes request = do
  Env{..} <- ask
  activations <- liftIO $ flip runSqlPool envPool $
    loadProviderActivations (prEnvironment request)
  filterM (runtimeReady (prEnvironment request) (prFlow request))
    (routePayments activations (requireCheckoutCompletion request))

requiredCapabilities :: ProductFlow -> [PaymentCapability]
requiredCapabilities flow =
  [CapabilityOneTime]
    <> if flow == FlowMarketplace
      then
        [ CapabilityConnectedAccounts
        , CapabilitySplitSettlement
        , CapabilitySellerPayouts
        ]
      else []

runtimeReady :: Checkout.CheckoutEnvironment -> ProductFlow -> PaymentRoute -> AppM Bool
runtimeReady environment flow route = case routeProvider route of
  Checkout.ProviderDatafast ->
    ((== environment) . ServiceStorefront.sdfEnvironment
      <$> ServiceStorefront.loadServiceDatafastEnv)
      `catchError` const (pure False)
  Checkout.ProviderPayPal -> do
    configured <- ((\(_, _, _, configuredEnvironment, _) ->
        pure (configuredEnvironment == environment))
      =<< ServiceStorefront.loadPaypalEnvForService)
      `catchError` const (pure False)
    webhookId <- liftIO (nonEmptyEnv "PAYPAL_WEBHOOK_ID")
    inboxKey <- liftIO (minimumEnvLength 32 "COMMERCE_EVENT_ENCRYPTION_KEY")
    pure (configured && webhookId && inboxKey)
  Checkout.ProviderBankTransfer ->
    liftIO (manualTransferInstructionsConfigured flow)
  Checkout.ProviderPlaceToPay ->
    liftIO (runtimeProviderMethodConfigured environment Checkout.ProviderPlaceToPay
      (routeMethod route))
  Checkout.ProviderPayPhone ->
    liftIO (runtimeProviderConfigured environment Checkout.ProviderPayPhone)
  Checkout.ProviderStripe -> pure False
  Checkout.ProviderCash -> pure False
  Checkout.ProviderPos -> pure False
  Checkout.ProviderCardano -> pure False

-- Merchandise-specific instructions must never enable another product's rail.
bankTransferInstructionsReady :: ProductFlow -> Bool -> Bool -> Bool
bankTransferInstructionsReady flow commerceReady merchReady =
  commerceReady || (flow == FlowMerchandise && merchReady)

manualTransferInstructionsConfigured :: ProductFlow -> IO Bool
manualTransferInstructionsConfigured flow = do
  generic <- nonEmptyEnv "COMMERCE_BANK_TRANSFER_INSTRUCTIONS"
  merch <- if not generic && flow == FlowMerchandise
    then nonEmptyEnv "MERCH_BANK_TRANSFER_INSTRUCTIONS"
    else pure False
  pure (bankTransferInstructionsReady flow generic merch)

publicPaymentRouteLabel :: PaymentRoute -> Maybe Text
publicPaymentRouteLabel route = case (routeProvider route, routeMethod route) of
  (Checkout.ProviderDatafast, MethodCard) -> Just "datafast"
  (Checkout.ProviderPayPal, MethodPayPalWallet) -> Just "paypal"
  (Checkout.ProviderPlaceToPay, MethodCard) -> Just "placetopay_card"
  (Checkout.ProviderPlaceToPay, MethodBankRedirect) ->
    Just "placetopay_bank_redirect"
  (Checkout.ProviderPlaceToPay, MethodDeunaQr) -> Just "placetopay_deuna_qr"
  (Checkout.ProviderPayPhone, MethodPayPhoneWallet) -> Just "payphone_wallet"
  (Checkout.ProviderBankTransfer, MethodManualBankTransfer) ->
    Just "bank_transfer"
  _ -> Nothing

nonEmptyEnv :: String -> IO Bool
nonEmptyEnv name = maybe False (not . T.null . T.strip . T.pack) <$> lookupEnv name

minimumEnvLength :: Int -> String -> IO Bool
minimumEnvLength minimumLength name =
  maybe False ((>= minimumLength) . T.length . T.strip . T.pack) <$> lookupEnv name
