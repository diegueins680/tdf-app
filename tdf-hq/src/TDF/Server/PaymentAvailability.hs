{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Runtime availability for payment methods that have complete public
-- checkout handlers today. Database verification remains authoritative, while
-- this layer also detects removed or environment-mismatched runtime secrets.
module TDF.Server.PaymentAvailability
  ( ProductFlow(..)
  , availableImplementedPaymentMethods
  , loadRuntimeReadyRoutes
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
import           TDF.DB (Env(..))
import qualified TDF.Server.ServiceStorefront as ServiceStorefront

type AppM = ReaderT Env Handler

-- | Return only the labels supported by the existing public product handlers.
-- PlaceToPay and PayPhone remain absent until their shared executors and return
-- flows are wired; documenting or enabling an adapter alone must not expose a
-- non-functional checkout choice.
availableImplementedPaymentMethods
  :: Checkout.CheckoutEnvironment
  -> ProductFlow
  -> Int64
  -> Text
  -> Bool
  -> AppM [Text]
availableImplementedPaymentMethods environment flow amountMinor currency allowManual = do
  routes <- concat <$> mapM (loadRuntimeReadyRoutes . requestFor)
    ([MethodCard, MethodPayPalWallet] <> [MethodManualBankTransfer | allowManual])
  pure . nub $
    [ label
    | label <- ["datafast", "paypal", "bank_transfer"]
    , any ((== Just label) . routeLabel) routes
    ]
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
  filterM (runtimeReady (prEnvironment request))
    (routePayments activations request)

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

runtimeReady :: Checkout.CheckoutEnvironment -> PaymentRoute -> AppM Bool
runtimeReady environment route = case routeProvider route of
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
    liftIO $ (||)
      <$> nonEmptyEnv "COMMERCE_BANK_TRANSFER_INSTRUCTIONS"
      <*> nonEmptyEnv "MERCH_BANK_TRANSFER_INSTRUCTIONS"
  -- These adapters currently have contract tests but no end-to-end shared
  -- executor and public return flow. Keep them unavailable even if an operator
  -- accidentally changes account metadata.
  Checkout.ProviderPlaceToPay -> pure False
  Checkout.ProviderPayPhone -> pure False
  Checkout.ProviderStripe -> pure False
  Checkout.ProviderCash -> pure False
  Checkout.ProviderPos -> pure False
  Checkout.ProviderCardano -> pure False

routeLabel :: PaymentRoute -> Maybe Text
routeLabel route = case (routeProvider route, routeMethod route) of
  (Checkout.ProviderDatafast, MethodCard) -> Just "datafast"
  (Checkout.ProviderPayPal, MethodPayPalWallet) -> Just "paypal"
  (Checkout.ProviderBankTransfer, MethodManualBankTransfer) ->
    Just "bank_transfer"
  _ -> Nothing

nonEmptyEnv :: String -> IO Bool
nonEmptyEnv name = maybe False (not . T.null . T.strip . T.pack) <$> lookupEnv name

minimumEnvLength :: Int -> String -> IO Bool
minimumEnvLength minimumLength name =
  maybe False ((>= minimumLength) . T.length . T.strip . T.pack) <$> lookupEnv name
