{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.PaymentCapabilities
  ( paymentCapabilitiesServer
  , parsePaymentMethod
  , parseProductFlow
  , parsePaymentCapability
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask)
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Database.Persist.Sql (runSqlPool)
import           Servant
import           System.Environment (lookupEnv)

import           TDF.API.PaymentCapabilities
import           TDF.Commerce.CheckoutStore
  ( CheckoutEnvironment
  , checkoutEnvironmentText
  , paymentProviderText
  , resolveCheckoutEnvironment
  )
import           TDF.Commerce.ProviderCapabilities
import           TDF.Commerce.ProviderCapabilityStore (loadProviderActivations)
import           TDF.DB (Env(..))

type AppM = ReaderT Env Handler

paymentCapabilitiesServer :: ServerT PaymentCapabilitiesAPI AppM
paymentCapabilitiesServer buyerCountry currency amountMinor method flow required = do
  Env{..} <- ask
  environment <- loadEnvironment
  request <- either (throwError . badRequest) pure $
    buildRequest environment buyerCountry currency amountMinor method flow required
  activations <- liftIO $ flip runSqlPool envPool $
    loadProviderActivations environment
  pure PaymentCapabilityResponseDTO
    { pcrEnvironment = checkoutEnvironmentText environment
    , pcrBuyerCountry = T.toUpper (T.strip buyerCountry)
    , pcrCurrency = T.toUpper (T.strip currency)
    , pcrAmountMinor = amountMinor
    , pcrPaymentMethod = paymentMethodText (prMethod request)
    , pcrProductFlow = productFlowText (prFlow request)
    , pcrRoutes = map routeToDTO (routePayments activations request)
    , pcrFallbackPolicy = "Fallback requires authoritative confirmation that no charge was or will be created; ambiguous outcomes must be reconciled first."
    }

loadEnvironment :: AppM CheckoutEnvironment
loadEnvironment = do
  rawEnvironment <- liftIO (lookupEnv "COMMERCE_CHECKOUT_ENV")
  either (throwError . configurationError) pure
    (resolveCheckoutEnvironment rawEnvironment)

buildRequest
  :: CheckoutEnvironment
  -> Text
  -> Text
  -> Int64
  -> Text
  -> Text
  -> [Text]
  -> Either Text PaymentRouteRequest
buildRequest environment buyerCountry currency amountMinor rawMethod rawFlow rawRequired = do
  method <- parsePaymentMethod rawMethod
  flow <- parseProductFlow rawFlow
  required <- traverse parsePaymentCapability rawRequired
  if amountMinor <= 0
    then Left "amountMinor must be positive"
    else Right PaymentRouteRequest
      { prEnvironment = environment
      , prBuyerCountry = buyerCountry
      , prCurrency = currency
      , prAmountMinor = amountMinor
      , prMethod = method
      , prFlow = flow
      , prRequiredCapabilities = required
      }

parsePaymentMethod :: Text -> Either Text PaymentMethod
parsePaymentMethod rawMethod = maybe
  (Left "Unsupported paymentMethod")
  Right
  (paymentMethodFromText rawMethod)

parseProductFlow :: Text -> Either Text ProductFlow
parseProductFlow rawFlow = maybe
  (Left "Unsupported productFlow")
  Right
  (productFlowFromText rawFlow)

parsePaymentCapability :: Text -> Either Text PaymentCapability
parsePaymentCapability rawCapability = maybe
  (Left "Unsupported required payment capability")
  Right
  (paymentCapabilityFromText rawCapability)

routeToDTO :: PaymentRoute -> PaymentRouteDTO
routeToDTO PaymentRoute{..} = PaymentRouteDTO
  { prdProvider = paymentProviderText routeProvider
  , prdPaymentMethod = paymentMethodText routeMethod
  , prdCapabilities = map paymentCapabilityText routeCapabilities
  , prdPriority = routePriority
  }

badRequest :: Text -> ServerError
badRequest message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

configurationError :: Text -> ServerError
configurationError message = err503 { errBody = BL.fromStrict (TE.encodeUtf8 message) }
