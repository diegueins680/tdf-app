{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.ServiceStorefront
  ( serviceStorefrontPublicServer
  , serviceStorefrontAdminServer
  ) where

import           Data.Text (Text)
import           Control.Monad.Reader (ReaderT)
import           Servant

import           TDF.API.ServiceStorefront (ServiceStorefrontPublicAPI, ServiceStorefrontAdminAPI)
import           TDF.API.ServiceStorefrontTypes
import           TDF.API.Types (DatafastCheckoutDTO(..), PaypalCreateDTO(..), PaypalCaptureReq(..))
import           TDF.DTO.SocialEventsDTO (StripePaymentIntentDTO(..))
import           TDF.DB (Env)

type AppM = ReaderT Env Handler

-- | Stub public server for the service storefront.
-- Returns empty/placeholder data until full implementation is complete.
serviceStorefrontPublicServer :: ServerT ServiceStorefrontPublicAPI AppM
serviceStorefrontPublicServer =
       listPackagesStub
  :<|> getPackageStub
  :<|> createOrderStub
  :<|> getOrderStub
  :<|> createStripePaymentIntentStub
  :<|> createDatafastCheckoutStub
  :<|> confirmDatafastStatusStub
  :<|> createPaypalOrderStub
  :<|> capturePaypalStub
  :<|> createRevisionStub

-- | Stub admin server for the service storefront.
serviceStorefrontAdminServer :: ServerT ServiceStorefrontAdminAPI AppM
serviceStorefrontAdminServer =
       listOrdersAdminStub
  :<|> updateOrderAdminStub
  :<|> listPackagesAdminStub
  :<|> createPackageAdminStub
  :<|> updatePackageAdminStub

-- Stub implementations (to be replaced with real DB queries)

listPackagesStub :: AppM [ServiceStorefrontPackageDTO]
listPackagesStub = pure []

getPackageStub :: Text -> AppM ServiceStorefrontPackageDTO
getPackageStub _ = throwError err404 { errBody = "Package not found (stub)" }

createOrderStub :: ServiceStorefrontOrderCreate -> AppM ServiceStorefrontOrderDTO
createOrderStub _ = throwError err501 { errBody = "Order creation not yet implemented" }

getOrderStub :: Text -> AppM ServiceStorefrontOrderDTO
getOrderStub _ = throwError err404 { errBody = "Order not found (stub)" }

createStripePaymentIntentStub :: Text -> AppM StripePaymentIntentDTO
createStripePaymentIntentStub _ = throwError err501 { errBody = "Stripe not available in Ecuador" }

createDatafastCheckoutStub :: Text -> AppM DatafastCheckoutDTO
createDatafastCheckoutStub _ = throwError err501 { errBody = "Datafast checkout not yet implemented" }

confirmDatafastStatusStub :: Maybe Text -> Maybe Text -> AppM ServiceStorefrontOrderDTO
confirmDatafastStatusStub _ _ = throwError err501 { errBody = "Datafast status not yet implemented" }

createPaypalOrderStub :: Text -> AppM PaypalCreateDTO
createPaypalOrderStub _ = throwError err501 { errBody = "PayPal order not yet implemented" }

capturePaypalStub :: PaypalCaptureReq -> AppM ServiceStorefrontOrderDTO
capturePaypalStub _ = throwError err501 { errBody = "PayPal capture not yet implemented" }

createRevisionStub :: Text -> ServiceStorefrontRevisionCreate -> AppM ServiceStorefrontRevisionDTO
createRevisionStub _ _ = throwError err501 { errBody = "Revision creation not yet implemented" }

-- Admin stubs

listOrdersAdminStub :: Maybe Text -> Maybe Int -> Maybe Int -> AppM [ServiceStorefrontOrderDTO]
listOrdersAdminStub _ _ _ = pure []

updateOrderAdminStub :: Text -> ServiceStorefrontOrderUpdate -> AppM ServiceStorefrontOrderDTO
updateOrderAdminStub _ _ = throwError err501 { errBody = "Order update not yet implemented" }

listPackagesAdminStub :: AppM [ServiceStorefrontPackageDTO]
listPackagesAdminStub = pure []

createPackageAdminStub :: ServiceStorefrontPackageCreate -> AppM ServiceStorefrontPackageDTO
createPackageAdminStub _ = throwError err501 { errBody = "Package creation not yet implemented" }

updatePackageAdminStub :: Text -> ServiceStorefrontPackageUpdate -> AppM ServiceStorefrontPackageDTO
updatePackageAdminStub _ _ = throwError err501 { errBody = "Package update not yet implemented" }
