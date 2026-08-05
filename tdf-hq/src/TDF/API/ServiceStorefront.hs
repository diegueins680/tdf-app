{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.ServiceStorefront where

import           Data.Text (Text)
import           Servant

import           TDF.API.Types
  ( DatafastCheckoutDTO
  , PaypalCreateDTO
  , PaypalCaptureReq
  )
import           TDF.DTO.SocialEventsDTO (StripePaymentIntentDTO)
import           TDF.API.ServiceStorefrontTypes

-- | Public API for the service storefront (mixing/mastering).
-- No authentication required for browsing and ordering.
type ServiceStorefrontPublicAPI =
       "services" :> "storefront" :> Get '[JSON] [ServiceStorefrontPackageDTO]
  :<|> "services" :> "storefront" :> Capture "packageId" Text :> Get '[JSON] ServiceStorefrontPackageDTO
  :<|> "services" :> "storefront" :> "order" :> ReqBody '[JSON] ServiceStorefrontOrderCreate :> Post '[JSON] ServiceStorefrontOrderDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> Get '[JSON] ServiceStorefrontOrderDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> "stripe" :> "payment-intent" :> Post '[JSON] StripePaymentIntentDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> "datafast" :> "checkout" :> Post '[JSON] DatafastCheckoutDTO
  :<|> "services" :> "storefront" :> "datafast" :> "status" :> QueryParam "orderId" Text :> QueryParam "resourcePath" Text :> Get '[JSON] ServiceStorefrontOrderDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> "paypal" :> "create" :> Post '[JSON] PaypalCreateDTO
  :<|> "services" :> "storefront" :> "paypal" :> "capture" :> ReqBody '[JSON] PaypalCaptureReq :> Post '[JSON] ServiceStorefrontOrderDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> "revision" :> ReqBody '[JSON] ServiceStorefrontRevisionCreate :> Post '[JSON] ServiceStorefrontRevisionDTO

-- | Admin API for managing service storefront orders.
type ServiceStorefrontAdminAPI =
       "admin" :> "services" :> "storefront" :> "orders" :> QueryParam "status" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [ServiceStorefrontOrderDTO]
  :<|> "admin" :> "services" :> "storefront" :> "orders" :> Capture "orderId" Text :> ReqBody '[JSON] ServiceStorefrontOrderUpdate :> Put '[JSON] ServiceStorefrontOrderDTO
  :<|> "admin" :> "services" :> "storefront" :> "packages" :> Get '[JSON] [ServiceStorefrontPackageDTO]
  :<|> "admin" :> "services" :> "storefront" :> "packages" :> ReqBody '[JSON] ServiceStorefrontPackageCreate :> Post '[JSON] ServiceStorefrontPackageDTO
  :<|> "admin" :> "services" :> "storefront" :> "packages" :> Capture "packageId" Text :> ReqBody '[JSON] ServiceStorefrontPackageUpdate :> Put '[JSON] ServiceStorefrontPackageDTO
