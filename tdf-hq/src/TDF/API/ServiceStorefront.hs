{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.ServiceStorefront where

import           Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import           Servant

import           TDF.API.Types
  ( DatafastCheckoutDTO
  , PaypalCreateDTO
  , RawJSON
  )
import           TDF.DTO.SocialEventsDTO (StripePaymentIntentDTO)
import           TDF.API.ServiceStorefrontTypes

-- | Public API for the service storefront (mixing/mastering).
-- No authentication required for browsing and ordering.
type ServiceStorefrontPublicAPI =
       "services" :> "storefront" :> Get '[JSON] [ServiceStorefrontPackageDTO]
  :<|> "services" :> "storefront" :> Capture "packageId" Text :> Get '[JSON] ServiceStorefrontPackageDTO
  :<|> "services" :> "storefront" :> "order" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] ServiceStorefrontOrderCreate :> Post '[JSON] ServiceStorefrontOrderDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> Header "X-Order-Lookup-Token" Text :> Get '[JSON] ServiceStorefrontOrderDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> "stripe" :> "payment-intent" :> Header "X-Order-Lookup-Token" Text :> Post '[JSON] StripePaymentIntentDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> "datafast" :> "checkout" :> Header "X-Order-Lookup-Token" Text :> Post '[JSON] DatafastCheckoutDTO
  :<|> "services" :> "storefront" :> "datafast" :> "status" :> QueryParam "orderId" Text :> QueryParam "resourcePath" Text :> Header "X-Order-Lookup-Token" Text :> Get '[JSON] ServiceStorefrontOrderDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> "paypal" :> "create" :> Header "X-Order-Lookup-Token" Text :> Post '[JSON] PaypalCreateDTO
  :<|> "services" :> "storefront" :> "paypal" :> "capture" :> Header "X-Order-Lookup-Token" Text :> ReqBody '[JSON] ServiceStorefrontPaypalCaptureReq :> Post '[JSON] ServiceStorefrontOrderDTO
  :<|> "services" :> "storefront" :> "paypal" :> "webhook"
         :> Header "PayPal-Transmission-Id" Text
         :> Header "PayPal-Transmission-Time" Text
         :> Header "PayPal-Cert-Url" Text
         :> Header "PayPal-Auth-Algo" Text
         :> Header "PayPal-Transmission-Sig" Text
         :> ReqBody '[RawJSON] BL.ByteString
         :> Post '[JSON] NoContent
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> "manual-payment" :> Header "X-Order-Lookup-Token" Text :> ReqBody '[JSON] ServiceStorefrontManualPaymentCreate :> Post '[JSON] ServiceStorefrontOrderDTO
  :<|> "services" :> "storefront" :> "order" :> Capture "orderId" Text :> "revision" :> Header "X-Order-Lookup-Token" Text :> ReqBody '[JSON] ServiceStorefrontRevisionCreate :> Post '[JSON] ServiceStorefrontRevisionDTO

-- | Admin API for managing service storefront orders.
type ServiceStorefrontAdminAPI =
       "admin" :> "services" :> "storefront" :> "orders" :> QueryParam "status" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [ServiceStorefrontOrderDTO]
  :<|> "admin" :> "services" :> "storefront" :> "orders" :> Capture "orderId" Text :> ReqBody '[JSON] ServiceStorefrontOrderUpdate :> Put '[JSON] ServiceStorefrontOrderDTO
  :<|> "admin" :> "services" :> "storefront" :> "packages" :> Get '[JSON] [ServiceStorefrontPackageDTO]
  :<|> "admin" :> "services" :> "storefront" :> "packages" :> ReqBody '[JSON] ServiceStorefrontPackageCreate :> Post '[JSON] ServiceStorefrontPackageDTO
  :<|> "admin" :> "services" :> "storefront" :> "packages" :> Capture "packageId" Text :> ReqBody '[JSON] ServiceStorefrontPackageUpdate :> Put '[JSON] ServiceStorefrontPackageDTO
  :<|> "admin" :> "services" :> "storefront" :> "orders" :> Capture "orderId" Text :> "refunds" :> Get '[JSON] [ServiceStorefrontRefundDTO]
  :<|> "admin" :> "services" :> "storefront" :> "orders" :> Capture "orderId" Text :> "refunds" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] ServiceStorefrontRefundCreate :> Post '[JSON] ServiceStorefrontRefundDTO
  :<|> "admin" :> "services" :> "storefront" :> "refunds" :> Capture "refundId" Text :> "approve" :> Post '[JSON] ServiceStorefrontRefundDTO
  :<|> "admin" :> "services" :> "storefront" :> "orders" :> Capture "orderId" Text :> "reconcile" :> Post '[JSON] ServiceStorefrontReconciliationDTO
