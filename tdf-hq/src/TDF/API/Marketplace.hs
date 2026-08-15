{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Marketplace where

import           Data.Text (Text)
import           Servant

import           TDF.API.Types
  ( MarketplaceItemDTO
  , MarketplaceCartDTO
  , MarketplaceCartItemUpdate
  , MarketplaceCheckoutReq
  , MarketplaceOrderDTO
  , MarketplaceOrderUpdate
  , MarketplaceFulfillmentUpdate
  , DatafastCheckoutDTO
  , PaypalCreateDTO
  , PaypalCaptureReq
  )
import           TDF.DTO.SocialEventsDTO (StripePaymentIntentDTO)

type MarketplaceAPI =
       Get '[JSON] [MarketplaceItemDTO]
  :<|> Capture "id" Text :> Get '[JSON] MarketplaceItemDTO
  :<|> "cart" :> Post '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> Get '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> "items" :> ReqBody '[JSON] MarketplaceCartItemUpdate :> Post '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> "checkout" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] MarketplaceOrderDTO
  :<|> "cart" :> Capture "cartId" Text :> "stripe" :> "payment-intent" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] StripePaymentIntentDTO
  :<|> "cart" :> Capture "cartId" Text :> "datafast" :> "checkout" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] DatafastCheckoutDTO
  :<|> "datafast" :> "status" :> Header "X-Order-Lookup-Token" Text :> QueryParam "orderId" Text :> QueryParam "resourcePath" Text :> Get '[JSON] MarketplaceOrderDTO
  :<|> "cart" :> Capture "cartId" Text :> "paypal" :> "create" :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] PaypalCreateDTO
  :<|> "paypal" :> "capture" :> Header "X-Order-Lookup-Token" Text :> ReqBody '[JSON] PaypalCaptureReq :> Post '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> Header "X-Order-Lookup-Token" Text :> Get '[JSON] MarketplaceOrderDTO

type MarketplaceAdminAPI =
       "orders" :> QueryParam "status" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [MarketplaceOrderDTO]
  :<|> "orders" :> Capture "orderId" Text :> ReqBody '[JSON] MarketplaceOrderUpdate :> Put '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> "fulfillment" :> ReqBody '[JSON] MarketplaceFulfillmentUpdate :> Put '[JSON] MarketplaceOrderDTO
