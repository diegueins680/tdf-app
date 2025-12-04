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
  , PaypalCreateDTO
  , PaypalCaptureReq
  )

type MarketplaceAPI =
       Get '[JSON] [MarketplaceItemDTO]
  :<|> Capture "id" Text :> Get '[JSON] MarketplaceItemDTO
  :<|> "cart" :> Post '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> Get '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> "items" :> ReqBody '[JSON] MarketplaceCartItemUpdate :> Post '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> "checkout" :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] MarketplaceOrderDTO
  :<|> "cart" :> Capture "cartId" Text :> "paypal" :> "create" :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] PaypalCreateDTO
  :<|> "paypal" :> "capture" :> ReqBody '[JSON] PaypalCaptureReq :> Post '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> Get '[JSON] MarketplaceOrderDTO

type MarketplaceAdminAPI =
       "orders" :> QueryParam "status" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] [MarketplaceOrderDTO]
  :<|> "orders" :> Capture "orderId" Text :> ReqBody '[JSON] MarketplaceOrderUpdate :> Put '[JSON] MarketplaceOrderDTO
