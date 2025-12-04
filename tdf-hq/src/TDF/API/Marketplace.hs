{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Marketplace where

import           Data.Text (Text)
import           Servant

import           TDF.API.Types (MarketplaceItemDTO, MarketplaceCartDTO, MarketplaceCartItemUpdate, MarketplaceCheckoutReq, MarketplaceOrderDTO)

type MarketplaceAPI =
       Get '[JSON] [MarketplaceItemDTO]
  :<|> Capture "id" Text :> Get '[JSON] MarketplaceItemDTO
  :<|> "cart" :> Post '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> Get '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> "items" :> ReqBody '[JSON] MarketplaceCartItemUpdate :> Post '[JSON] MarketplaceCartDTO
  :<|> "cart" :> Capture "cartId" Text :> "checkout" :> ReqBody '[JSON] MarketplaceCheckoutReq :> Post '[JSON] MarketplaceOrderDTO
  :<|> "orders" :> Capture "orderId" Text :> Get '[JSON] MarketplaceOrderDTO
