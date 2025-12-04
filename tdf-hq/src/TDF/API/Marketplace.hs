{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Marketplace where

import           Data.Text (Text)
import           Servant

import           TDF.API.Types (MarketplaceItemDTO)

type MarketplaceAPI =
       Get '[JSON] [MarketplaceItemDTO]
  :<|> Capture "id" Text :> Get '[JSON] MarketplaceItemDTO
