{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Services where

import           Data.Int   (Int64)
import           Servant

import           TDF.API.Types (ServiceCatalogCreate, ServiceCatalogDTO, ServiceCatalogUpdate)

type ServiceCatalogPublicAPI =
       "services" :> "catalog" :> "public" :> Get '[JSON] [ServiceCatalogDTO]

type ServiceCatalogAPI =
       "services" :> "catalog" :> QueryParam "includeInactive" Bool :> Get '[JSON] [ServiceCatalogDTO]
  :<|> "services" :> "catalog" :> ReqBody '[JSON] ServiceCatalogCreate :> PostCreated '[JSON] ServiceCatalogDTO
  :<|> "services" :> "catalog" :> Capture "id" Int64 :> ReqBody '[JSON] ServiceCatalogUpdate :> Patch '[JSON] ServiceCatalogDTO
  :<|> "services" :> "catalog" :> Capture "id" Int64 :> Delete '[JSON] NoContent
