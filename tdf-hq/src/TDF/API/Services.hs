{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Services where

import           Servant

import           Data.Text (Text)
import           TDF.API.Types (ServiceCatalogEnvelopeDTO)

type ServiceCatalogPublicAPI =
       "services" :> "catalog" :> "public"
         :> QueryParam "locale" Text
         :> Header "If-None-Match" Text
         :> Get '[JSON] (Headers '[Header "ETag" Text] ServiceCatalogEnvelopeDTO)

type ServiceCatalogAPI =
       "services" :> "catalog"
         :> QueryParam "includeInactive" Bool
         :> QueryParam "locale" Text
         :> Header "If-None-Match" Text
         :> Get '[JSON] (Headers '[Header "ETag" Text] ServiceCatalogEnvelopeDTO)
