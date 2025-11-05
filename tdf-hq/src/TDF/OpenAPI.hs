{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.OpenAPI where

import Data.OpenApi
import Data.Proxy (Proxy(..))
import Servant.OpenApi
import TDF.API
import Control.Lens
import Data.Text (Text)

-- | Generate OpenAPI specification
apiSpec :: OpenApi
apiSpec = toOpenApi (Proxy :: Proxy API)
  & info.title .~ "TDF Records API"
  & info.version .~ "1.0"
  & info.description ?~ "API for TDF Records Management Platform - User Role Management"
  & info.license ?~ License "MIT" Nothing
