{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Server
  ( application
  ) where

import Data.Proxy (Proxy(..))
import Network.Wai (Application)
import Servant

import TDF.API (API, server)

application :: Application
application = serve (Proxy :: Proxy API) server

