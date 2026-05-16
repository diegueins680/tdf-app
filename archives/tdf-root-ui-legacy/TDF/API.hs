{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API
  ( API
  , server
  ) where

import Servant

import TDF.Meta (MetaAPI, VersionInfo(..), metaServer)

-- | Top-level API type for the service. Extend with other sub-APIs as needed.
type API = MetaAPI

-- | Combined server.
server :: Server API
server = metaServer (VersionInfo "dev")

