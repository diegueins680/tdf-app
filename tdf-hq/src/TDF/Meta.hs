{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module TDF.Meta
  ( MetaAPI
  , metaServer
  , BuildInfo(..)
  , redocIndex
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (ToJSON)
import           Data.Time                  (UTCTime, getCurrentTime)
import           Data.Version               (showVersion)
import           GHC.Generics               (Generic)
import           Network.HTTP.Media         ((//), (/:))
import           Servant
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Data.FileEmbed             (embedFile)
import qualified Paths_tdf_hq               as Paths

-- | Basic build info for the About dialog.
data BuildInfo = BuildInfo
  { app     :: T.Text
  , version :: T.Text
  , builtAt :: UTCTime
  } deriving (Show, Generic)
instance ToJSON BuildInfo

type MetaAPI =
       "version"      :> Get '[JSON] BuildInfo
  :<|> "openapi.yaml" :> Get '[PlainText] T.Text
  :<|> "docs"         :> Get '[HTML] T.Text

metaServer :: Server MetaAPI
metaServer = versionH :<|> openapiH :<|> docsH
  where
    versionH = do
      now <- liftIO getCurrentTime
      pure BuildInfo
        { app     = "tdf-hq"
        , version = T.pack (showVersion Paths.version)
        , builtAt = now
        }
    openapiH = pure openapiSpec
    docsH    = pure (T.pack redocIndex)

openapiSpec :: T.Text
openapiSpec = TE.decodeUtf8 $(embedFile "docs/openapi/api.yaml")

-- | Minimal content type to serve HTML documents encoded as UTF-8.
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML T.Text where
  mimeRender _ = TLE.encodeUtf8 . TL.fromStrict

-- | Minimal Redoc index that points to /openapi.yaml on the same server.
redocIndex :: String
redocIndex = unlines
  [ "<!doctype html>"
  , "<html lang='en'>"
  , "<head>"
  , "  <meta charset='utf-8'/>"
  , "  <meta name='viewport' content='width=device-width, initial-scale=1'/>"
  , "  <title>TDF API Docs</title>"
  , "</head>"
  , "<body>"
  , "  <div id='redoc-container'></div>"
  , "  <script src='https://cdn.redoc.ly/redoc/latest/bundles/redoc.standalone.js'></script>"
  , "  <script>"
  , "    window.addEventListener('load', function () {"
  , "      Redoc.init('/openapi.yaml', {}, document.getElementById('redoc-container'));"
  , "    });"
  , "  </script>"
  , "</body>"
  , "</html>"
  ]

{-
To mount these routes in your Servant 'API' and 'Server', add something like:

  import TDF.Meta (MetaAPI, metaServer)
  type API = MetaAPI :<|> ExistingAPI
  server :: Server API
  server = metaServer :<|> existingServer

If your app exposes 'app :: Application' instead, wrap your existing server:

  import Network.Wai (Application)
  import Servant
  app :: Application
  app = serve (Proxy :: Proxy MetaAPI) metaServer <|> existingApp

Adjust the wiring to your codebase as needed.
-}
