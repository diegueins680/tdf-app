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
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import           Data.Time                  (UTCTime, getCurrentTime)
import           Data.Version               (showVersion)
import           GHC.Generics               (Generic)
import           Network.HTTP.Media         ((//), (/:))
import           Servant
import qualified Data.Text                  as T
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
  :<|> "openapi.yaml" :> Get '[YAML] BL.ByteString
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

openapiSpec :: BL.ByteString
openapiSpec = BL.fromStrict $(embedFile "docs/openapi/api.yaml")

logoDataUri :: String
logoDataUri =
  "data:image/svg+xml;base64," <>
  BC.unpack (B64.encode $(embedFile "docs/assets/tdf-logo-white.svg"))

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
  , "  <style>"
  , "    body { margin: 0; font-family: 'Inter', 'Segoe UI', sans-serif; background: #090b12; }"
  , "    .docs-shell { min-height: 100vh; background: linear-gradient(180deg,#090b12 0%,#0d101a 32%,#f6f7fb 32%); }"
  , "    .docs-hero { padding: 32px 40px 48px; display: flex; align-items: center; gap: 28px; }"
  , "    .docs-hero img { width: 220px; max-width: 60vw; height: auto; display: block; filter: drop-shadow(0 6px 20px rgba(0,0,0,0.35)); }"
  , "    .docs-hero span { color: #f7f8fb; letter-spacing: 0.32em; font-size: 0.95rem; text-transform: uppercase; }"
  , "    #redoc-container { background: #f6f7fb; border-radius: 36px 36px 0 0; padding-top: 8px; min-height: calc(100vh - 150px); box-shadow: 0 -20px 60px rgba(9,11,18,0.35); }"
  , "    @media (max-width: 640px) {"
  , "      .docs-hero { flex-direction: column; align-items: flex-start; padding: 24px; gap: 16px; }"
  , "      .docs-hero img { width: 180px; }"
  , "      .docs-hero span { letter-spacing: 0.2em; }"
  , "      #redoc-container { border-radius: 28px 28px 0 0; }"
  , "    }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <div class='docs-shell'>"
  , "    <header class='docs-hero'>"
  , "      <img src='" <> logoDataUri <> "' alt='TDF Records logo'/>"
  , "      <span>API REFERENCE</span>"
  , "    </header>"
  , "    <div id='redoc-container'></div>"
  , "  </div>"
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
-- | Serve the embedded OpenAPI document with the canonical YAML mime type.
data YAML

instance Accept YAML where
  contentType _ = "application" // "yaml"

instance MimeRender YAML BL.ByteString where
  mimeRender _ = id
