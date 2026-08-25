{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Meta
  ( MetaAPI
  , metaServer
  , VersionInfo(..)
  ) where

import Data.Aeson (ToJSON)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.HTML.Blaze (HTML)
import qualified Data.ByteString.Lazy as LBS
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

-- | Response payload returned by the version endpoint.
newtype VersionInfo = VersionInfo
  { version :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON VersionInfo

-- | API surface served by meta routes (version + docs helpers).
type MetaAPI =
       "version" :> Get '[JSON] VersionInfo
  :<|> "openapi.yaml" :> Get '[OctetStream, OpenApiYaml] LBS.ByteString
  :<|> "docs" :> Get '[HTML] Html

-- | Custom content-type so Redoc receives a YAML response.
data OpenApiYaml

instance Accept OpenApiYaml where
  contentType _ = "application" // "yaml" /: ("charset", "utf-8")

instance MimeRender OpenApiYaml LBS.ByteString where
  mimeRender _ = id

-- | Server implementing the 'MetaAPI'.
metaServer :: VersionInfo -> Server MetaAPI
metaServer ver =
       pure ver
  :<|> pure embeddedOpenApi
  :<|> pure docsPage

embeddedOpenApi :: LBS.ByteString
embeddedOpenApi = LBS.fromStrict $(makeRelativeToProject "docs/openapi/lessons-and-receipts.yaml" >>= embedFile)

-- | Inline Redoc HTML pointing to the bundled spec.
docsPage :: Html

-- Slight inline page mirroring what Redoc's standalone build expects.
docsPage =
  H.docTypeHtml $ do
    H.head $ do
      H.meta H.! HA.charset "utf-8"
      H.title "TDF API Documentation"
      H.link H.! HA.rel "stylesheet" H.! HA.href "https://fonts.googleapis.com/css?family=Montserrat:300,400,700|Roboto:300,400,700"
      H.style "body { margin: 0; padding: 0; }"
    H.body $ do
      H.div H.! HA.id "redoc-container" $ mempty
      H.script H.! HA.src "https://cdn.jsdelivr.net/npm/redoc@next/bundles/redoc.standalone.js" $ mempty
      H.script $ H.preEscapedToHtml ("Redoc.init('/openapi.yaml', {}, document.getElementById('redoc-container'));" :: Text)

