{-# LANGUAGE OverloadedStrings #-}

module TDF.App.StartupResponse (startupApp) where

import Data.Aeson (encode, object, (.=))
import Data.Text (Text)
import Network.HTTP.Types (status503)
import Network.Wai (Application, pathInfo, responseLBS)

-- A listening socket is not readiness: the database and runtime registries
-- must finish initialization before the load balancer admits user traffic.
startupApp :: Application
startupApp req send =
  send $ responseLBS status503 headers $ encode $ object $
    ["message" .= ("El servicio está arrancando. Intenta de nuevo en unos segundos." :: Text)]
      ++ if pathInfo req == ["health"]
           then ["status" .= ("starting" :: Text), "db" .= ("starting" :: Text)]
           else ["error" .= ("starting" :: Text)]
  where
    headers =
      [ ("Content-Type", "application/json; charset=utf-8")
      , ("Cache-Control", "no-store")
      , ("Retry-After", "5")
      ]
