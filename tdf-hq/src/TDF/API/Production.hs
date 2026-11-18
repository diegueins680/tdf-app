{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TDF.API.Production (ProductionAPI, ProductionAudio) where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.UUID (UUID)
import Servant

type ProductionAudio = Headers '[Header "Cache-Control" Text, Header "X-Content-Type-Options" Text] BL.ByteString

type ProductionAPI = "productions" :>
  ( Get '[JSON] (Headers '[Header "Cache-Control" Text] Value)
  :<|> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> Capture "project" UUID :> "actions" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> Capture "project" UUID :> "versions" :> Capture "version" UUID :> "audio"
     :> Get '[OctetStream] ProductionAudio
  )

