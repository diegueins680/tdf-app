{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TDF.API.Production (ProductionAPI) where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.UUID (UUID)
import Servant

type ProductionAPI = "productions" :>
  ( Get '[JSON] Value
  :<|> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> Capture "project" UUID :> "actions" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> Capture "project" UUID :> "versions" :> Capture "version" UUID :> "audio"
     :> Get '[OctetStream] (Headers '[Header "Cache-Control" Text, Header "X-Content-Type-Options" Text] BL.ByteString)
  )

