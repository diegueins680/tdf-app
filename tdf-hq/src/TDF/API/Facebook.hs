{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Facebook where

import           Data.Aeson (Value)
import           Data.Text (Text)
import           Servant

type FacebookAPI =
       "facebook" :> "messages"
         :> QueryParam "limit" Int
         :> QueryParam "direction" Text
         :> QueryParam "repliedOnly" Text
         :> Get '[JSON] Value

type FacebookWebhookAPI =
       "facebook" :> "webhook"
         :> QueryParam "hub.mode" Text
         :> QueryParam "hub.verify_token" Text
         :> QueryParam "hub.challenge" Text
         :> Get '[PlainText] Text
  :<|> "facebook" :> "webhook" :> ReqBody '[JSON] Value :> Post '[JSON] NoContent
