{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.API.Facebook where

import           Data.Aeson
  ( FromJSON(..)
  , ToJSON
  , Value
  , defaultOptions
  , genericParseJSON
  , rejectUnknownFields
  )
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant

data FacebookReplyReq = FacebookReplyReq
  { frSenderId :: Text
  , frMessage  :: Text
  , frExternalId :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON FacebookReplyReq where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }
instance ToJSON FacebookReplyReq

type FacebookAPI =
       "facebook" :> "reply" :> ReqBody '[JSON] FacebookReplyReq :> Post '[JSON] Value
  :<|> "facebook" :> "messages"
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
