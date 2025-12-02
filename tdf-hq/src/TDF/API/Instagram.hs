{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.API.Instagram where

import           Data.Aeson (Value)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant

data InstagramReplyReq = InstagramReplyReq
  { irSenderId :: Text
  , irMessage  :: Text
  } deriving (Show, Generic)

instance FromJSON InstagramReplyReq
instance ToJSON InstagramReplyReq

type InstagramAPI =
       "instagram" :> "webhook" :> ReqBody '[JSON] Value :> Post '[JSON] NoContent
  :<|> "instagram" :> "reply"   :> ReqBody '[JSON] InstagramReplyReq :> Post '[JSON] Value
  :<|> "instagram" :> "messages" :> Get '[JSON] Value
