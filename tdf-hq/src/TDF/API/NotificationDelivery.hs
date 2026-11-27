{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
-- | MSG-DELIVERY-001 wire contract: device push subscriptions, per-channel chat
-- notice preferences and recipient-authenticated chat read receipts.
module TDF.API.NotificationDelivery
  ( NotificationDeliveryAPI
  , PushSubscriptionRegister(..)
  , PushSubscriptionRevoke(..)
  , NotificationChannelsDTO(..)
  , ChatThreadReadRequest(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

data PushSubscriptionRegister = PushSubscriptionRegister
  { provider :: Text
  , token :: Text
  , platform :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON PushSubscriptionRegister
instance ToJSON PushSubscriptionRegister

newtype PushSubscriptionRevoke = PushSubscriptionRevoke
  { revokeToken :: Text
  } deriving (Show, Eq, Generic)
instance FromJSON PushSubscriptionRevoke where
  parseJSON = withObject "PushSubscriptionRevoke" $ \o -> PushSubscriptionRevoke <$> o .: "token"
instance ToJSON PushSubscriptionRevoke where
  toJSON (PushSubscriptionRevoke value) = object ["token" .= value]

-- | Chat notice channels. The in-app bell is always on; these control the
-- external reminders only.
data NotificationChannelsDTO = NotificationChannelsDTO
  { email :: Bool
  , push :: Bool
  } deriving (Show, Eq, Generic)
instance FromJSON NotificationChannelsDTO
instance ToJSON NotificationChannelsDTO

newtype ChatThreadReadRequest = ChatThreadReadRequest
  { lastMessageId :: Int64
  } deriving (Show, Eq, Generic)
instance FromJSON ChatThreadReadRequest
instance ToJSON ChatThreadReadRequest

type NotificationDeliveryAPI =
       "notifications" :> "push-subscriptions"
         :> ReqBody '[JSON] PushSubscriptionRegister :> Verb 'POST 204 '[JSON] NoContent
  :<|> "notifications" :> "push-subscriptions" :> "revoke"
         :> ReqBody '[JSON] PushSubscriptionRevoke :> Verb 'POST 204 '[JSON] NoContent
  :<|> "notifications" :> "channels" :> Get '[JSON] NotificationChannelsDTO
  :<|> "notifications" :> "channels"
         :> ReqBody '[JSON] NotificationChannelsDTO :> Put '[JSON] NotificationChannelsDTO
  :<|> "chat" :> "threads" :> Capture "threadId" Int64 :> "read"
         :> ReqBody '[JSON] ChatThreadReadRequest :> Verb 'POST 204 '[JSON] NoContent
