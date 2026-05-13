{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.API.Instagram where

import           Data.Aeson
  ( FromJSON(..)
  , Options
  , ToJSON(..)
  , Value
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , rejectUnknownFields
  )
import           Data.Char (toLower)
import           Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import           GHC.Generics (Generic)
import           Servant

import           TDF.API.Types (RawJSON)

data InstagramReplyReq = InstagramReplyReq
  { irSenderId :: Text
  , irMessage  :: Text
  , irExternalId :: Maybe Text
  } deriving (Show, Generic)

instagramReplyReqOptions :: Options
instagramReplyReqOptions =
  defaultOptions
    { fieldLabelModifier = \field ->
        case drop 2 field of
          c:rest -> toLower c : rest
          [] -> []
    , rejectUnknownFields = True
    }

instance FromJSON InstagramReplyReq where
  parseJSON = genericParseJSON instagramReplyReqOptions
instance ToJSON InstagramReplyReq where
  toJSON = genericToJSON instagramReplyReqOptions

type InstagramAPI =
       "instagram" :> "reply"   :> ReqBody '[JSON] InstagramReplyReq :> Post '[JSON] Value
  :<|> "instagram" :> "messages"
         :> QueryParam "limit" Int
         :> QueryParam "direction" Text
         :> QueryParam "repliedOnly" Text
         :> Get '[JSON] Value

type InstagramWebhookAPI =
       "instagram" :> "webhook"
         :> QueryParam "hub.mode" Text
         :> QueryParam "hub.verify_token" Text
         :> QueryParam "hub.challenge" Text
         :> Get '[PlainText] Text
  :<|> "instagram" :> "webhook"
         :> Header "X-Hub-Signature-256" Text
         :> ReqBody '[RawJSON] BL.ByteString
         :> Post '[JSON] NoContent
