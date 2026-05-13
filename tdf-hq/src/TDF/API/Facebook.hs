{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.API.Facebook where

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

data FacebookReplyReq = FacebookReplyReq
  { frSenderId :: Text
  , frMessage  :: Text
  , frExternalId :: Maybe Text
  } deriving (Show, Generic)

facebookReplyReqOptions :: Options
facebookReplyReqOptions =
  defaultOptions
    { fieldLabelModifier = \field ->
        case drop 2 field of
          c:rest -> toLower c : rest
          [] -> []
    , rejectUnknownFields = True
    }

instance FromJSON FacebookReplyReq where
  parseJSON = genericParseJSON facebookReplyReqOptions
instance ToJSON FacebookReplyReq where
  toJSON = genericToJSON facebookReplyReqOptions

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
  :<|> "facebook" :> "webhook"
         :> Header "X-Hub-Signature-256" Text
         :> ReqBody '[RawJSON] BL.ByteString
         :> Post '[JSON] NoContent
