{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Facebook where

import           Data.Aeson
  ( FromJSON(..)
  , Object
  , Options
  , ToJSON(..)
  , Value
  , defaultOptions
  , fieldLabelModifier
  , genericToJSON
  , withObject
  , (.:)
  , (.:?)
  )
import qualified Data.Aeson.Key    as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import           Data.Aeson.Types  (Parser)
import           Data.Char (toLower)
import           Data.Text (Text)
import qualified Data.Text as T
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
    }

instance FromJSON FacebookReplyReq where
  parseJSON = parseFacebookReplyReq
instance ToJSON FacebookReplyReq where
  toJSON = genericToJSON facebookReplyReqOptions

parseFacebookReplyReq :: Value -> Parser FacebookReplyReq
parseFacebookReplyReq =
  withObject "FacebookReplyReq" $ \obj -> do
    rejectUnknownFacebookReplyKeys obj
    let hasCanonical = hasAnyKey facebookCanonicalReplyKeys obj
        hasLegacy = hasAnyKey facebookLegacyReplyKeys obj
    if hasCanonical && hasLegacy
      then fail "Use either canonical Facebook reply keys or legacy fr* keys, not both"
      else do
        let (senderKey, messageKey, externalKey) =
              if hasLegacy
                then ("frSenderId", "frMessage", "frExternalId")
                else ("senderId", "message", "externalId")
        FacebookReplyReq
          <$> obj .: AesonKey.fromText senderKey
          <*> obj .: AesonKey.fromText messageKey
          <*> obj .:? AesonKey.fromText externalKey

rejectUnknownFacebookReplyKeys :: Object -> Parser ()
rejectUnknownFacebookReplyKeys obj =
  case filter (`notElem` allowedKeys) providedKeys of
    [] -> pure ()
    unexpected ->
      fail ("Unexpected FacebookReplyReq keys: " <> show (map T.unpack unexpected))
  where
    allowedKeys = facebookCanonicalReplyKeys <> facebookLegacyReplyKeys
    providedKeys = map AesonKey.toText (AesonKeyMap.keys obj)

facebookCanonicalReplyKeys :: [Text]
facebookCanonicalReplyKeys =
  [ "senderId"
  , "message"
  , "externalId"
  ]

facebookLegacyReplyKeys :: [Text]
facebookLegacyReplyKeys =
  [ "frSenderId"
  , "frMessage"
  , "frExternalId"
  ]

hasAnyKey :: [Text] -> Object -> Bool
hasAnyKey keys obj =
  any (flip AesonKeyMap.member obj . AesonKey.fromText) keys

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
