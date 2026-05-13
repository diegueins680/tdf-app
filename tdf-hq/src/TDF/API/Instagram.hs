{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Instagram where

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
    }

instance FromJSON InstagramReplyReq where
  parseJSON = parseInstagramReplyReq
instance ToJSON InstagramReplyReq where
  toJSON = genericToJSON instagramReplyReqOptions

parseInstagramReplyReq :: Value -> Parser InstagramReplyReq
parseInstagramReplyReq =
  withObject "InstagramReplyReq" $ \obj -> do
    rejectUnknownInstagramReplyKeys obj
    let hasCanonical = hasAnyKey instagramCanonicalReplyKeys obj
        hasLegacy = hasAnyKey instagramLegacyReplyKeys obj
    if hasCanonical && hasLegacy
      then fail "Use either canonical Instagram reply keys or legacy ir* keys, not both"
      else do
        let (senderKey, messageKey, externalKey) =
              if hasLegacy
                then ("irSenderId", "irMessage", "irExternalId")
                else ("senderId", "message", "externalId")
        InstagramReplyReq
          <$> obj .: AesonKey.fromText senderKey
          <*> obj .: AesonKey.fromText messageKey
          <*> obj .:? AesonKey.fromText externalKey

rejectUnknownInstagramReplyKeys :: Object -> Parser ()
rejectUnknownInstagramReplyKeys obj =
  case filter (`notElem` allowedKeys) providedKeys of
    [] -> pure ()
    unexpected ->
      fail ("Unexpected InstagramReplyReq keys: " <> show (map T.unpack unexpected))
  where
    allowedKeys = instagramCanonicalReplyKeys <> instagramLegacyReplyKeys
    providedKeys = map AesonKey.toText (AesonKeyMap.keys obj)

instagramCanonicalReplyKeys :: [Text]
instagramCanonicalReplyKeys =
  [ "senderId"
  , "message"
  , "externalId"
  ]

instagramLegacyReplyKeys :: [Text]
instagramLegacyReplyKeys =
  [ "irSenderId"
  , "irMessage"
  , "irExternalId"
  ]

hasAnyKey :: [Text] -> Object -> Bool
hasAnyKey keys obj =
  any (flip AesonKeyMap.member obj . AesonKey.fromText) keys

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
