{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.WhatsApp.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data WAMetaWebhook = WAMetaWebhook
  { entry :: [WAEntry]
  } deriving (Show, Generic)
instance FromJSON WAMetaWebhook

data WAEntry = WAEntry
  { changes :: [WAChange]
  } deriving (Show, Generic)
instance FromJSON WAEntry

data WAChange = WAChange
  { value :: WAValue
  } deriving (Show, Generic)
instance FromJSON WAChange

data WAText = WAText
  { body :: Text
  } deriving (Show, Generic)
instance FromJSON WAText

data WAMessage = WAMessage
  { waId  :: Maybe Text
  , waType :: Text
  , from  :: Text
  , text  :: Maybe WAText
  } deriving (Show)

instance FromJSON WAMessage where
  parseJSON = withObject "WAMessage" $ \o -> do
    i  <- o .:? "id"
    t  <- o .:  "type"
    f  <- o .:  "from"
    mt <- o .:? "text"
    pure (WAMessage i t f mt)

data WAValue = WAValue
  { messages :: Maybe [WAMessage]
  } deriving (Show, Generic)
instance FromJSON WAValue
