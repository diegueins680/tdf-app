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

data WAReferral = WAReferral
  { sourceId   :: Maybe Text
  , sourceType :: Maybe Text
  , sourceUrl  :: Maybe Text
  , headline   :: Maybe Text
  , waBody     :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON WAReferral where
  parseJSON = withObject "WAReferral" $ \o -> do
    sourceIdVal <- o .:? "source_id"
    sourceTypeVal <- o .:? "source_type"
    sourceUrlVal <- o .:? "source_url"
    headlineVal <- o .:? "headline"
    waBodyVal <- o .:? "body"
    pure WAReferral
      { sourceId = sourceIdVal
      , sourceType = sourceTypeVal
      , sourceUrl = sourceUrlVal
      , headline = headlineVal
      , waBody = waBodyVal
      }

data WAContext = WAContext
  { waContextReferral :: Maybe WAReferral
  } deriving (Show, Generic)
instance FromJSON WAContext where
  parseJSON = withObject "WAContext" $ \o -> do
    referralVal <- o .:? "referral"
    pure WAContext { waContextReferral = referralVal }

data WAMessage = WAMessage
  { waId  :: Maybe Text
  , waType :: Text
  , from  :: Text
  , text  :: Maybe WAText
  , waReferral :: Maybe WAReferral
  , waContext  :: Maybe WAContext
  , waTimestamp :: Maybe Text
  } deriving (Show)

instance FromJSON WAMessage where
  parseJSON = withObject "WAMessage" $ \o -> do
    i  <- o .:? "id"
    t  <- o .:  "type"
    f  <- o .:  "from"
    mt <- o .:? "text"
    ref <- o .:? "referral"
    ctx <- o .:? "context"
    ts  <- o .:? "timestamp"
    pure (WAMessage i t f mt ref ctx ts)

data WAValue = WAValue
  { messages :: Maybe [WAMessage]
  } deriving (Show, Generic)
instance FromJSON WAValue
