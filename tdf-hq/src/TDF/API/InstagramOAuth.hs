{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.InstagramOAuth where

import           Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, omitNothingFields)
import           Data.Char (toLower)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Servant

camelDrop :: Int -> String -> String
camelDrop n xs = case drop n xs of
  (c:cs) -> toLower c : cs
  []     -> []

data InstagramOAuthExchangeRequest = InstagramOAuthExchangeRequest
  { ioeCode        :: Text
  , ioeRedirectUri :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON InstagramOAuthExchangeRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data InstagramOAuthPage = InstagramOAuthPage
  { iopPageId              :: Text
  , iopPageName            :: Text
  , iopInstagramUserId     :: Maybe Text
  , iopInstagramUsername   :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON InstagramOAuthPage where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3, omitNothingFields = True }

data InstagramMediaDTO = InstagramMediaDTO
  { imdId        :: Text
  , imdCaption   :: Maybe Text
  , imdMediaUrl  :: Maybe Text
  , imdPermalink :: Maybe Text
  , imdTimestamp :: Maybe UTCTime
  } deriving (Show, Generic)

instance ToJSON InstagramMediaDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3, omitNothingFields = True }

data InstagramOAuthExchangeResponse = InstagramOAuthExchangeResponse
  { ioeUserId            :: Text
  , ioeUserName          :: Maybe Text
  , ioeTokenType         :: Text
  , ioeExpiresIn         :: Int
  , ioePages             :: [InstagramOAuthPage]
  , ioeInstagramUserId   :: Maybe Text
  , ioeInstagramUsername :: Maybe Text
  , ioeMedia             :: [InstagramMediaDTO]
  } deriving (Show, Generic)

instance ToJSON InstagramOAuthExchangeResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3, omitNothingFields = True }

type InstagramOAuthAPI =
  "instagram" :> "oauth" :>
    "exchange" :> ReqBody '[JSON] InstagramOAuthExchangeRequest :> Post '[JSON] InstagramOAuthExchangeResponse
