{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.InstagramOAuth where

import           Data.Aeson (FromJSON(..), Options(rejectUnknownFields), ToJSON(..),
                             defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON,
                             omitNothingFields)
import           Data.Char (toLower)
import           Data.Text (Text)
import qualified Data.Text as T
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
  parseJSON value = do
    request <- genericParseJSON strictObjectOptions value
    code <- maybe (fail "code cannot be blank") pure (nonEmptyText (ioeCode request))
    pure request
      { ioeCode = code
      , ioeRedirectUri = ioeRedirectUri request >>= nonEmptyText
      }

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

strictObjectOptions :: Options
strictObjectOptions =
  defaultOptions
    { fieldLabelModifier = camelDrop 3
    , rejectUnknownFields = True
    }

nonEmptyText :: Text -> Maybe Text
nonEmptyText rawText =
  let trimmed = T.strip rawText
  in if T.null trimmed then Nothing else Just trimmed
