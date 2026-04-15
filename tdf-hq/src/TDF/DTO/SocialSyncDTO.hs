{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DTO.SocialSyncDTO where

import           Control.Monad (when)
import           Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericParseJSON, genericToJSON)
import           Data.Aeson.Types (Options(..))
import           Data.Char (toLower)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)

camelDrop :: Int -> String -> String
camelDrop n xs = case drop n xs of
  (c:cs) -> toLower c : cs
  []     -> []

data SocialSyncPostIn = SocialSyncPostIn
  { sspPlatform        :: Text
  , sspExternalPostId  :: Text
  , sspCaption         :: Maybe Text
  , sspPermalink       :: Maybe Text
  , sspMediaUrls       :: Maybe [Text]
  , sspPostedAt        :: Maybe UTCTime
  , sspArtistPartyId   :: Maybe Text
  , sspArtistProfileId :: Maybe Text
  , sspIngestSource    :: Maybe Text
  , sspLikeCount       :: Maybe Int
  , sspCommentCount    :: Maybe Int
  , sspShareCount      :: Maybe Int
  , sspViewCount       :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON SocialSyncPostIn where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelDrop 3
    , rejectUnknownFields = True
    }

data SocialSyncIngestRequest = SocialSyncIngestRequest
  { ssirPosts :: [SocialSyncPostIn]
  } deriving (Show, Generic)

instance FromJSON SocialSyncIngestRequest where
  parseJSON value = do
    request <- genericParseJSON defaultOptions
      { fieldLabelModifier = camelDrop 4
      , rejectUnknownFields = True
      } value
    when (null (ssirPosts request)) $
      fail "posts must contain at least one post"
    pure request

data SocialSyncIngestResponse = SocialSyncIngestResponse
  { ssirInserted :: Int
  , ssirUpdated  :: Int
  , ssirTotal    :: Int
  } deriving (Show, Generic)

instance ToJSON SocialSyncIngestResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 4 }

data SocialSyncMetricsDTO = SocialSyncMetricsDTO
  { ssmLikes    :: Maybe Int
  , ssmComments :: Maybe Int
  , ssmShares   :: Maybe Int
  , ssmViews    :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON SocialSyncMetricsDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3, omitNothingFields = True }

data SocialSyncPostDTO = SocialSyncPostDTO
  { sspdId              :: Text
  , sspdPlatform        :: Text
  , sspdExternalPostId  :: Text
  , sspdArtistPartyId   :: Maybe Text
  , sspdArtistProfileId :: Maybe Text
  , sspdCaption         :: Maybe Text
  , sspdPermalink       :: Maybe Text
  , sspdMediaUrls       :: [Text]
  , sspdPostedAt        :: Maybe UTCTime
  , sspdFetchedAt       :: UTCTime
  , sspdSummary         :: Maybe Text
  , sspdTags            :: [Text]
  , sspdIngestSource    :: Text
  , sspdMetrics         :: SocialSyncMetricsDTO
  } deriving (Show, Generic)

instance ToJSON SocialSyncPostDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 4, omitNothingFields = True }
