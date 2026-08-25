{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DTO.SocialSyncDTO where

import           Control.Monad (when)
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), defaultOptions, genericParseJSON, genericToJSON, withObject)
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import           Data.Aeson.Types (Options(..), Parser)
import           Data.Char (toLower)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
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
  parseJSON value = do
    rejectNullSocialSyncOptionalFields
      "SocialSyncPostIn"
      [ "caption"
      , "permalink"
      , "mediaUrls"
      , "postedAt"
      , "artistPartyId"
      , "artistProfileId"
      , "ingestSource"
      , "likeCount"
      , "commentCount"
      , "shareCount"
      , "viewCount"
      ]
      value
    post <- genericParseJSON defaultOptions
      { fieldLabelModifier = camelDrop 3
      , rejectUnknownFields = True
      } value
    platform <- validateRequiredIdentityField "platform" (sspPlatform post)
    externalPostId <- validateRequiredIdentityField "externalPostId" (sspExternalPostId post)
    validateOptionalMetricCount "likeCount" (sspLikeCount post)
    validateOptionalMetricCount "commentCount" (sspCommentCount post)
    validateOptionalMetricCount "shareCount" (sspShareCount post)
    validateOptionalMetricCount "viewCount" (sspViewCount post)
    pure post
      { sspPlatform = platform
      , sspExternalPostId = externalPostId
      }

validateRequiredIdentityField :: String -> Text -> Parser Text
validateRequiredIdentityField fieldName rawValue =
  let value = T.strip rawValue
  in if T.null value
       then fail (fieldName <> " must not be blank")
       else pure value

rejectNullSocialSyncOptionalFields :: String -> [Text] -> Value -> Parser ()
rejectNullSocialSyncOptionalFields objectName fieldNames =
  withObject objectName $ \o ->
    let rejectNullField fieldName =
          case AKM.lookup (AKey.fromText fieldName) o of
            Just Null -> fail (T.unpack fieldName <> " must be omitted instead of null")
            _         -> pure ()
    in mapM_ rejectNullField fieldNames

data SocialSyncIngestRequest = SocialSyncIngestRequest
  { ssirPosts :: [SocialSyncPostIn]
  } deriving (Show, Generic)

maxSocialSyncIngestPosts :: Int
maxSocialSyncIngestPosts = 500

instance FromJSON SocialSyncIngestRequest where
  parseJSON value = do
    request <- genericParseJSON defaultOptions
      { fieldLabelModifier = camelDrop 4
      , rejectUnknownFields = True
      } value
    when (null (ssirPosts request)) $
      fail "posts must contain at least one post"
    when (length (ssirPosts request) > maxSocialSyncIngestPosts) $
      fail ("posts must contain at most " <> show maxSocialSyncIngestPosts <> " posts")
    validateUniquePostIdentities (ssirPosts request)
    pure request

validateUniquePostIdentities :: [SocialSyncPostIn] -> Parser ()
validateUniquePostIdentities = go Set.empty
  where
    go _ [] = pure ()
    go seen (post : rest) =
      case postIdentityKey post of
        Nothing -> go seen rest
        Just key
          | Set.member key seen ->
              fail "posts must not contain duplicate platform/externalPostId pairs"
          | otherwise ->
              go (Set.insert key seen) rest

    postIdentityKey post =
      let platformKey = T.toLower (T.strip (sspPlatform post))
          externalPostKey = T.strip (sspExternalPostId post)
      in if T.null platformKey || T.null externalPostKey
           then Nothing
           else Just (platformKey, externalPostKey)

validateOptionalMetricCount :: String -> Maybe Int -> Parser ()
validateOptionalMetricCount _ Nothing = pure ()
validateOptionalMetricCount fieldName (Just metricCount) =
  when (metricCount < 0) $
    fail (fieldName <> " must be greater than or equal to 0")

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
