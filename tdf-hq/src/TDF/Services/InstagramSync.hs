{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Services.InstagramSync
  ( InstagramMedia(..)
  , buildUserMediaRequestUrl
  , fetchUserMedia
  ) where

import           Data.Aeson (FromJSON(..), eitherDecode, withObject, (.:), (.:?), (.!=))
import           Data.Char (isControl, isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           GHC.Generics (Generic)
import           Control.Exception (try)
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Client (Request, HttpException, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Data.ByteString.Lazy as BL

import           TDF.Config (AppConfig(..), normalizeConfiguredGraphNodeId)

data InstagramMedia = InstagramMedia
  { imId        :: Text
  , imCaption   :: Maybe Text
  , imMediaUrl  :: Maybe Text
  , imPermalink :: Maybe Text
  , imTimestamp :: Maybe UTCTime
  } deriving (Show, Generic)

instance FromJSON InstagramMedia where
  parseJSON = withObject "InstagramMedia" $ \o -> do
    imId <- o .: "id"
    imCaption <- o .:? "caption"
    imMediaUrl <- o .:? "media_url"
    imPermalink <- o .:? "permalink"
    tsTxt <- o .:? "timestamp"
    imTimestamp <- traverse iso8601ParseM tsTxt
    pure InstagramMedia{..}

newtype InstagramMediaList = InstagramMediaList [InstagramMedia]

instance FromJSON InstagramMediaList where
  parseJSON = withObject "InstagramMediaList" $ \o ->
    InstagramMediaList <$> o .:? "data" .!= []

-- | Fetch media for a given Instagram user id (or handle, if your token supports it).
fetchUserMedia :: AppConfig -> Text -> Text -> IO (Either Text [InstagramMedia])
fetchUserMedia cfg accessToken userId = do
  manager <- newManager tlsManagerSettings
  case buildUserMediaRequestUrl cfg accessToken userId of
    Left err -> pure (Left err)
    Right urlStr -> do
      reqE <- tryParse urlStr
      case reqE of
        Left err -> pure (Left err)
        Right req -> do
          respE <- try (HC.httpLbs req manager) :: IO (Either HttpException (HC.Response BL.ByteString))
          case respE of
            Left httpErr -> pure (Left (T.pack (show httpErr)))
            Right resp -> do
              let status = statusCode (HC.responseStatus resp)
              if status >= 200 && status < 300
                then case eitherDecode (HC.responseBody resp) of
                  Left decodeErr -> pure (Left (T.pack decodeErr))
                  Right (InstagramMediaList xs) -> pure (Right xs)
                else pure (Left ("HTTP " <> T.pack (show status)))
  where
    tryParse :: String -> IO (Either Text Request)
    tryParse raw = do
      res <- try (HC.parseRequest raw) :: IO (Either HttpException Request)
      pure $ case res of
        Left err -> Left (T.pack (show err))
        Right req -> Right req

buildUserMediaRequestUrl :: AppConfig -> Text -> Text -> Either Text String
buildUserMediaRequestUrl cfg rawAccessToken rawUserId = do
  userId <- normalizeGraphNodeId rawUserId
  accessToken <- normalizeGraphAccessToken rawAccessToken
  let fields = "id,caption,media_url,permalink,timestamp"
      base = T.dropWhileEnd (== '/') (instagramGraphBase cfg)
      query =
        TE.decodeUtf8 $
          renderSimpleQuery
            True
            [ ("fields", TE.encodeUtf8 fields)
            , ("access_token", TE.encodeUtf8 accessToken)
            ]
  pure (T.unpack (base <> "/" <> userId <> "/media" <> query))

normalizeGraphNodeId :: Text -> Either Text Text
normalizeGraphNodeId rawUserId =
  case normalizeConfiguredGraphNodeId "Instagram user id" (T.unpack rawUserId) of
    Left err -> Left (T.pack err)
    Right Nothing -> Left "Instagram user id is required"
    Right (Just userId) -> Right userId

normalizeGraphAccessToken :: Text -> Either Text Text
normalizeGraphAccessToken rawAccessToken
  | T.null accessToken =
      Left "Instagram access token is required"
  | T.any (\ch -> isControl ch || isSpace ch) accessToken =
      Left "Instagram access token must not contain whitespace or control characters"
  | otherwise =
      Right accessToken
  where
    accessToken = T.strip rawAccessToken
