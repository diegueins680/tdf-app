{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Services.InstagramSync
  ( InstagramMedia(..)
  , fetchUserMedia
  ) where

import           Data.Aeson (FromJSON(..), eitherDecode, withObject, (.:), (.:?), (.!=))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           GHC.Generics (Generic)
import           Control.Exception (try)
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Client (Request, HttpException)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as BL

import           TDF.Config (AppConfig(..))

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

newtype InstagramMediaList = InstagramMediaList { imlData :: [InstagramMedia] }

instance FromJSON InstagramMediaList where
  parseJSON = withObject "InstagramMediaList" $ \o ->
    InstagramMediaList <$> o .:? "data" .!= []

-- | Fetch media for a given Instagram user id (or handle, if your token supports it).
fetchUserMedia :: AppConfig -> Text -> Text -> IO (Either Text [InstagramMedia])
fetchUserMedia cfg accessToken userId = do
  manager <- HC.newManager tlsManagerSettings
  let fields = "id,caption,media_url,permalink,timestamp"
      urlTxt = instagramGraphBase cfg <> "/" <> userId <> "/media?fields=" <> fields <> "&access_token=" <> accessToken
      urlStr = T.unpack urlTxt
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
