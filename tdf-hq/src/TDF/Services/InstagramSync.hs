{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Services.InstagramSync
  ( InstagramMedia(..)
  , InstagramMediaList(..)
  , buildUserMediaRequestUrl
  , fetchUserMedia
  ) where

import           Control.Exception (try)
import           Control.Monad (when)
import           Data.Aeson (FromJSON(..), eitherDecode, withObject, (.:), (.:?))
import           Data.Aeson.Types (Parser)
import           Data.Char (GeneralCategory(Format), generalCategory, isControl, isSpace)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           GHC.Generics (Generic)
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Client (Request, HttpException)
import           TDF.DB (sharedTlsManager)
import           Network.HTTP.Types.Status (statusCode)
import           Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Data.ByteString.Lazy as BL

import           TDF.Config
  ( AppConfig(..)
  , normalizeConfiguredGraphNodeId
  , normalizeConfiguredHttpsUrl
  )

data InstagramMedia = InstagramMedia
  { imId        :: Text
  , imCaption   :: Maybe Text
  , imMediaUrl  :: Maybe Text
  , imPermalink :: Maybe Text
  , imTimestamp :: Maybe UTCTime
  } deriving (Show, Generic)

instance FromJSON InstagramMedia where
  parseJSON = withObject "InstagramMedia" $ \o -> do
    imId <- validateInstagramMediaId =<< o .: "id"
    imCaption <- o .:? "caption"
    imMediaUrl <- traverse (validateInstagramMediaUrl "media_url") =<< o .:? "media_url"
    imPermalink <- traverse (validateInstagramMediaUrl "permalink") =<< o .:? "permalink"
    tsTxt <- o .:? "timestamp"
    imTimestamp <- traverse iso8601ParseM tsTxt
    pure InstagramMedia{..}

validateInstagramMediaId :: Text -> Parser Text
validateInstagramMediaId rawMediaId
  | T.null mediaId =
      fail "Instagram media id is required"
  | T.length mediaId > 256 =
      fail "Instagram media id must be 256 characters or fewer"
  | T.any (\ch -> isSpace ch || isControl ch) mediaId =
      fail "Instagram media id must not contain whitespace or control characters"
  | T.any isHiddenInstagramFormattingChar mediaId =
      fail "Instagram media id must not contain hidden formatting characters"
  | T.any (`elem` ("/?#" :: String)) mediaId =
      fail "Instagram media id must not contain path or query delimiters"
  | otherwise =
      pure mediaId
  where
    mediaId = T.strip rawMediaId

validateInstagramMediaUrl :: Text -> Text -> Parser Text
validateInstagramMediaUrl fieldName rawUrl
  | T.null url =
      fail (T.unpack fieldName <> " must not be blank")
  | T.length url > 2048 =
      fail (T.unpack fieldName <> " must be 2048 characters or fewer")
  | T.any (\ch -> isSpace ch || isControl ch) url =
      fail (T.unpack fieldName <> " must not contain whitespace or control characters")
  | T.any isHiddenInstagramFormattingChar url =
      fail (T.unpack fieldName <> " must not contain hidden formatting characters")
  | not (isPublicHttpsUrl fieldName url) =
      fail (T.unpack fieldName <> " must be an absolute public https URL")
  | fieldName == "permalink" && not (isInstagramPermalinkUrl url) =
      fail "permalink must be an Instagram URL"
  | otherwise =
      pure url
  where
    url = T.strip rawUrl

isHiddenInstagramFormattingChar :: Char -> Bool
isHiddenInstagramFormattingChar ch =
  generalCategory ch == Format

isPublicHttpsUrl :: Text -> Text -> Bool
isPublicHttpsUrl fieldName url =
  case normalizeConfiguredHttpsUrl (T.unpack fieldName) (T.unpack url) of
    Right (Just normalized) -> normalized == url
    _ -> False

isInstagramPermalinkUrl :: Text -> Bool
isInstagramPermalinkUrl url =
  host `elem` ["instagram.com", "www.instagram.com"] && T.null portSuffix
  where
    remainder = T.drop 8 url
    authority = T.takeWhile (`notElem` ("/?#" :: String)) remainder
    (rawHost, portSuffix) = T.breakOn ":" authority
    host = T.toLower rawHost

newtype InstagramMediaList = InstagramMediaList [InstagramMedia]

instance FromJSON InstagramMediaList where
  parseJSON = withObject "InstagramMediaList" $ \o -> do
    media <- o .: "data"
    let mediaIds = map imId media
    when (length mediaIds /= Set.size (Set.fromList mediaIds)) $
      fail "Instagram media list must not contain duplicate media ids"
    pure (InstagramMediaList media)

-- | Fetch media for a given Instagram user id (or handle, if your token supports it).
fetchUserMedia :: AppConfig -> Text -> Text -> IO (Either Text [InstagramMedia])
fetchUserMedia cfg accessToken userId = do
  manager <- pure sharedTlsManager
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
  | T.length accessToken > maxInstagramGraphAccessTokenChars =
      Left "Instagram access token must be 4096 characters or fewer"
  | T.any (\ch -> isControl ch || isSpace ch) accessToken =
      Left "Instagram access token must not contain whitespace or control characters"
  | T.any isHiddenInstagramFormattingChar accessToken =
      Left "Instagram access token must not contain hidden formatting characters"
  | T.any (not . isVisibleAsciiTokenChar) accessToken =
      Left "Instagram access token must contain visible ASCII characters only"
  | otherwise =
      Right accessToken
  where
    accessToken = T.strip rawAccessToken

maxInstagramGraphAccessTokenChars :: Int
maxInstagramGraphAccessTokenChars = 4096

isVisibleAsciiTokenChar :: Char -> Bool
isVisibleAsciiTokenChar ch =
  ch >= '!' && ch <= '~'
