{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Official public metadata only. No OAuth, video downloads, HTML scraping or
-- duration-based Shorts classification. The caller owns durable checkpoints.
module TDF.Services.YouTube
  ( YouTubeKey
  , youTubeKey
  , ProviderError(..)
  , Channel(..)
  , UploadPage(..)
  , Video(..)
  , VideoResult(..)
  , Thumbnail(..)
  , fetchChannel
  , fetchUploadPage
  , fetchVideos
  , decodeChannel
  , decodeUploadPage
  , decodeVideos
  , durationSeconds
  , validVideoId
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (unless, when)
import Data.Aeson (Key, Object, Value, eitherDecode, withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (nub, sortOn)
import Data.Maybe (catMaybes, isJust)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (renderSimpleQuery)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Text.Read (readMaybe)

newtype YouTubeKey = YouTubeKey Text
instance Show YouTubeKey where show _ = "YouTubeKey <redacted>"

youTubeKey :: Text -> Either ProviderError YouTubeKey
youTubeKey raw
  | T.null key = Left MissingCredential
  | T.length key > 256 || T.any (not . idCharacter) key = Left InvalidCredential
  | otherwise = Right (YouTubeKey key)
  where key = T.strip raw

-- Never retain provider bodies, credential headers or HttpException's request.
data ProviderError = MissingCredential | InvalidCredential | InvalidIdentity
  | TransportFailure | ProviderHttp Int | UnexpectedContentType | OversizedResponse
  | InvalidResponse
  deriving (Eq, Show)

data Channel = Channel
  { channelId :: Text, channelTitle :: Text, uploadsPlaylistId :: Text
  } deriving (Eq, Show)

data UploadPage = UploadPage
  { uploadVideoIds :: [Text], uploadNextPage :: Maybe Text
  } deriving (Eq, Show)

data Thumbnail = Thumbnail
  { thumbnailUrl :: Text, thumbnailWidth :: Int, thumbnailHeight :: Int
  } deriving (Eq, Show)

data Video = Video
  { videoId :: Text
  , videoChannelId :: Text
  , videoTitle :: Text
  , videoDescription :: Text
  , videoPublishedAt :: UTCTime
  , videoDurationSeconds :: Int
  , videoThumbnails :: [Thumbnail]
  , videoEmbeddable :: Bool
  , videoCompletedLive :: Bool
  } deriving (Eq, Show)

-- Nonpublic metadata is deliberately discarded at this boundary. Missing means
-- unavailable to this public request, not proof of deletion or a removal command.
data VideoResult = PublicVideo Video | ReviewVideo Text Text
  | NonPublicVideo Text | MissingVideo Text
  deriving (Eq, Show)

idCharacter :: Char -> Bool
idCharacter c = isAsciiLower c || isAsciiUpper c || (c >= '0' && c <= '9')
  || c == '_' || c == '-'

validVideoId :: Text -> Bool
validVideoId t = T.length t == 11 && T.all idCharacter t

validChannelId :: Text -> Bool
validChannelId t = T.length t == 24 && "UC" `T.isPrefixOf` t && T.all idCharacter t

validPlaylistId :: Text -> Bool
validPlaylistId t = T.length t == 24 && "UU" `T.isPrefixOf` t && T.all idCharacter t

fetchChannel :: HTTP.Manager -> YouTubeKey -> Text -> IO (Either ProviderError Channel)
fetchChannel manager key expected
  | not (validChannelId expected) = pure (Left InvalidIdentity)
  | otherwise = fmap (>>= decodeChannel expected) $
      request manager key "channels" [("part", "snippet,contentDetails"), ("id", expected)]

fetchUploadPage :: HTTP.Manager -> YouTubeKey -> Text -> Maybe Text
  -> IO (Either ProviderError UploadPage)
fetchUploadPage manager key playlist token
  | not (validPlaylistId playlist) || maybe False (not . validToken) token =
      pure (Left InvalidIdentity)
  | otherwise = fmap (>>= decodeUploadPage) $ request manager key "playlistItems"
      ([("part", "contentDetails"), ("playlistId", playlist), ("maxResults", "50")]
        <> maybe [] (\t -> [("pageToken", t)]) token)

fetchVideos :: HTTP.Manager -> YouTubeKey -> Text -> [Text]
  -> IO (Either ProviderError [VideoResult])
fetchVideos manager key expected ids
  | not (validChannelId expected) || null ids || length ids > 50
      || any (not . validVideoId) ids || length (nub ids) /= length ids =
        pure (Left InvalidIdentity)
  | otherwise = fmap (>>= decodeVideos expected ids) $ request manager key "videos"
      [("part", "snippet,contentDetails,status,liveStreamingDetails"), ("id", T.intercalate "," ids)]

validToken :: Text -> Bool
validToken t = not (T.null t) && T.length t <= 1024 && T.all (\c -> c >= '!' && c <= '~') t

request :: HTTP.Manager -> YouTubeKey -> Text -> [(Text, Text)]
  -> IO (Either ProviderError BL.ByteString)
request manager (YouTubeKey key) endpoint query = do
  base <- HTTP.parseRequest ("https://www.googleapis.com/youtube/v3/" <> T.unpack endpoint)
  let req = base
        { HTTP.queryString = renderSimpleQuery True [(TE.encodeUtf8 k, TE.encodeUtf8 v) | (k,v) <- query]
        , HTTP.requestHeaders = [("X-Goog-Api-Key", TE.encodeUtf8 key), ("Accept", "application/json")]
        , HTTP.redirectCount = 0
        , HTTP.responseTimeout = HTTP.responseTimeoutMicro (10 * 1000000)
        , HTTP.checkResponse = \_ _ -> pure ()
        }
      attempt n = do
        result <- timeout (15 * 1000000) (try (HTTP.withResponse req manager readResponse))
          :: IO (Maybe (Either HTTP.HttpException (Either ProviderError BL.ByteString)))
        let safe = maybe (Left TransportFailure)
              (either (const (Left TransportFailure)) id) result
            retry = case safe of
              Left TransportFailure -> True
              Left (ProviderHttp status) -> status == 429 || status >= 500
              _ -> False
        if retry && n < (2 :: Int) then do
          jitter <- randomRIO (0, 250000)
          threadDelay ((2 ^ n) * 1000000 + jitter)
          attempt (n + 1)
        else pure safe
  attempt 0

readResponse :: HTTP.Response HTTP.BodyReader -> IO (Either ProviderError BL.ByteString)
readResponse response
  | status /= 200 = pure (Left (ProviderHttp status))
  | mediaType /= "application/json" = pure (Left UnexpectedContentType)
  | otherwise = readBounded 0 []
  where
    status = statusCode (HTTP.responseStatus response)
    mediaType = BS8.takeWhile (/= ';') $
      maybe "" id (lookup "Content-Type" (HTTP.responseHeaders response))
    readBounded count chunks = do
      chunk <- HTTP.brRead (HTTP.responseBody response)
      let total = count + BS.length chunk
      if total > 2 * 1024 * 1024 then pure (Left OversizedResponse)
      else if BS.null chunk then pure (Right (BL.fromChunks (reverse chunks)))
      else readBounded total (chunk : chunks)

decodeWith :: (Value -> Parser a) -> BL.ByteString -> Either ProviderError a
decodeWith parser body = case eitherDecode body >>= parseEither parser of
  Left _ -> Left InvalidResponse
  Right value -> Right value

decodeChannel :: Text -> BL.ByteString -> Either ProviderError Channel
decodeChannel expected = decodeWith $ withObject "channel response" $ \root -> do
  items <- root .: "items"
  case items of
    [item] -> withObject "channel" (\o -> do
      channelId <- o .: "id"
      unless (channelId == expected && validChannelId channelId) (fail "channel mismatch")
      snippet <- o .: "snippet"
      channelTitle <- snippet .: "title"
      details <- o .: "contentDetails"
      playlists <- details .: "relatedPlaylists"
      uploadsPlaylistId <- playlists .: "uploads"
      unless (validPlaylistId uploadsPlaylistId) (fail "invalid uploads playlist")
      pure Channel{..}) item
    _ -> fail "channel missing or ambiguous"

decodeUploadPage :: BL.ByteString -> Either ProviderError UploadPage
decodeUploadPage = decodeWith $ withObject "playlist response" $ \o -> do
  items <- o .: "items"
  uploadVideoIds <- mapM (withObject "playlist item" $ \item -> do
    details <- item .: "contentDetails"
    ident <- details .: "videoId"
    unless (validVideoId ident) (fail "invalid video identity")
    pure ident) items
  uploadNextPage <- o .:? "nextPageToken"
  unless (length items <= 50 && length (nub uploadVideoIds) == length uploadVideoIds)
    (fail "invalid page size or duplicate identity")
  unless (maybe True validToken uploadNextPage) (fail "invalid checkpoint")
  when (null items && isJust uploadNextPage) (fail "empty nonterminal page")
  pure UploadPage{..}

decodeVideos :: Text -> [Text] -> BL.ByteString -> Either ProviderError [VideoResult]
decodeVideos expected requested = decodeWith $ withObject "videos response" $ \o -> do
  items <- o .: "items"
  pairs <- mapM (withObject "video" $ \item -> do
    ident <- item .: "id"
    unless (validVideoId ident && ident `elem` requested) (fail "unexpected video")
    result <- parseVideo expected ident item
    pure (ident, result)) items
  let returned = map fst pairs
  unless (length (nub returned) == length returned) (fail "duplicate video")
  pure [maybe (MissingVideo ident) id (lookup ident pairs) | ident <- requested]

parseVideo :: Text -> Text -> Object -> Parser VideoResult
parseVideo expected ident item = do
  status <- item .: "status"
  privacy <- status .: "privacyStatus" :: Parser Text
  if privacy /= "public" then pure (NonPublicVideo ident) else do
    snippet <- item .: "snippet"
    owner <- snippet .: "channelId"
    upload <- status .: "uploadStatus" :: Parser Text
    broadcast <- snippet .: "liveBroadcastContent" :: Parser Text
    live <- item .:? "liveStreamingDetails"
    started <- maybe (pure Nothing) (.:? "actualStartTime") live :: Parser (Maybe UTCTime)
    ended <- maybe (pure Nothing) (.:? "actualEndTime") live :: Parser (Maybe UTCTime)
    if owner /= expected then pure (ReviewVideo ident "channel_mismatch")
    else if upload /= "processed" then pure (ReviewVideo ident "not_processed")
    else if broadcast /= "none" || (isJust live && not (completed started ended))
      then pure (ReviewVideo ident "live_not_completed")
    else do
      videoTitle <- snippet .: "title"
      videoDescription <- snippet .: "description"
      unless (not (T.null (T.strip videoTitle)) && T.length videoTitle <= 1000
        && T.length videoDescription <= 20000) (fail "invalid text bounds")
      videoPublishedAt <- snippet .: "publishedAt"
      details <- item .: "contentDetails"
      rawDuration <- details .: "duration"
      videoDurationSeconds <- maybe (fail "invalid duration") pure (durationSeconds rawDuration)
      thumbnails <- snippet .:? "thumbnails" .!= mempty
      videoThumbnails <- fmap (sortOn (Down . thumbnailWidth) . catMaybes) $
        mapM (parseThumbnail ident thumbnails) ["maxres", "standard", "high", "medium", "default"]
      videoEmbeddable <- status .: "embeddable"
      pure (PublicVideo Video
        { videoId = ident, videoChannelId = owner, videoCompletedLive = isJust ended, .. })
  where
    completed (Just start) (Just end) = end >= start
    completed _ _ = False

parseThumbnail :: Text -> Object -> Key -> Parser (Maybe Thumbnail)
parseThumbnail ident thumbnails size = do
  value <- thumbnails .:? size
  case value of
    Nothing -> pure Nothing
    Just o -> do
      thumbnailUrl <- o .: "url"
      thumbnailWidth <- o .: "width"
      thumbnailHeight <- o .: "height"
      let prefixes = ["https://i.ytimg.com/vi/", "https://i.ytimg.com/vi_webp/"]
          belongs = any (\prefix -> (prefix <> ident <> "/") `T.isPrefixOf` thumbnailUrl) prefixes
      pure $ if belongs && thumbnailWidth > 0 && thumbnailHeight > 0
        && thumbnailWidth <= 16384 && thumbnailHeight <= 16384
        then Just Thumbnail{..} else Nothing

-- YouTube documents ISO 8601 durations. Parse ordered day/hour/minute/second
-- components without treating a short duration as proof of the Shorts format.
durationSeconds :: Text -> Maybe Int
durationSeconds raw = do
  rest <- T.stripPrefix "P" raw
  let (days, time) = T.breakOn "T" rest
  d <- if T.null days then Just 0 else component "D" days
  clock <- if T.null time then Just "" else T.stripPrefix "T" time
  (h, afterH) <- optionalComponent "H" clock
  (m, afterM) <- optionalComponent "M" afterH
  s <- if T.null afterM then Just 0 else component "S" afterM
  let total = d * 86400 + h * 3600 + m * 60 + s :: Integer
  if raw `elem` ["P", "PT"] || total <= 0 || total > fromIntegral (maxBound :: Int) `div` 1000
    then Nothing else Just (fromIntegral total)
  where
    component unit t = do
      digits <- T.stripSuffix unit t
      if T.null digits || not (T.all (\c -> c >= '0' && c <= '9') digits)
        then Nothing else readMaybe (T.unpack digits)
    optionalComponent unit t =
      let (prefix, suffix) = T.breakOn unit t in
      if T.null suffix then Just (0, t)
      else do value <- component unit (prefix <> unit)
              pure (value, T.drop 1 suffix)
