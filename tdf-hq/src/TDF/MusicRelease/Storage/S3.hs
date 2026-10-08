{-# LANGUAGE OverloadedStrings #-}

module TDF.MusicRelease.Storage.S3
  ( S3SigningConfig(..)
  , S3SigningError(..)
  , presignGetObject
  , presignCreateMultipart
  , presignUploadPart
  , presignCompleteMultipart
  , presignAbortMultipart
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (convert)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Word (Word8)

data S3SigningConfig = S3SigningConfig
  { s3Endpoint :: Text
  , s3Region :: Text
  , s3AccessKeyId :: Text
  , s3SecretAccessKey :: Text
  , s3SessionToken :: Maybe Text
  } deriving (Eq, Show)

data S3SigningError
  = InvalidEndpoint
  | InvalidBucket
  | InvalidObjectKey
  | InvalidExpiry
  | InvalidCredential
  deriving (Eq, Show)

presignGetObject
  :: S3SigningConfig
  -> UTCTime
  -> Int
  -> Text
  -> Text
  -> Either S3SigningError Text
presignGetObject config now expirySeconds bucket objectKey = do
  presignObject config now expirySeconds "GET" bucket objectKey []

presignCreateMultipart :: S3SigningConfig -> UTCTime -> Int -> Text -> Text -> Either S3SigningError Text
presignCreateMultipart config now expirySeconds bucket objectKey =
  presignObject config now expirySeconds "POST" bucket objectKey [("uploads", "")]

presignUploadPart :: S3SigningConfig -> UTCTime -> Int -> Text -> Text -> Text -> Int -> Either S3SigningError Text
presignUploadPart config now expirySeconds bucket objectKey uploadId partNumber
  | partNumber < 1 || partNumber > 10000 = Left InvalidObjectKey
  | not (validUploadId uploadId) = Left InvalidObjectKey
  | otherwise = presignObject config now expirySeconds "PUT" bucket objectKey
      [("partNumber", T.pack (show partNumber)), ("uploadId", uploadId)]

presignCompleteMultipart :: S3SigningConfig -> UTCTime -> Int -> Text -> Text -> Text -> Either S3SigningError Text
presignCompleteMultipart config now expirySeconds bucket objectKey uploadId
  | not (validUploadId uploadId) = Left InvalidObjectKey
  | otherwise = presignObject config now expirySeconds "POST" bucket objectKey [("uploadId", uploadId)]

presignAbortMultipart :: S3SigningConfig -> UTCTime -> Int -> Text -> Text -> Text -> Either S3SigningError Text
presignAbortMultipart config now expirySeconds bucket objectKey uploadId
  | not (validUploadId uploadId) = Left InvalidObjectKey
  | otherwise = presignObject config now expirySeconds "DELETE" bucket objectKey [("uploadId", uploadId)]

presignObject
  :: S3SigningConfig -> UTCTime -> Int -> Text -> Text -> Text -> [(Text, Text)]
  -> Either S3SigningError Text
presignObject config now expirySeconds method bucket objectKey operationParameters = do
  host <- endpointHost (s3Endpoint config)
  validateConfig config
  if expirySeconds < 1 || expirySeconds > 900 then Left InvalidExpiry else Right ()
  if validBucket bucket then Right () else Left InvalidBucket
  if validObjectKey objectKey then Right () else Left InvalidObjectKey
  let timestamp = formatUtc "%Y%m%dT%H%M%SZ" now
      dateStamp = formatUtc "%Y%m%d" now
      scope = dateStamp <> "/" <> s3Region config <> "/s3/aws4_request"
      credential = s3AccessKeyId config <> "/" <> scope
      canonicalUri = "/" <> encodeSegment bucket <> "/" <> T.intercalate "/" (map encodeSegment (T.splitOn "/" objectKey))
      baseParameters =
        [ ("X-Amz-Algorithm", "AWS4-HMAC-SHA256")
        , ("X-Amz-Credential", credential)
        , ("X-Amz-Date", timestamp)
        , ("X-Amz-Expires", T.pack (show expirySeconds))
        , ("X-Amz-SignedHeaders", "host")
        ]
      parametersWithOperation = operationParameters <> baseParameters
      parameters = maybe parametersWithOperation (\token -> ("X-Amz-Security-Token", token) : parametersWithOperation) (s3SessionToken config)
      canonicalQuery = queryText parameters
      canonicalRequest = T.intercalate "\n"
        [ method, canonicalUri, canonicalQuery, "host:" <> T.toLower host <> "\n", "host", "UNSIGNED-PAYLOAD" ]
      stringToSign = T.intercalate "\n"
        [ "AWS4-HMAC-SHA256", timestamp, scope, sha256Hex (TE.encodeUtf8 canonicalRequest) ]
      signingKey = hmacBytes
        (hmacBytes
          (hmacBytes
            (hmacBytes (TE.encodeUtf8 ("AWS4" <> s3SecretAccessKey config)) (TE.encodeUtf8 dateStamp))
            (TE.encodeUtf8 (s3Region config)))
          "s3")
        "aws4_request"
      signature = hexBytes (hmacBytes signingKey (TE.encodeUtf8 stringToSign))
      endpoint = T.dropWhileEnd (== '/') (T.strip (s3Endpoint config))
  pure (endpoint <> canonicalUri <> "?" <> canonicalQuery <> "&X-Amz-Signature=" <> signature)

validUploadId :: Text -> Bool
validUploadId uploadId =
  not (T.null uploadId) && T.length uploadId <= 1024
    && T.all (\character -> character >= '!' && character <= '~' && character `notElem` ("&#" :: String)) uploadId

validateConfig :: S3SigningConfig -> Either S3SigningError ()
validateConfig config
  | any (T.null . T.strip) [s3Region config, s3AccessKeyId config, s3SecretAccessKey config] = Left InvalidCredential
  | T.length (s3AccessKeyId config) > 256 || T.length (s3SecretAccessKey config) > 512 = Left InvalidCredential
  | maybe False (\token -> T.null (T.strip token) || T.length token > 4096) (s3SessionToken config) = Left InvalidCredential
  | otherwise = Right ()

endpointHost :: Text -> Either S3SigningError Text
endpointHost rawEndpoint =
  case T.stripPrefix "https://" (T.strip rawEndpoint) of
    Just authority
      | not (T.null authority)
      , not (T.any (`elem` ("/?#@" :: String)) authority)
      , T.all validHostCharacter authority -> Right authority
    _ -> Left InvalidEndpoint
  where
    validHostCharacter character =
      (character >= 'A' && character <= 'Z')
        || (character >= 'a' && character <= 'z')
        || (character >= '0' && character <= '9')
        || character `elem` (".-:" :: String)

validBucket :: Text -> Bool
validBucket bucket =
  T.length bucket >= 3 && T.length bucket <= 63
    && T.head bucket /= '.' && T.last bucket /= '.'
    && T.all (\character ->
      (character >= 'a' && character <= 'z')
        || (character >= '0' && character <= '9')
        || character `elem` (".-" :: String)) bucket

validObjectKey :: Text -> Bool
validObjectKey objectKey =
  not (T.null objectKey)
    && T.length objectKey <= 1024
    && not (T.isPrefixOf "/" objectKey)
    && all validSegment (T.splitOn "/" objectKey)
    && T.all (\character -> character >= ' ' && character /= '\DEL' && character /= '\\') objectKey
  where
    validSegment segment = not (T.null segment) && segment /= "." && segment /= ".." && segment /= "~"

queryText :: [(Text, Text)] -> Text
queryText parameters = T.intercalate "&"
  [awsEncode key <> "=" <> awsEncode value | (key, value) <- sortOn (awsEncode . fst) parameters]

encodeSegment :: Text -> Text
encodeSegment = awsEncode

awsEncode :: Text -> Text
awsEncode = TE.decodeUtf8 . BS.concatMap encodeByte . TE.encodeUtf8
  where
    encodeByte byte
      | isUnreserved byte = BS.singleton byte
      | otherwise = BS.pack [37, hexDigit (byte `div` 16), hexDigit (byte `mod` 16)]
    isUnreserved byte =
      (byte >= 65 && byte <= 90) || (byte >= 97 && byte <= 122)
        || (byte >= 48 && byte <= 57) || byte `elem` [45,46,95,126]

hexDigit :: Word8 -> Word8
hexDigit value
  | value < 10 = 48 + value
  | otherwise = 65 + value - 10

sha256Hex :: BS.ByteString -> Text
sha256Hex bytes = hexBytes (convert (hash bytes :: Digest SHA256))

hmacBytes :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmacBytes key value = convert (hmac key value :: HMAC SHA256)

hexBytes :: BS.ByteString -> Text
hexBytes = TE.decodeUtf8 . BAE.convertToBase BAE.Base16

formatUtc :: String -> UTCTime -> Text
formatUtc pattern = T.pack . formatTime defaultTimeLocale pattern
