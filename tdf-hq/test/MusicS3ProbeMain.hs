{-# LANGUAGE OverloadedStrings #-}

-- Local integration-test bridge; calls the production signer, never a replica.
module Main (main) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import System.Environment (getEnv)
import System.IO (BufferMode(LineBuffering), hSetBuffering, isEOF, stdout)
import TDF.MusicRelease.Storage.S3

main :: IO ()
main = do
  endpoint <- T.pack <$> getEnv "MUSIC_S3_ENDPOINT"
  unless ("https://127.0.0.1:" `T.isPrefixOf` endpoint) $
    fail "Integration signer only permits the local test endpoint"
  access <- T.pack <$> getEnv "MUSIC_S3_ACCESS_KEY_ID"
  secret <- T.pack <$> getEnv "MUSIC_S3_SECRET_ACCESS_KEY"
  let config = S3SigningConfig endpoint "us-east-1" access secret Nothing
  hSetBuffering stdout LineBuffering
  loop config

loop :: S3SigningConfig -> IO ()
loop config = do
  done <- isEOF
  unless done $ do
    line <- BL.fromStrict <$> BS.getLine
    now <- getCurrentTime
    let result = do
          value <- eitherDecode line
          (operation, bucket, key, uploadId, part, expiry, offset) <- parseEither
            (withObject "probe" $ \o -> (,,,,,,)
              <$> o .: "operation" <*> o .: "bucket" <*> o .: "key"
              <*> o .:? "uploadId" .!= "" <*> o .:? "part" .!= 1
              <*> o .:? "expiry" .!= 60 <*> o .:? "offset" .!= (0 :: Integer)) value
          let issued = addUTCTime (fromInteger offset) now
              signed = case (operation :: T.Text) of
                "get" -> presignGetObject config issued expiry bucket key
                "create" -> presignCreateMultipart config issued expiry bucket key
                "part" -> presignUploadPart config issued expiry bucket key uploadId part
                "complete" -> presignCompleteMultipart config issued expiry bucket key uploadId
                "abort" -> presignAbortMultipart config issued expiry bucket key uploadId
                _ -> Left InvalidObjectKey
          either (Left . show) Right signed
    BL.putStrLn $ encode $ either
      (const (object ["error" .= ("Signing rejected" :: T.Text)]))
      (\url -> object ["url" .= url]) result
    loop config
