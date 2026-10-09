{-# LANGUAGE OverloadedStrings #-}

module TDF.MusicRelease.Storage.S3Spec (spec) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Test.Hspec
import TDF.MusicRelease.Storage.S3

spec :: Spec
spec = describe "TDF.MusicRelease.Storage.S3" $ do
  it "generates a deterministic path-style SigV4 URL without exposing the secret" $ do
    let result = presignGetObject config now 300 "tdf-music-private" "derivatives/áudio 01.m4a"
    result `shouldSatisfy` either (const False) (T.isPrefixOf "https://example.r2.cloudflarestorage.com/tdf-music-private/derivatives/%C3%A1udio%2001.m4a?")
    result `shouldSatisfy` either (const False) (T.isInfixOf "X-Amz-Expires=300")
    result `shouldSatisfy` either (const False) (T.isInfixOf "X-Amz-Signature=")
    result `shouldSatisfy` either (const False) (not . T.isInfixOf "super-secret")
    result `shouldBe` presignGetObject config now 300 "tdf-music-private" "derivatives/áudio 01.m4a"

  it "rejects traversal, insecure endpoints, invalid buckets, and long-lived URLs" $ do
    presignGetObject config now 300 "tdf-music-private" "../master.wav" `shouldSatisfy` isLeft
    presignGetObject config{ s3Endpoint="http://storage.test" } now 300 "tdf-music-private" "stream.m4a" `shouldSatisfy` isLeft
    presignGetObject config now 300 "INVALID_BUCKET" "stream.m4a" `shouldSatisfy` isLeft
    presignGetObject config now 901 "tdf-music-private" "stream.m4a" `shouldSatisfy` isLeft

  it "includes a temporary credential token as an encoded signed query value" $ do
    let result = presignGetObject config{ s3SessionToken=Just "token/+=" } now 60 "tdf-music-private" "stream.m4a"
    result `shouldSatisfy` either (const False) (T.isInfixOf "X-Amz-Security-Token=token%2F%2B%3D")

  it "scopes multipart URLs to one object, upload ID, operation, and part" $ do
    let createUrl = presignCreateMultipart config now 60 "tdf-music-private" "quarantine/session/master.wav"
        partUrl = presignUploadPart config now 60 "tdf-music-private" "quarantine/session/master.wav" "upload/id+1" 7
        completeUrl = presignCompleteMultipart config now 60 "tdf-music-private" "quarantine/session/master.wav" "upload/id+1"
        abortUrl = presignAbortMultipart config now 60 "tdf-music-private" "quarantine/session/master.wav" "upload/id+1"
    createUrl `shouldSatisfy` either (const False) (T.isInfixOf "uploads=")
    partUrl `shouldSatisfy` either (const False) (T.isInfixOf "partNumber=7")
    partUrl `shouldSatisfy` either (const False) (T.isInfixOf "uploadId=upload%2Fid%2B1")
    completeUrl `shouldSatisfy` either (const False) (T.isInfixOf "uploadId=upload%2Fid%2B1")
    abortUrl `shouldSatisfy` either (const False) (T.isInfixOf "uploadId=upload%2Fid%2B1")
    presignUploadPart config now 60 "tdf-music-private" "stream.m4a" "upload-1" 10001 `shouldSatisfy` isLeft

config :: S3SigningConfig
config = S3SigningConfig
  { s3Endpoint="https://example.r2.cloudflarestorage.com"
  , s3Region="auto"
  , s3AccessKeyId="synthetic-access-key"
  , s3SecretAccessKey="synthetic-super-secret"
  , s3SessionToken=Nothing
  }

now :: UTCTime
now = read "2026-09-12 12:00:00 UTC"
