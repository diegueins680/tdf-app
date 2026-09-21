{-# LANGUAGE OverloadedStrings #-}
module TDF.Services.YouTubeSpec (spec) where

import Data.Aeson (Value(Null), encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck (chooseInt, forAll, property)
import TDF.Services.YouTube

channel :: Text
channel = "UCx9Jpaw_XDrMtIdzWYlU51g"

ident :: Text
ident = "f2BabxM1Pjc"

fixture :: Text -> Text -> Bool -> Value -> Value
fixture privacy broadcast embeddable live = object
  [ "id" .= ident
  , "snippet" .= object
      [ "channelId" .= channel, "title" .= ("Federico" :: Text)
      , "description" .= ("Untrusted <script>text</script>" :: Text)
      , "publishedAt" .= ("2026-09-09T22:15:24Z" :: Text)
      , "liveBroadcastContent" .= broadcast
      , "thumbnails" .= object
          [ "high" .= object
              [ "url" .= ("https://i.ytimg.com/vi/f2BabxM1Pjc/hqdefault.jpg" :: Text)
              , "width" .= (480 :: Int), "height" .= (360 :: Int) ]
          , "maxres" .= object
              [ "url" .= ("https://i.ytimg.com/vi/ooPsIHsikYU/maxresdefault.jpg" :: Text)
              , "width" .= (1280 :: Int), "height" .= (720 :: Int) ] ] ]
  , "contentDetails" .= object ["duration" .= ("PT44M15S" :: Text)]
  , "status" .= object ["privacyStatus" .= privacy, "uploadStatus" .= ("processed" :: Text)
      , "embeddable" .= embeddable]
  , "liveStreamingDetails" .= live ]

response :: [Value] -> BL.ByteString
response items = encode (object ["items" .= items])

spec :: Spec
spec = do
  describe "YouTube public provider boundary" $ do
    it "verifies the returned stable channel and obtains its actual uploads playlist" $ do
      let body = response [object ["id" .= channel
            , "snippet" .= object ["title" .= ("TDF Records" :: Text)]
            , "contentDetails" .= object ["relatedPlaylists" .= object
                ["uploads" .= ("UUx9Jpaw_XDrMtIdzWYlU51g" :: Text)]]]]
      decodeChannel channel body `shouldBe`
        Right (Channel channel "TDF Records" "UUx9Jpaw_XDrMtIdzWYlU51g")
      decodeChannel "UC0000000000000000000000" body `shouldSatisfy` isLeft
    it "keeps pagination checkpoints and rejects truncated or malformed responses" $ do
      let item = object ["contentDetails" .= object ["videoId" .= ident]]
      decodeUploadPage (encode (object ["items" .= [item], "nextPageToken" .= ("opaque==" :: Text)]))
        `shouldBe` Right (UploadPage [ident] (Just "opaque=="))
      decodeUploadPage (response [item]) `shouldBe` Right (UploadPage [ident] Nothing)
      decodeUploadPage "{}" `shouldSatisfy` isLeft
      decodeUploadPage (response [item,item]) `shouldSatisfy` isLeft
      decodeUploadPage "{\"items\":[],\"nextPageToken\":\"again\"}" `shouldSatisfy` isLeft
    it "keeps real matching thumbnail metadata and non-embeddable provider links" $ do
      let body = response [fixture "public" "none" False (toJSONNull)]
      case decodeVideos channel [ident] body of
        Right [PublicVideo video] -> do
          videoId video `shouldBe` ident
          videoEmbeddable video `shouldBe` False
          videoDurationSeconds video `shouldBe` 2655
          videoThumbnails video `shouldBe`
            [Thumbnail "https://i.ytimg.com/vi/f2BabxM1Pjc/hqdefault.jpg" 480 360]
          videoCompletedLive video `shouldBe` False
        other -> expectationFailure (show other)
    it "discards private and unlisted content without retaining titles or descriptions" $ do
      mapM_ (\privacy -> decodeVideos channel [ident]
        (response [fixture privacy "none" True toJSONNull])
          `shouldBe` Right [NonPublicVideo ident]) ["private", "unlisted"]
    it "does not classify upcoming or active streams as recordings" $ do
      mapM_ (\broadcast -> decodeVideos channel [ident]
        (response [fixture "public" broadcast True toJSONNull])
          `shouldBe` Right [ReviewVideo ident "live_not_completed"]) ["live", "upcoming"]
      let active = object ["actualStartTime" .= ("2026-09-09T22:15:24Z" :: Text)]
      decodeVideos channel [ident] (response [fixture "public" "none" True active])
        `shouldBe` Right [ReviewVideo ident "live_not_completed"]
    it "accepts a completed livestream only with its actual end metadata" $ do
      let completed = object ["actualStartTime" .= ("2026-09-09T22:15:24Z" :: Text)
            , "actualEndTime" .= ("2026-09-09T23:15:24Z" :: Text)]
      case decodeVideos channel [ident] (response [fixture "public" "none" True completed]) of
        Right [PublicVideo video] -> videoCompletedLive video `shouldBe` True
        other -> expectationFailure (show other)
    it "distinguishes absent metadata from deletion and rejects unexpected or duplicate identities" $ do
      decodeVideos channel [ident] (response []) `shouldBe` Right [MissingVideo ident]
      decodeVideos channel ["ooPsIHsikYU"] (response [fixture "public" "none" True toJSONNull])
        `shouldSatisfy` isLeft
      decodeVideos channel [ident] (response (replicate 2 (fixture "public" "none" True toJSONNull)))
        `shouldSatisfy` isLeft
      decodeVideos "UC0000000000000000000000" [ident]
        (response [fixture "public" "none" True toJSONNull])
          `shouldBe` Right [ReviewVideo ident "channel_mismatch"]
    it "never exposes keys through Show" $ do
      fmap show (youTubeKey "test-key") `shouldBe` Right "YouTubeKey <redacted>"
      youTubeKey "" `shouldSatisfy` isLeft
    it "parses bounded ISO durations without inferring Shorts" $ do
      durationSeconds "PT15M3S" `shouldBe` Just 903
      durationSeconds "P1DT1H2M3S" `shouldBe` Just 90123
      mapM_ (\bad -> durationSeconds bad `shouldBe` Nothing)
        ["", "PT", "P", "PT0S", "PT-1S", "PT1M1H", "PT1H1H", "PT999999999999999999999S"]
    it "round-trips positive second durations" $ property $
      forAll (chooseInt (1, 10000000)) $ \seconds ->
        durationSeconds ("PT" <> T.pack (show seconds) <> "S") == Just seconds
  where
    toJSONNull = Null
