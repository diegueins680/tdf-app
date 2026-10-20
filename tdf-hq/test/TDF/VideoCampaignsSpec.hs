{-# LANGUAGE OverloadedStrings #-}
module TDF.VideoCampaignsSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Either (isLeft)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist.Sql (rawExecute, toSqlKey)
import Database.Persist.Sqlite (runSqlite)
import Test.Hspec
import TDF.Auth (AuthedUser(..), ModuleAccess(..))
import TDF.Models (RoleEnum(..))
import qualified TDF.VideoCampaigns as C

actor :: [RoleEnum] -> AuthedUser
actor roles = AuthedUser (toSqlKey 42) roles Set.empty Nothing Nothing

draft :: Value
draft = object
  [ "title" .= ("Una sesion" :: String), "videoUrl" .= ("" :: String)
  , "coverUrl" .= ("" :: String), "durationSeconds" .= (610.08 :: Double)
  , "status" .= ("draft" :: String), "channels" .= ["instagram" :: String]
  , "pieces" .= ([] :: [Value]) ]

spec :: Spec
spec = do
  describe "video campaign ownership" $ do
    it "allows the persisted artist's own party" $
      C.campaignScope (actor [Artist]) Nothing `shouldBe` Right (toSqlKey 42)
    it "denies another artist, fan and nominal admin without module access" $ do
      C.campaignScope (actor [Artist]) (Just 51) `shouldSatisfy` isLeft
      C.campaignScope (actor [Fan]) Nothing `shouldSatisfy` isLeft
      C.campaignScope (actor [Admin]) (Just 51) `shouldSatisfy` isLeft
    it "allows a strict administrator with authoritative admin module access" $
      C.campaignScope ((actor [Admin]) { auModules = Set.singleton ModuleAdmin })
        (Just 51) `shouldBe` Right (toSqlKey 51)
    it "rejects invalid target and duplicate roles" $ do
      C.campaignScope (actor [Artist]) (Just 0) `shouldSatisfy` isLeft
      C.campaignScope (actor [Artist, Artist]) Nothing `shouldSatisfy` isLeft
  describe "video campaign validation" $ do
    it "accepts incomplete draft preparation" $ C.validateDocument draft `shouldBe` Right ()
    it "rejects missing document fields" $
      C.validateDocument (object ["status" .= ("published" :: String)]) `shouldSatisfy` isLeft
    it "does not admit external publishing or incomplete review-ready campaigns" $ do
      let payload status = object
            [ "title" .= ("Session" :: String), "status" .= status
            , "videoUrl" .= ("https://cdn.example.com/video.mp4" :: String)
            , "coverUrl" .= ("" :: String), "durationSeconds" .= (610 :: Int)
            , "channels" .= ["tiktok" :: String], "pieces" .= ([] :: [Value]) ]
      C.validateDocument (payload ("active" :: String)) `shouldSatisfy` isLeft
      C.validateDocument (payload ("ready" :: String)) `shouldSatisfy` isLeft
  describe "video campaign persistence and conflict recovery" $ do
    it "replays a lost save response without a duplicate and isolates artist lists" $ do
      now <- getCurrentTime
      (first, replay, foreignRows, rows, stale) <- runSqlite ":memory:" $ do
        rawExecute (T.unlines
          [ "CREATE TABLE video_campaign (id INTEGER PRIMARY KEY,"
          , "artist_party_id INTEGER NOT NULL, campaign_key TEXT NOT NULL,"
          , "revision INTEGER NOT NULL, document TEXT NOT NULL,"
          , "created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL,"
          , "UNIQUE(artist_party_id,campaign_key))" ]) []
        a <- C.saveCampaign (toSqlKey 42) "key" (C.CampaignWrite 0 draft) now
        b <- C.saveCampaign (toSqlKey 42) "key" (C.CampaignWrite 0 draft) now
        other <- C.listCampaigns (toSqlKey 51)
        own <- C.listCampaigns (toSqlKey 42)
        _ <- C.saveCampaign (toSqlKey 42) "key" (C.CampaignWrite 1 draft) now
        old <- C.saveCampaign (toSqlKey 42) "key" (C.CampaignWrite 0 draft) now
        pure (a, b, other, own, old)
      fmap C.revision first `shouldBe` Right 1
      fmap C.revision replay `shouldBe` Right 1
      fmap length foreignRows `shouldBe` Right 0
      fmap length rows `shouldBe` Right 1
      stale `shouldSatisfy` isLeft
