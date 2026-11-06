{-# LANGUAGE OverloadedStrings #-}
module TDF.VideoCampaignsSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Aeson (Value, object, (.=))
import Data.Either (isLeft)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist.Sql (rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sqlite (runSqlite)
import Test.Hspec
import System.Environment (lookupEnv)
import TDF.Test.DisposablePostgres (isDisposablePostgresUrl)
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
  describe "video campaign current directory grants" $ do
    it "isolates artist, actor, revocation, moderation and publication capability in PostgreSQL" $ do
      configured <- lookupEnv "TDF_VIDEO_CAMPAIGN_MANAGER_TEST_DB"
      case configured of
        Nothing -> pendingWith "Set TDF_VIDEO_CAMPAIGN_MANAGER_TEST_DB to an isolated loopback _test database"
        Just url -> do
          isDisposablePostgresUrl False url `shouldBe` True
          outcomes <- runNoLoggingT $ withPostgresqlPool (BS.pack url) 1 $ \pool -> liftIO $ runSqlPool (do
            rawExecute "CREATE TEMP TABLE directory_profile(id integer PRIMARY KEY,subject_party_id bigint,profile_kind text,profile_status text,moderation_status text,visibility text) ON COMMIT DROP" []
            rawExecute "CREATE TEMP TABLE directory_profile_manager(profile_id integer,account_party_id bigint,active boolean,can_view_private boolean,can_edit boolean,can_publish boolean) ON COMMIT DROP" []
            rawExecute "INSERT INTO directory_profile VALUES(1,51,'band','published','allowed','public'),(2,52,'artist','published','allowed','public')" []
            rawExecute "INSERT INTO directory_profile_manager VALUES(1,42,true,true,true,false),(2,99,true,true,true,true)" []
            let allowed actorId artistId publish = C.campaignManagerAccess (toSqlKey actorId) (toSqlKey artistId) publish
            a <- allowed 42 51 False
            b <- allowed 42 51 True
            c <- allowed 42 52 False
            d <- allowed 99 51 False
            rawExecute "UPDATE directory_profile_manager SET can_publish=true WHERE profile_id=1" []
            e <- allowed 42 51 True
            rawExecute "UPDATE directory_profile SET subject_party_id=53 WHERE id=1" []
            f <- allowed 42 51 False
            rawExecute "UPDATE directory_profile SET subject_party_id=51 WHERE id=1" []
            rawExecute "UPDATE directory_profile_manager SET active=false WHERE profile_id=1" []
            g <- allowed 42 51 False
            rawExecute "UPDATE directory_profile_manager SET active=true WHERE profile_id=1" []
            rawExecute "UPDATE directory_profile SET profile_kind='person' WHERE id=1" []
            h <- allowed 42 51 False
            rawExecute "UPDATE directory_profile SET profile_kind='band',moderation_status='blocked' WHERE id=1" []
            i <- allowed 42 51 False
            rawExecute "UPDATE directory_profile SET moderation_status='allowed',profile_status='suspended' WHERE id=1" []
            j <- allowed 42 51 False
            rawExecute "UPDATE directory_profile SET profile_status='published',visibility='private' WHERE id=1" []
            k <- allowed 42 51 False
            l <- allowed 42 51 True
            rawExecute "UPDATE directory_profile_manager SET can_view_private=false WHERE profile_id=1" []
            m <- allowed 42 51 False
            pure [a,b,c,d,e,f,g,h,i,j,k,l,m]) pool
          outcomes `shouldBe` [True,False,False,False,True,False,False,False,False,False,True,False,False]
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
    it "requires saved rights and technical review in addition to a complete ready draft" $ do
      let reviewed rights technical state = object
            [ "title" .= ("Session" :: String), "status" .= state
            , "videoUrl" .= ("https://cdn.example.com/video.mp4" :: String)
            , "coverUrl" .= ("" :: String), "durationSeconds" .= (610 :: Int)
            , "channels" .= ["tiktok" :: String]
            , "rightsConfirmed" .= rights, "technicalReviewed" .= technical
            , "pieces" .= [object
                [ "id" .= ("clip" :: String), "channel" .= ("tiktok" :: String)
                , "day" .= (0 :: Int), "caption" .= ("Ver sesion" :: String)
                , "assetUrl" .= ("https://cdn.example.com/clip.mp4" :: String)
                , "status" .= ("prepared" :: String), "publishedUrl" .= ("" :: String)
                , "sourceStart" .= (0 :: Int), "sourceEnd" .= (30 :: Int) ] ] ]
      C.publicationVideo (reviewed True True ("ready" :: String))
        `shouldBe` Right "https://cdn.example.com/video.mp4"
      C.publicationVideo (reviewed False True ("ready" :: String)) `shouldSatisfy` isLeft
      C.publicationVideo (reviewed True False ("ready" :: String)) `shouldSatisfy` isLeft
      C.publicationVideo (reviewed True True ("draft" :: String)) `shouldSatisfy` isLeft
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
        backend <- ask
        liftIO $ flip runReaderT backend $ do
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
