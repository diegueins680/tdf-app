{-# LANGUAGE OverloadedStrings #-}
module TDF.LiveIntakeIdentitySpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool, toSqlKey)
import Database.Persist.Postgresql (createPostgresqlPool)
import Servant (NoContent(..), ServerError, errHTTPCode)
import System.Environment (lookupEnv)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (openBinaryTempFile, hClose)
import qualified Data.Text as T
import Servant.Multipart (FileData(..))
import Test.Hspec
import TDF.API.LiveSessions
import TDF.Auth (AuthedUser(..), modulesForRoles)
import TDF.Config (loadConfig)
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(..))
import TDF.ServerLiveSessions (liveSessionsServer)

spec :: Spec
spec = describe "live-intake-identity-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_LIVE_INTAKE_TEST_DB")
  case configured of
    Nothing -> it "requires the isolated integration runner" $ pendingWith "Run scripts/test-live-intake-identity.sh"
    Just connection | not ("dbname=tdf_live_intake_identity_test_" `isInfixOf` connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 5)) $
      beforeWith reset $ do
        it "does not treat shared email as identity or provision credentials or roles" $ \pool -> do
          submit pool member "shared-email-request" payload `shouldReturn` Right NoContent
          counts pool `shouldReturn` [4,1,1,1,1,0,0]
          rows <- runSqlPool (rawSql "SELECT count(*) FROM party WHERE primary_email='shared@example.test'" []) pool
          rows `shouldBe` [Single (2 :: Int64)]
        it "serializes concurrent retries into one intake and one new contact" $ \pool -> do
          boxes <- forM [1..5 :: Int] $ \_ -> do
            box <- newEmptyMVar
            void $ forkIO $ do
              result <- try (submit pool member "concurrent-request" payload) :: IO (Either SomeException (Either ServerError NoContent))
              putMVar box result
            pure box
          results <- mapM takeMVar boxes
          map (either (const False) (== Right NoContent)) results `shouldBe` replicate 5 True
          counts pool `shouldReturn` [4,1,1,1,1,0,0]
          submit pool member "concurrent-request" payload `shouldReturn` Right NoContent
          counts pool `shouldReturn` [4,1,1,1,1,0,0]
        it "rejects changed accepted payloads without additional effects" $ \pool -> do
          submit pool member "changed-request-identity" payload `shouldReturn` Right NoContent
          changed <- submit pool member "changed-request-identity" (payload { lsiBandName = "Changed" })
          fmap (const 200) (either (Left . errHTTPCode) Right changed) `shouldBe` Left 409
          counts pool `shouldReturn` [4,1,1,1,1,0,0]
        it "scopes request identity to the actor" $ \pool -> do
          submit pool member "same-request-different-actors" payload `shouldReturn` Right NoContent
          submit pool admin "same-request-different-actors" payload `shouldReturn` Right NoContent
          counts pool `shouldReturn` [5,2,2,2,2,0,0]
        it "allows authorized explicit reuse without changing the contact" $ \pool -> do
          let selected = musician { lsmPartyId = Just 3, lsmIsExisting = True }
          submit pool admin "authorized-reference" (payload { lsiMusicians = [selected] }) `shouldReturn` Right NoContent
          counts pool `shouldReturn` [3,1,1,1,1,0,0]
        it "rolls back earlier contact creation when a later reference is unauthorized" $ \pool -> do
          let selected = musician { lsmPartyId = Just 3, lsmIsExisting = True }
          result <- submit pool member "partial-failure-request" (payload { lsiMusicians = [musician,selected] })
          either errHTTPCode (const 200) result `shouldBe` 403
          counts pool `shouldReturn` [3,0,0,0,0,0,0]
        it "blocks archived references without leaving new contacts behind" $ \pool -> do
          runSqlPool (rawExecute "INSERT INTO identity_party_archive VALUES (3)" []) pool
          let selected = musician { lsmPartyId = Just 3, lsmIsExisting = True }
          result <- try (submit pool admin "archived-reference-request" (payload { lsiMusicians = [musician,selected] })) :: IO (Either SomeException (Either ServerError NoContent))
          either (const True) isLeft result `shouldBe` True
          counts pool `shouldReturn` [3,0,0,0,0,0,0]
        it "reuses rider storage on retry without duplicating the intake" $ \pool -> do
          temp <- getTemporaryDirectory
          bracket (openBinaryTempFile temp "synthetic-intake-rider")
            (\(path, _) -> removeFile path) $ \(path, handle) -> do
              BS.hPutStr handle "synthetic rider"
              hClose handle
              let rider = FileData "rider" "synthetic-intake-rider.txt" "text/plain" path
                  body = payload { lsiRider = Just rider }
              submit pool member "rider-storage-request" body `shouldReturn` Right NoContent
              submit pool member "rider-storage-request" body `shouldReturn` Right NoContent
              counts pool `shouldReturn` [4,1,1,1,1,0,0]
              paths <- runSqlPool (rawSql "SELECT rider_path FROM live_session_intake" []) pool
              case paths of
                [Single stored] -> do
                  BS.readFile (T.unpack stored) `shouldReturn` "synthetic rider"
                  removeFile (T.unpack stored)
                _ -> expectationFailure "Expected one persisted rider"
        it "rejects missing submission identity before creating any records" $ \pool -> do
          cfg <- loadConfig
          result <- runReaderT (runExceptT (liveSessionsServer member Nothing payload)) (Env pool cfg)
          either errHTTPCode (const 200) result `shouldBe` 400
          counts pool `shouldReturn` [3,0,0,0,0,0,0]

admin, member :: AuthedUser
admin = AuthedUser (toSqlKey 1) [Admin] (modulesForRoles [Admin]) Nothing
member = AuthedUser (toSqlKey 2) [] Set.empty Nothing

musician :: LiveSessionMusicianPayload
musician = LiveSessionMusicianPayload Nothing "Synthetic musician" (Just "shared@example.test") Nothing Nothing False
payload :: LiveSessionIntakePayload
payload = LiveSessionIntakePayload "Synthetic band" Nothing Nothing Nothing Nothing Nothing Nothing Nothing True (Just "TDF Live Sessions v2") [musician] [LiveSessionSongPayload "Synthetic song" Nothing Nothing Nothing (Just 0)] Nothing

submit :: ConnectionPool -> AuthedUser -> Text -> LiveSessionIntakePayload -> IO (Either ServerError NoContent)
submit pool user key body = do
  cfg <- loadConfig
  runReaderT (runExceptT (liveSessionsServer user (Just key) body)) (Env pool cfg)

reset :: ConnectionPool -> IO ConnectionPool
reset pool = do
  runSqlPool (do
    rawExecute "TRUNCATE identity_live_intake_request, live_session_song, live_session_musician, live_session_intake, identity_party_archive, party RESTART IDENTITY CASCADE" []
    rawExecute "INSERT INTO party(display_name,is_org,primary_email,created_at) VALUES ('Admin',false,NULL,now()),('Member',false,NULL,now()),('Separate person',false,'shared@example.test',now())" []
    ) pool
  pure pool

counts :: ConnectionPool -> IO [Int64]
counts pool = runSqlPool (do
  rows <- rawSql "SELECT (SELECT count(*) FROM party),(SELECT count(*) FROM live_session_intake),(SELECT count(*) FROM live_session_musician),(SELECT count(*) FROM live_session_song),(SELECT count(*) FROM identity_live_intake_request),(SELECT count(*) FROM user_credential),(SELECT count(*) FROM party_role)" []
  case rows of
    [(Single a,Single b,Single c,Single d,Single e,Single f,Single g)] -> pure [a,b,c,d,e,f,g]
    _ -> fail "Missing intake counts") pool
