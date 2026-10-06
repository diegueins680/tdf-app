{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module SessionSpec (sessionSpec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp (testWithApplication)
import Database.Persist.Sql (PersistValue(..), Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool, toSqlKey, transactionUndo)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import System.Environment (setEnv)
import System.Timeout (timeout)
import Test.Hspec
import TDF.Auth (AuthedUser(..), loadAuthedUser)
import TDF.DB (Env(..))
import TDF.Social.API (SocialV2API)
import TDF.Social.Server (socialV2Server)
import TDF.Social.Session
import SessionModelCases

type ProtectedSocial = AuthProtect "bearer-token" :> SocialV2API
-- Emulate an in-flight request whose real loadAuthedUser already returned.
-- This is deliberately not a second fresh authentication check at dispatch.
snapshotApplication :: Env -> AuthedUser -> Application
snapshotApplication env user = serveWithContext (Proxy @ProtectedSocial)
  ((mkAuthHandler (\_ -> pure user) :: AuthHandler Request AuthedUser) :. EmptyContext) $
  hoistServerWithContext (Proxy @ProtectedSocial) (Proxy @'[AuthHandler Request AuthedUser])
    (`runReaderT` env) (socialV2Server :: ServerT ProtectedSocial (ReaderT Env Handler))

sessionSpec :: Env -> Spec
sessionSpec env = before_ reset $ describe "session authority on real PostgreSQL" $ do
  forM_ (zip [1::Int ..] sessionCases) $ \(n, SessionCase active1 active6 live1 live2 tokenId actor allowed) ->
    it ("refines checked model Read case " <> show n) $ do
      user <- authenticated tokenId
      sql "UPDATE api_token SET active=? WHERE id=1" [PersistBool active1]
      sql "UPDATE api_token SET active=? WHERE id=6" [PersistBool active6]
      sql "UPDATE user_credential SET active=? WHERE party_id=1" [PersistBool live1]
      sql "UPDATE user_credential SET active=? WHERE party_id=2" [PersistBool live2]
      result <- run $ withSocialSession ReadSession (user {auPartyId=toSqlKey actor}) (pure ())
      isRight result `shouldBe` allowed
  it "denies both handler reads and writes after their captured token is revoked" $ do
    user <- authenticated 1
    sql "UPDATE api_token SET active=false WHERE id=1" []
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    testWithApplication (pure (snapshotApplication env user)) $ \port -> do
      req <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/v2/me")
      statusCode . HTTP.responseStatus <$> HTTP.httpLbs req manager >>= (`shouldBe` 401)
      post <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/v2/preferences")
      response <- HTTP.httpLbs post {HTTP.method="PUT",
        HTTP.requestHeaders=[("Content-Type","application/json")],
        HTTP.requestBody=HTTP.RequestBodyLBS "{\"discoverable\":true,\"personalized\":true,\"expectedRevision\":0}"} manager
      statusCode (HTTP.responseStatus response) `shouldBe` 401
    otherSession <- authenticated 6
    isRight <$> (run $ withSocialSession ReadSession otherSession (pure ())) >>= (`shouldBe` True)
  it "revalidates reset-only purpose using the same Unicode-aware rule as authentication" $ do
    user <- authenticated 1
    sql "UPDATE api_token SET label=? WHERE id=1" [PersistText "\x2003Password-Reset:synthetic\x2003"]
    result <- run $ withSocialSession ReadSession user (pure ())
    fmap (const ()) result `shouldBe` Left err401
  it "denies a token whose owner changed after authentication" $ do
    user <- authenticated 1
    sql "UPDATE api_token SET party_id=2 WHERE id=1" []
    result <- run $ withSocialSession ReadSession user (pure ())
    result `shouldBe` Left err401
  it "denies a deleted token even though its account remains active" $ do
    user <- authenticated 1
    result <- run $ do
      rawExecute "DELETE FROM api_token WHERE id=1" []
      denied <- withSocialSession ReadSession user (pure ())
      transactionUndo
      pure denied
    result `shouldBe` Left err401
  it "denies a socially closed account with an otherwise active token" $ do
    user <- authenticated 1
    result <- run $ do
      _ <- rawSql "SELECT social_v2_close(1)::text" [] :: SqlPersistT IO [Single Text]
      denied <- withSocialSession ReadSession user (pure ())
      transactionUndo
      pure denied
    result `shouldBe` Left err404
  it "rejects synthetic actors with no captured token identity" $ do
    user <- authenticated 1
    result <- run $ withSocialSession ReadSession (user {auApiTokenId=Nothing}) (pure ())
    result `shouldBe` Left err401
  it "serializes a write that wins the race before token revocation" $ do
    user <- authenticated 1
    entered <- newEmptyMVar
    release <- newEmptyMVar
    let writing = run $ withSocialSession (WriteSession Nothing) user $ do
          rawExecute "INSERT INTO social_session_effect VALUES(1)" []
          liftIO $ putMVar entered () >> takeMVar release
        revoking = run $ do
          rawExecute "SET LOCAL application_name='social-session-revoke'" []
          rawExecute "UPDATE api_token SET active=false WHERE id=1" []
    withAsync writing $ \writer -> do
      bounded (takeMVar entered)
      withAsync revoking $ \revoker -> do
        blocked "social-session-revoke"
        putMVar release ()
        bounded (wait writer) >>= (`shouldBe` Right ())
        bounded (wait revoker)
    effectCount >>= (`shouldBe` 1)
    result <- run $ withSocialSession (WriteSession Nothing) user (rawExecute "INSERT INTO social_session_effect VALUES(2)" [])
    result `shouldBe` Left err401
    effectCount >>= (`shouldBe` 1)
  it "denies a write waiting behind a committed token revocation" $ do
    user <- authenticated 1
    entered <- newEmptyMVar
    release <- newEmptyMVar
    let revoking = run $ do
          rawExecute "UPDATE api_token SET active=false WHERE id=1" []
          liftIO $ putMVar entered () >> takeMVar release
        writing = run $ do
          rawExecute "SET LOCAL application_name='social-session-write'" []
          withSocialSession (WriteSession Nothing) user (rawExecute "INSERT INTO social_session_effect VALUES(1)" [])
    withAsync revoking $ \revoker -> do
      bounded (takeMVar entered)
      withAsync writing $ \writer -> do
        blocked "social-session-write"
        putMVar release ()
        bounded (wait revoker)
        bounded (wait writer) >>= (`shouldBe` Left err401)
    effectCount >>= (`shouldBe` 0)
  it "allows simultaneous shared reads while revocation waits for the held read" $ do
    user <- authenticated 1
    entered <- newEmptyMVar
    release <- newEmptyMVar
    withAsync (run $ withSocialSession ReadSession user (liftIO $ putMVar entered () >> takeMVar release)) $ \reader -> do
      bounded (takeMVar entered)
      bounded (run $ withSocialSession ReadSession user (pure ())) >>= (`shouldBe` Right ())
      withAsync (run $ do
        rawExecute "SET LOCAL application_name='social-session-read-revoke'" []
        rawExecute "UPDATE api_token SET active=false WHERE id=1" []) $ \revoker -> do
          blocked "social-session-read-revoke"
          putMVar release ()
          bounded (wait reader) >>= (`shouldBe` Right ())
          bounded (wait revoker)
  it "keeps password-reset credential-before-token locking free of inversion" $ do
    user <- authenticated 1
    entered <- newEmptyMVar
    release <- newEmptyMVar
    let resetting = run $ do
          rawExecute "UPDATE user_credential SET active=false WHERE party_id=1" []
          liftIO $ putMVar entered () >> takeMVar release
          rawExecute "UPDATE api_token SET active=false WHERE id=1" []
        writing = run $ do
          rawExecute "SET LOCAL application_name='social-session-reset-write'" []
          withSocialSession (WriteSession Nothing) user (rawExecute "INSERT INTO social_session_effect VALUES(1)" [])
    withAsync resetting $ \resetter -> do
      bounded (takeMVar entered)
      withAsync writing $ \writer -> do
        blocked "social-session-reset-write"
        putMVar release ()
        bounded (wait resetter)
        bounded (wait writer) >>= (`shouldBe` Left err401)
    effectCount >>= (`shouldBe` 0)
  where
    run action = runSqlPool action (envPool env)
    sql statement values = run (rawExecute statement values)
    reset = do
      setEnv "SOCIAL_V2_ENABLED" "true"
      sql "UPDATE social_v2_runtime SET enabled=true" []
      sql "UPDATE party SET is_org=false WHERE id IN (1,2)" []
      sql "UPDATE user_credential SET active=true WHERE party_id IN (1,2)" []
      sql "UPDATE api_token SET active=true,party_id=1,label=NULL WHERE id IN (1,6)" []
      sql "TRUNCATE social_session_effect" []
    authenticated tokenId = do
      let bearer = if tokenId == (1::Int64) then "synthetic-1" else "synthetic-alt"
      result <- run (loadAuthedUser bearer)
      user <- maybe (expectationFailure "Fixture authentication failed" >> fail "Missing fixture session") pure result
      auApiTokenId user `shouldBe` Just (toSqlKey tokenId)
      pure user
    effectCount = do
      [Single n] <- run (rawSql "SELECT count(*) FROM social_session_effect" [])
      pure (n :: Int64)
    bounded operation = do
      result <- timeout 10000000 operation
      maybe (expectationFailure "Session race did not reach its barrier within 10s" >> fail "Race timeout") pure result
    blocked name = bounded loop
      where
        loop = do
          [Single waiting] <- run (rawSql "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE application_name=? AND wait_event_type='Lock')" [PersistText name])
          unless waiting (threadDelay 10000 >> loop)
