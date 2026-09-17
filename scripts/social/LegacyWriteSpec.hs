{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module LegacyWriteSpec (legacyWriteSpec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception (bracket_)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Vector as V
import Database.Persist.Sql (PersistValue(..), Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool, toSqlKey)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import System.Timeout (timeout)
import Test.Hspec
import TDF.API.SocialRelationships (RelationshipWritesAPI)
import TDF.Auth (AuthedUser(..), authContext, loadAuthedUser)
import TDF.DB (Env(..))
import TDF.Social.RelationshipWrites
import TDF.Social.Session
import LegacyWriteModelCases

type ProtectedWrites = AuthProtect "bearer-token" :> RelationshipWritesAPI
application :: Env -> Maybe AuthedUser -> Application
application env captured = serveWithContext (Proxy @ProtectedWrites) context $
  hoistServerWithContext (Proxy @ProtectedWrites) (Proxy @'[AuthHandler Request AuthedUser])
    (`runReaderT` env) (\user -> addFriend user :<|> removeFriend user :<|> exchangeVCard user)
  where
    context = case captured of
      Nothing -> authContext env
      Just user -> (mkAuthHandler (\_ -> pure user) :: AuthHandler Request AuthedUser) :. EmptyContext

legacyWriteSpec :: Env -> Spec
legacyWriteSpec env = before_ reset $ describe "legacy write retirement with real bearer HTTP/PostgreSQL" $ do
  forM_ (zip [1::Int ..] legacyWriteCases) $ \(n,(enabled,activated,pair,closed,token,op,history,allowed,after)) ->
    it ("refines checked Commit observation " <> show n) $ do
      user <- authenticated
      setRuntime enabled activated
      whenSql pair "INSERT INTO social_v2_pair(party_a,party_b) VALUES(1,2)"
      whenSql closed "INSERT INTO social_v2_preference(party_id,closed) VALUES(2,true)"
      sql "UPDATE api_token SET active=? WHERE id=1" [PersistBool token]
      forM_ history $ \direction -> sql
        "INSERT INTO party_follow(follower_party_id,following_party_id,via_nfc,created_at) VALUES(?,?,true,'2026-01-01T23:00:00Z')"
        (map PersistInt64 (if direction==1 then [1,2] else [2,1]))
      withApp (Just user) $ \call -> do
        response <- call "" op "2"
        status response `shouldBe` (if not token then 401 else if allowed then 200 else 410)
        if allowed then pure () else do
          BS.isInfixOf "Synthetic 2" (BL.toStrict (HTTP.responseBody response)) `shouldBe` False
      directions >>= (`shouldBe` after)
      [Single consent] <- run (rawSql "SELECT EXISTS(SELECT 1 FROM social_v2_pair WHERE consent_a OR consent_b OR follow_a OR follow_b)" [])
      consent `shouldBe` False
  it "preserves legacy DTOs, timestamps, NFC provenance and retry uniqueness before activation" $ withApp Nothing $ \call -> do
    a <- call "synthetic-1" "add" "2"
    status a `shouldBe` 200
    length (items a) `shouldBe` 1
    sql "UPDATE party_follow SET created_at='2026-01-01T23:00:00Z'" []
    b <- call "synthetic-1" "vcard" "2"
    status b `shouldBe` 200
    length (items b) `shouldBe` 2
    map (field "pfViaNfc") (items b) `shouldBe` [Just (Bool True),Just (Bool True)]
    map (field "pfStartedAt") (items b) `shouldBe` [Just (String "2026-01-01"),Just (String "2026-01-01")]
    status <$> call "synthetic-1" "vcard" "2" >>= (`shouldBe` 200)
    directions >>= (`shouldBe` [1,2])
    status <$> call "synthetic-1" "remove" "2" >>= (`shouldBe` 200)
    status <$> call "synthetic-1" "remove" "2" >>= (`shouldBe` 200)
    directions >>= (`shouldBe` [])
  it "serializes competing reciprocal requests without duplicate edges or fabricated consent" $ withApp Nothing $ \call -> do
    withAsync (call "synthetic-1" "add" "2") $ \first -> do
      status <$> call "synthetic-2" "vcard" "1" >>= (`shouldBe` 200)
      status <$> wait first >>= (`shouldBe` 200)
    directions >>= (`shouldBe` [1,2])
    [Single canonical] <- run (rawSql "SELECT count(*) FROM social_v2_pair" []) :: IO [Single Int64]
    canonical `shouldBe` 0
  it "denies anonymous, invalid, self and missing targets with compatible statuses" $ withApp Nothing $ \call -> do
    forM_ ["add","vcard","remove"] $ \op -> do
      status <$> call "" op "2" >>= (`shouldBe` 401)
      forM_ ["0","-1","1"] $ \target -> status <$> call "synthetic-1" op target >>= (`shouldBe` 400)
    status <$> call "synthetic-1" "add" "999" >>= (`shouldBe` 404)
    status <$> call "synthetic-1" "remove" "999" >>= (`shouldBe` 200)
  it "keeps retirement after activation/pause and never returns historical names" $ do
    sql "INSERT INTO party_follow(follower_party_id,following_party_id) VALUES(1,2),(2,1)" []
    sql "UPDATE social_v2_runtime SET enabled=true" []
    sql "UPDATE social_v2_runtime SET enabled=false" []
    withApp Nothing $ \call -> forM_ ["add","vcard","remove"] $ \op -> do
      r <- call "synthetic-1" op "2"
      status r `shouldBe` 410
      lookup "Cache-Control" (HTTP.responseHeaders r) `shouldBe` Just "no-store"
    directions >>= (`shouldBe` [1,2])
  it "preserves old installation behavior when both foundation and adapter are absent" $
    bracket_
      (sql "ALTER FUNCTION social_v2_lock_legacy_write(bigint,bigint) RENAME TO social_fixture_lock_legacy_write; ALTER TABLE social_v2_runtime RENAME TO social_fixture_runtime" [])
      (sql "ALTER TABLE social_fixture_runtime RENAME TO social_v2_runtime; ALTER FUNCTION social_fixture_lock_legacy_write(bigint,bigint) RENAME TO social_v2_lock_legacy_write" []) $
      withApp Nothing $ \call -> do
        status <$> call "synthetic-1" "add" "2" >>= (`shouldBe` 200)
        status <$> call "synthetic-1" "vcard" "2" >>= (`shouldBe` 200)
        directions >>= (`shouldBe` [1,2])
        status <$> call "synthetic-1" "remove" "2" >>= (`shouldBe` 200)
        directions >>= (`shouldBe` [])
  it "fails closed for a partial foundation rather than dispatching the legacy handler" $
    bracket_
      (sql "ALTER FUNCTION social_v2_lock_legacy_write(bigint,bigint) RENAME TO social_fixture_lock_legacy_write" [])
      (sql "ALTER FUNCTION social_fixture_lock_legacy_write(bigint,bigint) RENAME TO social_v2_lock_legacy_write" []) $
      withApp Nothing $ \call -> forM_ ["add","vcard","remove"] $ \op ->
        status <$> call "synthetic-1" op "2" >>= (`shouldBe` 503)
  it "reauthorizes a captured token after account switching or reset-only relabeling" $ do
    user <- authenticated
    sql "UPDATE api_token SET party_id=2 WHERE id=1" []
    withApp (Just user) $ \call -> status <$> call "" "add" "2" >>= (`shouldBe` 401)
    sql "UPDATE api_token SET party_id=1,label='password-reset:fixture' WHERE id=1" []
    withApp (Just user) $ \call -> status <$> call "" "vcard" "2" >>= (`shouldBe` 401)
  forM_ [("activation","UPDATE social_v2_runtime SET enabled=true",410),
         ("pair creation","SELECT id FROM party WHERE id IN (1,2) ORDER BY id FOR UPDATE; INSERT INTO social_v2_pair(party_a,party_b,block_a) VALUES(1,2,true)",410),
         ("closure","SELECT id FROM party WHERE id=2 FOR UPDATE; INSERT INTO social_v2_preference(party_id,closed) VALUES(2,true)",410),
         ("token revocation","UPDATE api_token SET active=false WHERE id=1",401)] $ \(label,authority,expected) ->
    it ("denies the HTTP writer waiting behind "<>label) $ do
      user <- authenticated
      entered <- newEmptyMVar
      release <- newEmptyMVar
      let change = run $ rawExecute authority [] >> liftIO (putMVar entered () >> takeMVar release)
      withAsync change $ \changing -> do
        bounded (takeMVar entered)
        withApp (Just user) $ \call -> withAsync (call "" "add" "2") $ \writing -> do
          blockedQuery
          putMVar release ()
          bounded (wait changing)
          status <$> bounded (wait writing) >>= (`shouldBe` expected)
      directions >>= (`shouldBe` [])
  it "lets the locked write finish before activation, then rejects its HTTP retry" $ do
    user <- authenticated
    entered <- newEmptyMVar
    release <- newEmptyMVar
    let writeFirst = run $ do
          [Single permitted] <- rawSql "SELECT social_v2_lock_legacy_write(1,2)" []
          liftIO $ permitted `shouldBe` True
          withCurrentSession (WriteSession (Just (toSqlKey 2))) user $ do
            rawExecute "INSERT INTO party_follow(follower_party_id,following_party_id) VALUES(1,2),(2,1)" []
            liftIO $ putMVar entered () >> takeMVar release
    withAsync writeFirst $ \writing -> do
      bounded (takeMVar entered)
      withAsync (sql "UPDATE social_v2_runtime SET enabled=true" []) $ \activating -> do
        blockedQuery
        putMVar release ()
        bounded (wait writing) >>= (`shouldBe` Right ())
        bounded (wait activating)
    withApp Nothing $ \call -> status <$> call "synthetic-1" "add" "2" >>= (`shouldBe` 410)
    directions >>= (`shouldBe` [1,2])
  it "rejects a stale repeatable-read transaction" $ do
    result <- run $ do
      rawExecute "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ" []
      rawExecute "DO $$ BEGIN PERFORM social_v2_lock_legacy_write(1,2); RAISE EXCEPTION 'unsafe isolation accepted'; EXCEPTION WHEN feature_not_supported THEN NULL; END $$" []
    result `shouldBe` ()
  where
    run action = runSqlPool action (envPool env)
    sql statement params = run (rawExecute statement params)
    whenSql condition statement = if condition then sql statement [] else pure ()
    authenticated = do
      Just user <- run (loadAuthedUser "synthetic-1")
      pure user
    setRuntime enabled activated = bracket_
      (sql "ALTER TABLE social_v2_runtime DISABLE TRIGGER social_v2_activation_memory" [])
      (sql "ALTER TABLE social_v2_runtime ENABLE TRIGGER social_v2_activation_memory" [])
      (sql "UPDATE social_v2_runtime SET enabled=?,activated_once=?" [PersistBool enabled,PersistBool activated])
    reset = do
      sql "TRUNCATE social_v2_command,social_v2_pair,social_v2_preference,party_follow RESTART IDENTITY" []
      setRuntime False False
      sql "UPDATE user_credential SET active=true" []
      sql "UPDATE party SET is_org=false" []
      sql "UPDATE api_token SET active=true,label=NULL,party_id=CASE WHEN id=6 THEN 1 ELSE id END" []
    directions = do
      rows <- run (rawSql "SELECT CASE WHEN follower_party_id=1 THEN 1::bigint ELSE 2::bigint END FROM party_follow ORDER BY follower_party_id" []) :: IO [Single Int64]
      pure [fromIntegral n :: Int | Single n <- rows]
    bounded action = timeout 10000000 action >>= maybe (fail "Legacy write race timed out") pure
    blockedQuery = bounded loop
      where
        loop = do
          [Single waiting] <- run (rawSql "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock')" [])
          unless waiting (threadDelay 10000 >> loop)
    withApp captured action = do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      testWithApplication (pure (application env captured)) $ \port -> action $ \token op target -> do
        let path = if op=="vcard" then "/vcard-exchange" else "/friends/"<>target
            body = if op=="vcard" then "{\"vcerPartyId\":"<>BL.fromStrict (BS.pack target)<>"}" else "{}"
        req <- HTTP.parseRequest ("http://127.0.0.1:"<>show port<>path)
        HTTP.httpLbs req {HTTP.method=if op=="remove" then "DELETE" else "POST",
          HTTP.requestHeaders=[("Content-Type","application/json")]<>[("Authorization","Bearer "<>token) | not (BS.null token)],
          HTTP.requestBody=HTTP.RequestBodyLBS body,HTTP.responseTimeout=HTTP.responseTimeoutMicro 15000000} manager
    status = statusCode . HTTP.responseStatus
    items response = case decode (HTTP.responseBody response) of Just (Array rows) -> V.toList rows; _ -> []
    field key (Object fields) = KM.lookup key fields
    field _ _ = Nothing
