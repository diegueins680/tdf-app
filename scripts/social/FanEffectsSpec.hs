{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module FanEffectsSpec (fanEffectsSpec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception (bracket_)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Sql (PersistValue(..), Single(..), rawExecute, rawSql, runSqlPool)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import System.Timeout (timeout)
import Test.Hspec
import TDF.API.FanFollowing (FollowArtistAPI, UnfollowArtistAPI)
import TDF.Auth (AuthedUser(..), authContext, loadAuthedUser)
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(..))
import TDF.Social.FanEffects (followArtist, unfollowArtist)
import FanEffectsModelCases

type ProtectedFollow = AuthProtect "bearer-token" :> (FollowArtistAPI :<|> UnfollowArtistAPI)
application :: Env -> Maybe AuthedUser -> Application
application env captured = serveWithContext (Proxy @ProtectedFollow) context $
  hoistServerWithContext (Proxy @ProtectedFollow) (Proxy @'[AuthHandler Request AuthedUser])
    (`runReaderT` env) (\user -> followArtist user :<|> unfollowArtist user)
  where
    context = case captured of
      Nothing -> authContext env
      Just user -> (mkAuthHandler (\_ -> pure user) :: AuthHandler Request AuthedUser) :. EmptyContext

fanEffectsSpec :: Env -> Spec
fanEffectsSpec env = before_ reset $ describe "artist-follow production handler, bearer HTTP and fan effects" $ do
  forM_ (zip [1::Int ..] fanCases) $ \(n,FanCase enabled activated governed eligible token subscribed club profile history alerts accepted afterSubscription afterProfile afterEdges afterAlerts) ->
    it ("refines checked FanEffects Commit "<>show n) $ do
      user <- authenticated
      setRuntime enabled activated
      when governed $ sql (if eligible
        then "INSERT INTO social_v2_pair(party_a,party_b) VALUES(2,3)"
        else "INSERT INTO social_v2_pair(party_a,party_b,block_b) VALUES(1,5,true)") []
      when (not eligible && not governed) $ sql "UPDATE user_credential SET active=false WHERE party_id=5" []
      sql "UPDATE api_token SET active=? WHERE id=1" [PersistBool token]
      when subscribed $ sql "INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES(1,5,'2026-01-01')" []
      when (not club) $ sql "UPDATE fan_club SET artist_party_id=4 WHERE id=1" []
      when profile $ sql "INSERT INTO fan_club_member_profile(party_id,club_id,handle,bio,joined_at) VALUES(1,1,'kept','old bio','2026-01-01')" []
      forM_ history $ \edge -> do
        let (a,b) = edgePair edge
        sql "INSERT INTO party_follow(follower_party_id,following_party_id,via_nfc,created_at) VALUES(?,?,true,'2026-01-01')"
          [PersistInt64 a,PersistInt64 b]
      sql "INSERT INTO notification(recipient_party_id,notif_type,title,body,is_read,created_at) SELECT 5,'artist_liked','old','historical',false,'2026-01-01' FROM generate_series(1,?::integer)" [PersistInt64 (fromIntegral alerts)]
      withApp (Just user) $ \call -> do
        response <- call "" "5"
        status response `shouldBe` (if not token then 401 else if accepted then 200 else 404)
        when accepted $ field "ffArtistId" response `shouldBe` Just (Number 5)
      exists "SELECT EXISTS(SELECT 1 FROM fan_follow WHERE fan_party_id=1 AND artist_party_id=5)" >>= (`shouldBe` afterSubscription)
      exists "SELECT EXISTS(SELECT 1 FROM fan_club_member_profile WHERE party_id=1 AND club_id=1)" >>= (`shouldBe` afterProfile)
      storedEdges >>= (`shouldBe` afterEdges)
      count "SELECT count(*) FROM notification" >>= (`shouldBe` fromIntegral afterAlerts)
      count "SELECT count(*) FROM engagement_event" >>= (`shouldBe` (if accepted && not subscribed then 1 else 0))
      exists "SELECT EXISTS(SELECT 1 FROM social_v2_pair WHERE consent_a OR consent_b OR follow_a OR follow_b)" >>= (`shouldBe` False)
      when profile $ do
        [Single handle] <- run (rawSql "SELECT handle FROM fan_club_member_profile WHERE party_id=1" [])
        (handle::Text) `shouldBe` "kept"
  it "uses the exact old DTO and named notification before cutover, with idempotent retry" $ withApp Nothing $ \call -> do
    r <- call "synthetic-1" "5"
    status r `shouldBe` 200
    field "ffArtistName" r `shouldBe` Just (String "Synthetic 5")
    field "ffHeroImageUrl" r `shouldBe` Just (String "https://example.invalid/artist.png")
    storedEdges >>= (`shouldBe` [1,2,3,4])
    [Single body] <- run (rawSql "SELECT body FROM notification" [])
    (body::Text) `shouldBe` "Synthetic 1 empezó a seguir tu perfil."
    sql "UPDATE fan_follow SET created_at='2026-01-01T23:00:00Z'" []
    r2 <- call "synthetic-1" "5"
    field "ffStartedAt" r2 `shouldBe` Just (String "2026-01-01")
    count "SELECT count(*) FROM notification" >>= (`shouldBe` 1)
    count "SELECT count(*) FROM engagement_event" >>= (`shouldBe` 1)
  it "keeps artist subscriptions after activation and pause without creating member profiles, follows or alerts" $ do
    sql "UPDATE social_v2_runtime SET enabled=true" []
    sql "UPDATE social_v2_runtime SET enabled=false" []
    withApp Nothing $ \call -> status <$> call "synthetic-1" "5" >>= (`shouldBe` 200)
    count "SELECT count(*) FROM fan_follow" >>= (`shouldBe` 1)
    count "SELECT count(*) FROM party_follow" >>= (`shouldBe` 0)
    count "SELECT count(*) FROM notification" >>= (`shouldBe` 0)
    count "SELECT count(*) FROM fan_club_member_profile WHERE party_id=1" >>= (`shouldBe` 0)
  it "does not even read the member roster or notification relation after retirement" $ do
    sql "UPDATE social_v2_runtime SET enabled=true" []
    bracket_
      (sql "ALTER TABLE fan_club_member_profile RENAME TO fan_fixture_profiles; ALTER TABLE notification RENAME TO fan_fixture_notifications" [])
      (sql "ALTER TABLE fan_fixture_profiles RENAME TO fan_club_member_profile; ALTER TABLE fan_fixture_notifications RENAME TO notification" []) $
      withApp Nothing $ \call -> status <$> call "synthetic-1" "5" >>= (`shouldBe` 200)
  it "retires effects for unrelated canonical closure while preserving the requested subscription" $ do
    sql "INSERT INTO social_v2_preference(party_id,closed) VALUES(4,true)" []
    withApp Nothing $ \call -> status <$> call "synthetic-1" "5" >>= (`shouldBe` 200)
    storedEdges >>= (`shouldBe` [])
    count "SELECT count(*) FROM notification" >>= (`shouldBe` 0)
  it "requires Fan or Customer, rejects malformed identities, and never treats an Admin role as a fan grant" $ do
    user <- authenticated
    withApp Nothing $ \call -> do
      status <$> call "" "5" >>= (`shouldBe` 401)
      forM_ ["0","-1","1"] $ \target -> status <$> call "synthetic-1" target >>= (`shouldBe` 400)
      status <$> call "synthetic-1" "999" >>= (`shouldBe` 404)
    forM_ [[],[Admin],[Fan,Fan]] $ \roles -> withApp (Just user {auRoles=roles}) $ \call ->
      status <$> call "" "5" >>= (`shouldBe` 403)
    withApp (Just user {auRoles=[Customer]}) $ \call -> status <$> call "" "5" >>= (`shouldBe` 200)
  it "fails closed for a missing adapter and keeps the no-foundation compatibility contract" $
    bracket_
      (sql "ALTER FUNCTION social_v2_lock_fan_effects() RENAME TO social_fixture_lock_fan_effects" [])
      (sql "ALTER FUNCTION social_fixture_lock_fan_effects() RENAME TO social_v2_lock_fan_effects" []) $ do
        withApp Nothing $ \call -> status <$> call "synthetic-1" "5" >>= (`shouldBe` 503)
        bracket_ (sql "ALTER TABLE social_v2_runtime RENAME TO social_fixture_runtime" [])
          (sql "ALTER TABLE social_fixture_runtime RENAME TO social_v2_runtime" []) $
          withApp Nothing $ \call -> status <$> call "synthetic-1" "5" >>= (`shouldBe` 200)
  it "lets the owner unfollow a blocked or socially closed target, preserving historical artifacts" $ withMethods Nothing $ \call -> do
    status <$> call "synthetic-1" "POST" "5" >>= (`shouldBe` 200)
    sql "UPDATE social_v2_runtime SET enabled=true" []
    sql "INSERT INTO social_v2_pair(party_a,party_b,block_b) VALUES(1,5,true)" []
    status <$> call "synthetic-1" "POST" "5" >>= (`shouldBe` 404)
    status <$> call "synthetic-1" "DELETE" "5" >>= (`shouldBe` 200)
    status <$> call "synthetic-1" "DELETE" "5" >>= (`shouldBe` 200)
    count "SELECT count(*) FROM fan_follow" >>= (`shouldBe` 0)
    count "SELECT count(*) FROM engagement_event WHERE event_type='unfollow'" >>= (`shouldBe` 1)
    storedEdges >>= (`shouldBe` [1,2,3,4])
    count "SELECT count(*) FROM notification" >>= (`shouldBe` 1)
    count "SELECT count(*) FROM fan_club_member_profile WHERE party_id=1" >>= (`shouldBe` 1)
  it "reauthorizes unfollow's captured token instead of removing a subscription after revocation" $ do
    user <- authenticated
    sql "INSERT INTO fan_follow(fan_party_id,artist_party_id) VALUES(1,5)" []
    sql "UPDATE api_token SET active=false WHERE id=1" []
    withMethods (Just user) $ \call -> status <$> call "" "DELETE" "5" >>= (`shouldBe` 401)
    count "SELECT count(*) FROM fan_follow" >>= (`shouldBe` 1)
  it "serializes a subscription removal ahead of a waiting follow, which may then recreate it" $ do
    sql "INSERT INTO fan_follow(fan_party_id,artist_party_id) VALUES(1,5)" []
    entered <- newEmptyMVar
    release <- newEmptyMVar
    let removing = run $ do
          rawExecute "DO $$ BEGIN PERFORM id FROM party WHERE id IN (1,5) ORDER BY id FOR UPDATE; END $$" []
          rawExecute "DELETE FROM fan_follow WHERE fan_party_id=1 AND artist_party_id=5" []
          liftIO $ putMVar entered () >> takeMVar release
    withAsync removing $ \remover -> do
      bounded (takeMVar entered)
      withApp Nothing $ \call -> withAsync (call "synthetic-1" "5") $ \following -> do
        blocked "Lock"
        putMVar release ()
        bounded (wait remover)
        status <$> bounded (wait following) >>= (`shouldBe` 200)
    count "SELECT count(*) FROM fan_follow" >>= (`shouldBe` 1)
    count "SELECT count(*) FROM notification" >>= (`shouldBe` 1)
  forM_ [False,True] $ \activation ->
    it (if activation then "finishes an admitted HTTP follow before activation and suppresses retry effects"
        else "finishes an HTTP follow before a competing HTTP unfollow") $
    bracket_
      (sql "CREATE FUNCTION fan_fixture_gate() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_advisory_xact_lock(91042); RETURN NEW; END $$; CREATE TRIGGER fan_fixture_gate BEFORE INSERT ON fan_follow FOR EACH ROW EXECUTE FUNCTION fan_fixture_gate()" [])
      (sql "DROP TRIGGER fan_fixture_gate ON fan_follow; DROP FUNCTION fan_fixture_gate()" []) $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        withAsync (run $ rawExecute "DO $$ BEGIN PERFORM pg_advisory_xact_lock(91042); END $$" [] >> liftIO (putMVar entered () >> takeMVar release)) $ \gate -> do
          bounded (takeMVar entered)
          withMethods Nothing $ \call -> withAsync (call "synthetic-1" "POST" "5") $ \following -> do
            blocked "Lock"
            let competing = if activation then sql "UPDATE social_v2_runtime SET enabled=true" []
                  else status <$> call "synthetic-1" "DELETE" "5" >>= (`shouldBe` 200)
            withAsync competing $ \competitor -> do
              bounded (waitForTwoLocks)
              putMVar release ()
              bounded (wait gate)
              status <$> bounded (wait following) >>= (`shouldBe` 200)
              bounded (wait competitor)
            when activation $ status <$> call "synthetic-1" "POST" "5" >>= (`shouldBe` 200)
        count "SELECT count(*) FROM fan_follow" >>= (`shouldBe` (if activation then 1 else 0))
        count "SELECT count(*) FROM notification" >>= (`shouldBe` 1)
        storedEdges >>= (`shouldBe` [1,2,3,4])
  it "serializes competing retries into one subscription, engagement event and notification" $ withApp Nothing $ \call -> do
    withAsync (call "synthetic-1" "5") $ \first -> do
      status <$> call "synthetic-1" "5" >>= (`shouldBe` 200)
      status <$> wait first >>= (`shouldBe` 200)
    count "SELECT count(*) FROM fan_follow" >>= (`shouldBe` 1)
    count "SELECT count(*) FROM engagement_event" >>= (`shouldBe` 1)
    count "SELECT count(*) FROM notification" >>= (`shouldBe` 1)
    storedEdges >>= (`shouldBe` [1,2,3,4])
  forM_ [("activation","UPDATE social_v2_runtime SET enabled=true",200),
         ("block","SELECT id FROM party WHERE id IN (1,5) ORDER BY id FOR UPDATE; INSERT INTO social_v2_pair(party_a,party_b,block_b) VALUES(1,5,true)",404),
         ("closure","SELECT id FROM party WHERE id=1 FOR UPDATE; INSERT INTO social_v2_preference(party_id,closed) VALUES(1,true)",404),
         ("token revocation","UPDATE api_token SET active=false WHERE id=1",401)] $ \(label,authority,expected) ->
    it ("reauthorizes the actual HTTP request waiting behind "<>label) $ do
      user <- authenticated
      entered <- newEmptyMVar
      release <- newEmptyMVar
      withAsync (run $ rawExecute authority [] >> liftIO (putMVar entered () >> takeMVar release)) $ \changing -> do
        bounded (takeMVar entered)
        withApp (Just user) $ \call -> withAsync (call "" "5") $ \writing -> do
          blocked "Lock"
          putMVar release ()
          bounded (wait changing)
          status <$> bounded (wait writing) >>= (`shouldBe` expected)
      storedEdges >>= (`shouldBe` [])
      count "SELECT count(*) FROM notification" >>= (`shouldBe` 0)
  where
    run action = runSqlPool action (envPool env)
    sql statement params = run (rawExecute statement params)
    exists statement = do [Single value] <- run (rawSql statement []); pure (value::Bool)
    count statement = do [Single value] <- run (rawSql statement []); pure (value::Int64)
    authenticated = do Just user <- run (loadAuthedUser "synthetic-1"); pure user
    edgePair edge = case edge of 1 -> (1,2); 2 -> (2,1); 3 -> (1,3); _ -> (3,1)
    storedEdges = do
      values <- run (rawSql "SELECT CASE WHEN follower_party_id=1 AND following_party_id=2 THEN 1::bigint WHEN follower_party_id=2 AND following_party_id=1 THEN 2 WHEN follower_party_id=1 AND following_party_id=3 THEN 3 ELSE 4 END FROM party_follow ORDER BY 1" [])
      pure [fromIntegral n::Int | Single (n::Int64) <- values]
    setRuntime enabled activated = bracket_
      (sql "ALTER TABLE social_v2_runtime DISABLE TRIGGER social_v2_activation_memory" [])
      (sql "ALTER TABLE social_v2_runtime ENABLE TRIGGER social_v2_activation_memory" [])
      (sql "UPDATE social_v2_runtime SET enabled=?,activated_once=?" [PersistBool enabled,PersistBool activated])
    reset = do
      sql "TRUNCATE social_v2_command,social_v2_pair,social_v2_preference,party_follow,fan_follow,fan_club_member_profile,notification,engagement_event,artist_profile,party_security_role,security_role RESTART IDENTITY" []
      setRuntime False False
      sql "UPDATE user_credential SET active=true; UPDATE party SET is_org=false; UPDATE fan_club SET artist_party_id=5 WHERE id=1" []
      sql "UPDATE api_token SET active=true,label=NULL,party_id=CASE WHEN id=6 THEN 1 ELSE id END" []
      sql "INSERT INTO artist_profile(artist_party_id,hero_image_url) VALUES(5,'https://example.invalid/artist.png')" []
      sql "INSERT INTO fan_club_member_profile(party_id,club_id,joined_at) VALUES(2,1,'2026-01-01'),(3,1,'2026-01-01')" []
      sql "INSERT INTO security_role(id,active,code,name_es,name_en,workflow_state_id) VALUES('00000000-0000-0000-0000-000000000001',true,'fan','Fan','Fan','00000000-0000-0000-0000-000000000002')" []
      sql "INSERT INTO party_security_role(id,party_id,role_id,approval_mode,active,created_at,version) VALUES('00000000-0000-0000-0000-000000000003',1,'00000000-0000-0000-0000-000000000001','bootstrap',true,now(),1)" []
    bounded action = timeout 10000000 action >>= maybe (fail "Fan effect race timed out") pure
    waitForTwoLocks = do
      locks <- count "SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock'"
      unless (locks>=2) (threadDelay 10000 >> waitForTwoLocks)
    blocked event = bounded loop
      where
        loop = do
          waiting <- exists ("SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='"<>event<>"')")
          unless waiting (threadDelay 10000 >> loop)
    withApp captured action = withMethods captured (\call -> action (\token target -> call token "POST" target))
    withMethods captured action = do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      testWithApplication (pure (application env captured)) $ \port -> action $ \token method target -> do
        req <- HTTP.parseRequest ("http://127.0.0.1:"<>show port<>"/"<>target)
        HTTP.httpLbs req {HTTP.method=method,HTTP.requestHeaders=[("Authorization","Bearer "<>token) | not (BS.null token)],
          HTTP.responseTimeout=HTTP.responseTimeoutMicro 15000000} manager
    status = statusCode . HTTP.responseStatus
    field key response = case decode (HTTP.responseBody response) of
      Just (Object fields) -> KM.lookup key fields
      _ -> Nothing
