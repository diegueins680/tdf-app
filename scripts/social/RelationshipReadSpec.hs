{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module RelationshipReadSpec (relationshipReadSpec) where

import Control.Exception (bracket_)
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Database.Persist.Sql (rawExecute, runSqlPool)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import System.Environment (setEnv)
import Test.Hspec
import TDF.API.SocialRelationships (RelationshipReadsAPI)
import TDF.Auth (AuthedUser(..), authContext, loadAuthedUser)
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(..))
import TDF.Social.RelationshipReads (relationshipList, suggestions)

type ProtectedRelationships = AuthProtect "bearer-token" :> RelationshipReadsAPI
application :: Env -> Maybe AuthedUser -> Application
application env captured = serveWithContext (Proxy @ProtectedRelationships) context $
  hoistServerWithContext (Proxy @ProtectedRelationships) (Proxy @'[AuthHandler Request AuthedUser])
    (`runReaderT` env) (\user -> relationshipList user "followers" (throwError err501)
      :<|> relationshipList user "following" (throwError err501)
      :<|> relationshipList user "friends" (throwError err501)
      :<|> suggestions user (throwError err501))
  where
    context = case captured of
      Nothing -> authContext env
      Just user -> (mkAuthHandler (\_ -> pure user) :: AuthHandler Request AuthedUser) :. EmptyContext

relationshipReadSpec :: Env -> Spec
relationshipReadSpec env = before_ reset $ describe "legacy relationship read adapter with bearer HTTP and PostgreSQL" $ do
  it "preserves direction, NFC metadata and UTC dates with deterministic tie ordering" $ withApp Nothing $ \call -> do
    following <- call "synthetic-1" "/following"
    status following `shouldBe` 200
    map (field "pfFollowingId") (items following) `shouldBe` [Just (Number 3),Just (Number 2)]
    let row=last (items following)
    field "pfFollowerName" row `shouldBe` Just (String "Synthetic 1")
    field "pfFollowingName" row `shouldBe` Just (String "Synthetic 2")
    field "pfViaNfc" row `shouldBe` Just (Bool True)
    field "pfStartedAt" row `shouldBe` Just (String "2026-01-01")
    followers <- call "synthetic-1" "/followers"
    map (field "pfFollowerId") (items followers) `shouldBe` [Just (Number 4),Just (Number 2)]
    friends <- call "synthetic-1" "/friends"
    map (field "pfFollowingId") (items friends) `shouldBe` [Just (Number 2)]
  it "excludes malformed legacy self-links without deleting stored history" $ do
    sql "INSERT INTO party_follow(follower_party_id,following_party_id,created_at) VALUES(1,1,now())"
    withApp Nothing $ \call -> do
      forM_ ["/following","/friends"] $ \path -> do
        r <- call "synthetic-1" path
        map (field "pfFollowingId") (items r) `shouldNotContain` [Just (Number 1)]
      r <- call "synthetic-1" "/followers"
      map (field "pfFollowerId") (items r) `shouldNotContain` [Just (Number 1)]
  it "removes both directions and the mutual-list row after either side blocks" $ do
    sql "INSERT INTO social_v2_pair(party_a,party_b,block_b) VALUES(1,2,true)"
    withApp Nothing $ \call -> do
      forM_ ["/following","/followers","/friends"] $ \path -> do
        r <- call "synthetic-1" path
        status r `shouldBe` 200
        BS.isInfixOf "Synthetic 2" (BL.toStrict (HTTP.responseBody r)) `shouldBe` False
      sql "UPDATE social_v2_pair SET block_a=true,block_b=false"
      items <$> call "synthetic-1" "/friends" >>= (`shouldBe` [])
  it "keeps denial after pause and does not let administrator roles bypass it" $ do
    Just user <- run (loadAuthedUser "synthetic-1")
    sql "INSERT INTO social_v2_pair(party_a,party_b,block_a) VALUES(1,2,true)"
    sql "UPDATE social_v2_runtime SET enabled=false"
    setEnv "SOCIAL_V2_ENABLED" "false"
    withApp (Just user {auRoles=[Admin]}) $ \call -> items <$> call "" "/friends" >>= (`shouldBe` [])
  it "rechecks closure and credential revocation without exposing names or counts" $ withApp Nothing $ \call -> do
    sql "UPDATE user_credential SET active=false WHERE party_id=2"
    items <$> call "synthetic-1" "/friends" >>= (`shouldBe` [])
    sql "INSERT INTO social_v2_preference(party_id,closed) VALUES(1,true)"
    forM_ paths $ \path -> do
      r <- call "synthetic-1" path
      status r `shouldBe` 200
      items r `shouldBe` []
  it "does not resurrect deleted legacy edges or treat a surviving reverse edge as mutual" $ withApp Nothing $ \call -> do
    sql "DELETE FROM party_follow WHERE follower_party_id=1 AND following_party_id=2"
    items <$> call "synthetic-1" "/friends" >>= (`shouldBe` [])
    r <- call "synthetic-1" "/following"
    map (field "pfFollowingId") (items r) `shouldBe` [Just (Number 3)]
  it "retains direct relationship visibility under mute and Discover opt-out" $ do
    sql "INSERT INTO social_v2_pair(party_a,party_b,mute_a) VALUES(1,2,true)"
    sql "INSERT INTO social_v2_preference(party_id,discoverable) VALUES(2,false)"
    withApp Nothing $ \call -> length . items <$> call "synthetic-1" "/friends" >>= (`shouldBe` 1)
  it "requires real bearer auth and revalidates a captured revoked token on every route" $ do
    Just user <- run (loadAuthedUser "synthetic-1")
    withApp Nothing $ \call -> forM_ paths $ \path -> status <$> call "" path >>= (`shouldBe` 401)
    sql "UPDATE api_token SET active=false WHERE id=1"
    withApp (Just user) $ \call -> forM_ paths $ \path -> status <$> call "" path >>= (`shouldBe` 401)
  it "retains the exact pre-enforcement suggestion DTO but retires counts after activation and pause" $ do
    neverActivated
    withApp Nothing $ \call -> do
      r <- call "synthetic-1" "/suggestions"
      status r `shouldBe` 200
      map (field "sfPartyId") (items r) `shouldBe` [Just (Number 5)]
      map (field "sfMutualCount") (items r) `shouldBe` [Just (Number 2)]
      sql "UPDATE social_v2_runtime SET enabled=true"
      items <$> call "synthetic-1" "/suggestions" >>= (`shouldBe` [])
      sql "UPDATE social_v2_runtime SET enabled=false"
      items <$> call "synthetic-1" "/suggestions" >>= (`shouldBe` [])
  it "retires inference when canonical pair or closure state exists anywhere, before activation" $ do
    neverActivated
    sql "INSERT INTO social_v2_pair(party_a,party_b,block_a) VALUES(4,5,true)"
    withApp Nothing $ \call -> items <$> call "synthetic-1" "/suggestions" >>= (`shouldBe` [])
    sql "DELETE FROM social_v2_pair"
    sql "INSERT INTO social_v2_preference(party_id,closed) VALUES(5,true)"
    withApp Nothing $ \call -> items <$> call "synthetic-1" "/suggestions" >>= (`shouldBe` [])
  it "fails closed with an incomplete migration, including a never-activated foundation" $
    bracket_
      (sql "ALTER FUNCTION social_v2_relationship_rows(bigint,text) RENAME TO social_fixture_relationship_rows")
      (sql "ALTER FUNCTION social_fixture_relationship_rows(bigint,text) RENAME TO social_v2_relationship_rows") $ do
        neverActivated
        withApp Nothing $ \call -> forM_ paths $ \path -> status <$> call "synthetic-1" path >>= (`shouldBe` 503)
  it "excludes organization identities under activated account-only eligibility" $ do
    sql "UPDATE party SET is_org=true WHERE id=2"
    withApp Nothing $ \call -> items <$> call "synthetic-1" "/friends" >>= (`shouldBe` [])
  where
    paths=["/followers","/following","/friends","/suggestions"]
    run action = runSqlPool action (envPool env)
    sql statement = run (rawExecute statement [])
    neverActivated = bracket_
      (sql "ALTER TABLE social_v2_runtime DISABLE TRIGGER social_v2_activation_memory")
      (sql "ALTER TABLE social_v2_runtime ENABLE TRIGGER social_v2_activation_memory")
      (sql "UPDATE social_v2_runtime SET enabled=false,activated_once=false")
    reset = do
      setEnv "SOCIAL_V2_ENABLED" "true"
      sql "TRUNCATE social_v2_command,social_v2_pair,social_v2_preference,party_follow RESTART IDENTITY"
      sql "UPDATE social_v2_runtime SET enabled=true"
      sql "UPDATE party SET is_org=false"
      sql "UPDATE user_credential SET active=true"
      sql "UPDATE api_token SET active=true,label=NULL,party_id=CASE WHEN id=6 THEN 1 ELSE id END"
      sql "INSERT INTO party_follow(follower_party_id,following_party_id,via_nfc,created_at) VALUES(1,2,true,'2026-01-01T23:00:00Z'),(2,1,false,'2026-01-01T23:00:00Z'),(1,3,false,'2026-01-01T23:00:00Z'),(4,1,false,'2026-01-01T23:00:00Z'),(2,5,false,'2026-01-01T23:00:00Z'),(3,5,false,'2026-01-01T23:00:00Z')"
    withApp captured action = do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      testWithApplication (pure (application env captured)) $ \port -> action $ \token path -> do
        req <- HTTP.parseRequest ("http://127.0.0.1:"<>show port<>path)
        HTTP.httpLbs req {HTTP.requestHeaders=[("Authorization","Bearer "<>token) | not (BS.null token)],
          HTTP.responseTimeout=HTTP.responseTimeoutMicro 10000000} manager
    status = statusCode . HTTP.responseStatus
    decoded response = case decode (HTTP.responseBody response) of Just value -> value; _ -> Null
    items response = case decoded response of Array values -> V.toList values; _ -> []
    field key (Object fields) = KM.lookup key fields
    field _ _ = Nothing
