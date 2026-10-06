{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module ProfileSpec (profileSpec) where

import Control.Exception (bracket_)
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
import TDF.API.SocialProfiles (SocialProfilesAPI)
import TDF.Auth (AuthedUser(..), authContext, loadAuthedUser)
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(..))
import TDF.Social.Profiles (profileList, profileGet)

type ProtectedProfiles = AuthProtect "bearer-token" :> SocialProfilesAPI
application :: Env -> Maybe AuthedUser -> Application
application env captured = serveWithContext (Proxy @ProtectedProfiles) context $
  hoistServerWithContext (Proxy @ProtectedProfiles) (Proxy @'[AuthHandler Request AuthedUser])
    (`runReaderT` env) (\user -> profileList user (\_ -> throwError err501) :<|> profileGet user (\_ -> throwError err501))
  where
    context = case captured of
      Nothing -> authContext env
      Just user -> (mkAuthHandler (\_ -> pure user) :: AuthHandler Request AuthedUser) :. EmptyContext

profileSpec :: Env -> Spec
profileSpec env = before_ reset $ describe "profile reads with real bearer HTTP and PostgreSQL" $ do
  it "keeps all five DTO fields and Unicode name fallback without requiring a connection" $ withApp Nothing $ \call -> do
    response <- call "synthetic-1" "/profiles/2"
    status response `shouldBe` 200
    field "sppPartyId" (decoded response) `shouldBe` Just (Number 2)
    field "sppDisplayName" (decoded response) `shouldBe` Just (String "Preferred name")
    field "sppAvatarUrl" (decoded response) `shouldBe` Just (String "https://example.test/avatar")
    field "sppBio" (decoded response) `shouldBe` Just (String "profile bio 2")
    field "sppCity" (decoded response) `shouldBe` Just (String "Quito")
    sql "UPDATE fan_profile SET display_name=E'\\t  ' WHERE fan_party_id=2"
    fallback <- call "synthetic-1" "/profiles/2"
    field "sppDisplayName" (decoded fallback) `shouldBe` Just (String "Synthetic 2")
  it "filters the entire blocked profile while preserving requested batch order" $ do
    sql "INSERT INTO social_v2_pair(party_a,party_b,block_b) VALUES(1,2,true)"
    withApp Nothing $ \call -> do
      response <- call "synthetic-1" "/profiles?partyId=3&partyId=2&partyId=999&partyId=1"
      status response `shouldBe` 200
      map (field "sppPartyId") (items response) `shouldBe` [Just (Number 3),Just (Number 1)]
      BS.isInfixOf "Preferred name" (BL.toStrict (HTTP.responseBody response)) `shouldBe` False
      BS.isInfixOf "profile bio 2" (BL.toStrict (HTTP.responseBody response)) `shouldBe` False
  it "uses the same not-found response for absent and denied profiles, including administrators" $ do
    Just user <- run (loadAuthedUser "synthetic-1")
    sql "INSERT INTO social_v2_pair(party_a,party_b,block_a) VALUES(1,2,true)"
    withApp (Just user {auRoles=[Admin]}) $ \call -> do
      denied <- call "" "/profiles/2"
      missing <- call "" "/profiles/999"
      map status [denied,missing] `shouldBe` [404,404]
      HTTP.responseBody denied `shouldBe` HTTP.responseBody missing
  it "retains block enforcement while both feature flags are paused" $ do
    sql "INSERT INTO social_v2_pair(party_a,party_b,block_b) VALUES(1,2,true)"
    sql "UPDATE social_v2_runtime SET enabled=false"
    setEnv "SOCIAL_V2_ENABLED" "false"
    withApp Nothing $ \call -> status <$> call "synthetic-1" "/profiles/2" >>= (`shouldBe` 404)
  it "does not treat mute, non-discoverability or absent connection as direct profile privacy" $ do
    sql "INSERT INTO social_v2_pair(party_a,party_b,mute_a) VALUES(1,2,true)"
    sql "INSERT INTO social_v2_preference(party_id,discoverable) VALUES(2,false)"
    withApp Nothing $ \call -> status <$> call "synthetic-1" "/profiles/2" >>= (`shouldBe` 200)
  it "rechecks target credential, closure and actor closure before returning fields" $ withApp Nothing $ \call -> do
    sql "UPDATE user_credential SET active=false WHERE party_id=2"
    status <$> call "synthetic-1" "/profiles/2" >>= (`shouldBe` 404)
    sql "UPDATE user_credential SET active=true WHERE party_id=2"
    sql "INSERT INTO social_v2_preference(party_id,closed) VALUES(2,true)"
    status <$> call "synthetic-1" "/profiles/2" >>= (`shouldBe` 404)
    sql "INSERT INTO social_v2_preference(party_id,closed) VALUES(1,true)"
    response <- call "synthetic-1" "/profiles?partyId=1&partyId=3"
    status response `shouldBe` 200
    items response `shouldBe` []
  it "requires real bearer auth and rechecks a captured token even for an empty batch" $ do
    Just user <- run (loadAuthedUser "synthetic-1")
    withApp Nothing $ \call -> status <$> call "" "/profiles/2" >>= (`shouldBe` 401)
    sql "UPDATE api_token SET active=false WHERE id=1"
    withApp (Just user) $ \call -> do
      status <$> call "" "/profiles/2" >>= (`shouldBe` 401)
      status <$> call "" "/profiles" >>= (`shouldBe` 401)
  it "bounds and validates identifiers before serving rows" $ withApp Nothing $ \call -> do
    mapM_ (\path -> status <$> call "synthetic-1" path >>= (`shouldBe` 400))
      ["/profiles/0","/profiles?partyId=0","/profiles?partyId=2&partyId=2",
       "/profiles?"<>concat ["partyId="<>show n<>"&" | n <- [1..101::Int]]]
    response <- call "synthetic-1" "/profiles"
    status response `shouldBe` 200
    items response `shouldBe` []
    status <$> call "synthetic-1" ("/profiles?"<>concat ["partyId="<>show n<>"&" | n <- [1..100::Int]]) >>= (`shouldBe` 200)
  it "excludes organization identities within the activated account-only pilot" $ do
    sql "UPDATE party SET is_org=true WHERE id=2"
    withApp Nothing $ \call -> do
      status <$> call "synthetic-1" "/profiles/2" >>= (`shouldBe` 404)
      status <$> call "synthetic-2" "/profiles/1" >>= (`shouldBe` 404)
  it "fails closed on a partial migration without relying on activation or existing pairs" $
    bracket_
      (sql "ALTER FUNCTION social_v2_profiles(bigint,bigint[]) RENAME TO social_fixture_profiles")
      (sql "ALTER FUNCTION social_fixture_profiles(bigint,bigint[]) RENAME TO social_v2_profiles") $
        withApp Nothing $ \call -> do
          neverActivated
          status <$> call "synthetic-1" "/profiles/2" >>= (`shouldBe` 503)
          status <$> call "synthetic-1" "/profiles" >>= (`shouldBe` 503)
  it "preserves never-activated legacy visibility until canonical authority applies" $ do
    neverActivated
    sql "UPDATE user_credential SET active=false WHERE party_id=2"
    withApp Nothing $ \call -> do
      status <$> call "synthetic-1" "/profiles/2" >>= (`shouldBe` 200)
      sql "INSERT INTO social_v2_pair(party_a,party_b,block_b) VALUES(1,2,true)"
      status <$> call "synthetic-1" "/profiles/2" >>= (`shouldBe` 404)
  where
    run action = runSqlPool action (envPool env)
    sql statement = run (rawExecute statement [])
    -- Fixture-only reset of the irreversible activation latch to represent a fresh
    -- installation. Ordinary UPDATE must not reset it (covered by DM/schema tests).
    neverActivated = bracket_
      (sql "ALTER TABLE social_v2_runtime DISABLE TRIGGER social_v2_activation_memory")
      (sql "ALTER TABLE social_v2_runtime ENABLE TRIGGER social_v2_activation_memory")
      (sql "UPDATE social_v2_runtime SET enabled=false,activated_once=false")
    reset = do
      setEnv "SOCIAL_V2_ENABLED" "true"
      sql "TRUNCATE social_v2_command,social_v2_pair,social_v2_preference"
      sql "UPDATE social_v2_runtime SET enabled=true"
      sql "UPDATE party SET is_org=false"
      sql "UPDATE user_credential SET active=true"
      sql "UPDATE api_token SET active=true,label=NULL,party_id=CASE WHEN id=6 THEN 1 ELSE id END"
      sql "UPDATE fan_profile SET display_name=U&'\\2003 Preferred name \\2003',avatar_url='https://example.test/avatar',city='Quito' WHERE fan_party_id=2"
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
