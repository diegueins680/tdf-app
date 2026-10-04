{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module ChatSpec (chatSpec) where

import Control.Exception (bracket_)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Database.Persist.Sql (rawExecute, rawSql, Single(..), SqlPersistT, runSqlPool)
import Database.PostgreSQL.Simple (SqlError(..), ExecStatus(..))
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import System.Environment (setEnv)
import Test.Hspec
import TDF.API.Chat (ChatAPI)
import TDF.Auth (AuthedUser(..), authContext, loadAuthedUser)
import TDF.DB (Env(..))
import TDF.Models (RoleEnum(..))
import TDF.Social.Chat (chatPolicyServer,mapChatSqlError)

type ProtectedChat = AuthProtect "bearer-token" :> ChatAPI
fallback :: ServerT ChatAPI (ReaderT Env Handler)
fallback = throwError err501 :<|> (\_ -> throwError err501)
  :<|> (\_ _ _ _ -> throwError err501) :<|> (\_ _ -> throwError err501)
application :: Env -> Maybe AuthedUser -> Application
application env captured = serveWithContext (Proxy @ProtectedChat) context $
  hoistServerWithContext (Proxy @ProtectedChat) (Proxy @'[AuthHandler Request AuthedUser])
    (`runReaderT` env) (\user -> chatPolicyServer user fallback)
  where
    context = case captured of
      Nothing -> authContext env
      Just user -> (mkAuthHandler (\_ -> pure user) :: AuthHandler Request AuthedUser) :. EmptyContext

chatSpec :: Env -> Spec
chatSpec env = before_ reset $ describe "legacy chat adapter with actual bearer HTTP and PostgreSQL" $ do
  it "keeps existing DTOs and returns only a participant's eligible thread and preview" $ withApp Nothing $ \call -> do
    r <- call "synthetic-1" "GET" "/chat/threads" ""
    status r `shouldBe` 200
    length (items r) `shouldBe` 1
    field "ctThreadId" (head (items r)) `shouldBe` Just (Number 1)
    field "ctLastMessage" (head (items r)) `shouldBe` Just (String "local cursor")
    BS.isInfixOf "foreign secret" (BL.toStrict (HTTP.responseBody r)) `shouldBe` False
  it "filters blocked previews and history while the process and database feature flags are paused" $ do
    sql "UPDATE social_v2_pair SET consent_a=false,consent_b=false,block_b=true WHERE party_a=1 AND party_b=2"
    setEnv "SOCIAL_V2_ENABLED" "false"
    sql "UPDATE social_v2_runtime SET enabled=false"
    withApp Nothing $ \call -> do
      threads <- call "synthetic-1" "GET" "/chat/threads" ""
      status threads `shouldBe` 200
      items threads `shouldBe` []
      status <$> call "synthetic-1" "GET" "/chat/threads/1/messages" "" >>= (`shouldBe` 404)
      status <$> call "synthetic-1" "POST" "/chat/threads/1/messages" "{\"csmBody\":\"denied\"}" >>= (`shouldBe` 403)
    messageCount >>= (`shouldBe` 3)
  it "does not let administrator context bypass a canonical block or participant check" $ do
    Just user <- run (loadAuthedUser "synthetic-1")
    sql "UPDATE social_v2_pair SET consent_a=false,consent_b=false,block_b=true WHERE party_a=1 AND party_b=2"
    withApp (Just user {auRoles=[Admin]}) $ \call -> do
      status <$> call "" "GET" "/chat/threads/1/messages" "" >>= (`shouldBe` 404)
      status <$> call "" "POST" "/chat/threads/dm/2" "" >>= (`shouldBe` 403)
      status <$> call "" "GET" "/chat/threads/2/messages" "" >>= (`shouldBe` 404)
  it "checks authorization before distinguishing any cursor or returning names/bodies" $ do
    sql "UPDATE social_v2_pair SET consent_a=false,consent_b=false,block_b=true WHERE party_a=1 AND party_b=2"
    withApp Nothing $ \call -> do
      a <- call "synthetic-1" "GET" "/chat/threads/1/messages?beforeId=3" ""
      b <- call "synthetic-1" "GET" "/chat/threads/1/messages?beforeId=999" ""
      c <- call "synthetic-3" "GET" "/chat/threads/1/messages?beforeId=2" ""
      map status [a,b,c] `shouldBe` [404,404,404]
      HTTP.responseBody a `shouldBe` HTTP.responseBody b
      HTTP.responseBody a `shouldBe` HTTP.responseBody c
  it "preserves message ordering, scoped cursors and deterministic page boundaries" $ withApp Nothing $ \call -> do
    first <- call "synthetic-1" "GET" "/chat/threads/1/messages?limit=1" ""
    map (field "cmId") (items first) `shouldBe` [Just (Number 2)]
    older <- call "synthetic-1" "GET" "/chat/threads/1/messages?beforeId=2&limit=1" ""
    map (field "cmId") (items older) `shouldBe` [Just (Number 1)]
    after <- call "synthetic-1" "GET" "/chat/threads/1/messages?afterId=1" ""
    map (field "cmId") (items after) `shouldBe` [Just (Number 2)]
    status <$> call "synthetic-1" "GET" "/chat/threads/1/messages?beforeId=3" "" >>= (`shouldBe` 404)
    status <$> call "synthetic-1" "GET" "/chat/threads/1/messages?beforeId=1&afterId=2" "" >>= (`shouldBe` 400)
    status <$> call "synthetic-1" "GET" "/chat/threads/1/messages?limit=201" "" >>= (`shouldBe` 400)
  it "rechecks history and previews after credential revocation, closure and disconnect" $ withApp Nothing $ \call -> do
    status <$> call "synthetic-1" "GET" "/chat/threads/1/messages" "" >>= (`shouldBe` 200)
    sql "UPDATE user_credential SET active=false WHERE party_id=2"
    items <$> call "synthetic-1" "GET" "/chat/threads" "" >>= (`shouldBe` [])
    status <$> call "synthetic-1" "GET" "/chat/threads/1/messages" "" >>= (`shouldBe` 404)
    sql "UPDATE user_credential SET active=true WHERE party_id=2"
    sql "INSERT INTO social_v2_preference(party_id,closed) VALUES(2,true)"
    status <$> call "synthetic-1" "GET" "/chat/threads/1/messages" "" >>= (`shouldBe` 404)
    sql "DELETE FROM social_v2_preference WHERE party_id=2"
    sql "UPDATE social_v2_pair SET consent_b=false WHERE party_a=1 AND party_b=2"
    status <$> call "synthetic-1" "GET" "/chat/threads/1/messages" "" >>= (`shouldBe` 404)
  it "requires real bearer auth and denies captured-token revocation on both reads and sends" $ do
    Just user <- run (loadAuthedUser "synthetic-1")
    withApp Nothing $ \call -> status <$> call "" "GET" "/chat/threads" "" >>= (`shouldBe` 401)
    sql "UPDATE api_token SET active=false WHERE id=1"
    withApp (Just user) $ \call -> do
      status <$> call "" "GET" "/chat/threads" "" >>= (`shouldBe` 401)
      status <$> call "" "POST" "/chat/threads/1/messages" "{\"csmBody\":\"denied\"}" >>= (`shouldBe` 401)
    messageCount >>= (`shouldBe` 3)
  it "creates one normalized thread under retries and sends only as the authenticated participant" $ do
    sql "INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b) VALUES(1,5,true,true)"
    withApp Nothing $ \call -> do
      a <- call "synthetic-1" "POST" "/chat/threads/dm/5" ""
      b <- call "synthetic-1" "POST" "/chat/threads/dm/5" ""
      status a `shouldBe` 200
      (decode (HTTP.responseBody a)::Maybe Value) `shouldBe` decode (HTTP.responseBody b)
      sent <- call "synthetic-1" "POST" "/chat/threads/1/messages" "{\"csmBody\":\"  useful collaboration  \"}"
      status sent `shouldBe` 200
      field "cmBody" (decoded sent) `shouldBe` Just (String "useful collaboration")
      field "cmSenderPartyId" (decoded sent) `shouldBe` Just (Number 1)
      status <$> call "synthetic-1" "POST" "/chat/threads/1/messages" "{\"csmBody\":\"x\",\"senderPartyId\":2}" >>= (`shouldBe` 400)
  it "does not auto-consent from manufactured mutual follows after activation" $ do
    sql "INSERT INTO party_follow(follower_party_id,following_party_id) VALUES(1,5),(5,1)"
    withApp Nothing $ \call -> status <$> call "synthetic-1" "POST" "/chat/threads/dm/5" "" >>= (`shouldBe` 403)
  it "fails closed if a required migration function is missing after activation" $
    bracket_
      (sql "ALTER FUNCTION social_v2_chat_threads(bigint) RENAME TO social_fixture_missing_threads")
      (sql "ALTER FUNCTION social_fixture_missing_threads(bigint) RENAME TO social_v2_chat_threads") $
        withApp Nothing $ \call -> do
          status <$> call "synthetic-1" "GET" "/chat/threads" "" >>= (`shouldBe` 503)
          status <$> call "synthetic-1" "GET" "/chat/threads/1/messages" "" >>= (`shouldBe` 503)
  it "maps only known database denial codes and hides SQL details" $ do
    let dbError state message = SqlError state FatalError message "private message text" "private detail"
    errHTTPCode (mapChatSqlError (dbError "42501" "social_dm_not_permitted")) `shouldBe` 403
    errHTTPCode (mapChatSqlError (dbError "40P01" "deadlock detected")) `shouldBe` 503
    errHTTPCode (mapChatSqlError (dbError "42501" "unexpected table permission")) `shouldBe` 500
    BS.isInfixOf "private" (BL.toStrict (errBody (mapChatSqlError (dbError "23505" "private")))) `shouldBe` False
  where
    run action = runSqlPool action (envPool env)
    sql statement = run (rawExecute statement [])
    messageCount = do
      [Single n] <- run (rawSql "SELECT count(*) FROM chat_message" [] :: SqlPersistT IO [Single Int])
      pure n
    reset = do
      setEnv "SOCIAL_V2_ENABLED" "true"
      sql "TRUNCATE social_v2_command,social_v2_pair,social_v2_preference,party_follow"
      sql "UPDATE social_v2_runtime SET enabled=true"
      sql "UPDATE party SET is_org=false"
      sql "UPDATE user_credential SET active=true"
      sql "UPDATE api_token SET active=true,label=NULL,party_id=CASE WHEN id=6 THEN 1 ELSE id END"
      sql "INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b) VALUES(1,2,true,true),(2,3,true,true)"
      sql "TRUNCATE chat_message,chat_thread RESTART IDENTITY"
      sql "INSERT INTO chat_thread(dm_party_a,dm_party_b) VALUES(1,2),(2,3)"
      sql "INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'local first'),(1,2,'local cursor'),(2,2,'foreign secret')"
    withApp captured action = do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      testWithApplication (pure (application env captured)) $ \port -> action $ \token method path body -> do
        req <- HTTP.parseRequest ("http://127.0.0.1:"<>show port<>path)
        HTTP.httpLbs req {HTTP.method=method,HTTP.requestBody=HTTP.RequestBodyLBS body,
          HTTP.requestHeaders=[("Content-Type","application/json")]++[("Authorization","Bearer "<>token) | not (BS.null token)],
          HTTP.responseTimeout=HTTP.responseTimeoutMicro 10000000} manager
    status = statusCode . HTTP.responseStatus
    decoded response = case decode (HTTP.responseBody response) of Just value -> value; _ -> Null
    items response = case decoded response of Array values -> V.toList values; _ -> []
    field key (Object fields) = KM.lookup key fields
    field _ _ = Nothing
