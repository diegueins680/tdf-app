{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), decode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Database.Persist.Sql (rawExecute, runSqlPool)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)
import System.Environment (getEnv, setEnv)
import Test.Hspec
import TDF.Auth (AuthedUser, authContext)
import TDF.Config (AppConfig(..))
import TDF.DB (Env(..), makePool)
import TDF.Social.API (SocialV2API)
import TDF.Social.Server (socialV2Server)
import SessionSpec (sessionSpec)
import ChatSpec (chatSpec)
import ProfileSpec (profileSpec)
import RelationshipReadSpec (relationshipReadSpec)
import RelationshipReadBenchmark (benchmarkRelationshipReads)
import ProfileBenchmark (benchmarkProfiles)
import SessionBenchmark (benchmarkSession)

type ProtectedSocial = AuthProtect "bearer-token" :> SocialV2API
application :: Env -> Application
application env = serveWithContext (Proxy @ProtectedSocial) (authContext env) $
  hoistServerWithContext (Proxy @ProtectedSocial) (Proxy @'[AuthHandler Request AuthedUser])
    (`runReaderT` env) (socialV2Server :: ServerT ProtectedSocial (ReaderT Env Handler))

main :: IO ()
main = do
  connection <- BS.pack <$> getEnv "TDF_SOCIAL_HTTP_DB"
  pool <- makePool connection
  -- Authentication only demands this field. No developer/production config is loaded.
  let cfg = AppConfig {sessionCookieName = "social-fixture"}
      env = Env pool cfg
      sql statement = runSqlPool (rawExecute statement []) pool
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  testWithApplication (pure (application env)) $ \port -> do
    let call token method path body = do
          base <- HTTP.parseRequest ("http://127.0.0.1:" ++ show port ++ path)
          HTTP.httpLbs base {HTTP.method=method,
            HTTP.requestHeaders=[("Content-Type","application/json")] ++
              [("Authorization","Bearer "<>token) | not (BS.null token)],
            HTTP.requestBody=HTTP.RequestBodyLBS body, HTTP.responseTimeout=HTTP.responseTimeoutMicro 10000000} manager
        status response = statusCode (HTTP.responseStatus response)
        field key response = case decode (HTTP.responseBody response) of
          Just (Object fields) -> KM.lookup key fields
          _ -> Nothing
        command token target body = call token "POST" ("/v2/relationships/"<>target) body
    hspec $ do
      it "requires real bearer authentication even when the process gate is closed" $ do
        setEnv "SOCIAL_V2_ENABLED" "false"
        status <$> call "" "GET" "/v2/me" "" >>= (`shouldBe` 401)
        status <$> call "synthetic-1" "GET" "/v2/me" "" >>= (`shouldBe` 404)
      it "requires both process and database gates" $ do
        setEnv "SOCIAL_V2_ENABLED" "true"
        status <$> call "synthetic-1" "GET" "/v2/me" "" >>= (`shouldBe` 404)
        sql "UPDATE social_v2_runtime SET enabled=true"
        status <$> call "synthetic-1" "GET" "/v2/me" "" >>= (`shouldBe` 200)
      it "derives the actor from the token and rejects identity injection" $ do
        status <$> command "synthetic-1" "2" "{\"operation\":\"request\",\"expectedRevision\":0,\"requestKey\":\"request\"}" >>= (`shouldBe` 200)
        status <$> command "synthetic-2" "1" "{\"operation\":\"accept\",\"expectedRevision\":1,\"requestKey\":\"spoof\",\"actor\":1}" >>= (`shouldBe` 400)
        response <- command "synthetic-2" "1" "{\"operation\":\"accept\",\"expectedRevision\":1,\"requestKey\":\"accept\"}"
        status response `shouldBe` 200
        (decode (HTTP.responseBody response) :: Maybe Value) `shouldBe` Just (object
          ["partyId" .= (1::Int),"revision" .= (2::Int),"following" .= False,"requested" .= True,
           "incoming" .= True,"connected" .= True,"blocked" .= False,"muted" .= False,"dismissed" .= False])
      it "withdraws only the caller's consent and leaves the other intent intact" $ do
        removed <- command "synthetic-1" "2" "{\"operation\":\"disconnect\",\"expectedRevision\":2,\"requestKey\":\"withdraw\"}"
        status removed `shouldBe` 200
        field "requested" removed `shouldBe` Just (Bool False)
        field "incoming" removed `shouldBe` Just (Bool True)
        other <- call "synthetic-2" "GET" "/v2/relationships/1" ""
        field "requested" other `shouldBe` Just (Bool True)
        status <$> command "synthetic-1" "2" "{\"operation\":\"request\",\"expectedRevision\":3,\"requestKey\":\"renew-own-intent\"}" >>= (`shouldBe` 200)
      it "denies blocked reads and stale acceptance but preserves the owner's unblock control" $ do
        status <$> command "synthetic-2" "1" "{\"operation\":\"block\",\"expectedRevision\":4,\"requestKey\":\"block\"}" >>= (`shouldBe` 200)
        status <$> call "synthetic-1" "GET" "/v2/relationships/2" "" >>= (`shouldBe` 404)
        status <$> command "synthetic-1" "2" "{\"operation\":\"accept\",\"expectedRevision\":1,\"requestKey\":\"late\"}" >>= (`shouldBe` 404)
        status <$> call "synthetic-2" "GET" "/v2/relationships/1" "" >>= (`shouldBe` 200)
      it "serves an authorized Following page and rejects malformed cursors and page sizes" $ do
        response <- call "synthetic-2" "GET" "/v2/following?limit=1" ""
        status response `shouldBe` 200
        BS.isInfixOf "newest" (BL.toStrict (HTTP.responseBody response)) `shouldBe` True
        BS.isInfixOf "must not leak" (BL.toStrict (HTTP.responseBody response)) `shouldBe` False
        status <$> call "synthetic-2" "GET" "/v2/following?cursor=abc" "" >>= (`shouldBe` 400)
        status <$> call "synthetic-2" "GET" "/v2/following?limit=51" "" >>= (`shouldBe` 400)
      it "rechecks authoritative membership on the next HTTP request" $ do
        sql "DELETE FROM fan_follow WHERE fan_party_id=2"
        response <- call "synthetic-2" "GET" "/v2/following" ""
        (decode (HTTP.responseBody response) :: Maybe Value) `shouldBe`
          Just (object ["items" .= ([]::[Value]),"nextCursor" .= (Nothing::Maybe String)])
      it "denies inactive tokens and organization identities" $ do
        sql "UPDATE api_token SET active=false WHERE party_id=2"
        status <$> call "synthetic-2" "GET" "/v2/me" "" >>= (`shouldBe` 401)
        sql "UPDATE party SET is_org=true WHERE id=3"
        status <$> call "synthetic-3" "GET" "/v2/me" "" >>= (`shouldBe` 404)

      sessionSpec env
      chatSpec env
      profileSpec env
      relationshipReadSpec env

  benchmarkSession env
  benchmarkProfiles env
  benchmarkRelationshipReads env
