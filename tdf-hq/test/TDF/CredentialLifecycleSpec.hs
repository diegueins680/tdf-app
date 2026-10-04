{-# LANGUAGE OverloadedStrings #-}
module TDF.CredentialLifecycleSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception (finally)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist (Entity(..), toPersistValue)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Hspec
import TDF.Auth (lockCredentialForSession, revokeInteractiveSessions)
import TDF.DTO (LoginResponse)
import TDF.Models (PartyId, UserCredentialId, UserCredential(..))
import TDF.ServerAuth (GoogleProfile(..), completeGoogleLogin)

-- This starts after provider cryptographic verification. No provider request is
-- made, and the database is the same fully migrated synthetic HTTP fixture.
spec :: Spec
spec = describe "credential-lifecycle-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_CREDENTIAL_LIFECYCLE_DATABASE_URL")
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-credential-lifecycle.sh"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 4)) $
      afterAll destroyAllResources $ do
        it "revalidates a bound Google credential after disable wins the lock" $ \pool -> do
          (_, credential, profile) <- fixture pool
          ready <- newEmptyMVar
          release <- newEmptyMVar
          label <- observerLabel
          withAsync (runSqlPool (do
            disable credential
            liftIO (putMVar ready () >> takeMVar release)) pool) $ \disabling ->
            (do
              within (takeMVar ready)
              withAsync (runSqlPool (observeAs label >> google profile) pool) $ \login ->
                (do
                  waitBlocked pool label
                  putMVar release ()
                  within (wait disabling)
                  within (wait login) >>= (`shouldSatisfy` isLeft)
                  activeGoogleCount pool credential `shouldReturn` 0
                ) `finally` void (tryPutMVar release ())
              ) `finally` void (tryPutMVar release ())
        it "revokes a Google session when disable follows committed issuance" $ \pool -> do
          (_, credential, profile) <- fixture pool
          ready <- newEmptyMVar
          release <- newEmptyMVar
          label <- observerLabel
          withAsync (runSqlPool (do
            result <- google profile
            liftIO (putMVar ready () >> takeMVar release)
            pure result) pool) $ \loggingIn ->
            (do
              within (takeMVar ready)
              withAsync (runSqlPool (observeAs label >> disable credential) pool) $ \disabling ->
                (do
                  waitBlocked pool label
                  putMVar release ()
                  within (wait loggingIn) >>= (`shouldSatisfy` isRight)
                  within (wait disabling)
                  activeGoogleCount pool credential `shouldReturn` 0
                ) `finally` void (tryPutMVar release ())
              ) `finally` void (tryPutMVar release ())

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://localhost/",
   "postgresql://postgres:postgres@postgres:5432/"]

within :: IO a -> IO a
within action = timeout 20000000 action >>= maybe (fail "Credential lifecycle barrier timed out") pure

-- PostgreSQL truncates application_name at 63 bytes. Keep the entire unique
-- UUID inside that bound so overlapping test runs cannot satisfy our barrier.
observerLabel :: IO Text
observerLabel = ("tdf_google_" <>) . UUID.toText <$> nextRandom

observeAs :: Text -> SqlPersistT IO ()
observeAs label = do
  _ <- rawSql "SELECT set_config('application_name', ?, true)"
    [toPersistValue label] :: SqlPersistT IO [Single Text]
  pure ()

waitBlocked :: ConnectionPool -> Text -> IO ()
waitBlocked pool label = within loop
  where
    loop = do
      rows <- runSqlPool (rawSql
        "SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND application_name=? AND wait_event_type='Lock'"
        [toPersistValue label] :: SqlPersistT IO [Single Int64]) pool
      if rows == [Single 1] then pure () else threadDelay 10000 >> loop

disable :: UserCredentialId -> SqlPersistT IO ()
disable credentialId = do
  current <- lockCredentialForSession credentialId
  case current of
    Nothing -> liftIO (fail "Synthetic credential disappeared")
    Just (Entity _ credential) -> do
      rawExecute "UPDATE user_credential SET active=false WHERE id=?" [toPersistValue credentialId]
      revokeInteractiveSessions (userCredentialPartyId credential)

google :: GoogleProfile -> SqlPersistT IO (Either Text LoginResponse)
google = completeGoogleLogin Nothing Nothing Nothing Nothing Nothing

activeGoogleCount :: ConnectionPool -> UserCredentialId -> IO Int64
activeGoogleCount pool credential = do
  rows <- runSqlPool (rawSql
    "SELECT count(*) FROM api_token t JOIN user_credential c ON c.party_id=t.party_id WHERE c.id=? AND t.active AND t.label LIKE 'google-login:%'"
    [toPersistValue credential]) pool
  case rows of
    [Single count] -> pure count
    _ -> fail "Expected one token count"

fixture :: ConnectionPool -> IO (PartyId, UserCredentialId, GoogleProfile)
fixture pool = do
  subject <- ("synthetic-google-lifecycle-" <>) . UUID.toText <$> nextRandom
  runSqlPool (do
    parties <- rawSql
      "INSERT INTO party(display_name,is_org,primary_email,created_at) VALUES (?,false,'google-lifecycle@example.test',now()) RETURNING id"
      [toPersistValue subject]
    partyId <- case parties of
      [Single key] -> pure key
      _ -> liftIO (fail "Expected synthetic Party")
    credentials <- rawSql
      "INSERT INTO user_credential(party_id,username,password_hash,active) VALUES (?,?,'synthetic-unused-password',true) RETURNING id"
      [toPersistValue partyId,toPersistValue subject]
    credential <- case credentials of
      [Single key] -> pure key
      _ -> liftIO (fail "Expected synthetic credential")
    rawExecute
      "INSERT INTO party_security_role(party_id,role_id,approval_mode,active) SELECT ?,id,'bootstrap',true FROM security_role WHERE code='admin' AND active"
      [toPersistValue partyId]
    rawExecute
      "INSERT INTO auth_provider_identity(issuer,subject,credential_id,verification_method) VALUES ('https://accounts.google.com',?,?,'new-account')"
      [toPersistValue subject,toPersistValue credential]
    pure (partyId, credential, GoogleProfile "https://accounts.google.com" subject
      "google-lifecycle@example.test" Nothing Nothing)) pool
