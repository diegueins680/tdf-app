{-# LANGUAGE OverloadedStrings #-}
module TDF.ProviderIdentitySpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, void)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Text (Text)
import Database.Persist (Entity(..))
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), fromSqlKey, rawExecute, rawSql, runSqlPool)
import System.Environment (lookupEnv)
import Test.Hspec
import TDF.DTO (LoginRequest(..))
import TDF.ServerAuth (resolveGoogleCredential)

spec :: Spec
spec = describe "provider-identity-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_PROVIDER_IDENTITY_TEST_DB")
  case configured of
    Nothing -> it "requires the isolated integration runner" $ pendingWith "Run scripts/test-provider-identity.sh"
    Just connection | not ("dbname=tdf_provider_identity_test_" `isInfixOf` connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 4)) $ do
      it "does not select accounts by matching contact details" $ \pool -> do
        resolve pool "new-subject" Nothing `shouldReturn` Right Nothing
        countBindings pool `shouldReturn` 0
      it "requires the current password and rejects disabled accounts" $ \pool -> do
        resolve pool "wrong" (proof "one" "wrong") >>= (`shouldSatisfy` isLeft)
        resolve pool "disabled" (proof "disabled" "correct-password") >>= (`shouldSatisfy` isLeft)
        countBindings pool `shouldReturn` 0
      it "binds the exact credential despite shared email and permits a different subject for a separate account" $ \pool -> do
        resolve pool "subject-one" (proof "one" "correct-password") `shouldReturn` Right (Just 1)
        resolve pool "subject-two" (proof "two" "correct-password") `shouldReturn` Right (Just 2)
        resolve pool "subject-one" Nothing `shouldReturn` Right (Just 1)
        countBindings pool `shouldReturn` 2
      it "rejects reassignment even when the other account password is supplied" $ \pool -> do
        resolve pool "subject-one" (proof "two" "correct-password") >>= (`shouldSatisfy` isLeft)
        resolve pool "subject-one" Nothing `shouldReturn` Right (Just 1)
      it "preserves subject binding after an email change and makes retries idempotent" $ \pool -> do
        runSqlPool (rawExecute "UPDATE party SET primary_email='changed@example.test' WHERE id=1" []) pool
        resolve pool "subject-one" Nothing `shouldReturn` Right (Just 1)
        resolve pool "subject-one" (proof "one" "correct-password") `shouldReturn` Right (Just 1)
        countBindings pool `shouldReturn` 2
      it "serializes concurrent linking and leaves one immutable binding" $ \pool -> do
        boxes <- forM [1..4 :: Int] $ \_ -> do
          box <- newEmptyMVar
          void $ forkIO $ do
            result <- try (resolve pool "concurrent" (proof "one" "correct-password")) :: IO (Either SomeException (Either Text (Maybe Int64)))
            putMVar box result
          pure box
        results <- mapM takeMVar boxes
        map (either (const False) (== Right (Just 1))) results `shouldBe` replicate 4 True
        countBindings pool `shouldReturn` 3
      it "rolls back a binding on transaction failure without altering credentials" $ \pool -> do
        result <- try (runSqlPool (do
          _ <- resolveGoogleCredential "https://accounts.google.com" "rolled-back" (proof "one" "correct-password")
          rawExecute "SELECT 1/0" []) pool) :: IO (Either SomeException ())
        result `shouldSatisfy` isLeft
        resolve pool "rolled-back" Nothing `shouldReturn` Right Nothing
        rows <- runSqlPool (rawSql "SELECT count(*) FROM user_credential" []) pool
        rows `shouldBe` [Single (3 :: Int64)]
      it "denies an inactive bound credential without returning it" $ \pool -> do
        runSqlPool (rawExecute "UPDATE user_credential SET active=false WHERE id=1" []) pool
        resolve pool "subject-one" Nothing >>= (`shouldSatisfy` isLeft)

proof :: Text -> Text -> Maybe LoginRequest
proof name passwordValue = Just (LoginRequest name passwordValue)
resolve :: ConnectionPool -> Text -> Maybe LoginRequest -> IO (Either Text (Maybe Int64))
resolve pool subject p = fmap (fmap (fmap (\(Entity key _) -> fromSqlKey key))) $
  runSqlPool (resolveGoogleCredential "https://accounts.google.com" subject p) pool
countBindings :: ConnectionPool -> IO Int64
countBindings pool = do
  rows <- runSqlPool (rawSql "SELECT count(*) FROM auth_provider_identity" []) pool
  case rows of
    [Single n] -> pure n
    _ -> fail "Expected binding count"
