{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Database.Persist.Sql hiding (loadConfig)
import Database.Persist.Postgresql (withPostgresqlPool)
import System.Environment (getEnv)
import Test.Hspec
import TDF.Config (loadConfig,AppConfig(..))
import TDF.DB (Env(..))
import TDF.Ticketing.Confirmation

main :: IO ()
main = do
  dsn <- getEnv "TICKET_CONFIRMATION_TEST_DSN"
  cfg <- loadConfig
  runNoLoggingT $ withPostgresqlPool (BS.pack dsn) 8 $ \pool -> liftIO $ do
    names <- runSqlPool (rawSql "SELECT current_database()" []) pool
    unless (names == [Single ("tdf_ticket_confirmation_worker_test" :: Text)]) $
      fail "Refusing to mutate a non-test database"
    let env = Env pool cfg{appBaseUrl=Just "https://www.tdfrecords.net",emailConfig=Nothing}
        sql statement = runSqlPool (rawExecute statement []) pool
        queue = void $ runSqlPool (rawSql "SELECT event_ticket_queue_confirmation(1) IS NULL" [] :: SqlPersistT IO [Single Bool]) pool
        reset = do
          sql "DELETE FROM event_ticket_confirmation_delivery"
          sql "UPDATE event_ticket_order SET status='paid' WHERE id=1"
          sql "UPDATE event_ticket SET status='issued',checked_in_at=NULL,current_holder_party_id=NULL WHERE order_ref_id=1"
        state = runSqlPool (rawSql "SELECT state FROM event_ticket_confirmation_delivery WHERE order_id=1" [] :: SqlPersistT IO [Single Text]) pool
    hspec $ before_ reset $ describe "durable public ticket confirmation worker on PostgreSQL" $ do
      it "allows only one of eight workers to send a queued confirmation" $ do
        queue
        sent <- newMVar ([] :: [Confirmation])
        let send receipt = modifyMVar_ sent (pure . (receipt:)) >> threadDelay 10000
        results <- mapConcurrently (const (processConfirmationWith env send)) [1..8 :: Int]
        length (filter id results) `shouldBe` 1
        messages <- readMVar sent
        length messages `shouldBe` 1
        confirmationCodes (head messages) `shouldBe` ["TDF-ABCDEF012345","TDF-ABCDEF012346"]
        confirmationDate (head messages) `shouldBe` "2026-10-24 14:00 (America/Guayaquil)"
        state `shouldReturn` [Single "accepted"]
        queue
        processConfirmationWith env send `shouldReturn` False
      it "retains a failed send for retry without storing exception PII" $ do
        queue
        processConfirmationWith env (const (ioError (userError "private@example.invalid token=SECRET"))) `shouldReturn` True
        state `shouldReturn` [Single "pending"]
        errors <- runSqlPool (rawSql "SELECT last_error_code FROM event_ticket_confirmation_delivery" [] :: SqlPersistT IO [Single Text]) pool
        errors `shouldBe` [Single "delivery_failed"]
        processConfirmationWith env (const (fail "Backoff was bypassed")) `shouldReturn` False
        sql "UPDATE event_ticket_confirmation_delivery SET next_attempt_at=NOW()-INTERVAL '1 minute'"
        processConfirmationWith env (const (pure ())) `shouldReturn` True
        state `shouldReturn` [Single "accepted"]
      it "does not send codes after cancellation" $ do
        queue
        sql "UPDATE event_ticket_order SET status='cancelled' WHERE id=1"
        processConfirmationWith env (const (fail "Cancelled ticket sent")) `shouldReturn` True
        state `shouldReturn` [Single "cancelled"]
      it "does not send a transferred credential to the former holder" $ do
        queue
        sql "UPDATE event_ticket SET current_holder_party_id='99' WHERE id=1"
        sent <- newMVar ([] :: [Confirmation])
        processConfirmationWith env (\receipt -> modifyMVar_ sent (pure . (receipt:))) `shouldReturn` True
        messages <- readMVar sent
        map confirmationCodes messages `shouldBe` [["TDF-ABCDEF012346"]]
      it "cancels delivery when no unused buyer-held tickets remain" $ do
        queue
        sql "UPDATE event_ticket SET status='checked_in',checked_in_at=NOW() WHERE order_ref_id=1"
        processConfirmationWith env (const (fail "Used ticket sent")) `shouldReturn` True
        state `shouldReturn` [Single "cancelled"]
