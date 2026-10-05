{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless,void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql hiding (loadConfig)
import System.Environment (getEnv,setEnv,lookupEnv)
import Text.Read (readMaybe)
import TDF.Config (loadConfig,AppConfig(..),EmailConfig(..))
import TDF.DB (Env(..))
import TDF.DisposableTicketDatabase (safeConfirmationDatabase)
import TDF.Ticketing.Confirmation (startTicketConfirmationWorker)

main :: IO ()
main = do
  dsn <- getEnv "TICKET_CONFIRMATION_TEST_DSN"
  ci <- (== Just "true") <$> lookupEnv "CI"
  overrides <- traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"]
  unless (safeConfirmationDatabase ci overrides dsn) $
    fail "Refusing non-local confirmation fixture routing or libpq overrides"
  port <- getEnv "TICKET_CONFIRMATION_SMTP_PORT" >>= maybe (fail "Invalid local SMTP port") pure . readMaybe
  unless (port>1024 && port<65536) (fail "Refusing invalid test port")
  cfg <- loadConfig
  runNoLoggingT $ withPostgresqlPool (BS.pack dsn) 4 $ \pool -> liftIO $ do
    names <- runSqlPool (rawSql "SELECT current_database()" []) pool
    unless (names == [Single ("tdf_ticket_confirmation_worker_test" :: Text)]) $
      fail "Refusing to mutate a non-test database"
    runSqlPool (do
      rawExecute "DELETE FROM event_ticket_confirmation_delivery" []
      rawExecute "UPDATE event_ticket_order SET status='paid' WHERE id=1" []
      rawExecute "UPDATE social_event SET title='Synthetic <script>alert(1)</script>' WHERE id=1" []
      rawExecute "UPDATE event_ticket SET status='issued',checked_in_at=NULL,current_holder_party_id=NULL WHERE order_ref_id=1" []
      void (rawSql "SELECT event_ticket_queue_confirmation(1) IS NULL" [] :: SqlPersistT IO [Single Bool])) pool
    let smtp = EmailConfig "TDF test" "noreply@example.invalid" "127.0.0.1" port "synthetic" "synthetic" False []
        env = Env pool cfg{appBaseUrl=Just "https://www.tdfrecords.net",emailConfig=Just smtp}
        await 0 = fail "Local SMTP confirmation did not complete"
        await remaining = do
          states <- runSqlPool (rawSql "SELECT state,attempts FROM event_ticket_confirmation_delivery WHERE order_id=1" [] :: SqlPersistT IO [(Single Text,Single Int)]) pool
          case states of
            [(Single "accepted",Single 2)] -> pure ()
            [(Single "pending",Single 1)] -> do
              runSqlPool (rawExecute "UPDATE event_ticket_confirmation_delivery SET next_attempt_at=NOW() WHERE order_id=1" []) pool
              threadDelay 100000
              await (remaining-1)
            _ -> threadDelay 100000 >> await (remaining-1)
    setEnv "TICKET_CONFIRMATION_EMAIL_ENABLED" "true"
    startTicketConfirmationWorker env
    await (200 :: Int)
    putStrLn "Real local SMTP refusal, durable retry and SMTP acceptance passed (not external inbox delivery)."
