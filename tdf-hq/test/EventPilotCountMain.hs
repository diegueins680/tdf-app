{-# LANGUAGE OverloadedStrings #-}

-- Runs only against the disposable mixed research/discovery boundary fixture.
import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (destroyAllResources)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (SqlPersistT, rawExecute, runSqlPool, transactionUndo)
import System.Environment (getEnv)
import TDF.Services.EventDiscovery (countEventPilotIdentitiesDb)

main :: IO ()
main = do
  dsn <- BS.pack <$> getEnv "TDF_EVENT_BOUNDARY_TEST_DATABASE_URL"
  bracket (runNoLoggingT (createPostgresqlPool dsn 1)) destroyAllResources $ \pool ->
    runSqlPool verifyCounts pool
  putStrLn "Pilot status count passed mixed capacity, canonical deduplication and suppression checks."

verifyCounts :: SqlPersistT IO ()
verifyCounts = do
  -- The fixture has 18 active candidates, three imported canonical events and
  -- one candidate linked to an import: shared capacity is 20, not 18 or 21.
  expectCount "mixed research/import identities" 20
  rawExecute "UPDATE external_event_ref SET source_status='suppressed' WHERE provider='fixture' AND external_id='discovery-1'" []
  expectCount "another reference still owns the same canonical event" 20
  rawExecute "UPDATE external_event_ref SET source_status='suppressed' WHERE provider='second' AND external_id='discovery-1'" []
  expectCount "all references suppressed release one slot" 19
  rawExecute "UPDATE external_event_ref SET source_status='draft:on_sale' WHERE provider='fixture' AND external_id='discovery-1'" []
  expectCount "restoring one reference consumes exactly one slot" 20
  transactionUndo

expectCount :: String -> Int -> SqlPersistT IO ()
expectCount label expected = do
  actual <- countEventPilotIdentitiesDb
  liftIO $ unless (actual == expected) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)
