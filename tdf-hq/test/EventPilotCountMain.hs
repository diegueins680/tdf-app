{-# LANGUAGE OverloadedStrings #-}

-- Runs only against the disposable mixed research/discovery boundary fixture.
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (destroyAllResources)
import Data.List (isInfixOf)
import Data.Time (UTCTime(..), getCurrentTime)
import Database.Persist (Entity(..), getBy)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, rawExecute, runSqlPool, transactionUndo)
import System.Environment (getEnv)
import qualified TDF.Models.SocialEventsModels as Social
import TDF.Services.EventDiscovery
  ( DiscoverySyncStats(..), beginEventDiscoveryRun, completeEventDiscoverySourceRun
  , countEventPilotIdentitiesDb )

main :: IO ()
main = do
  dsn <- BS.pack <$> getEnv "TDF_EVENT_BOUNDARY_TEST_DATABASE_URL"
  bracket (runNoLoggingT (createPostgresqlPool dsn 1)) destroyAllResources $ \pool -> do
    runSqlPool verifyCounts pool
    verifyEmptySourceCompletion pool
  putStrLn "Pilot status count passed mixed capacity, canonical deduplication and suppression checks."

verifyEmptySourceCompletion :: ConnectionPool -> IO ()
verifyEmptySourceCompletion pool = do
  let exec sql = runSqlPool (rawExecute sql []) pool
      provider = "fixture-structured"
  exec "CREATE TABLE external_event_discovery_run (id bigserial PRIMARY KEY, provider text NOT NULL, run_date date NOT NULL, scheduled_for timestamptz, status text NOT NULL, cities_count integer NOT NULL, events_seen integer NOT NULL, events_created integer NOT NULL, events_updated integer NOT NULL, venues_created integer NOT NULL, artists_created integer NOT NULL, error_message text, started_at timestamptz NOT NULL, finished_at timestamptz, UNIQUE(provider, scheduled_for))"
  Just (Entity sourceKey _) <- runSqlPool (getBy (Social.UniqueEventDiscoverySource provider)) pool
  clockNow <- getCurrentTime
  let now = clockNow{utctDayTime = fromInteger (floor (utctDayTime clockNow))}
  Just handle <- beginEventDiscoveryRun pool provider now now
  let complete = completeEventDiscoverySourceRun pool sourceKey handle now provider [] [] (DiscoverySyncStats 0 0 0 0 0)
      expectRunning = do
        Just (Entity _ run) <- runSqlPool (getBy (Social.UniqueExternalEventDiscoverySlot provider (Just now))) pool
        Just (Entity _ source) <- runSqlPool (getBy (Social.UniqueEventDiscoverySource provider)) pool
        unless (Social.externalEventDiscoveryRunStatus run == "running"
          && Social.externalEventDiscoveryRunFinishedAt run == Nothing
          && Social.eventDiscoverySourceLastSuccessAt source == Nothing) $
          fail "Rejected completion wrote success evidence"
  exec "UPDATE event_discovery_source SET enabled=false WHERE source_key='fixture-structured'"
  expectFailure "Event source disabled or unavailable before completion" complete
  expectRunning
  exec "UPDATE event_discovery_source SET enabled=true WHERE source_key='fixture-structured'"
  -- A failure in the final success write must roll back the run completion too.
  exec "ALTER TABLE event_discovery_source ADD CONSTRAINT event_pilot_test_no_success CHECK (last_success_at IS NULL)"
  expectFailure "event_pilot_test_no_success" complete
  expectRunning
  exec "ALTER TABLE event_discovery_source DROP CONSTRAINT event_pilot_test_no_success"
  complete
  Just (Entity _ run) <- runSqlPool (getBy (Social.UniqueExternalEventDiscoverySlot provider (Just now))) pool
  Just (Entity _ source) <- runSqlPool (getBy (Social.UniqueEventDiscoverySource provider)) pool
  unless (Social.externalEventDiscoveryRunStatus run == "completed"
    && Social.externalEventDiscoveryRunFinishedAt run == Just now
    && Social.eventDiscoverySourceLastSuccessAt source == Just now) $
    fail "Enabled source did not commit both success records"
  putStrLn "Source completion passed disabled empty-feed rejection, atomic rollback and enabled completion."

expectFailure :: String -> IO () -> IO ()
expectFailure message action = do
  result <- try action :: IO (Either SomeException ())
  case result of
    Left err | message `isInfixOf` displayException err -> pure ()
    _ -> fail ("Expected completion failure containing: " <> message)

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
