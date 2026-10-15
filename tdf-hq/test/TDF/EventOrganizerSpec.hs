{-# LANGUAGE OverloadedStrings #-}
module TDF.EventOrganizerSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (unless)
import Control.Concurrent.Async (mapConcurrently, withAsync, wait)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (isRight)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (get, toPersistValue)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, Single(..), rawExecute, rawSql, runSqlPool, toSqlKey)
import Servant (ServerError(errBody, errHTTPCode))
import Servant.Server.Internal.Handler (runHandler)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Test.Hspec
import TDF.DB (Env(..))
import TDF.Models.SocialEventsModels (SocialEvent(..), SocialEventId)
import TDF.Server.SocialEventsHandlers (assignMissingEventOrganizer, requireEventManager)

-- EVT-OWNER-CLAIM-001: an event without an organizer is assigned at most once,
-- only on request of a caller the handler marked as allowed, and the stored
-- row decides between simultaneous callers. Runs against PostgreSQL because
-- the guarantee rests on its row lock and conditional update.
spec :: Spec
spec = describe "event-organizer-postgresql" $ do
  configured <- runIO (lookupEnv "TDF_EVENT_ORGANIZER_DATABASE_URL")
  routingOverrides <- runIO (traverse lookupEnv ["PGHOSTADDR", "PGSERVICE", "PGSERVICEFILE"])
  case configured of
    Nothing -> it "requires the isolated fully migrated integration runner" $
      pendingWith "Run scripts/test-event-organizer.sh"
    Just _ | any (maybe False (not . null)) routingOverrides ->
      it "refuses inherited libpq routing overrides" $
        expectationFailure "Unset PGHOSTADDR, PGSERVICE and PGSERVICEFILE for the isolated runner"
    Just connection | not (safeConnection connection) ->
      it "refuses a non-disposable database" $ expectationFailure "Use the isolated integration runner"
    Just connection -> beforeAll (runNoLoggingT (createPostgresqlPool (BS.pack connection) 10)) $
      afterAll (\pool -> removeFixtures pool >> destroyAllResources pool) $ do
        it "refuses an organizer-only route on an event without an organizer and writes nothing" $ \pool -> do
          (key, row) <- ownerless pool Nothing
          outcome <- runHandler (runReaderT (requireEventManager "41" row)
            Env {envPool = pool, envConfig = error "organizer check needs no config"})
          refusal outcome `shouldBe` Just (403, noOrganizer)
          organizerOf pool key `shouldReturn` Nothing

        it "treats a blank organizer as missing and assigns it once" $ \pool -> do
          (key, _) <- ownerless pool (Just "   ")
          first <- attempt pool "41" key
          fmap socialEventOrganizerPartyId (either (const Nothing) Just first) `shouldBe` Just (Just "41")
          second <- attempt pool "42" key
          refusal second `shouldBe` Just (403, "Only the event organizer can manage this event")
          organizerOf pool key `shouldReturn` Just "41"

        it "treats an organizer of tabs and line breaks as missing and assigns it once" $ \pool -> do
          (key, _) <- ownerless pool (Just "\t\n \r")
          first <- attempt pool "41" key
          fmap socialEventOrganizerPartyId (either (const Nothing) Just first) `shouldBe` Just (Just "41")
          second <- attempt pool "42" key
          refusal second `shouldBe` Just (403, "Only the event organizer can manage this event")
          organizerOf pool key `shouldReturn` Just "41"

        it "admits exactly one of several simultaneous assignments" $ \pool -> do
          (key, _) <- ownerless pool Nothing
          let callers = map (T.pack . show) [51 .. 58 :: Int]
          outcomes <- within (mapConcurrently (\party -> (,) party <$> attempt pool party key) callers)
          let winners = [party | (party, outcome) <- outcomes, isRight outcome]
          length winners `shouldBe` 1
          [refusal outcome | (_, outcome) <- outcomes, not (isRight outcome)]
            `shouldBe` replicate 7 (Just (403, "Only the event organizer can manage this event"))
          organizerOf pool key `shouldReturn` Just (head winners)

        it "waits for an uncommitted assignment and then yields to it" $ \pool -> do
          (key, _) <- ownerless pool Nothing
          holding <- newEmptyMVar
          release <- newEmptyMVar
          let holder = runSqlPool (do
                rawExecute "UPDATE social_event SET organizer_party_id = '61' WHERE id = ?" [toPersistValue key]
                liftIO (putMVar holding ())
                liftIO (takeMVar release)) pool
          within $ withAsync holder $ \held -> do
            takeMVar holding
            withAsync (attempt pool "62" key) $ \late -> do
              awaitLockWait pool
              organizerOf pool key `shouldReturn` Nothing
              putMVar release ()
              wait held
              outcome <- wait late
              refusal outcome `shouldBe` Just (403, "Only the event organizer can manage this event")
          organizerOf pool key `shouldReturn` Just "61"

        it "keeps an assigned organizer against every later caller" $ \pool -> do
          (key, _) <- ownerless pool Nothing
          isRight <$> attempt pool "71" key `shouldReturn` True
          refusal <$> attempt pool "72" key
            `shouldReturn` Just (403, "Only the event organizer can manage this event")
          isRight <$> attempt pool "71" key `shouldReturn` True
          organizerOf pool key `shouldReturn` Just "71"

attempt :: ConnectionPool -> Text -> SocialEventId -> IO (Either ServerError SocialEvent)
attempt pool party key = do
  now <- getCurrentTime
  runSqlPool (assignMissingEventOrganizer party now key) pool

noOrganizer :: String
noOrganizer = "This event has no organizer; a strict administrator must assign one by editing the event"

refusal :: Either ServerError a -> Maybe (Int, String)
refusal = either (\err -> Just (errHTTPCode err, BL8.unpack (errBody err))) (const Nothing)

ownerless :: ConnectionPool -> Maybe Text -> IO (SocialEventId, SocialEvent)
ownerless pool organizer = do
  ident <- randomRIO (7200000000, 7299999999 :: Int64)
  let key = toSqlKey ident
  runSqlPool (rawExecute
    "INSERT INTO social_event(id, organizer_party_id, title, start_time, end_time, event_type_id, workflow_state_id) \
    \SELECT ?, ?, 'Organizer assignment fixture', NOW() + INTERVAL '30 days', NOW() + INTERVAL '31 days', id, \
    \'00000000-0000-4000-8000-000000000232' FROM event_type WHERE code = 'concert'"
    [toPersistValue ident, toPersistValue organizer]) pool
  row <- runSqlPool (get key) pool >>= maybe (fail "organizer fixture was not inserted") pure
  pure (key, row)

removeFixtures :: ConnectionPool -> IO ()
removeFixtures = runSqlPool (rawExecute
  "DELETE FROM social_event WHERE id BETWEEN 7200000000 AND 7299999999 \
  \AND title = 'Organizer assignment fixture'" [])

organizerOf :: ConnectionPool -> SocialEventId -> IO (Maybe Text)
organizerOf pool key = do
  rows <- runSqlPool (rawSql "SELECT organizer_party_id FROM social_event WHERE id = ?" [toPersistValue key]) pool
  case rows :: [Single (Maybe Text)] of
    [Single organizer] -> pure organizer
    _ -> fail "organizer fixture disappeared"

awaitLockWait :: ConnectionPool -> IO ()
awaitLockWait pool = do
  rows <- runSqlPool (rawSql
    "SELECT count(*) FROM pg_stat_activity WHERE datname = current_database() AND wait_event_type = 'Lock' \
    \AND query LIKE 'UPDATE social_event SET organizer_party_id = %updated_at%'" []) pool
  unless (any (\(Single n) -> n > (0 :: Int64)) rows) $ threadDelay 20000 >> awaitLockWait pool

safeConnection :: String -> Bool
safeConnection value = not (any (`elem` value) ['?', '#'])
  && "_test" `isSuffixOf` value && any (`isPrefixOf` value)
  ["postgresql://127.0.0.1/", "postgresql://127.0.0.1:", "postgresql://localhost/",
   "postgresql://localhost:", "postgresql://postgres:postgres@postgres:5432/"]

within :: IO a -> IO a
within action = timeout 30000000 action >>= maybe (fail "Organizer assignment case timed out") pure
