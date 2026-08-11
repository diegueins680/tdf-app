{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.Operations.Worker
  ( OperationsWorkerStats(..)
  , operationsMaintenanceTick
  , startOperationsWorker
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , fromException
  , throwIO
  , try
  )
import Control.Monad (forever, void)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool)
import System.IO (hPutStrLn, stderr)

import TDF.DB (Env(..))

data OperationsWorkerStats = OperationsWorkerStats
  { outboxProcessed :: Int
  , outboxFailed :: Int
  , outboxDeadLettered :: Int
  , slaRemindersCreated :: Int
  , slaBreachesCreated :: Int
  , workItemsArchived :: Int
  } deriving (Show, Eq)

emptyStats :: OperationsWorkerStats
emptyStats = OperationsWorkerStats 0 0 0 0 0 0

startOperationsWorker :: Env -> IO ()
startOperationsWorker env = void (forkIO (workerLoop env))

workerLoop :: Env -> IO ()
workerLoop env = forever $ do
  result <- trySync (operationsMaintenanceTick env)
  case result of
    Left err ->
      hPutStrLn stderr
        ("{\"component\":\"operations-worker\",\"level\":\"error\",\"error\":\""
          <> redactLogValue (displayException err) <> "\"}")
    Right stats
      | stats /= emptyStats ->
          putStrLn
            ("{\"component\":\"operations-worker\",\"level\":\"info\",\"stats\":\""
              <> redactLogValue (show stats) <> "\"}")
      | otherwise -> pure ()
  threadDelay 1000000

operationsMaintenanceTick :: Env -> IO OperationsWorkerStats
operationsMaintenanceTick Env{envPool} = runSqlPool tick envPool
  where
    tick = do
      installedRows <- rawSql
        "SELECT to_regprocedure('operations_process_outbox_batch(integer,text)') IS NOT NULL"
        [] :: SqlPersistT IO [Single Bool]
      case installedRows of
        [Single True] -> do
          -- SKIP LOCKED and the per-aggregate predecessor predicate are the
          -- concurrency boundary. Avoid a global advisory lock so multiple
          -- application replicas can drain independent aggregates safely.
          outboxRows <- rawSql
            "SELECT processed, failed, dead_lettered FROM operations_process_outbox_batch(250, 'tdf-hq-operations-worker')"
            [] :: SqlPersistT IO [(Single Int, Single Int, Single Int)]
          slaRows <- rawSql
            "SELECT reminders_created, breached_created FROM operations_tick_sla(now())"
            [] :: SqlPersistT IO [(Single Int, Single Int)]
          archiveRows <- rawSql archiveSql [] :: SqlPersistT IO [Single Int64]
          let (processed, failed, dead) = case outboxRows of
                [(Single p, Single f, Single d)] -> (p, f, d)
                _ -> (0, 0, 0)
              (reminders, breaches) = case slaRows of
                [(Single r, Single b)] -> (r, b)
                _ -> (0, 0)
              archived = case archiveRows of
                [Single count] -> fromIntegral count
                _ -> 0
          pure OperationsWorkerStats
            { outboxProcessed = processed
            , outboxFailed = failed
            , outboxDeadLettered = dead
            , slaRemindersCreated = reminders
            , slaBreachesCreated = breaches
            , workItemsArchived = archived
            }
        _ -> pure emptyStats

archiveSql :: Text
archiveSql =
  "WITH archived AS ( \
  \ UPDATE operations_work_item SET status = 'archived', archived_at = now(), updated_at = now(), version = version + 1 \
  \ WHERE status = 'resolved' AND resolved_at < now() - interval '90 days' \
  \ RETURNING id, organization_id, branch_id \
  \), stream AS ( \
  \ INSERT INTO operations_stream_event (organization_id, branch_id, event_type, work_item_id, payload) \
  \ SELECT organization_id, branch_id, 'work_item.archived', id, jsonb_build_object('workItemId', id, 'reason', 'retention_90_days') FROM archived \
  \), audit AS ( \
  \ INSERT INTO operations_admin_audit (organization_id, branch_id, acting_role, source_client, action, target_entity_type, target_entity_id, new_value, request_id, correlation_id, reason) \
  \ SELECT organization_id, branch_id, 'system', 'tdf-hq-operations-worker', 'auto_archive', 'operations_work_item', id::text, jsonb_build_object('status', 'archived'), gen_random_uuid()::text, id::text, 'resolved for 90 days' FROM archived \
  \) SELECT count(*)::bigint FROM archived"

trySync :: IO a -> IO (Either SomeException a)
trySync action = do
  result <- try action
  case result of
    Left err
      | Just async <- (fromException err :: Maybe SomeAsyncException) -> throwIO async
      | otherwise -> pure (Left err)
    Right value -> pure (Right value)

redactLogValue :: String -> String
redactLogValue = take 1000 . map replaceUnsafe
  where
    replaceUnsafe c
      | c `elem` ['\n', '\r', '\t', '"'] = ' '
      | otherwise = c
