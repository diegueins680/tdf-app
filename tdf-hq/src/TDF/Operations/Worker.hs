{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.Operations.Worker
  ( OperationsWorkerStats(..)
  , operationsMaintenanceTick
  , operationsWorkerIterationWith
  , startOperationsWorker
  , operationsWorkerEnabled
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception.Safe (tryAny)
import Control.Monad (forever, void)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool)
import System.IO (hPutStrLn, stderr)
import System.Environment (lookupEnv)

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
startOperationsWorker env = do
  configured <- lookupEnv "OPERATIONS_WORKER_ENABLED"
  case operationsWorkerEnabled configured of
    Right True -> void (forkIO (workerLoop env))
    Right False -> putStrLn "[operations] worker disabled by configuration"
    Left message -> fail message

operationsWorkerEnabled :: Maybe String -> Either String Bool
operationsWorkerEnabled Nothing = Right True
operationsWorkerEnabled (Just "true") = Right True
operationsWorkerEnabled (Just "false") = Right False
operationsWorkerEnabled _ = Left "OPERATIONS_WORKER_ENABLED must be true or false"

workerLoop :: Env -> IO ()
workerLoop env = forever $ do
  operationsWorkerIterationWith (operationsMaintenanceTick env) (hPutStrLn stderr) putStrLn
  threadDelay 1000000

operationsWorkerIterationWith :: IO OperationsWorkerStats -> (String -> IO ()) -> (String -> IO ()) -> IO ()
operationsWorkerIterationWith tick logError logInfo = do
  result <- tryAny tick
  case result of
    Left _ -> void $ tryAny $ logError
      "{\"component\":\"operations-worker\",\"level\":\"error\",\"message\":\"tick failed\"}"
    Right stats
      | stats /= emptyStats -> void $ tryAny $ logInfo $ BL.unpack $ encode $ object
          [ "component" .= ("operations-worker" :: Text), "level" .= ("info" :: Text)
          , "processed" .= outboxProcessed stats, "failed" .= outboxFailed stats
          , "deadLettered" .= outboxDeadLettered stats, "slaReminders" .= slaRemindersCreated stats
          , "slaBreaches" .= slaBreachesCreated stats, "archived" .= workItemsArchived stats
          ]
      | otherwise -> pure ()

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
          (processed, failed, dead) <- case outboxRows of
            [(Single p, Single f, Single d)] | all (>= 0) [p,f,d] -> pure (p, f, d)
            _ -> fail "Invalid operations outbox result"
          (reminders, breaches) <- case slaRows of
            [(Single r, Single b)] | r >= 0 && b >= 0 -> pure (r, b)
            _ -> fail "Invalid operations SLA result"
          archived <- case archiveRows of
            [Single count] | count >= 0 -> pure (fromIntegral count)
            _ -> fail "Invalid operations archive result"
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
