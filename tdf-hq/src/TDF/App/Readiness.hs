{-# LANGUAGE OverloadedStrings #-}
module TDF.App.Readiness (databaseReady, withinReadinessDeadline) where

import qualified Control.Exception.Safe as Safe
import Database.Persist.Sql (ConnectionPool, Single(..), rawSql, runSqlPool)
import System.Timeout (timeout)

-- Includes pool acquisition and the round trip, not just query execution.
-- Cancellation propagates; synchronous failures never expose SQL/credentials.
withinReadinessDeadline :: Int -> IO Bool -> IO Bool
withinReadinessDeadline microseconds check = do
  result <- timeout microseconds (Safe.tryAny check)
  pure $ case result of
    Just (Right True) -> True
    _ -> False

databaseReady :: ConnectionPool -> IO Bool
databaseReady pool = withinReadinessDeadline 2000000 $ do
  rows <- runSqlPool (rawSql "SELECT 1" []) pool
  pure (rows == [Single (1 :: Int)])
