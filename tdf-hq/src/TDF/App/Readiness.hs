{-# LANGUAGE OverloadedStrings #-}
module TDF.App.Readiness (databaseReady, databaseReadyAttempts, probeUntil, readinessAttempts, withinReadinessDeadline) where

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

-- | Attempts one probe may make: the pool size of TDF.DB.makePool plus one.
-- The pool can hold up to pool-size idle connections PostgreSQL has already
-- closed (restart, failover, terminated sessions); every failed attempt
-- destroys the connection it took, so one probe discards every stale
-- connection and still has an attempt left to answer from a live one once
-- PostgreSQL is back. An unavailable database fails every attempt inside the
-- same deadline.
readinessAttempts :: Int
readinessAttempts = 11

databaseReady :: ConnectionPool -> IO Bool
databaseReady = databaseReadyAttempts readinessAttempts

databaseReadyAttempts :: Int -> ConnectionPool -> IO Bool
databaseReadyAttempts attempts pool = withinReadinessDeadline 2000000 $ probeUntil attempts $ do
  rows <- runSqlPool (rawSql "SELECT 1" []) pool
  pure (rows == [Single (1 :: Int)])

-- | Runs the probe until it returns without a synchronous exception or the
-- attempts are spent. A probe result, true or false, is final; asynchronous
-- cancellation propagates through every attempt.
probeUntil :: Int -> IO Bool -> IO Bool
probeUntil attempts probe
  | attempts <= 0 = pure False
  | otherwise = do
      result <- Safe.tryAny probe
      case result of
        Right ready -> pure ready
        Left _ -> probeUntil (attempts - 1) probe
