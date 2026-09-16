{-# LANGUAGE OverloadedStrings #-}
module LegacyWriteBenchmark (benchmarkLegacyWrites) where

import Control.Exception (bracket_)
import Control.Monad (forM, forM_, replicateM_, unless, when)
import Data.List (sort)
import Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import TDF.DB (Env(..))

-- Acceptance declared before measurement: guard SQL/transaction warm p95 <=50ms
-- on a private 10,005-account fixture, degree 0 and 10,004. Reference is the same
-- predicate without locks, not the whole old writer. No HTTP/production claim.
benchmarkLegacyWrites :: Env -> IO ()
benchmarkLegacyWrites env = do
  enabled <- lookupEnv "TDF_SOCIAL_LEGACY_WRITE_BENCHMARK"
  when (enabled == Just "1") $ do
    let run action = runSqlPool action (envPool env)
        sql statement = run (rawExecute statement [])
        measured action = do
          start <- getMonotonicTimeNSec
          result <- action
          end <- getMonotonicTimeNSec
          pure (result,fromIntegral (end-start)/1000000::Double)
        percentile fraction values = sort values !! (ceiling (fraction*fromIntegral (length values))-1)
    sql "TRUNCATE social_v2_command,social_v2_pair,social_v2_preference"
    bracket_ (sql "ALTER TABLE social_v2_runtime DISABLE TRIGGER social_v2_activation_memory")
      (sql "ALTER TABLE social_v2_runtime ENABLE TRIGGER social_v2_activation_memory")
      (sql "UPDATE social_v2_runtime SET enabled=false,activated_once=false")
    sql "INSERT INTO party SELECT n,'Synthetic '||n,false FROM generate_series(6,10005) n ON CONFLICT DO NOTHING"
    sql "INSERT INTO user_credential SELECT n,n,true FROM generate_series(6,10005) n ON CONFLICT DO NOTHING"
    sql "ANALYZE party; ANALYZE user_credential"
    forM_ [False,True] $ \dense -> do
      when dense $ do
        sql "INSERT INTO social_v2_pair(party_a,party_b) SELECT 1,n FROM generate_series(2,10005) n"
        sql "ANALYZE social_v2_pair"
      let reference = run (rawSql "SELECT NOT social_v2_dm_required(1,2)" [] :: SqlPersistT IO [Single Bool])
          guarded = run (rawSql "SELECT social_v2_lock_legacy_write(1,2)" [] :: SqlPersistT IO [Single Bool])
          pair n = do
            ((plain,plainMs),(checked,checkedMs)) <- if even n
              then do a <- measured reference; b <- measured guarded; pure (a,b)
              else do b <- measured guarded; a <- measured reference; pure (a,b)
            unless (plain==checked && checked==[Single (not dense)]) (fail "Guard benchmark changed eligibility")
            pure (plainMs,checkedMs)
      replicateM_ 5 (pair (0::Int))
      samples <- forM [1..40::Int] pair
      let plain=map fst samples
          checked=map snd samples
          p95=percentile 0.95
      putStrLn ("LEGACY WRITE GUARD SQL: parties=10005 degree="<>show (if dense then 10004::Int else 0)<>
        " warmups=5 alternating-pairs=40")
      putStrLn ("reference p50/p95 ms: "<>show (percentile 0.5 plain,p95 plain))
      putStrLn ("protected p50/p95 ms: "<>show (percentile 0.5 checked,p95 checked))
      unless (p95 checked<=50) (fail "Guard warm p95 exceeds declared 50ms fixture threshold")
    putStrLn "PASS: fixture-only legacy guard p95 <=50ms; excludes bearer, mutation and HTTP costs"
