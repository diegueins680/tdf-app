{-# LANGUAGE OverloadedStrings #-}
module ProfileBenchmark (benchmarkProfiles) where

import Control.Monad (forM, forM_, replicateM_, unless, when)
import Data.List (sort)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import TDF.DB (Env(..))

type Row = (Single Int64,Single Text,Single (Maybe Text),Single (Maybe Text),
            Single (Maybe Text),Single (Maybe Text))

-- Declared acceptance before measurement: protected SQL warm p95 <=50ms for
-- batches <=100 over 10,005 synthetic parties, sparse and 10,004-degree actor.
-- Reference is an equivalent unprotected join, NOT the complete legacy handler.
-- Excludes bearer/session overhead (separately qualified) and network delivery.
benchmarkProfiles :: Env -> IO ()
benchmarkProfiles env = do
  enabled <- lookupEnv "TDF_SOCIAL_PROFILE_BENCHMARK"
  when (enabled == Just "1") $ do
    let run action = runSqlPool action (envPool env)
        sql statement = run (rawExecute statement [])
        measured action = do
          start <- getMonotonicTimeNSec
          result <- action
          end <- getMonotonicTimeNSec
          pure (result, fromIntegral (end-start) / 1000000 :: Double)
        percentile fraction values = sort values !! (ceiling (fraction * fromIntegral (length values)) - 1)
    sql "TRUNCATE social_v2_command,social_v2_pair,social_v2_preference"
    sql "UPDATE social_v2_runtime SET enabled=true"
    sql "UPDATE user_credential SET active=true"
    sql "INSERT INTO party SELECT n,'Synthetic '||n,false FROM generate_series(6,10005) n"
    sql "INSERT INTO user_credential SELECT n,n,true FROM generate_series(6,10005) n"
    sql "INSERT INTO fan_profile(fan_party_id,bio) SELECT n,'profile bio '||n FROM generate_series(6,10005) n"
    sql "ANALYZE party; ANALYZE user_credential; ANALYZE fan_profile"
    forM_ [False,True] $ \dense -> do
      when dense $ do
        sql "INSERT INTO social_v2_pair(party_a,party_b,follow_a) SELECT 1,n,true FROM generate_series(2,10005) n"
        sql "ANALYZE social_v2_pair"
      forM_ [1,100] $ \size -> do
        let params = [PersistInt64 size]
            reference = run (rawSql
              "SELECT p.id,p.display_name::text,f.display_name::text,f.avatar_url::text,f.bio::text,f.city::text FROM generate_series(2,1+?::bigint) r(id) JOIN party p ON p.id=r.id LEFT JOIN fan_profile f ON f.fan_party_id=p.id ORDER BY r.id"
              params :: SqlPersistT IO [Row])
            guarded = run (rawSql
              "SELECT * FROM social_v2_profiles(1,ARRAY(SELECT generate_series(2,1+?::bigint)))"
              params :: SqlPersistT IO [Row])
            pair n = do
              ((plain,plainMs),(checked,checkedMs)) <- if even n
                then do a <- measured reference; b <- measured guarded; pure (a,b)
                else do b <- measured guarded; a <- measured reference; pure (a,b)
              unless (plain==checked && length checked==fromIntegral size)
                (fail "Profile projection benchmark changed eligible payload/order")
              pure (plainMs,checkedMs)
        replicateM_ 5 (pair (0::Int))
        samples <- forM [1..40::Int] pair
        let plain = map fst samples
            checked = map snd samples
            p95 = percentile 0.95
        putStrLn ("PROFILE SQL: parties=10005 degree="<>show (if dense then 10004::Int else 0)<>
          " batch="<>show size<>" warmups=5 alternating-pairs=40")
        putStrLn ("reference p50/p95 ms: "<>show (percentile 0.5 plain,p95 plain))
        putStrLn ("protected p50/p95 ms: "<>show (percentile 0.5 checked,p95 checked))
        unless (p95 checked<=50) (fail "Protected profile warm p95 exceeds declared 50ms fixture threshold")
    putStrLn "PASS: fixture-only protected SQL p95 <=50ms; no production capacity or HTTP latency claim"
