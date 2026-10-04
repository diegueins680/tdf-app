{-# LANGUAGE OverloadedStrings #-}
module SessionBenchmark (benchmarkSession) where

import Control.Monad (forM, replicateM_, unless, when)
import Data.List (sort)
import Data.Text (Text)
import Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import TDF.Auth (loadAuthedUser)
import TDF.DB (Env(..))
import TDF.Social.Session

-- Opt-in, private-fixture fixed-cost measurement, not a high-degree/feed benchmark.
-- Acceptance declared before comparing: added warm p95 <=20ms on local loopback.
benchmarkSession :: Env -> IO ()
benchmarkSession env = do
  enabled <- lookupEnv "TDF_SOCIAL_SESSION_BENCHMARK"
  when (enabled == Just "1") $ do
    let run action = runSqlPool action (envPool env)
        measured action = do
          start <- getMonotonicTimeNSec
          result <- action
          end <- getMonotonicTimeNSec
          pure (result, fromIntegral (end-start) / 1000000 :: Double)
    run $ do
      rawExecute "UPDATE user_credential SET active=true WHERE party_id=1" []
      rawExecute "UPDATE api_token SET active=true,party_id=1,label=NULL WHERE id=1" []
    Just user <- run (loadAuthedUser "synthetic-1")
    let statement = rawSql "SELECT social_v2_me(1)::text" [] :: SqlPersistT IO [Single Text]
        baseline = run statement
        guarded = run (withSocialSession ReadSession user statement)
        pair n = do
          ((plain, plainMs),(checked,checkedMs)) <- if even n
            then do a <- measured baseline; b <- measured guarded; pure (a,b)
            else do b <- measured guarded; a <- measured baseline; pure (a,b)
          unless (checked == Right plain) (fail "Benchmark changed the authorized result")
          pure (plainMs, checkedMs, checkedMs-plainMs)
    replicateM_ 5 (pair (0::Int))
    samples <- forM [1..40::Int] pair
    let percentile fraction values = sort values !! (ceiling (fraction * fromIntegral (length values)) - 1)
        plain = [a | (a,_,_) <- samples]
        checked = [b | (_,b,_) <- samples]
        added = [c | (_,_,c) <- samples]
        p95 = percentile 0.95
    putStrLn "SESSION OVERHEAD: native loopback, synthetic five-account fixture, 5 warmups, 40 alternating paired samples"
    putStrLn ("baseline p50/p95 ms: " <> show (percentile 0.5 plain,p95 plain))
    putStrLn ("guarded p50/p95 ms: " <> show (percentile 0.5 checked,p95 checked))
    putStrLn ("paired added p95 ms: " <> show (p95 added))
    unless (p95 added <= 20) (fail "Added warm p95 exceeds declared 20ms fixture threshold")
    putStrLn "PASS: fixture-only added warm p95 <=20ms; no production/high-degree HTTP claim"
