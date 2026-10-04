{-# LANGUAGE OverloadedStrings #-}
module FanEffectsBenchmark (benchmarkFanEffects) where

import Control.Exception (bracket_)
import Control.Monad (forM, forM_, replicateM_, unless, when)
import Control.Monad.Reader (runReaderT)
import Data.List (sort)
import qualified Data.Text
import Database.Persist.Sql (Single(..), rawExecute, rawSql, runSqlPool)
import GHC.Clock (getMonotonicTimeNSec)
import Servant (runHandler)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import TDF.Auth (loadAuthedUser)
import TDF.DB (Env(..))
import TDF.Social.FanEffects (followArtist)

-- Predeclared fixture acceptance: retired whole-handler warm p95 <=100ms with
-- 100 members and <=250ms with 10,000; no roster/profile/notification writes.
-- One legacy first-follow probe has a 10s ceiling, not a production capacity test.
benchmarkFanEffects :: Env -> IO ()
benchmarkFanEffects env = do
  enabled <- lookupEnv "TDF_SOCIAL_FAN_EFFECTS_BENCHMARK"
  when (enabled == Just "1") $ do
    let run action = runSqlPool action (envPool env)
        sql statement = run (rawExecute statement [])
        measured action = do
          start <- getMonotonicTimeNSec
          result <- action
          end <- getMonotonicTimeNSec
          pure (result,fromIntegral (end-start)/1000000::Double)
        percentile fraction values = sort values !! (ceiling (fraction*fromIntegral (length values))-1)
        clearEffects = sql "TRUNCATE party_follow,fan_follow,notification,engagement_event; DELETE FROM fan_club_member_profile WHERE party_id=1"
        neverActivated = bracket_ (sql "ALTER TABLE social_v2_runtime DISABLE TRIGGER social_v2_activation_memory")
          (sql "ALTER TABLE social_v2_runtime ENABLE TRIGGER social_v2_activation_memory")
          (sql "UPDATE social_v2_runtime SET enabled=false,activated_once=false")
    sql "TRUNCATE social_v2_command,social_v2_pair,social_v2_preference"
    sql "UPDATE api_token SET active=true,party_id=1,label=NULL WHERE id=1"
    sql "UPDATE user_credential SET active=true; UPDATE party SET is_org=false"
    sql "INSERT INTO party SELECT n,'Synthetic '||n,false FROM generate_series(6,10005) n ON CONFLICT DO NOTHING"
    sql "UPDATE fan_club SET artist_party_id=5 WHERE id=1"
    sql "ANALYZE party"
    Just user <- run (loadAuthedUser "synthetic-1")
    let follow = do
          outcome <- runHandler (runReaderT (followArtist user 5) env)
          either (fail . show) (const (pure ())) outcome
    forM_ [100,10000::Int] $ \degree -> do
      sql "TRUNCATE fan_club_member_profile"
      sql ("INSERT INTO fan_club_member_profile(party_id,club_id,joined_at) SELECT n,1,now() FROM generate_series(6,"<>fromString (show (degree+5))<>") n")
      sql "ANALYZE fan_club_member_profile"
      neverActivated
      clearEffects
      baseline <- timeout 10000000 (measured follow)
      putStrLn ("FAN EFFECTS HANDLER: parties=10005 members="<>show degree<>" legacy first-follow probe="<>
        maybe "exceeded 10000ms; cancelled/rolled back (no percentile)" (\(_,ms) -> show ms<>"ms (one sample, no percentile)") baseline)
      clearEffects
      sql "UPDATE social_v2_runtime SET enabled=true"
      let candidate = clearEffects >> measured follow
      replicateM_ 5 candidate
      samples <- forM [1..40::Int] (const candidate)
      let timings = map snd samples
          p95=percentile 0.95 timings
          threshold=if degree==100 then 100 else 250
      putStrLn ("retired handler warmups=5 samples=40 p50/p95 ms: "<>show (percentile 0.5 timings,p95))
      [Single clean] <- run (rawSql "SELECT NOT EXISTS(SELECT 1 FROM party_follow) AND NOT EXISTS(SELECT 1 FROM notification) AND NOT EXISTS(SELECT 1 FROM fan_club_member_profile WHERE party_id=1) AND (SELECT count(*) FROM fan_follow)=1 AND (SELECT count(*) FROM engagement_event)=1" [])
      unless (clean && p95<=threshold) (fail "Retired fan-effects performance/correctness threshold failed")
    putStrLn "PASS: synthetic handler thresholds and no implicit effects; excludes fresh auth/HTTP encoding/network"
  where
    fromString = Data.Text.pack
