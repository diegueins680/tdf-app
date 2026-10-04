{-# LANGUAGE OverloadedStrings #-}
module RelationshipReadBenchmark (benchmarkRelationshipReads) where

import Control.Monad (forM, forM_, replicateM_, unless, when)
import Data.Int (Int64)
import Data.List (sort)
import Data.Text (Text)
import Data.Time (Day)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (Single(..), SqlPersistT, rawExecute, rawSql, runSqlPool)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import TDF.DB (Env(..))

type Row=(Single Int64,Single Int64,Single Text,Single Text,Single Bool,Single Day)

-- Before execution: protected projection warm p95 <=250ms at degree 100,
-- <=1500ms at degree 10,004. This existing unpaginated contract is NOT scale-safe.
-- Reference is an equivalent unprotected join; not the complete old handler.
benchmarkRelationshipReads :: Env -> IO ()
benchmarkRelationshipReads env = do
  enabled <- lookupEnv "TDF_SOCIAL_RELATIONSHIP_BENCHMARK"
  when (enabled==Just "1") $ do
    let run action=runSqlPool action (envPool env)
        sql statement=run (rawExecute statement [])
        measured action=do
          start <- getMonotonicTimeNSec
          result <- action
          end <- getMonotonicTimeNSec
          pure (result,fromIntegral (end-start)/1000000::Double)
        percentile fraction values=sort values !! (ceiling (fraction*fromIntegral (length values))-1)
    sql "TRUNCATE social_v2_command,social_v2_pair,social_v2_preference,party_follow RESTART IDENTITY"
    sql "UPDATE social_v2_runtime SET enabled=true"
    sql "UPDATE user_credential SET active=true"
    sql "UPDATE party SET is_org=false"
    sql "INSERT INTO party SELECT n,'Synthetic '||n,false FROM generate_series(6,10005) n"
    sql "INSERT INTO user_credential SELECT n,n,true FROM generate_series(6,10005) n"
    sql "ANALYZE party; ANALYZE user_credential"
    forM_ [100,10004::Int64] $ \degree -> do
      sql "TRUNCATE party_follow RESTART IDENTITY"
      run $ do
        rawExecute "INSERT INTO party_follow(follower_party_id,following_party_id,created_at) SELECT 1,n,'2026-01-01' FROM generate_series(2,1+?::bigint) n" [PersistInt64 degree]
        rawExecute "INSERT INTO party_follow(follower_party_id,following_party_id,created_at) SELECT n,1,'2026-01-01' FROM generate_series(2,1+?::bigint) n" [PersistInt64 degree]
      sql "ANALYZE party_follow"
      forM_ ["following","followers","friends"::Text] $ \kind -> do
        let filterSql=if kind=="followers" then "f.following_party_id=1" else
              "f.follower_party_id=1"<>(if kind=="friends" then " AND EXISTS(SELECT 1 FROM party_follow r WHERE r.follower_party_id=f.following_party_id AND r.following_party_id=1)" else "")
            reference=run (rawSql
              ("SELECT f.follower_party_id,f.following_party_id,p.display_name::text,q.display_name::text,f.via_nfc,(f.created_at AT TIME ZONE 'UTC')::date FROM party_follow f JOIN party p ON p.id=f.follower_party_id JOIN party q ON q.id=f.following_party_id WHERE "<>filterSql<>" ORDER BY f.created_at DESC,f.id DESC") [] :: SqlPersistT IO [Row])
            guarded=run (rawSql "SELECT * FROM social_v2_relationship_rows(1,?)" [PersistText kind] :: SqlPersistT IO [Row])
            pair n=do
              ((plain,plainMs),(checked,checkedMs)) <- if even n
                then do a <- measured reference; b <- measured guarded; pure (a,b)
                else do b <- measured guarded; a <- measured reference; pure (a,b)
              unless (plain==checked && length checked==fromIntegral degree)
                (fail "Relationship benchmark changed rows/order/metadata")
              pure (plainMs,checkedMs)
        replicateM_ 3 (pair (0::Int))
        samples <- forM [1..20::Int] pair
        let plain=map fst samples
            checked=map snd samples
            p95=percentile 0.95
            threshold=if degree==100 then 250 else 1500
        putStrLn ("RELATIONSHIP SQL: parties=10005 degree="<>show degree<>" collection="<>show kind<>" warmups=3 alternating-pairs=20")
        putStrLn ("reference p50/p95 ms: "<>show (percentile 0.5 plain,p95 plain))
        putStrLn ("protected p50/p95 ms: "<>show (percentile 0.5 checked,p95 checked))
        unless (p95 checked<=threshold) (fail "Protected list exceeds predeclared fixture p95 threshold")
    putStrLn "PASS: fixture SQL thresholds; no production or HTTP capacity claim; pagination remains required for scale"
