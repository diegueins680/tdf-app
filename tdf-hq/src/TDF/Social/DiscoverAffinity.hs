{-# LANGUAGE OverloadedStrings #-}

-- | Affinity order of the people in Discover (SOCIAL-DISCOVER-AFFINITY-001,
-- @formal/system/social-discover-affinity.md@).
--
-- @GET /social/v2/discover@ is served by a SQL function. This module decides
-- which form of it a request runs and states the order as a pure function,
-- the reference the SQL of @tdf-hq/sql/2026-11-27_social_discover_affinity.sql@
-- is tested against. The reference is not on the request path.
--
-- The rule: of the candidates the actor may be shown, at most half a page of
-- those the actor reacted or replied to lead, most interaction first; every
-- other candidate keeps the previous order. Nothing is added or dropped.
module TDF.Social.DiscoverAffinity
  ( affinityWindowDays
  , replyWeight
  , interactionCap
  , evidenceUnits
  , promotedCount
  , discoverOrder
  , discoverPage
  , runsAffinityStatement
  , baseDiscoverSql
  , affinityDiscoverSql
  , affinityInstalledSql
  , discoverRows
  ) where

import           Data.Int             (Int64)
import           Data.List            (sortBy)
import           Data.Ord             (Down (..), comparing)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Database.Persist     (PersistValue)
import           Database.Persist.Sql (Single (..), SqlPersistT, rawSql)

-- | Interactions older than this many days are not read.
affinityWindowDays :: Int
affinityWindowDays = 90

-- | One reply counts as this many reactions.
replyWeight :: Int
replyWeight = 2

-- | Interaction units beyond this change nothing.
interactionCap :: Int
interactionCap = 20

-- | @min (reactions + replyWeight * replies) interactionCap@, never negative.
evidenceUnits :: Int -> Int -> Int
evidenceUnits reactions replies =
  min interactionCap (max 0 reactions + replyWeight * max 0 replies)

-- | How many candidates affinity may place first on a page of the given size.
promotedCount :: Int -> Int
promotedCount pageSize = max 0 pageSize `div` 2

-- | The whole order. @base@ is the previous order of the candidates, with
-- unique ids. The 'promotedCount' candidates with the most units lead, ties by
-- ascending id; a candidate without units is never among them. All others
-- follow in the order of @base@.
discoverOrder :: Int -> (a -> Int64) -> (a -> Int) -> [a] -> [a]
discoverOrder pageSize ident units base = promoted ++ rest
  where
    promoted =
      take (promotedCount pageSize)
        (sortBy (comparing (\c -> (Down (units c), ident c))) (filter ((> 0) . units) base))
    promotedIds = Set.fromList (map ident promoted)
    rest = filter ((`Set.notMember` promotedIds) . ident) base

-- | The page a request returns.
discoverPage :: Int -> (a -> Int64) -> (a -> Int) -> [a] -> [a]
discoverPage pageSize ident units = take (max 0 pageSize) . discoverOrder pageSize ident units

-- | Whether a request runs the affinity form of the function: only with the
-- gate on and the function installed. The actor's opt-out is honoured inside
-- the function.
runsAffinityStatement :: Bool -> Bool -> Bool
runsAffinityStatement gateOn installed = gateOn && installed

-- | The statement every request ran before this change. Parameters: actor,
-- page size.
baseDiscoverSql :: Text
baseDiscoverSql = "SELECT social_v2_discover(?,?)::text"

-- | Parameters: actor, page size.
affinityDiscoverSql :: Text
affinityDiscoverSql = "SELECT social_v2_discover(?,?,true)::text"

-- | Whether the three-argument function exists. No parameters.
affinityInstalledSql :: Text
affinityInstalledSql =
  "SELECT to_regprocedure('social_v2_discover(bigint,integer,boolean)') IS NOT NULL"

-- | Run Discover for one request. With the gate off this is exactly the
-- previous statement and nothing else is read.
discoverRows :: Bool -> PersistValue -> PersistValue -> SqlPersistT IO [Single Text]
discoverRows gateOn actor size = do
  installed <-
    if gateOn
      then do
        rows <- rawSql affinityInstalledSql [] :: SqlPersistT IO [Single Bool]
        pure (rows == [Single True])
      else pure False
  if runsAffinityStatement gateOn installed
    then rawSql affinityDiscoverSql [actor, size]
    else rawSql baseDiscoverSql [actor, size]
