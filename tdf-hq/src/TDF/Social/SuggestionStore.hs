{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Storage, nightly run and read path of friend suggestions by personalized
-- PageRank (COMMUNITY-FRIEND-SUGGESTION-001,
-- formal/system/friend-suggestions-pagerank.md).
--
-- The ranking itself is 'TDF.Social.SuggestionRank'. This module holds the SQL
-- (as exported text, so that scripts/test-friend-suggestions-sql.py runs the
-- same statements on PostgreSQL), the run that stores scores and the adapter
-- that serves them or falls back to the previous answer.
module TDF.Social.SuggestionStore
  ( suggestionFlagName
  , suggestionsEnabled
  , minimumAccountAgeDays
  , maximumRunAgeHours
  , suggestionSchemaInstalledSql
  , eligibleAccountsSql
  , optedOutAccountsSql
  , blockedPairsSql
  , followRowsSql
  , insertRunSql
  , deleteEarlierRunsSql
  , insertViewerSql
  , insertScoreSql
  , servedSuggestionsSql
  , RunSummary (..)
  , runSuggestionScores
  , computeSuggestions
  , readStoredSuggestions
  , suggestions
  , startFriendSuggestionJob
  ) where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Exception         (SomeAsyncException, SomeException, bracket,
                                            displayException, fromException, throwIO, try)
import           Control.Monad             (forM_, forever, void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (ReaderT, asks)
import           Data.Int                  (Int64)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (mapMaybe)
import           Data.Pool                 (withResource)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime)
import           Database.Persist          (PersistValue (..))
import           Database.Persist.Sql      (ConnectionPool, IsolationLevel (RepeatableRead),
                                            Single (..), SqlPersistT, fromSqlKey, rawExecute,
                                            rawSql, runSqlConn, runSqlPool,
                                            runSqlPoolWithIsolation)
import           Database.Persist.SqlBackend (getRDBMS)
import           Servant                   (Handler)
import           System.Environment        (lookupEnv)

import           TDF.Auth                  (AuthedUser (..))
import           TDF.DB                    (Env (..))
import           TDF.DTO                   (SuggestedFriendDTO (..))
import qualified TDF.LogBuffer             as LogBuf
import           TDF.Social.Session        (SessionAccess (..), withCurrentSession)
import           TDF.Social.SuggestionRank

type ReadM = ReaderT Env Handler

-- | Runtime gate, off unless the variable is exactly @true@.
suggestionFlagName :: String
suggestionFlagName = "FRIEND_SUGGESTIONS_PAGERANK_ENABLED"

suggestionsEnabled :: IO Bool
suggestionsEnabled = (== Just "true") <$> lookupEnv suggestionFlagName

-- | An account younger than this at the start of a run is not in the graph.
minimumAccountAgeDays :: Int
minimumAccountAgeDays = 7

-- | A run older than this is not served.
maximumRunAgeHours :: Int
maximumRunAgeHours = 48

-- | Whether the score tables, the provider identity table and the social-v2
-- policy the read path relies on are all present (no parameters).
suggestionSchemaInstalledSql :: Text
suggestionSchemaInstalledSql =
  "SELECT to_regclass('social_suggestion_run') IS NOT NULL \
  \AND to_regclass('social_suggestion_viewer') IS NOT NULL \
  \AND to_regclass('social_suggestion_score') IS NOT NULL \
  \AND to_regclass('auth_provider_identity') IS NOT NULL \
  \AND to_regclass('party_email_verification') IS NOT NULL \
  \AND to_regclass('social_v2_pair') IS NOT NULL \
  \AND to_regclass('social_v2_preference') IS NOT NULL \
  \AND to_regprocedure('social_v2_profile_eligible(bigint,bigint[])') IS NOT NULL"

-- | The conditions an account must meet now to be a node, apart from its age:
-- a person with an active credential whose email is verified (a sign-in
-- provider bound the credential, or the party completed a password reset), and whose social profile is not closed. @p@ is the party alias.
liveVerifiedAccount :: Text
liveVerifiedAccount =
  "NOT p.is_org \
  \AND EXISTS (SELECT 1 FROM user_credential u \
  \WHERE u.party_id = p.id AND u.active \
  \AND (EXISTS (SELECT 1 FROM auth_provider_identity i WHERE i.credential_id = u.id) \
  \OR EXISTS (SELECT 1 FROM party_email_verification v WHERE v.party_id = p.id))) \
  \AND NOT EXISTS (SELECT 1 FROM social_v2_preference s \
  \WHERE s.party_id = p.id AND s.closed)"

-- | Accounts that are nodes of a run. Two parameters: the run's start time and
-- the minimum age in days.
eligibleAccountsSql :: Text
eligibleAccountsSql =
  "SELECT p.id FROM party p WHERE " <> liveVerifiedAccount
    <> " AND p.created_at <= ?::timestamptz - make_interval(days => ?)"

-- | Accounts that asked not to receive personalized results (no parameters).
optedOutAccountsSql :: Text
optedOutAccountsSql =
  "SELECT party_id FROM social_v2_preference WHERE NOT personalized"

-- | Pairs with a block in either direction (no parameters).
blockedPairsSql :: Text
blockedPairsSql =
  "SELECT party_a, party_b FROM social_v2_pair WHERE block_a OR block_b"

-- | Each side controls its own outgoing canonical follow (no parameters).
followRowsSql :: Text
followRowsSql =
  "SELECT party_a, party_b FROM social_v2_pair WHERE follow_a \
  \UNION ALL SELECT party_b, party_a FROM social_v2_pair WHERE follow_b"

-- | Record a run. Parameters: start time, damping, iterations, viewer count.
insertRunSql :: Text
insertRunSql =
  "INSERT INTO social_suggestion_run(started_at, completed_at, damping, iterations, viewer_count) \
  \VALUES (?, clock_timestamp(), ?, ?, ?) RETURNING id"

-- | Remove every earlier run and, by cascade, its viewers and scores. One
-- parameter: the run to keep.
deleteEarlierRunsSql :: Text
deleteEarlierRunsSql = "DELETE FROM social_suggestion_run WHERE id <> ?"

-- | Parameters: viewer, run, support as a PostgreSQL array literal.
insertViewerSql :: Text
insertViewerSql =
  "INSERT INTO social_suggestion_viewer(viewer_id, run_id, support) VALUES (?, ?, ?::bigint[])"

-- | Parameters: viewer, candidate, position, score.
insertScoreSql :: Text
insertScoreSql =
  "INSERT INTO social_suggestion_score(viewer_id, candidate_id, position, score) VALUES (?, ?, ?, ?)"

-- | Validate support and select candidates in one PostgreSQL statement so both
-- use the same run snapshot during nightly replacement. No row means fallback.
-- A valid run with no eligible candidate returns one NULL via the left join.
-- Parameters: viewer, maximum run age in hours, candidate limit.
servedSuggestionsSql :: Text
servedSuggestionsSql =
  "WITH usable AS (SELECT w.viewer_id FROM social_suggestion_viewer w \
  \JOIN social_suggestion_run r ON r.id = w.run_id \
  \WHERE w.viewer_id = ? \
  \AND r.completed_at > clock_timestamp() - make_interval(hours => ?) \
  \AND coalesce((SELECT o.personalized FROM social_v2_preference o \
  \WHERE o.party_id = w.viewer_id), true) \
  \AND NOT EXISTS (SELECT 1 FROM unnest(w.support) AS member(id) \
  \WHERE NOT EXISTS (SELECT 1 FROM party p WHERE p.id = member.id AND "
    <> liveVerifiedAccount
    <> ") OR EXISTS (SELECT 1 FROM social_v2_pair e \
       \WHERE e.party_a = least(w.viewer_id, member.id) \
       \AND e.party_b = greatest(w.viewer_id, member.id) \
       \AND (e.block_a OR e.block_b)))) "
    <> "SELECT candidate.candidate_id FROM usable w LEFT JOIN LATERAL (SELECT c.candidate_id, c.position FROM social_suggestion_score c \
  \WHERE c.viewer_id = w.viewer_id \
  \AND c.candidate_id IN (SELECT profile_id FROM social_v2_profile_eligible(w.viewer_id, \
  \ARRAY(SELECT s.candidate_id FROM social_suggestion_score s WHERE s.viewer_id = w.viewer_id))) \
  \AND NOT EXISTS (SELECT 1 FROM social_v2_pair f \
  \WHERE f.party_a = least(w.viewer_id, c.candidate_id) AND f.party_b = greatest(w.viewer_id, c.candidate_id) \
  \AND (f.follow_a OR f.follow_b)) \
  \ORDER BY c.position LIMIT ?) candidate ON true ORDER BY candidate.position"

data RunSummary = RunSummary
  { runViewers    :: !Int
  , runCandidates :: !Int
  } deriving (Eq, Show)

-- | The stored result of every viewer that is a node and did not opt out.
computeSuggestions
  :: Set.Set Int64
  -- ^ eligible accounts
  -> Set.Set Int64
  -- ^ accounts that opted out of personalization
  -> [(Int64, Int64)]
  -- ^ pairs with a block in either direction
  -> [(Int64, Int64)]
  -- ^ follows as (follower, followed)
  -> [ViewerSuggestions Double]
computeSuggestions eligible optedOut blocks follows =
  mapMaybe forViewer (Set.toList (eligible `Set.difference` optedOut))
  where
    base = followGraph eligible follows
    blockedBy =
      Map.fromListWith Set.union
        (concat [[(a, Set.singleton b), (b, Set.singleton a)] | (a, b) <- blocks])
    connectedTo =
      Map.fromListWith Set.union
        (concat
          [ [(follower, Set.singleton followed), (followed, Set.singleton follower)]
          | (follower, followed) <- follows
          ])
    forViewer viewer =
      suggestionsForViewer base
        (Map.findWithDefault Set.empty viewer blockedBy)
        (Map.findWithDefault Set.empty viewer connectedTo)
        viewer

-- | One run: read accounts, blocks and follows in a single snapshot, compute
-- every viewer, then replace all stored results in a single transaction.
-- Readers see the previous run or this one, never a mixture; a failure leaves
-- the previous run untouched.
runSuggestionScores :: ConnectionPool -> UTCTime -> IO RunSummary
runSuggestionScores pool startedAt = do
  (eligibleRows, optedOutRows, blockRows, followRows) <-
    runSqlPoolWithIsolation readSnapshot pool RepeatableRead
  let results =
        computeSuggestions
          (Set.fromList [party | Single party <- eligibleRows])
          (Set.fromList [party | Single party <- optedOutRows])
          [(a, b) | (Single a, Single b) <- blockRows]
          [(follower, followed) | (Single follower, Single followed) <- followRows]
  runSqlPool (storeResults results) pool
  pure RunSummary
    { runViewers = length results
    , runCandidates = sum (map (length . vsCandidates) results)
    }
  where
    readSnapshot = do
      eligibleRows <- rawSql eligibleAccountsSql
        [PersistUTCTime startedAt, PersistInt64 (fromIntegral minimumAccountAgeDays)]
      optedOutRows <- rawSql optedOutAccountsSql []
      blockRows <- rawSql blockedPairsSql []
      followRows <- rawSql followRowsSql []
      pure
        ( eligibleRows :: [Single Int64]
        , optedOutRows :: [Single Int64]
        , blockRows :: [(Single Int64, Single Int64)]
        , followRows :: [(Single Int64, Single Int64)]
        )

    storeResults :: [ViewerSuggestions Double] -> SqlPersistT IO ()
    storeResults results = do
      runRows <- rawSql insertRunSql
        [ PersistUTCTime startedAt
        , PersistDouble walkDamping
        , PersistInt64 (fromIntegral walkIterations)
        , PersistInt64 (fromIntegral (length results))
        ]
      case runRows of
        [Single (runId :: Int64)] -> do
          rawExecute deleteEarlierRunsSql [PersistInt64 runId]
          forM_ results $ \result -> do
            rawExecute insertViewerSql
              [ PersistInt64 (vsViewer result)
              , PersistInt64 runId
              , PersistText (arrayLiteral (Set.toAscList (vsSupport result)))
              ]
            forM_ (zip [1 :: Int64 ..] (vsCandidates result)) $ \(position, (candidate, score)) ->
              rawExecute insertScoreSql
                [ PersistInt64 (vsViewer result)
                , PersistInt64 candidate
                , PersistInt64 position
                , PersistDouble score
                ]
        _ -> liftIO (throwIO (userError "Friend suggestion run row was not created"))

arrayLiteral :: [Int64] -> Text
arrayLiteral ids = "{" <> T.intercalate "," (map (T.pack . show) ids) <> "}"

-- | The viewer's stored candidates, or 'Nothing' when the answer must be the
-- fallback: schema or policy not installed, no usable stored result, or the
-- viewer opted out. Runs in the caller's transaction on PostgreSQL only.
readStoredSuggestions :: Int64 -> SqlPersistT IO (Maybe [Int64])
readStoredSuggestions viewer = do
  backend <- T.toCaseFold <$> getRDBMS
  if backend /= "postgresql"
    then pure Nothing
    else do
      installed <- rawSql suggestionSchemaInstalledSql []
      if installed /= [Single True]
        then pure Nothing
        else do
          rows <- rawSql servedSuggestionsSql
            [ PersistInt64 viewer, PersistInt64 (fromIntegral maximumRunAgeHours)
            , PersistInt64 (fromIntegral servedSuggestionLimit)
            ]
          pure $ case rows of
            [] -> Nothing
            _  -> Just [candidate | Single (Just candidate) <- rows]

-- | The route's answer. With the flag off this is @fallback@ and nothing else
-- runs. With it on, the stored candidates are served when
-- 'readStoredSuggestions' allows; an invalid session, any database error and
-- every refusal lead to @fallback@, which applies its own checks.
suggestions :: AuthedUser -> ReadM [SuggestedFriendDTO] -> Bool -> ReadM [SuggestedFriendDTO]
suggestions _ fallback False = fallback
suggestions user fallback True = do
  enabled <- liftIO suggestionsEnabled
  if not enabled
    then fallback
    else do
      pool <- asks envPool
      outcome <- liftIO . tryNonAsync $
        runSqlPool
          (withCurrentSession ReadSession user
            (readStoredSuggestions (fromSqlKey (auPartyId user))))
          pool
      case outcome of
        Right (Right (Just candidates)) ->
          pure [SuggestedFriendDTO candidate 0 (Just "network") | candidate <- candidates]
        _ -> fallback

tryNonAsync :: IO a -> IO (Either SomeException a)
tryNonAsync action = do
  result <- try action
  case result of
    Left err
      | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
      | otherwise -> pure (Left err)
    Right value -> pure (Right value)

-- | Nightly run at 08:00 UTC (03:00 America/Guayaquil). Every API replica
-- starts the loop; a PostgreSQL advisory lock lets one of them run. Does
-- nothing unless the flag is on at start-up.
startFriendSuggestionJob :: Env -> IO ()
startFriendSuggestionJob env = do
  let pool = envPool env
  enabled <- suggestionsEnabled
  if not enabled
    then LogBuf.addLog LogBuf.LogInfo "[Cron][FriendSuggestions] Disabled by configuration."
    else do
      void . forkIO . forever $ do
        now <- getCurrentTime
        threadDelay (max 1 (ceiling (realToFrac (diffUTCTime (nextRunAfter now) now) * (1000000 :: Double))))
        outcome <- tryNonAsync (withLeaderLock pool (runSuggestionScores pool =<< getCurrentTime))
        LogBuf.addLog LogBuf.LogInfo $ case outcome of
          Left err -> "[Cron][FriendSuggestions] Run failed: " <> T.pack (displayException err)
          Right Nothing -> "[Cron][FriendSuggestions] Another API replica owns this run; skipping."
          Right (Just summary) ->
            "[Cron][FriendSuggestions] Stored " <> T.pack (show (runCandidates summary))
              <> " candidates for " <> T.pack (show (runViewers summary)) <> " viewers."
      LogBuf.addLog LogBuf.LogInfo "[Cron][FriendSuggestions] Scheduled daily at 08:00 UTC."

-- | The first 08:00 UTC strictly after the given instant.
nextRunAfter :: UTCTime -> UTCTime
nextRunAfter now
  | today > now = today
  | otherwise = addUTCTime 86400 today
  where
    today = UTCTime (utctDay now) (8 * 60 * 60)

withLeaderLock :: ConnectionPool -> IO a -> IO (Maybe a)
withLeaderLock pool action =
  withResource pool $ \backend -> bracket
    (runSqlConn (rawSql "SELECT pg_try_advisory_lock(?)" lockParameters :: SqlPersistT IO [Single Bool]) backend)
    (\locked -> when (locked == [Single True]) $ void $
      runSqlConn (rawSql "SELECT pg_advisory_unlock(?)" lockParameters :: SqlPersistT IO [Single Bool]) backend)
    (\locked -> if locked == [Single True] then Just <$> action else pure Nothing)
  where
    -- Dedicated to friend suggestions across replicas. Artist similarity schedules independently.
    lockParameters = [PersistInt64 8401320261125]
