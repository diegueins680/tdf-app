{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Reputation.Worker
  ( ReputationWorkerSettings(..)
  , ReputationWorkerStats(..)
  , parseReputationWorkerSettings
  , reputationWorkerTick
  , startReputationWorker
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception.Safe (displayException, tryAny)
import Control.Monad (foldM, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql
  ( ConnectionPool, Single(..), SqlPersistT, rawSql, runSqlPool )
import System.Environment (getEnvironment)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import TDF.DB (Env(..))

data ReputationWorkerSettings = ReputationWorkerSettings
  { rwsEnvironment :: Text
  , rwsBatchSize :: Int
  , rwsPollSeconds :: Int
  } deriving (Eq, Show)

data ReputationWorkerStats = ReputationWorkerStats
  { rwsClaimed :: Int
  , rwsProcessed :: Int
  , rwsFanOut :: Int
  , rwsRetried :: Int
  , rwsDeadLettered :: Int
  , rwsFailed :: Int
  } deriving (Eq, Show)

data ReputationWorkerHealth = ReputationWorkerHealth
  { rwhQueueDepth :: Int64
  , rwhProcessingCount :: Int64
  , rwhDeadLetterCount :: Int64
  , rwhOldestDueAgeSeconds :: Int64
  } deriving (Eq, Show)

emptyStats :: ReputationWorkerStats
emptyStats = ReputationWorkerStats 0 0 0 0 0 0

parseReputationWorkerSettings
  :: [(String, String)]
  -> Either String (Maybe ReputationWorkerSettings)
parseReputationWorkerSettings environment = do
  enabled <- parseBoolean "REPUTATION_AGGREGATION_WORKER_ENABLED" False
    (lookup "REPUTATION_AGGREGATION_WORKER_ENABLED" environment)
  if not enabled
    then Right Nothing
    else do
      workerEnvironment <- case fmap normalize
        (lookup "REPUTATION_AGGREGATION_ENVIRONMENT" environment) of
          Just "staging" -> Right "staging"
          Just "test" -> Right "test"
          Just "production" -> Left
            "REPUTATION_AGGREGATION_WORKER_ENABLED cannot be true in production"
          _ -> Left
            "REPUTATION_AGGREGATION_ENVIRONMENT must be staging or test when the worker is enabled"
      mode <- pure $ maybe "simulation" normalize
        (lookup "REPUTATION_AGGREGATION_MODE" environment)
      unless (mode == "simulation") $
        Left "REPUTATION_AGGREGATION_MODE must remain simulation"
      batchSize <- parseBoundedInt
        "REPUTATION_AGGREGATION_BATCH_SIZE" 25 1 100
        (lookup "REPUTATION_AGGREGATION_BATCH_SIZE" environment)
      pollSeconds <- parseBoundedInt
        "REPUTATION_AGGREGATION_POLL_SECONDS" 5 1 60
        (lookup "REPUTATION_AGGREGATION_POLL_SECONDS" environment)
      Right $ Just ReputationWorkerSettings
        { rwsEnvironment = T.pack workerEnvironment
        , rwsBatchSize = batchSize
        , rwsPollSeconds = pollSeconds
        }
  where
    normalize = map toLower . trim

startReputationWorker :: Env -> IO ()
startReputationWorker env@Env{envPool} = do
  environment <- getEnvironment
  settings <- either (ioError . userError) pure
    (parseReputationWorkerSettings environment)
  case settings of
    Nothing -> pure ()
    Just activeSettings -> do
      installed <- runSqlPool reputationWorkerInstalled envPool
      unless installed $
        ioError (userError
          "Reputation aggregation worker is enabled before its reviewed migration is installed")
      gateEnabled <- runSqlPool
        (reputationWorkerGateEnabled (rwsEnvironment activeSettings)) envPool
      unless gateEnabled $
        ioError (userError
          "Reputation aggregation worker is enabled but its staging database gate is closed")
      workerId <- ("reputation-worker-" <>) . toText <$> nextRandom
      void $ forkIO (workerLoop env activeSettings workerId)

workerLoop :: Env -> ReputationWorkerSettings -> Text -> IO ()
workerLoop env settings workerId = forever $ do
  outcome <- tryAny (reputationWorkerTick env settings workerId)
  case outcome of
    Left err -> hPutStrLn stderr $
      "{\"component\":\"reputation-aggregation-worker\",\"level\":\"error\"," <>
      "\"message\":\"tick failed\",\"error\":\"" <>
      redactLogValue (displayException err) <> "\"}"
    Right (stats, health) ->
      when (stats /= emptyStats || rwhDeadLetterCount health > 0) $
        putStrLn (renderWorkerMetrics settings stats health)
  threadDelay (rwsPollSeconds settings * 1000000)

reputationWorkerTick
  :: Env
  -> ReputationWorkerSettings
  -> Text
  -> IO (ReputationWorkerStats, ReputationWorkerHealth)
reputationWorkerTick Env{envPool} settings workerId = do
  now <- getCurrentTime
  claims <- runSqlPool (claimEvents settings workerId now) envPool
  stats <- foldM (processClaim envPool settings now) emptyStats claims
  health <- runSqlPool (loadWorkerHealth (rwsEnvironment settings)) envPool
  pure (stats, health)

processClaim
  :: ConnectionPool
  -> ReputationWorkerSettings
  -> UTCTime
  -> ReputationWorkerStats
  -> (Single Text, Single Text, Single Int)
  -> IO ReputationWorkerStats
processClaim envPool settings now stats (Single eventId, Single claimToken, _) = do
  processed <- tryAny $ runSqlPool
    (completeEvent eventId claimToken now) envPool
  let claimedStats = stats { rwsClaimed = rwsClaimed stats + 1 }
  case processed of
    Right "processed" -> pure claimedStats
      { rwsProcessed = rwsProcessed claimedStats + 1 }
    Right "fan_out" -> pure claimedStats
      { rwsFanOut = rwsFanOut claimedStats + 1 }
    Right _ -> failClaim "invalid_processing_result" claimedStats
    Left err -> do
      hPutStrLn stderr $
        "{\"component\":\"reputation-aggregation-worker\",\"level\":\"warning\"," <>
        "\"message\":\"event processing failed\",\"error\":\"" <>
        redactLogValue (displayException err) <> "\"}"
      failClaim "processing_failed" claimedStats
  where
    failClaim errorCode currentStats = do
      failure <- tryAny $ runSqlPool
        (failEvent (rwsEnvironment settings) eventId claimToken errorCode now) envPool
      case failure of
        Right "retry" -> pure currentStats
          { rwsRetried = rwsRetried currentStats + 1
          , rwsFailed = rwsFailed currentStats + 1
          }
        Right "dead_letter" -> pure currentStats
          { rwsDeadLettered = rwsDeadLettered currentStats + 1
          , rwsFailed = rwsFailed currentStats + 1
          }
        _ -> pure currentStats { rwsFailed = rwsFailed currentStats + 1 }

claimEvents
  :: ReputationWorkerSettings
  -> Text
  -> UTCTime
  -> SqlPersistT IO [(Single Text, Single Text, Single Int)]
claimEvents settings workerId now = rawSql
  "SELECT event_id, claim_token, claimed_attempt FROM reputation_claim_aggregation_events(?,?,?,?)"
  [ PersistText (rwsEnvironment settings)
  , PersistText workerId
  , PersistInt64 (fromIntegral (rwsBatchSize settings))
  , PersistUTCTime now
  ]

completeEvent :: Text -> Text -> UTCTime -> SqlPersistT IO Text
completeEvent eventId claimToken now = do
  rows <- rawSql
    "SELECT reputation_complete_aggregation_event(?::uuid,?::uuid,?)"
    [PersistText eventId, PersistText claimToken, PersistUTCTime now]
      :: SqlPersistT IO [Single Text]
  case rows of
    [Single outcome] -> pure outcome
    _ -> liftWorkerError "Reputation event completion returned an ambiguous result"

failEvent
  :: Text
  -> Text
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO Text
failEvent environment eventId claimToken errorCode now = do
  rows <- rawSql
    "SELECT reputation_fail_aggregation_event(?,?::uuid,?::uuid,?,?)"
    [ PersistText environment
    , PersistText eventId
    , PersistText claimToken
    , PersistText errorCode
    , PersistUTCTime now
    ] :: SqlPersistT IO [Single Text]
  case rows of
    [Single outcome] -> pure outcome
    _ -> liftWorkerError "Reputation event failure handling returned an ambiguous result"

reputationWorkerInstalled :: SqlPersistT IO Bool
reputationWorkerInstalled = do
  rows <- rawSql
    "SELECT to_regclass('reputation_aggregation_outbox') IS NOT NULL"
    [] :: SqlPersistT IO [Single Bool]
  pure (rows == [Single True])

reputationWorkerGateEnabled :: Text -> SqlPersistT IO Bool
reputationWorkerGateEnabled environment = do
  rows <- rawSql
    "SELECT enabled AND simulation_only FROM reputation_worker_control WHERE environment=?"
    [PersistText environment] :: SqlPersistT IO [Single Bool]
  pure (rows == [Single True])

loadWorkerHealth :: Text -> SqlPersistT IO ReputationWorkerHealth
loadWorkerHealth environment = do
  rows <- rawSql
    "SELECT queue_depth, processing_count, dead_letter_count, oldest_due_age_seconds \
    \FROM reputation_worker_health WHERE environment=?"
    [PersistText environment]
      :: SqlPersistT IO [(Single Int64, Single Int64, Single Int64, Single Int64)]
  case rows of
    [(Single queueDepth, Single processingCount,
      Single deadLetterCount, Single oldestDueAgeSeconds)] ->
        pure ReputationWorkerHealth
          { rwhQueueDepth = queueDepth
          , rwhProcessingCount = processingCount
          , rwhDeadLetterCount = deadLetterCount
          , rwhOldestDueAgeSeconds = oldestDueAgeSeconds
          }
    _ -> liftWorkerError "Reputation worker health query returned an ambiguous result"

renderWorkerMetrics
  :: ReputationWorkerSettings
  -> ReputationWorkerStats
  -> ReputationWorkerHealth
  -> String
renderWorkerMetrics settings stats health =
  "{\"component\":\"reputation-aggregation-worker\",\"level\":\"info\"," <>
  "\"environment\":\"" <> T.unpack (rwsEnvironment settings) <> "\"," <>
  "\"mode\":\"simulation\",\"claimed\":" <> show (rwsClaimed stats) <>
  ",\"processed\":" <> show (rwsProcessed stats) <>
  ",\"fanOut\":" <> show (rwsFanOut stats) <>
  ",\"retried\":" <> show (rwsRetried stats) <>
  ",\"deadLettered\":" <> show (rwsDeadLettered stats) <>
  ",\"failed\":" <> show (rwsFailed stats) <>
  ",\"queueDepth\":" <> show (rwhQueueDepth health) <>
  ",\"processingCount\":" <> show (rwhProcessingCount health) <>
  ",\"deadLetterCount\":" <> show (rwhDeadLetterCount health) <>
  ",\"oldestDueAgeSeconds\":" <> show (rwhOldestDueAgeSeconds health) <> "}"

parseBoolean :: String -> Bool -> Maybe String -> Either String Bool
parseBoolean _ defaultValue Nothing = Right defaultValue
parseBoolean name _ (Just rawValue) = case map toLower (trim rawValue) of
  "true" -> Right True
  "false" -> Right False
  _ -> Left (name <> " must be a boolean flag")

parseBoundedInt
  :: String
  -> Int
  -> Int
  -> Int
  -> Maybe String
  -> Either String Int
parseBoundedInt _ defaultValue _ _ Nothing = Right defaultValue
parseBoundedInt name _ lower upper (Just rawValue) =
  case readMaybe (trim rawValue) of
    Just value | value >= lower && value <= upper -> Right value
    _ -> Left $ name <> " must be between " <> show lower <> " and " <> show upper

trim :: String -> String
trim = T.unpack . T.strip . T.pack

liftWorkerError :: String -> SqlPersistT IO a
liftWorkerError = liftIO . ioError . userError

redactLogValue :: String -> String
redactLogValue = take 500 . map replaceUnsafe
  where
    replaceUnsafe character
      | character `elem` ['\n', '\r', '\t', '"'] = ' '
      | otherwise = character
