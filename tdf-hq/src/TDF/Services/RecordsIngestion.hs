{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- All execution entry points use one connection/session lock and commit a page
-- with its checkpoint. Provider failures never mean upstream deletion.
module TDF.Services.RecordsIngestion
  ( runRecordsIngestion, runRecordsIngestionWith, RecordsProvider(..)
  , recordsIngestionSlot, recordsReconciliationSlot, startRecordsIngestionJob, videoPayload ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (Exception, SomeException, SomeAsyncException, bracket, fromException, throwIO, try)
import Control.Monad (forM, forM_, forever, void)
import Data.Aeson (Value(..), object, (.=), encode, decodeStrict')
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Pool (withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime(..), getCurrentTime, addDays, dayOfWeek, DayOfWeek(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (SqlPersistT, Single(..), rawSql, rawExecute, runSqlConn, runSqlPool)
import System.Environment (lookupEnv)
import TDF.DB (ConnectionPool, sharedTlsManager)
import qualified TDF.LogBuffer as Log
import qualified TDF.Services.YouTube as Y

newtype ProviderFailure = ProviderFailure Y.ProviderError deriving Show
instance Exception ProviderFailure

providerResult :: Either Y.ProviderError a -> IO a
providerResult = either (throwIO . ProviderFailure) pure

-- Only server-side code can substitute this interface. HTTP callers cannot
-- supply provider metadata or network destinations.
data RecordsProvider = RecordsProvider
  { readChannel :: Text -> IO (Either Y.ProviderError Y.Channel)
  , readPage :: Text -> Maybe Text -> IO (Either Y.ProviderError Y.UploadPage)
  , readVideos :: Text -> [Text] -> IO (Either Y.ProviderError [Y.VideoResult])
  }

officialProvider :: RecordsProvider
officialProvider = RecordsProvider
  (\channel -> withKey (\key -> Y.fetchChannel sharedTlsManager key channel))
  (\playlist token -> withKey (\key -> Y.fetchUploadPage sharedTlsManager key playlist token))
  (\channel ids -> withKey (\key -> Y.fetchVideos sharedTlsManager key channel ids))
  where
    withKey request = do
      secret <- maybe "" T.pack <$> lookupEnv "YOUTUBE_API_KEY"
      either (pure . Left) request (Y.youTubeKey secret)

recordsIngestionSlot :: Int -> UTCTime -> UTCTime
recordsIngestionSlot seconds now = posixSecondsToUTCTime . fromInteger $
  (floor (utcTimeToPOSIXSeconds now) `div` interval) * interval
  where interval = fromIntegral (max 300 (min 86400 seconds))

-- Sunday 00:00 UTC is a single weekly reconciliation identity. The latest
-- weekly slot is caught up once after downtime, without replaying missed weeks.
recordsReconciliationSlot :: UTCTime -> UTCTime
recordsReconciliationSlot now = UTCTime (addDays (negate offset) day) 0
  where
    day = utctDay now
    offset = case dayOfWeek day of
      Sunday -> 0; Monday -> 1; Tuesday -> 2; Wednesday -> 3
      Thursday -> 4; Friday -> 5; Saturday -> 6

startRecordsIngestionJob :: ConnectionPool -> IO ()
startRecordsIngestionJob pool = do
  Log.addLog Log.LogInfo "[Cron][Records] Hourly videos; weekly Sunday reconciliation; database enable/approval gates."
  void . forkIO . forever $ do
    result <- try cycleOnce
    case result of
      Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> Log.addLog Log.LogError "[Cron][Records] Scheduler/database failure"
      Right () -> pure ()
    threadDelay (60 * 1000000)
  where
    cycleOnce = do
      -- Retention still applies when imports are stopped or sources disabled.
      void $ runSqlPool (rawSql "SELECT tdf_expire_records_provider_data()" [] :: SqlPersistT IO [Single Int]) pool
      sources <- runSqlPool (rawSql
        "SELECT a.id,c.interval_seconds FROM social_sync_account a CROSS JOIN records_ingestion_control c WHERE c.enabled AND a.platform='youtube' AND a.records_ingestion->>'enabled'='true' AND nullif(a.records_ingestion->>'approvalReference','') IS NOT NULL AND a.records_ingestion->>'approvedAt' IS NOT NULL ORDER BY a.records_ingestion->>'lastAttemptAt' NULLS FIRST,a.id LIMIT 100"
        [] :: SqlPersistT IO [(Single Int64, Single Int)]) pool
      forM_ sources $ \(Single sourceId, Single interval) -> do
        now <- getCurrentTime
        -- Resume any interrupted run before a newer schedule identity. This
        -- applies to incremental pages as well: bursts cannot be truncated.
        pending <- runSqlPool (rawSql
          "SELECT run_code,coalesce((report::jsonb->>'full')::boolean,false) FROM catalog_backfill_run WHERE candidate_revision='youtube-api-v1' AND NOT dry_run AND report::jsonb->>'sourceAccountId'=? AND status IN ('running','partial','failed','deferred') ORDER BY started_at LIMIT 1"
          [PersistText (T.pack (show sourceId))] :: SqlPersistT IO [(Single Text, Single Bool)]) pool
        let weekly = T.pack (show (recordsReconciliationSlot now))
            prefix fullRun = (if fullRun then "records-youtube:full:" else "records-youtube:incremental:") <> T.pack (show sourceId) <> ":"
        weeklyDone <- runSqlPool (rawSql
          "SELECT count(*)::bigint FROM catalog_backfill_run WHERE run_code=? AND candidate_revision='youtube-api-v1' AND NOT dry_run AND status='completed'"
          [PersistText (prefix True <> weekly)] :: SqlPersistT IO [Single Int64]) pool
        let (key, fullRun) = case pending of
              (Single code, Single mode):_ -> (T.drop (T.length (prefix mode)) code, mode)
              _ | weeklyDone == [Single 0] -> (weekly, True)
                | otherwise -> (T.pack (show (recordsIngestionSlot interval now)), False)
        result <- try (runRecordsIngestion pool sourceId key fullRun False)
        case result of
          Right outcome -> Log.addLog Log.LogInfo ("[Cron][Records] " <> jsonText outcome)
          Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
            Just _ -> throwIO err
            Nothing -> Log.addLog Log.LogError ("[Cron][Records] Source failed: " <> T.pack (show sourceId))

jsonText :: Value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . encode

fieldText :: Text -> Value -> Maybe Text
fieldText name (Object obj) = case KM.lookup (fromStringKey name) obj of
  Just (String value) -> Just value
  _ -> Nothing
  where fromStringKey = Key.fromText
fieldText _ _ = Nothing

-- Only the official adapter can construct an eligible public Video. The SQL
-- boundary independently checks eligibility, channel, approval and image ID.
videoPayload :: UTCTime -> Y.Video -> Value
videoPayload now video = object
  [ "id" .= Y.videoId video, "channelId" .= Y.videoChannelId video
  , "title" .= Y.videoTitle video, "description" .= Y.videoDescription video
  , "publishedAt" .= Y.videoPublishedAt video, "verifiedAt" .= now
  , "durationSeconds" .= Y.videoDurationSeconds video
  , "thumbnailUrl" .= fmap Y.thumbnailUrl (listToMaybe (Y.videoThumbnails video))
  , "thumbnails" .= map (\t -> object ["url" .= Y.thumbnailUrl t,
      "width" .= Y.thumbnailWidth t,"height" .= Y.thumbnailHeight t]) (Y.videoThumbnails video)
  , "embeddable" .= Y.videoEmbeddable video, "completedLive" .= Y.videoCompletedLive video
  , "privacyStatus" .= ("public" :: Text), "uploadStatus" .= ("processed" :: Text)
  , "eligible" .= True
  ]

-- Caller supplies a stable execution key (same key for a retry), never metadata
-- or provider URLs. Full runs resume at the last committed page, at most two
-- pages per invocation for both incremental and reconciliation executions.
runRecordsIngestion :: ConnectionPool -> Int64 -> Text -> Bool -> Bool -> IO Value
runRecordsIngestion = runRecordsIngestionWith officialProvider

runRecordsIngestionWith :: RecordsProvider -> ConnectionPool -> Int64 -> Text -> Bool -> Bool -> IO Value
runRecordsIngestionWith provider pool accountId executionKey full dryRun
  | accountId <= 0 || accountId > 2147483647 || T.null executionKey || T.length executionKey > 150 =
      pure (object ["status" .= ("invalid_request" :: Text)])
  | otherwise = withResource pool $ \backend -> bracket
      (runSqlConn (rawSql "SELECT pg_try_advisory_lock(20260920, ?::integer)"
        [PersistInt64 accountId] :: SqlPersistT IO [Single Bool]) backend)
      (\locked -> if locked == [Single True] then
        void (runSqlConn (rawSql "SELECT pg_advisory_unlock(20260920, ?::integer)"
          [PersistInt64 accountId] :: SqlPersistT IO [Single Bool]) backend)
        else pure ())
      (\locked -> if locked == [Single True] then runLocked backend
        else pure (object ["status" .= ("busy" :: Text)]))
  where
    runLocked backend = do
      now <- getCurrentTime
      sources <- runSqlConn (rawSql
        ("SELECT external_user_id FROM social_sync_account WHERE id=? AND platform='youtube' AND records_ingestion->>'enabled'='true' AND nullif(records_ingestion->>'approvalReference','') IS NOT NULL AND records_ingestion->>'approvedAt' IS NOT NULL"
          <> if dryRun then "" else " AND EXISTS(SELECT 1 FROM records_ingestion_control WHERE singleton AND enabled)")
        [PersistInt64 accountId] :: SqlPersistT IO [Single Text]) backend
      case sources of
        [Single channel] -> do
          let runCode = (if full then "records-youtube:full:" else "records-youtube:incremental:") <> T.pack (show accountId) <> ":" <> executionKey
              initial = object ["sourceAccountId" .= T.pack (show accountId), "full" .= full,
                "checkpoint" .= Null, "pages" .= (0 :: Int), "created" .= (0 :: Int),
                "updated" .= (0 :: Int), "unchanged" .= (0 :: Int), "reviewed" .= (0 :: Int)]
          pending <- runSqlConn (rawSql
            "SELECT id::text,run_code,report::text FROM catalog_backfill_run WHERE candidate_revision='youtube-api-v1' AND dry_run=? AND report::jsonb->>'sourceAccountId'=? AND status IN ('running','partial','failed','deferred') AND run_code<>? ORDER BY started_at LIMIT 1"
            [PersistBool dryRun,PersistText (T.pack (show accountId)),PersistText runCode]
            :: SqlPersistT IO [(Single Text,Single Text,Single Text)]) backend
          if not (null pending) then case pending of
            (Single ident,Single code,Single saved):_ -> pure (object
              ["status" .= ("resume_required" :: Text),"runId" .= ident,"key" .= code,
               "report" .= (decodeStrict' (TE.encodeUtf8 saved) :: Maybe Value)])
            _ -> fail "unreachable pending run"
          else claimAndRun backend channel now runCode initial
        _ -> pure (object ["status" .= ("disabled_or_unapproved" :: Text)])

    claimAndRun backend channel now runCode initial = do
          rows <- runSqlConn (rawSql
            "INSERT INTO catalog_backfill_run(run_code,candidate_revision,dry_run,status,started_at,report,correlation_id) VALUES(?,'youtube-api-v1',?,'running',?, ?,?) ON CONFLICT(run_code,candidate_revision,dry_run) DO UPDATE SET correlation_id=catalog_backfill_run.correlation_id RETURNING id::text,status,report::text"
            [PersistText runCode,PersistBool dryRun,PersistUTCTime now,PersistText (jsonText initial),PersistText runCode]
            :: SqlPersistT IO [(Single Text,Single Text,Single Text)]) backend
          case rows of
            [(Single runId,Single status,Single report)]
              | status `elem` ["completed","cancelled"] -> pure (object ["runId" .= runId,"status" .= status,"replay" .= True])
              | otherwise -> do
                  -- A durable attempt time bounds retries across replicas,
                  -- scheduler restarts and different manual execution keys.
                  permit <- runSqlConn (rawSql
                    "UPDATE social_sync_account SET records_ingestion=jsonb_set(records_ingestion,'{lastAttemptAt}',to_jsonb(now())) WHERE id=? AND (records_ingestion->>'lastAttemptAt' IS NULL OR (records_ingestion->>'lastAttemptAt')::timestamptz < now()-interval '60 seconds') RETURNING id"
                    [PersistInt64 accountId] :: SqlPersistT IO [Single Int64]) backend
                  if null permit then do
                    runSqlConn (rawExecute "UPDATE catalog_backfill_run SET status='deferred' WHERE id=?::uuid AND status='running' AND scanned_rows=0" [PersistText runId]) backend
                    pure (object ["runId" .= runId,"status" .= ("rate_limited" :: Text)])
                  else do
                   runSqlConn (rawExecute "UPDATE catalog_backfill_run SET status='running',completed_at=NULL,report=(report::jsonb-'error')::text WHERE id=?::uuid"
                     [PersistText runId]) backend
                   result <- try $ do
                     budget <- runSqlConn (rawSql
                       "INSERT INTO records_ingestion_quota(day,reserved_units) VALUES((now() AT TIME ZONE 'UTC')::date,15) ON CONFLICT(day) DO UPDATE SET reserved_units=records_ingestion_quota.reserved_units+15 WHERE records_ingestion_quota.reserved_units+15<=9000 RETURNING reserved_units"
                       [] :: SqlPersistT IO [Single Int]) backend
                     if null budget then throwIO (ProviderFailure (Y.ProviderHttp 429)) else pure ()
                     verifiedChannel <- readChannel provider channel >>= providerResult
                     if Y.channelId verifiedChannel /= channel then throwIO (ProviderFailure Y.InvalidIdentity) else pure ()
                     let saved = decodeStrict' (TE.encodeUtf8 report)
                         token = saved >>= fieldText "checkpoint"
                         phase = maybe "uploads" id (saved >>= fieldText "phase")
                     ingestPages backend channel (Y.uploadsPlaylistId verifiedChannel) runId phase token 0
                   case result of
                     Left (err :: SomeException) -> case fromException err :: Maybe SomeAsyncException of
                       Just _ -> throwIO err
                       Nothing -> do
                         -- Do not serialize exception requests or credentials.
                         let reason = case fromException err :: Maybe ProviderFailure of
                               Just (ProviderFailure providerError) -> T.pack (show providerError)
                               Nothing -> "database_or_checkpoint_failure"
                         runSqlConn (rawExecute
                           "UPDATE catalog_backfill_run SET status='failed',completed_at=now(),report=((report::jsonb)||jsonb_build_object('error',?::text))::text WHERE id=?::uuid"
                           [PersistText reason,PersistText runId]) backend
                         pure (object ["runId" .= runId,"status" .= ("failed" :: Text)])
                     Right status' -> pure (object ["runId" .= runId,"status" .= status'])
            _ -> fail "Unable to claim canonical ingestion run"

    ingestPages backend channel playlist runId phase token pageCount = do
      page <- if phase == "known" then do
        rows <- runSqlConn (rawSql
          "SELECT external_code FROM record_external_resource WHERE source_account_id=? AND external_code>? ORDER BY external_code LIMIT 51"
          [PersistInt64 accountId, PersistText (maybe "" id token)] :: SqlPersistT IO [Single Text]) backend
        let ids = map unSingle rows
            batch = take 50 ids
        pure (Y.UploadPage batch (if length ids > 50 then Just (last batch) else Nothing))
        else readPage provider playlist token >>= providerResult
      videos <- if null (Y.uploadVideoIds page) then pure []
        else readVideos provider channel (Y.uploadVideoIds page) >>= providerResult
      now <- getCurrentTime
      let next = Y.uploadNextPage page
          nextPhase = if phase == "uploads" && next == Nothing && full then "known" else phase
          completed = next == Nothing && nextPhase == phase
          continuing = not completed && pageCount < (1 :: Int)
          status = if completed then "completed" else if continuing then "running" else "partial" :: Text
      if next == token && next /= Nothing then fail "repeated provider checkpoint" else pure ()
      -- All writes for this page and its next checkpoint commit together.
      runSqlConn (do
        enabled <- rawSql
          "SELECT true FROM records_ingestion_control c JOIN social_sync_account a ON a.id=? WHERE c.singleton AND (c.enabled OR ?) AND a.records_ingestion->>'enabled'='true' AND nullif(a.records_ingestion->>'approvalReference','') IS NOT NULL AND a.records_ingestion->>'approvedAt' IS NOT NULL FOR SHARE OF c,a"
          [PersistInt64 accountId, PersistBool dryRun] :: SqlPersistT IO [Single Bool]
        if enabled /= [Single True] then fail "ingestion stopped" else pure ()
        actions <- forM videos $ \video -> case video of
          Y.PublicVideo value | not dryRun -> do
            rows <- rawSql "SELECT tdf_ingest_public_video(?,?::uuid,?::jsonb)"
              [PersistInt64 accountId,PersistText runId,PersistText (jsonText (videoPayload now value))]
              :: SqlPersistT IO [Single Text]
            case rows of [Single action] -> pure action; _ -> fail "missing ingestion result"
          Y.PublicVideo _ -> pure "eligible"
          Y.ReviewVideo ident reason -> recordReview runId ident reason "reviewed"
          Y.NonPublicVideo ident -> unavailableVideo now runId ident "nonpublic" "skipped"
          Y.MissingVideo ident -> unavailableVideo now runId ident "not_publicly_accessible" "unavailable"
        let n action = length (filter (==action) actions)
            counters = object ["created" .= n "created", "updated" .= n "updated",
              "unchanged" .= n "unchanged", "reviewed" .= n "reviewed", "skipped" .= n "skipped",
              "unavailable" .= n "unavailable", "eligible" .= n "eligible"]
        rawExecute
          "UPDATE catalog_backfill_run SET status=?,completed_at=CASE WHEN ? THEN now() ELSE NULL END,scanned_rows=scanned_rows+?,mapped_rows=mapped_rows+?,ambiguous_rows=ambiguous_rows+?,rejected_rows=rejected_rows+?,report=((report::jsonb)||jsonb_build_object('checkpoint',?::text,'phase',?::text,'pages',coalesce((report::jsonb->>'pages')::integer,0)+1,'lastPageCounts',?::jsonb,'counts',(SELECT jsonb_object_agg(k,coalesce((report::jsonb->'counts'->>k)::integer,0)+v::integer) FROM jsonb_each_text(?::jsonb) AS c(k,v))))::text WHERE id=?::uuid"
          [PersistText status,PersistBool completed,PersistInt64 (fromIntegral (length videos)),
           PersistInt64 (fromIntegral (n "created"+n "updated"+n "unchanged")),
           PersistInt64 (fromIntegral (n "reviewed")),PersistInt64 (fromIntegral (n "skipped"+n "unavailable")),
           maybe PersistNull PersistText next,PersistText nextPhase,PersistText (jsonText counters),PersistText (jsonText counters),PersistText runId]
        if dryRun then pure () else rawExecute
          "UPDATE social_sync_account SET last_synced_at=CASE WHEN ? THEN ? ELSE last_synced_at END,updated_at=? WHERE id=?"
          [PersistBool completed,PersistUTCTime now,PersistUTCTime now,PersistInt64 accountId]
        ) backend
      if completed || pageCount >= (1 :: Int) then pure status
        else ingestPages backend channel playlist runId nextPhase next (pageCount+1)

      where
        unavailableVideo now runId' ident reason action = do
          if dryRun then pure () else void (rawSql
            "SELECT tdf_mark_video_unavailable(?,?::uuid,?,?,?)"
            [PersistInt64 accountId,PersistText runId',PersistText ident,PersistText reason,PersistUTCTime now]
            :: SqlPersistT IO [Single Text])
          recordReview runId' ident reason action

        recordReview :: Text -> Text -> Text -> Text -> SqlPersistT IO Text
        recordReview runId' ident reason action = do
          if dryRun then pure () else rawExecute
            "INSERT INTO records_ingestion_change(run_id,source_account_id,action,after_value) SELECT ?::uuid,?,?,?::jsonb WHERE NOT EXISTS (SELECT 1 FROM records_ingestion_change WHERE run_id=?::uuid AND source_account_id=? AND after_value->>'externalCode'=? AND action=?)"
            [PersistText runId',PersistInt64 accountId,PersistText action,
             PersistText (jsonText (object ["externalCode" .= ident,"reason" .= reason])),
             PersistText runId',PersistInt64 accountId,PersistText ident,PersistText action]
          pure action
