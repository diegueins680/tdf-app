{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Concurrent
import Control.Exception
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Postgresql
import System.Environment
import TDF.Services.RecordsIngestion
import qualified TDF.Services.YouTube as Y

main :: IO ()
main = do
  connection <- BS.pack <$> getEnv "TDF_RECORDS_TEST_DATABASE_URL"
  runNoLoggingT $ withPostgresqlPool connection 4 $ \pool -> liftIO $ do
    let sql q p = runSqlPool (rawExecute q p) pool
        scalar q = runSqlPool (rawSql q [] :: SqlPersistT IO [Single Int64]) pool
        resetRate = sql "UPDATE social_sync_account SET records_ingestion=records_ingestion-'lastAttemptAt'" []
        status (Object o) = KM.lookup "status" o
        status _ = Nothing
        check label condition = unless condition (fail label)
        channel = "UCx9Jpaw_XDrMtIdzWYlU51g"
        video ident = Y.Video ident channel "Fixture video" "Fixture description" (UTCTime (fromGregorian 2026 9 1) 0) 120 [] True False
    now <- getCurrentTime
    sql "UPDATE social_sync_account SET records_ingestion=jsonb_set(records_ingestion,'{approvedAt}',to_jsonb(now()))" []
    sql "UPDATE catalog_backfill_run SET status='completed' WHERE run_code='runtime-fixture'" []
    pages <- newIORef ([] :: [Maybe Text])
    let provider = RecordsProvider
          (\_ -> pure (Right (Y.Channel channel "Fixture channel" "UUx9Jpaw_XDrMtIdzWYlU51g")))
          (\_ token -> do
            modifyIORef' pages (<>[token])
            pure . Right $ case token of
              Nothing -> Y.UploadPage ["probeVID001"] (Just "page-2")
              Just "page-2" -> Y.UploadPage ["probeVID002"] (Just "page-3")
              _ -> Y.UploadPage ["probeVID003"] Nothing)
          (\_ ids -> pure (Right (map (Y.PublicVideo . video) ids)))
        run key full dry = runRecordsIngestionWith provider pool 1 key full dry
    resetRate
    first <- run "pagination" False False
    check "first invocation must be partial" (status first == Just (String "partial"))
    other <- run "competing-key" False False
    check "another entry point must resume unfinished work" (status other == Just (String "resume_required"))
    resetRate
    resumed <- run "pagination" False False
    check "resumed run must complete" (status resumed == Just (String "completed"))
    observed <- readIORef pages
    check "durable checkpoint must avoid replaying previous pages" (observed == [Nothing,Just "page-2",Just "page-3"])
    countBefore <- scalar "SELECT count(*) FROM records_ingestion_change"
    replay <- run "pagination" False False
    countAfter <- scalar "SELECT count(*) FROM records_ingestion_change"
    check "completed key must replay with no side effects" (status replay == Just (String "completed") && countBefore==countAfter)
    resetRate
    sql "UPDATE records_ingestion_control SET enabled=false" []
    dry <- run "dry-stopped" False True
    check "dry run while stopped" (status dry == Just (String "partial"))
    blocked <- run "stopped" False False
    check "real import while stopped" (status blocked == Just (String "disabled_or_unapproved"))
    sql "UPDATE records_ingestion_control SET enabled=true" []
    resetRate
    let outage = provider { readPage = \_ _ -> pure (Left (Y.ProviderHttp 503)) }
    failure <- runRecordsIngestionWith outage pool 1 "outage" False False
    check "provider outage must fail explicitly" (status failure == Just (String "failed"))
    available <- scalar "SELECT count(*) FROM record_external_resource WHERE external_code LIKE 'probeVID%' AND availability='available'"
    check "outage must not withdraw known videos" (available == [Single 3])
    resetRate
    _ <- run "outage" False False
    resetRate
    _ <- run "outage" False False
    resetRate
    let missing = provider { readPage = \_ _ -> pure (Right (Y.UploadPage [] Nothing)), readVideos = \_ ids -> pure (Right (map Y.MissingVideo ids)) }
    reconciled <- runRecordsIngestionWith missing pool 1 "known-reconciliation" True False
    check "full reconciliation completes known-resource phase" (status reconciled == Just (String "completed"))
    unavailable <- scalar "SELECT count(*) FROM record_external_resource WHERE external_code LIKE 'probeVID%' AND availability='unavailable' AND availability_reason='not_publicly_accessible'"
    check "known absent videos require direct metadata verification" (unavailable == [Single 3])
    resetRate
    entered <- newEmptyMVar
    release <- newEmptyMVar
    completed <- newEmptyMVar
    let slow = provider { readPage = \_ _ -> putMVar entered () >> takeMVar release >> pure (Right (Y.UploadPage [] Nothing)) }
    worker <- forkIO $ (try (runRecordsIngestionWith slow pool 1 "slow" False False) :: IO (Either SomeException Value)) >>= putMVar completed
    takeMVar entered
    competing <- run "race" False False
    check "concurrent manual/scheduler must be busy" (status competing == Just (String "busy"))
    killThread worker
    void (takeMVar completed)
    resetRate
    recovered <- run "slow" False False
    check "cancellation must release the advisory lock" (status recovered == Just (String "partial"))
    putStrLn ("Runtime probes passed: checkpoint/resume, cross-key control, replay, stopped dry-run, outage safety, known-resource reconciliation, race, cancellation. Started " <> show now)
