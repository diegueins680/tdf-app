{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Network.Wai.Handler.Warp as Warp
import           Control.Concurrent      (forkFinally, myThreadId, throwTo, threadDelay)
import           Control.Exception       (SomeException, displayException, handle, throwIO, try)
import           Control.Monad            (forM_, when)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Char8    as BS
import           Data.Int                (Int64)
import           Data.IORef              (newIORef, readIORef, writeIORef)
import           Data.Maybe               (mapMaybe)
import           Data.Text                (Text)
import qualified Data.Text               as T
import           Database.Persist         ( (=.), upsert )
import           Database.Persist.Sql     (SqlPersistT, Single(..), rawExecute, rawSql, runMigration,
                                           runSqlPool, toSqlKey)
import           Database.Persist.Types   (PersistValue (PersistText))
import           Network.HTTP.Types       (RequestHeaders, status200, status500, status503)
import           Network.Wai              (Application, Middleware, mapResponseHeaders, pathInfo, requestHeaders, responseHeaders,
                                           responseLBS)
import           System.IO                (hPutStrLn, hSetEncoding, stdout, stderr)
import           GHC.IO.Encoding          (utf8, setLocaleEncoding)
import           Text.Read                (readMaybe)

import           TDF.Cors                 (corsPolicy)

import           TDF.Config     (AppConfig, appPort, dbConnString, loadConfig, ragEmbeddingDim, resetDb, runMigrations, seedDatabase)
import           TDF.Cron       (startCoursePaymentReminderJob, startInstagramSyncJob, startSocialAutoReplyJob)
import           TDF.DB         (Env(..), ConnectionPool, makePool)
import           TDF.Models     (EntityField (PartyRoleActive), PartyId, PartyRole(..), RoleEnum, migrateAll)
import           TDF.ModelsExtra (migrateExtra)
import           TDF.Trials.Models (migrateTrials)
import           TDF.Models.SocialEventsModels (migrateSocialEvents)
import           TDF.Server     (mkApp)
import           TDF.Seed       (seedAll)
main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  cfg <- loadConfig
  appCors <- corsPolicy

  let -- Ensure Warp-generated exception responses still carry CORS headers so browsers don't block them.
      addCorsToExceptionResponse ex =
        let base = Warp.defaultOnExceptionResponse ex
            hs = responseHeaders base
            extra =
              [ ("Access-Control-Allow-Origin", "*")
              , ("Vary", "Origin")
              ]
            merged = extra ++ filter (\(k, _) -> k /= "Access-Control-Allow-Origin" && k /= "Vary") hs
        in mapResponseHeaders (const merged) base
      warpSettings =
        Warp.setPort (appPort cfg) $
          Warp.setHost "0.0.0.0" $
            Warp.setOnExceptionResponse addCorsToExceptionResponse Warp.defaultSettings
      addCorsFallback :: Middleware
      addCorsFallback next req send =
        let originHeader = lookup "origin" (requestHeaders req :: RequestHeaders)
            originValue = maybe "*" id originHeader
            extra =
              [ ("Access-Control-Allow-Origin", originValue)
              , ("Vary", "Origin")
              ]
            applyHeaders res =
              let hs = responseHeaders res
                  merged = extra ++ filter (\(k, _) -> k /= "Access-Control-Allow-Origin" && k /= "Vary") hs
              in mapResponseHeaders (const merged) res
        in handle (\(ex :: SomeException) -> do
              hPutStrLn stderr ("Unhandled exception: " <> displayException ex)
              send (responseLBS status500 extra "Internal server error")
           ) $
             next req (\res -> send (applyHeaders res))
      rootOk :: Middleware
      rootOk next req send =
        if null (pathInfo req)
          then send (responseLBS status200 [("Content-Type", "text/plain")] "ok")
          else next req send
      wrapApp :: Application -> Application
      wrapApp = addCorsFallback . appCors . rootOk
      bootApp :: Application
      bootApp req send =
        case pathInfo req of
          ["health"] ->
            send
              (responseLBS status200 [("Content-Type", "application/json")] "{\"status\":\"starting\",\"db\":\"starting\"}")
          _ ->
            send (responseLBS status503 [("Content-Type", "application/json")] "{\"error\":\"starting\"}")

  appRef <- newIORef (wrapApp bootApp)

  mainThread <- myThreadId
  let setupApp = do
        pool <- makePoolWithRetry 5 (BS.pack (dbConnString cfg))
        if resetDb cfg
          then do
            putStrLn "Resetting DB schema..."
            runSqlPool resetSchema pool
          else
            putStrLn "RESET_DB disabled, preserving existing schema."
        if runMigrations cfg
          then do
            putStrLn "Running DB migrations..."
            runSqlPool (runAllMigrations cfg) pool
          else
            putStrLn "RUN_MIGRATIONS disabled (using pre-initialized schema)."
        when (seedDatabase cfg) $ do
          putStrLn "Seeding initial data..."
          runSqlPool seedAll pool
        putStrLn ("Starting server on port " <> show (appPort cfg))
        let env = Env{ envPool = pool, envConfig = cfg }
        writeIORef appRef (wrapApp (mkApp env))
        startCoursePaymentReminderJob env
        startInstagramSyncJob env
        startSocialAutoReplyJob env

  _ <-
    forkFinally
      setupApp
      (\res -> case res of
        Left err -> throwTo mainThread err
        Right _ -> pure ()
      )

  Warp.runSettings warpSettings $ \req send -> do
    app <- readIORef appRef
    app req send

resetSchema :: SqlPersistT IO ()
resetSchema = do
  rawExecute "DROP EXTENSION IF EXISTS pgcrypto" []
  rawExecute "DROP SCHEMA IF EXISTS public CASCADE" []
  rawExecute "CREATE SCHEMA public" []
  rawExecute "GRANT ALL ON SCHEMA public TO CURRENT_USER" []
  rawExecute "GRANT ALL ON SCHEMA public TO public" []

runAllMigrations :: AppConfig -> SqlPersistT IO ()
runAllMigrations cfg = do
  rawExecute "CREATE EXTENSION IF NOT EXISTS pgcrypto" []
  vectorAvailable <- hasVectorExtension
  if vectorAvailable
    then do
      rawExecute "CREATE EXTENSION IF NOT EXISTS vector" []
      let embeddingDim = ragEmbeddingDim cfg
      rawExecute
        (T.concat
          [ "CREATE TABLE IF NOT EXISTS rag_chunk ( "
          , " id BIGSERIAL PRIMARY KEY, "
          , " source TEXT NOT NULL, "
          , " source_id TEXT, "
          , " chunk_index INT NOT NULL, "
          , " content TEXT NOT NULL, "
          , " metadata JSONB, "
          , " embedding vector("
          , T.pack (show embeddingDim)
          , ") NOT NULL, "
          , " created_at TIMESTAMPTZ NOT NULL DEFAULT now(), "
          , " updated_at TIMESTAMPTZ NOT NULL DEFAULT now() "
          , ")"
          ]) []
      rawExecute
        "CREATE UNIQUE INDEX IF NOT EXISTS rag_chunk_source_key \
        \ON rag_chunk (source, source_id, chunk_index)" []
      rawExecute
        "CREATE INDEX IF NOT EXISTS rag_chunk_embedding_idx \
        \ON rag_chunk USING ivfflat (embedding vector_cosine_ops) WITH (lists = 100)" []
      rawExecute
        "CREATE INDEX IF NOT EXISTS rag_chunk_source_idx ON rag_chunk (source)" []
      rawExecute
        "CREATE INDEX IF NOT EXISTS rag_chunk_source_id_idx ON rag_chunk (source_id)" []
    else liftIO $ putStrLn "Vector extension not available; skipping rag_chunk setup."
  renameLegacyPartyRoleConstraint
  legacyRoles <- captureLegacyPartyRoles
  dropLegacyPartyColumns
  runMigration migrateAll
  runMigration migrateExtra
  runMigration migrateSocialEvents
  runMigration migrateTrials
  restoreLegacyPartyRoles legacyRoles

hasVectorExtension :: SqlPersistT IO Bool
hasVectorExtension = do
  rows <- rawSql
    "SELECT 1 FROM pg_available_extensions WHERE name = 'vector'"
    [] :: SqlPersistT IO [Single Int]
  pure (not (null rows))

captureLegacyPartyRoles :: SqlPersistT IO [(PartyId, RoleEnum)]
captureLegacyPartyRoles = do
  hasStatus <- columnExists "status"
  if not hasStatus
    then pure []
    else do
      rows <- rawSql
        "SELECT id, status FROM party WHERE status IS NOT NULL"
        [] :: SqlPersistT IO [(Single Int64, Single Text)]
      pure (mapMaybe rowToRole rows)
  where
    rowToRole :: (Single Int64, Single Text) -> Maybe (PartyId, RoleEnum)
    rowToRole (Single pid, Single roleTxt) =
      case readMaybe (T.unpack roleTxt) of
        Just role -> Just (toSqlKey pid, role)
        Nothing   -> Nothing

dropLegacyPartyColumns :: SqlPersistT IO ()
dropLegacyPartyColumns = do
  rawExecute "ALTER TABLE party DROP COLUMN IF EXISTS status" []
  rawExecute "ALTER TABLE party DROP COLUMN IF EXISTS updated_at" []

renameLegacyPartyRoleConstraint :: SqlPersistT IO ()
renameLegacyPartyRoleConstraint = do
  rawExecute
    "DO $$ \
    \BEGIN \
    \  IF EXISTS ( \
    \    SELECT 1 \
    \    FROM information_schema.table_constraints \
    \    WHERE constraint_schema = 'public' \
    \      AND table_name = 'party_role_assignment' \
    \      AND constraint_name = 'unique_party_role' \
    \  ) THEN \
    \    ALTER TABLE party_role_assignment \
    \      RENAME CONSTRAINT unique_party_role \
    \      TO unique_party_role_assignment; \
    \  END IF; \
    \END $$;"
    []

restoreLegacyPartyRoles :: [(PartyId, RoleEnum)] -> SqlPersistT IO ()
restoreLegacyPartyRoles [] = pure ()
restoreLegacyPartyRoles roles =
  forM_ roles $ \(pid, role) ->
    upsert (PartyRole pid role True) [PartyRoleActive =. True]

-- Retry DB pool creation to avoid failing fast on boot when the DB is not ready yet.
makePoolWithRetry :: Int -> BS.ByteString -> IO ConnectionPool
makePoolWithRetry retries connStr = do
  result <- try (makePool connStr) :: IO (Either SomeException ConnectionPool)
  case result of
    Right pool -> pure pool
    Left err ->
      if retries <= 0
        then do
          putStrLn "Failed to connect to database after retries. Crashing."
          throwIO err
        else do
          putStrLn $ "DB connection failed, retrying... attempts left: " <> show retries
          threadDelay (5 * 1000 * 1000)
          makePoolWithRetry (retries - 1) connStr

columnExists :: Text -> SqlPersistT IO Bool
columnExists column = do
  rows <- rawSql
    "SELECT 1 \
    \FROM information_schema.columns \
    \WHERE table_schema = 'public' \
      \AND table_name = ? \
      \AND column_name = ? \
    \LIMIT 1"
    [PersistText "party", PersistText column] :: SqlPersistT IO [Single Int]
  pure (not (null rows))
