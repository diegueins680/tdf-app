{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.App.Boot
  ( runBootServer
  , validateSeedDatabaseStartup
  ) where

import Control.Concurrent (forkFinally, myThreadId, threadDelay, throwTo)
import Control.Exception (SomeException, displayException, handle, throwIO, try)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist (upsert, (=.))
import Database.Persist.Sql (
    Single (..),
    SqlPersistT,
    rawExecute,
    rawSql,
    runMigration,
    runSqlPool,
    toSqlKey,
  )
import Database.Persist.Types (PersistValue (PersistText))
import Network.HTTP.Types (status200, status500, status503)
import Network.Wai (
    Application,
    Middleware,
    mapResponseHeaders,
    pathInfo,
    responseHeaders,
    responseLBS,
  )
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getEnvironment)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import TDF.Config (
    AppConfig,
    appPort,
    dbConnString,
    loadConfig,
    ragEmbeddingDim,
    resetDb,
    runMigrations,
    seedDatabase,
  )
import TDF.Cors (corsPolicy)
import TDF.Cron (
    startCoursePaymentReminderJob,
    startInstagramSyncJob,
    startSocialAutoReplyJob,
  )
import TDF.DB (ConnectionPool, Env (..), makePool)
import TDF.Models (
    EntityField (PartyRoleActive),
    PartyId,
    PartyRole (..),
    RoleEnum,
    migrateAll,
    roleFromText,
  )
import TDF.Models.SocialEventsModels (migrateSocialEvents)
import TDF.ModelsExtra (migrateExtra)
import TDF.Seed (seedAll, seedRecordsCmsContent, seededCredentialSeedingAllowed)
import TDF.Server (mkApp)
import TDF.Trials.Models (migrateTrials)

runBootServer :: IO ()
runBootServer = do
  cfg <- loadConfig
  startupEnv <- getEnvironment
  case validateSeedDatabaseStartup (seedDatabase cfg) startupEnv of
    Right () -> pure ()
    Left msg -> do
      hPutStrLn stderr msg
      throwIO (userError msg)
  appCors <- corsPolicy

  let
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
      handle
            ( \(ex :: SomeException) -> do
                hPutStrLn stderr ("Unhandled exception: " <> displayException ex)
                send (responseLBS status500 [("Content-Type", "text/plain; charset=utf-8")] "Internal server error")
            )
            $ next req send
    rootOk :: Middleware
    rootOk next req send =
      if null (pathInfo req)
        then send (responseLBS status200 [("Content-Type", "text/plain")] "ok")
        else next req send
    wrapApp :: Application -> Application
    wrapApp = appCors . addCorsFallback . rootOk
    bootStartingHeaders =
      [ ("Content-Type", "application/json")
      , ("Retry-After", "5")
      ]
    bootApp :: Application
    bootApp req send =
      case pathInfo req of
        ["health"] ->
          send
            (responseLBS status200 bootStartingHeaders "{\"status\":\"starting\",\"db\":\"starting\",\"message\":\"El servicio está arrancando. Intenta de nuevo en unos segundos.\"}")
        _ ->
          send (responseLBS status503 bootStartingHeaders "{\"error\":\"starting\",\"message\":\"El servicio está arrancando. Intenta de nuevo en unos segundos.\"}")

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
        now <- getCurrentTime
        putStrLn "Ensuring public CMS defaults..."
        runSqlPool (seedRecordsCmsContent now) pool
        when (seedDatabase cfg) $ do
          putStrLn "Seeding initial data..."
          runSqlPool seedAll pool
        putStrLn ("Starting server on port " <> show (appPort cfg))
        let env = Env{envPool = pool, envConfig = cfg}
        writeIORef appRef (wrapApp (mkApp env))
        startCoursePaymentReminderJob env
        startInstagramSyncJob env
        startSocialAutoReplyJob env

  _ <-
    forkFinally
      setupApp
      ( \res -> case res of
          Left err -> throwTo mainThread err
          Right _ -> pure ()
      )

  Warp.runSettings warpSettings $ \req send -> do
    app <- readIORef appRef
    app req send

validateSeedDatabaseStartup :: Bool -> [(String, String)] -> Either String ()
validateSeedDatabaseStartup shouldSeed env
  | shouldSeed && not (seededCredentialSeedingAllowed env) =
      Left
        "SEED_DB=true is not allowed in hosted or production runtimes. \
        \Disable SEED_DB before starting this service."
  | otherwise = Right ()

resetSchema :: SqlPersistT IO ()
resetSchema = do
  rawExecute "DROP EXTENSION IF EXISTS pgcrypto" []
  rawExecute "DROP SCHEMA IF EXISTS public CASCADE" []
  rawExecute "CREATE SCHEMA public" []
  rawExecute "GRANT ALL ON SCHEMA public TO CURRENT_USER" []
  rawExecute "GRANT ALL ON SCHEMA public TO public" []

runAllMigrations :: AppConfig -> SqlPersistT IO ()
runAllMigrations cfg = do
  ensureExtensionInstalled "pgcrypto"
  vectorAvailable <- hasVectorExtension
  if vectorAvailable
    then do
      ensureExtensionInstalled "vector"
      let embeddingDim = ragEmbeddingDim cfg
      rawExecute
        ( T.concat
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
            ]
        )
        []
      rawExecute
        "CREATE UNIQUE INDEX IF NOT EXISTS rag_chunk_source_key \
        \ON rag_chunk (source, source_id, chunk_index)"
        []
      rawExecute
        "CREATE INDEX IF NOT EXISTS rag_chunk_embedding_idx \
        \ON rag_chunk USING ivfflat (embedding vector_cosine_ops) WITH (lists = 100)"
        []
      rawExecute
        "CREATE INDEX IF NOT EXISTS rag_chunk_source_idx ON rag_chunk (source)"
        []
      rawExecute
        "CREATE INDEX IF NOT EXISTS rag_chunk_source_id_idx ON rag_chunk (source_id)"
        []
    else liftIO $ putStrLn "Vector extension not available; skipping rag_chunk setup."
  renameLegacyPartyRoleConstraint
  legacyRoles <- captureLegacyPartyRoles
  dropLegacyPartyColumns
  runMigration migrateAll
  runMigration migrateExtra
  ensureBrainTagsArray
  runMigration migrateSocialEvents
  runMigration migrateTrials
  restoreLegacyPartyRoles legacyRoles

hasVectorExtension :: SqlPersistT IO Bool
hasVectorExtension = do
  rows <-
    rawSql
      "SELECT 1 FROM pg_available_extensions WHERE name = 'vector'"
      [] ::
      SqlPersistT IO [Single Int]
  pure (not (null rows))

extensionInstalled :: Text -> SqlPersistT IO Bool
extensionInstalled extensionName = do
  rows <-
    rawSql
      "SELECT 1 FROM pg_extension WHERE extname = ? LIMIT 1"
      [PersistText extensionName] ::
      SqlPersistT IO [Single Int]
  pure (not (null rows))

ensureExtensionInstalled :: Text -> SqlPersistT IO ()
ensureExtensionInstalled extensionName = do
  installed <- extensionInstalled extensionName
  if installed
    then liftIO $ putStrLn ("Extension " <> T.unpack extensionName <> " already installed; skipping CREATE EXTENSION.")
    else rawExecute (T.concat ["CREATE EXTENSION IF NOT EXISTS ", extensionName]) []

ensureBrainTagsArray :: SqlPersistT IO ()
ensureBrainTagsArray = do
  mType <- lookupColumnType "studio_brain_entry" "tags"
  case mType of
    Nothing -> pure ()
    Just (dataType, udtName) -> do
      let normalizedType = T.toLower dataType
          normalizedUdt = T.toLower udtName
      if normalizedType == "array" && normalizedUdt == "_text"
        then pure ()
        else
          if normalizedType `elem` ["text", "character varying", "varchar"]
            then
              rawExecute
                "ALTER TABLE studio_brain_entry \
                \ALTER COLUMN tags TYPE text[] \
                \USING CASE \
                \WHEN tags IS NULL OR tags = '' THEN NULL \
                \ELSE string_to_array(tags, ',') \
                \END"
                []
            else
              if normalizedType == "jsonb" || normalizedUdt == "jsonb"
                then
                  rawExecute
                    "ALTER TABLE studio_brain_entry \
                    \ALTER COLUMN tags TYPE text[] \
                    \USING CASE \
                    \WHEN tags IS NULL THEN NULL \
                    \WHEN jsonb_typeof(tags) = 'array' \
                    \  THEN ARRAY(SELECT jsonb_array_elements_text(tags)) \
                    \ELSE string_to_array(trim(both '\"' from tags::text), ',') \
                    \END"
                    []
                else do
                  let message =
                        "[migrations] studio_brain_entry.tags type="
                          <> T.unpack dataType
                          <> " ("
                          <> T.unpack udtName
                          <> "); skipping conversion."
                  liftIO $ putStrLn message

captureLegacyPartyRoles :: SqlPersistT IO [(PartyId, RoleEnum)]
captureLegacyPartyRoles = do
  hasStatus <- columnExists "status"
  if not hasStatus
    then pure []
    else do
      rows <-
        rawSql
          "SELECT id, status FROM party WHERE status IS NOT NULL"
          [] ::
          SqlPersistT IO [(Single Int64, Single Text)]
      pure (mapMaybe rowToRole rows)
  where
    rowToRole :: (Single Int64, Single Text) -> Maybe (PartyId, RoleEnum)
    rowToRole (Single pid, Single roleTxt) =
      case roleFromText roleTxt of
        Just role -> Just (toSqlKey pid, role)
        Nothing ->
          case readMaybe (T.unpack roleTxt) of
            Just role -> Just (toSqlKey pid, role)
            Nothing -> Nothing

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
  rows <-
    rawSql
      "SELECT 1 \
      \FROM information_schema.columns \
      \WHERE table_schema = 'public' \
      \AND table_name = ? \
      \AND column_name = ? \
      \LIMIT 1"
      [PersistText "party", PersistText column] ::
      SqlPersistT IO [Single Int]
  pure (not (null rows))

lookupColumnType :: Text -> Text -> SqlPersistT IO (Maybe (Text, Text))
lookupColumnType tableName columnName = do
  rows <-
    rawSql
      "SELECT data_type, udt_name \
      \FROM information_schema.columns \
      \WHERE table_schema = 'public' \
      \  AND table_name = ? \
      \  AND column_name = ? \
      \LIMIT 1"
      [PersistText tableName, PersistText columnName] ::
      SqlPersistT IO [(Single Text, Single Text)]
  pure $ case rows of
    (Single dataType, Single udtName) : _ -> Just (dataType, udtName)
    _ -> Nothing
