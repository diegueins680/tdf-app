{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TDF.App.Boot
  ( runBootServer
  , validateDatabaseStartupSafety
  , validateSeedDatabaseStartup
  ) where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, displayException, handle, throwIO, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Sql (
    Single (..),
    SqlPersistT,
    rawExecute,
    rawSql,
    runMigration,
    runSqlPool,
    toSqlKey,
  )
import Database.Persist.Types (PersistValue (PersistBool, PersistText))
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
    defaultCurrency,
    defaultLocale,
    loadConfig,
    ragEmbeddingDim,
    resetDb,
    runMigrations,
    seedDatabase,
    supportedCurrencies,
    supportedLocales,
  )
import TDF.Cors (corsPolicy)
import TDF.CampaignAutomation (startCampaignAutomationJob)
import qualified TDF.CMS.Models as CMS
import qualified TDF.Catalog.Models as Catalog
import TDF.Catalog.Integrity (applyCatalogIntegrity)
import TDF.Catalog.Security (ensureBootstrapSecurityRole, validateSecurityRegistry)
import TDF.Catalog.Seed (seedCatalogFoundation, validateCatalogRuntimeRegistries)
import qualified TDF.DDEX.Models as Ddex
import TDF.Cron (
    startCoursePaymentReminderJob,
    startEventDiscoveryJob,
    startEventLogisticsRecheckJob,
    startArtistEnrichmentJob,
    startInstagramSyncJob,
    startSocialAutoReplyJob,
  )
import TDF.DB (ConnectionPool, Env (..), makePool)
import TDF.Models (
    PartyId,
    RoleEnum,
    migrateAll,
    roleFromText,
  )
import TDF.Models.SocialEventsModels (migrateSocialEvents)
import TDF.ModelsExtra (migrateExtra)
import TDF.Operations.Worker (startOperationsWorker)
import TDF.Seed (seedAll, seededCredentialSeedingAllowed)
import TDF.Server (mkApp)
import TDF.Trials.Models (migrateTrials)

runBootServer :: IO ()
runBootServer = do
  cfg <- loadConfig
  startupEnv <- getEnvironment
  case validateDatabaseStartupSafety (resetDb cfg) (seedDatabase cfg) startupEnv of
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
        putStrLn "Validating persisted catalog runtime registries..."
        runSqlPool validateCatalogRuntimeRegistries pool
        runSqlPool (validateRegionalDeploymentConfig cfg) pool
        when (seedDatabase cfg) $ do
          putStrLn "Seeding initial data..."
          runSqlPool seedAll pool
        putStrLn ("Starting server on port " <> show (appPort cfg))
        let env = Env{envPool = pool, envConfig = cfg}
        writeIORef appRef (wrapApp (mkApp env))
        startCoursePaymentReminderJob env
        startEventDiscoveryJob env
        startEventLogisticsRecheckJob env
        startArtistEnrichmentJob env
        startInstagramSyncJob env
        startSocialAutoReplyJob env
        startCampaignAutomationJob env
        startOperationsWorker env

  serverResult <- newEmptyMVar
  _ <-
    forkFinally
      (Warp.runSettings warpSettings $ \req send -> do
        app <- readIORef appRef
        app req send)
      (putMVar serverResult)

  -- Keep initialization on the main thread so a failed migration or startup
  -- registry check always terminates the process with a non-zero exit status.
  -- Warp runs concurrently only to expose the bounded `starting` health state.
  setupApp
  takeMVar serverResult >>= either throwIO pure

validateSeedDatabaseStartup :: Bool -> [(String, String)] -> Either String ()
validateSeedDatabaseStartup shouldSeed env
  | shouldSeed && not (seededCredentialSeedingAllowed env) =
      Left
        "SEED_DB=true is not allowed in hosted or production runtimes. \
        \Disable SEED_DB before starting this service."
  | otherwise = Right ()

validateDatabaseStartupSafety :: Bool -> Bool -> [(String, String)] -> Either String ()
validateDatabaseStartupSafety shouldReset shouldSeed env
  | shouldReset && not (seededCredentialSeedingAllowed env) =
      Left
        "RESET_DB=true is not allowed in hosted or production runtimes. \
        \Disable RESET_DB before starting this service."
  | otherwise = validateSeedDatabaseStartup shouldSeed env

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
  legacyRoles <- captureLegacyPartyRoles
  dropLegacyPartyColumns
  runMigration migrateAll
  runMigration CMS.migrateCMS
  runMigration Catalog.migrateCatalogGovernance
  runMigration Catalog.migrateCatalogSecurity
  runMigration Catalog.migrateCatalogReferences
  runMigration Catalog.migrateCatalogDomains
  -- DDEX documents, exports and partner policies now reference governed
  -- catalog rows and workflow states through canonical UUID foreign keys.
  runMigration Ddex.migrateDdex
  -- Extra operational models now carry typed foreign keys into catalog
  -- domains (for example pipeline cards -> services/workflow states), so the
  -- referenced catalog tables must exist before Persistent migrates them.
  runMigration migrateExtra
  rawExecute
    "CREATE UNIQUE INDEX IF NOT EXISTS uq_marketplace_cart_active_stripe_payment \
    \ON marketplace_order (cart_id) \
    \WHERE cart_id IS NOT NULL AND status = 'stripe_pending'"
    []
  -- Social catalog consumers must exist before catalog integrity installs
  -- their foreign keys and validates legacy reference columns. This ordering
  -- is required for a clean database as well as an in-place upgrade.
  runMigration migrateSocialEvents
  -- Upgrade validation functions before seeding values that use newly
  -- recognized policy triggers. This keeps rolling upgrades restartable when
  -- an older trigger is already installed in the target database.
  applyCatalogIntegrity
  seedCatalogFoundation
  syncRegionalDeploymentEnablement cfg
  ensureBrainTagsArray
  runMigration migrateTrials
  ensureInternationalColumns
  restoreLegacyPartyRoles legacyRoles
  validateSecurityRegistry

syncRegionalDeploymentEnablement :: AppConfig -> SqlPersistT IO ()
syncRegionalDeploymentEnablement cfg = do
  localeRows <- rawSql
    "SELECT code FROM locale_reference ORDER BY code"
    [] :: SqlPersistT IO [Single Text]
  activeLocaleRows <- rawSql
    "SELECT code FROM locale_reference WHERE active AND deprecated_at IS NULL ORDER BY code"
    [] :: SqlPersistT IO [Single Text]
  let activeLocaleCodes = [code | Single code <- activeLocaleRows]
  unless (all (`elem` activeLocaleCodes) (supportedLocales cfg)) $
    liftIO . ioError . userError $
      "SUPPORTED_LOCALES contains a value without an active persisted locale_reference"
  rawExecute
    "UPDATE deployment_locale_enablement enabled SET default_locale=FALSE, updated_at=CURRENT_TIMESTAMP, version=enabled.version+1 FROM locale_reference item WHERE enabled.locale_id=item.id AND enabled.deployment_code='default' AND enabled.default_locale AND item.code<>?"
    [PersistText (defaultLocale cfg)]
  forM_ [code | Single code <- localeRows] $ \code -> do
    let enabled = code `elem` supportedLocales cfg
        isDefault = enabled && code == defaultLocale cfg
    rawExecute
      "INSERT INTO deployment_locale_enablement (deployment_code, locale_id, enabled, default_locale, updated_at, version) SELECT 'default', id, ?, ?, CURRENT_TIMESTAMP, 1 FROM locale_reference WHERE code=? ON CONFLICT (deployment_code, locale_id) DO UPDATE SET enabled=EXCLUDED.enabled, default_locale=EXCLUDED.default_locale, updated_at=CURRENT_TIMESTAMP, version=deployment_locale_enablement.version+1 WHERE deployment_locale_enablement.enabled IS DISTINCT FROM EXCLUDED.enabled OR deployment_locale_enablement.default_locale IS DISTINCT FROM EXCLUDED.default_locale"
      [PersistBool enabled, PersistBool isDefault, PersistText code]
  currencyRows <- rawSql
    "SELECT code FROM currency_reference ORDER BY code"
    [] :: SqlPersistT IO [Single Text]
  activeCurrencyRows <- rawSql
    "SELECT code FROM currency_reference WHERE active AND deprecated_at IS NULL ORDER BY code"
    [] :: SqlPersistT IO [Single Text]
  let activeCurrencyCodes = [code | Single code <- activeCurrencyRows]
  unless (all (`elem` activeCurrencyCodes) (supportedCurrencies cfg)) $
    liftIO . ioError . userError $
      "SUPPORTED_CURRENCIES contains a value without an active persisted currency_reference"
  rawExecute
    "UPDATE deployment_currency_enablement enabled SET default_currency=FALSE, updated_at=CURRENT_TIMESTAMP, version=enabled.version+1 FROM currency_reference item WHERE enabled.currency_id=item.id AND enabled.deployment_code='default' AND enabled.default_currency AND item.code<>?"
    [PersistText (defaultCurrency cfg)]
  forM_ [code | Single code <- currencyRows] $ \code -> do
    let enabled = code `elem` supportedCurrencies cfg
        isDefault = enabled && code == defaultCurrency cfg
    rawExecute
      "INSERT INTO deployment_currency_enablement (deployment_code, currency_id, enabled, default_currency, updated_at, version) SELECT 'default', id, ?, ?, CURRENT_TIMESTAMP, 1 FROM currency_reference WHERE code=? ON CONFLICT (deployment_code, currency_id) DO UPDATE SET enabled=EXCLUDED.enabled, default_currency=EXCLUDED.default_currency, updated_at=CURRENT_TIMESTAMP, version=deployment_currency_enablement.version+1 WHERE deployment_currency_enablement.enabled IS DISTINCT FROM EXCLUDED.enabled OR deployment_currency_enablement.default_currency IS DISTINCT FROM EXCLUDED.default_currency"
      [PersistBool enabled, PersistBool isDefault, PersistText code]

validateRegionalDeploymentConfig :: AppConfig -> SqlPersistT IO ()
validateRegionalDeploymentConfig cfg = do
  localeRows <- rawSql
    "SELECT item.code FROM deployment_locale_enablement enabled JOIN locale_reference item ON item.id=enabled.locale_id WHERE enabled.deployment_code='default' AND enabled.enabled AND item.active AND item.deprecated_at IS NULL ORDER BY item.code"
    [] :: SqlPersistT IO [Single Text]
  currencyRows <- rawSql
    "SELECT item.code FROM deployment_currency_enablement enabled JOIN currency_reference item ON item.id=enabled.currency_id WHERE enabled.deployment_code='default' AND enabled.enabled AND item.active AND item.deprecated_at IS NULL ORDER BY item.code"
    [] :: SqlPersistT IO [Single Text]
  localeDefaultRows <- rawSql
    "SELECT item.code FROM deployment_locale_enablement enabled JOIN locale_reference item ON item.id=enabled.locale_id WHERE enabled.deployment_code='default' AND enabled.enabled AND enabled.default_locale AND item.active AND item.deprecated_at IS NULL"
    [] :: SqlPersistT IO [Single Text]
  currencyDefaultRows <- rawSql
    "SELECT item.code FROM deployment_currency_enablement enabled JOIN currency_reference item ON item.id=enabled.currency_id WHERE enabled.deployment_code='default' AND enabled.enabled AND enabled.default_currency AND item.active AND item.deprecated_at IS NULL"
    [] :: SqlPersistT IO [Single Text]
  unresolvedPreferences <- rawSql
    "SELECT COUNT(*) FROM user_locale_preferences WHERE locale_id IS NULL OR currency_id IS NULL"
    [] :: SqlPersistT IO [Single Int]
  let enabledLocales = sort [code | Single code <- localeRows]
      enabledCurrencies = sort [code | Single code <- currencyRows]
  unless (enabledLocales == sort (supportedLocales cfg)) $
    liftIO . ioError . userError $ "Persisted deployment locales do not match configured references: " <> show enabledLocales
  unless (enabledCurrencies == sort (supportedCurrencies cfg)) $
    liftIO . ioError . userError $ "Persisted deployment currencies do not match configured references: " <> show enabledCurrencies
  unless ([code | Single code <- localeDefaultRows] == [defaultLocale cfg]) $
    liftIO $ ioError (userError "Persisted deployment locale default does not match DEFAULT_LOCALE")
  unless ([code | Single code <- currencyDefaultRows] == [defaultCurrency cfg]) $
    liftIO $ ioError (userError "Persisted deployment currency default does not match DEFAULT_CURRENCY")
  case unresolvedPreferences of
    [Single 0] -> pure ()
    [Single countValue] -> liftIO . ioError . userError $
      "User locale preferences require canonical localeId and currencyId before cutover: " <> show countValue
    _ -> liftIO $ ioError (userError "Unable to validate canonical user locale preferences")

ensureInternationalColumns :: SqlPersistT IO ()
ensureInternationalColumns = do
  rawExecute "ALTER TABLE party ADD COLUMN IF NOT EXISTS country_code TEXT" []
  rawExecute "ALTER TABLE artist_profile ADD COLUMN IF NOT EXISTS country_code TEXT" []
  rawExecute "ALTER TABLE social_artist_profile ADD COLUMN IF NOT EXISTS country_code TEXT" []
  rawExecute "ALTER TABLE venue ADD COLUMN IF NOT EXISTS country_code TEXT" []
  rawExecute "ALTER TABLE venue ADD COLUMN IF NOT EXISTS timezone TEXT" []
  rawExecute "ALTER TABLE social_event ADD COLUMN IF NOT EXISTS timezone TEXT" []

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
  hasStatus <- columnExists "status"
  when hasStatus $ rawExecute "ALTER TABLE party DROP COLUMN status" []
  hasUpdatedAt <- columnExists "updated_at"
  when hasUpdatedAt $ rawExecute "ALTER TABLE party DROP COLUMN updated_at" []

restoreLegacyPartyRoles :: [(PartyId, RoleEnum)] -> SqlPersistT IO ()
restoreLegacyPartyRoles [] = pure ()
restoreLegacyPartyRoles roles = do
  now <- liftIO getCurrentTime
  forM_ roles $ \(pid, role) -> do
    result <- ensureBootstrapSecurityRole pid role now
    case result of
      Left message -> liftIO . ioError . userError $ T.unpack message
      Right () -> pure ()

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
