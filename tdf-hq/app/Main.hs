{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Wai.Handler.Warp as Warp
import           Control.Concurrent      (threadDelay)
import           Control.Exception       (SomeException, try)
import           Control.Monad            (forM_, when)
import qualified Data.ByteString.Char8    as BS
import           Data.Int                (Int64)
import           Data.Maybe               (mapMaybe)
import           Data.Text                (Text)
import qualified Data.Text               as T
import           Database.Persist         ( (=.), upsert )
import           Database.Persist.Sql     (SqlPersistT, Single(..), rawExecute, rawSql, runMigration,
                                           runSqlPool, toSqlKey)
import           Database.Persist.Types   (PersistValue (PersistText))
import           System.IO                (hSetEncoding, stdout, stderr)
import           GHC.IO.Encoding          (utf8, setLocaleEncoding)
import           Text.Read                (readMaybe)

import           TDF.Cors                 (corsPolicy)

import           TDF.Config     (appPort, dbConnString, loadConfig, resetDb, runMigrations, seedDatabase)
import           TDF.Cron       (startCoursePaymentReminderJob)
import           TDF.DB         (Env(..), ConnectionPool, makePool)
import qualified TDF.DB         as DB
import           TDF.Models     (EntityField (PartyRoleActive), PartyId, PartyRole(..), RoleEnum, migrateAll)
import           TDF.ModelsExtra (migrateExtra)
import           TDF.Trials.Models (migrateTrials)
import           TDF.Server     (mkApp)
import           TDF.Seed       (seedAll)
main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  cfg  <- loadConfig
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
      runSqlPool runAllMigrations pool
    else
      putStrLn "RUN_MIGRATIONS disabled (using pre-initialized schema)."
  when (seedDatabase cfg) $ do
    putStrLn "Seeding initial data..."
    runSqlPool seedAll pool
  putStrLn ("Starting server on port " <> show (appPort cfg))

  let env = Env{ envPool = pool, envConfig = cfg }
  appCors <- corsPolicy
  let app = mkApp env

  startCoursePaymentReminderJob env
  Warp.run (appPort cfg) (appCors app)

resetSchema :: SqlPersistT IO ()
resetSchema = do
  rawExecute "DROP EXTENSION IF EXISTS pgcrypto" []
  rawExecute "DROP SCHEMA IF EXISTS public CASCADE" []
  rawExecute "CREATE SCHEMA public" []
  rawExecute "GRANT ALL ON SCHEMA public TO CURRENT_USER" []
  rawExecute "GRANT ALL ON SCHEMA public TO public" []

runAllMigrations :: SqlPersistT IO ()
runAllMigrations = do
  rawExecute "CREATE EXTENSION IF NOT EXISTS pgcrypto" []
  renameLegacyPartyRoleConstraint
  legacyRoles <- captureLegacyPartyRoles
  dropLegacyPartyColumns
  runMigration migrateAll
  runMigration migrateExtra
  runMigration migrateTrials
  restoreLegacyPartyRoles legacyRoles

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
          error (show err)
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
