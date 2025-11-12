{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network.Wai.Handler.Warp as Warp
import           Control.Monad            (forM_, when)
import           Data.ByteString.Char8    (pack)
import           Data.Char                (isSpace, toLower)
import           Data.Int                (Int64)
import           Data.List               (dropWhileEnd)
import           Data.Maybe               (mapMaybe)
import           Data.Text                (Text)
import qualified Data.Text               as T
import           Database.Persist         ( (=.), upsert )
import           Database.Persist.Sql     (SqlPersistT, Single(..), rawExecute, rawSql, runMigration,
                                           runSqlPool, toSqlKey)
import           Database.Persist.Types   (PersistValue (PersistText))
import           System.Environment       (lookupEnv)
import           Text.Read                (readMaybe)

import           Network.Wai.Middleware.Cors
                 ( cors
                 , simpleCorsResourcePolicy
                 , CorsResourcePolicy(..)
                 , simpleHeaders
                 )

import           TDF.Config     (appPort, dbConnString, loadConfig, resetDb, seedDatabase)
import           TDF.DB         (Env(..), makePool)
import           TDF.Models     (EntityField (PartyRoleActive), PartyId, PartyRole(..), RoleEnum, migrateAll)
import           TDF.ModelsExtra (migrateExtra)
import           TDF.Trials.Models (migrateTrials)
import           TDF.Server     (mkApp)
import           TDF.Seed       (seedAll)
main :: IO ()
main = do
  cfg  <- loadConfig
  pool <- makePool (pack (dbConnString cfg))
  if resetDb cfg
    then do
      putStrLn "Resetting DB schema..."
      runSqlPool resetSchema pool
    else
      putStrLn "RESET_DB disabled, preserving existing schema."
  putStrLn "Running DB migrations..."
  runSqlPool runMigrations pool
  when (seedDatabase cfg) $ do
    putStrLn "Seeding initial data..."
    runSqlPool seedAll pool
  putStrLn ("Starting server on port " <> show (appPort cfg))

  let allowedOriginsBase =
        [ "http://localhost:5173"
        , "http://127.0.0.1:5173"
        , "http://localhost:4173"
        , "http://127.0.0.1:4173"
        , "http://localhost:3000"
        , "http://127.0.0.1:3000"
        , "http://localhost:5174"
        , "http://127.0.0.1:5174"
        , "https://tdf-ui.onrender.com"
        , "https://tdf-7t2qa.onrender.com"
        , "https://tdfui.pages.dev"
        ]
  listEnvs <- mapM lookupEnv ["ALLOW_ORIGINS", "ALLOWED_ORIGINS", "CORS_ALLOW_ORIGINS"]
  singleEnvs <- mapM lookupEnv ["ALLOW_ORIGIN", "ALLOWED_ORIGIN", "CORS_ALLOW_ORIGIN"]
  allowAllFlag <- anyTrue ["ALLOW_ALL_ORIGINS", "ALLOWED_ORIGINS_ALL", "CORS_ALLOW_ALL"]
  let fromListEnv =
        concatMap (maybe [] (map pack . splitComma)) listEnvs
      fromOneEnv =
        concatMap (maybe [] (\origin -> [pack origin])) singleEnvs
      allowedOrigins = allowedOriginsBase <> fromListEnv <> fromOneEnv
      allowedHeaders =
        "Authorization"
        : "Content-Type"
        : "X-Requested-With"
        : simpleHeaders
      corsPolicy =
        simpleCorsResourcePolicy
          { corsRequestHeaders = allowedHeaders
          , corsMethods        = ["GET","POST","PUT","PATCH","DELETE","OPTIONS"]
          , corsOrigins        = if allowAllFlag then Nothing else Just (allowedOrigins, True)
          }
      app = mkApp Env{ envPool = pool, envConfig = cfg }

  Warp.run (appPort cfg) (cors (const $ Just corsPolicy) app)

-- | Split a comma-separated list into trimmed entries.
splitComma :: String -> [String]
splitComma = go . dropWhile isSpace
  where
    go [] = []
    go s =
      let (h, t) = break (== ',') s
          h'     = trim h
      in if null t
           then [h']
           else h' : go (drop 1 t)
    trim = dropWhileEnd isSpace . dropWhile isSpace

anyTrue :: [String] -> IO Bool
anyTrue names = do
  vals <- mapM lookupEnv names
  pure (any (maybe False parseBool) vals)

parseBool :: String -> Bool
parseBool val =
  case map toLower (dropWhile isSpace val) of
    ""      -> False
    "true"  -> True
    "1"     -> True
    "yes"   -> True
    "on"    -> True
    _       -> False

resetSchema :: SqlPersistT IO ()
resetSchema = do
  rawExecute "DROP EXTENSION IF EXISTS pgcrypto" []
  rawExecute "DROP SCHEMA IF EXISTS public CASCADE" []
  rawExecute "CREATE SCHEMA public" []
  rawExecute "GRANT ALL ON SCHEMA public TO CURRENT_USER" []
  rawExecute "GRANT ALL ON SCHEMA public TO public" []

runMigrations :: SqlPersistT IO ()
runMigrations = do
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
