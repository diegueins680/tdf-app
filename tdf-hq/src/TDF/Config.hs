{-# LANGUAGE OverloadedStrings #-}
module TDF.Config where

import           Data.Char          (toLower)
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           System.Environment (lookupEnv)

data AppConfig = AppConfig
  { dbHost       :: String
  , dbPort       :: String
  , dbUser       :: String
  , dbPass       :: String
  , dbName       :: String
  , appPort      :: Int
  , resetDb      :: Bool
  , seedDatabase :: Bool
  , seedTriggerToken :: Maybe Text
  } deriving (Show)

dbConnString :: AppConfig -> String
dbConnString cfg =
  "host="    <> dbHost cfg    <>
  " port="   <> dbPort cfg    <>
  " user="   <> dbUser cfg    <>
  " password=" <> dbPass cfg  <>
  " dbname=" <> dbName cfg    -- no 'pool=' here; pooling is managed by createPostgresqlPool

loadConfig :: IO AppConfig
loadConfig = do
  h       <- get "DB_HOST" "127.0.0.1"
  p       <- get "DB_PORT" "5432"
  u       <- get "DB_USER" "postgres"
  w       <- get "DB_PASS" "postgres"
  d       <- get "DB_NAME" "tdf_hq"
  ap      <- get "APP_PORT" "8080"
  rdb     <- get "RESET_DB" "false"
  sdb     <- get "SEED_DB" "true"
  seedEnv <- lookupEnv "SEED_TRIGGER_TOKEN"
  pure AppConfig
    { dbHost = h
    , dbPort = p
    , dbUser = u
    , dbPass = w
    , dbName = d
    , appPort = read ap
    , resetDb = asBool rdb
    , seedDatabase = asBool sdb
    , seedTriggerToken = mkSeedToken seedEnv
    }
  where
    get k def = fmap (fromMaybe def) (lookupEnv k)
    asBool v = case fmap toLower v of
      "true"  -> True
      "1"     -> True
      "yes"   -> True
      "on"    -> True
      _       -> False
    mkSeedToken mVal =
      case fmap (T.strip . T.pack) mVal of
        Nothing -> Just defaultSeed
        Just txt | T.null txt -> Nothing
                 | otherwise -> Just txt
    defaultSeed = T.pack "tdf-bootstrap-seed"
