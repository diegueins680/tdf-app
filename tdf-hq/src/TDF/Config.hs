{-# LANGUAGE OverloadedStrings #-}
module TDF.Config where

import           Data.Char          (toLower)
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           System.Environment (lookupEnv)

data EmailConfig = EmailConfig
  { emailFromName    :: Text
  , emailFromAddress :: Text
  , smtpHost         :: Text
  , smtpPort         :: Int
  , smtpUsername     :: Text
  , smtpPassword     :: Text
  , smtpUseTLS       :: Bool
  } deriving (Show)

data AppConfig = AppConfig
  { dbHost          :: String
  , dbPort          :: String
  , dbUser          :: String
  , dbPass          :: String
  , dbName          :: String
  , appPort         :: Int
  , resetDb         :: Bool
  , seedDatabase    :: Bool
  , seedTriggerToken :: Maybe Text
  , appBaseUrl      :: Maybe Text
  , emailConfig     :: Maybe EmailConfig
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
  h          <- get "DB_HOST" "127.0.0.1"
  p          <- get "DB_PORT" "5432"
  u          <- get "DB_USER" "postgres"
  w          <- get "DB_PASS" "postgres"
  d          <- get "DB_NAME" "tdf_hq"
  ap         <- get "APP_PORT" "8080"
  rdb        <- get "RESET_DB" "false"
  sdb        <- get "SEED_DB" "true"
  seedEnv    <- lookupEnv "SEED_TRIGGER_TOKEN"
  baseUrlEnv <- lookupEnv "HQ_APP_URL"
  smtpHostEnv <- lookupEnv "SMTP_HOST"
  smtpPortEnv <- lookupEnv "SMTP_PORT"
  smtpUserEnv <- lookupEnv "SMTP_USERNAME"
  smtpPassEnv <- lookupEnv "SMTP_PASSWORD"
  smtpFromEnv <- lookupEnv "SMTP_FROM"
  smtpFromNameEnv <- lookupEnv "SMTP_FROM_NAME"
  smtpTlsEnv  <- lookupEnv "SMTP_TLS"
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
    , appBaseUrl = fmap (T.strip . T.pack) baseUrlEnv
    , emailConfig = mkEmailConfig smtpHostEnv smtpUserEnv smtpPassEnv smtpFromEnv smtpFromNameEnv smtpPortEnv smtpTlsEnv
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
    mkEmailConfig mHost mUser mPass mFrom mFromName mPort mTls = do
      host <- fmap (T.strip . T.pack) mHost
      user <- fmap (T.strip . T.pack) mUser
      pass <- fmap (T.strip . T.pack) mPass
      addr <- fmap (T.strip . T.pack) mFrom
      let name = maybe "TDF Records" (T.strip . T.pack) mFromName
          portVal = maybe 587 read mPort
          useTls = maybe True asBool mTls
      pure EmailConfig
        { emailFromName = if T.null name then "TDF Records" else name
        , emailFromAddress = addr
        , smtpHost = host
        , smtpPort = portVal
        , smtpUsername = user
        , smtpPassword = pass
        , smtpUseTLS = useTls
        }
