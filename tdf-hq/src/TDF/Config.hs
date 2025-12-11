{-# LANGUAGE OverloadedStrings #-}
module TDF.Config where

import           Control.Applicative ((<|>))
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
  , runMigrations   :: Bool
  , seedTriggerToken :: Maybe Text
  , appBaseUrl      :: Maybe Text
  , courseDefaultSlug :: Text
  , courseDefaultMapUrl :: Maybe Text
  , courseDefaultInstructorAvatar :: Maybe Text
  , emailConfig     :: Maybe EmailConfig
  , googleClientId  :: Maybe Text
  , instagramAppToken :: Maybe Text
  , instagramGraphBase :: Text
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
  mig        <- get "RUN_MIGRATIONS" "true"
  seedEnv    <- lookupEnv "SEED_TRIGGER_TOKEN"
  baseUrlEnv <- lookupEnv "HQ_APP_URL"
  googleClientIdEnv <- lookupEnv "GOOGLE_CLIENT_ID"
  courseSlugEnv <- lookupEnv "COURSE_DEFAULT_SLUG"
  courseMapEnv <- lookupEnv "COURSE_DEFAULT_MAP_URL"
  courseInstructorAvatarEnv <- lookupEnv "COURSE_DEFAULT_INSTRUCTOR_AVATAR"
  smtpHostEnv <- lookupEnv "SMTP_HOST"
  smtpPortEnv <- lookupEnv "SMTP_PORT"
  smtpUserEnv <- lookupEnv "SMTP_USERNAME" <|> lookupEnv "SMTP_USER"
  smtpPassEnv <- lookupEnv "SMTP_PASSWORD" <|> lookupEnv "SMTP_PASS"
  smtpFromEnv <- lookupEnv "SMTP_FROM"
  smtpFromNameEnv <- lookupEnv "SMTP_FROM_NAME"
  smtpTlsEnv  <- lookupEnv "SMTP_TLS"
  igTokenEnv <- lookupEnv "INSTAGRAM_APP_TOKEN"
  igBaseEnv <- lookupEnv "INSTAGRAM_GRAPH_BASE"
  pure AppConfig
    { dbHost = h
    , dbPort = p
    , dbUser = u
    , dbPass = w
    , dbName = d
    , appPort = read ap
    , resetDb = asBool rdb
    , seedDatabase = asBool sdb
    , runMigrations = asBool mig
    , seedTriggerToken = mkSeedToken seedEnv
    , appBaseUrl = fmap (T.strip . T.pack) baseUrlEnv
    , courseDefaultSlug = maybe "produccion-musical-dic-2025" (T.strip . T.pack) courseSlugEnv
    , courseDefaultMapUrl = fmap (T.strip . T.pack) courseMapEnv
    , courseDefaultInstructorAvatar = fmap (T.strip . T.pack) courseInstructorAvatarEnv
    , emailConfig = mkEmailConfig smtpHostEnv smtpUserEnv smtpPassEnv smtpFromEnv smtpFromNameEnv smtpPortEnv smtpTlsEnv
    , googleClientId = fmap (T.strip . T.pack) googleClientIdEnv
    , instagramAppToken = fmap (T.strip . T.pack) igTokenEnv
    , instagramGraphBase = maybe "https://graph.instagram.com" (T.strip . T.pack) igBaseEnv
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

defaultAppBase :: Text
defaultAppBase = "http://localhost:5173"

sanitizeBaseUrl :: Text -> Text
sanitizeBaseUrl base =
  let trimmed = T.dropWhileEnd (== '/') (T.strip base)
  in if T.null trimmed then defaultAppBase else trimmed

resolveAppBase :: Maybe Text -> Text
resolveAppBase = sanitizeBaseUrl . fromMaybe defaultAppBase

resolveConfiguredAppBase :: AppConfig -> Text
resolveConfiguredAppBase cfg = resolveAppBase (appBaseUrl cfg)

courseSlugFallback :: AppConfig -> Text
courseSlugFallback cfg =
  let val = T.strip (courseDefaultSlug cfg)
  in if T.null val then "produccion-musical-dic-2025" else val

courseMapFallback :: AppConfig -> Text
courseMapFallback cfg = fromMaybe "https://maps.app.goo.gl/6pVYZ2CsbvQfGhAz6" (courseDefaultMapUrl cfg >>= nonEmpty)

courseInstructorAvatarFallback :: AppConfig -> Text
courseInstructorAvatarFallback cfg =
  let base = resolveConfiguredAppBase cfg
  in fromMaybe (base <> "/assets/esteban-munoz.jpg") (courseDefaultInstructorAvatar cfg >>= nonEmpty)

nonEmpty :: Text -> Maybe Text
nonEmpty txt =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just trimmed
