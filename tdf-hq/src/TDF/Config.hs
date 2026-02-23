{-# LANGUAGE OverloadedStrings #-}
module TDF.Config where

import           Control.Applicative ((<|>))
import           Control.Monad      (filterM)
import           Data.Char          (toLower)
import           Data.Maybe         (catMaybes, fromMaybe, listToMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           System.Directory  (doesDirectoryExist)
import           System.Environment (lookupEnv)
import           Text.Read          (readMaybe)

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
  , assetsBaseUrl   :: Maybe Text
  , assetsRootDir   :: FilePath
  , courseDefaultSlug :: Text
  , courseDefaultMapUrl :: Maybe Text
  , courseDefaultInstructorAvatar :: Maybe Text
  , openAiApiKey    :: Maybe Text
  , openAiModel     :: Text
  , openAiEmbedModel :: Text
  , chatKitWorkflowId :: Maybe Text
  , chatKitApiBase :: Text
  , ragTopK         :: Int
  , ragChunkWords   :: Int
  , ragChunkOverlap :: Int
  , ragAvailabilityDays :: Int
  , ragAvailabilityPerResource :: Int
  , ragRefreshHours :: Int
  , ragEmbedBatchSize :: Int
  , emailConfig     :: Maybe EmailConfig
  , googleClientId  :: Maybe Text
  , facebookAppId   :: Maybe Text
  , facebookAppSecret :: Maybe Text
  , facebookGraphBase :: Text
  , facebookMessagingToken :: Maybe Text
  , facebookMessagingPageId :: Maybe Text
  , facebookMessagingApiBase :: Text
  , instagramAppToken :: Maybe Text
  , instagramGraphBase :: Text
  , instagramMessagingToken :: Maybe Text
  , instagramMessagingAccountId :: Maybe Text
  , instagramMessagingApiBase :: Text
  , instagramVerifyToken :: Maybe Text
  } deriving (Show)

openAiEmbedDimensions :: Text -> Maybe Int
openAiEmbedDimensions model =
  case T.toLower (T.strip model) of
    "text-embedding-3-small" -> Just 1536
    "text-embedding-3-large" -> Just 3072
    "text-embedding-ada-002" -> Just 1536
    _ -> Nothing

ragEmbeddingDim :: AppConfig -> Int
ragEmbeddingDim cfg = fromMaybe 1536 (openAiEmbedDimensions (openAiEmbedModel cfg))

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
  sdb        <- get "SEED_DB" "false"
  mig        <- get "RUN_MIGRATIONS" "true"
  seedEnv    <- lookupEnv "SEED_TRIGGER_TOKEN"
  baseUrlEnv <- lookupEnv "HQ_APP_URL"
  assetsBaseEnv <- lookupEnv "HQ_ASSETS_BASE_URL"
  assetsDirEnv <- lookupEnv "HQ_ASSETS_DIR"
  googleClientIdEnv <- lookupEnv "GOOGLE_CLIENT_ID"
  fbAppIdEnv <- lookupEnv "FACEBOOK_APP_ID" <|> lookupEnv "META_APP_ID"
  fbAppSecretEnv <- lookupEnv "FACEBOOK_APP_SECRET" <|> lookupEnv "META_APP_SECRET"
  fbGraphBaseEnv <- lookupEnv "FACEBOOK_GRAPH_BASE"
  fbMsgTokenEnv <- lookupEnv "FACEBOOK_MESSAGING_TOKEN" <|> lookupEnv "FACEBOOK_PAGE_ACCESS_TOKEN"
  fbMsgPageIdEnv <- lookupEnv "FACEBOOK_MESSAGING_PAGE_ID" <|> lookupEnv "FACEBOOK_PAGE_ID"
  fbMsgBaseEnv <- lookupEnv "FACEBOOK_MESSAGING_API_BASE"
  courseSlugEnv <- lookupEnv "COURSE_DEFAULT_SLUG"
  courseMapEnv <- lookupEnv "COURSE_DEFAULT_MAP_URL"
  courseInstructorAvatarEnv <- lookupEnv "COURSE_DEFAULT_INSTRUCTOR_AVATAR"
  openAiKeyEnv <- lookupEnv "OPENAI_API_KEY"
  openAiModelEnv <- lookupEnv "OPENAI_MODEL"
  openAiEmbedModelEnv <- lookupEnv "OPENAI_EMBED_MODEL"
  chatKitWorkflowEnv <- lookupEnv "CHATKIT_WORKFLOW_ID" <|> lookupEnv "VITE_CHATKIT_WORKFLOW_ID"
  chatKitApiBaseEnv <- lookupEnv "CHATKIT_API_BASE"
  ragTopKEnv <- lookupEnv "RAG_TOP_K"
  ragChunkWordsEnv <- lookupEnv "RAG_CHUNK_WORDS"
  ragChunkOverlapEnv <- lookupEnv "RAG_CHUNK_OVERLAP"
  ragAvailabilityDaysEnv <- lookupEnv "RAG_AVAILABILITY_DAYS"
  ragAvailabilityPerResourceEnv <- lookupEnv "RAG_AVAILABILITY_PER_RESOURCE"
  ragRefreshHoursEnv <- lookupEnv "RAG_REFRESH_HOURS"
  ragEmbedBatchSizeEnv <- lookupEnv "RAG_EMBED_BATCH_SIZE"
  smtpHostEnv <- lookupEnv "SMTP_HOST"
  smtpPortEnv <- lookupEnv "SMTP_PORT"
  smtpUserEnv <- lookupEnv "SMTP_USERNAME" <|> lookupEnv "SMTP_USER"
  smtpPassEnv <- lookupEnv "SMTP_PASSWORD" <|> lookupEnv "SMTP_PASS"
  smtpFromEnv <- lookupEnv "SMTP_FROM"
  smtpFromNameEnv <- lookupEnv "SMTP_FROM_NAME"
  smtpTlsEnv  <- lookupEnv "SMTP_TLS"
  igTokenEnv <- lookupEnv "INSTAGRAM_APP_TOKEN"
  igBaseEnv <- lookupEnv "INSTAGRAM_GRAPH_BASE"
  igMsgTokenEnv <- lookupEnv "INSTAGRAM_MESSAGING_TOKEN"
  igMsgAccountEnv <- lookupEnv "INSTAGRAM_MESSAGING_ACCOUNT_ID"
  igMsgBaseEnv <- lookupEnv "INSTAGRAM_MESSAGING_API_BASE"
  igVerifyEnv <- lookupEnv "INSTAGRAM_VERIFY_TOKEN" <|> lookupEnv "IG_VERIFY_TOKEN"
  assetsRoot <- resolveAssetsRootDir (assetsDirEnv >>= nonEmptyPath)
  let fbGraphBase = fromMaybe "https://graph.facebook.com/v20.0" (fbGraphBaseEnv >>= nonEmpty . T.pack)
      igGraphBase = maybe "https://graph.instagram.com" (T.strip . T.pack) igBaseEnv
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
    , assetsBaseUrl = fmap (T.strip . T.pack) assetsBaseEnv
    , assetsRootDir = assetsRoot
    , courseDefaultSlug = maybe "produccion-musical-feb-2026" (T.strip . T.pack) courseSlugEnv
    , courseDefaultMapUrl = fmap (T.strip . T.pack) courseMapEnv
    , courseDefaultInstructorAvatar = fmap (T.strip . T.pack) courseInstructorAvatarEnv
    , openAiApiKey = openAiKeyEnv >>= nonEmpty . T.pack
    , openAiModel = fromMaybe "gpt-4.1-mini" (openAiModelEnv >>= nonEmpty . T.pack)
    , openAiEmbedModel = fromMaybe "text-embedding-3-small" (openAiEmbedModelEnv >>= nonEmpty . T.pack)
    , chatKitWorkflowId = chatKitWorkflowEnv >>= nonEmpty . T.pack
    , chatKitApiBase = fromMaybe "https://api.openai.com" (chatKitApiBaseEnv >>= nonEmpty . T.pack)
    , ragTopK = parseInt 8 ragTopKEnv
    , ragChunkWords = parseInt 220 ragChunkWordsEnv
    , ragChunkOverlap = parseInt 40 ragChunkOverlapEnv
    , ragAvailabilityDays = parseInt 14 ragAvailabilityDaysEnv
    , ragAvailabilityPerResource = parseInt 6 ragAvailabilityPerResourceEnv
    , ragRefreshHours = parseInt 24 ragRefreshHoursEnv
    , ragEmbedBatchSize = parseInt 64 ragEmbedBatchSizeEnv
    , emailConfig = mkEmailConfig smtpHostEnv smtpUserEnv smtpPassEnv smtpFromEnv smtpFromNameEnv smtpPortEnv smtpTlsEnv
    , googleClientId = fmap (T.strip . T.pack) googleClientIdEnv
    , facebookAppId = fbAppIdEnv >>= nonEmpty . T.pack
    , facebookAppSecret = fbAppSecretEnv >>= nonEmpty . T.pack
    , facebookGraphBase = fbGraphBase
    , facebookMessagingToken = fmap (T.strip . T.pack) fbMsgTokenEnv >>= nonEmpty
    , facebookMessagingPageId = fmap (T.strip . T.pack) fbMsgPageIdEnv >>= nonEmpty
    , facebookMessagingApiBase = maybe fbGraphBase (T.strip . T.pack) fbMsgBaseEnv
    , instagramAppToken = fmap (T.strip . T.pack) igTokenEnv
    , instagramGraphBase = igGraphBase
    , instagramMessagingToken =
        case fmap (T.strip . T.pack) igMsgTokenEnv of
          Just val | not (T.null val) -> Just val
          _ -> fmap (T.strip . T.pack) igTokenEnv
    , instagramMessagingAccountId = fmap (T.strip . T.pack) igMsgAccountEnv
    , instagramMessagingApiBase = maybe "https://graph.facebook.com/v20.0" (T.strip . T.pack) igMsgBaseEnv
    , instagramVerifyToken = fmap (T.strip . T.pack) igVerifyEnv >>= nonEmpty
    }
  where
    get k def = fmap (fromMaybe def) (lookupEnv k)
    asBool v = case fmap toLower v of
      "true"  -> True
      "1"     -> True
      "yes"   -> True
      "on"    -> True
      _       -> False
    parseInt def mVal =
      case mVal >>= readMaybe of
        Just n | n > 0 -> n
        _ -> def
    mkSeedToken mVal =
      case fmap (T.strip . T.pack) mVal of
        Nothing  -> Nothing
        Just txt | T.null txt -> Nothing
                 | otherwise -> Just txt
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

resolveAssetsRootDir :: Maybe FilePath -> IO FilePath
resolveAssetsRootDir mEnv = do
  let candidates = catMaybes [mEnv] ++ ["/app/assets", "tdf-hq/assets", "assets"]
  existing <- filterM doesDirectoryExist candidates
  pure $ fromMaybe (fromMaybe "assets" mEnv) (listToMaybe existing)

defaultAppBase :: Text
defaultAppBase = "https://tdf-app.pages.dev"
defaultAssetsBase :: Text
defaultAssetsBase = "https://tdf-hq.fly.dev/assets/serve"

sanitizeBaseUrl :: Text -> Text
sanitizeBaseUrl base =
  let trimmed = T.dropWhileEnd (== '/') (T.strip base)
  in if T.null trimmed then defaultAppBase else trimmed

resolveAppBase :: Maybe Text -> Text
resolveAppBase = sanitizeBaseUrl . fromMaybe defaultAppBase

resolveConfiguredAppBase :: AppConfig -> Text
resolveConfiguredAppBase cfg = resolveAppBase (appBaseUrl cfg)
resolveConfiguredAssetsBase :: AppConfig -> Text
resolveConfiguredAssetsBase cfg = sanitizeBaseUrl (fromMaybe defaultAssetsBase (assetsBaseUrl cfg))

courseSlugFallback :: AppConfig -> Text
courseSlugFallback cfg =
  let val = T.strip (courseDefaultSlug cfg)
  in if T.null val then "produccion-musical-feb-2026" else val

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

nonEmptyPath :: String -> Maybe FilePath
nonEmptyPath = fmap T.unpack . nonEmpty . T.pack
