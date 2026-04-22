{-# LANGUAGE OverloadedStrings #-}
module TDF.Config where

import           Control.Applicative ((<|>))
import           Control.Monad      ((>=>), filterM, when)
import           Data.Char          (isControl, isDigit, isSpace, toLower)
import           Data.List          (isPrefixOf)
import           Data.Maybe         (fromMaybe, isNothing, listToMaybe)
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
  , dbConnUrl       :: Maybe String
  , dbSslMode       :: Maybe String
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
  , sessionCookieName :: Text
  , sessionCookieDomain :: Maybe Text
  , sessionCookiePath :: Text
  , sessionCookieSecure :: Bool
  , sessionCookieSameSite :: Text
  , sessionCookieMaxAgeSeconds :: Maybe Int
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
  ensureReadWriteTargetSession (fromMaybe keywordStyle (dbConnUrl cfg))
  where
    keywordStyle =
      appendKeywordOption "sslmode" (dbSslMode cfg) $
        "host="    <> dbHost cfg    <>
        " port="   <> dbPort cfg    <>
        " user="   <> dbUser cfg    <>
        " password=" <> dbPass cfg  <>
        " dbname=" <> dbName cfg    -- no 'pool=' here; pooling is managed by createPostgresqlPool

appendKeywordOption :: String -> Maybe String -> String -> String
appendKeywordOption option mValue rawConn =
  case mValue >>= normalizeConnOption of
    Just value -> rawConn <> " " <> option <> "=" <> value
    Nothing -> rawConn

normalizeConnOption :: String -> Maybe String
normalizeConnOption raw =
  let trimmed = T.unpack (T.strip (T.pack raw))
  in if null trimmed then Nothing else Just trimmed

validateFallbackConnUrl :: String -> String -> Either String String
validateFallbackConnUrl envName raw
  | hasScheme "postgresql://" =
      validateAuthority "postgresql://"
  | hasScheme "postgres://" =
      validateAuthority "postgres://"
  | "://" `T.isInfixOf` value =
      Left (envName <> " must use postgres:// or postgresql://")
  | otherwise =
      Right raw
  where
    value = T.strip (T.pack raw)
    lowerValue = T.toLower value
    hasScheme :: String -> Bool
    hasScheme scheme = T.pack scheme `T.isPrefixOf` lowerValue
    validateAuthority :: String -> Either String String
    validateAuthority scheme
      | T.any isSpace value =
          Left (envName <> " must not contain whitespace")
      | "#" `T.isInfixOf` value =
          Left (envName <> " must not include a fragment")
      | otherwise =
          let remainder = T.drop (length scheme) value
              authority = T.takeWhile (`notElem` ("/?#" :: String)) remainder
              databasePath = T.drop (T.length authority) remainder
              atCount = T.count "@" authority
              hostPort =
                case reverse (T.splitOn "@" authority) of
                  [] -> ""
                  h:_ -> h
          in if T.null authority
               then Left (envName <> " must include a PostgreSQL host")
               else if atCount > 1
                 then Left (envName <> " must not contain multiple @ separators")
               else
                 validateConnectionHostPort hostPort
                   *> validateConnectionDatabasePath databasePath
                   *> validateConnectionQueryParams databasePath
                   *> Right raw

    validateConnectionHostPort :: Text -> Either String ()
    validateConnectionHostPort hostPort
      | "[" `T.isPrefixOf` hostPort =
          let host = T.takeWhile (/= ']') (T.drop 1 hostPort)
              suffix = T.drop (T.length host + 1) hostPort
          in if T.null host || not ("]" `T.isPrefixOf` suffix)
               then Left (envName <> " must include a PostgreSQL host")
               else if not (isValidBracketedConnectionHost host)
                 then Left (envName <> " must include a valid PostgreSQL host")
               else validateConnectionPortSuffix (T.drop 1 suffix)
      | otherwise =
          let (host, suffix) = T.breakOn ":" hostPort
          in if T.null host
               then Left (envName <> " must include a PostgreSQL host")
               else validateConnectionPortSuffix suffix

    isValidBracketedConnectionHost :: Text -> Bool
    isValidBracketedConnectionHost host =
      T.any (== ':') host
        && not (":::" `T.isInfixOf` host)
        && T.all (`elem` ("0123456789abcdefABCDEF:." :: String)) host

    validateConnectionPortSuffix :: Text -> Either String ()
    validateConnectionPortSuffix suffix
      | T.null suffix = Right ()
      | ":" `T.isPrefixOf` suffix =
          let port = T.drop 1 suffix
          in if T.null port || T.any (not . isDigit) port
               then Left (envName <> " port must be numeric")
               else case readMaybe (T.unpack port) :: Maybe Int of
                 Just portNumber | portNumber >= 1 && portNumber <= 65535 -> Right ()
                 _ -> Left (envName <> " port must be between 1 and 65535")
      | otherwise = Left (envName <> " port must be numeric")

    validateConnectionDatabasePath :: Text -> Either String ()
    validateConnectionDatabasePath path
      | not ("/" `T.isPrefixOf` path) =
          Left (envName <> " must include a database name")
      | otherwise =
          let databaseName = T.takeWhile (`notElem` ("?#" :: String)) (T.drop 1 path)
          in if T.null databaseName || "/" `T.isInfixOf` databaseName
               then Left (envName <> " must include a database name")
               else Right ()

    validateConnectionQueryParams :: Text -> Either String ()
    validateConnectionQueryParams path =
      case T.dropWhile (/= '?') path of
        "" -> Right ()
        queryWithMarker ->
          let query = T.drop 1 queryWithMarker
              queryParamNames =
                [ T.strip key
                | pair <- T.splitOn "&" query
                , let (key, _) = T.breakOn "=" pair
                ]
              targetSessionAttrs =
                [ T.strip (T.drop 1 rest)
                | pair <- T.splitOn "&" query
                , let (key, rest) = T.breakOn "=" pair
                , T.toLower key == "target_session_attrs"
                ]
              sslModes =
                [ T.strip (T.drop 1 rest)
                | pair <- T.splitOn "&" query
                , let (key, rest) = T.breakOn "=" pair
                , T.toLower key == "sslmode"
                ]
              rejectDuplicate name values =
                if length values > 1
                  then Left (envName <> " " <> name <> " must be provided at most once")
                  else Right ()
              validateRecognizedParams =
                if T.null query || any T.null queryParamNames
                  then Left (envName <> " query parameters must include names")
                  else if any T.null targetSessionAttrs
                    then Left (envName <> " target_session_attrs must not be blank")
                  else if any (/= "read-write") (map (T.toLower . T.strip) targetSessionAttrs)
                    then Left (envName <> " target_session_attrs must be read-write")
                  else if any T.null sslModes
                    then Left (envName <> " sslmode must not be blank")
                  else if any (not . isValidConnectionSslMode) sslModes
                    then Left (invalidConnectionSslModeMessage envName)
                  else Right ()
          in rejectDuplicate "target_session_attrs" targetSessionAttrs
               *> rejectDuplicate "sslmode" sslModes
               *> validateRecognizedParams

isValidConnectionSslMode :: Text -> Bool
isValidConnectionSslMode rawMode =
  T.toLower (T.strip rawMode)
    `elem` [ "disable"
           , "allow"
           , "prefer"
           , "require"
           , "verify-ca"
           , "verify-full"
           ]

invalidConnectionSslModeMessage :: String -> String
invalidConnectionSslModeMessage envName =
  envName <> " sslmode must be one of: disable, allow, prefer, require, verify-ca, verify-full"

validateDbSslMode :: String -> String -> Either String String
validateDbSslMode envName rawMode
  | T.any isSpace sslMode || T.any isControl sslMode =
      Left (envName <> " must be a single sslmode value")
  | isValidConnectionSslMode sslMode =
      Right (T.unpack sslMode)
  | otherwise =
      Left (invalidConnectionSslModeMessage envName)
  where
    sslMode = T.toLower (T.strip (T.pack rawMode))

validateKeywordDbConnField :: Bool -> String -> String -> IO String
validateKeywordDbConnField False _ rawValue = pure rawValue
validateKeywordDbConnField True fieldName rawValue =
  let value = T.pack rawValue
  in if T.null value
       then fail (fieldName <> " must not be empty")
       else if T.any (\ch -> isSpace ch || isControl ch) value
         then fail (fieldName <> " must not contain whitespace or control characters")
         else pure rawValue

validateKeywordDbPort :: Bool -> String -> String -> IO String
validateKeywordDbPort False _ rawPort = pure rawPort
validateKeywordDbPort True fieldName rawPort =
  case readMaybe rawPort :: Maybe Int of
    Just portNumber | portNumber >= 1 && portNumber <= 65535 -> pure rawPort
    _ -> fail (fieldName <> " must be a port number between 1 and 65535")

validatePortEnv :: String -> Int -> Maybe String -> IO Int
validatePortEnv _ defaultValue Nothing = pure defaultValue
validatePortEnv envName defaultValue (Just rawValue)
  | T.null normalized = pure defaultValue
  | otherwise =
      case readMaybe (T.unpack normalized) of
        Just parsed | parsed >= 1 && parsed <= 65535 -> pure parsed
        _ -> fail (envName <> " must be a port number between 1 and 65535")
  where
    normalized = T.strip (T.pack rawValue)

validatePositiveIntEnv :: String -> Int -> Maybe String -> IO Int
validatePositiveIntEnv _ defaultValue Nothing = pure defaultValue
validatePositiveIntEnv envName defaultValue (Just rawValue)
  | T.null normalized = pure defaultValue
  | otherwise =
      case readMaybe (T.unpack normalized) of
        Just parsed | parsed > 0 -> pure parsed
        _ -> fail (envName <> " must be a positive integer")
  where
    normalized = T.strip (T.pack rawValue)

validateNonNegativeIntEnv :: String -> Int -> Maybe String -> IO Int
validateNonNegativeIntEnv _ defaultValue Nothing = pure defaultValue
validateNonNegativeIntEnv envName defaultValue (Just rawValue)
  | T.null normalized = pure defaultValue
  | otherwise =
      case readMaybe (T.unpack normalized) of
        Just parsed | parsed >= 0 -> pure parsed
        _ -> fail (envName <> " must be a non-negative integer")
  where
    normalized = T.strip (T.pack rawValue)

extractConnUrlParam :: String -> String -> Maybe String
extractConnUrlParam rawKey connUrl =
  case dropWhile (/= '?') connUrl of
    [] -> Nothing
    (_:query) -> listToMaybe
      [ value
      | pair <- splitOn '&' query
      , let (key, rest) = break (== '=') pair
      , map toLower key == map toLower rawKey
      , not (null rest)
      , value <- maybeToList (normalizeConnOption (drop 1 rest))
      ]
  where
    splitOn _ [] = []
    splitOn delim input =
      let (chunk, rest) = break (== delim) input
      in chunk : case rest of
        [] -> []
        (_:remaining) -> splitOn delim remaining

    maybeToList Nothing = []
    maybeToList (Just value) = [value]

ensureReadWriteTargetSession :: String -> String
ensureReadWriteTargetSession rawConn
  | isPostgresUrl normalized && hasTargetSessionAttrsUrlParam rawConn = rawConn
  | isPostgresUrl normalized =
      rawConn
        <> if '?' `elem` rawConn
             then "&target_session_attrs=read-write"
             else "?target_session_attrs=read-write"
  | hasTargetSessionAttrsKeyword normalized = rawConn
  | otherwise = rawConn <> " target_session_attrs=read-write"
  where
    normalized = map toLower rawConn
    hasTargetSessionAttrsUrlParam conn =
      case extractConnUrlParam "target_session_attrs" conn of
        Just _ -> True
        Nothing -> False
    hasTargetSessionAttrsKeyword conn =
      any ("target_session_attrs=" `isPrefixOf`) (words conn)
    isPostgresUrl conn =
      "postgresql://" `isPrefixOf` conn || "postgres://" `isPrefixOf` conn

loadConfig :: IO AppConfig
loadConfig = do
  -- Prefer a connection URL unless every keyword-style field is present.
  -- Fly/Koyeb environments can expose partial PG* variables alongside DATABASE_URL.
  keywordDbEnvConfigured <- allEnvPresent
    [ ["DB_HOST", "PGHOST"]
    , ["DB_PORT", "PGPORT"]
    , ["DB_USER", "PGUSER"]
    , ["DB_PASS", "PGPASSWORD"]
    , ["DB_NAME", "PGDATABASE"]
    ]
  fallbackConnUrl <- lookupFirstConnUrlEnv (not keywordDbEnvConfigured)
    ["DATABASE_URL", "DATABASE_PRIVATE_URL", "POSTGRES_URL", "POSTGRES_PRISMA_URL"]
  connUrl    <- if keywordDbEnvConfigured then pure Nothing else pure fallbackConnUrl
  rawHost    <- getWithFallback ["DB_HOST", "PGHOST"] "127.0.0.1"
  rawPort    <- getWithFallback ["DB_PORT", "PGPORT"] "5432"
  rawUser    <- getWithFallback ["DB_USER", "PGUSER"] "postgres"
  rawPass    <- getWithFallback ["DB_PASS", "PGPASSWORD"] "postgres"
  rawName    <- getWithFallback ["DB_NAME", "PGDATABASE"] "tdf_hq"
  let usingKeywordConn = isNothing connUrl
  h          <- validateKeywordDbConnField usingKeywordConn "DB_HOST/PGHOST" rawHost
  p          <- validateKeywordDbPort usingKeywordConn "DB_PORT/PGPORT" rawPort
  u          <- validateKeywordDbConnField usingKeywordConn "DB_USER/PGUSER" rawUser
  w          <- validateKeywordDbConnField usingKeywordConn "DB_PASS/PGPASSWORD" rawPass
  d          <- validateKeywordDbConnField usingKeywordConn "DB_NAME/PGDATABASE" rawName
  sslModeEnvRaw <- lookupFirstNamedEnv ["DB_SSLMODE", "PGSSLMODE"]
  sslModeEnv <-
    case sslModeEnvRaw of
      Nothing -> pure Nothing
      Just (envName, rawMode) ->
        case validateDbSslMode envName rawMode of
          Left msg -> fail msg
          Right mode -> pure (Just mode)
  appPortVal <- lookupEnv "APP_PORT" >>= validatePositiveIntEnv "APP_PORT" 8080
  rdbEnv     <- lookupEnv "RESET_DB"
  sdbEnv     <- lookupEnv "SEED_DB"
  migEnv     <- lookupEnv "RUN_MIGRATIONS"
  seedEnv    <- lookupEnv "SEED_TRIGGER_TOKEN"
  baseUrlEnv <- lookupEnv "HQ_APP_URL"
  assetsBaseEnv <- lookupEnv "HQ_ASSETS_BASE_URL"
  assetsDirEnv <- lookupEnv "HQ_ASSETS_DIR"
  googleClientIdEnv <- lookupEnv "GOOGLE_CLIENT_ID"
  fbAppIdEnv <- lookupFirstEnv ["FACEBOOK_APP_ID", "META_APP_ID"]
  fbAppSecretEnv <- lookupFirstEnv ["FACEBOOK_APP_SECRET", "META_APP_SECRET"]
  fbGraphBaseEnv <- lookupEnv "FACEBOOK_GRAPH_BASE"
  fbMsgTokenEnv <- lookupFirstEnv
    ["FACEBOOK_MESSAGING_TOKEN", "FACEBOOK_PAGE_ACCESS_TOKEN"]
  fbMsgPageIdEnv <- lookupFirstNamedEnv ["FACEBOOK_MESSAGING_PAGE_ID", "FACEBOOK_PAGE_ID"]
  fbMsgBaseEnv <- lookupEnv "FACEBOOK_MESSAGING_API_BASE"
  courseSlugEnv <- lookupEnv "COURSE_DEFAULT_SLUG"
  courseMapEnv <- lookupEnv "COURSE_DEFAULT_MAP_URL"
  courseInstructorAvatarEnv <- lookupEnv "COURSE_DEFAULT_INSTRUCTOR_AVATAR"
  openAiKeyEnv <- lookupEnv "OPENAI_API_KEY"
  openAiModelEnv <- lookupEnv "OPENAI_MODEL"
  openAiEmbedModelEnv <- lookupEnv "OPENAI_EMBED_MODEL"
  chatKitWorkflowEnv <- lookupFirstNamedEnv
    ["CHATKIT_WORKFLOW_ID", "VITE_CHATKIT_WORKFLOW_ID"]
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
  smtpUserEnv <- lookupFirstEnv ["SMTP_USERNAME", "SMTP_USER"]
  smtpPassEnv <- lookupFirstEnv ["SMTP_PASSWORD", "SMTP_PASS"]
  smtpFromEnv <- lookupEnv "SMTP_FROM"
  smtpFromNameEnv <- lookupEnv "SMTP_FROM_NAME"
  smtpTlsEnv  <- lookupEnv "SMTP_TLS"
  igTokenEnv <- lookupEnv "INSTAGRAM_APP_TOKEN"
  igBaseEnv <- lookupEnv "INSTAGRAM_GRAPH_BASE"
  igMsgTokenEnv <- lookupEnv "INSTAGRAM_MESSAGING_TOKEN"
  igMsgAccountEnv <- lookupEnv "INSTAGRAM_MESSAGING_ACCOUNT_ID"
  igMsgBaseEnv <- lookupEnv "INSTAGRAM_MESSAGING_API_BASE"
  igVerifyEnv <- lookupFirstEnv ["INSTAGRAM_VERIFY_TOKEN", "IG_VERIFY_TOKEN"]
  sessionCookieNameEnv <- lookupEnv "SESSION_COOKIE_NAME"
  sessionCookieDomainEnv <- lookupEnv "SESSION_COOKIE_DOMAIN"
  sessionCookiePathEnv <- lookupEnv "SESSION_COOKIE_PATH"
  sessionCookieSecureEnv <- lookupEnv "SESSION_COOKIE_SECURE"
  sessionCookieSameSiteEnv <- lookupEnv "SESSION_COOKIE_SAMESITE"
  sessionCookieMaxAgeEnv <- lookupEnv "SESSION_COOKIE_MAX_AGE"
  assetsRoot <- resolveAssetsRootDir (assetsDirEnv >>= nonEmptyPath)
  appBaseUrlVal <- validateConfiguredBaseUrl "HQ_APP_URL" baseUrlEnv
  assetsBaseUrlVal <- validateConfiguredBaseUrl "HQ_ASSETS_BASE_URL" assetsBaseEnv
  courseDefaultSlugVal <- validateConfiguredCourseSlug courseSlugEnv
  courseMapUrl <- validateConfiguredHttpsUrl "COURSE_DEFAULT_MAP_URL" courseMapEnv
  courseInstructorAvatar <- validateConfiguredHttpsUrl "COURSE_DEFAULT_INSTRUCTOR_AVATAR" courseInstructorAvatarEnv
  chatKitApiBaseVal <-
    validateConfiguredApiBaseUrl
      "CHATKIT_API_BASE"
      "https://api.openai.com"
      chatKitApiBaseEnv
  chatKitWorkflowIdVal <-
    validateConfiguredChatKitWorkflowId chatKitWorkflowEnv
  openAiEmbedModelVal <-
    validateConfiguredOpenAiEmbedModel openAiEmbedModelEnv
  ragTopKVal <- validatePositiveIntEnv "RAG_TOP_K" 8 ragTopKEnv
  ragChunkWordsVal <- validatePositiveIntEnv "RAG_CHUNK_WORDS" 220 ragChunkWordsEnv
  let ragChunkOverlapDefault = min 40 (ragChunkWordsVal - 1)
  ragChunkOverlapVal <-
    validateNonNegativeIntEnv
      "RAG_CHUNK_OVERLAP"
      ragChunkOverlapDefault
      ragChunkOverlapEnv
  when (ragChunkOverlapVal >= ragChunkWordsVal) $
    fail "RAG_CHUNK_OVERLAP must be less than RAG_CHUNK_WORDS"
  ragAvailabilityDaysVal <-
    validatePositiveIntEnv "RAG_AVAILABILITY_DAYS" 14 ragAvailabilityDaysEnv
  ragAvailabilityPerResourceVal <-
    validatePositiveIntEnv "RAG_AVAILABILITY_PER_RESOURCE" 6 ragAvailabilityPerResourceEnv
  ragRefreshHoursVal <- validatePositiveIntEnv "RAG_REFRESH_HOURS" 24 ragRefreshHoursEnv
  ragEmbedBatchSizeVal <- validatePositiveIntEnv "RAG_EMBED_BATCH_SIZE" 64 ragEmbedBatchSizeEnv
  resetDbVal <- validateStartupBooleanFlag "RESET_DB" False rdbEnv
  seedDatabaseVal <- validateStartupBooleanFlag "SEED_DB" False sdbEnv
  runMigrationsVal <- validateStartupBooleanFlag "RUN_MIGRATIONS" True migEnv
  fbGraphBase <-
    validateConfiguredApiBaseUrl
      "FACEBOOK_GRAPH_BASE"
      "https://graph.facebook.com/v20.0"
      fbGraphBaseEnv
  fbMsgBase <-
    validateConfiguredApiBaseUrl
      "FACEBOOK_MESSAGING_API_BASE"
      fbGraphBase
      fbMsgBaseEnv
  fbMsgPageId <- validateConfiguredGraphNodeId fbMsgPageIdEnv
  igGraphBase <-
    validateConfiguredApiBaseUrl
      "INSTAGRAM_GRAPH_BASE"
      "https://graph.instagram.com"
      igBaseEnv
  igMsgBase <-
    validateConfiguredApiBaseUrl
      "INSTAGRAM_MESSAGING_API_BASE"
      "https://graph.facebook.com/v20.0"
      igMsgBaseEnv
  igMsgAccountId <-
    validateConfiguredGraphNodeId
      (fmap (\value -> ("INSTAGRAM_MESSAGING_ACCOUNT_ID", value)) igMsgAccountEnv)
  cookieName <- validateSessionCookieName sessionCookieNameEnv
  cookieDomain <- validateSessionCookieDomain sessionCookieDomainEnv
  cookiePath <- validateSessionCookiePath sessionCookiePathEnv
  emailCfg <- mkEmailConfig smtpHostEnv smtpUserEnv smtpPassEnv smtpFromEnv smtpFromNameEnv smtpPortEnv smtpTlsEnv
  let normalizedAppBase = appBaseUrlVal
      cookieSecureDefault =
        maybe False (\base -> "https://" `T.isPrefixOf` T.toLower base) normalizedAppBase
  cookieSecure <-
    validateStartupBooleanFlag
      "SESSION_COOKIE_SECURE"
      cookieSecureDefault
      sessionCookieSecureEnv
  cookieSameSite <- validateSessionCookieSameSite cookieSecure sessionCookieSameSiteEnv
  cookieMaxAge <- validateSessionCookieMaxAge sessionCookieMaxAgeEnv
  validateSessionCookiePolicy cookieSecure cookieSameSite
  seedToken <- validateSeedTriggerToken seedEnv
  pure AppConfig
    { dbHost = h
    , dbPort = p
    , dbUser = u
    , dbPass = w
    , dbName = d
    , dbConnUrl = connUrl
    , dbSslMode = sslModeEnv <|> (connUrl >>= extractConnUrlParam "sslmode")
    , appPort = appPortVal
    , resetDb = resetDbVal
    , seedDatabase = seedDatabaseVal
    , runMigrations = runMigrationsVal
    , seedTriggerToken = seedToken
    , appBaseUrl = normalizedAppBase
    , assetsBaseUrl = assetsBaseUrlVal
    , assetsRootDir = assetsRoot
    , courseDefaultSlug = courseDefaultSlugVal
    , courseDefaultMapUrl = courseMapUrl
    , courseDefaultInstructorAvatar = courseInstructorAvatar
    , openAiApiKey = openAiKeyEnv >>= nonEmpty . T.pack
    , openAiModel = fromMaybe "gpt-5-chat-latest" (openAiModelEnv >>= nonEmpty . T.pack)
    , openAiEmbedModel = openAiEmbedModelVal
    , chatKitWorkflowId = chatKitWorkflowIdVal
    , chatKitApiBase = chatKitApiBaseVal
    , ragTopK = ragTopKVal
    , ragChunkWords = ragChunkWordsVal
    , ragChunkOverlap = ragChunkOverlapVal
    , ragAvailabilityDays = ragAvailabilityDaysVal
    , ragAvailabilityPerResource = ragAvailabilityPerResourceVal
    , ragRefreshHours = ragRefreshHoursVal
    , ragEmbedBatchSize = ragEmbedBatchSizeVal
    , emailConfig = emailCfg
    , googleClientId = fmap (T.strip . T.pack) googleClientIdEnv
    , facebookAppId = fbAppIdEnv >>= nonEmpty . T.pack
    , facebookAppSecret = fbAppSecretEnv >>= nonEmpty . T.pack
    , facebookGraphBase = fbGraphBase
    , facebookMessagingToken = fmap (T.strip . T.pack) fbMsgTokenEnv >>= nonEmpty
    , facebookMessagingPageId = fbMsgPageId
    , facebookMessagingApiBase = fbMsgBase
    , instagramAppToken = fmap (T.strip . T.pack) igTokenEnv
    , instagramGraphBase = igGraphBase
    , instagramMessagingToken =
        case fmap (T.strip . T.pack) igMsgTokenEnv of
          Just val | not (T.null val) -> Just val
          _ -> fmap (T.strip . T.pack) igTokenEnv
    , instagramMessagingAccountId = igMsgAccountId
    , instagramMessagingApiBase = igMsgBase
    , instagramVerifyToken = fmap (T.strip . T.pack) igVerifyEnv >>= nonEmpty
    , sessionCookieName = cookieName
    , sessionCookieDomain = cookieDomain
    , sessionCookiePath = cookiePath
    , sessionCookieSecure = cookieSecure
    , sessionCookieSameSite = cookieSameSite
    , sessionCookieMaxAgeSeconds = cookieMaxAge
    }
  where
    getWithFallback keys def = fmap (fromMaybe def) (lookupFirstEnv keys)
    allEnvPresent [] = pure True
    allEnvPresent (keys:rest) = do
      value <- lookupFirstEnv keys
      case value of
        Just _ -> allEnvPresent rest
        Nothing -> pure False
    lookupFirstEnv [] = pure Nothing
    lookupFirstEnv (key:rest) = do
      value <- lookupEnv key
      case value >>= normalizeEnvString of
        Just normalized -> pure (Just normalized)
        Nothing -> lookupFirstEnv rest
    lookupFirstNamedEnv [] = pure Nothing
    lookupFirstNamedEnv (key:rest) = do
      value <- lookupEnv key
      case value >>= normalizeEnvString of
        Just normalized -> pure (Just (key, normalized))
        Nothing -> lookupFirstNamedEnv rest
    lookupFirstConnUrlEnv _ [] = pure Nothing
    lookupFirstConnUrlEnv requireValid (key:rest) = do
      value <- lookupEnv key
      case value >>= normalizeEnvString of
        Nothing -> lookupFirstConnUrlEnv requireValid rest
        Just normalized ->
          case validateFallbackConnUrl key normalized of
            Right conn -> pure (Just conn)
            Left msg
              | requireValid -> fail msg
              | otherwise -> lookupFirstConnUrlEnv requireValid rest
    normalizeEnvString raw =
      let trimmed = T.unpack (T.strip (T.pack raw))
      in if null trimmed then Nothing else Just trimmed
    asBool v = case fmap toLower v of
      "true"  -> True
      "1"     -> True
      "yes"   -> True
      "on"    -> True
      _       -> False
    validateSeedTriggerToken mVal =
      case fmap (T.strip . T.pack) mVal of
        Nothing  -> pure Nothing
        Just txt
          | T.null txt -> pure Nothing
          | T.any (\ch -> isSpace ch || isControl ch) txt ->
              fail "SEED_TRIGGER_TOKEN must not contain whitespace or control characters"
          | T.length txt < 16 ->
              fail "SEED_TRIGGER_TOKEN must be at least 16 characters"
          | T.length txt > 512 ->
              fail "SEED_TRIGGER_TOKEN must be 512 characters or fewer"
          | otherwise -> pure (Just txt)
    mkEmailConfig mHost mUser mPass mFrom mFromName mPort mTls = do
      let host = normalizeRequiredSmtpValue mHost
          user = normalizeRequiredSmtpValue mUser
          pass = normalizeRequiredSmtpValue mPass
          fromAddr = normalizeRequiredSmtpValue mFrom
          required :: [(String, Maybe Text)]
          required =
            [ ("SMTP_HOST", host)
            , ("SMTP_USERNAME/SMTP_USER", user)
            , ("SMTP_PASSWORD/SMTP_PASS", pass)
            , ("SMTP_FROM", fromAddr)
            ]
          anyConfigured = any (maybe False (const True) . snd) required
          missing = [label | (label, Nothing) <- required]
      if not anyConfigured
        then pure Nothing
        else case (missing, host, user, pass, fromAddr) of
          ([], Just hostVal, Just userVal, Just passVal, Just rawFromAddr) -> do
            when (T.any (\ch -> isControl ch || isSpace ch) hostVal) $
              fail "SMTP_HOST must not contain whitespace or control characters"
            normalizedFrom <- case normalizeConfiguredEmailAddress rawFromAddr of
              Just email -> pure email
              Nothing -> fail "SMTP_FROM must be a valid email address"
            let name = maybe "TDF Records" (T.strip . T.pack) mFromName
                useTls = maybe True asBool mTls
            portVal <- validatePortEnv "SMTP_PORT" 587 mPort
            pure $
              Just EmailConfig
                { emailFromName = if T.null name then "TDF Records" else name
                , emailFromAddress = normalizedFrom
                , smtpHost = hostVal
                , smtpPort = portVal
                , smtpUsername = userVal
                , smtpPassword = passVal
                , smtpUseTLS = useTls
                }
          _ ->
            fail $
              "SMTP configuration requires non-empty SMTP_HOST, SMTP_USERNAME/SMTP_USER, "
                <> "SMTP_PASSWORD/SMTP_PASS, and SMTP_FROM"

    normalizeRequiredSmtpValue =
      fmap (T.strip . T.pack) >=> nonEmpty

    normalizeConfiguredEmailAddress rawEmail =
      let normalized = T.toLower (T.strip rawEmail)
      in if isValidConfiguredEmailAddress normalized then Just normalized else Nothing

    isValidConfiguredEmailAddress email =
      case T.splitOn "@" email of
        [localPart, domainPart] ->
          validEmailPart localPart
            && validDomain domainPart
            && not (".." `T.isInfixOf` localPart)
            && not (".." `T.isInfixOf` domainPart)
            && not (T.isPrefixOf "." localPart)
            && not (T.isSuffixOf "." localPart)
        _ -> False

    validEmailPart part =
      not (T.null part)
        && T.all
          (\ch ->
            (ch >= 'a' && ch <= 'z')
              || (ch >= '0' && ch <= '9')
              || ch `elem` (".!#$%&'*+/=?^_`{|}~-" :: String)
          )
          part

    validDomain domain =
      let labels = T.splitOn "." domain
      in length labels >= 2 && all validDomainLabel labels

    validDomainLabel label =
      not (T.null label)
        && not (T.isPrefixOf "-" label)
        && not (T.isSuffixOf "-" label)
        && T.all
          (\ch ->
            (ch >= 'a' && ch <= 'z')
              || (ch >= '0' && ch <= '9')
              || ch == '-'
          )
          label

validateSessionCookieSameSite :: Bool -> Maybe String -> IO Text
validateSessionCookieSameSite cookieSecure rawSameSite =
  case rawSameSite of
    Nothing -> pure defaultValue
    Just raw ->
      let trimmed = T.strip (T.pack raw)
      in if T.null trimmed
           then pure defaultValue
           else case map toLower (T.unpack trimmed) of
             "lax" -> pure "Lax"
             "strict" -> pure "Strict"
             "none" -> pure "None"
             _ -> fail "SESSION_COOKIE_SAMESITE must be one of: Lax, Strict, None"
  where
    defaultValue = if cookieSecure then "None" else "Lax"

validateSessionCookiePolicy :: Bool -> Text -> IO ()
validateSessionCookiePolicy cookieSecure cookieSameSite =
  when (T.toLower cookieSameSite == "none" && not cookieSecure) $
    fail "SESSION_COOKIE_SAMESITE=None requires secure session cookies"

validateSessionCookieMaxAge :: Maybe String -> IO (Maybe Int)
validateSessionCookieMaxAge Nothing = pure (Just defaultSessionCookieMaxAgeSeconds)
validateSessionCookieMaxAge (Just rawMaxAge)
  | T.null trimmed = pure (Just defaultSessionCookieMaxAgeSeconds)
  | otherwise =
      case readMaybe (T.unpack trimmed) of
        Just maxAge | maxAge > 0 -> pure (Just maxAge)
        _ -> fail "SESSION_COOKIE_MAX_AGE must be a positive integer number of seconds"
  where
    trimmed = T.strip (T.pack rawMaxAge)

defaultSessionCookieMaxAgeSeconds :: Int
defaultSessionCookieMaxAgeSeconds = 60 * 60 * 24 * 30

validateStartupBooleanFlag :: String -> Bool -> Maybe String -> IO Bool
validateStartupBooleanFlag _ defaultValue Nothing = pure defaultValue
validateStartupBooleanFlag envName defaultValue (Just rawValue)
  | T.null normalized = pure defaultValue
  | otherwise =
      case T.unpack normalized of
        "true" -> pure True
        "1" -> pure True
        "yes" -> pure True
        "on" -> pure True
        "false" -> pure False
        "0" -> pure False
        "no" -> pure False
        "off" -> pure False
        _ ->
          fail $
            envName
              <> " must be a boolean flag (true/false, 1/0, yes/no, on/off)"
  where
    normalized = T.toLower (T.strip (T.pack rawValue))

validateSessionCookieDomain :: Maybe String -> IO (Maybe Text)
validateSessionCookieDomain rawDomain =
  case normalizeSessionCookieDomain rawDomain of
    Left msg -> fail msg
    Right domainVal -> pure domainVal

normalizeSessionCookieDomain :: Maybe String -> Either String (Maybe Text)
normalizeSessionCookieDomain Nothing = Right Nothing
normalizeSessionCookieDomain (Just rawDomain)
  | T.null trimmed = Right Nothing
  | "http://" `T.isPrefixOf` lowered || "https://" `T.isPrefixOf` lowered =
      invalid
  | T.any invalidDomainChar trimmed =
      invalid
  | T.null canonical =
      invalid
  | T.isPrefixOf "." canonical || T.isSuffixOf "." canonical =
      invalid
  | any (not . isValidDomainLabel) (T.splitOn "." canonical) =
      invalid
  | otherwise =
      Right (Just canonical)
  where
    trimmed = T.strip (T.pack rawDomain)
    lowered = T.toLower trimmed
    canonical =
      case T.stripPrefix "." lowered of
        Just rest -> rest
        Nothing -> lowered
    invalid =
      Left
        "SESSION_COOKIE_DOMAIN must be a cookie domain without scheme, port, path, whitespace, separators, or control characters"
    invalidDomainChar ch =
      isControl ch
        || isSpace ch
        || ch `elem` (";,:/\\@[]()" :: String)
    isValidDomainLabel label =
      not (T.null label)
        && not (T.isPrefixOf "-" label)
        && not (T.isSuffixOf "-" label)
        && T.all isDomainChar label
    isDomainChar ch =
      (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '-'

validateSessionCookieName :: Maybe String -> IO Text
validateSessionCookieName rawName =
  case normalizeSessionCookieName rawName of
    Left msg -> fail msg
    Right name -> pure name

normalizeSessionCookieName :: Maybe String -> Either String Text
normalizeSessionCookieName Nothing = Right "tdf_session"
normalizeSessionCookieName (Just rawName)
  | T.null name = Right "tdf_session"
  | T.all isCookieNameChar name = Right name
  | otherwise = invalid
  where
    name = T.strip (T.pack rawName)
    invalid =
      Left
        "SESSION_COOKIE_NAME must be a cookie token with no whitespace, separators, or control characters"
    separators = ("()<>@,;:\\\"/[]?={} \t" :: String)
    isCookieNameChar ch =
      not (isControl ch) && ch < '\DEL' && ch `notElem` separators

validateSessionCookiePath :: Maybe String -> IO Text
validateSessionCookiePath rawPath =
  case normalizeSessionCookiePath rawPath of
    Left msg -> fail msg
    Right path -> pure path

normalizeSessionCookiePath :: Maybe String -> Either String Text
normalizeSessionCookiePath Nothing = Right "/"
normalizeSessionCookiePath (Just rawPath)
  | T.null path = Right "/"
  | not ("/" `T.isPrefixOf` path) =
      invalid
  | T.any invalidPathChar path =
      invalid
  | otherwise =
      Right path
  where
    path = T.strip (T.pack rawPath)
    invalid = Left "SESSION_COOKIE_PATH must start with / and contain no whitespace, semicolons, commas, or control characters"
    invalidPathChar ch =
      isControl ch || isSpace ch || ch == ';' || ch == ','

validateConfiguredHttpsUrl :: String -> Maybe String -> IO (Maybe Text)
validateConfiguredHttpsUrl _ Nothing = pure Nothing
validateConfiguredHttpsUrl envName (Just rawUrl) =
  case normalizeConfiguredHttpsUrl envName rawUrl of
    Left msg -> fail msg
    Right urlVal -> pure urlVal

validateConfiguredBaseUrl :: String -> Maybe String -> IO (Maybe Text)
validateConfiguredBaseUrl _ Nothing = pure Nothing
validateConfiguredBaseUrl envName (Just rawUrl) =
  case normalizeConfiguredBaseUrl envName rawUrl of
    Left msg -> fail msg
    Right urlVal -> pure urlVal

validateConfiguredApiBaseUrl :: String -> Text -> Maybe String -> IO Text
validateConfiguredApiBaseUrl _ defaultUrl Nothing = pure defaultUrl
validateConfiguredApiBaseUrl envName defaultUrl (Just rawUrl)
  | T.null trimmed = pure defaultUrl
  | otherwise =
      case normalizeConfiguredApiBaseUrl envName (T.unpack trimmed) of
        Left msg -> fail msg
        Right urlVal -> pure urlVal
  where
    trimmed = T.strip (T.pack rawUrl)

validateConfiguredChatKitWorkflowId :: Maybe (String, String) -> IO (Maybe Text)
validateConfiguredChatKitWorkflowId Nothing = pure Nothing
validateConfiguredChatKitWorkflowId (Just (envName, rawWorkflowId)) =
  case normalizeConfiguredChatKitWorkflowId envName rawWorkflowId of
    Left msg -> fail msg
    Right workflowId -> pure workflowId

validateConfiguredGraphNodeId :: Maybe (String, String) -> IO (Maybe Text)
validateConfiguredGraphNodeId Nothing = pure Nothing
validateConfiguredGraphNodeId (Just (envName, rawNodeId)) =
  case normalizeConfiguredGraphNodeId envName rawNodeId of
    Left msg -> fail msg
    Right nodeId -> pure nodeId

validateConfiguredOpenAiEmbedModel :: Maybe String -> IO Text
validateConfiguredOpenAiEmbedModel Nothing = pure defaultOpenAiEmbedModel
validateConfiguredOpenAiEmbedModel (Just rawModel)
  | T.null model = pure defaultOpenAiEmbedModel
  | otherwise =
      case openAiEmbedDimensions model of
        Just _ -> pure model
        Nothing ->
          fail
            ( "OPENAI_EMBED_MODEL must be one of: text-embedding-3-small, "
              <> "text-embedding-3-large, text-embedding-ada-002"
            )
  where
    model = T.toLower (T.strip (T.pack rawModel))

defaultOpenAiEmbedModel :: Text
defaultOpenAiEmbedModel = "text-embedding-3-small"

normalizeConfiguredChatKitWorkflowId :: String -> String -> Either String (Maybe Text)
normalizeConfiguredChatKitWorkflowId envName rawWorkflowId
  | T.null workflowId = Right Nothing
  | T.length workflowId > 256 =
      Left (envName <> " must be 256 characters or fewer")
  | T.any isSpace workflowId =
      Left (envName <> " must not contain whitespace")
  | T.any isControl workflowId =
      Left (envName <> " must not contain control characters")
  | T.any (not . isChatKitWorkflowIdChar) workflowId =
      Left (envName <> " must use only ASCII letters, digits, '.', '_' or '-'")
  | otherwise = Right (Just workflowId)
  where
    workflowId = T.strip (T.pack rawWorkflowId)
    isChatKitWorkflowIdChar ch =
      (ch >= 'a' && ch <= 'z')
        || (ch >= 'A' && ch <= 'Z')
        || (ch >= '0' && ch <= '9')
        || ch `elem` ("._-" :: String)

normalizeConfiguredGraphNodeId :: String -> String -> Either String (Maybe Text)
normalizeConfiguredGraphNodeId envName rawNodeId
  | T.null nodeId = Right Nothing
  | T.length nodeId > 128 = invalid
  | not (T.any isGraphNodeIdAtom nodeId) = invalid
  | T.any (not . isGraphNodeIdChar) nodeId = invalid
  | otherwise = Right (Just nodeId)
  where
    nodeId = T.strip (T.pack rawNodeId)
    invalid =
      Left
        ( envName
            <> " must be a Graph node id using only ASCII letters, numbers, "
            <> "'.', '_' or '-' with at least one letter or number (128 chars max)"
        )
    isGraphNodeIdAtom ch =
      (ch >= 'a' && ch <= 'z')
        || (ch >= 'A' && ch <= 'Z')
        || (ch >= '0' && ch <= '9')
    isGraphNodeIdChar ch =
      isGraphNodeIdAtom ch || ch `elem` ("._-" :: String)

normalizeConfiguredApiBaseUrl :: String -> String -> Either String Text
normalizeConfiguredApiBaseUrl envName rawUrl
  | T.null (T.strip (T.pack rawUrl)) =
      Left (envName <> " must be an absolute https URL")
  | otherwise = do
      mUrl <- normalizeConfiguredHttpsUrl envName rawUrl
      case mUrl of
        Nothing ->
          Left (envName <> " must be an absolute https URL")
        Just urlVal
          | T.any (`elem` ("?#" :: String)) urlVal ->
              Left (envName <> " must be an absolute https URL without query or fragment")
          | otherwise ->
              Right (T.dropWhileEnd (== '/') urlVal)

validateConfiguredCourseSlug :: Maybe String -> IO Text
validateConfiguredCourseSlug rawSlug =
  case normalizeConfiguredCourseSlug rawSlug of
    Left msg -> fail msg
    Right slug -> pure slug

normalizeConfiguredCourseSlug :: Maybe String -> Either String Text
normalizeConfiguredCourseSlug Nothing = Right defaultCourseSlug
normalizeConfiguredCourseSlug (Just rawSlug)
  | T.null slugVal = Right defaultCourseSlug
  | T.length slugVal > 96 = invalid
  | T.all isSlugChar slugVal && T.any isSlugAtom slugVal = Right slugVal
  | otherwise = invalid
  where
    slugVal = T.toLower (T.strip (T.pack rawSlug))
    isSlugAtom ch = (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9')
    isSlugChar ch = isSlugAtom ch || ch == '-'
    invalid =
      Left
        "COURSE_DEFAULT_SLUG must use only ASCII letters, numbers, and hyphens (96 chars max)"

defaultCourseSlug :: Text
defaultCourseSlug = "produccion-musical-abr-2026"

normalizeConfiguredBaseUrl :: String -> String -> Either String (Maybe Text)
normalizeConfiguredBaseUrl envName rawUrl
  | T.null trimmed = Right Nothing
  | T.any isSpace trimmed =
      invalid
  | T.any (`elem` ("?#" :: String)) trimmed =
      Left (envName <> " must be an absolute http(s) URL without query or fragment")
  | "https://" `T.isPrefixOf` lowerUrl =
      validateRemainder (T.drop 8 trimmed)
  | "http://" `T.isPrefixOf` lowerUrl =
      validateRemainder (T.drop 7 trimmed)
  | otherwise =
      invalid
  where
    trimmed = T.strip (T.pack rawUrl)
    lowerUrl = T.toLower trimmed
    invalid = Left (envName <> " must be an absolute http(s) URL")

    validateRemainder remainder =
      if hasValidAuthority remainder
        then Right (Just trimmed)
        else invalid

    hasValidAuthority remainder =
      let authority = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') remainder
      in validateAuthority authority

    validateAuthority rawAuthority
      | T.null rawAuthority = False
      | T.any (== '@') rawAuthority = False
      | "[" `T.isPrefixOf` rawAuthority =
          let (hostPart, rest) = T.breakOn "]" rawAuthority
              host = T.drop 1 hostPart
          in not (T.null rest)
            && validateBracketedHost host
            && validatePortSuffix (T.drop 1 rest)
      | T.count ":" rawAuthority > 1 = False
      | otherwise =
          let (host, portSuffix) = T.breakOn ":" rawAuthority
          in validateHost host && validatePortSuffix portSuffix

    validateHost host =
      let normalizedHost = T.toLower host
      in not (T.null normalizedHost)
        && hasPublicOrLocalhostShape normalizedHost
        && not (T.isPrefixOf "." normalizedHost)
        && not (T.isSuffixOf "." normalizedHost)
        && not (isAmbiguousNumericHost normalizedHost)
        && all isValidHostLabel (T.splitOn "." normalizedHost)

    hasPublicOrLocalhostShape host =
      host == "localhost"
        || ".localhost" `T.isSuffixOf` host
        || "." `T.isInfixOf` host
        || case parseIpv4Octets host of
             Just _ -> True
             Nothing -> False

    validateBracketedHost host =
      not (T.null host)
        && T.any (== ':') host
        && T.all (`elem` ("0123456789abcdefABCDEF:." :: String)) host

    validatePortSuffix suffix
      | T.null suffix = True
      | ":" `T.isPrefixOf` suffix =
          let port = T.drop 1 suffix
          in not (T.null port)
            && T.all (\ch -> ch >= '0' && ch <= '9') port
            && maybe False (\portNumber -> portNumber >= (1 :: Int) && portNumber <= 65535)
                (readMaybe (T.unpack port))
      | otherwise = False

    isValidHostLabel label =
      not (T.null label)
        && not (T.isPrefixOf "-" label)
        && not (T.isSuffixOf "-" label)
        && T.all isHostChar label

    isHostChar ch =
      (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '-'

    isAmbiguousNumericHost host =
      T.all (\ch -> isDigit ch || ch == '.') host
        && isNothing (parseIpv4Octets host)

    parseIpv4Octets host =
      case T.splitOn "." host of
        [a, b, c, d] -> do
          oa <- parseOctet a
          ob <- parseOctet b
          oc <- parseOctet c
          od <- parseOctet d
          pure (oa, ob, oc, od)
        _ -> Nothing

    parseOctet octet
      | T.null octet || T.any (not . isDigit) octet = Nothing
      | T.length octet > 1 && T.head octet == '0' = Nothing
      | otherwise = do
          value <- readMaybe (T.unpack octet)
          if value >= (0 :: Int) && value <= 255
            then Just value
            else Nothing

normalizeConfiguredHttpsUrl :: String -> String -> Either String (Maybe Text)
normalizeConfiguredHttpsUrl envName rawUrl
  | T.null trimmed = Right Nothing
  | T.any isSpace trimmed =
      invalid
  | not ("https://" `T.isPrefixOf` lowerUrl) =
      invalid
  | not (hasValidAuthority (T.drop 8 trimmed)) =
      invalid
  | otherwise =
      Right (Just trimmed)
  where
    trimmed = T.strip (T.pack rawUrl)
    lowerUrl = T.toLower trimmed
    invalid = Left (envName <> " must be an absolute https URL")

    hasValidAuthority remainder =
      let authority = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') remainder
          (host, portSuffix) = T.breakOn ":" authority
      in not (T.null authority)
        && validateHost host
        && validatePortSuffix portSuffix

    validateHost host =
      let normalizedHost = T.toLower host
          labels = T.splitOn "." normalizedHost
      in length labels >= 2
        && not (normalizedHost == "localhost" || ".localhost" `T.isSuffixOf` normalizedHost)
        && all isValidHostLabel labels
        && not (isAmbiguousNumericHost normalizedHost)
        && not (isNonPublicIpv4Host normalizedHost)

    isValidHostLabel label =
      not (T.null label)
        && not (T.isPrefixOf "-" label)
        && not (T.isSuffixOf "-" label)
        && T.all isHostChar label

    isHostChar ch =
      (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '-'

    isAmbiguousNumericHost host =
      T.all (\ch -> isDigit ch || ch == '.') host
        && isNothing (parseIpv4Octets host)

    validatePortSuffix suffix
      | T.null suffix = True
      | ":" `T.isPrefixOf` suffix =
          let port = T.drop 1 suffix
          in not (T.null port)
            && T.all (\ch -> ch >= '0' && ch <= '9') port
            && maybe False (\portNumber -> portNumber >= (1 :: Int) && portNumber <= 65535)
                (readMaybe (T.unpack port))
      | otherwise = False

    isNonPublicIpv4Host host =
      case parseIpv4Octets host of
        Nothing -> False
        Just (a, b, c, _) ->
          a == (0 :: Int)
            || a == 10
            || a == 127
            || (a == 100 && b >= 64 && b <= 127)
            || (a == 169 && b == 254)
            || (a == 172 && b >= 16 && b <= 31)
            || (a == 192 && b == 0 && c == 0)
            || (a == 192 && b == 0 && c == 2)
            || (a == 192 && b == 168)
            || (a == 198 && (b == 18 || b == 19))
            || (a == 198 && b == 51 && c == 100)
            || (a == 203 && b == 0 && c == 113)
            || (a >= 224 && a <= 255)

    parseIpv4Octets host =
      case T.splitOn "." host of
        [a, b, c, d] -> do
          oa <- parseOctet a
          ob <- parseOctet b
          oc <- parseOctet c
          od <- parseOctet d
          pure (oa, ob, oc, od)
        _ -> Nothing

    parseOctet octet
      | T.null octet || T.any (not . isDigit) octet = Nothing
      | T.length octet > 1 && T.head octet == '0' = Nothing
      | otherwise = do
          value <- readMaybe (T.unpack octet)
          if value >= (0 :: Int) && value <= 255
            then Just value
            else Nothing

resolveAssetsRootDir :: Maybe FilePath -> IO FilePath
resolveAssetsRootDir (Just configuredPath) = do
  exists <- doesDirectoryExist configuredPath
  if exists
    then pure configuredPath
    else fail "HQ_ASSETS_DIR must point to an existing directory"
resolveAssetsRootDir Nothing = do
  let candidates = ["/app/assets", "tdf-hq/assets", "assets"]
  existing <- filterM doesDirectoryExist candidates
  pure $ fromMaybe "assets" (listToMaybe existing)

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
  in if T.null val then defaultCourseSlug else val

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
