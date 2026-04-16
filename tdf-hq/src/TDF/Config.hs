{-# LANGUAGE OverloadedStrings #-}
module TDF.Config where

import           Control.Applicative ((<|>))
import           Control.Monad      (filterM, when)
import           Data.Char          (isControl, isDigit, isSpace, toLower)
import           Data.List          (isInfixOf, isPrefixOf)
import           Data.Maybe         (catMaybes, fromMaybe, isNothing, listToMaybe)
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
      | otherwise =
          let remainder = T.drop (length scheme) value
              authority = T.takeWhile (`notElem` ("/?#" :: String)) remainder
              hostPort =
                case reverse (T.splitOn "@" authority) of
                  [] -> ""
                  h:_ -> h
          in if T.null authority
               then Left (envName <> " must include a PostgreSQL host")
               else validateConnectionHostPort hostPort *> Right raw

    validateConnectionHostPort :: Text -> Either String ()
    validateConnectionHostPort hostPort
      | "[" `T.isPrefixOf` hostPort =
          let host = T.takeWhile (/= ']') (T.drop 1 hostPort)
              suffix = T.drop (T.length host + 1) hostPort
          in if T.null host || not ("]" `T.isPrefixOf` suffix)
               then Left (envName <> " must include a PostgreSQL host")
               else validateConnectionPortSuffix (T.drop 1 suffix)
      | otherwise =
          let (host, suffix) = T.breakOn ":" hostPort
          in if T.null host
               then Left (envName <> " must include a PostgreSQL host")
               else validateConnectionPortSuffix suffix

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
  | hasTargetSessionAttrs normalized = rawConn
  | isPostgresUrl normalized =
      rawConn <> if '?' `elem` rawConn then "&target_session_attrs=read-write" else "?target_session_attrs=read-write"
  | otherwise = rawConn <> " target_session_attrs=read-write"
  where
    normalized = map toLower rawConn
    hasTargetSessionAttrs conn = "target_session_attrs=" `isInfixOf` conn
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
  h          <- getWithFallback ["DB_HOST", "PGHOST"] "127.0.0.1"
  p          <- getWithFallback ["DB_PORT", "PGPORT"] "5432"
  u          <- getWithFallback ["DB_USER", "PGUSER"] "postgres"
  w          <- getWithFallback ["DB_PASS", "PGPASSWORD"] "postgres"
  d          <- getWithFallback ["DB_NAME", "PGDATABASE"] "tdf_hq"
  sslModeEnv <- lookupFirstEnv ["DB_SSLMODE", "PGSSLMODE"]
  ap         <- get "APP_PORT" "8080"
  rdb        <- get "RESET_DB" "false"
  sdb        <- get "SEED_DB" "false"
  mig        <- get "RUN_MIGRATIONS" "true"
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
  fbMsgPageIdEnv <- lookupFirstEnv ["FACEBOOK_MESSAGING_PAGE_ID", "FACEBOOK_PAGE_ID"]
  fbMsgBaseEnv <- lookupEnv "FACEBOOK_MESSAGING_API_BASE"
  courseSlugEnv <- lookupEnv "COURSE_DEFAULT_SLUG"
  courseMapEnv <- lookupEnv "COURSE_DEFAULT_MAP_URL"
  courseInstructorAvatarEnv <- lookupEnv "COURSE_DEFAULT_INSTRUCTOR_AVATAR"
  openAiKeyEnv <- lookupEnv "OPENAI_API_KEY"
  openAiModelEnv <- lookupEnv "OPENAI_MODEL"
  openAiEmbedModelEnv <- lookupEnv "OPENAI_EMBED_MODEL"
  chatKitWorkflowEnv <- lookupFirstEnv
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
  courseDefaultSlugVal <- validateConfiguredCourseSlug courseSlugEnv
  courseMapUrl <- validateConfiguredHttpsUrl "COURSE_DEFAULT_MAP_URL" courseMapEnv
  courseInstructorAvatar <- validateConfiguredHttpsUrl "COURSE_DEFAULT_INSTRUCTOR_AVATAR" courseInstructorAvatarEnv
  cookiePath <- validateSessionCookiePath sessionCookiePathEnv
  let fbGraphBase = fromMaybe "https://graph.facebook.com/v20.0" (fbGraphBaseEnv >>= nonEmpty . T.pack)
      igGraphBase = maybe "https://graph.instagram.com" (T.strip . T.pack) igBaseEnv
      normalizedAppBase = fmap (T.strip . T.pack) baseUrlEnv >>= nonEmpty
      cookieSecureDefault =
        maybe False (\base -> "https://" `T.isPrefixOf` T.toLower base) normalizedAppBase
      cookieSecure = maybe cookieSecureDefault asBool sessionCookieSecureEnv
      cookieSameSite =
        normalizeSameSiteValue $
          fromMaybe
            (if cookieSecure then "None" else "Lax")
            sessionCookieSameSiteEnv
  validateSessionCookiePolicy cookieSecure cookieSameSite
  pure AppConfig
    { dbHost = h
    , dbPort = p
    , dbUser = u
    , dbPass = w
    , dbName = d
    , dbConnUrl = connUrl
    , dbSslMode = sslModeEnv <|> (fallbackConnUrl >>= extractConnUrlParam "sslmode")
    , appPort = parseInt 8080 (Just ap)
    , resetDb = asBool rdb
    , seedDatabase = asBool sdb
    , runMigrations = asBool mig
    , seedTriggerToken = mkSeedToken seedEnv
    , appBaseUrl = normalizedAppBase
    , assetsBaseUrl = fmap (T.strip . T.pack) assetsBaseEnv
    , assetsRootDir = assetsRoot
    , courseDefaultSlug = courseDefaultSlugVal
    , courseDefaultMapUrl = courseMapUrl
    , courseDefaultInstructorAvatar = courseInstructorAvatar
    , openAiApiKey = openAiKeyEnv >>= nonEmpty . T.pack
    , openAiModel = fromMaybe "gpt-5-chat-latest" (openAiModelEnv >>= nonEmpty . T.pack)
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
    , sessionCookieName = fromMaybe "tdf_session" (sessionCookieNameEnv >>= nonEmpty . T.pack)
    , sessionCookieDomain = fmap (T.strip . T.pack) sessionCookieDomainEnv >>= nonEmpty
    , sessionCookiePath = cookiePath
    , sessionCookieSecure = cookieSecure
    , sessionCookieSameSite = cookieSameSite
    , sessionCookieMaxAgeSeconds = parsePositiveInt sessionCookieMaxAgeEnv <|> Just (60 * 60 * 24 * 30)
    }
  where
    get k def = fmap (fromMaybe def) (lookupEnv k)
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
    parseInt def mVal =
      case mVal >>= readMaybe of
        Just n | n > 0 -> n
        _ -> def
    parsePositiveInt mVal =
      case mVal >>= readMaybe of
        Just n | n > 0 -> Just n
        _ -> Nothing
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
          portVal = parseInt 587 mPort
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

normalizeSameSiteValue :: String -> Text
normalizeSameSiteValue raw =
  case map toLower (T.unpack (T.strip (T.pack raw))) of
    "strict" -> "Strict"
    "none" -> "None"
    _ -> "Lax"

validateSessionCookiePolicy :: Bool -> Text -> IO ()
validateSessionCookiePolicy cookieSecure cookieSameSite =
  when (T.toLower cookieSameSite == "none" && not cookieSecure) $
    fail "SESSION_COOKIE_SAMESITE=None requires secure session cookies"

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
        && not (isPrivateIpv4Host normalizedHost)

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

    isPrivateIpv4Host host =
      case parseIpv4Octets host of
        Nothing -> False
        Just (a, b, _, _) ->
          a == (0 :: Int)
            || a == 10
            || a == 127
            || (a == 100 && b >= 64 && b <= 127)
            || (a == 169 && b == 254)
            || (a == 172 && b >= 16 && b <= 31)
            || (a == 192 && b == 168)

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
