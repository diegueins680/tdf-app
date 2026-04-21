{-# LANGUAGE OverloadedStrings #-}
module TDF.Cors
  ( corsPolicy
  , deriveCorsOriginFromAppBase
  , isTrustedPreviewOrigin
  , lookupFirstNonEmptyEnv
  ) where
import Network.Wai (Middleware, Request, requestHeaders)
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit, isSpace, toLower)
import Data.List (dropWhileEnd, intercalate, nub)
import Data.Maybe (isNothing)
import Text.Read (readMaybe)

corsPolicy :: IO Middleware
corsPolicy = do
  originsEnv <- lookupFirstNonEmptyEnv
    [ "ALLOWED_ORIGINS"
    , "ALLOW_ORIGINS"
    , "ALLOW_ORIGIN"
    , "CORS_ALLOW_ORIGINS"
    , "CORS_ALLOW_ORIGIN"
    ]
  hqBaseEnv <- lookupEnv "HQ_APP_URL"
  allowAllEnv <- lookupFirstNonEmptyEnv
    [ "ALLOW_ALL_ORIGINS"
    , "CORS_ALLOW_ALL_ORIGINS"
    ]
  disableDefaultsEnv <- lookupFirstNonEmptyEnv
    [ "CORS_DISABLE_DEFAULTS"
    , "DISABLE_DEFAULT_CORS"
    ]
  allowAllFlag <- either (ioError . userError) pure $
    maybe (Right False) (parseBoolFlag "ALLOW_ALL_ORIGINS") allowAllEnv
  disableDefaultsFlag <- either (ioError . userError) pure $
    maybe (Right False) (parseBoolFlag "CORS_DISABLE_DEFAULTS") disableDefaultsEnv
  let defaultsCore =
        [ "http://localhost:5173"
        , "http://127.0.0.1:5173"
        , "https://tdfui.pages.dev"
        ]
      hqBaseCandidates = filter (not . null . trim) (maybe [] pure hqBaseEnv)
      parsed = maybe [] splitComma originsEnv
  hqBaseDefaults <- either (ioError . userError) pure $
    traverse deriveCorsOriginFromAppBase hqBaseCandidates
  filtered <- either (ioError . userError) pure $
    traverse normalizeConfiguredCorsOrigin (filter (not . null) parsed)
  let
      defaults = defaultsCore ++ hqBaseDefaults
      includeDefaults = not disableDefaultsFlag
      merged = (if includeDefaults then defaults else []) ++ filtered
      deduped = nub merged
      allowAll = allowAllFlag || any (== "*") deduped
      effective =
        if null deduped
          then if includeDefaults then defaults else []
          else deduped
      wildcard = allowAll || any (== "*") deduped
      explicitOriginSetting = Just (map BS.pack effective, True)
      basePolicy = simpleCorsResourcePolicy
        { corsOrigins            = explicitOriginSetting
        , corsRequestHeaders     = "authorization":"content-type":"x-requested-with":simpleHeaders
        , corsMethods            = ["GET","POST","PUT","PATCH","DELETE","OPTIONS"]
        , corsRequireOrigin      = False
        , corsIgnoreFailures     = False
        }
      allowPagesDevWildcard = True
      allowAllPolicy = basePolicy { corsOrigins = Nothing }
      allowOriginPolicy origin = basePolicy { corsOrigins = Just ([origin], True) }
      choosePolicy :: Request -> Maybe CorsResourcePolicy
      choosePolicy req =
        case lookup "origin" (requestHeaders req) of
          Nothing -> Just allowAllPolicy -- allow health/public probes without CORS failures
          Just o
            | allowAll -> Just (allowOriginPolicy o)
            | allowPagesDevWildcard && isTrustedPreviewOrigin o -> Just (allowOriginPolicy o)
            | otherwise -> Just basePolicy
      originLog =
        if wildcard
          then "*"
          else if null effective then "(none)" else intercalate "," effective
      notes =
        (if includeDefaults then "" else " defaults=off")
          <> (if allowAll then " allowAll=true" else "")
          <> (if null deduped && includeDefaults && not allowAll then " (fallback to defaults)" else "")
  putStrLn $ "[cors] origins=" <> originLog <> notes
  putStrLn $ "[cors] trusted preview wildcard=" <> show allowPagesDevWildcard
  pure (cors choosePolicy)

-- | Allow credentialed preview origins only for known TDF Pages projects.
isTrustedPreviewOrigin :: BS.ByteString -> Bool
isTrustedPreviewOrigin origin =
  case parseHttpsOriginHost origin of
    Nothing -> False
    Just host ->
      any (matchesTrustedPagesHost host)
        [ "tdfui.pages.dev"
        , "tdf-app.pages.dev"
        ]
  where
    matchesTrustedPagesHost host root =
      host == root || ("." <> root) `BS.isSuffixOf` host

parseHttpsOriginHost :: BS.ByteString -> Maybe BS.ByteString
parseHttpsOriginHost origin = do
  remainder <- BS.stripPrefix "https://" (BS.map toLower origin)
  let (host, suffix) = BS.break (`elem` (":/?#" :: String)) remainder
  if BS.null host || not (validOriginHost host) || not (BS.null suffix)
    then Nothing
    else Just host

normalizeConfiguredCorsOrigin :: String -> Either String String
normalizeConfiguredCorsOrigin raw =
  let normalized = normalizeOrigin raw
  in case normalized of
    "*" -> Right "*"
    _ ->
      case parseHttpOrigin normalized of
        Just origin -> Right origin
        Nothing ->
          Left $
            "Configured CORS origins must be absolute http(s) origins "
              <> "without path, query, or fragment: "
              <> raw

parseHttpOrigin :: String -> Maybe String
parseHttpOrigin origin =
  let lowered = BS.map toLower (BS.pack origin)
  in case BS.stripPrefix "https://" lowered of
    Just remainder -> parseWithScheme "https://" remainder
    Nothing ->
      case BS.stripPrefix "http://" lowered of
        Just remainder -> parseWithScheme "http://" remainder
        Nothing -> Nothing
  where
    parseWithScheme scheme remainder =
      let (host, suffix) = BS.break (`elem` (":/?#" :: String)) remainder
      in if BS.null host || not (validOriginHost host) || not (validOriginSuffix suffix)
        then Nothing
        else Just (BS.unpack (scheme <> host <> suffix))

-- | Convert a configured app base URL into the origin shape required by CORS.
deriveCorsOriginFromAppBase :: String -> Either String String
deriveCorsOriginFromAppBase raw =
  case parseHttpBaseOrigin (trim raw) of
    Just origin -> Right origin
    Nothing ->
      Left $
        "HQ_APP_URL CORS fallback must be an absolute http(s) URL "
          <> "with a valid origin and no query or fragment: "
          <> raw

parseHttpBaseOrigin :: String -> Maybe String
parseHttpBaseOrigin raw
  | null raw || any isSpace raw = Nothing
  | otherwise =
      let lowered = BS.map toLower (BS.pack raw)
      in case BS.stripPrefix "https://" lowered of
        Just remainder -> parseWithScheme "https://" remainder
        Nothing ->
          case BS.stripPrefix "http://" lowered of
            Just remainder -> parseWithScheme "http://" remainder
            Nothing -> Nothing
  where
    parseWithScheme scheme remainder =
      let (authority, suffix) = BS.break (`elem` ("/?#" :: String)) remainder
          origin = BS.unpack (scheme <> authority)
      in if BS.null authority || not (validBaseSuffix suffix)
        then Nothing
        else parseHttpOrigin origin

    validBaseSuffix suffix =
      BS.null suffix
        || ("/" `BS.isPrefixOf` suffix && not (BS.any (\c -> c == '?' || c == '#') suffix))

validOriginHost :: BS.ByteString -> Bool
validOriginHost host =
  BS.length host <= 253
    && not (isAmbiguousNumericHost host)
    && all validLabel (BS.split '.' host)
  where
    isAmbiguousNumericHost candidate =
      BS.all (\ch -> isDigit ch || ch == '.') candidate
        && isNothing (parseIpv4Octets candidate)

    parseIpv4Octets candidate =
      case BS.split '.' candidate of
        [a, b, c, d] -> do
          oa <- parseOctet a
          ob <- parseOctet b
          oc <- parseOctet c
          od <- parseOctet d
          pure (oa, ob, oc, od)
        _ -> Nothing

    parseOctet octet
      | BS.null octet || BS.any (not . isDigit) octet = Nothing
      | BS.length octet > 1 && BS.head octet == '0' = Nothing
      | otherwise = do
          value <- readMaybe (BS.unpack octet)
          if value >= (0 :: Int) && value <= 255
            then Just value
            else Nothing

    validLabel label =
      not (BS.null label)
        && BS.length label <= 63
        && not ("-" `BS.isPrefixOf` label)
        && not ("-" `BS.isSuffixOf` label)
        && BS.all isHostnameChar label
    isHostnameChar c =
      (c >= 'a' && c <= 'z') || isDigit c || c == '-'

validOriginSuffix :: BS.ByteString -> Bool
validOriginSuffix suffix
  | BS.null suffix = True
  | ":" `BS.isPrefixOf` suffix =
      let port = BS.drop 1 suffix
      in not (BS.null port)
        && BS.all isDigit port
        && maybe False (\portNumber -> portNumber >= (1 :: Int) && portNumber <= 65535)
            (readMaybe (BS.unpack port))
  | otherwise = False

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

-- | Remove surrounding whitespace and trailing slashes to avoid origin mismatches.
normalizeOrigin :: String -> String
normalizeOrigin = dropTrailingSlash . trim
  where
    dropTrailingSlash = dropWhileEnd (== '/')

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseBoolFlag :: String -> String -> Either String Bool
parseBoolFlag name raw =
  case map toLower (trim raw) of
    "1"     -> Right True
    "true"  -> Right True
    "yes"   -> Right True
    "on"    -> Right True
    "*"     -> Right True
    "0"     -> Right False
    "false" -> Right False
    "no"    -> Right False
    "off"   -> Right False
    value ->
      Left $
        name
          <> " must be a boolean CORS flag (true/false, 1/0, yes/no, on/off), got: "
          <> value

lookupFirstNonEmptyEnv :: [String] -> IO (Maybe String)
lookupFirstNonEmptyEnv [] = pure Nothing
lookupFirstNonEmptyEnv (key:rest) = do
  value <- lookupEnv key
  case value of
    Just raw | not (null (trim raw)) -> pure (Just raw)
    _ -> lookupFirstNonEmptyEnv rest
