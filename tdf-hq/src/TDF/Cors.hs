{-# LANGUAGE OverloadedStrings #-}
module TDF.Cors
  ( corsPolicy
  , isTrustedPreviewOrigin
  , lookupFirstNonEmptyEnv
  ) where
import Network.Wai (Middleware, Request, requestHeaders)
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit, isSpace, toLower)
import Data.List (dropWhileEnd, intercalate, nub)
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
  let defaultsCore =
        [ "http://localhost:5173"
        , "http://127.0.0.1:5173"
        , "https://tdfui.pages.dev"
        ]
      defaults =
        defaultsCore ++ maybe [] (\raw -> [normalizeOrigin raw]) hqBaseEnv
      parsed = maybe [] splitComma originsEnv
      normalized = map normalizeOrigin parsed
      filtered = filter (not . null) normalized
      includeDefaults = not (maybe False parseBool disableDefaultsEnv)
      merged = (if includeDefaults then defaults else []) ++ filtered
      deduped = nub merged
      allowAll = maybe False parseBool allowAllEnv || any (== "*") deduped
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
  if BS.null host || not (validOriginSuffix suffix)
    then Nothing
    else Just host

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

parseBool :: String -> Bool
parseBool raw =
  case map toLower (trim raw) of
    "1"      -> True
    "true"   -> True
    "yes"    -> True
    "on"     -> True
    "*"      -> True
    _        -> False

lookupFirstNonEmptyEnv :: [String] -> IO (Maybe String)
lookupFirstNonEmptyEnv [] = pure Nothing
lookupFirstNonEmptyEnv (key:rest) = do
  value <- lookupEnv key
  case value of
    Just raw | not (null (trim raw)) -> pure (Just raw)
    _ -> lookupFirstNonEmptyEnv rest
