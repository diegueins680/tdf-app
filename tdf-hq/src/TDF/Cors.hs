{-# LANGUAGE OverloadedStrings #-}
module TDF.Cors (corsPolicy) where
import Network.Wai (Middleware, Request, requestHeaders)
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace, toLower)
import Control.Applicative ((<|>))
import Data.List (dropWhileEnd, intercalate, nub)

corsPolicy :: IO Middleware
corsPolicy = do
  originsEnv <- lookupEnv "ALLOWED_ORIGINS"
            <|> lookupEnv "ALLOW_ORIGINS"
            <|> lookupEnv "ALLOW_ORIGIN"
            <|> lookupEnv "CORS_ALLOW_ORIGINS"
            <|> lookupEnv "CORS_ALLOW_ORIGIN"
  allowAllEnv <- lookupEnv "ALLOW_ALL_ORIGINS"
             <|> lookupEnv "CORS_ALLOW_ALL_ORIGINS"
  disableDefaultsEnv <- lookupEnv "CORS_DISABLE_DEFAULTS"
                    <|> lookupEnv "DISABLE_DEFAULT_CORS"
  let defaults =
        [ "http://localhost:5173"
        , "http://127.0.0.1:5173"
        , "https://tdfui.pages.dev"
        , "https://tdf-app.pages.dev"
        ]
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
      originSetting =
        if wildcard
          then Nothing
          else Just (map BS.pack effective, True)
      basePolicy = simpleCorsResourcePolicy
        { corsOrigins            = originSetting
        , corsRequestHeaders     = "authorization":"content-type":"x-requested-with":simpleHeaders
        , corsMethods            = ["GET","POST","PUT","PATCH","DELETE","OPTIONS"]
        , corsRequireOrigin      = False
        , corsIgnoreFailures     = False
        }
      allowPagesDevWildcard = True
      allowAllPolicy = basePolicy { corsOrigins = Nothing }
      pagesHost origin =
        let lowered = BS.map toLower origin
        in ".pages.dev" `BS.isSuffixOf` lowered || ".vercel.app" `BS.isSuffixOf` lowered
      choosePolicy :: Request -> Maybe CorsResourcePolicy
      choosePolicy req =
        if allowAll
          then Just allowAllPolicy
          else
            case lookup "origin" (requestHeaders req) of
              Nothing -> Just allowAllPolicy -- allow health/public probes without CORS failures
              Just o | allowPagesDevWildcard && pagesHost o -> Just allowAllPolicy
              _ -> Just basePolicy
      originLog =
        if wildcard
          then "*"
          else if null effective then "(none)" else intercalate "," effective
      notes =
        (if includeDefaults then "" else " defaults=off")
          <> (if allowAll then " allowAll=true" else "")
          <> (if null deduped && includeDefaults && not allowAll then " (fallback to defaults)" else "")
  putStrLn $ "[cors] origins=" <> originLog <> notes
  putStrLn $ "[cors] pages.dev wildcard=" <> show allowPagesDevWildcard
  pure (cors choosePolicy)

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
