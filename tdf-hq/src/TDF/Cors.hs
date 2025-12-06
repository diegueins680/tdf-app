{-# LANGUAGE OverloadedStrings #-}
module TDF.Cors (corsPolicy) where
import Network.Wai (Middleware)
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
             <|> lookupEnv "ALLOW_ORIGINS_ALL"
             <|> lookupEnv "CORS_ALLOW_ALL"
  let defaults =
        [ "http://localhost:5173"
        , "http://127.0.0.1:5173"
        , "https://tdfui.pages.dev"
        , "https://tdf-app.pages.dev"
        ]
      parsed = maybe [] splitComma originsEnv
      normalized = map normalizeOrigin parsed
      filtered = filter (not . null) normalized
      origins = nub (if null filtered then defaults else filtered)
      wildcard = any (== "*") origins
      -- Default to allow all to keep the public web client working even if env vars are missing.
      allowAll = wildcard || maybe True asBool allowAllEnv
      originSetting =
        if allowAll
          then Nothing
          else Just (map BS.pack origins, True)
      policy = simpleCorsResourcePolicy
        { corsOrigins            = originSetting
        , corsRequestHeaders     = "authorization":"content-type":"x-requested-with":simpleHeaders
        , corsMethods            = ["GET","POST","PUT","PATCH","DELETE","OPTIONS"]
        , corsRequireOrigin      = False
        , corsIgnoreFailures     = False
        }
  putStrLn $ "[cors] allowAll=" <> show allowAll
          <> " origins=" <> if allowAll then "*" else intercalate "," origins
  pure (cors (const (Just policy)))

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

asBool :: String -> Bool
asBool v =
  case map toLower (dropWhile isSpace v) of
    "true"  -> True
    "1"     -> True
    "yes"   -> True
    "on"    -> True
    _       -> False
