{-# LANGUAGE OverloadedStrings #-}
module TDF.Cors (corsPolicy) where
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace, toLower)

corsPolicy :: IO Middleware
corsPolicy = do
  originsEnv <- lookupEnv "ALLOWED_ORIGINS"
  let defaults =
        [ "http://localhost:5173"
        , "http://127.0.0.1:5173"
        , "https://tdfui.pages.dev"
        , "https://tdf-app.pages.dev"
        ]
      origins  = maybe defaults splitComma originsEnv
      originSetting = Just (map BS.pack origins, True)
      policy = simpleCorsResourcePolicy
        { corsOrigins            = originSetting
        , corsRequestHeaders     = "authorization":"content-type":"x-requested-with":simpleHeaders
        , corsMethods            = ["GET","POST","PUT","PATCH","DELETE","OPTIONS"]
        , corsRequireOrigin      = False
        , corsIgnoreFailures     = False
        }
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
    trim = dropWhileEnd isSpace . dropWhile isSpace
    dropWhileEnd p = reverse . dropWhile p . reverse

asBool :: String -> Bool
asBool v =
  case map toLower (dropWhile isSpace v) of
    "true"  -> True
    "1"     -> True
    "yes"   -> True
    "on"    -> True
    _       -> False
