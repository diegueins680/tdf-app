{-# LANGUAGE OverloadedStrings #-}
module TDF.Cors (corsPolicy) where
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isSpace)

corsPolicy :: IO Middleware
corsPolicy = do
  origins <- fmap (maybe ["http://localhost:5173","https://tdfui.pages.dev","https://tdf-app.pages.dev"] splitComma) (lookupEnv "ALLOWED_ORIGINS")
  let policy = simpleCorsResourcePolicy
        { corsOrigins        = Just (map BS.pack origins, True)
        , corsRequestHeaders = "authorization":"content-type":simpleHeaders
        , corsMethods        = "GET":"POST":"PATCH":"OPTIONS":simpleMethods
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
