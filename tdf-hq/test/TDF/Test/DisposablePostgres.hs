module TDF.Test.DisposablePostgres (isDisposablePostgresUrl) where

import Data.List (isSuffixOf)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Text.Read (readMaybe)

-- Validate the parsed destination, never an authority-shaped string prefix.
-- Keep this grammar aligned with scripts/lib/disposable-postgres-url.mjs.
isDisposablePostgresUrl :: Bool -> String -> Bool
isDisposablePostgresUrl ci value = case parseURI value of
  Just uri
    | uriScheme uri `elem` ["postgres:", "postgresql:"]
    , null (uriQuery uri), null (uriFragment uri)
    , Just authority <- uriAuthority uri
    , uriRegName authority `elem` (["127.0.0.1", "localhost"] ++ ["postgres" | ci])
    , validPort (uriPort authority)
    , '/' : database <- uriPath uri ->
        "_test" `isSuffixOf` database && length database > length ("_test" :: String)
          && all (`elem` (['_', '-'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])) database
  _ -> False
 where
  validPort "" = True
  validPort ":" = True
  validPort (':' : digits)
    | all (`elem` ['0' .. '9']) digits =
        maybe False (\n -> n > 0 && n <= 65535) (readMaybe digits :: Maybe Int)
  validPort _ = False
