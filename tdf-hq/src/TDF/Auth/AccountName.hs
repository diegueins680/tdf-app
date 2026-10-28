{-# LANGUAGE OverloadedStrings #-}

module TDF.Auth.AccountName (initialAccountName) where

import Data.Text (Text)
import qualified Data.Text as T

-- A provider may supply a name; never substitute its private email when it
-- does not. Keep credential/contact data out of this default-name API.
initialAccountName :: Maybe Text -> Text
initialAccountName supplied =
  case T.strip <$> supplied of
    Just name | not (T.null name) -> name
    _ -> "Cuenta TDF"
