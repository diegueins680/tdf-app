module Main where

import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as ByteString
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (runSqlPool)
import System.Environment (getEnv)
import TDF.Catalog.Seed (seedCatalogFoundation, validateCatalogRuntimeRegistries)

main :: IO ()
main = do
  databaseUrl <- ByteString.pack <$> getEnv "DATABASE_URL"
  pool <- runNoLoggingT $ createPostgresqlPool databaseUrl 1
  runSqlPool
    (seedCatalogFoundation >> validateCatalogRuntimeRegistries)
    pool
