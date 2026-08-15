module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (runSqlPool, showMigration)
import System.Environment (getEnv)
import qualified TDF.Catalog.Models as Catalog

main :: IO ()
main = do
  databaseUrl <- ByteString.pack <$> getEnv "DATABASE_URL"
  runNoLoggingT $ withPostgresqlPool databaseUrl 1 $ \pool -> do
    statements <- runSqlPool
      ( concat <$> mapM
          showMigration
          [ Catalog.migrateCatalogGovernance
          , Catalog.migrateCatalogSecurity
          , Catalog.migrateCatalogReferences
          , Catalog.migrateCatalogDomains
          ]
      )
      pool
    liftIO $ mapM_ (putStrLn . Text.unpack) statements
