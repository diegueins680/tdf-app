{-# LANGUAGE OverloadedStrings #-}
module TDF.DB
  ( Env(..)
  , ConnectionPool
  , makePool
  , runExtraMigrations
  , sharedTlsManager
  ) where

import           Control.Monad.Logger (runStdoutLoggingT)
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql (SqlPersistT, runMigration)
import qualified Database.Persist.Sql as Sql
import           Data.ByteString (ByteString)
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.IO.Unsafe (unsafePerformIO)

import           TDF.Config hiding (runMigrations)
import           TDF.Trials.Models (migrateTrials)
import           TDF.CMS.Models (migrateCMS)
import           TDF.Calendar.Models (migrateCalendar)

data Env = Env
  { envPool   :: ConnectionPool
  , envConfig :: AppConfig
  }

type ConnectionPool = Sql.ConnectionPool

-- | Shared TLS connection manager for all outbound HTTPS requests.
-- Creating a new 'Manager' per request defeats connection pooling and
-- leaks file descriptors under load.  This top-level value is created
-- once on first use and shared for the lifetime of the process.
sharedTlsManager :: Manager
sharedTlsManager = unsafePerformIO (newManager tlsManagerSettings)
{-# NOINLINE sharedTlsManager #-}

makePool :: ByteString -> IO ConnectionPool
makePool conn = runStdoutLoggingT $ createPostgresqlPool conn 10

runExtraMigrations :: SqlPersistT IO () -> SqlPersistT IO ()
runExtraMigrations base = base >> runMigration migrateTrials >> runMigration migrateCMS >> runMigration migrateCalendar
