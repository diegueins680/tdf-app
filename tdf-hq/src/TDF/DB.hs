{-# LANGUAGE OverloadedStrings #-}
module TDF.DB where

import           Control.Monad.Logger (runStdoutLoggingT)
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql (SqlPersistT, ConnectionPool, runMigration)
import           Data.ByteString (ByteString)

import           TDF.Config
import           TDF.Trials.Models (migrateTrials)
import           TDF.CMS.Models (migrateCMS)
import           TDF.Calendar.Models (migrateCalendar)

data Env = Env
  { envPool   :: ConnectionPool
  , envConfig :: AppConfig
  }

makePool :: ByteString -> IO ConnectionPool
makePool conn = runStdoutLoggingT $ createPostgresqlPool conn 10

runMigrations :: SqlPersistT IO () -> SqlPersistT IO ()
runMigrations base = base >> runMigration migrateTrials >> runMigration migrateCMS >> runMigration migrateCalendar
