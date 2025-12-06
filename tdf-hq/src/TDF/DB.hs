{-# LANGUAGE OverloadedStrings #-}
module TDF.DB
  ( Env(..)
  , ConnectionPool
  , makePool
  , runExtraMigrations
  ) where

import           Control.Monad.Logger (runStdoutLoggingT)
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql (SqlPersistT, runMigration)
import qualified Database.Persist.Sql as Sql
import           Data.ByteString (ByteString)

import           TDF.Config hiding (runMigrations)
import           TDF.Trials.Models (migrateTrials)
import           TDF.CMS.Models (migrateCMS)
import           TDF.Calendar.Models (migrateCalendar)

data Env = Env
  { envPool   :: ConnectionPool
  , envConfig :: AppConfig
  }

type ConnectionPool = Sql.ConnectionPool

makePool :: ByteString -> IO ConnectionPool
makePool conn = runStdoutLoggingT $ createPostgresqlPool conn 10

runExtraMigrations :: SqlPersistT IO () -> SqlPersistT IO ()
runExtraMigrations base = base >> runMigration migrateTrials >> runMigration migrateCMS >> runMigration migrateCalendar
