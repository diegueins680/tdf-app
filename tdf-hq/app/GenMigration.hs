module Main where

import Database.Persist.Sql
import TDF.Models
import qualified Data.Text as T

main :: IO ()
main = do
  let sqls = showMigration migrateAll
  mapM_ (putStrLn . T.unpack) sqls
