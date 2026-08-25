{-# LANGUAGE OverloadedStrings #-}
import Database.Persist.Sql
import TDF.Models
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  let sqls = showMigration migrateAll
  mapM_ (putStrLn . T.unpack) sqls
