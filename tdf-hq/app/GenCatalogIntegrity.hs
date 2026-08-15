{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TextIO
import TDF.Catalog.Integrity (catalogIntegrityStatements)

main :: IO ()
main = do
  TextIO.putStrLn "\\set ON_ERROR_STOP on"
  TextIO.putStrLn ""
  TextIO.putStrLn "BEGIN;"
  TextIO.putStrLn "SET LOCAL statement_timeout = '15min';"
  TextIO.putStrLn "SET LOCAL lock_timeout = '2s';"
  TextIO.putStrLn "SELECT pg_advisory_xact_lock(hashtextextended('tdf-catalog-integrity-v1', 0));"
  mapM_ (TextIO.putStrLn . (<> ";")) catalogIntegrityStatements
  TextIO.putStrLn "COMMIT;"
