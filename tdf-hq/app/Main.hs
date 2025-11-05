{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import TDF.DB (createPool, runMigrations)
import TDF.Server (app)
import TDF.OpenAPI (apiSpec)
import Data.OpenApi.Schema (ToSchema)
import Data.Text (pack)
import qualified Data.Text as T

main :: IO ()
main = do
  -- Get database connection string from environment or use default
  dbHost <- lookupEnv "DB_HOST"
  dbPort <- lookupEnv "DB_PORT"
  dbName <- lookupEnv "DB_NAME"
  dbUser <- lookupEnv "DB_USER"
  dbPass <- lookupEnv "DB_PASSWORD"
  
  let connStr = pack $ "host=" ++ maybe "127.0.0.1" id dbHost
              ++ " port=" ++ maybe "5432" id dbPort
              ++ " user=" ++ maybe "tdf" id dbUser
              ++ " password=" ++ maybe "tdf" id dbPass
              ++ " dbname=" ++ maybe "tdfhq" id dbName
  
  putStrLn "========================================"
  putStrLn "  TDF Records API - User Management"
  putStrLn "========================================"
  putStrLn ""
  putStrLn $ "Database: " ++ T.unpack connStr
  putStrLn ""
  
  -- Create connection pool and run migrations
  pool <- createPool connStr
  putStrLn "[1/3] Running database migrations..."
  runMigrations pool
  putStrLn "      ✓ Migrations completed"
  putStrLn ""
  
  -- Generate OpenAPI spec
  putStrLn "[2/3] Generating OpenAPI specification..."
  BL.writeFile "openapi.json" (encodePretty apiSpec)
  putStrLn "      ✓ OpenAPI spec saved to: openapi.json"
  putStrLn ""
  
  -- Start server
  let port = 8080
  putStrLn "[3/3] Starting API server..."
  putStrLn $ "      ✓ Server running on http://localhost:" ++ show port
  putStrLn ""
  putStrLn "API Endpoints:"
  putStrLn "  GET  /api/users          - List all users"
  putStrLn "  PUT  /api/users/:id/role - Update user role"
  putStrLn ""
  putStrLn "========================================"
  run port (app pool)
