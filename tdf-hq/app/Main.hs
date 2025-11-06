{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)

import TDF.API
import TDF.Server

-- Database connection string
connStr :: ConnectionString
connStr = "host=localhost dbname=tdf_hq user=postgres password=postgres port=5432"

main :: IO ()
main = runStdoutLoggingT $ do
    -- Create database connection pool
    pool <- createPostgresqlPool connStr 10
    
    liftIO $ do
        putStrLn "TDF HQ API Server starting on port 8080..."
        putStrLn "API endpoints:"
        putStrLn "  GET  /api/users           - List all users with roles"
        putStrLn "  GET  /api/users/:id/roles - Get roles for a user"
        putStrLn "  PUT  /api/users/:id/roles - Update roles for a user"
        
        -- Start the server
        run 8080 $ serve api $ server pool
