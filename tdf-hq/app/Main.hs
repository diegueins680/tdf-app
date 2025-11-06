{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.IO.Class (liftIO)

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
        putStrLn "  GET  /health              - Health check"
        putStrLn "  GET  /version             - Backend version metadata"
        putStrLn "  GET  /parties             - List parties"
        putStrLn "  POST /parties             - Create party"
        putStrLn "  GET  /parties/:id         - Get party"
        putStrLn "  PUT  /parties/:id         - Update party"
        putStrLn "  POST /parties/:id/roles   - Add role to party"
        putStrLn "  GET  /bookings            - List bookings"
        putStrLn "  POST /bookings            - Create booking"
        putStrLn "  GET  /api/users           - List all users with roles"
        putStrLn "  GET  /api/users/:id/roles - Get roles for a user"
        putStrLn "  PUT  /api/users/:id/roles - Update roles for a user"
        
        -- Start the server
        run 8080 $ serve api $ server pool
