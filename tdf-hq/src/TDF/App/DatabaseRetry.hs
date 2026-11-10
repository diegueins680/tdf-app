module TDF.App.DatabaseRetry (retryDatabaseConnection) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import qualified Control.Exception.Safe as Safe

-- Cancellation must escape the retry boundary so shutdown can join startup.
retryDatabaseConnection :: Int -> IO a -> IO a
retryDatabaseConnection retries connect = do
  result <- Safe.tryAny connect
  case result of
    Right pool -> pure pool
    Left err ->
      if retries <= 0
        then do
          putStrLn "Failed to connect to database after retries. Crashing."
          throwIO err
        else do
          putStrLn $ "DB connection failed, retrying... attempts left: " <> show retries
          threadDelay (5 * 1000 * 1000)
          retryDatabaseConnection (retries - 1) connect
