-- | Startup retries may recover synchronous failures, never process cancellation.
module TDF.App.StartupRetry (retryStartup) where

import Control.Exception (SomeException, throwIO)
import qualified Control.Exception.Safe as Safe

retryStartup :: Int -> (Int -> IO ()) -> IO a -> IO a
retryStartup retries beforeRetry operation = do
  result <- Safe.tryAny operation
  case result of
    Right value -> pure value
    Left err
      | retries <= 0 -> throwIO (err :: SomeException)
      | otherwise -> do
          beforeRetry retries
          retryStartup (retries - 1) beforeRetry operation
