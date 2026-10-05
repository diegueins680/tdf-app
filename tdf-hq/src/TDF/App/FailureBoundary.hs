{-# LANGUAGE OverloadedStrings #-}
module TDF.App.FailureBoundary
  ( requestExceptionBoundary, reportUnhandledException, internalErrorResponse
  , bestEffortActivity, fixedFailure
  ) where

import Control.Exception (SomeException, SomeAsyncException, fromException, throwIO)
import qualified Control.Exception.Safe as Safe
import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Text (Text)
import Network.HTTP.Types (status500)
import Network.Wai (Middleware, Response, responseLBS)

internalErrorResponse :: Response
internalErrorResponse = responseLBS status500
  [("Content-Type", "text/plain; charset=utf-8")] "Internal server error"

-- Never render an exception or request: these can contain SQL values, bearer
-- credentials or private URLs. Logging failure must not replace the response.
reportUnhandledException :: (Text -> IO ()) -> SomeException -> IO ()
reportUnhandledException logger exception =
  case fromException exception :: Maybe SomeAsyncException of
    Just _ -> pure ()
    Nothing -> void $ Safe.tryAny (logger "[HTTP] Unhandled request failure")

requestExceptionBoundary :: (Text -> IO ()) -> Middleware
requestExceptionBoundary logger next request send = do
  responseStarted <- newIORef False
  let sendOnce response = do
        alreadyStarted <- atomicModifyIORef' responseStarted (\started -> (True, started))
        when alreadyStarted (throwIO (userError "Response delivery already started"))
        send response
      failed exception = do
        started <- readIORef responseStarted
        if started then throwIO exception else do
          reportUnhandledException logger exception
          sendOnce internalErrorResponse
  Safe.handleAny failed (next request sendOnce)

bestEffortActivity :: (Text -> IO ()) -> IO () -> IO ()
bestEffortActivity logger action = Safe.handleAny
  (\_ -> void $ Safe.tryAny (logger "[Auth][Activity] Audit persistence failed")) action

-- Labels are static identifiers selected by the caller, never exception text.
fixedFailure :: Text -> IO a -> IO (Either Text a)
fixedFailure label action = either (const (Left label)) Right <$> Safe.tryAny action
