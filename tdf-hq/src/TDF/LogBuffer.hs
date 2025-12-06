{-# LANGUAGE OverloadedStrings #-}
module TDF.LogBuffer
  ( LogEntry(..)
  , LogLevel(..)
  , addLog
  , getRecentLogs
  , clearLogs
  ) where

import           Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import           Data.Text (Text)
import           Data.Time (UTCTime, getCurrentTime)
import           Data.List (sortOn)
import           Data.Ord (Down(..))
import           System.IO.Unsafe (unsafePerformIO)

data LogLevel
  = LogInfo
  | LogWarning
  | LogError
  deriving (Eq, Ord, Show)

data LogEntry = LogEntry
  { logTimestamp :: UTCTime
  , logLevel     :: LogLevel
  , logMessage   :: Text
  } deriving (Show, Eq)

instance Ord LogEntry where
  compare a b = compare (logTimestamp a) (logTimestamp b)

-- In-memory log buffer (max 1000 entries)
maxLogEntries :: Int
maxLogEntries = 1000

logBufferRef :: IORef [LogEntry]
logBufferRef = unsafePerformIO $ newIORef []

-- Add a log entry
addLog :: LogLevel -> Text -> IO ()
addLog level message = do
  timestamp <- getCurrentTime
  let entry = LogEntry { logTimestamp = timestamp, logLevel = level, logMessage = message }
  modifyIORef' logBufferRef $ \logs ->
    let newLogs = entry : logs
        -- Keep only the most recent maxLogEntries
        limited = take maxLogEntries newLogs
    in limited

-- Get recent logs (most recent first)
getRecentLogs :: Int -> IO [LogEntry]
getRecentLogs limit = do
  logs <- readIORef logBufferRef
  return $ take limit $ sortOn (Down . logTimestamp) logs

-- Clear all logs
clearLogs :: IO ()
clearLogs = writeIORef logBufferRef []

