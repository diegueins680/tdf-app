{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.Observability
  ( -- * Metrics
    DdexMetrics(..)
  , emptyMetrics
  , recordDocumentUpload
  , recordValidation
  , recordImport
  , recordExport
    -- * Logging
  , DdexLogEvent(..)
  , DdexLogLevel(..)
  , logDdexEvent
    -- * Audit Trail
  , AuditEntry(..)
  , recordAuditEntry
  , getAuditTrail
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')

-- | DDEX operation metrics
data DdexMetrics = DdexMetrics
  { mDocumentsUploaded   :: !Int
  , mDocumentsValidated  :: !Int
  , mDocumentsImported   :: !Int
  , mDocumentsExported   :: !Int
  , mValidationErrors    :: !Int
  , mValidationWarnings  :: !Int
  , mImportConflicts     :: !Int
  , mImportEntitiesCreated :: !Int
  , mImportEntitiesUpdated :: !Int
  , mExportGenerations   :: !Int
  , mLastError           :: Maybe Text
  } deriving (Show, Eq)

-- | Empty metrics
emptyMetrics :: DdexMetrics
emptyMetrics = DdexMetrics
  { mDocumentsUploaded = 0
  , mDocumentsValidated = 0
  , mDocumentsImported = 0
  , mDocumentsExported = 0
  , mValidationErrors = 0
  , mValidationWarnings = 0
  , mImportConflicts = 0
  , mImportEntitiesCreated = 0
  , mImportEntitiesUpdated = 0
  , mExportGenerations = 0
  , mLastError = Nothing
  }

-- | Record document upload
recordDocumentUpload :: IORef DdexMetrics -> IO ()
recordDocumentUpload ref = modifyIORef' ref $ \m ->
  m { mDocumentsUploaded = mDocumentsUploaded m + 1 }

-- | Record validation result
recordValidation :: IORef DdexMetrics -> Int -> Int -> IO ()
recordValidation ref errors warnings = modifyIORef' ref $ \m ->
  m { mDocumentsValidated = mDocumentsValidated m + 1
    , mValidationErrors = mValidationErrors m + errors
    , mValidationWarnings = mValidationWarnings m + warnings
    }

-- | Record import result
recordImport :: IORef DdexMetrics -> Int -> Int -> Int -> IO ()
recordImport ref conflicts created updated = modifyIORef' ref $ \m ->
  m { mDocumentsImported = mDocumentsImported m + 1
    , mImportConflicts = mImportConflicts m + conflicts
    , mImportEntitiesCreated = mImportEntitiesCreated m + created
    , mImportEntitiesUpdated = mImportEntitiesUpdated m + updated
    }

-- | Record export generation
recordExport :: IORef DdexMetrics -> IO ()
recordExport ref = modifyIORef' ref $ \m ->
  m { mDocumentsExported = mDocumentsExported m + 1
    , mExportGenerations = mExportGenerations m + 1
    }

-- | Log level for DDEX events
data DdexLogLevel
  = LogDebug
  | LogInfo
  | LogWarning
  | LogError
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | DDEX log event
data DdexLogEvent = DdexLogEvent
  { leTimestamp  :: UTCTime
  , leLevel      :: DdexLogLevel
  , leDocumentId :: Maybe Int64
  , leEventType  :: Text
  , leMessage    :: Text
  , leDetails    :: Maybe Text
  } deriving (Show, Eq)

-- | Log a DDEX event
logDdexEvent :: DdexLogLevel -> Maybe Int64 -> Text -> Text -> IO DdexLogEvent
logDdexEvent level docId eventType message = do
  now <- getCurrentTime
  return DdexLogEvent
    { leTimestamp = now
    , leLevel = level
    , leDocumentId = docId
    , leEventType = eventType
    , leMessage = message
    , leDetails = Nothing
    }

-- | Audit trail entry
data AuditEntry = AuditEntry
  { aeTimestamp  :: UTCTime
  , aeUserId     :: Int64
  , aeAction     :: Text
  , aeEntityType :: Text
  , aeEntityId   :: Maybe Int64
  , aeDetails    :: Maybe Text
  , aeIpAddress  :: Maybe Text
  } deriving (Show, Eq)

-- | Record an audit entry
-- TODO: Implement actual database storage
recordAuditEntry :: AuditEntry -> IO ()
recordAuditEntry _entry = do
  -- In production, this would insert into an audit_log table
  return ()

-- | Get audit trail for an entity
-- TODO: Implement actual database query
getAuditTrail :: Text -> Int64 -> IO [AuditEntry]
getAuditTrail _entityType _entityId = do
  -- In production, this would query the audit_log table
  return []
