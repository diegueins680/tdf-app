{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DDEX.Jobs
  ( -- * Job queue operations
    createValidationJob
  , createImportJob
  , createExportJob
    -- * Job status
  , DdexJobType(..)
  , DdexJobStatus(..)
    -- * Worker configuration
  , WorkerConfig(..)
  , defaultWorkerConfig
  ) where

import Data.Text (Text)
import Data.Int (Int64)
import Data.Time (UTCTime)
import TDF.DDEX.Types

-- | Configuration for the background job worker
data WorkerConfig = WorkerConfig
  { workerPollIntervalSeconds :: Int   -- ^ How often to check for new jobs
  , workerLeaseSeconds        :: Int   -- ^ How long to lease a job
  , workerMaxRetries          :: Int   -- ^ Maximum retry attempts
  , workerConcurrency         :: Int   -- ^ Number of concurrent workers
  } deriving (Show, Eq)

-- | Default worker configuration
defaultWorkerConfig :: WorkerConfig
defaultWorkerConfig = WorkerConfig
  { workerPollIntervalSeconds = 5
  , workerLeaseSeconds = 300  -- 5 minutes
  , workerMaxRetries = 3
  , workerConcurrency = 2
  }

-- | Create a validation job for a DDEX document
-- Returns the job ID
createValidationJob :: Int64 -> IO (Maybe Int64)
createValidationJob documentId = do
  -- TODO: Insert into ddex_job table with job_type = 'Validate'
  -- For now, return Nothing as placeholder
  pure Nothing

-- | Create an import job for a validated document
createImportJob :: Int64 -> Int64 -> IO (Maybe Int64)
createImportJob documentId planId = do
  -- TODO: Insert into ddex_job table with job_type = 'Import'
  pure Nothing

-- | Create an export job for a catalog release
createExportJob :: Int64 -> Int64 -> IO (Maybe Int64)
createExportJob releaseId partnerId = do
  -- TODO: Insert into ddex_job table with job_type = 'Export'
  pure Nothing

-- Note: The actual job worker implementation would:
-- 1. Poll ddex_job for pending jobs (FOR UPDATE SKIP LOCKED)
-- 2. Claim jobs by setting status = 'Processing' and leased_until
-- 3. Dispatch to appropriate handler based on job_type
-- 4. On success: set status = 'Completed'
-- 5. On failure: increment attempts, set status = 'Retry' or 'Failed'
-- 6. Heartbeat: update leased_until periodically for long-running jobs
--
-- This would be started in App/Boot.hs alongside the main server.
