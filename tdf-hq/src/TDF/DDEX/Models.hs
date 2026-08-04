{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.DDEX.Models where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH
import GHC.Generics (Generic)

-- Document status enum
data DdexDocumentStatusEnum
  = StatusReceived
  | StatusQuarantined
  | StatusQueued
  | StatusValidating
  | StatusInvalid
  | StatusValid
  | StatusMappingRequired
  | StatusReadyToImport
  | StatusImporting
  | StatusImported
  | StatusImportFailed
  | StatusSuperseded
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "DdexDocumentStatusEnum"

-- DDEX family enum
data DdexFamilyEnum
  = FamilyERN
  | FamilyRIN
  | FamilyDSR
  | FamilyMEAD
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "DdexFamilyEnum"

-- Job type enum
data DdexJobTypeEnum
  = JobValidate
  | JobImport
  | JobExport
  | JobCleanup
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "DdexJobTypeEnum"

-- Job status enum
data DdexJobStatusEnum
  = JobPending
  | JobProcessing
  | JobCompleted
  | JobFailed
  | JobRetry
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "DdexJobStatusEnum"

-- Import plan status enum
data ImportPlanStatusEnum
  = PlanDraft
  | PlanResolved
  | PlanCommitted
  | PlanAbandoned
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "ImportPlanStatusEnum"

-- Validation result enum
data ValidationResultEnum
  = ResultSuccess
  | ResultFailure
  | ResultWarning
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "ValidationResultEnum"

-- Validation severity enum
data ValidationSeverityEnum
  = SeverityError
  | SeverityWarning
  | SeverityInfo
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "ValidationSeverityEnum"

-- Validation layer enum
data ValidationLayerEnum
  = LayerXML
  | LayerXSD
  | LayerAVS
  | LayerBusiness
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "ValidationLayerEnum"

-- Import operation enum
data ImportOperationEnum
  = OpCreate
  | OpUpdate
  | OpSkip
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "ImportOperationEnum"

-- Import run status enum
data ImportRunStatusEnum
  = RunPending
  | RunRunning
  | RunSuccess
  | RunFailed
  | RunRolledBack
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "ImportRunStatusEnum"

share [mkPersist sqlSettings, mkMigrate "migrateDdex"] [persistLowerCase|

-- DDEX Documents
DdexDocument
  fileName Text
  privateUri Text
  sha256 Text
  sizeBytes Int
  family DdexFamilyEnum
  version Text
  namespace Text Maybe
  messageType Text Maybe
  status DdexDocumentStatusEnum
  uploadedBy Int  -- FK to app_user
  messageId Text Maybe
  senderId Text Maybe
  recipientId Text Maybe
  createdAt UTCTime
  UniqueDdexDocumentSha256 sha256
  deriving Show

-- DDEX Message Headers
DdexMessageHeader
  documentId DdexDocumentId
  messageId Text
  threadId Text Maybe
  senderDpid Text Maybe
  recipientDpid Text Maybe
  createdDate UTCTime Maybe
  controlType Text Maybe
  deriving Show

-- Validation Runs
DdexValidationRun
  documentId DdexDocumentId
  validatorVersion Text Maybe
  schemaVersion Text Maybe
  startedAt UTCTime
  finishedAt UTCTime Maybe
  result ValidationResultEnum Maybe
  errorCount Int
  warningCount Int
  deriving Show

-- Validation Issues
DdexValidationIssue
  validationRunId DdexValidationRunId
  severity ValidationSeverityEnum
  layer ValidationLayerEnum
  code Text Maybe
  lineNumber Int Maybe
  columnNumber Int Maybe
  xpathRef Text Maybe
  message Text
  suggestion Text Maybe
  deriving Show

-- Import Plans
DdexImportPlan
  documentId DdexDocumentId
  status ImportPlanStatusEnum
  snapshotJson Text  -- JSONB stored as Text
  version Int
  createdAt UTCTime
  deriving Show

-- Import Runs
DdexImportRun
  planId DdexImportPlanId
  actorId Int
  status ImportRunStatusEnum
  startedAt UTCTime
  finishedAt UTCTime Maybe
  errorMessage Text Maybe
  deriving Show

-- Import Changes (Audit)
DdexImportChange
  importRunId DdexImportRunId
  entityType Text
  entityId Int Maybe
  operation ImportOperationEnum
  previousState Text Maybe  -- JSONB
  newState Text  -- JSONB
  deriving Show

-- DDEX Exports
DdexExport
  releaseId Int  -- FK to catalog_release
  partnerId Int Maybe  -- FK to ddex_partner
  ernVersion Text
  profileName Text Maybe
  xmlChecksum Text
  privateUri Text
  validationResult Text Maybe
  createdAt UTCTime
  deriving Show

-- DDEX Partners
DdexPartner
  name Text
  dpid Text Maybe
  allowedVersions [Text]  -- Array
  rulesJson Text Maybe  -- JSONB
  namingConvention Text Maybe
  isActive Bool
  UniqueDdexPartnerName name
  deriving Show

-- Background Jobs
DdexJob
  jobType DdexJobTypeEnum
  entityId Int  -- Document ID, Plan ID, etc.
  status DdexJobStatusEnum
  attempts Int
  leasedUntil UTCTime Maybe
  lastError Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

|]
