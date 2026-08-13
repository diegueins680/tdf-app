{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.DDEX.Models where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Persist.TH
import GHC.Generics (Generic)
import qualified TDF.Catalog.Models as Catalog
import TDF.UUIDInstances ()

-- Legacy persisted constructors retained only as reversible migration evidence.
-- Runtime parsing remains exhaustive in TDF.DDEX.Types, while database rows
-- below own identity, labels, ordering, and availability.
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

share [mkPersist sqlSettings, mkMigrate "migrateDdex"] [persistLowerCase|

-- Persisted mirrors for executable DDEX dispatch discriminants. Runtime code
-- may recognize the immutable codes, while labels, ordering and availability
-- remain database-authoritative.
DdexJobOperation
  Id UUID default=gen_random_uuid()
  code Text
  nameEs Text
  nameEn Text
  descriptionEs Text Maybe
  descriptionEn Text Maybe
  active Bool default=True
  sortOrder Int default=0
  version Int default=1
  UniqueDdexJobOperationCode code
  deriving Show Generic

DdexImportOperation
  Id UUID default=gen_random_uuid()
  code Text
  nameEs Text
  nameEn Text
  descriptionEs Text Maybe
  descriptionEn Text Maybe
  active Bool default=True
  sortOrder Int default=0
  version Int default=1
  UniqueDdexImportOperationCode code
  deriving Show Generic

DdexValidationResult
  Id UUID default=gen_random_uuid()
  code Text
  nameEs Text
  nameEn Text
  descriptionEs Text Maybe
  descriptionEn Text Maybe
  active Bool default=True
  sortOrder Int default=0
  version Int default=1
  UniqueDdexValidationResultCode code
  deriving Show Generic

DdexValidationSeverity
  Id UUID default=gen_random_uuid()
  code Text
  nameEs Text
  nameEn Text
  descriptionEs Text Maybe
  descriptionEn Text Maybe
  active Bool default=True
  sortOrder Int default=0
  version Int default=1
  UniqueDdexValidationSeverityCode code
  deriving Show Generic

DdexValidationLayer
  Id UUID default=gen_random_uuid()
  code Text
  nameEs Text
  nameEn Text
  descriptionEs Text Maybe
  descriptionEn Text Maybe
  active Bool default=True
  sortOrder Int default=0
  version Int default=1
  UniqueDdexValidationLayerCode code
  deriving Show Generic

-- DDEX Documents
DdexDocument
  fileName Text
  privateUri Text
  sha256 Text
  sizeBytes Int
  standardVersionId Catalog.DdexStandardVersionId Maybe
  messageTypeId Catalog.DdexMessageTypeId Maybe
  workflowStateId Catalog.WorkflowStateId Maybe
  family Text Maybe
  version Text Maybe
  namespace Text Maybe
  messageType Text Maybe
  status Text Maybe
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
  workflowStateId Catalog.WorkflowStateId Maybe
  validationResultId DdexValidationResultId Maybe sql=result_id
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
  severityId DdexValidationSeverityId Maybe
  layerId DdexValidationLayerId Maybe
  severity ValidationSeverityEnum Maybe
  layer ValidationLayerEnum Maybe
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
  workflowStateId Catalog.WorkflowStateId Maybe
  status Text Maybe
  snapshotJson Text  -- JSONB stored as Text
  version Int
  createdAt UTCTime
  deriving Show

-- Import Runs
DdexImportRun
  planId DdexImportPlanId
  actorId Int
  workflowStateId Catalog.WorkflowStateId Maybe
  status Text Maybe
  startedAt UTCTime
  finishedAt UTCTime Maybe
  errorMessage Text Maybe
  deriving Show

-- Import Changes (Audit)
DdexImportChange
  importRunId DdexImportRunId
  entityType Text
  entityId Int Maybe
  operationId DdexImportOperationId Maybe
  operation Text Maybe
  previousState Text Maybe  -- JSONB
  newState Text  -- JSONB
  deriving Show

-- DDEX Exports
DdexExport
  releaseId Int  -- FK to catalog_release
  partnerId Int Maybe  -- FK to ddex_partner
  standardVersionId Catalog.DdexStandardVersionId Maybe
  workflowStateId Catalog.WorkflowStateId Maybe
  ernVersion Text Maybe
  profileName Text Maybe
  xmlChecksum Text
  privateUri Text
  validationResultId DdexValidationResultId Maybe sql=validation_result_id
  validationResult Text Maybe
  createdAt UTCTime
  deriving Show

-- DDEX Partners
DdexPartner
  name Text
  dpid Text Maybe
  rulesJson Text Maybe  -- JSONB
  namingConvention Text Maybe
  isActive Bool
  UniqueDdexPartnerName name
  deriving Show

DdexPartnerStandardVersion
  partnerId DdexPartnerId
  standardVersionId Catalog.DdexStandardVersionId
  sortOrder Int default=0
  active Bool default=True
  createdAt UTCTime
  UniqueDdexPartnerStandardVersion partnerId standardVersionId
  deriving Show

-- Durable work queue. jobType/status are nullable migration-evidence columns;
-- current writers use operationId/workflowStateId exclusively.
DdexJob
  jobOperationId DdexJobOperationId Maybe sql=operation_id
  jobType Text Maybe
  entityId Int
  workflowStateId Catalog.WorkflowStateId Maybe
  status Text Maybe
  attempts Int
  leasedUntil UTCTime Maybe
  lastError Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

|]
