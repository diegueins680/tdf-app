{-# LANGUAGE OverloadedStrings #-}

module TDF.DDEX.DB
  ( -- * Documents
    insertDocument
  , getDocumentById
  , listDocuments
  , updateDocumentStatus
  , findDocumentBySha256
    -- * Validation
  , insertValidationRun
  , insertValidationIssue
  , getLatestValidationRun
  , getValidationReport
    -- * Import Plans
  , insertImportPlan
  , getImportPlanById
  , updateImportPlanStatus
    -- * Jobs
  , insertJob
  , claimJob
  , completeJob
  , failJob
    -- * Partners
  , insertPartner
  , listPartners
    -- * Enum conversions
  , toStatusEnum
  , fromStatusEnum
  , toFamilyEnum
  , fromFamilyEnum
  , toJobTypeEnum
  , toJobStatusEnum
  ) where

import Data.Text (Text)
import Data.Time (getCurrentTime, addUTCTime)
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import TDF.DDEX.Models
import qualified TDF.DDEX.Types as T

-- ============================================================
-- Enum Conversions
-- ============================================================

-- | Convert Types.DdexDocumentStatus to Models enum
toStatusEnum :: T.DdexDocumentStatus -> DdexDocumentStatusEnum
toStatusEnum T.StatusReceived = StatusReceived
toStatusEnum T.StatusQuarantined = StatusQuarantined
toStatusEnum T.StatusQueued = StatusQueued
toStatusEnum T.StatusValidating = StatusValidating
toStatusEnum T.StatusInvalid = StatusInvalid
toStatusEnum T.StatusValid = StatusValid
toStatusEnum T.StatusMappingRequired = StatusMappingRequired
toStatusEnum T.StatusReadyToImport = StatusReadyToImport
toStatusEnum T.StatusImporting = StatusImporting
toStatusEnum T.StatusImported = StatusImported
toStatusEnum T.StatusImportFailed = StatusImportFailed
toStatusEnum T.StatusSuperseded = StatusSuperseded

-- | Convert Models enum to Types.DdexDocumentStatus
fromStatusEnum :: DdexDocumentStatusEnum -> T.DdexDocumentStatus
fromStatusEnum StatusReceived = T.StatusReceived
fromStatusEnum StatusQuarantined = T.StatusQuarantined
fromStatusEnum StatusQueued = T.StatusQueued
fromStatusEnum StatusValidating = T.StatusValidating
fromStatusEnum StatusInvalid = T.StatusInvalid
fromStatusEnum StatusValid = T.StatusValid
fromStatusEnum StatusMappingRequired = T.StatusMappingRequired
fromStatusEnum StatusReadyToImport = T.StatusReadyToImport
fromStatusEnum StatusImporting = T.StatusImporting
fromStatusEnum StatusImported = T.StatusImported
fromStatusEnum StatusImportFailed = T.StatusImportFailed
fromStatusEnum StatusSuperseded = T.StatusSuperseded

-- | Convert Types.DdexFamily to Models enum
toFamilyEnum :: T.DdexFamily -> DdexFamilyEnum
toFamilyEnum T.FamilyERN = FamilyERN
toFamilyEnum T.FamilyRIN = FamilyRIN
toFamilyEnum T.FamilyDSR = FamilyDSR
toFamilyEnum T.FamilyMEAD = FamilyMEAD

-- | Convert Models enum to Types.DdexFamily
fromFamilyEnum :: DdexFamilyEnum -> T.DdexFamily
fromFamilyEnum FamilyERN = T.FamilyERN
fromFamilyEnum FamilyRIN = T.FamilyRIN
fromFamilyEnum FamilyDSR = T.FamilyDSR
fromFamilyEnum FamilyMEAD = T.FamilyMEAD

-- | Convert Types.DdexJobType to Models enum
toJobTypeEnum :: T.DdexJobType -> DdexJobTypeEnum
toJobTypeEnum T.JobValidate = JobValidate
toJobTypeEnum T.JobImport = JobImport
toJobTypeEnum T.JobExport = JobExport
toJobTypeEnum T.JobCleanup = JobCleanup

-- | Convert Types.DdexJobStatus to Models enum
toJobStatusEnum :: T.DdexJobStatus -> DdexJobStatusEnum
toJobStatusEnum T.JobPending = JobPending
toJobStatusEnum T.JobProcessing = JobProcessing
toJobStatusEnum T.JobCompleted = JobCompleted
toJobStatusEnum T.JobFailed = JobFailed
toJobStatusEnum T.JobRetry = JobRetry

-- ============================================================
-- Document Operations
-- ============================================================

-- | Insert a new DDEX document
insertDocument :: Text -> Text -> Text -> Int -> T.DdexFamily -> Text -> Maybe Text -> Maybe Text -> T.DdexDocumentStatus -> Int -> Maybe Text -> Maybe Text -> Maybe Text -> SqlPersistT IO DdexDocumentId
insertDocument fileName privateUri sha256 sizeBytes family version namespace messageType status uploadedBy messageId senderId recipientId = do
  now <- liftIO getCurrentTime
  let familyEnum = toFamilyEnum family
      statusEnum = toStatusEnum status
  insert $ DdexDocument
    { ddexDocumentFileName = fileName
    , ddexDocumentPrivateUri = privateUri
    , ddexDocumentSha256 = sha256
    , ddexDocumentSizeBytes = sizeBytes
    , ddexDocumentFamily = familyEnum
    , ddexDocumentVersion = version
    , ddexDocumentNamespace = namespace
    , ddexDocumentMessageType = messageType
    , ddexDocumentStatus = statusEnum
    , ddexDocumentUploadedBy = uploadedBy
    , ddexDocumentMessageId = messageId
    , ddexDocumentSenderId = senderId
    , ddexDocumentRecipientId = recipientId
    , ddexDocumentCreatedAt = now
    }

-- | Get document by ID
getDocumentById :: DdexDocumentId -> SqlPersistT IO (Maybe DdexDocument)
getDocumentById = get

-- | List documents with optional filters
listDocuments :: Maybe Text -> Maybe Text -> SqlPersistT IO [DdexDocument]
listDocuments mStatus _mPartner = do
  let filters = case mStatus of
        Nothing -> []
        Just s -> case parseStatus s of
          Nothing -> []
          Just st -> [DdexDocumentStatus ==. st]
  results <- selectList filters [Desc DdexDocumentCreatedAt]
  return $ map entityVal results
  where
    parseStatus "received" = Just StatusReceived
    parseStatus "quarantined" = Just StatusQuarantined
    parseStatus "queued" = Just StatusQueued
    parseStatus "validating" = Just StatusValidating
    parseStatus "invalid" = Just StatusInvalid
    parseStatus "valid" = Just StatusValid
    parseStatus "mapping_required" = Just StatusMappingRequired
    parseStatus "ready_to_import" = Just StatusReadyToImport
    parseStatus "importing" = Just StatusImporting
    parseStatus "imported" = Just StatusImported
    parseStatus "import_failed" = Just StatusImportFailed
    parseStatus "superseded" = Just StatusSuperseded
    parseStatus _ = Nothing

-- | Update document status
updateDocumentStatus :: DdexDocumentId -> T.DdexDocumentStatus -> SqlPersistT IO ()
updateDocumentStatus docId status = do
  let statusEnum = toStatusEnum status
  update docId [DdexDocumentStatus =. statusEnum]

-- | Find document by SHA-256 hash
findDocumentBySha256 :: Text -> SqlPersistT IO (Maybe DdexDocument)
findDocumentBySha256 sha = do
  results <- selectList [DdexDocumentSha256 ==. sha] [LimitTo 1]
  return $ case results of
    [] -> Nothing
    (x:_) -> Just (entityVal x)

-- ============================================================
-- Validation Operations
-- ============================================================

-- | Insert a validation run
insertValidationRun :: DdexDocumentId -> Maybe Text -> Maybe Text -> SqlPersistT IO DdexValidationRunId
insertValidationRun docId validatorVersion schemaVersion = do
  now <- liftIO getCurrentTime
  insert $ DdexValidationRun
    { ddexValidationRunDocumentId = docId
    , ddexValidationRunValidatorVersion = validatorVersion
    , ddexValidationRunSchemaVersion = schemaVersion
    , ddexValidationRunStartedAt = now
    , ddexValidationRunFinishedAt = Nothing
    , ddexValidationRunResult = Nothing
    , ddexValidationRunErrorCount = 0
    , ddexValidationRunWarningCount = 0
    }

-- | Insert a validation issue
insertValidationIssue :: DdexValidationRunId -> ValidationSeverityEnum -> ValidationLayerEnum -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Text -> Maybe Text -> SqlPersistT IO DdexValidationIssueId
insertValidationIssue runId severity layer code lineNumber colNumber xpath message suggestion = do
  insert $ DdexValidationIssue
    { ddexValidationIssueValidationRunId = runId
    , ddexValidationIssueSeverity = severity
    , ddexValidationIssueLayer = layer
    , ddexValidationIssueCode = code
    , ddexValidationIssueLineNumber = lineNumber
    , ddexValidationIssueColumnNumber = colNumber
    , ddexValidationIssueXpathRef = xpath
    , ddexValidationIssueMessage = message
    , ddexValidationIssueSuggestion = suggestion
    }

-- | Get latest validation run for a document
getLatestValidationRun :: DdexDocumentId -> SqlPersistT IO (Maybe (Entity DdexValidationRun))
getLatestValidationRun docId = do
  results <- selectList [DdexValidationRunDocumentId ==. docId] [Desc DdexValidationRunStartedAt, LimitTo 1]
  return $ case results of
    [] -> Nothing
    (x:_) -> Just x

-- | Get validation report (run + issues)
getValidationReport :: DdexDocumentId -> SqlPersistT IO (Maybe (DdexValidationRun, [DdexValidationIssue]))
getValidationReport docId = do
  mRun <- getLatestValidationRun docId
  case mRun of
    Nothing -> return Nothing
    Just runEntity -> do
      let runId = entityKey runEntity
      issues <- selectList [DdexValidationIssueValidationRunId ==. runId] []
      return $ Just (entityVal runEntity, map entityVal issues)

-- ============================================================
-- Import Plan Operations
-- ============================================================

-- | Insert an import plan
insertImportPlan :: DdexDocumentId -> Text -> SqlPersistT IO DdexImportPlanId
insertImportPlan docId snapshotJson = do
  now <- liftIO getCurrentTime
  insert $ DdexImportPlan
    { ddexImportPlanDocumentId = docId
    , ddexImportPlanStatus = PlanDraft
    , ddexImportPlanSnapshotJson = snapshotJson
    , ddexImportPlanVersion = 1
    , ddexImportPlanCreatedAt = now
    }

-- | Get import plan by ID
getImportPlanById :: DdexImportPlanId -> SqlPersistT IO (Maybe DdexImportPlan)
getImportPlanById = get

-- | Update import plan status
updateImportPlanStatus :: DdexImportPlanId -> ImportPlanStatusEnum -> SqlPersistT IO ()
updateImportPlanStatus planId status = do
  update planId [DdexImportPlanStatus =. status]

-- ============================================================
-- Job Queue Operations
-- ============================================================

-- | Insert a background job
insertJob :: DdexJobTypeEnum -> Int -> SqlPersistT IO DdexJobId
insertJob jobType entityId = do
  now <- liftIO getCurrentTime
  insert $ DdexJob
    { ddexJobJobType = jobType
    , ddexJobEntityId = entityId
    , ddexJobStatus = JobPending
    , ddexJobAttempts = 0
    , ddexJobLeasedUntil = Nothing
    , ddexJobLastError = Nothing
    , ddexJobCreatedAt = now
    , ddexJobUpdatedAt = now
    }

-- | Claim a job for processing
-- Note: This is a simplified version. For production, use raw SQL with FOR UPDATE SKIP LOCKED
claimJob :: DdexJobTypeEnum -> Int -> SqlPersistT IO (Maybe (DdexJobId, DdexJob))
claimJob jobType leaseSeconds = do
  -- Find a pending job
  results <- selectList
    [ DdexJobJobType ==. jobType
    , DdexJobStatus ==. JobPending
    ]
    [Asc DdexJobCreatedAt, LimitTo 1]
  case results of
    [] -> return Nothing
    (jobEntity:_) -> do
      let jobId = entityKey jobEntity
          job = entityVal jobEntity
      now <- liftIO getCurrentTime
      let leaseUntil = addUTCTime (fromIntegral leaseSeconds) now
      -- Update job to processing status with lease
      update jobId
        [ DdexJobStatus =. JobProcessing
        , DdexJobLeasedUntil =. Just leaseUntil
        , DdexJobUpdatedAt =. now
        ]
      -- Return updated job
      mUpdated <- get jobId
      case mUpdated of
        Nothing -> return Nothing
        Just updated -> return $ Just (jobId, updated)

-- | Mark a job as completed
completeJob :: DdexJobId -> SqlPersistT IO ()
completeJob jobId = do
  now <- liftIO getCurrentTime
  update jobId
    [ DdexJobStatus =. JobCompleted
    , DdexJobUpdatedAt =. now
    ]

-- | Mark a job as failed
failJob :: DdexJobId -> Text -> SqlPersistT IO ()
failJob jobId errorMsg = do
  now <- liftIO getCurrentTime
  update jobId
    [ DdexJobStatus =. JobFailed
    , DdexJobLastError =. Just errorMsg
    , DdexJobUpdatedAt =. now
    ]

-- ============================================================
-- Partner Operations
-- ============================================================

-- | Insert a partner
insertPartner :: Text -> Maybe Text -> [Text] -> SqlPersistT IO DdexPartnerId
insertPartner name dpid allowedVersions = do
  insert $ DdexPartner
    { ddexPartnerName = name
    , ddexPartnerDpid = dpid
    , ddexPartnerAllowedVersions = allowedVersions
    , ddexPartnerRulesJson = Nothing
    , ddexPartnerNamingConvention = Nothing
    , ddexPartnerIsActive = True
    }

-- | List all partners
listPartners :: SqlPersistT IO [DdexPartner]
listPartners = do
  results <- selectList [DdexPartnerIsActive ==. True] [Asc DdexPartnerName]
  return $ map entityVal results
