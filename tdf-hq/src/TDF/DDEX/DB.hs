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
  ) where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import TDF.DDEX.Models
import qualified TDF.DDEX.Types as T

-- | Insert a new DDEX document
-- TODO: Implement with actual DB insertion
insertDocument :: Text -> Text -> Text -> Int -> T.DdexFamily -> Text -> Maybe Text -> Maybe Text -> T.DdexDocumentStatus -> Int -> Maybe Text -> Maybe Text -> Maybe Text -> SqlPersistT IO DdexDocumentId
insertDocument _fileName _privateUri _sha256 _sizeBytes _family _version _namespace _messageType _status _uploadedBy _messageId _senderId _recipientId = do
  -- Placeholder: would insert into database
  error "insertDocument not yet implemented"

-- | Get document by ID
getDocumentById :: DdexDocumentId -> SqlPersistT IO (Maybe DdexDocument)
getDocumentById = get

-- | List documents with optional filters
listDocuments :: Maybe Text -> Maybe Text -> SqlPersistT IO [DdexDocument]
listDocuments _mStatus _mPartner = do
  -- TODO: Implement filtering
  results <- selectList [] [Desc DdexDocumentCreatedAt]
  return $ map entityVal results

-- | Update document status
updateDocumentStatus :: DdexDocumentId -> T.DdexDocumentStatus -> SqlPersistT IO ()
updateDocumentStatus _docId _status = do
  -- TODO: Implement status update
  return ()

-- | Find document by SHA-256 hash
findDocumentBySha256 :: Text -> SqlPersistT IO (Maybe DdexDocument)
findDocumentBySha256 sha = do
  results <- selectList [DdexDocumentSha256 ==. sha] [LimitTo 1]
  return $ case results of
    [] -> Nothing
    (x:_) -> Just (entityVal x)

-- | Insert a validation run
insertValidationRun :: DdexDocumentId -> Maybe Text -> Maybe Text -> SqlPersistT IO DdexValidationRunId
insertValidationRun _docId _validatorVersion _schemaVersion = do
  -- TODO: Implement
  error "insertValidationRun not yet implemented"

-- | Insert a validation issue
insertValidationIssue :: DdexValidationRunId -> ValidationSeverityEnum -> ValidationLayerEnum -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Text -> Maybe Text -> SqlPersistT IO DdexValidationIssueId
insertValidationIssue _runId _severity _layer _code _line _col _xpath _message _suggestion = do
  -- TODO: Implement
  error "insertValidationIssue not yet implemented"

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

-- | Insert an import plan
insertImportPlan :: DdexDocumentId -> Text -> SqlPersistT IO DdexImportPlanId
insertImportPlan _docId _snapshotJson = do
  -- TODO: Implement
  error "insertImportPlan not yet implemented"

-- | Get import plan by ID
getImportPlanById :: DdexImportPlanId -> SqlPersistT IO (Maybe DdexImportPlan)
getImportPlanById = get

-- | Update import plan status
updateImportPlanStatus :: DdexImportPlanId -> ImportPlanStatusEnum -> SqlPersistT IO ()
updateImportPlanStatus _planId _status = do
  -- TODO: Implement
  return ()

-- | Insert a background job
insertJob :: DdexJobTypeEnum -> Int -> SqlPersistT IO DdexJobId
insertJob _jobType _entityId = do
  -- TODO: Implement
  error "insertJob not yet implemented"

-- | Claim a job for processing
claimJob :: DdexJobTypeEnum -> Int -> SqlPersistT IO (Maybe (DdexJobId, DdexJob))
claimJob _jobType _leaseSeconds = do
  -- TODO: Implement with FOR UPDATE SKIP LOCKED
  return Nothing

-- | Mark a job as completed
completeJob :: DdexJobId -> SqlPersistT IO ()
completeJob _jobId = do
  -- TODO: Implement
  return ()

-- | Mark a job as failed
failJob :: DdexJobId -> Text -> SqlPersistT IO ()
failJob _jobId _errorMsg = do
  -- TODO: Implement
  return ()

-- | Insert a partner
insertPartner :: Text -> Maybe Text -> [Text] -> SqlPersistT IO DdexPartnerId
insertPartner _name _dpid _allowedVersions = do
  -- TODO: Implement
  error "insertPartner not yet implemented"

-- | List all partners
listPartners :: SqlPersistT IO [DdexPartner]
listPartners = do
  results <- selectList [DdexPartnerIsActive ==. True] [Asc DdexPartnerName]
  return $ map entityVal results
