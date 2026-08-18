{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

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
  , completeValidationRun
  , getLatestValidationRun
  , getValidationReport
    -- * Partners
  , insertPartner
  , listPartnersWithVersions
  ) where

import Data.Text (Text)
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import qualified TDF.Catalog.Models as Catalog
import TDF.DDEX.Models
import qualified TDF.DDEX.Types as Types

-- ============================================================
-- Document Operations
-- ============================================================

-- | Insert a new DDEX document using canonical persisted relationships.
-- Legacy string columns remain migration evidence and current writes clear them.
insertDocument :: Text -> Text -> Text -> Int -> Catalog.DdexStandardVersionId -> Maybe Catalog.DdexMessageTypeId -> Catalog.WorkflowStateId -> Maybe Text -> Int -> Maybe Text -> Maybe Text -> Maybe Text -> SqlPersistT IO (Maybe DdexDocumentId)
insertDocument fileName privateUri sha256 sizeBytes standardVersionId messageTypeId workflowStateId namespace uploadedBy messageId senderId recipientId = do
  now <- liftIO getCurrentTime
  insertUnique $ DdexDocument
    { ddexDocumentFileName = fileName
    , ddexDocumentPrivateUri = privateUri
    , ddexDocumentSha256 = sha256
    , ddexDocumentSizeBytes = sizeBytes
    , ddexDocumentStandardVersionId = Just standardVersionId
    , ddexDocumentMessageTypeId = messageTypeId
    , ddexDocumentWorkflowStateId = Just workflowStateId
    , ddexDocumentFamily = Nothing
    , ddexDocumentVersion = Nothing
    , ddexDocumentNamespace = namespace
    , ddexDocumentMessageType = Nothing
    , ddexDocumentStatus = Nothing
    , ddexDocumentUploadedBy = uploadedBy
    , ddexDocumentMessageId = messageId
    , ddexDocumentSenderId = senderId
    , ddexDocumentRecipientId = recipientId
    , ddexDocumentCreatedAt = now
    }

-- | Get document by ID (returns Entity with key)
getDocumentById :: DdexDocumentId -> SqlPersistT IO (Maybe (Entity DdexDocument))
getDocumentById docId = do
  mDoc <- get docId
  return $ fmap (Entity docId) mDoc

-- | List documents with optional filters (returns Entities with keys)
listDocuments :: Maybe Catalog.WorkflowStateId -> SqlPersistT IO [Entity DdexDocument]
listDocuments mWorkflowStateId =
  selectList
    (maybe [] (\stateId -> [DdexDocumentWorkflowStateId ==. Just stateId]) mWorkflowStateId)
    [Desc DdexDocumentCreatedAt]

-- | Update document status
updateDocumentStatus :: DdexDocumentId -> Catalog.WorkflowStateId -> SqlPersistT IO ()
updateDocumentStatus docId workflowStateId =
  update docId
    [ DdexDocumentWorkflowStateId =. Just workflowStateId
    , DdexDocumentStatus =. Nothing
    ]

-- | Find document by SHA-256 hash (returns Entity with key)
findDocumentBySha256 :: Text -> SqlPersistT IO (Maybe (Entity DdexDocument))
findDocumentBySha256 sha = do
  results <- selectList [DdexDocumentSha256 ==. sha] [LimitTo 1]
  return $ case results of
    [] -> Nothing
    (x:_) -> Just x

-- ============================================================
-- Validation Operations
-- ============================================================

insertValidationRun
  :: DdexDocumentId
  -> Catalog.WorkflowStateId
  -> Maybe Text
  -> Maybe Text
  -> SqlPersistT IO DdexValidationRunId
insertValidationRun documentId workflowStateId validatorVersion schemaVersion = do
  now <- liftIO getCurrentTime
  insert DdexValidationRun
    { ddexValidationRunDocumentId = documentId
    , ddexValidationRunWorkflowStateId = Just workflowStateId
    , ddexValidationRunValidationResultId = Nothing
    , ddexValidationRunValidatorVersion = validatorVersion
    , ddexValidationRunSchemaVersion = schemaVersion
    , ddexValidationRunStartedAt = now
    , ddexValidationRunFinishedAt = Nothing
    , ddexValidationRunResult = Nothing
    , ddexValidationRunErrorCount = 0
    , ddexValidationRunWarningCount = 0
    }

-- | Insert a validation issue
insertValidationIssue :: DdexValidationRunId -> Types.ValidationSeverity -> Types.ValidationLayer -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Text -> Maybe Text -> SqlPersistT IO DdexValidationIssueId
insertValidationIssue runId severity layer code lineNumber colNumber xpath message suggestion = do
  severityEntity <- getBy (UniqueDdexValidationSeverityCode (validationSeverityCode severity))
  layerEntity <- getBy (UniqueDdexValidationLayerCode (validationLayerCode layer))
  severityId <- requireActive "severity" ddexValidationSeverityActive severityEntity
  layerId <- requireActive "layer" ddexValidationLayerActive layerEntity
  insert $ DdexValidationIssue
    { ddexValidationIssueValidationRunId = runId
    , ddexValidationIssueSeverityId = Just severityId
    , ddexValidationIssueLayerId = Just layerId
    , ddexValidationIssueSeverity = Nothing
    , ddexValidationIssueLayer = Nothing
    , ddexValidationIssueCode = code
    , ddexValidationIssueLineNumber = lineNumber
    , ddexValidationIssueColumnNumber = colNumber
    , ddexValidationIssueXpathRef = xpath
    , ddexValidationIssueMessage = message
    , ddexValidationIssueSuggestion = suggestion
    }
  where
    requireActive label isActive candidate = case candidate of
      Just (Entity identifier value) | isActive value -> pure identifier
      _ -> liftIO . ioError . userError $
        "Missing active persisted DDEX validation " <> label <> " registry row"

validationSeverityCode :: Types.ValidationSeverity -> Text
validationSeverityCode Types.SeverityError = "error"
validationSeverityCode Types.SeverityWarning = "warning"
validationSeverityCode Types.SeverityInfo = "info"

validationLayerCode :: Types.ValidationLayer -> Text
validationLayerCode Types.LayerXML = "xml"
validationLayerCode Types.LayerXSD = "xsd"
validationLayerCode Types.LayerAVS = "avs"
validationLayerCode Types.LayerBusiness = "business"

completeValidationRun
  :: DdexValidationRunId
  -> Catalog.WorkflowStateId
  -> DdexValidationResultId
  -> ValidationResultEnum
  -> Int
  -> Int
  -> SqlPersistT IO ()
completeValidationRun runId workflowStateId resultId result errorCount warningCount = do
  now <- liftIO getCurrentTime
  update runId
    [ DdexValidationRunWorkflowStateId =. Just workflowStateId
    , DdexValidationRunValidationResultId =. Just resultId
    , DdexValidationRunFinishedAt =. Just now
    , DdexValidationRunResult =. Just result
    , DdexValidationRunErrorCount =. errorCount
    , DdexValidationRunWarningCount =. warningCount
    ]

-- | Get latest validation run for a document
getLatestValidationRun :: DdexDocumentId -> SqlPersistT IO (Maybe (Entity DdexValidationRun))
getLatestValidationRun docId = do
  results <- selectList [DdexValidationRunDocumentId ==. docId] [Desc DdexValidationRunStartedAt, LimitTo 1]
  return $ case results of
    [] -> Nothing
    (x:_) -> Just x

-- | Get validation report (run Entity + issues)
getValidationReport
  :: DdexDocumentId
  -> SqlPersistT IO
       (Maybe
         ( Entity DdexValidationRun
         , [(DdexValidationIssue, Entity DdexValidationSeverity, Entity DdexValidationLayer)]
         ))
getValidationReport docId = do
  mRun <- getLatestValidationRun docId
  case mRun of
    Nothing -> return Nothing
    Just runEntity -> do
      let runId = entityKey runEntity
      issues <- selectList [DdexValidationIssueValidationRunId ==. runId] []
      resolved <- traverse resolveIssue issues
      return $ Just (runEntity, resolved)
  where
    resolveIssue (Entity _ issue) = do
      severityId <- maybe missing pure (ddexValidationIssueSeverityId issue)
      layerId <- maybe missing pure (ddexValidationIssueLayerId issue)
      severity <- getEntity severityId >>= maybe missing pure
      layer <- getEntity layerId >>= maybe missing pure
      if ddexValidationSeverityActive (entityVal severity)
          && ddexValidationLayerActive (entityVal layer)
        then pure (issue, severity, layer)
        else missing
    missing = liftIO . ioError . userError $
      "DDEX validation report contains a missing or inactive canonical reference"

-- ============================================================
-- Partner Operations
-- ============================================================

-- | Insert a partner and its allowed governed standard versions atomically.
insertPartner :: Text -> Maybe Text -> [Catalog.DdexStandardVersionId] -> SqlPersistT IO DdexPartnerId
insertPartner name dpid allowedVersionIds = do
  now <- liftIO getCurrentTime
  partnerId <- insert $ DdexPartner
    { ddexPartnerName = name
    , ddexPartnerDpid = dpid
    , ddexPartnerRulesJson = Nothing
    , ddexPartnerNamingConvention = Nothing
    , ddexPartnerIsActive = True
    }
  mapM_ (insertVersion partnerId now) (zip [0 :: Int ..] allowedVersionIds)
  pure partnerId
  where
    insertVersion partnerId now (position, standardVersionId) =
      insert_ DdexPartnerStandardVersion
        { ddexPartnerStandardVersionPartnerId = partnerId
        , ddexPartnerStandardVersionStandardVersionId = standardVersionId
        , ddexPartnerStandardVersionSortOrder = position
        , ddexPartnerStandardVersionActive = True
        , ddexPartnerStandardVersionCreatedAt = now
        }

-- | List partners and their ordered, active governed standard versions.
listPartnersWithVersions :: SqlPersistT IO [(Entity DdexPartner, [Entity Catalog.DdexStandardVersion])]
listPartnersWithVersions = do
  partners <- selectList [DdexPartnerIsActive ==. True] [Asc DdexPartnerName]
  let partnerIds = map entityKey partners
  memberships <- selectList
    [ DdexPartnerStandardVersionPartnerId <-. partnerIds
    , DdexPartnerStandardVersionActive ==. True
    ]
    [ Asc DdexPartnerStandardVersionPartnerId
    , Asc DdexPartnerStandardVersionSortOrder
    ]
  versions <- selectList
    [ Catalog.DdexStandardVersionId <-. map (ddexPartnerStandardVersionStandardVersionId . entityVal) memberships
    , Catalog.DdexStandardVersionActive ==. True
    ]
    []
  let versionById = [(entityKey row, row) | row <- versions]
      partnerVersions partnerId =
        [ version
        | Entity _ membership <- memberships
        , ddexPartnerStandardVersionPartnerId membership == partnerId
        , Just version <- [lookup (ddexPartnerStandardVersionStandardVersionId membership) versionById]
        ]
  pure [(partner, partnerVersions (entityKey partner)) | partner <- partners]
