{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.Server.DDEX (ddexServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad (forM, unless, when)
import Control.Applicative ((<|>))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Char (isControl)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Database.Persist (Entity(..), PersistValue(PersistText), SelectOpt(Asc), get, getBy, selectFirst, selectList, entityKey, entityVal, toPersistValue, (==.), (<-.))
import Database.Persist.Sql (Single(..), SqlPersistT, rawSql, runSqlPool, toSqlKey, fromSqlKey)
import Servant
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, takeFileName)
import TDF.API.DDEX
import TDF.Auth (AuthedUser(..), validateModuleAccess, ModuleAccess(..))
import qualified TDF.Catalog.Models as Catalog
import TDF.DB (Env(..))
import qualified TDF.DDEX.DB as DB
import qualified TDF.DDEX.Detect as Detect
import qualified TDF.DDEX.ERN.V432.BusinessRules as BusinessRules
import qualified TDF.DDEX.ERN.V432.Normalize as Normalize
import qualified TDF.DDEX.ERN.V432.Parse as Parse
import qualified TDF.DDEX.ERN.V432.Types as ERN
import qualified TDF.DDEX.Models as M
import qualified TDF.DDEX.Security as Security
import qualified TDF.DDEX.Storage as Storage
import qualified TDF.DDEX.Types as DDEXTypes
import Web.PathPieces (PathPiece, fromPathPiece, toPathPiece)

type AppM = ReaderT Env Handler

requireDdexCapability :: AuthedUser -> Text -> AppM ()
requireDdexCapability user permissionCode = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask
  rows <- liftIO $ runSqlPool
    (rawSql
      "SELECT EXISTS (SELECT 1 FROM party_security_role psr JOIN security_role r ON r.id=psr.role_id JOIN role_permission rp ON rp.role_id=r.id JOIN security_permission p ON p.id=rp.permission_id JOIN security_action a ON a.id=p.action_id JOIN security_module m ON m.id=p.module_id WHERE psr.party_id=? AND psr.active=TRUE AND r.active=TRUE AND rp.active=TRUE AND p.active=TRUE AND a.active=TRUE AND m.active=TRUE AND p.code=?)"
      [toPersistValue (auPartyId user), PersistText permissionCode])
    (envPool env)
  case rows of
    [Single True] -> pure ()
    _ -> throwError err403
      { errBody = BL.fromStrict (TE.encodeUtf8 ("Missing DDEX capability: " <> permissionCode))
      }

referenceSnapshotHandler :: AuthedUser -> Maybe Text -> AppM DdexReferenceSnapshotDTO
referenceSnapshotHandler user requestedLocale = do
  requireDdexCapability user "catalog.read"
  let locale = normalizeLocale requestedLocale
  env <- ask
  (standardRows, supportRows, messageTypeRows, workflowRow, stateRows,
    operationalWorkflowRows, operationalStateRows, jobOperationRows, importOperationRows,
    validationResultRows, validationSeverityRows, validationLayerRows) <-
      liftIO $ runSqlPool loadReferenceRows (envPool env)
  let supportMap = Map.fromList [(Catalog.ddexStandardSupportStandardVersionId value, value) | Entity _ value <- supportRows]
      standardVersionIds = map entityKey standardRows
      messageTypes = filter (\(Entity _ value) -> Catalog.ddexMessageTypeStandardVersionId value `elem` standardVersionIds) messageTypeRows
      revision = max 1 $
        sum [fromIntegral (Catalog.ddexStandardVersionVersion value) | Entity _ value <- standardRows]
          + sum [fromIntegral (Catalog.ddexStandardSupportVersion value) | Entity _ value <- supportRows]
          + sum [fromIntegral (Catalog.ddexMessageTypeVersion value) | Entity _ value <- messageTypes]
          + maybe 0 (Catalog.workflowDefinitionCacheRevision . entityVal) workflowRow
          + sum [Catalog.workflowDefinitionCacheRevision value | Entity _ value <- operationalWorkflowRows]
          + sum [fromIntegral (Catalog.workflowStateVersion value) | Entity _ value <- operationalStateRows]
          + sum [fromIntegral (M.ddexJobOperationVersion value) | Entity _ value <- jobOperationRows]
          + sum [fromIntegral (M.ddexImportOperationVersion value) | Entity _ value <- importOperationRows]
          + sum [fromIntegral (M.ddexValidationResultVersion value) | Entity _ value <- validationResultRows]
          + sum [fromIntegral (M.ddexValidationSeverityVersion value) | Entity _ value <- validationSeverityRows]
          + sum [fromIntegral (M.ddexValidationLayerVersion value) | Entity _ value <- validationLayerRows]
  pure DdexReferenceSnapshotDTO
    { ddexReferenceRevision = revision
    , ddexReferenceLocale = locale
    , ddexReferenceStandardVersions = map (standardVersionDTO locale supportMap) standardRows
    , ddexReferenceMessageTypes = map (messageTypeDTO locale) messageTypes
    , ddexReferenceDocumentStates = map (documentStateDTO locale) stateRows
    , ddexReferenceOperationalWorkflows =
        map (operationalWorkflowDTO locale operationalStateRows) operationalWorkflowRows
    , ddexReferenceJobOperations = map (jobOperationDTO locale) jobOperationRows
    , ddexReferenceImportOperations = map (importOperationDTO locale) importOperationRows
    , ddexReferenceValidationResults = map (validationResultDTO locale) validationResultRows
    , ddexReferenceValidationSeverities = map (validationSeverityDTO locale) validationSeverityRows
    , ddexReferenceValidationLayers = map (validationLayerDTO locale) validationLayerRows
    }
  where
    loadReferenceRows = do
      standards <- selectList [Catalog.DdexStandardVersionActive ==. True] [Asc Catalog.DdexStandardVersionSortOrder, Asc Catalog.DdexStandardVersionStandardCode, Asc Catalog.DdexStandardVersionVersionCode]
      supports <- selectList [Catalog.DdexStandardSupportDeploymentCode ==. "default", Catalog.DdexStandardSupportActive ==. True] []
      messages <- selectList [Catalog.DdexMessageTypeActive ==. True] [Asc Catalog.DdexMessageTypeSortOrder, Asc Catalog.DdexMessageTypeCode]
      workflow <- getBy (Catalog.UniqueWorkflowDefinitionCode "ddex-document-lifecycle")
      states <- case workflow of
        Nothing -> pure []
        Just (Entity workflowId _) -> selectList [Catalog.WorkflowStateWorkflowId ==. workflowId, Catalog.WorkflowStateActive ==. True] [Asc Catalog.WorkflowStateSortOrder]
      allWorkflows <- selectList [Catalog.WorkflowDefinitionActive ==. True] []
      let operationalWorkflows = sortOn (Catalog.workflowDefinitionCode . entityVal) $
            filter ((`elem` operationalWorkflowCodes) . Catalog.workflowDefinitionCode . entityVal) allWorkflows
          operationalWorkflowIds = map entityKey operationalWorkflows
      operationalStates <- selectList
        [ Catalog.WorkflowStateWorkflowId <-. operationalWorkflowIds
        , Catalog.WorkflowStateActive ==. True
        ]
        [Asc Catalog.WorkflowStateSortOrder]
      jobOperations <- selectList [M.DdexJobOperationActive ==. True]
        [Asc M.DdexJobOperationSortOrder, Asc M.DdexJobOperationCode]
      importOperations <- selectList [M.DdexImportOperationActive ==. True]
        [Asc M.DdexImportOperationSortOrder, Asc M.DdexImportOperationCode]
      validationResults <- selectList [M.DdexValidationResultActive ==. True]
        [Asc M.DdexValidationResultSortOrder, Asc M.DdexValidationResultCode]
      validationSeverities <- selectList [M.DdexValidationSeverityActive ==. True]
        [Asc M.DdexValidationSeveritySortOrder, Asc M.DdexValidationSeverityCode]
      validationLayers <- selectList [M.DdexValidationLayerActive ==. True]
        [Asc M.DdexValidationLayerSortOrder, Asc M.DdexValidationLayerCode]
      pure (standards, supports, messages, workflow, states, operationalWorkflows,
        operationalStates, jobOperations, importOperations, validationResults,
        validationSeverities, validationLayers)

    -- Stable executable registry identifiers; persisted rows remain authoritative
    -- for labels, ordering, availability, and state membership.
    operationalWorkflowCodes =
      [ "ddex-validation-lifecycle"
      , "ddex-import-plan-lifecycle"
      , "ddex-import-run-lifecycle"
      , "ddex-export-lifecycle"
      , "ddex-job-lifecycle"
      ]

normalizeLocale :: Maybe Text -> Text
normalizeLocale requested
  | maybe False ("en" `T.isPrefixOf`) (T.toLower . T.strip <$> requested) = "en"
  | otherwise = "es"

localized :: Text -> Text -> Text -> Text
localized locale spanish english
  | locale == "en" = english
  | otherwise = spanish

standardVersionDTO
  :: Text
  -> Map.Map Catalog.DdexStandardVersionId Catalog.DdexStandardSupport
  -> Entity Catalog.DdexStandardVersion
  -> DdexStandardVersionDTO
standardVersionDTO locale supportMap (Entity versionId value) =
  let support = Map.lookup versionId supportMap
  in DdexStandardVersionDTO
    { ddexStandardVersionId = toPathPiece versionId
    , ddexStandardCode = Catalog.ddexStandardVersionStandardCode value
    , ddexVersionCode = Catalog.ddexStandardVersionVersionCode value
    , ddexStandardVersionName = localized locale (Catalog.ddexStandardVersionNameEs value) (Catalog.ddexStandardVersionNameEn value)
    , ddexStandardVersionNameEs = Catalog.ddexStandardVersionNameEs value
    , ddexStandardVersionNameEn = Catalog.ddexStandardVersionNameEn value
    , ddexStandardSourceVersion = Catalog.ddexStandardVersionSourceVersion value
    , ddexStandardSourceUri = Catalog.ddexStandardVersionSourceUri value
    , ddexStandardDetectionEnabled = maybe False Catalog.ddexStandardSupportDetectionEnabled support
    , ddexStandardValidationEnabled = maybe False Catalog.ddexStandardSupportValidationEnabled support
    , ddexStandardImportEnabled = maybe False Catalog.ddexStandardSupportImportEnabled support
    , ddexStandardExportEnabled = maybe False Catalog.ddexStandardSupportExportEnabled support
    , ddexStandardVersionRevision = Catalog.ddexStandardVersionVersion value + maybe 0 Catalog.ddexStandardSupportVersion support
    }

messageTypeDTO :: Text -> Entity Catalog.DdexMessageType -> DdexMessageTypeDTO
messageTypeDTO locale (Entity messageTypeId value) = DdexMessageTypeDTO
  { ddexMessageTypeId = toPathPiece messageTypeId
  , ddexMessageTypeStandardVersionId = toPathPiece (Catalog.ddexMessageTypeStandardVersionId value)
  , ddexMessageTypeCode = Catalog.ddexMessageTypeCode value
  , ddexMessageTypeName = localized locale (Catalog.ddexMessageTypeNameEs value) (Catalog.ddexMessageTypeNameEn value)
  , ddexMessageTypeNameEs = Catalog.ddexMessageTypeNameEs value
  , ddexMessageTypeNameEn = Catalog.ddexMessageTypeNameEn value
  , ddexMessageTypeRuntimeSupported = Catalog.ddexMessageTypeRuntimeSupported value
  , ddexMessageTypeRevision = Catalog.ddexMessageTypeVersion value
  }

documentStateDTO :: Text -> Entity Catalog.WorkflowState -> DdexDocumentStateDTO
documentStateDTO locale (Entity stateId value) = DdexDocumentStateDTO
  { ddexDocumentStateId = toPathPiece stateId
  , ddexDocumentStateCode = Catalog.workflowStateCode value
  , ddexDocumentStateName = localized locale (Catalog.workflowStateNameEs value) (Catalog.workflowStateNameEn value)
  , ddexDocumentStateNameEs = Catalog.workflowStateNameEs value
  , ddexDocumentStateNameEn = Catalog.workflowStateNameEn value
  , ddexDocumentStateSortOrder = Catalog.workflowStateSortOrder value
  , ddexDocumentStateTerminal = Catalog.workflowStateTerminal value
  , ddexDocumentStateRevision = Catalog.workflowStateVersion value
  }

operationalWorkflowDTO
  :: Text
  -> [Entity Catalog.WorkflowState]
  -> Entity Catalog.WorkflowDefinition
  -> DdexOperationalWorkflowDTO
operationalWorkflowDTO locale stateRows (Entity workflowId value) = DdexOperationalWorkflowDTO
  { ddexOperationalWorkflowId = toPathPiece workflowId
  , ddexOperationalWorkflowCode = Catalog.workflowDefinitionCode value
  , ddexOperationalWorkflowName = localized locale
      (Catalog.workflowDefinitionNameEs value) (Catalog.workflowDefinitionNameEn value)
  , ddexOperationalWorkflowNameEs = Catalog.workflowDefinitionNameEs value
  , ddexOperationalWorkflowNameEn = Catalog.workflowDefinitionNameEn value
  , ddexOperationalWorkflowSensitive = Catalog.workflowDefinitionSensitive value
  , ddexOperationalWorkflowRevision = Catalog.workflowDefinitionCacheRevision value
  , ddexOperationalWorkflowStates = map (operationalStateDTO locale) $
      filter ((== workflowId) . Catalog.workflowStateWorkflowId . entityVal) stateRows
  }

operationalStateDTO :: Text -> Entity Catalog.WorkflowState -> DdexOperationalStateDTO
operationalStateDTO locale (Entity stateId value) = DdexOperationalStateDTO
  { ddexOperationalStateId = toPathPiece stateId
  , ddexOperationalStateCode = Catalog.workflowStateCode value
  , ddexOperationalStateName = localized locale
      (Catalog.workflowStateNameEs value) (Catalog.workflowStateNameEn value)
  , ddexOperationalStateNameEs = Catalog.workflowStateNameEs value
  , ddexOperationalStateNameEn = Catalog.workflowStateNameEn value
  , ddexOperationalStateSortOrder = Catalog.workflowStateSortOrder value
  , ddexOperationalStateTerminal = Catalog.workflowStateTerminal value
  , ddexOperationalStateRevision = Catalog.workflowStateVersion value
  }

jobOperationDTO :: Text -> Entity M.DdexJobOperation -> DdexOperationDTO
jobOperationDTO locale (Entity operationId value) = DdexOperationDTO
  { ddexOperationId = toPathPiece operationId
  , ddexOperationCode = M.ddexJobOperationCode value
  , ddexOperationName = localized locale
      (M.ddexJobOperationNameEs value) (M.ddexJobOperationNameEn value)
  , ddexOperationNameEs = M.ddexJobOperationNameEs value
  , ddexOperationNameEn = M.ddexJobOperationNameEn value
  , ddexOperationDescription = localizedOptional locale
      (M.ddexJobOperationDescriptionEs value) (M.ddexJobOperationDescriptionEn value)
  , ddexOperationDescriptionEs = M.ddexJobOperationDescriptionEs value
  , ddexOperationDescriptionEn = M.ddexJobOperationDescriptionEn value
  , ddexOperationSortOrder = M.ddexJobOperationSortOrder value
  , ddexOperationRevision = M.ddexJobOperationVersion value
  }

importOperationDTO :: Text -> Entity M.DdexImportOperation -> DdexOperationDTO
importOperationDTO locale (Entity operationId value) = DdexOperationDTO
  { ddexOperationId = toPathPiece operationId
  , ddexOperationCode = M.ddexImportOperationCode value
  , ddexOperationName = localized locale
      (M.ddexImportOperationNameEs value) (M.ddexImportOperationNameEn value)
  , ddexOperationNameEs = M.ddexImportOperationNameEs value
  , ddexOperationNameEn = M.ddexImportOperationNameEn value
  , ddexOperationDescription = localizedOptional locale
      (M.ddexImportOperationDescriptionEs value) (M.ddexImportOperationDescriptionEn value)
  , ddexOperationDescriptionEs = M.ddexImportOperationDescriptionEs value
  , ddexOperationDescriptionEn = M.ddexImportOperationDescriptionEn value
  , ddexOperationSortOrder = M.ddexImportOperationSortOrder value
  , ddexOperationRevision = M.ddexImportOperationVersion value
  }

localizedOptional :: Text -> Maybe Text -> Maybe Text -> Maybe Text
localizedOptional locale spanish english
  | locale == "en" = english <|> spanish
  | otherwise = spanish <|> english

validationResultDTO :: Text -> Entity M.DdexValidationResult -> DdexValidationReferenceDTO
validationResultDTO locale (Entity identifier value) = validationReferenceDTO locale
  identifier (M.ddexValidationResultCode value) (M.ddexValidationResultNameEs value)
  (M.ddexValidationResultNameEn value) (M.ddexValidationResultDescriptionEs value)
  (M.ddexValidationResultDescriptionEn value) (M.ddexValidationResultSortOrder value)
  (M.ddexValidationResultVersion value)

validationSeverityDTO :: Text -> Entity M.DdexValidationSeverity -> DdexValidationReferenceDTO
validationSeverityDTO locale (Entity identifier value) = validationReferenceDTO locale
  identifier (M.ddexValidationSeverityCode value) (M.ddexValidationSeverityNameEs value)
  (M.ddexValidationSeverityNameEn value) (M.ddexValidationSeverityDescriptionEs value)
  (M.ddexValidationSeverityDescriptionEn value) (M.ddexValidationSeveritySortOrder value)
  (M.ddexValidationSeverityVersion value)

validationLayerDTO :: Text -> Entity M.DdexValidationLayer -> DdexValidationReferenceDTO
validationLayerDTO locale (Entity identifier value) = validationReferenceDTO locale
  identifier (M.ddexValidationLayerCode value) (M.ddexValidationLayerNameEs value)
  (M.ddexValidationLayerNameEn value) (M.ddexValidationLayerDescriptionEs value)
  (M.ddexValidationLayerDescriptionEn value) (M.ddexValidationLayerSortOrder value)
  (M.ddexValidationLayerVersion value)

validationReferenceDTO
  :: PathPiece key
  => Text -> key -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Int -> Int
  -> DdexValidationReferenceDTO
validationReferenceDTO locale identifier code nameEs nameEn descriptionEs descriptionEn sortOrder revision =
  DdexValidationReferenceDTO
    { ddexValidationReferenceId = toPathPiece identifier
    , ddexValidationReferenceCode = code
    , ddexValidationReferenceName = localized locale nameEs nameEn
    , ddexValidationReferenceNameEs = nameEs
    , ddexValidationReferenceNameEn = nameEn
    , ddexValidationReferenceDescription = localizedOptional locale descriptionEs descriptionEn
    , ddexValidationReferenceDescriptionEs = descriptionEs
    , ddexValidationReferenceDescriptionEn = descriptionEn
    , ddexValidationReferenceSortOrder = sortOrder
    , ddexValidationReferenceRevision = revision
    }

-- | Main DDEX Server Implementation
ddexServer :: AuthedUser -> ServerT DDEXAPI AppM
ddexServer user =
       referenceSnapshotHandler user
  :<|> uploadDocumentHandler user
  :<|> listDocumentsHandler user
  :<|> getDocumentHandler user
  :<|> downloadRawHandler user
  :<|> validateDocumentHandler user
  :<|> getValidationReportHandler user
  :<|> getPreviewHandler user
  :<|> createImportPlanHandler user
  :<|> resolveImportPlanHandler user
  :<|> commitImportPlanHandler user
  :<|> createExportHandler user
  :<|> downloadExportHandler user
  :<|> listPartnersHandler user
  :<|> createPartnerHandler user

-- | Upload a DDEX document
uploadDocumentHandler :: AuthedUser -> DdexUploadRequest -> AppM DdexDocumentDTO
uploadDocumentHandler user DdexUploadRequest{..} = do
  requireDdexCapability user "catalog.import"
  fileName <- either (throwError . invalidUpload) pure (validateUploadName uploadFileName)
  let uploadMimeType = T.toLower (T.strip uploadContentType)
  unless (uploadMimeType `elem` ["application/xml", "text/xml"]) $
    throwError err415 { errBody = "DDEX upload must use application/xml or text/xml" }
  when (T.length uploadContentBase64 > 69905068) $
    throwError err413 { errBody = "DDEX upload exceeds the 50 MiB decoded limit" }
  decoded <- case B64.decode (TE.encodeUtf8 (T.strip uploadContentBase64)) of
    Left _ -> throwError err400 { errBody = "DDEX upload content is not valid base64" }
    Right value -> pure (BL.fromStrict value)
  _ <- either (throwError . unsafeXml) pure $
    Security.safeParseXml Security.defaultXmlParseConfig decoded
  detection <- maybe
    (throwError err422 { errBody = "The XML root and namespace do not identify a supported DDEX family" })
    pure
    (Detect.detectDocument decoded)
  (standardVersionId, messageTypeId, receivedStateId) <- resolveUploadReferences detection
  storage <- loadDdexStorage
  env <- ask
  let sha256 = Storage.computeSha256 decoded
  existing <- liftIO $ runSqlPool (DB.findDocumentBySha256 sha256) (envPool env)
  case existing of
    Just document -> loadDocumentDTOs [document] >>= singleDocumentDTO
    Nothing -> do
      stored <- liftIO $ Storage.storeFile storage fileName decoded uploadMimeType
      let namespace = nonEmpty (DDEXTypes.detectionNamespace detection)
          uploadActor = fromIntegral (fromSqlKey (auPartyId user))
      inserted <- liftIO $ runSqlPool
        (DB.insertDocument
          fileName
          (Storage.storedFileUri stored)
          sha256
          (fromIntegral (Storage.storedFileSize stored))
          standardVersionId
          messageTypeId
          receivedStateId
          namespace
          uploadActor
          Nothing
          Nothing
          Nothing)
        (envPool env)
      case inserted of
        Nothing -> do
          _ <- liftIO $ Storage.deleteFile storage (Storage.storedFilePath stored)
          raced <- liftIO $ runSqlPool (DB.findDocumentBySha256 sha256) (envPool env)
          maybe
            (throwError err409 { errBody = "A concurrent upload created this document; retry retrieval" })
            (\document -> loadDocumentDTOs [document] >>= singleDocumentDTO)
            raced
        Just documentId -> do
          created <- liftIO $ runSqlPool (DB.getDocumentById documentId) (envPool env)
          maybe
            (throwError err500 { errBody = "Stored DDEX document could not be reloaded" })
            (\document -> loadDocumentDTOs [document] >>= singleDocumentDTO)
            created

-- | List DDEX documents
listDocumentsHandler :: AuthedUser -> Maybe Text -> AppM [DdexDocumentDTO]
listDocumentsHandler user mWorkflowStateId = do
  requireDdexCapability user "catalog.read"
  env <- ask
  stateId <- traverse parseCanonicalWorkflowStateId mWorkflowStateId
  docEntities <- liftIO $ runSqlPool (DB.listDocuments stateId) (envPool env)
  loadDocumentDTOs docEntities

-- | Get a single DDEX document
getDocumentHandler :: AuthedUser -> Int -> AppM DdexDocumentDTO
getDocumentHandler user docId = do
  requireDdexCapability user "catalog.read"
  env <- ask
  mDocEntity <- liftIO $ runSqlPool (DB.getDocumentById (toSqlKey (fromIntegral docId))) (envPool env)
  case mDocEntity of
    Nothing -> throwError err404 { errBody = "Document not found" }
    Just docEntity -> do
      documents <- loadDocumentDTOs [docEntity]
      case documents of
        [document] -> pure document
        _ -> throwError err500 { errBody = "Unable to resolve canonical DDEX references" }

ensureDocumentExists :: Int -> AppM ()
ensureDocumentExists docId = do
  env <- ask
  documentExists <- liftIO $ runSqlPool
    (get (toSqlKey (fromIntegral docId) :: M.DdexDocumentId))
    (envPool env)
  when (isNothing documentExists) $
    throwError err404 { errBody = "Document not found" }

-- | Download raw XML file
downloadRawHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadRawHandler user docId = do
  requireDdexCapability user "catalog.read"
  (document, content) <- loadStoredDocument docId
  pure DdexDownloadResponse
    { downloadFileName = M.ddexDocumentFileName document
    , downloadContentType = "application/xml"
    , downloadContentBase64 = TE.decodeUtf8 (B64.encode (BL.toStrict content))
    }

-- | Validate a document
validateDocumentHandler :: AuthedUser -> Int -> AppM ValidationRunDTO
validateDocumentHandler user docId = do
  requireDdexCapability user "catalog.import"
  (document, content) <- loadStoredDocument docId
  standard <- loadDocumentStandard document
  env <- ask
  (runningState, failedState, warningState, invalidState, mappingState) <-
    loadValidationStates
  now <- liftIO getCurrentTime
  runId <- liftIO $ runSqlPool
    (DB.insertValidationRun
      (toSqlKey (fromIntegral docId))
      (entityKey runningState)
      (Just "tdf-structural-2")
      (Just (Catalog.ddexStandardVersionVersionCode standard)))
    (envPool env)
  let internalIssues = validateStoredErn standard docId content
      profileIssue = InternalValidationIssue
        DDEXTypes.SeverityWarning
        DDEXTypes.LayerXSD
        "PROFILE_VALIDATION_REQUIRED"
        "Official DDEX XSD and recipient business-profile validation has not run"
        (Just "Configure licensed schemas and an approved recipient profile before import or delivery")
      issues = internalIssues ++ [profileIssue]
      errorCount = length [() | issue <- issues, iviSeverity issue == DDEXTypes.SeverityError]
      warningCount = length [() | issue <- issues, iviSeverity issue == DDEXTypes.SeverityWarning]
      (resultCode, legacyResult, finalValidationState, finalDocumentState)
        | errorCount > 0 = ("failure", M.ResultFailure, failedState, invalidState)
        | otherwise = ("warning", M.ResultWarning, warningState, mappingState)
  resultId <- loadValidationResultId resultCode
  liftIO $ runSqlPool (do
    prepareDocumentValidation (toSqlKey (fromIntegral docId)) document
    mapM_ (persistInternalIssue runId) issues
    DB.completeValidationRun
      runId
      (entityKey finalValidationState)
      resultId
      legacyResult
      errorCount
      warningCount
    DB.updateDocumentStatus (toSqlKey (fromIntegral docId)) (entityKey finalDocumentState))
    (envPool env)
  finished <- liftIO getCurrentTime
  let finalState = entityVal finalValidationState
  pure ValidationRunDTO
    { validationRunId = fromIntegral $ fromSqlKey runId
    , validationRunDocumentId = docId
    , validationRunWorkflowStateId = toPathPiece (entityKey finalValidationState)
    , validationRunWorkflowStateCode = Catalog.workflowStateCode finalState
    , validationRunWorkflowStateNameEs = Catalog.workflowStateNameEs finalState
    , validationRunWorkflowStateNameEn = Catalog.workflowStateNameEn finalState
    , validationRunStartedAt = now
    , validationRunFinishedAt = Just finished
    }

-- | Get validation report
getValidationReportHandler :: AuthedUser -> Int -> AppM ValidationReportDTO
getValidationReportHandler user docId = do
  requireDdexCapability user "catalog.read"
  env <- ask
  let documentId = toSqlKey (fromIntegral docId)
  documentExists <- liftIO $ runSqlPool (get documentId) (envPool env)
  when (isNothing documentExists) $
    throwError err404 { errBody = "Document not found" }
  mReport <- liftIO $ runSqlPool (DB.getValidationReport documentId) (envPool env)
  case mReport of
    Nothing -> throwError err404 { errBody = "Validation report not found" }
    Just (runEntity, issues) -> return ValidationReportDTO
      { reportRunId = fromIntegral $ fromSqlKey (entityKey runEntity)
      , reportIssues = map issueToDTO issues
      , reportIsValid = M.ddexValidationRunResult (entityVal runEntity) == Just M.ResultSuccess
      }

-- | Get document preview
getPreviewHandler :: AuthedUser -> Int -> AppM DdexPreviewDTO
getPreviewHandler user docId = do
  requireDdexCapability user "catalog.read"
  (document, content) <- loadStoredDocument docId
  standard <- loadDocumentStandard document
  unless
    (Catalog.ddexStandardVersionStandardCode standard == "ERN"
      && Catalog.ddexStandardVersionVersionCode standard == "4.3.2") $
    throwError err503 { errBody = "Preview is enabled only for the validated ERN 4.3.2 profile" }
  parsed <- either (throwError . parseFailure) pure (Parse.parseErnMessage content)
  normalized <- either (throwError . normalizeFailure) pure (Normalize.normalizeErnMessage docId parsed)
  pure DdexPreviewDTO
    { previewMessageId = ERN.mhMessageId (ERN.ernMessageHeader parsed)
    , previewSender = T.pack (show (ERN.mhSenderPartyId (ERN.ernMessageHeader parsed)))
    , previewReleaseCount = length (Normalize.ciReleases normalized)
    , previewResourceCount = length (Normalize.ciResources normalized)
    , previewWarnings =
        [ "Preview only: no catalog records, deliveries, or statuses were changed."
        , "Official XSD and recipient-profile validation must pass before import."
        ]
    }

-- | Create import plan
createImportPlanHandler :: AuthedUser -> Int -> AppM ImportPlanDTO
createImportPlanHandler user docId = do
  requireDdexCapability user "catalog.import"
  ensureDocumentExists docId
  capabilityDisabled "DDEX import planning" "official schema/profile validation and the transactional conflict engine are not enabled"

-- | Resolve import plan conflicts
resolveImportPlanHandler :: AuthedUser -> Int -> ImportPlanResolution -> AppM ImportPlanDTO
resolveImportPlanHandler user _ _ = do
  requireDdexCapability user "catalog.import"
  capabilityDisabled "DDEX import resolution" "the transactional conflict engine is not enabled"

-- | Commit import plan
commitImportPlanHandler :: AuthedUser -> Int -> AppM ImportRunDTO
commitImportPlanHandler user _ = do
  requireDdexCapability user "catalog.import"
  capabilityDisabled "DDEX import commit" "rollback-tested catalog import is not enabled"

-- | Create export
createExportHandler :: AuthedUser -> DdexExportRequest -> AppM DdexExportDTO
createExportHandler user req = do
  requireDdexCapability user "catalog.export"
  standardVersionId <- parseCanonicalStandardVersionId (exportStandardVersionId req)
  env <- ask
  exportAllowed <- liftIO $ runSqlPool (partnerAllowsExportVersion (toSqlKey (fromIntegral (exportPartnerId req))) standardVersionId) (envPool env)
  unless exportAllowed $
    throwError err422 { errBody = "Partner does not allow an active export-enabled DDEX standard version" }
  capabilityDisabled "DDEX export" "a validated recipient profile, sender DPID, and private package store are required"

-- | Download export
downloadExportHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadExportHandler user _exportId = do
  requireDdexCapability user "catalog.export"
  capabilityDisabled "DDEX export download" "no immutable validated export package exists"

-- | List partners
listPartnersHandler :: AuthedUser -> AppM [DdexPartnerDTO]
listPartnersHandler user = do
  requireDdexCapability user "catalog.read"
  env <- ask
  partnerRows <- liftIO $ runSqlPool DB.listPartnersWithVersions (envPool env)
  supportRows <- liftIO $ runSqlPool (selectList [Catalog.DdexStandardSupportDeploymentCode ==. "default", Catalog.DdexStandardSupportActive ==. True] []) (envPool env)
  let supportMap = Map.fromList [(Catalog.ddexStandardSupportStandardVersionId value, value) | Entity _ value <- supportRows]
  pure [partnerEntityToDTO supportMap partner versions | (partner, versions) <- partnerRows]

-- | Create partner
createPartnerHandler :: AuthedUser -> DdexPartnerCreateRequest -> AppM DdexPartnerDTO
createPartnerHandler user req = do
  requireDdexCapability user "catalog.import"
  env <- ask
  let rawVersionIds = partnerAllowedStandardVersionIds req
  when (null rawVersionIds || length rawVersionIds /= length (nub rawVersionIds)) $
    throwError err400 { errBody = "partnerAllowedStandardVersionIds must contain unique canonical UUIDs" }
  versionIds <- traverse parseCanonicalStandardVersionId rawVersionIds
  validCount <- liftIO $ runSqlPool (countAllowedPartnerVersions versionIds) (envPool env)
  unless (validCount == length versionIds) $
    throwError err422 { errBody = "Every partner standard version must be active and enabled for detection" }
  partnerId <- liftIO $ runSqlPool (DB.insertPartner (T.strip (partnerName req)) (T.strip <$> partnerDpid req) versionIds) (envPool env)
  mPartner <- liftIO $ runSqlPool (get partnerId) (envPool env)
  case mPartner of
    Nothing -> throwError err500 { errBody = "Failed to create partner" }
    Just partner -> do
      rows <- liftIO $ runSqlPool DB.listPartnersWithVersions (envPool env)
      case [versions | (Entity key _, versions) <- rows, key == partnerId] of
        [versions] -> do
          supportRows <- liftIO $ runSqlPool (selectList [Catalog.DdexStandardSupportDeploymentCode ==. "default", Catalog.DdexStandardSupportActive ==. True] []) (envPool env)
          let supportMap = Map.fromList [(Catalog.ddexStandardSupportStandardVersionId value, value) | Entity _ value <- supportRows]
          pure (partnerToDTO supportMap partnerId partner versions)
        _ -> throwError err500 { errBody = "Failed to resolve partner standard versions" }

resolveUploadReferences
  :: DDEXTypes.DdexDetection
  -> AppM
       ( Catalog.DdexStandardVersionId
       , Maybe Catalog.DdexMessageTypeId
       , Catalog.WorkflowStateId
       )
resolveUploadReferences detection = do
  env <- ask
  let standardCode = DDEXTypes.familyToText (DDEXTypes.detectionFamily detection)
      versionCode = DDEXTypes.detectionVersion detection
      messageCode = T.takeWhileEnd (/= ':') (DDEXTypes.detectionRoot detection)
  result <- liftIO $ runSqlPool (do
    standard <- selectFirst
      [ Catalog.DdexStandardVersionStandardCode ==. standardCode
      , Catalog.DdexStandardVersionVersionCode ==. versionCode
      , Catalog.DdexStandardVersionActive ==. True
      ]
      []
    case standard of
      Nothing -> pure Nothing
      Just (Entity standardId _) -> do
        support <- selectFirst
          [ Catalog.DdexStandardSupportStandardVersionId ==. standardId
          , Catalog.DdexStandardSupportDeploymentCode ==. "default"
          , Catalog.DdexStandardSupportDetectionEnabled ==. True
          , Catalog.DdexStandardSupportActive ==. True
          ]
          []
        message <- selectFirst
          [ Catalog.DdexMessageTypeStandardVersionId ==. standardId
          , Catalog.DdexMessageTypeCode ==. messageCode
          , Catalog.DdexMessageTypeRuntimeSupported ==. True
          , Catalog.DdexMessageTypeActive ==. True
          ]
          []
        received <- findWorkflowState "ddex-document-lifecycle" "received"
        pure $ case (support, received) of
          (Just _, Just (Entity receivedId _)) ->
            Just (standardId, entityKey <$> message, receivedId)
          _ -> Nothing)
    (envPool env)
  maybe
    (throwError err422
      { errBody = "Detected DDEX standard is not enabled in the governed runtime profile" })
    pure
    result

singleDocumentDTO :: [DdexDocumentDTO] -> AppM DdexDocumentDTO
singleDocumentDTO [document] = pure document
singleDocumentDTO _ = throwError err500
  { errBody = "Unable to resolve canonical DDEX document references" }

loadValidationStates
  :: AppM
       ( Entity Catalog.WorkflowState
       , Entity Catalog.WorkflowState
       , Entity Catalog.WorkflowState
       , Entity Catalog.WorkflowState
       , Entity Catalog.WorkflowState
       )
loadValidationStates = do
  env <- ask
  states <- liftIO $ runSqlPool
    ((,,,,)
      <$> requireWorkflowState "ddex-validation-lifecycle" "running"
      <*> requireWorkflowState "ddex-validation-lifecycle" "failed"
      <*> requireWorkflowState "ddex-validation-lifecycle" "warning"
      <*> requireWorkflowState "ddex-document-lifecycle" "invalid"
      <*> requireWorkflowState "ddex-document-lifecycle" "mapping_required")
    (envPool env)
  pure states

loadValidationResultId :: Text -> AppM M.DdexValidationResultId
loadValidationResultId code = do
  env <- ask
  result <- liftIO $ runSqlPool
    (getBy (M.UniqueDdexValidationResultCode code))
    (envPool env)
  case result of
    Just (Entity resultId value) | M.ddexValidationResultActive value -> pure resultId
    _ -> throwError err503
      { errBody = "The governed DDEX validation-result registry is unavailable" }

findWorkflowState
  :: Text
  -> Text
  -> SqlPersistT IO (Maybe (Entity Catalog.WorkflowState))
findWorkflowState workflowCode stateCode = do
  workflow <- getBy (Catalog.UniqueWorkflowDefinitionCode workflowCode)
  case workflow of
    Just (Entity workflowId value) | Catalog.workflowDefinitionActive value -> do
      state <- getBy (Catalog.UniqueWorkflowStateCode workflowId stateCode)
      pure $ case state of
        Just row | Catalog.workflowStateActive (entityVal row) -> Just row
        _ -> Nothing
    _ -> pure Nothing

requireWorkflowState
  :: Text
  -> Text
  -> SqlPersistT IO (Entity Catalog.WorkflowState)
requireWorkflowState workflowCode stateCode = do
  state <- findWorkflowState workflowCode stateCode
  maybe
    (liftIO . ioError . userError $
      "Missing active workflow state " <> T.unpack workflowCode <> "/" <> T.unpack stateCode)
    pure
    state

prepareDocumentValidation
  :: M.DdexDocumentId
  -> M.DdexDocument
  -> SqlPersistT IO ()
prepareDocumentValidation documentId document = do
  currentStateId <- maybe
    (liftIO . ioError . userError $ "DDEX document has no governed workflow state")
    pure
    (M.ddexDocumentWorkflowStateId document)
  currentState <- get currentStateId >>= maybe
    (liftIO . ioError . userError $ "DDEX document references a missing workflow state")
    pure
  queued <- requireWorkflowState "ddex-document-lifecycle" "queued"
  validating <- requireWorkflowState "ddex-document-lifecycle" "validating"
  case Catalog.workflowStateCode currentState of
    "received" -> queueThenValidate queued validating
    "quarantined" -> queueThenValidate queued validating
    "invalid" -> queueThenValidate queued validating
    "queued" -> DB.updateDocumentStatus documentId (entityKey validating)
    "validating" -> pure ()
    _ -> liftIO . ioError . userError $
      "DDEX document cannot be revalidated from its current workflow state"
  where
    queueThenValidate queued validating = do
      DB.updateDocumentStatus documentId (entityKey queued)
      DB.updateDocumentStatus documentId (entityKey validating)

loadDdexStorage :: AppM Storage.StorageBackend
loadDdexStorage = do
  backend <- liftIO $ lookupEnv "DDEX_STORAGE_BACKEND"
  root <- liftIO $ lookupEnv "DDEX_PRIVATE_STORAGE_ROOT"
  case (backend, root) of
    (Just "local-private", Just storageRoot)
      | isAbsolute storageRoot && not (null storageRoot) ->
          pure $ Storage.localStorageBackend Storage.StorageConfig
            { Storage.storageBasePath = storageRoot
            , Storage.storageBucket = "ddex-private"
            }
    _ -> capabilityDisabled
      "DDEX private storage"
      "set DDEX_STORAGE_BACKEND=local-private and an absolute DDEX_PRIVATE_STORAGE_ROOT for staging; production object storage is not configured"

loadStoredDocument :: Int -> AppM (M.DdexDocument, BL.ByteString)
loadStoredDocument docId = do
  env <- ask
  row <- liftIO $ runSqlPool
    (DB.getDocumentById (toSqlKey (fromIntegral docId)))
    (envPool env)
  document <- maybe
    (throwError err404 { errBody = "Document not found" })
    (pure . entityVal)
    row
  relativePath <- case T.stripPrefix "local-private://" (M.ddexDocumentPrivateUri document) of
    Just path | not (T.null path) -> pure (T.unpack path)
    _ -> capabilityDisabled
      "DDEX document retrieval"
      "the document uses an unavailable private-storage adapter"
  storage <- loadDdexStorage
  content <- liftIO $ Storage.retrieveFile storage relativePath
  bytes <- maybe
    (throwError err404 { errBody = "Private DDEX object is unavailable" })
    pure
    content
  unless (Storage.computeSha256 bytes == M.ddexDocumentSha256 document) $
    throwError err500 { errBody = "Private DDEX object checksum verification failed" }
  pure (document, bytes)

loadDocumentStandard :: M.DdexDocument -> AppM Catalog.DdexStandardVersion
loadDocumentStandard document = do
  standardId <- maybe
    (throwError err500 { errBody = "DDEX document is missing standardVersionId" })
    pure
    (M.ddexDocumentStandardVersionId document)
  env <- ask
  standard <- liftIO $ runSqlPool (get standardId) (envPool env)
  maybe
    (throwError err500 { errBody = "DDEX document references an unknown standard version" })
    pure
    standard

validateUploadName :: Text -> Either Text Text
validateUploadName rawName =
  let cleanName = T.strip rawName
      fileNameOnly = T.pack (takeFileName (T.unpack cleanName))
  in if T.null cleanName
        || T.length cleanName > 255
        || fileNameOnly /= cleanName
        || T.any isControl cleanName
        || not (".xml" `T.isSuffixOf` T.toLower cleanName)
      then Left "DDEX filename must be a safe .xml basename of at most 255 characters"
      else Right cleanName

nonEmpty :: Text -> Maybe Text
nonEmpty value
  | T.null (T.strip value) = Nothing
  | otherwise = Just (T.strip value)

invalidUpload :: Text -> ServerError
invalidUpload message = err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

unsafeXml :: [Security.XmlSecurityError] -> ServerError
unsafeXml errors = err422
  { errBody = BL.fromStrict (TE.encodeUtf8 ("Unsafe or malformed XML: " <> T.pack (show errors))) }

parseFailure :: [Parse.ParseError] -> ServerError
parseFailure errors = err422
  { errBody = BL.fromStrict (TE.encodeUtf8 ("ERN parsing failed: " <> T.pack (show errors))) }

normalizeFailure :: [Normalize.NormalizationError] -> ServerError
normalizeFailure errors = err422
  { errBody = BL.fromStrict (TE.encodeUtf8 ("ERN normalization failed: " <> T.pack (show errors))) }

capabilityDisabled :: Text -> Text -> AppM a
capabilityDisabled capability blocker =
  throwError err503
    { errBody = BL.fromStrict . TE.encodeUtf8 $
        capability <> " is feature-disabled: " <> blocker
    }

data InternalValidationIssue = InternalValidationIssue
  { iviSeverity   :: DDEXTypes.ValidationSeverity
  , iviLayer      :: DDEXTypes.ValidationLayer
  , iviCode       :: Text
  , iviMessage    :: Text
  , iviSuggestion :: Maybe Text
  }

validateStoredErn
  :: Catalog.DdexStandardVersion
  -> Int
  -> BL.ByteString
  -> [InternalValidationIssue]
validateStoredErn standard docId content
  | Catalog.ddexStandardVersionStandardCode standard /= "ERN" =
      [unsupported "FAMILY_NOT_IMPLEMENTED" "Only ERN intake validation is implemented"]
  | Catalog.ddexStandardVersionVersionCode standard /= "4.3.2" =
      [unsupported "ERN_PROFILE_NOT_IMPLEMENTED" "Only ERN 4.3.2 intake validation is implemented"]
  | otherwise = case Parse.parseErnMessage content of
      Left errors -> map parseIssue errors
      Right ern -> case Normalize.normalizeErnMessage docId ern of
        Left errors -> map normalizationIssue errors
        Right normalized -> map businessIssue (BusinessRules.validateBusinessRules normalized)
  where
    unsupported code message = InternalValidationIssue
      DDEXTypes.SeverityError DDEXTypes.LayerBusiness code message Nothing
    parseIssue issue = InternalValidationIssue
      DDEXTypes.SeverityError
      DDEXTypes.LayerXML
      "ERN_PARSE_ERROR"
      (Parse.peMessage issue)
      Nothing
    normalizationIssue issue = InternalValidationIssue
      DDEXTypes.SeverityError
      DDEXTypes.LayerBusiness
      "ERN_NORMALIZATION_ERROR"
      (Normalize.neMessage issue)
      Nothing
    businessIssue violation = InternalValidationIssue
      (case BusinessRules.brvSeverity violation of
        BusinessRules.RuleError -> DDEXTypes.SeverityError
        BusinessRules.RuleWarning -> DDEXTypes.SeverityWarning
        BusinessRules.RuleInfo -> DDEXTypes.SeverityInfo)
      DDEXTypes.LayerBusiness
      (BusinessRules.brvRule violation)
      (BusinessRules.brvMessage violation)
      (BusinessRules.brvSuggestion violation)

persistInternalIssue
  :: M.DdexValidationRunId
  -> InternalValidationIssue
  -> SqlPersistT IO ()
persistInternalIssue runId issue = do
  _ <- DB.insertValidationIssue
    runId
    (iviSeverity issue)
    (iviLayer issue)
    (Just (iviCode issue))
    Nothing
    Nothing
    Nothing
    (iviMessage issue)
    (iviSuggestion issue)
  pure ()

-- ============================================================
-- Conversion helpers
-- ============================================================

parseCanonicalWorkflowStateId :: Text -> AppM Catalog.WorkflowStateId
parseCanonicalWorkflowStateId = parseCanonicalId "workflowStateId"

parseCanonicalStandardVersionId :: Text -> AppM Catalog.DdexStandardVersionId
parseCanonicalStandardVersionId = parseCanonicalId "standardVersionId"

parseCanonicalId :: PathPiece key => Text -> Text -> AppM key
parseCanonicalId fieldName raw =
  let normalized = T.strip raw
  in case fromPathPiece normalized of
    Just key | toPathPiece key == normalized -> pure key
    _ -> throwError err400
      { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a canonical lowercase UUID"))
      }

countAllowedPartnerVersions :: [Catalog.DdexStandardVersionId] -> SqlPersistT IO Int
countAllowedPartnerVersions versionIds = do
  versions <- selectList
    [ Catalog.DdexStandardVersionId <-. versionIds
    , Catalog.DdexStandardVersionActive ==. True
    ]
    []
  supports <- selectList
    [ Catalog.DdexStandardSupportStandardVersionId <-. map entityKey versions
    , Catalog.DdexStandardSupportDeploymentCode ==. "default"
    , Catalog.DdexStandardSupportActive ==. True
    , Catalog.DdexStandardSupportDetectionEnabled ==. True
    ]
    []
  pure (length supports)

partnerAllowsExportVersion :: M.DdexPartnerId -> Catalog.DdexStandardVersionId -> SqlPersistT IO Bool
partnerAllowsExportVersion partnerId standardVersionId = do
  partner <- get partnerId
  membership <- selectFirst
    [ M.DdexPartnerStandardVersionPartnerId ==. partnerId
    , M.DdexPartnerStandardVersionStandardVersionId ==. standardVersionId
    , M.DdexPartnerStandardVersionActive ==. True
    ]
    []
  standardVersion <- get standardVersionId
  support <- selectFirst
    [ Catalog.DdexStandardSupportStandardVersionId ==. standardVersionId
    , Catalog.DdexStandardSupportDeploymentCode ==. "default"
    , Catalog.DdexStandardSupportActive ==. True
    , Catalog.DdexStandardSupportExportEnabled ==. True
    ]
    []
  pure $
    maybe False M.ddexPartnerIsActive partner
      && maybe False Catalog.ddexStandardVersionActive standardVersion
      && maybe False (const True) membership
      && maybe False (const True) support

loadDocumentDTOs :: [Entity M.DdexDocument] -> AppM [DdexDocumentDTO]
loadDocumentDTOs documents = do
  let standardVersionIds = nub (catMaybes (map (M.ddexDocumentStandardVersionId . entityVal) documents))
      messageTypeIds = nub (catMaybes (map (M.ddexDocumentMessageTypeId . entityVal) documents))
      workflowStateIds = nub (catMaybes (map (M.ddexDocumentWorkflowStateId . entityVal) documents))
  env <- ask
  (standards, messageTypes, workflowStates) <- liftIO $ runSqlPool
    ((,,)
      <$> selectList [Catalog.DdexStandardVersionId <-. standardVersionIds] []
      <*> selectList [Catalog.DdexMessageTypeId <-. messageTypeIds] []
      <*> selectList [Catalog.WorkflowStateId <-. workflowStateIds] [])
    (envPool env)
  let standardMap = Map.fromList [(entityKey row, entityVal row) | row <- standards]
      messageTypeMap = Map.fromList [(entityKey row, entityVal row) | row <- messageTypes]
      workflowStateMap = Map.fromList [(entityKey row, entityVal row) | row <- workflowStates]
  forM documents $ \document ->
    either
      (\message -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 message) })
      pure
      (documentEntityToDTO standardMap messageTypeMap workflowStateMap document)

documentEntityToDTO
  :: Map.Map Catalog.DdexStandardVersionId Catalog.DdexStandardVersion
  -> Map.Map Catalog.DdexMessageTypeId Catalog.DdexMessageType
  -> Map.Map Catalog.WorkflowStateId Catalog.WorkflowState
  -> Entity M.DdexDocument
  -> Either Text DdexDocumentDTO
documentEntityToDTO standardMap messageTypeMap workflowStateMap docEntity = do
  let doc = entityVal docEntity
      docId = entityKey docEntity
  standardVersionId <- maybe (Left "DDEX document is missing standardVersionId") Right (M.ddexDocumentStandardVersionId doc)
  standardVersion <- maybe (Left "DDEX document references an unknown standard version") Right (Map.lookup standardVersionId standardMap)
  workflowStateId <- maybe (Left "DDEX document is missing workflowStateId") Right (M.ddexDocumentWorkflowStateId doc)
  workflowState <- maybe (Left "DDEX document references an unknown workflow state") Right (Map.lookup workflowStateId workflowStateMap)
  let messageTypeId = M.ddexDocumentMessageTypeId doc
      messageTypeCode = messageTypeId >>= (Catalog.ddexMessageTypeCode <$>) . (`Map.lookup` messageTypeMap)
  pure DdexDocumentDTO
    { ddexDocumentId = fromIntegral $ fromSqlKey docId
    , ddexDocumentFileName = M.ddexDocumentFileName doc
    , ddexDocumentSha256 = M.ddexDocumentSha256 doc
    , ddexDocumentStandardVersionId = toPathPiece standardVersionId
    , ddexDocumentStandardCode = Catalog.ddexStandardVersionStandardCode standardVersion
    , ddexDocumentVersionCode = Catalog.ddexStandardVersionVersionCode standardVersion
    , ddexDocumentMessageTypeId = toPathPiece <$> messageTypeId
    , ddexDocumentMessageTypeCode = messageTypeCode
    , ddexDocumentWorkflowStateId = toPathPiece workflowStateId
    , ddexDocumentWorkflowStateCode = Catalog.workflowStateCode workflowState
    , ddexDocumentWorkflowStateNameEs = Catalog.workflowStateNameEs workflowState
    , ddexDocumentWorkflowStateNameEn = Catalog.workflowStateNameEn workflowState
    , ddexDocumentMessageId = M.ddexDocumentMessageId doc
    , ddexDocumentSenderId = M.ddexDocumentSenderId doc
    , ddexDocumentRecipientId = M.ddexDocumentRecipientId doc
    , ddexDocumentCreatedAt = M.ddexDocumentCreatedAt doc
    }

-- | Convert DdexValidationIssue to ValidationIssueDTO
issueToDTO
  :: (M.DdexValidationIssue, Entity M.DdexValidationSeverity, Entity M.DdexValidationLayer)
  -> ValidationIssueDTO
issueToDTO (issue, Entity severityId severity, Entity layerId layer) = ValidationIssueDTO
  { issueSeverityId = toPathPiece severityId
  , issueSeverityCode = M.ddexValidationSeverityCode severity
  , issueSeverityNameEs = M.ddexValidationSeverityNameEs severity
  , issueSeverityNameEn = M.ddexValidationSeverityNameEn severity
  , issueLayerId = toPathPiece layerId
  , issueLayerCode = M.ddexValidationLayerCode layer
  , issueLayerNameEs = M.ddexValidationLayerNameEs layer
  , issueLayerNameEn = M.ddexValidationLayerNameEn layer
  , issueCode = maybe "" id (M.ddexValidationIssueCode issue)
  , issueMessage = M.ddexValidationIssueMessage issue
  , issueLine = M.ddexValidationIssueLineNumber issue
  , issueColumn = M.ddexValidationIssueColumnNumber issue
  }

partnerEntityToDTO
  :: Map.Map Catalog.DdexStandardVersionId Catalog.DdexStandardSupport
  -> Entity M.DdexPartner
  -> [Entity Catalog.DdexStandardVersion]
  -> DdexPartnerDTO
partnerEntityToDTO supportMap (Entity partnerId partner) =
  partnerToDTO supportMap partnerId partner

-- | Convert DdexPartner with key to DdexPartnerDTO
partnerToDTO
  :: Map.Map Catalog.DdexStandardVersionId Catalog.DdexStandardSupport
  -> M.DdexPartnerId
  -> M.DdexPartner
  -> [Entity Catalog.DdexStandardVersion]
  -> DdexPartnerDTO
partnerToDTO supportMap partnerId partner versions = DdexPartnerDTO
  { ddexPartnerId = fromIntegral $ fromSqlKey partnerId
  , ddexPartnerName = M.ddexPartnerName partner
  , ddexPartnerDpid = M.ddexPartnerDpid partner
  , ddexPartnerAllowedStandardVersions = map (standardVersionDTO "es" supportMap) versions
  }
