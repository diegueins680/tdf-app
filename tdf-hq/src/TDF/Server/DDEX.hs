{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.DDEX (ddexServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (get, entityKey, entityVal, Entity)
import Database.Persist.Sql (runSqlPool, toSqlKey, fromSqlKey)
import Servant
import TDF.API.DDEX
import TDF.Auth (AuthedUser, validateModuleAccess, ModuleAccess(..))
import TDF.DB (Env(..))
import qualified TDF.DDEX.DB as DB
import qualified TDF.DDEX.Models as M

type AppM = ReaderT Env Handler

-- | Main DDEX Server Implementation
ddexServer :: AuthedUser -> ServerT DDEXAPI AppM
ddexServer user =
       uploadDocumentHandler user
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
  :<|> getCatalogByDocumentHandler user

-- | Upload a DDEX document
uploadDocumentHandler :: AuthedUser -> DdexUploadRequest -> AppM DdexDocumentDTO
uploadDocumentHandler user _req = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  -- TODO: Implement actual file storage and SHA-256 calculation
  throwError err501 { errBody = "Not Implemented: Upload requires file storage integration" }

-- | List DDEX documents
listDocumentsHandler :: AuthedUser -> Maybe Text -> Maybe Text -> AppM [DdexDocumentDTO]
listDocumentsHandler user mStatus mPartner = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask
  docEntities <- liftIO $ runSqlPool (DB.listDocuments mStatus mPartner) (envPool env)
  return $ map documentEntityToDTO docEntities

-- | Get a single DDEX document
getDocumentHandler :: AuthedUser -> Int -> AppM DdexDocumentDTO
getDocumentHandler user docId = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask
  mDocEntity <- liftIO $ runSqlPool (DB.getDocumentById (toSqlKey (fromIntegral docId))) (envPool env)
  case mDocEntity of
    Nothing -> throwError err404 { errBody = "Document not found" }
    Just docEntity -> return $ documentEntityToDTO docEntity

-- | Download raw XML file
downloadRawHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadRawHandler _user _ = throwError err501 { errBody = "Not Implemented: Download Raw" }

-- | Validate a document
validateDocumentHandler :: AuthedUser -> Int -> AppM ValidationRunDTO
validateDocumentHandler user docId = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask
  runId <- liftIO $ runSqlPool (DB.insertValidationRun (toSqlKey (fromIntegral docId)) (Just "1.0") Nothing) (envPool env)
  return ValidationRunDTO
    { validationRunId = fromIntegral $ fromSqlKey runId
    , validationRunDocumentId = docId
    , validationRunStatus = "pending"
    , validationRunStartedAt = read "2026-01-01 00:00:00 UTC"
    , validationRunFinishedAt = Nothing
    }

-- | Get validation report
getValidationReportHandler :: AuthedUser -> Int -> AppM ValidationReportDTO
getValidationReportHandler user docId = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask
  mReport <- liftIO $ runSqlPool (DB.getValidationReport (toSqlKey (fromIntegral docId))) (envPool env)
  case mReport of
    Nothing -> return ValidationReportDTO
      { reportRunId = 0
      , reportIssues = []
      , reportIsValid = True
      }
    Just (runEntity, issues) -> return ValidationReportDTO
      { reportRunId = fromIntegral $ fromSqlKey (entityKey runEntity)
      , reportIssues = map issueToDTO issues
      , reportIsValid = M.ddexValidationRunErrorCount (entityVal runEntity) == 0
      }

-- | Get document preview
getPreviewHandler :: AuthedUser -> Int -> AppM DdexPreviewDTO
getPreviewHandler _user _ = throwError err501 { errBody = "Not Implemented: Preview" }

-- | Create import plan
createImportPlanHandler :: AuthedUser -> Int -> AppM ImportPlanDTO
createImportPlanHandler user docId = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask
  planId <- liftIO $ runSqlPool (DB.insertImportPlan (toSqlKey (fromIntegral docId)) "{}") (envPool env)
  return ImportPlanDTO
    { importPlanId = fromIntegral $ fromSqlKey planId
    , importPlanDocumentId = docId
    , importPlanStatus = "draft"
    , importPlanConflicts = []
    , importPlanChanges = []
    }

-- | Resolve import plan conflicts
resolveImportPlanHandler :: AuthedUser -> Int -> ImportPlanResolution -> AppM ImportPlanDTO
resolveImportPlanHandler _user _ _ = throwError err501 { errBody = "Not Implemented: Resolve Plan" }

-- | Commit import plan
commitImportPlanHandler :: AuthedUser -> Int -> AppM ImportRunDTO
commitImportPlanHandler _user _ = throwError err501 { errBody = "Not Implemented: Commit Plan" }

-- | Create export
createExportHandler :: AuthedUser -> DdexExportRequest -> AppM DdexExportDTO
createExportHandler user req = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask
  now <- liftIO getCurrentTime

  -- TODO: Fetch catalog entities from database
  -- For now, return a stub response
  let exportId = 0  -- TODO: Insert into ddex_export table
      checksum = "placeholder-checksum"

  return DdexExportDTO
    { ddexExportId = exportId
    , ddexExportReleaseId = exportReleaseId req
    , ddexExportPartnerId = exportPartnerId req
    , ddexExportStatus = "pending"
    , ddexExportXmlChecksum = checksum
    , ddexExportCreatedAt = now
    }

-- | Download export
downloadExportHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadExportHandler user exportId = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask

  -- TODO: Fetch export from database and generate XML
  -- For now, return a stub response
  return DdexDownloadResponse
    { downloadFileName = "export-" <> T.pack (show exportId) <> ".xml"
    , downloadContentType = "application/xml"
    , downloadContentBase64 = ""  -- TODO: Generate actual XML
    }

-- | List partners
listPartnersHandler :: AuthedUser -> AppM [DdexPartnerDTO]
listPartnersHandler user = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask
  partnerEntities <- liftIO $ runSqlPool DB.listPartners (envPool env)
  return $ map partnerEntityToDTO partnerEntities

-- | Create partner
createPartnerHandler :: AuthedUser -> DdexPartnerCreateRequest -> AppM DdexPartnerDTO
createPartnerHandler user req = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  env <- ask
  partnerId <- liftIO $ runSqlPool (DB.insertPartner (partnerName req) (partnerDpid req) (partnerAllowedVersions req)) (envPool env)
  mPartner <- liftIO $ runSqlPool (get partnerId) (envPool env)
  case mPartner of
    Nothing -> throwError err500 { errBody = "Failed to create partner" }
    Just partner -> return $ partnerToDTO partnerId partner

-- | Get catalog releases by document
getCatalogByDocumentHandler :: AuthedUser -> Maybe Int -> AppM [CatalogReleaseDTO]
getCatalogByDocumentHandler _user _ = pure []

-- ============================================================
-- Conversion helpers
-- ============================================================

-- | Convert Entity DdexDocument to DdexDocumentDTO
documentEntityToDTO :: Entity M.DdexDocument -> DdexDocumentDTO
documentEntityToDTO docEntity =
  let doc = entityVal docEntity
      docId = entityKey docEntity
  in DdexDocumentDTO
    { ddexDocumentId = fromIntegral $ fromSqlKey docId
    , ddexDocumentFileName = M.ddexDocumentFileName doc
    , ddexDocumentSha256 = M.ddexDocumentSha256 doc
    , ddexDocumentFamily = T.pack $ show $ M.ddexDocumentFamily doc
    , ddexDocumentVersion = M.ddexDocumentVersion doc
    , ddexDocumentStatus = T.pack $ show $ M.ddexDocumentStatus doc
    , ddexDocumentMessageId = M.ddexDocumentMessageId doc
    , ddexDocumentSenderId = M.ddexDocumentSenderId doc
    , ddexDocumentRecipientId = M.ddexDocumentRecipientId doc
    , ddexDocumentCreatedAt = M.ddexDocumentCreatedAt doc
    }

-- | Convert DdexValidationIssue to ValidationIssueDTO
issueToDTO :: M.DdexValidationIssue -> ValidationIssueDTO
issueToDTO issue = ValidationIssueDTO
  { issueSeverity = T.pack $ show $ M.ddexValidationIssueSeverity issue
  , issueLayer = T.pack $ show $ M.ddexValidationIssueLayer issue
  , issueCode = maybe "" id (M.ddexValidationIssueCode issue)
  , issueMessage = M.ddexValidationIssueMessage issue
  , issueLine = M.ddexValidationIssueLineNumber issue
  , issueColumn = M.ddexValidationIssueColumnNumber issue
  }

-- | Convert Entity DdexPartner to DdexPartnerDTO
partnerEntityToDTO :: Entity M.DdexPartner -> DdexPartnerDTO
partnerEntityToDTO partnerEntity =
  let partner = entityVal partnerEntity
      partnerId = entityKey partnerEntity
  in DdexPartnerDTO
    { ddexPartnerId = fromIntegral $ fromSqlKey partnerId
    , ddexPartnerName = M.ddexPartnerName partner
    , ddexPartnerDpid = M.ddexPartnerDpid partner
    , ddexPartnerAllowedVersions = M.ddexPartnerAllowedVersions partner
    }

-- | Convert DdexPartner with key to DdexPartnerDTO
partnerToDTO :: M.DdexPartnerId -> M.DdexPartner -> DdexPartnerDTO
partnerToDTO partnerId partner = DdexPartnerDTO
  { ddexPartnerId = fromIntegral $ fromSqlKey partnerId
  , ddexPartnerName = M.ddexPartnerName partner
  , ddexPartnerDpid = M.ddexPartnerDpid partner
  , ddexPartnerAllowedVersions = M.ddexPartnerAllowedVersions partner
  }
