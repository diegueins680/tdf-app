{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TDF.Server.DDEX (ddexServer, validateDdexAccess) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.Set as Set
import Database.Persist (get, entityKey, entityVal, Entity)
import Database.Persist.Sql (runSqlPool, toSqlKey, fromSqlKey)
import Servant
import TDF.API.DDEX
import TDF.Auth (AuthedUser(..), validateModuleAccess, moduleName, ModuleAccess(..))
import TDF.DB (Env(..))
import qualified TDF.DDEX.DB as DB
import qualified TDF.DDEX.Models as M
import qualified TDF.DDEX.Types as DDEXTypes
import TDF.FeatureRegistry (findRegistryFeature, registryFeatureAllows)

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
  requireDdexAccess "label.ddex.inbox" "import" user
  -- TODO: Implement actual file storage and SHA-256 calculation
  throwError err501 { errBody = "Not Implemented: Upload requires file storage integration" }

-- | List DDEX documents
listDocumentsHandler :: AuthedUser -> Maybe Text -> Maybe Text -> AppM [DdexDocumentDTO]
listDocumentsHandler user mStatus mPartner = do
  requireDdexAccess "label.ddex.inbox" "view" user
  let validStatuses =
        [ "received", "quarantined", "queued", "validating", "invalid", "valid"
        , "mapping_required", "ready_to_import", "importing", "imported"
        , "import_failed", "superseded"
        ]
  when (maybe False (`notElem` validStatuses) mStatus) $
    throwError err400 { errBody = "Unsupported DDEX document status" }
  env <- ask
  docEntities <- liftIO $ runSqlPool (DB.listDocuments mStatus mPartner) (envPool env)
  return $ map documentEntityToDTO docEntities

-- | Get a single DDEX document
getDocumentHandler :: AuthedUser -> Int -> AppM DdexDocumentDTO
getDocumentHandler user docId = do
  requireDdexAccess "label.ddex.document" "view" user
  env <- ask
  mDocEntity <- liftIO $ runSqlPool (DB.getDocumentById (toSqlKey (fromIntegral docId))) (envPool env)
  case mDocEntity of
    Nothing -> throwError err404 { errBody = "Document not found" }
    Just docEntity -> return $ documentEntityToDTO docEntity

-- | Download raw XML file
downloadRawHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadRawHandler user _ = do
  requireDdexAccess "label.ddex.document" "view" user
  throwError err501 { errBody = "Not Implemented: Download Raw" }

-- | Validate a document
validateDocumentHandler :: AuthedUser -> Int -> AppM ValidationRunDTO
validateDocumentHandler user docId = do
  requireDdexAccess "label.ddex.document" "validate" user
  env <- ask
  mDocument <- liftIO $ runSqlPool (DB.getDocumentById (toSqlKey (fromIntegral docId))) (envPool env)
  case mDocument of
    Nothing -> throwError err404 { errBody = "Document not found" }
    Just _ -> pure ()
  now <- liftIO getCurrentTime
  runId <- liftIO $ runSqlPool (DB.insertValidationRun (toSqlKey (fromIntegral docId)) (Just "1.0") Nothing) (envPool env)
  return ValidationRunDTO
    { validationRunId = fromIntegral $ fromSqlKey runId
    , validationRunDocumentId = docId
    , validationRunStatus = "pending"
    , validationRunStartedAt = now
    , validationRunFinishedAt = Nothing
    }

-- | Get validation report
getValidationReportHandler :: AuthedUser -> Int -> AppM ValidationReportDTO
getValidationReportHandler user docId = do
  requireDdexAccess "label.ddex.document" "view" user
  env <- ask
  mReport <- liftIO $ runSqlPool (DB.getValidationReport (toSqlKey (fromIntegral docId))) (envPool env)
  case mReport of
    Nothing -> throwError err404 { errBody = "No validation report is available" }
    Just (runEntity, issues) -> return ValidationReportDTO
      { reportRunId = fromIntegral $ fromSqlKey (entityKey runEntity)
      , reportIssues = map issueToDTO issues
      , reportIsValid = M.ddexValidationRunErrorCount (entityVal runEntity) == 0
      }

-- | Get document preview
getPreviewHandler :: AuthedUser -> Int -> AppM DdexPreviewDTO
getPreviewHandler user _ = do
  requireDdexAccess "label.ddex.document" "view" user
  throwError err501 { errBody = "Not Implemented: Preview" }

-- | Create import plan
createImportPlanHandler :: AuthedUser -> Int -> AppM ImportPlanDTO
createImportPlanHandler user _docId = do
  requireDdexAccess "label.ddex.import" "import" user
  throwError err501 { errBody = "Not Implemented: Import plan generation" }

-- | Resolve import plan conflicts
resolveImportPlanHandler :: AuthedUser -> Int -> ImportPlanResolution -> AppM ImportPlanDTO
resolveImportPlanHandler user _ _ = do
  requireDdexAccess "label.ddex.import" "import" user
  throwError err501 { errBody = "Not Implemented: Resolve Plan" }

-- | Commit import plan
commitImportPlanHandler :: AuthedUser -> Int -> AppM ImportRunDTO
commitImportPlanHandler user _ = do
  requireDdexAccess "label.ddex.import" "approve" user
  throwError err501 { errBody = "Not Implemented: Commit Plan" }

-- | Create export
createExportHandler :: AuthedUser -> DdexExportRequest -> AppM DdexExportDTO
createExportHandler user _req = do
  requireDdexAccess "label.ddex.inbox" "export" user
  throwError err501 { errBody = "Not Implemented: DDEX export persistence and rendering" }

-- | Download export
downloadExportHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadExportHandler user exportId = do
  requireDdexAccess "label.ddex.inbox" "export" user
  let _ = exportId
  throwError err501 { errBody = "Not Implemented: DDEX export download" }

-- | List partners
listPartnersHandler :: AuthedUser -> AppM [DdexPartnerDTO]
listPartnersHandler user = do
  requireDdexAccess "label.ddex.partners" "view" user
  env <- ask
  partnerEntities <- liftIO $ runSqlPool DB.listPartners (envPool env)
  return $ map partnerEntityToDTO partnerEntities

-- | Create partner
createPartnerHandler :: AuthedUser -> DdexPartnerCreateRequest -> AppM DdexPartnerDTO
createPartnerHandler user req = do
  requireDdexAccess "label.ddex.partners" "create" user
  let cleanName = T.strip (partnerName req)
      cleanDpid = T.strip <$> partnerDpid req
      versions = partnerAllowedVersions req
      supportedVersions = ["3.8.2", "4.2", "4.3"]
      invalidText value maxLength = T.null value || T.length value > maxLength || T.any isControl value
  if invalidText cleanName 160
      || maybe False (\value -> invalidText value 200) cleanDpid
      || null versions
      || length versions > 10
      || length versions /= Set.size (Set.fromList versions)
      || any (`notElem` supportedVersions) versions
    then throwError err400 { errBody = "Invalid DDEX partner configuration" }
    else pure ()
  env <- ask
  mPartnerId <- liftIO $ runSqlPool (DB.insertPartner cleanName cleanDpid versions) (envPool env)
  case mPartnerId of
    Nothing -> throwError err409 { errBody = "DDEX partner already exists" }
    Just partnerId -> do
      mPartner <- liftIO $ runSqlPool (get partnerId) (envPool env)
      case mPartner of
        Nothing -> throwError err500 { errBody = "Failed to create partner" }
        Just partner -> return $ partnerToDTO partnerId partner

-- | Get catalog releases by document
getCatalogByDocumentHandler :: AuthedUser -> Maybe Int -> AppM [CatalogReleaseDTO]
getCatalogByDocumentHandler user _ = do
  requireDdexAccess "label.ddex.document" "view" user
  throwError err501 { errBody = "Not Implemented: DDEX catalog read-through" }

requireDdexAccess :: Text -> Text -> AuthedUser -> AppM ()
requireDdexAccess featureId action user =
  either throwError pure (validateDdexAccess featureId action user)

validateDdexAccess :: Text -> Text -> AuthedUser -> Either ServerError ()
validateDdexAccess featureId action user@AuthedUser{..} = do
  validateModuleAccess ModuleCatalog user
  case findRegistryFeature featureId of
    Nothing -> Left err500 { errBody = "DDEX feature authorization is not configured" }
    Just feature ->
      let modules = map moduleName (Set.toList auModules)
      in if registryFeatureAllows auRoles modules feature action
          then Right ()
          else Left err403 { errBody = "Feature action is not permitted" }

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
    , ddexDocumentFamily = DDEXTypes.familyToText . DB.fromFamilyEnum $ M.ddexDocumentFamily doc
    , ddexDocumentVersion = M.ddexDocumentVersion doc
    , ddexDocumentStatus = DDEXTypes.documentStatusToText . DB.fromStatusEnum $ M.ddexDocumentStatus doc
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
