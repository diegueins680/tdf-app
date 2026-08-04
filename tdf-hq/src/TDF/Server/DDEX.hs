{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.DDEX (ddexServer) where

import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import Servant
import TDF.API.DDEX
import TDF.Auth (AuthedUser, validateModuleAccess, ModuleAccess(..))
import TDF.DB (Env)

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

-- Placeholder handlers returning stubs or errors
-- In full implementation, these would contain business logic

uploadDocumentHandler :: AuthedUser -> DdexUploadRequest -> AppM DdexDocumentDTO
uploadDocumentHandler user _req = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  throwError err501 { errBody = "Not Implemented: Upload" }

listDocumentsHandler :: AuthedUser -> Maybe Text -> Maybe Text -> AppM [DdexDocumentDTO]
listDocumentsHandler _user _status _partner = pure []

getDocumentHandler :: AuthedUser -> Int -> AppM DdexDocumentDTO
getDocumentHandler _user _ = throwError err501 { errBody = "Not Implemented: Get Document" }

downloadRawHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadRawHandler _user _ = throwError err501 { errBody = "Not Implemented: Download Raw" }

validateDocumentHandler :: AuthedUser -> Int -> AppM ValidationRunDTO
validateDocumentHandler _user _ = throwError err501 { errBody = "Not Implemented: Validate" }

getValidationReportHandler :: AuthedUser -> Int -> AppM ValidationReportDTO
getValidationReportHandler _user _ = throwError err501 { errBody = "Not Implemented: Get Report" }

getPreviewHandler :: AuthedUser -> Int -> AppM DdexPreviewDTO
getPreviewHandler _user _ = throwError err501 { errBody = "Not Implemented: Preview" }

createImportPlanHandler :: AuthedUser -> Int -> AppM ImportPlanDTO
createImportPlanHandler _user _ = throwError err501 { errBody = "Not Implemented: Create Plan" }

resolveImportPlanHandler :: AuthedUser -> Int -> ImportPlanResolution -> AppM ImportPlanDTO
resolveImportPlanHandler _user _ _ = throwError err501 { errBody = "Not Implemented: Resolve Plan" }

commitImportPlanHandler :: AuthedUser -> Int -> AppM ImportRunDTO
commitImportPlanHandler _user _ = throwError err501 { errBody = "Not Implemented: Commit Plan" }

createExportHandler :: AuthedUser -> DdexExportRequest -> AppM DdexExportDTO
createExportHandler _user _ = throwError err501 { errBody = "Not Implemented: Create Export" }

downloadExportHandler :: AuthedUser -> Int -> AppM DdexDownloadResponse
downloadExportHandler _user _ = throwError err501 { errBody = "Not Implemented: Download Export" }

listPartnersHandler :: AuthedUser -> AppM [DdexPartnerDTO]
listPartnersHandler _user = pure []

createPartnerHandler :: AuthedUser -> DdexPartnerCreateRequest -> AppM DdexPartnerDTO
createPartnerHandler _user _ = throwError err501 { errBody = "Not Implemented: Create Partner" }

getCatalogByDocumentHandler :: AuthedUser -> Maybe Int -> AppM [CatalogReleaseDTO]
getCatalogByDocumentHandler _user _ = pure []
