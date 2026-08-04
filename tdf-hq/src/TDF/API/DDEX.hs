{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.DDEX
  ( DDEXAPI
  , DdexUploadRequest(..)
  , DdexDownloadResponse(..)
  , DdexPreviewDTO(..)
  , ImportPlanResolution(..)
  , ConflictResolution(..)
  , DdexExportRequest(..)
  , DdexPartnerCreateRequest(..)
  , DdexDocumentDTO(..)
  , ValidationRunDTO(..)
  , ValidationReportDTO(..)
  , ValidationIssueDTO(..)
  , ImportPlanDTO(..)
  , ImportConflictDTO(..)
  , ImportRunDTO(..)
  , DdexExportDTO(..)
  , DdexPartnerDTO(..)
  , CatalogReleaseDTO(..)
  ) where

import Servant
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Main DDEX API Type
type DDEXAPI =
       -- Documents
       "documents" :> ReqBody '[JSON] DdexUploadRequest :> Post '[JSON] DdexDocumentDTO
  :<|> "documents" :> QueryParam "status" Text :> QueryParam "partner" Text :> Get '[JSON] [DdexDocumentDTO]
  :<|> "documents" :> Capture "id" Int :> Get '[JSON] DdexDocumentDTO
  :<|> "documents" :> Capture "id" Int :> "raw" :> Get '[JSON] DdexDownloadResponse
  :<|> "documents" :> Capture "id" Int :> "validation-runs" :> Post '[JSON] ValidationRunDTO
  :<|> "documents" :> Capture "id" Int :> "validation-runs" :> "latest" :> Get '[JSON] ValidationReportDTO
  :<|> "documents" :> Capture "id" Int :> "preview" :> Get '[JSON] DdexPreviewDTO
  :<|> "documents" :> Capture "id" Int :> "import-plans" :> Post '[JSON] ImportPlanDTO
  :<|> "import-plans" :> Capture "id" Int :> ReqBody '[JSON] ImportPlanResolution :> Patch '[JSON] ImportPlanDTO
  :<|> "import-plans" :> Capture "id" Int :> "commit" :> Post '[JSON] ImportRunDTO
       -- Exports
  :<|> "exports" :> ReqBody '[JSON] DdexExportRequest :> Post '[JSON] DdexExportDTO
  :<|> "exports" :> Capture "id" Int :> "download" :> Get '[JSON] DdexDownloadResponse
       -- Partners
  :<|> "partners" :> Get '[JSON] [DdexPartnerDTO]
  :<|> "partners" :> ReqBody '[JSON] DdexPartnerCreateRequest :> Post '[JSON] DdexPartnerDTO
       -- Catalog Read-Through (Optional convenience)
  :<|> "catalog" :> "releases" :> QueryParam "ddex_document_id" Int :> Get '[JSON] [CatalogReleaseDTO]

-- | Request to upload a document
data DdexUploadRequest = DdexUploadRequest
  { uploadFileName :: Text
  , uploadContentType :: Text
  , uploadContentBase64 :: Text -- Simplified for phase 1; real impl uses Multipart
  } deriving (Show, Eq, Generic)

instance ToJSON DdexUploadRequest
instance FromJSON DdexUploadRequest

-- | Response for downloading raw files
data DdexDownloadResponse = DdexDownloadResponse
  { downloadFileName :: Text
  , downloadContentType :: Text
  , downloadContentBase64 :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON DdexDownloadResponse
instance FromJSON DdexDownloadResponse

-- | Preview of what will be imported
data DdexPreviewDTO = DdexPreviewDTO
  { previewMessageId :: Text
  , previewSender :: Text
  , previewReleaseCount :: Int
  , previewResourceCount :: Int
  , previewWarnings :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON DdexPreviewDTO
instance FromJSON DdexPreviewDTO

-- | Resolution of conflicts in an import plan
data ImportPlanResolution = ImportPlanResolution
  { resolutionPlanId :: Int
  , resolutionConflicts :: [ConflictResolution]
  } deriving (Show, Eq, Generic)

instance ToJSON ImportPlanResolution
instance FromJSON ImportPlanResolution

data ConflictResolution = ConflictResolution
  { resolutionConflictId :: Int
  , resolutionAction :: Text -- 'UseExisting', 'CreateNew', 'Ignore'
  , resolutionTargetId :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON ConflictResolution
instance FromJSON ConflictResolution

-- | Request to generate an export
data DdexExportRequest = DdexExportRequest
  { exportReleaseId :: Int
  , exportPartnerId :: Int
  , exportProfile :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON DdexExportRequest
instance FromJSON DdexExportRequest

-- | Request to create a partner
data DdexPartnerCreateRequest = DdexPartnerCreateRequest
  { partnerName :: Text
  , partnerDpid :: Maybe Text
  , partnerAllowedVersions :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON DdexPartnerCreateRequest
instance FromJSON DdexPartnerCreateRequest

data DdexDocumentDTO = DdexDocumentDTO
  { ddexDocumentId :: Int
  , ddexDocumentFileName :: Text
  , ddexDocumentSha256 :: Text
  , ddexDocumentFamily :: Text
  , ddexDocumentVersion :: Text
  , ddexDocumentStatus :: Text
  , ddexDocumentMessageId :: Maybe Text
  , ddexDocumentSenderId :: Maybe Text
  , ddexDocumentRecipientId :: Maybe Text
  , ddexDocumentCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON DdexDocumentDTO
instance FromJSON DdexDocumentDTO

data ValidationRunDTO = ValidationRunDTO
  { validationRunId :: Int
  , validationRunDocumentId :: Int
  , validationRunStatus :: Text
  , validationRunStartedAt :: UTCTime
  , validationRunFinishedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ValidationRunDTO
instance FromJSON ValidationRunDTO

data ValidationReportDTO = ValidationReportDTO
  { reportRunId :: Int
  , reportIssues :: [ValidationIssueDTO]
  , reportIsValid :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ValidationReportDTO
instance FromJSON ValidationReportDTO

data ValidationIssueDTO = ValidationIssueDTO
  { issueSeverity :: Text
  , issueLayer :: Text
  , issueCode :: Text
  , issueMessage :: Text
  , issueLine :: Maybe Int
  , issueColumn :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON ValidationIssueDTO
instance FromJSON ValidationIssueDTO

data ImportPlanDTO = ImportPlanDTO
  { importPlanId :: Int
  , importPlanDocumentId :: Int
  , importPlanStatus :: Text
  , importPlanConflicts :: [ImportConflictDTO]
  , importPlanChanges :: [Text] -- Summary of changes
  } deriving (Show, Eq, Generic)

instance ToJSON ImportPlanDTO
instance FromJSON ImportPlanDTO

data ImportConflictDTO = ImportConflictDTO
  { conflictId :: Int
  , conflictEntityType :: Text
  , conflictIdentifier :: Text
  , conflictDescription :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ImportConflictDTO
instance FromJSON ImportConflictDTO

data ImportRunDTO = ImportRunDTO
  { importRunId :: Int
  , importRunPlanId :: Int
  , importRunStatus :: Text
  , importRunEntitiesCreated :: Int
  , importRunEntitiesUpdated :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ImportRunDTO
instance FromJSON ImportRunDTO

data DdexExportDTO = DdexExportDTO
  { ddexExportId :: Int
  , ddexExportReleaseId :: Int
  , ddexExportPartnerId :: Int
  , ddexExportStatus :: Text
  , ddexExportXmlChecksum :: Text
  , ddexExportCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON DdexExportDTO
instance FromJSON DdexExportDTO

data DdexPartnerDTO = DdexPartnerDTO
  { ddexPartnerId :: Int
  , ddexPartnerName :: Text
  , ddexPartnerDpid :: Maybe Text
  , ddexPartnerAllowedVersions :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON DdexPartnerDTO
instance FromJSON DdexPartnerDTO

data CatalogReleaseDTO = CatalogReleaseDTO
  { catalogReleaseDtoId :: Int
  , catalogReleaseDtoTitle :: Text
  , catalogReleaseDtoType :: Text
  , catalogReleaseDtoUpc :: Maybe Text
  , catalogReleaseDtoReleaseDate :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogReleaseDTO
instance FromJSON CatalogReleaseDTO
