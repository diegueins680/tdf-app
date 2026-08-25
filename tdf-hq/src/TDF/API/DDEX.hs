{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.DDEX
  ( DDEXAPI
  , DdexReferenceSnapshotDTO(..)
  , DdexStandardVersionDTO(..)
  , DdexMessageTypeDTO(..)
  , DdexDocumentStateDTO(..)
  , DdexOperationalWorkflowDTO(..)
  , DdexOperationalStateDTO(..)
  , DdexOperationDTO(..)
  , DdexValidationReferenceDTO(..)
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
  ) where

import Servant
import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericParseJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Int (Int64)
import GHC.Generics (Generic)

-- | Main DDEX API Type
type DDEXAPI =
       -- Governed reference snapshot
       "references" :> QueryParam "locale" Text :> Get '[JSON] DdexReferenceSnapshotDTO
       -- Documents
  :<|> "documents" :> ReqBody '[JSON] DdexUploadRequest :> Post '[JSON] DdexDocumentDTO
  :<|> "documents" :> QueryParam "workflowStateId" Text :> Get '[JSON] [DdexDocumentDTO]
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

-- | Request to upload a document
data DdexUploadRequest = DdexUploadRequest
  { uploadFileName :: Text
  , uploadContentType :: Text
  , uploadContentBase64 :: Text -- Simplified for phase 1; real impl uses Multipart
  } deriving (Show, Eq, Generic)

instance ToJSON DdexUploadRequest
instance FromJSON DdexUploadRequest where
  parseJSON = genericParseJSON strictRequestOptions

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
instance FromJSON ImportPlanResolution where
  parseJSON = genericParseJSON strictRequestOptions

data ConflictResolution = ConflictResolution
  { resolutionConflictId :: Int
  , resolutionAction :: Text -- 'UseExisting', 'CreateNew', 'Ignore'
  , resolutionTargetId :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON ConflictResolution
instance FromJSON ConflictResolution where
  parseJSON = genericParseJSON strictRequestOptions

-- | Request to generate an export
data DdexExportRequest = DdexExportRequest
  { exportReleaseId :: Int
  , exportPartnerId :: Int
  , exportStandardVersionId :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON DdexExportRequest
instance FromJSON DdexExportRequest where
  parseJSON = genericParseJSON strictRequestOptions

-- | Request to create a partner
data DdexPartnerCreateRequest = DdexPartnerCreateRequest
  { partnerName :: Text
  , partnerDpid :: Maybe Text
  , partnerAllowedStandardVersionIds :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON DdexPartnerCreateRequest
instance FromJSON DdexPartnerCreateRequest where
  parseJSON = genericParseJSON strictRequestOptions

strictRequestOptions :: Aeson.Options
strictRequestOptions = defaultOptions { Aeson.rejectUnknownFields = True }

data DdexDocumentDTO = DdexDocumentDTO
  { ddexDocumentId :: Int
  , ddexDocumentFileName :: Text
  , ddexDocumentSha256 :: Text
  , ddexDocumentStandardVersionId :: Text
  , ddexDocumentStandardCode :: Text
  , ddexDocumentVersionCode :: Text
  , ddexDocumentMessageTypeId :: Maybe Text
  , ddexDocumentMessageTypeCode :: Maybe Text
  , ddexDocumentWorkflowStateId :: Text
  , ddexDocumentWorkflowStateCode :: Text
  , ddexDocumentWorkflowStateNameEs :: Text
  , ddexDocumentWorkflowStateNameEn :: Text
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
  , validationRunWorkflowStateId :: Text
  , validationRunWorkflowStateCode :: Text
  , validationRunWorkflowStateNameEs :: Text
  , validationRunWorkflowStateNameEn :: Text
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
  { issueSeverityId :: Text
  , issueSeverityCode :: Text
  , issueSeverityNameEs :: Text
  , issueSeverityNameEn :: Text
  , issueLayerId :: Text
  , issueLayerCode :: Text
  , issueLayerNameEs :: Text
  , issueLayerNameEn :: Text
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
  , importPlanWorkflowStateId :: Text
  , importPlanWorkflowStateCode :: Text
  , importPlanWorkflowStateNameEs :: Text
  , importPlanWorkflowStateNameEn :: Text
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
  , importRunWorkflowStateId :: Text
  , importRunWorkflowStateCode :: Text
  , importRunWorkflowStateNameEs :: Text
  , importRunWorkflowStateNameEn :: Text
  , importRunEntitiesCreated :: Int
  , importRunEntitiesUpdated :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ImportRunDTO
instance FromJSON ImportRunDTO

data DdexExportDTO = DdexExportDTO
  { ddexExportId :: Int
  , ddexExportReleaseId :: Int
  , ddexExportPartnerId :: Int
  , ddexExportWorkflowStateId :: Text
  , ddexExportWorkflowStateCode :: Text
  , ddexExportWorkflowStateNameEs :: Text
  , ddexExportWorkflowStateNameEn :: Text
  , ddexExportXmlChecksum :: Text
  , ddexExportCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON DdexExportDTO
instance FromJSON DdexExportDTO

data DdexPartnerDTO = DdexPartnerDTO
  { ddexPartnerId :: Int
  , ddexPartnerName :: Text
  , ddexPartnerDpid :: Maybe Text
  , ddexPartnerAllowedStandardVersions :: [DdexStandardVersionDTO]
  } deriving (Show, Eq, Generic)

instance ToJSON DdexPartnerDTO
instance FromJSON DdexPartnerDTO

data DdexReferenceSnapshotDTO = DdexReferenceSnapshotDTO
  { ddexReferenceRevision :: Int64
  , ddexReferenceLocale :: Text
  , ddexReferenceStandardVersions :: [DdexStandardVersionDTO]
  , ddexReferenceMessageTypes :: [DdexMessageTypeDTO]
  , ddexReferenceDocumentStates :: [DdexDocumentStateDTO]
  , ddexReferenceOperationalWorkflows :: [DdexOperationalWorkflowDTO]
  , ddexReferenceJobOperations :: [DdexOperationDTO]
  , ddexReferenceImportOperations :: [DdexOperationDTO]
  , ddexReferenceValidationResults :: [DdexValidationReferenceDTO]
  , ddexReferenceValidationSeverities :: [DdexValidationReferenceDTO]
  , ddexReferenceValidationLayers :: [DdexValidationReferenceDTO]
  } deriving (Show, Eq, Generic)

instance ToJSON DdexReferenceSnapshotDTO
instance FromJSON DdexReferenceSnapshotDTO

data DdexStandardVersionDTO = DdexStandardVersionDTO
  { ddexStandardVersionId :: Text
  , ddexStandardCode :: Text
  , ddexVersionCode :: Text
  , ddexStandardVersionName :: Text
  , ddexStandardVersionNameEs :: Text
  , ddexStandardVersionNameEn :: Text
  , ddexStandardSourceVersion :: Text
  , ddexStandardSourceUri :: Text
  , ddexStandardDetectionEnabled :: Bool
  , ddexStandardValidationEnabled :: Bool
  , ddexStandardImportEnabled :: Bool
  , ddexStandardExportEnabled :: Bool
  , ddexStandardVersionRevision :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DdexStandardVersionDTO
instance FromJSON DdexStandardVersionDTO

data DdexMessageTypeDTO = DdexMessageTypeDTO
  { ddexMessageTypeId :: Text
  , ddexMessageTypeStandardVersionId :: Text
  , ddexMessageTypeCode :: Text
  , ddexMessageTypeName :: Text
  , ddexMessageTypeNameEs :: Text
  , ddexMessageTypeNameEn :: Text
  , ddexMessageTypeRuntimeSupported :: Bool
  , ddexMessageTypeRevision :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DdexMessageTypeDTO
instance FromJSON DdexMessageTypeDTO

data DdexDocumentStateDTO = DdexDocumentStateDTO
  { ddexDocumentStateId :: Text
  , ddexDocumentStateCode :: Text
  , ddexDocumentStateName :: Text
  , ddexDocumentStateNameEs :: Text
  , ddexDocumentStateNameEn :: Text
  , ddexDocumentStateSortOrder :: Int
  , ddexDocumentStateTerminal :: Bool
  , ddexDocumentStateRevision :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DdexDocumentStateDTO
instance FromJSON DdexDocumentStateDTO

data DdexOperationalWorkflowDTO = DdexOperationalWorkflowDTO
  { ddexOperationalWorkflowId :: Text
  , ddexOperationalWorkflowCode :: Text
  , ddexOperationalWorkflowName :: Text
  , ddexOperationalWorkflowNameEs :: Text
  , ddexOperationalWorkflowNameEn :: Text
  , ddexOperationalWorkflowSensitive :: Bool
  , ddexOperationalWorkflowRevision :: Int64
  , ddexOperationalWorkflowStates :: [DdexOperationalStateDTO]
  } deriving (Show, Eq, Generic)

instance ToJSON DdexOperationalWorkflowDTO
instance FromJSON DdexOperationalWorkflowDTO

data DdexOperationalStateDTO = DdexOperationalStateDTO
  { ddexOperationalStateId :: Text
  , ddexOperationalStateCode :: Text
  , ddexOperationalStateName :: Text
  , ddexOperationalStateNameEs :: Text
  , ddexOperationalStateNameEn :: Text
  , ddexOperationalStateSortOrder :: Int
  , ddexOperationalStateTerminal :: Bool
  , ddexOperationalStateRevision :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DdexOperationalStateDTO
instance FromJSON DdexOperationalStateDTO

data DdexOperationDTO = DdexOperationDTO
  { ddexOperationId :: Text
  , ddexOperationCode :: Text
  , ddexOperationName :: Text
  , ddexOperationNameEs :: Text
  , ddexOperationNameEn :: Text
  , ddexOperationDescription :: Maybe Text
  , ddexOperationDescriptionEs :: Maybe Text
  , ddexOperationDescriptionEn :: Maybe Text
  , ddexOperationSortOrder :: Int
  , ddexOperationRevision :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DdexOperationDTO
instance FromJSON DdexOperationDTO

data DdexValidationReferenceDTO = DdexValidationReferenceDTO
  { ddexValidationReferenceId :: Text
  , ddexValidationReferenceCode :: Text
  , ddexValidationReferenceName :: Text
  , ddexValidationReferenceNameEs :: Text
  , ddexValidationReferenceNameEn :: Text
  , ddexValidationReferenceDescription :: Maybe Text
  , ddexValidationReferenceDescriptionEs :: Maybe Text
  , ddexValidationReferenceDescriptionEn :: Maybe Text
  , ddexValidationReferenceSortOrder :: Int
  , ddexValidationReferenceRevision :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DdexValidationReferenceDTO
instance FromJSON DdexValidationReferenceDTO
