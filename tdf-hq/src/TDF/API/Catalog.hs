{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Catalog
  ( PublicCatalogAPI
  , CatalogAPI
  , CatalogDefinitionDTO (..)
  , ContentTypeDTO (..)
  , WorkflowStateDTO (..)
  , WorkflowTransitionDTO (..)
  , WorkflowStatesDTO (..)
  , AuthoredContentDTO (..)
  , CatalogItemDTO (..)
  , CatalogDefaultDTO (..)
  , CatalogPageDTO (..)
  , CatalogBatchDTO (..)
  , CatalogDraftRequest (..)
  , ServiceOfferingDraft (..)
  , ServiceOfferingDefaultResourceDraft (..)
  , RadioAutoStopDraft (..)
  , AppearanceModeDraft (..)
  , CatalogRevisionDTO (..)
  , CatalogReviewRequest (..)
  , CatalogActivationRequest (..)
  , CatalogReorderRequest (..)
  , CatalogMergeRequest (..)
  , CatalogUsageDTO (..)
  , CatalogImportResultDTO (..)
  , RecordsFeedDTO (..)
  , RecordsCollectionDTO (..)
  , RecordsReleaseDTO (..)
  , RecordsRecordingDTO (..)
  , RecordsSessionDTO (..)
  , RecordsContributorDTO (..)
  , RecordsResourceDTO (..)
  , SecurityAdminAPI
  , SecurityRoleDTO (..)
  , SecurityPartyRoleAssignmentDTO (..)
  , SecurityGrantRevisionDTO (..)
  , PartyRoleGrantDraftRequest (..)
  , RolePermissionGrantDraftRequest (..)
  , SecurityGrantReviewRequest (..)
  , SecurityAuditEventDTO (..)
  , SelfFanRoleRequest (..)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , Options
  , ToJSON (..)
  , Value
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , omitNothingFields
  , rejectUnknownFields
  )
import Data.Char (toLower)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)
import Servant

-- Public responses include only definitions explicitly marked public and
-- items in the persisted published workflow state.
type PublicCatalogAPI =
       "catalogs" :> "definitions"
         :> QueryParam "locale" Text
         :> Get '[JSON] [CatalogDefinitionDTO]
  :<|> "catalogs" :> "batch"
         :> QueryParams "code" Text
         :> QueryParam "locale" Text
         :> QueryParam "q" Text
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> Header "If-None-Match" Text
         :> Get '[JSON] (Headers '[Header "ETag" Text] CatalogBatchDTO)
  :<|> "catalogs" :> Capture "catalogCode" Text :> "items"
         :> QueryParam "locale" Text
         :> QueryParam "q" Text
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> Header "If-None-Match" Text
         :> Get '[JSON] (Headers '[Header "ETag" Text] CatalogPageDTO)
  :<|> "catalogs" :> Capture "catalogCode" Text :> "items" :> Capture "itemId" Text
         :> QueryParam "locale" Text
         :> Get '[JSON] CatalogItemDTO
  :<|> "catalogs" :> "workflows" :> Capture "workflowCode" Text :> "states"
         :> QueryParam "locale" Text
         :> Header "If-None-Match" Text
         :> Get '[JSON] (Headers '[Header "ETag" Text] WorkflowStatesDTO)
  :<|> "records" :> "feed"
         :> QueryParam "locale" Text
         :> Header "If-None-Match" Text
         :> Get '[JSON] (Headers '[Header "ETag" Text] RecordsFeedDTO)

-- Administrative endpoints are intentionally separate from public reads.
-- Mutations operate on canonical UUID text and never accept slugs as IDs.
type CatalogAPI =
       "content-types"
         :> QueryParam "locale" Text
         :> Get '[JSON] [ContentTypeDTO]
  :<|> "workflow-states"
         :> QueryParam "workflowCode" Text
         :> QueryParam "locale" Text
         :> Get '[JSON] [WorkflowStateDTO]
  :<|> "authored-contents"
         :> QueryParam "locale" Text
         :> Get '[JSON] [AuthoredContentDTO]
  :<|> "definitions"
         :> QueryParam "locale" Text
         :> Get '[JSON] [CatalogDefinitionDTO]
  :<|> "batch"
         :> QueryParams "code" Text
         :> QueryParam "locale" Text
         :> QueryParam "q" Text
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> QueryParam "includeInactive" Bool
         :> Header "If-None-Match" Text
         :> Get '[JSON] (Headers '[Header "ETag" Text] CatalogBatchDTO)
  :<|> Capture "catalogCode" Text :> "items"
         :> QueryParam "locale" Text
         :> QueryParam "q" Text
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> QueryParam "includeInactive" Bool
         :> Header "If-None-Match" Text
         :> Get '[JSON] (Headers '[Header "ETag" Text] CatalogPageDTO)
  :<|> Capture "catalogCode" Text :> "items" :> Capture "itemId" Text
         :> QueryParam "locale" Text
         :> Get '[JSON] CatalogItemDTO
  :<|> Capture "catalogCode" Text :> "revisions"
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> Get '[JSON] [CatalogRevisionDTO]
  :<|> Capture "catalogCode" Text :> "revisions"
         :> ReqBody '[JSON] CatalogDraftRequest
         :> PostCreated '[JSON] CatalogRevisionDTO
  :<|> "revisions" :> Capture "revisionId" Text :> "submit"
         :> Post '[JSON] CatalogRevisionDTO
  :<|> "revisions" :> Capture "revisionId" Text :> "approve"
         :> ReqBody '[JSON] CatalogReviewRequest
         :> Post '[JSON] CatalogRevisionDTO
  :<|> "revisions" :> Capture "revisionId" Text :> "reject"
         :> ReqBody '[JSON] CatalogReviewRequest
         :> Post '[JSON] CatalogRevisionDTO
  :<|> Capture "catalogCode" Text :> "items" :> Capture "itemId" Text :> "activation"
         :> ReqBody '[JSON] CatalogActivationRequest
         :> Patch '[JSON] CatalogItemDTO
  :<|> Capture "catalogCode" Text :> "reorder"
         :> ReqBody '[JSON] CatalogReorderRequest
         :> Post '[JSON] NoContent
  :<|> Capture "catalogCode" Text :> "merge"
         :> ReqBody '[JSON] CatalogMergeRequest
         :> PostCreated '[JSON] CatalogRevisionDTO
  :<|> Capture "catalogCode" Text :> "usage"
         :> QueryParam "from" Day
         :> QueryParam "to" Day
         :> Get '[JSON] [CatalogUsageDTO]
  :<|> Capture "catalogCode" Text :> "export.csv"
         :> Get '[PlainText] Text
  :<|> Capture "catalogCode" Text :> "import.csv"
         :> QueryParam "dryRun" Bool
         :> ReqBody '[PlainText] Text
         :> Post '[JSON] CatalogImportResultDTO
  :<|> "security" :> SecurityAdminAPI

type SecurityAdminAPI =
       "roles" :> Get '[JSON] [SecurityRoleDTO]
  :<|> "party-role-assignments"
         :> QueryParam "partyId" Int64
         :> Get '[JSON] [SecurityPartyRoleAssignmentDTO]
  :<|> "revisions"
         :> QueryParam "workflowState" Text
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> Get '[JSON] [SecurityGrantRevisionDTO]
  :<|> "audit"
         :> QueryParam "partyId" Int64
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> Get '[JSON] [SecurityAuditEventDTO]
  :<|> "party-role-revisions"
         :> ReqBody '[JSON] PartyRoleGrantDraftRequest
         :> PostCreated '[JSON] SecurityGrantRevisionDTO
  :<|> "role-permission-revisions"
         :> ReqBody '[JSON] RolePermissionGrantDraftRequest
         :> PostCreated '[JSON] SecurityGrantRevisionDTO
  :<|> "revisions" :> Capture "revisionId" Text :> "submit"
         :> Post '[JSON] SecurityGrantRevisionDTO
  :<|> "revisions" :> Capture "revisionId" Text :> "approve"
         :> ReqBody '[JSON] SecurityGrantReviewRequest
         :> Post '[JSON] SecurityGrantRevisionDTO
  :<|> "revisions" :> Capture "revisionId" Text :> "reject"
         :> ReqBody '[JSON] SecurityGrantReviewRequest
         :> Post '[JSON] SecurityGrantRevisionDTO

data CatalogDefinitionDTO = CatalogDefinitionDTO
  { cdId :: Text
  , cdCode :: Text
  , cdClassification :: Text
  , cdEntityKind :: Text
  , cdName :: Text
  , cdDescription :: Maybe Text
  , cdPublicRead :: Bool
  , cdSensitive :: Bool
  , cdOrderingMode :: Text
  , cdSourceName :: Maybe Text
  , cdSourceVersion :: Maybe Text
  , cdSourceEffectiveDate :: Maybe Day
  , cdLastSyncedAt :: Maybe UTCTime
  , cdCacheRevision :: Int64
  , cdActive :: Bool
  , cdVersion :: Int
  } deriving (Show, Eq, Generic)

data ContentTypeDTO = ContentTypeDTO
  { ctId :: Text
  , ctCode :: Text
  , ctEntityKind :: Text
  , ctName :: Text
  , ctNameEs :: Text
  , ctNameEn :: Text
  , ctDescription :: Maybe Text
  , ctDescriptionEs :: Maybe Text
  , ctDescriptionEn :: Maybe Text
  , ctSchema :: Value
  , ctSchemaVersion :: Int
  , ctPublicRoutePattern :: Maybe Text
  , ctAdminRoutePattern :: Maybe Text
  , ctPublicRead :: Bool
  , ctActive :: Bool
  , ctWorkflowState :: Text
  , ctVersion :: Int
  } deriving (Show, Eq, Generic)

data WorkflowStateDTO = WorkflowStateDTO
  { wsId :: Text
  , wsWorkflowId :: Text
  , wsWorkflowCode :: Text
  , wsCode :: Text
  , wsName :: Text
  , wsNameEs :: Text
  , wsNameEn :: Text
  , wsSortOrder :: Int
  , wsTerminal :: Bool
  , wsActive :: Bool
  , wsInitialContexts :: [Text]
  , wsCapabilities :: [Text]
  , wsTransitions :: [WorkflowTransitionDTO]
  , wsVersion :: Int
  } deriving (Show, Eq, Generic)

data WorkflowTransitionDTO = WorkflowTransitionDTO
  { wtrToStateId :: Text
  , wtrDirectExecutionAllowed :: Bool
  , wtrRequiresReview :: Bool
  , wtrRequiresDistinctApprover :: Bool
  , wtrEffectiveFrom :: Maybe UTCTime
  , wtrEffectiveUntil :: Maybe UTCTime
  , wtrVersion :: Int
  } deriving (Show, Eq, Generic)

data WorkflowStatesDTO = WorkflowStatesDTO
  { wseWorkflowCode :: Text
  , wseLocale :: Text
  , wseRevision :: Int64
  , wseStates :: [WorkflowStateDTO]
  } deriving (Show, Eq, Generic)

data AuthoredContentDTO = AuthoredContentDTO
  { acId :: Text
  , acCode :: Text
  , acContentTypeId :: Text
  , acContentTypeCode :: Text
  , acEntityKind :: Text
  , acName :: Text
  , acNameEs :: Text
  , acNameEn :: Text
  , acDescription :: Maybe Text
  , acDescriptionEs :: Maybe Text
  , acDescriptionEn :: Maybe Text
  , acCurrentSlug :: Text
  , acPublicRoute :: Maybe Text
  , acSchema :: Value
  , acSchemaVersion :: Int
  , acSortOrder :: Int
  , acActive :: Bool
  , acWorkflowState :: Text
  , acRevision :: Int
  , acVersion :: Int
  } deriving (Show, Eq, Generic)

data CatalogItemDTO = CatalogItemDTO
  { ciId :: Text
  , ciCatalogId :: Text
  , ciCatalogCode :: Text
  , ciKind :: Text
  , ciCode :: Text
  , ciName :: Text
  , ciNameEs :: Text
  , ciNameEn :: Text
  , ciDescription :: Maybe Text
  , ciDescriptionEs :: Maybe Text
  , ciDescriptionEn :: Maybe Text
  , ciSearchAliases :: [Text]
  , ciCurrentSlug :: Maybe Text
  , ciParentId :: Maybe Text
  , ciSortOrder :: Int
  , ciActive :: Bool
  , ciWorkflowState :: Text
  , ciDeprecatedAt :: Maybe UTCTime
  , ciReplacementId :: Maybe Text
  , ciExternalCode :: Maybe Text
  , ciSourceVersion :: Maybe Text
  , ciDisplaySymbol :: Maybe Text
  , ciUsageCount :: Int64
  , ciVersion :: Int
  } deriving (Show, Eq, Generic)

data CatalogPageDTO = CatalogPageDTO
  { cpCatalog :: CatalogDefinitionDTO
  , cpItems :: [CatalogItemDTO]
  , cpDefaults :: [CatalogDefaultDTO]
  , cpPage :: Int
  , cpPageSize :: Int
  , cpTotal :: Int64
  , cpRevision :: Int64
  , cpLocale :: Text
  } deriving (Show, Eq, Generic)

data CatalogDefaultDTO = CatalogDefaultDTO
  { cdfEntityId :: Text
  , cdfScopeKind :: Text
  , cdfScopeId :: Text
  , cdfLocaleId :: Maybe Text
  , cdfEffectiveFrom :: Maybe UTCTime
  , cdfEffectiveUntil :: Maybe UTCTime
  , cdfVersion :: Int
  } deriving (Show, Eq, Generic)

data CatalogBatchDTO = CatalogBatchDTO
  { cbCatalogs :: [CatalogPageDTO]
  , cbRevision :: Int64
  , cbLocale :: Text
  } deriving (Show, Eq, Generic)

data RecordsContributorDTO = RecordsContributorDTO
  { rcId :: Text
  , rcCode :: Text
  , rcKind :: Text
  , rcName :: Text
  } deriving (Show, Eq, Generic)

data RecordsResourceDTO = RecordsResourceDTO
  { rrId :: Text
  , rrProviderCode :: Text
  , rrKind :: Text
  , rrExternalCode :: Text
  , rrUrl :: Text
  , rrLabel :: Maybe Text
  , rrDurationMs :: Maybe Int
  , rrThumbnailUrl :: Maybe Text
  , rrRelationKind :: Text
  , rrPrimary :: Bool
  , rrSortOrder :: Int
  } deriving (Show, Eq, Generic)

data RecordsCollectionDTO = RecordsCollectionDTO
  { rcoId :: Text
  , rcoCode :: Text
  , rcoKind :: Text
  , rcoName :: Text
  , rcoDescription :: Maybe Text
  , rcoPublicRoute :: Maybe Text
  , rcoResources :: [RecordsResourceDTO]
  , rcoRevision :: Int
  } deriving (Show, Eq, Generic)

data RecordsReleaseDTO = RecordsReleaseDTO
  { rreId :: Text
  , rreCode :: Text
  , rreSlug :: Maybe Text
  , rreTitle :: Text
  , rreDescription :: Maybe Text
  , rreReleaseTypeId :: Text
  , rreReleaseDate :: Maybe Day
  , rreContributors :: [RecordsContributorDTO]
  , rreResources :: [RecordsResourceDTO]
  , rreSortOrder :: Int
  , rreRevision :: Int
  } deriving (Show, Eq, Generic)

data RecordsRecordingDTO = RecordsRecordingDTO
  { rrgId :: Text
  , rrgCode :: Text
  , rrgSlug :: Maybe Text
  , rrgTitle :: Text
  , rrgDescription :: Maybe Text
  , rrgRecordingTypeId :: Text
  , rrgDurationMs :: Maybe Int
  , rrgContributors :: [RecordsContributorDTO]
  , rrgResources :: [RecordsResourceDTO]
  , rrgSortOrder :: Int
  , rrgRevision :: Int
  } deriving (Show, Eq, Generic)

data RecordsSessionDTO = RecordsSessionDTO
  { rssId :: Text
  , rssCode :: Text
  , rssSlug :: Maybe Text
  , rssTitle :: Text
  , rssDescription :: Maybe Text
  , rssSessionTypeId :: Text
  , rssRecordedAt :: Maybe UTCTime
  , rssContributors :: [RecordsContributorDTO]
  , rssResources :: [RecordsResourceDTO]
  , rssSortOrder :: Int
  , rssRevision :: Int
  } deriving (Show, Eq, Generic)

data RecordsFeedDTO = RecordsFeedDTO
  { rfLocale :: Text
  , rfRevision :: Int64
  , rfCollections :: [RecordsCollectionDTO]
  , rfReleases :: [RecordsReleaseDTO]
  , rfRecordings :: [RecordsRecordingDTO]
  , rfSessions :: [RecordsSessionDTO]
  } deriving (Show, Eq, Generic)

-- Strict common draft schema for catalog items. Domain-specific services may
-- add their own typed fields; unrestricted JSON is deliberately not accepted.
data CatalogDraftRequest = CatalogDraftRequest
  { cdrEntityId :: Maybe Text
  , cdrBaseVersion :: Maybe Int
  , cdrCode :: Text
  , cdrNameEs :: Text
  , cdrNameEn :: Text
  , cdrDescriptionEs :: Maybe Text
  , cdrDescriptionEn :: Maybe Text
  , cdrSearchAliasesEs :: [Text]
  , cdrSearchAliasesEn :: [Text]
  , cdrCurrentSlug :: Maybe Text
  , cdrParentId :: Maybe Text
  , cdrSortOrder :: Int
  , cdrExternalCode :: Maybe Text
  , cdrSourceVersion :: Maybe Text
  , cdrServiceOffering :: Maybe ServiceOfferingDraft
  , cdrRadioAutoStop :: Maybe RadioAutoStopDraft
  , cdrAppearanceMode :: Maybe AppearanceModeDraft
  , cdrDisplaySymbol :: Maybe Text
  , cdrGlobalDefault :: Maybe Bool
  , cdrReason :: Text
  , cdrSourcePlatform :: Text
  , cdrCorrelationId :: Text
  } deriving (Show, Eq, Generic)

data ServiceOfferingDraft = ServiceOfferingDraft
  { sodCategoryId :: Text
  , sodPricingModelId :: Text
  , sodRateCents :: Maybe Int
  , sodCurrencyId :: Text
  , sodBillingUnitEs :: Maybe Text
  , sodBillingUnitEn :: Maybe Text
  , sodTaxRateId :: Maybe Text
  , sodDefaultDurationMinutes :: Maybe Int
  , sodRequiresEngineer :: Bool
  , sodDefaultResources :: [ServiceOfferingDefaultResourceDraft]
  } deriving (Show, Eq, Generic)

data ServiceOfferingDefaultResourceDraft = ServiceOfferingDefaultResourceDraft
  { sordResourceId :: Text
  , sordSelectionModeId :: Text
  , sordSortOrder :: Int
  } deriving (Show, Eq, Generic)

data RadioAutoStopDraft = RadioAutoStopDraft
  { rasdDurationMinutes :: Int
  , rasdDefaultForBroadcast :: Bool
  } deriving (Show, Eq, Generic)

data AppearanceModeDraft = AppearanceModeDraft
  { amdDefaultForApplication :: Bool
  } deriving (Show, Eq, Generic)

data CatalogRevisionDTO = CatalogRevisionDTO
  { crId :: Text
  , crCatalogId :: Text
  , crCatalogCode :: Text
  , crEntityId :: Text
  , crWorkflowState :: Text
  , crBaseVersion :: Int
  , crProposedVersion :: Int
  , crDraft :: CatalogDraftRequest
  , crCreatedBy :: Int64
  , crCreatedAt :: UTCTime
  , crSubmittedAt :: Maybe UTCTime
  , crReviewedBy :: Maybe Int64
  , crReviewedAt :: Maybe UTCTime
  , crApprovedBy :: Maybe Int64
  , crApprovedAt :: Maybe UTCTime
  , crReviewerNotes :: Maybe Text
  , crRejectionReason :: Maybe Text
  , crScheduledPublishAt :: Maybe UTCTime
  , crPublishedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

data CatalogReviewRequest = CatalogReviewRequest
  { crrNotes :: Text
  , crrScheduledPublishAt :: Maybe UTCTime
  , crrEmergencyOverride :: Bool
  } deriving (Show, Eq, Generic)

data CatalogActivationRequest = CatalogActivationRequest
  { caActive :: Bool
  , caReplacementId :: Maybe Text
  , caReason :: Text
  , caExpectedVersion :: Int
  , caCorrelationId :: Text
  } deriving (Show, Eq, Generic)

data CatalogReorderRequest = CatalogReorderRequest
  { croOrderedItemIds :: [Text]
  , croExpectedCatalogRevision :: Int64
  , croReason :: Text
  , croCorrelationId :: Text
  } deriving (Show, Eq, Generic)

data CatalogMergeRequest = CatalogMergeRequest
  { cmSourceItemId :: Text
  , cmTargetItemId :: Text
  , cmReason :: Text
  , cmCorrelationId :: Text
  } deriving (Show, Eq, Generic)

data CatalogUsageDTO = CatalogUsageDTO
  { cuItemId :: Maybe Text
  , cuDay :: Day
  , cuSelectionCount :: Int64
  , cuHistoricalReferenceCount :: Int64
  , cuReplacementCount :: Int64
  , cuNoResultSearchCount :: Int64
  , cuFormFailureCount :: Int64
  } deriving (Show, Eq, Generic)

data CatalogImportResultDTO = CatalogImportResultDTO
  { cirImportJobId :: Text
  , cirDryRun :: Bool
  , cirStatus :: Text
  , cirTotalRows :: Int
  , cirAcceptedRows :: Int
  , cirRejectedRows :: Int
  , cirAmbiguousRows :: Int
  , cirErrors :: [Text]
  } deriving (Show, Eq, Generic)

data SecurityRoleDTO = SecurityRoleDTO
  { srId :: Text
  , srCode :: Text
  , srNameEs :: Text
  , srNameEn :: Text
  , srDescriptionEs :: Maybe Text
  , srDescriptionEn :: Maybe Text
  , srEmergencyAdministrator :: Bool
  , srSystemRole :: Bool
  , srSelfAssignable :: Bool
  , srAutomaticAssignable :: Bool
  , srActive :: Bool
  , srModuleCodes :: [Text]
  , srPermissionCodes :: [Text]
  , srVersion :: Int
  } deriving (Show, Eq, Generic)

data SecurityPartyRoleAssignmentDTO = SecurityPartyRoleAssignmentDTO
  { spaId :: Text
  , spaPartyId :: Int64
  , spaRoleId :: Text
  , spaRoleCode :: Text
  , spaRoleNameEs :: Text
  , spaActive :: Bool
  , spaGrantedBy :: Maybe Int64
  , spaApprovedBy :: Maybe Int64
  , spaApprovalMode :: Text
  , spaSourceRevisionId :: Maybe Text
  , spaSourcePolicyId :: Maybe Text
  , spaCreatedAt :: UTCTime
  , spaRevokedAt :: Maybe UTCTime
  , spaVersion :: Int
  } deriving (Show, Eq, Generic)

data PartyRoleGrantDraftRequest = PartyRoleGrantDraftRequest
  { prgPartyId :: Int64
  , prgRoleId :: Text
  , prgDesiredActive :: Bool
  , prgExpectedVersion :: Int
  , prgReason :: Text
  , prgSourcePlatform :: Text
  , prgCorrelationId :: Text
  } deriving (Show, Eq, Generic)

data RolePermissionGrantDraftRequest = RolePermissionGrantDraftRequest
  { rpgRoleId :: Text
  , rpgPermissionId :: Text
  , rpgDesiredActive :: Bool
  , rpgExpectedVersion :: Int
  , rpgReason :: Text
  , rpgSourcePlatform :: Text
  , rpgCorrelationId :: Text
  } deriving (Show, Eq, Generic)

data SecurityGrantReviewRequest = SecurityGrantReviewRequest
  { sgrNotes :: Text
  , sgrEmergencyOverrideReason :: Maybe Text
  } deriving (Show, Eq, Generic)

data SecurityGrantRevisionDTO = SecurityGrantRevisionDTO
  { sgrId :: Text
  , sgrChangeKind :: Text
  , sgrPartyId :: Maybe Int64
  , sgrRoleId :: Text
  , sgrPermissionId :: Maybe Text
  , sgrDesiredActive :: Bool
  , sgrExpectedVersion :: Int
  , sgrWorkflowState :: Text
  , sgrCreatedBy :: Int64
  , sgrCreatedAt :: UTCTime
  , sgrSubmittedAt :: Maybe UTCTime
  , sgrReviewedBy :: Maybe Int64
  , sgrReviewedAt :: Maybe UTCTime
  , sgrApprovedBy :: Maybe Int64
  , sgrApprovedAt :: Maybe UTCTime
  , sgrReviewerNotes :: Maybe Text
  , sgrRejectionReason :: Maybe Text
  , sgrApprovalMode :: Text
  , sgrEmergencyReason :: Maybe Text
  , sgrSourcePlatform :: Text
  , sgrCorrelationId :: Text
  , sgrReason :: Text
  , sgrResult :: Maybe Text
  , sgrVersion :: Int
  } deriving (Show, Eq, Generic)

data SecurityAuditEventDTO = SecurityAuditEventDTO
  { saeId :: Text
  , saeRevisionId :: Maybe Text
  , saeSourcePolicyId :: Maybe Text
  , saeEntityKind :: Text
  , saePartyId :: Maybe Int64
  , saeRoleId :: Text
  , saePermissionId :: Maybe Text
  , saeOperation :: Text
  , saePreviousActive :: Maybe Bool
  , saeNewActive :: Maybe Bool
  , saeActorId :: Maybe Int64
  , saeReviewerId :: Maybe Int64
  , saeApproverId :: Maybe Int64
  , saeOccurredAt :: UTCTime
  , saeSourcePlatform :: Text
  , saeReason :: Maybe Text
  , saeCorrelationId :: Text
  , saeApprovalMode :: Text
  , saeResult :: Text
  } deriving (Show, Eq, Generic)

-- A self-service request can target only the compile-recognized Fan role.
-- The caller never supplies a role code or id, and the request enters review
-- without changing the canonical assignment.
data SelfFanRoleRequest = SelfFanRoleRequest
  { sfrReason :: Text
  , sfrSourcePlatform :: Text
  , sfrCorrelationId :: Text
  } deriving (Show, Eq, Generic)

catalogJsonOptions :: Int -> Options
catalogJsonOptions prefixLength =
  defaultOptions
    { fieldLabelModifier = lowerFirst . drop prefixLength
    , omitNothingFields = True
    }
  where
    lowerFirst [] = []
    lowerFirst (first : rest) = toLower first : rest

strictCatalogJsonOptions :: Int -> Options
strictCatalogJsonOptions prefixLength =
  (catalogJsonOptions prefixLength) { rejectUnknownFields = True }

instance ToJSON CatalogDefinitionDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON CatalogDefinitionDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON ContentTypeDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON ContentTypeDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON WorkflowStateDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON WorkflowStateDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON WorkflowTransitionDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON WorkflowTransitionDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON WorkflowStatesDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON WorkflowStatesDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON AuthoredContentDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON AuthoredContentDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON CatalogItemDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON CatalogItemDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON CatalogDefaultDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON CatalogDefaultDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON CatalogPageDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON CatalogPageDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON CatalogBatchDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON CatalogBatchDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON RecordsContributorDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON RecordsContributorDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON RecordsResourceDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON RecordsResourceDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON RecordsCollectionDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON RecordsCollectionDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON RecordsReleaseDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON RecordsReleaseDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON RecordsRecordingDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON RecordsRecordingDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON RecordsSessionDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON RecordsSessionDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON RecordsFeedDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON RecordsFeedDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON CatalogDraftRequest where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON CatalogDraftRequest where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON ServiceOfferingDraft where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON ServiceOfferingDraft where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON ServiceOfferingDefaultResourceDraft where toJSON = genericToJSON (catalogJsonOptions 4)
instance FromJSON ServiceOfferingDefaultResourceDraft where parseJSON = genericParseJSON (strictCatalogJsonOptions 4)
instance ToJSON RadioAutoStopDraft where toJSON = genericToJSON (catalogJsonOptions 4)
instance FromJSON RadioAutoStopDraft where parseJSON = genericParseJSON (strictCatalogJsonOptions 4)
instance ToJSON AppearanceModeDraft where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON AppearanceModeDraft where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON CatalogRevisionDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON CatalogRevisionDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON CatalogReviewRequest where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON CatalogReviewRequest where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON CatalogActivationRequest where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON CatalogActivationRequest where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON CatalogReorderRequest where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON CatalogReorderRequest where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON CatalogMergeRequest where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON CatalogMergeRequest where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON CatalogUsageDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON CatalogUsageDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON CatalogImportResultDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON CatalogImportResultDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON SecurityRoleDTO where toJSON = genericToJSON (catalogJsonOptions 2)
instance FromJSON SecurityRoleDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 2)
instance ToJSON SecurityPartyRoleAssignmentDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON SecurityPartyRoleAssignmentDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON PartyRoleGrantDraftRequest where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON PartyRoleGrantDraftRequest where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON RolePermissionGrantDraftRequest where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON RolePermissionGrantDraftRequest where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON SecurityGrantReviewRequest where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON SecurityGrantReviewRequest where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON SecurityGrantRevisionDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON SecurityGrantRevisionDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON SecurityAuditEventDTO where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON SecurityAuditEventDTO where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
instance ToJSON SelfFanRoleRequest where toJSON = genericToJSON (catalogJsonOptions 3)
instance FromJSON SelfFanRoleRequest where parseJSON = genericParseJSON (strictCatalogJsonOptions 3)
