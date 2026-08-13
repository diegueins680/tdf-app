{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.Server.Catalog
  ( publicCatalogServer
  , catalogServer
  , createSelfFanRoleRequest
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, eitherDecodeStrict', genericParseJSON, genericToJSON, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Char (isAlphaNum, isControl, toLower)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist
import Database.Persist.Sql (Single (..), SqlPersistT, fromSqlKey, rawExecute, rawSql, runSqlPool, toSqlKey)
import GHC.Generics (Generic)
import Servant

import TDF.API.Catalog
import TDF.Auth (AuthedUser (..), ModuleAccess (ModuleAdmin, ModuleCatalog), validateModuleAccess)
import qualified TDF.Catalog.Models as M
import TDF.CMS.Models (AesonValue (..))
import TDF.DB (Env (..))
import TDF.Models (PartyId)
import qualified TDF.Models as Core

type AppM = ReaderT Env Handler

createSelfFanRoleRequest :: AuthedUser -> SelfFanRoleRequest -> AppM SecurityGrantRevisionDTO
createSelfFanRoleRequest user request = do
  reason <- validateSecurityAuditText "reason" 1 2000 (sfrReason request)
  sourcePlatform <- validateSecurityIdentifier "sourcePlatform" 2 50 (sfrSourcePlatform request)
  correlationId <- validateSecurityIdentifier "correlationId" 8 200 (sfrCorrelationId request)
  roleEntity <- runDB $ getBy (M.UniqueSecurityRoleCode (Core.roleRegistryCode Core.Fan))
  Entity roleKey roleValue <- maybe
    (throwError err503 { errBody = "The persisted Fan security role is unavailable" })
    pure
    roleEntity
  unless (M.securityRoleActive roleValue) $
    throwError err409 { errBody = "The persisted Fan security role is inactive" }
  unless (M.securityRoleSelfAssignable roleValue) $
    throwError err409 { errBody = "The persisted Fan security role does not allow self-service requests" }
  let partyKey = auPartyId user
  current <- runDB $ getBy (M.UniquePartySecurityRole partyKey roleKey)
  let currentActive = maybe False (M.partySecurityRoleActive . entityVal) current
      currentVersion = maybe 0 (M.partySecurityRoleVersion . entityVal) current
  when currentActive $
    throwError err409 { errBody = "The current account already has the Fan role" }
  draftState <- loadSecurityWorkflowStateKey "draft"
  reviewState <- loadSecurityWorkflowStateKey "review"
  pending <- runDB $ selectFirst
    [ M.SecurityGrantRevisionChangeKind ==. "party-role"
    , M.SecurityGrantRevisionPartyId ==. Just partyKey
    , M.SecurityGrantRevisionRoleId ==. roleKey
    , M.SecurityGrantRevisionDesiredActive ==. True
    , M.SecurityGrantRevisionWorkflowStateId <-. [draftState, reviewState]
    ]
    []
  when (isJust pending) $
    throwError err409 { errBody = "A Fan role request is already awaiting review" }
  duplicateCorrelation <- runDB $ getBy (M.UniqueSecurityGrantCorrelation correlationId)
  when (isJust duplicateCorrelation) $
    throwError err409 { errBody = "Security grant correlationId already exists" }
  now <- liftIO getCurrentTime
  let revision = M.SecurityGrantRevision
        { M.securityGrantRevisionChangeKind = "party-role"
        , M.securityGrantRevisionPartyId = Just partyKey
        , M.securityGrantRevisionRoleId = roleKey
        , M.securityGrantRevisionPermissionId = Nothing
        , M.securityGrantRevisionDesiredActive = True
        , M.securityGrantRevisionExpectedVersion = currentVersion
        , M.securityGrantRevisionWorkflowStateId = reviewState
        , M.securityGrantRevisionCreatedBy = partyKey
        , M.securityGrantRevisionCreatedAt = now
        , M.securityGrantRevisionSubmittedAt = Just now
        , M.securityGrantRevisionReviewedBy = Nothing
        , M.securityGrantRevisionReviewedAt = Nothing
        , M.securityGrantRevisionApprovedBy = Nothing
        , M.securityGrantRevisionApprovedAt = Nothing
        , M.securityGrantRevisionReviewerNotes = Nothing
        , M.securityGrantRevisionRejectionReason = Nothing
        , M.securityGrantRevisionApprovalMode = "normal"
        , M.securityGrantRevisionEmergencyReason = Nothing
        , M.securityGrantRevisionSourcePlatform = sourcePlatform
        , M.securityGrantRevisionCorrelationId = correlationId
        , M.securityGrantRevisionReason = reason
        , M.securityGrantRevisionResult = Just "self-request-submitted"
        , M.securityGrantRevisionVersion = 1
        }
  revisionKey <- runDB $ do
    insertedKey <- insert revision
    insertSecurityAuditEvent insertedKey revision "self-request-submitted"
      (Just currentActive) (Just True) (Just partyKey) Nothing Nothing "normal" "success" now
    pure insertedKey
  securityRevisionDTO (Entity revisionKey revision)

data CatalogTableFamily
  = FlatReferenceFamily
  | HierarchyFamily
  | FlatCatalogFamily
  | ServiceOfferingFamily
  | RadioAutoStopFamily
  | AppearanceModeFamily
  | ReadOnlyFamily
  deriving (Eq, Show)

-- Table and column fragments are selected only from this closed dispatcher.
-- They are protocol/implementation metadata, never the authoritative list of
-- catalogs: a matching active CatalogDefinition row is still required.
data CatalogTableSpec = CatalogTableSpec
  { ctsTable :: Text
  , ctsCatalogScoped :: Bool
  , ctsWorkflowScoped :: Bool
  , ctsFamily :: CatalogTableFamily
  , ctsCodeExpr :: Text
  , ctsNameEsExpr :: Text
  , ctsNameEnExpr :: Text
  , ctsDescriptionEsExpr :: Text
  , ctsDescriptionEnExpr :: Text
  , ctsParentExpr :: Text
  , ctsSlugExpr :: Text
  , ctsDeprecatedExpr :: Text
  , ctsReplacementExpr :: Text
  , ctsExternalCodeExpr :: Text
  , ctsSourceVersionExpr :: Text
  , ctsDisplaySymbolExpr :: Text
  , ctsUsageExpr :: Text
  , ctsDefaultScopeKind :: Maybe Text
  }

catalogTableSpec :: Text -> Maybe CatalogTableSpec
catalogTableSpec entityKind =
  case entityKind of
    "release_type" -> Just (flatReference "release_type_reference" "external_code" "source_version")
    "recording_type" -> Just (flatReference "recording_type_reference" "external_code" "source_version")
    "recording_session_type" -> Just (flatReference "recording_session_type" "external_code" "source_version")
    "service_category" -> Just (hierarchy "service_category" False)
    "service_pricing_model" -> Just (flatCatalog "service_pricing_model" False)
    "service_resource_selection_mode" -> Just (flatCatalog "service_resource_selection_mode" False)
    "service_offering" -> Just serviceOffering
    "radio_auto_stop_option" -> Just radioAutoStop
    "appearance_mode_option" -> Just appearanceMode
    "genre" -> Just (hierarchy "genre" True)
    "instrument" -> Just (hierarchy "instrument" True)
    "event_type" -> Just (flatCatalog "event_type" True)
    "booking_type" -> Just (flatCatalog "booking_type" True)
    "feedback_category" -> Just ((flatCatalog "feedback_category" True) { ctsDefaultScopeKind = Just "feedback-category" })
    "feedback_severity" -> Just ((flatCatalog "feedback_severity" True) { ctsDefaultScopeKind = Just "feedback-severity" })
    "content_category" -> Just (hierarchy "content_category" False)
    "tag" -> Just (flatCatalog "tag" True)
    "reaction_type" -> Just ((flatCatalog "reaction_type" True) { ctsDisplaySymbolExpr = "emoji" })
    "content_reaction_type" -> Just ((flatCatalog "content_reaction_type" True) { ctsDisplaySymbolExpr = "emoji" })
    "record_release" -> Just (readOnlyRecords "record_release")
    "recording" -> Just (readOnlyRecords "recording")
    "recording_session" -> Just (readOnlyRecords "recording_session")
    "record_contributor" -> Just (readOnlyCatalog "record_contributor" "NULL::uuid" False)
    "editorial_collection" -> Just (readOnlyCatalog "editorial_collection" "NULL::uuid" True)
    "authored_content" -> Just (readOnlyCatalog "authored_content" "NULL::uuid" True)
    "country_reference" -> Just ((governedReference "country_reference" "alpha3" "source_version") { ctsCodeExpr = "alpha2" })
    "subdivision_reference" -> Just (governedReferenceWithParent "subdivision_reference" "country_id" "NULL::text" "source_version")
    "city_reference" -> Just (governedReferenceWithParent "city_reference" "subdivision_id" "NULL::text" "source_version")
    "currency_reference" -> Just (governedReference "currency_reference" "code" "source_version")
    "tax_rate_reference" -> Just (governedReference "tax_rate_reference" "code" "source_version")
    "language_reference" -> Just ((governedReference "language_reference" "iso6392_t" "source_version") { ctsCodeExpr = "iso6392_t" })
    "locale_reference" -> Just (governedReferenceWithParent "locale_reference" "fallback_locale_id" "code" "source_version")
    "external_provider" -> Just (governedReference "external_provider" "code" "source_version")
    "external_provider_code" -> Just (governedReferenceWithParent "external_provider_code" "provider_id" "code" "source_version")
    "ddex_standard_version" -> Just ((governedReference "ddex_standard_version" "version_code" "source_version") { ctsCodeExpr = "standard_code || ':' || version_code" })
    "ddex_message_type" -> Just (governedReferenceWithParent "ddex_message_type" "standard_version_id" "code" "source_version")
    "ddex_vocabulary" -> Just (governedReferenceWithParent "ddex_vocabulary" "standard_version_id" "code" "source_version")
    "ddex_code" -> Just (governedReferenceWithParent "ddex_code" "vocabulary_id" "code" "source_version")
    "ddex_job_operation" -> Just (technicalRegistry "ddex_job_operation")
    "ddex_import_operation" -> Just (technicalRegistry "ddex_import_operation")
    "ddex_validation_result" -> Just (technicalRegistry "ddex_validation_result")
    "ddex_validation_severity" -> Just (technicalRegistry "ddex_validation_severity")
    "ddex_validation_layer" -> Just (technicalRegistry "ddex_validation_layer")
    _ -> Nothing
  where
    base table = CatalogTableSpec
      { ctsTable = table
      , ctsCatalogScoped = True
      , ctsWorkflowScoped = True
      , ctsFamily = ReadOnlyFamily
      , ctsCodeExpr = "code"
      , ctsNameEsExpr = "name_es"
      , ctsNameEnExpr = "name_en"
      , ctsDescriptionEsExpr = "description_es"
      , ctsDescriptionEnExpr = "description_en"
      , ctsParentExpr = "NULL::uuid"
      , ctsSlugExpr = "NULL::text"
      , ctsDeprecatedExpr = "NULL::timestamptz"
      , ctsReplacementExpr = "NULL::uuid"
      , ctsExternalCodeExpr = "NULL::text"
      , ctsSourceVersionExpr = "NULL::text"
      , ctsDisplaySymbolExpr = "NULL::text"
      , ctsUsageExpr = "0::bigint"
      , ctsDefaultScopeKind = Nothing
      }
    flatReference table externalExpr sourceExpr =
      (base table)
        { ctsFamily = FlatReferenceFamily
        , ctsDeprecatedExpr = "deprecated_at"
        , ctsReplacementExpr = "replacement_id"
        , ctsExternalCodeExpr = externalExpr
        , ctsSourceVersionExpr = sourceExpr
        }
    hierarchy table hasUsage =
      (base table)
        { ctsFamily = HierarchyFamily
        , ctsParentExpr = "parent_id"
        , ctsSlugExpr = "current_slug"
        , ctsDeprecatedExpr = "deprecated_at"
        , ctsReplacementExpr = "replacement_id"
        , ctsExternalCodeExpr = if hasUsage then "external_code" else "NULL::text"
        , ctsSourceVersionExpr = if hasUsage then "source_version" else "NULL::text"
        , ctsUsageExpr = if hasUsage then "usage_count" else "0::bigint"
        }
    flatCatalog table hasUsage =
      (base table)
        { ctsFamily = FlatCatalogFamily
        , ctsSlugExpr = "current_slug"
        , ctsDeprecatedExpr = "deprecated_at"
        , ctsReplacementExpr = "replacement_id"
        , ctsUsageExpr = if hasUsage then "usage_count" else "0::bigint"
        }
    serviceOffering =
      (base "service_offering")
        { ctsFamily = ServiceOfferingFamily
        , ctsParentExpr = "category_id"
        , ctsSlugExpr = "current_slug"
        , ctsDeprecatedExpr = "deprecated_at"
        , ctsReplacementExpr = "replacement_id"
        , ctsUsageExpr = "usage_count"
        }
    radioAutoStop =
      (base "radio_auto_stop_option")
        { ctsFamily = RadioAutoStopFamily
        , ctsSlugExpr = "current_slug"
        , ctsDeprecatedExpr = "deprecated_at"
        , ctsReplacementExpr = "replacement_id"
        }
    appearanceMode =
      (base "appearance_mode_option")
        { ctsFamily = AppearanceModeFamily
        , ctsSlugExpr = "current_slug"
        , ctsDeprecatedExpr = "deprecated_at"
        , ctsReplacementExpr = "replacement_id"
        }
    readOnlyCatalog table parentExpr hasSlug =
      (base table)
        { ctsParentExpr = parentExpr
        , ctsSlugExpr = if hasSlug then "current_slug" else "NULL::text"
        }
    readOnlyRecords table =
      (base table)
        { ctsNameEsExpr = "title_es"
        , ctsNameEnExpr = "title_en"
        , ctsSlugExpr = "current_slug"
        , ctsDeprecatedExpr = "deprecated_at"
        , ctsReplacementExpr = "replacement_id"
        , ctsUsageExpr = "usage_count"
        }
    governedReference table externalExpr sourceExpr =
      (base table)
        { ctsCatalogScoped = False
        , ctsWorkflowScoped = False
        , ctsDeprecatedExpr = "deprecated_at"
        , ctsReplacementExpr = "replacement_id"
        , ctsExternalCodeExpr = externalExpr
        , ctsSourceVersionExpr = sourceExpr
        }
    governedReferenceWithParent table parentExpr externalExpr sourceExpr =
      (governedReference table externalExpr sourceExpr) { ctsParentExpr = parentExpr }
    technicalRegistry table =
      (base table)
        { ctsCatalogScoped = False
        , ctsWorkflowScoped = False
        }

data RawCatalogItem = RawCatalogItem
  { riId :: Text
  , riCode :: Text
  , riNameEs :: Text
  , riNameEn :: Text
  , riDescriptionEs :: Maybe Text
  , riDescriptionEn :: Maybe Text
  , riSearchAliases :: [Text]
  , riCurrentSlug :: Maybe Text
  , riParentId :: Maybe Text
  , riSortOrder :: Int
  , riActive :: Bool
  , riWorkflowState :: Text
  , riDeprecatedAt :: Maybe UTCTime
  , riReplacementId :: Maybe Text
  , riExternalCode :: Maybe Text
  , riSourceVersion :: Maybe Text
  , riDisplaySymbol :: Maybe Text
  , riUsageCount :: Int64
  , riVersion :: Int
  } deriving (Show, Generic)

instance FromJSON RawCatalogItem where parseJSON = genericParseJSON rawItemJsonOptions
instance ToJSON RawCatalogItem where toJSON = genericToJSON rawItemJsonOptions

rawItemJsonOptions :: Aeson.Options
rawItemJsonOptions = defaultOptions { Aeson.fieldLabelModifier = lowerFirst . drop 2 }
  where
    lowerFirst [] = []
    lowerFirst (first : rest) = toLower first : rest

publicCatalogServer :: ServerT PublicCatalogAPI AppM
publicCatalogServer =
       listCatalogDefinitions True
  :<|> publicBatch
  :<|> publicPage
  :<|> publicItem
  :<|> publicWorkflowStates
  :<|> publicRecordsFeed
  where
    publicBatch codes locale query page pageSize ifNoneMatch =
      loadCatalogBatch True False codes locale query page pageSize >>= addBatchEtagIfChanged ifNoneMatch
    publicPage catalogCode locale query page pageSize ifNoneMatch =
      loadCatalogPageByCode True False catalogCode locale query page pageSize >>= addPageEtagIfChanged ifNoneMatch
    publicItem catalogCode itemId locale =
      loadCatalogItemById True catalogCode itemId locale
    publicWorkflowStates workflowCode locale ifNoneMatch =
      loadPublicWorkflowStates workflowCode locale >>= addWorkflowStatesEtagIfChanged ifNoneMatch
    publicRecordsFeed locale ifNoneMatch =
      loadRecordsFeed locale >>= addRecordsFeedEtagIfChanged ifNoneMatch

loadRecordsFeed :: Maybe Text -> AppM RecordsFeedDTO
loadRecordsFeed requestedLocale = do
  locale <- normalizeLocale requestedLocale
  workflowRow <- runDB (getBy (M.UniqueWorkflowDefinitionCode "catalog-publication"))
  Entity workflowKey _ <- maybe (throwError err503 { errBody = "Catalog publication workflow is unavailable" }) pure workflowRow
  publishedStateRow <- runDB (getBy (M.UniqueWorkflowStateCode workflowKey "published"))
  Entity publishedStateKey _ <- maybe (throwError err503 { errBody = "Published catalog state is unavailable" }) pure publishedStateRow
  collections <- runDB $ selectList
    [ M.EditorialCollectionCollectionType <-. ["release", "recording", "session"]
    , M.EditorialCollectionActive ==. True
    , M.EditorialCollectionWorkflowStateId ==. publishedStateKey
    ]
    [Asc M.EditorialCollectionSortOrder]
  let collectionKeys = map entityKey collections
      collectionKeysByKind kind = map entityKey (filter ((== kind) . M.editorialCollectionCollectionType . entityVal) collections)
  collectionResources <- runDB $ selectList
    [M.CollectionExternalResourceCollectionId <-. collectionKeys]
    [Asc M.CollectionExternalResourceSortOrder]
  releaseMemberships <- runDB $ selectList
    [M.CollectionReleaseCollectionId <-. collectionKeysByKind "release"]
    [Asc M.CollectionReleaseSortOrder, LimitTo 200]
  recordingMemberships <- runDB $ selectList
    [M.CollectionRecordingCollectionId <-. collectionKeysByKind "recording"]
    [Asc M.CollectionRecordingSortOrder, LimitTo 200]
  sessionMemberships <- runDB $ selectList
    [M.CollectionSessionCollectionId <-. collectionKeysByKind "session"]
    [Asc M.CollectionSessionSortOrder, LimitTo 200]
  let releaseKeys = map (M.collectionReleaseReleaseId . entityVal) releaseMemberships
      recordingKeys = map (M.collectionRecordingRecordingId . entityVal) recordingMemberships
      sessionKeys = map (M.collectionSessionSessionId . entityVal) sessionMemberships
  releases <- runDB $ selectList
    [ M.RecordReleaseId <-. releaseKeys
    , M.RecordReleaseActive ==. True
    , M.RecordReleaseWorkflowStateId ==. publishedStateKey
    ] []
  recordings <- runDB $ selectList
    [ M.RecordingId <-. recordingKeys
    , M.RecordingActive ==. True
    , M.RecordingWorkflowStateId ==. publishedStateKey
    ] []
  sessions <- runDB $ selectList
    [ M.RecordingSessionId <-. sessionKeys
    , M.RecordingSessionActive ==. True
    , M.RecordingSessionWorkflowStateId ==. publishedStateKey
    ] []
  releaseContributors <- runDB $ selectList [M.ReleaseContributorReleaseId <-. releaseKeys] [Asc M.ReleaseContributorSortOrder]
  recordingContributors <- runDB $ selectList [M.RecordingContributorRecordingId <-. recordingKeys] [Asc M.RecordingContributorSortOrder]
  sessionContributors <- runDB $ selectList [M.SessionContributorSessionId <-. sessionKeys] [Asc M.SessionContributorSortOrder]
  releaseResources <- runDB $ selectList [M.ReleaseExternalResourceReleaseId <-. releaseKeys] [Asc M.ReleaseExternalResourceSortOrder]
  recordingResources <- runDB $ selectList [M.RecordingExternalResourceRecordingId <-. recordingKeys] [Asc M.RecordingExternalResourceSortOrder]
  sessionResources <- runDB $ selectList [M.SessionExternalResourceSessionId <-. sessionKeys] [Asc M.SessionExternalResourceSortOrder]
  let contributorKeys = nub
        ( map (M.releaseContributorContributorId . entityVal) releaseContributors
       <> map (M.recordingContributorContributorId . entityVal) recordingContributors
       <> map (M.sessionContributorContributorId . entityVal) sessionContributors
        )
      resourceKeys = nub
        ( map (M.collectionExternalResourceResourceId . entityVal) collectionResources
       <> map (M.releaseExternalResourceResourceId . entityVal) releaseResources
       <> map (M.recordingExternalResourceResourceId . entityVal) recordingResources
       <> map (M.sessionExternalResourceResourceId . entityVal) sessionResources
        )
  contributors <- runDB $ selectList [M.RecordContributorId <-. contributorKeys, M.RecordContributorActive ==. True] []
  resources <- runDB $ selectList [M.RecordExternalResourceId <-. resourceKeys, M.RecordExternalResourceActive ==. True] []
  let providerKeys = nub (map (M.recordExternalResourceProviderId . entityVal) resources)
  providers <- runDB $ selectList [M.ExternalProviderId <-. providerKeys, M.ExternalProviderActive ==. True] []
  let contributorMap = Map.fromList [(entityKey row, row) | row <- contributors]
      resourceMap = Map.fromList [(entityKey row, row) | row <- resources]
      providerMap = Map.fromList [(entityKey row, row) | row <- providers]
      releaseOrder = Map.fromList [(M.collectionReleaseReleaseId row, M.collectionReleaseSortOrder row) | Entity _ row <- releaseMemberships]
      recordingOrder = Map.fromList [(M.collectionRecordingRecordingId row, M.collectionRecordingSortOrder row) | Entity _ row <- recordingMemberships]
      sessionOrder = Map.fromList [(M.collectionSessionSessionId row, M.collectionSessionSortOrder row) | Entity _ row <- sessionMemberships]
      orderedReleases = sortOn (\row -> Map.findWithDefault maxBound (entityKey row) releaseOrder) releases
      orderedRecordings = sortOn (\row -> Map.findWithDefault maxBound (entityKey row) recordingOrder) recordings
      orderedSessions = sortOn (\row -> Map.findWithDefault maxBound (entityKey row) sessionOrder) sessions
      releaseDTOFor row = recordsReleaseDTO locale contributorMap resourceMap providerMap releaseContributors releaseResources releaseOrder row
      recordingDTOFor row = recordsRecordingDTO locale contributorMap resourceMap providerMap recordingContributors recordingResources recordingOrder row
      sessionDTOFor row = recordsSessionDTO locale contributorMap resourceMap providerMap sessionContributors sessionResources sessionOrder row
      collectionDTOFor row = recordsCollectionDTO locale resourceMap providerMap collectionResources row
      versionTotal =
        sum (map (fromIntegral . M.editorialCollectionVersion . entityVal) collections)
          + sum (map (fromIntegral . M.recordReleaseVersion . entityVal) releases)
          + sum (map (fromIntegral . M.recordingVersion . entityVal) recordings)
          + sum (map (fromIntegral . M.recordingSessionVersion . entityVal) sessions)
          + sum (map (fromIntegral . M.recordExternalResourceVersion . entityVal) resources)
  pure RecordsFeedDTO
    { rfLocale = locale
    , rfRevision = max 1 versionTotal
    , rfCollections = map collectionDTOFor collections
    , rfReleases = map releaseDTOFor orderedReleases
    , rfRecordings = map recordingDTOFor orderedRecordings
    , rfSessions = map sessionDTOFor orderedSessions
    }

recordsCollectionDTO
  :: Text
  -> Map.Map M.RecordExternalResourceId (Entity M.RecordExternalResource)
  -> Map.Map M.ExternalProviderId (Entity M.ExternalProvider)
  -> [Entity M.CollectionExternalResource]
  -> Entity M.EditorialCollection
  -> RecordsCollectionDTO
recordsCollectionDTO locale resources providers relationships (Entity collectionKey collection) =
  RecordsCollectionDTO
    { rcoId = persistKeyText collectionKey
    , rcoCode = M.editorialCollectionCode collection
    , rcoKind = M.editorialCollectionCollectionType collection
    , rcoName = localized locale (M.editorialCollectionNameEs collection) (M.editorialCollectionNameEn collection)
    , rcoDescription = localizedMaybe locale (M.editorialCollectionDescriptionEs collection) (M.editorialCollectionDescriptionEn collection)
    , rcoPublicRoute = M.editorialCollectionPublicRoute collection
    , rcoResources = mapMaybeCollectionResource locale collectionKey relationships resources providers
    , rcoRevision = M.editorialCollectionVersion collection
    }

recordsReleaseDTO
  :: Text
  -> Map.Map M.RecordContributorId (Entity M.RecordContributor)
  -> Map.Map M.RecordExternalResourceId (Entity M.RecordExternalResource)
  -> Map.Map M.ExternalProviderId (Entity M.ExternalProvider)
  -> [Entity M.ReleaseContributor]
  -> [Entity M.ReleaseExternalResource]
  -> Map.Map M.RecordReleaseId Int
  -> Entity M.RecordRelease
  -> RecordsReleaseDTO
recordsReleaseDTO locale contributors resources providers creditRows resourceRows ordering (Entity releaseKey release) =
  RecordsReleaseDTO
    { rreId = persistKeyText releaseKey
    , rreCode = M.recordReleaseCode release
    , rreSlug = M.recordReleaseCurrentSlug release
    , rreTitle = localized locale (M.recordReleaseTitleEs release) (M.recordReleaseTitleEn release)
    , rreDescription = localizedMaybe locale (M.recordReleaseDescriptionEs release) (M.recordReleaseDescriptionEn release)
    , rreReleaseTypeId = persistKeyText (M.recordReleaseReleaseTypeId release)
    , rreReleaseDate = M.recordReleaseReleaseDate release
    , rreContributors = contributorDTOs locale contributors
        [ (M.releaseContributorContributorId row, M.releaseContributorSortOrder row)
        | Entity _ row <- creditRows, M.releaseContributorReleaseId row == releaseKey
        ]
    , rreResources = releaseResourceDTOs locale releaseKey resourceRows resources providers
    , rreSortOrder = Map.findWithDefault (M.recordReleaseSortOrder release) releaseKey ordering
    , rreRevision = M.recordReleaseVersion release
    }

recordsRecordingDTO
  :: Text
  -> Map.Map M.RecordContributorId (Entity M.RecordContributor)
  -> Map.Map M.RecordExternalResourceId (Entity M.RecordExternalResource)
  -> Map.Map M.ExternalProviderId (Entity M.ExternalProvider)
  -> [Entity M.RecordingContributor]
  -> [Entity M.RecordingExternalResource]
  -> Map.Map M.RecordingId Int
  -> Entity M.Recording
  -> RecordsRecordingDTO
recordsRecordingDTO locale contributors resources providers creditRows resourceRows ordering (Entity recordingKey recording) =
  RecordsRecordingDTO
    { rrgId = persistKeyText recordingKey
    , rrgCode = M.recordingCode recording
    , rrgSlug = M.recordingCurrentSlug recording
    , rrgTitle = localized locale (M.recordingTitleEs recording) (M.recordingTitleEn recording)
    , rrgDescription = localizedMaybe locale (M.recordingDescriptionEs recording) (M.recordingDescriptionEn recording)
    , rrgRecordingTypeId = persistKeyText (M.recordingRecordingTypeId recording)
    , rrgDurationMs = M.recordingDurationMs recording
    , rrgContributors = contributorDTOs locale contributors
        [ (M.recordingContributorContributorId row, M.recordingContributorSortOrder row)
        | Entity _ row <- creditRows, M.recordingContributorRecordingId row == recordingKey
        ]
    , rrgResources = recordingResourceDTOs locale recordingKey resourceRows resources providers
    , rrgSortOrder = Map.findWithDefault (M.recordingSortOrder recording) recordingKey ordering
    , rrgRevision = M.recordingVersion recording
    }

recordsSessionDTO
  :: Text
  -> Map.Map M.RecordContributorId (Entity M.RecordContributor)
  -> Map.Map M.RecordExternalResourceId (Entity M.RecordExternalResource)
  -> Map.Map M.ExternalProviderId (Entity M.ExternalProvider)
  -> [Entity M.SessionContributor]
  -> [Entity M.SessionExternalResource]
  -> Map.Map M.RecordingSessionId Int
  -> Entity M.RecordingSession
  -> RecordsSessionDTO
recordsSessionDTO locale contributors resources providers creditRows resourceRows ordering (Entity sessionKey session) =
  RecordsSessionDTO
    { rssId = persistKeyText sessionKey
    , rssCode = M.recordingSessionCode session
    , rssSlug = M.recordingSessionCurrentSlug session
    , rssTitle = localized locale (M.recordingSessionTitleEs session) (M.recordingSessionTitleEn session)
    , rssDescription = localizedMaybe locale (M.recordingSessionDescriptionEs session) (M.recordingSessionDescriptionEn session)
    , rssSessionTypeId = persistKeyText (M.recordingSessionSessionTypeId session)
    , rssRecordedAt = M.recordingSessionRecordedAt session
    , rssContributors = contributorDTOs locale contributors
        [ (M.sessionContributorContributorId row, M.sessionContributorSortOrder row)
        | Entity _ row <- creditRows, M.sessionContributorSessionId row == sessionKey
        ]
    , rssResources = sessionResourceDTOs locale sessionKey resourceRows resources providers
    , rssSortOrder = Map.findWithDefault (M.recordingSessionSortOrder session) sessionKey ordering
    , rssRevision = M.recordingSessionVersion session
    }

contributorDTOs
  :: Text
  -> Map.Map M.RecordContributorId (Entity M.RecordContributor)
  -> [(M.RecordContributorId, Int)]
  -> [RecordsContributorDTO]
contributorDTOs locale contributorMap relationships =
  [ RecordsContributorDTO
      { rcId = persistKeyText contributorKey
      , rcCode = M.recordContributorCode contributor
      , rcKind = M.recordContributorContributorKind contributor
      , rcName = localized locale (M.recordContributorNameEs contributor) (M.recordContributorNameEn contributor)
      }
  | (contributorKey, _) <- sortOn snd relationships
  , Just (Entity _ contributor) <- [Map.lookup contributorKey contributorMap]
  ]

releaseResourceDTOs
  :: Text
  -> M.RecordReleaseId
  -> [Entity M.ReleaseExternalResource]
  -> Map.Map M.RecordExternalResourceId (Entity M.RecordExternalResource)
  -> Map.Map M.ExternalProviderId (Entity M.ExternalProvider)
  -> [RecordsResourceDTO]
releaseResourceDTOs locale parentKey relationships resources providers =
  [ resourceDTO locale (M.releaseExternalResourceRelationKind relation) (M.releaseExternalResourcePrimaryResource relation) (M.releaseExternalResourceSortOrder relation) resource providers
  | Entity _ relation <- relationships
  , M.releaseExternalResourceReleaseId relation == parentKey
  , Just resource <- [Map.lookup (M.releaseExternalResourceResourceId relation) resources]
  ]

recordingResourceDTOs
  :: Text
  -> M.RecordingId
  -> [Entity M.RecordingExternalResource]
  -> Map.Map M.RecordExternalResourceId (Entity M.RecordExternalResource)
  -> Map.Map M.ExternalProviderId (Entity M.ExternalProvider)
  -> [RecordsResourceDTO]
recordingResourceDTOs locale parentKey relationships resources providers =
  [ resourceDTO locale (M.recordingExternalResourceRelationKind relation) (M.recordingExternalResourcePrimaryResource relation) (M.recordingExternalResourceSortOrder relation) resource providers
  | Entity _ relation <- relationships
  , M.recordingExternalResourceRecordingId relation == parentKey
  , Just resource <- [Map.lookup (M.recordingExternalResourceResourceId relation) resources]
  ]

sessionResourceDTOs
  :: Text
  -> M.RecordingSessionId
  -> [Entity M.SessionExternalResource]
  -> Map.Map M.RecordExternalResourceId (Entity M.RecordExternalResource)
  -> Map.Map M.ExternalProviderId (Entity M.ExternalProvider)
  -> [RecordsResourceDTO]
sessionResourceDTOs locale parentKey relationships resources providers =
  [ resourceDTO locale (M.sessionExternalResourceRelationKind relation) (M.sessionExternalResourcePrimaryResource relation) (M.sessionExternalResourceSortOrder relation) resource providers
  | Entity _ relation <- relationships
  , M.sessionExternalResourceSessionId relation == parentKey
  , Just resource <- [Map.lookup (M.sessionExternalResourceResourceId relation) resources]
  ]

mapMaybeCollectionResource
  :: Text
  -> M.EditorialCollectionId
  -> [Entity M.CollectionExternalResource]
  -> Map.Map M.RecordExternalResourceId (Entity M.RecordExternalResource)
  -> Map.Map M.ExternalProviderId (Entity M.ExternalProvider)
  -> [RecordsResourceDTO]
mapMaybeCollectionResource locale parentKey relationships resources providers =
  [ resourceDTO locale (M.collectionExternalResourceRelationKind relation) (M.collectionExternalResourcePrimaryResource relation) (M.collectionExternalResourceSortOrder relation) resource providers
  | Entity _ relation <- relationships
  , M.collectionExternalResourceCollectionId relation == parentKey
  , Just resource <- [Map.lookup (M.collectionExternalResourceResourceId relation) resources]
  ]

resourceDTO
  :: Text
  -> Text
  -> Bool
  -> Int
  -> Entity M.RecordExternalResource
  -> Map.Map M.ExternalProviderId (Entity M.ExternalProvider)
  -> RecordsResourceDTO
resourceDTO locale relationKind primaryResource sortOrder (Entity resourceKey resource) providers =
  RecordsResourceDTO
    { rrId = persistKeyText resourceKey
    , rrProviderCode = maybe "unknown" (M.externalProviderInternalCode . entityVal) (Map.lookup (M.recordExternalResourceProviderId resource) providers)
    , rrKind = M.recordExternalResourceResourceKind resource
    , rrExternalCode = M.recordExternalResourceExternalCode resource
    , rrUrl = M.recordExternalResourceCanonicalUrl resource
    , rrLabel = localizedMaybe locale (M.recordExternalResourceLabelEs resource) (M.recordExternalResourceLabelEn resource)
    , rrDurationMs = M.recordExternalResourceDurationMs resource
    , rrThumbnailUrl = M.recordExternalResourceThumbnailUrl resource
    , rrRelationKind = relationKind
    , rrPrimary = primaryResource
    , rrSortOrder = sortOrder
    }

localized :: Text -> Text -> Text -> Text
localized locale spanish english
  | T.isPrefixOf "en" locale = english
  | otherwise = spanish

localizedMaybe :: Text -> Maybe Text -> Maybe Text -> Maybe Text
localizedMaybe locale spanish english
  | T.isPrefixOf "en" locale = english <|> spanish
  | otherwise = spanish <|> english

addRecordsFeedEtagIfChanged :: Maybe Text -> RecordsFeedDTO -> AppM (Headers '[Header "ETag" Text] RecordsFeedDTO)
addRecordsFeedEtagIfChanged ifNoneMatch payload = do
  let etag = etagValue (rfRevision payload)
  rejectNotModified ifNoneMatch etag
  pure (addHeader etag payload)

catalogServer :: AuthedUser -> ServerT CatalogAPI AppM
catalogServer user =
       adminContentTypes
  :<|> adminWorkflowStates
  :<|> adminAuthoredContents
  :<|> adminDefinitions
  :<|> adminBatch
  :<|> adminPage
  :<|> adminItem
  :<|> listRevisionsHandler user
  :<|> createRevisionHandler user
  :<|> submitRevisionHandler user
  :<|> approveRevisionHandler user
  :<|> rejectRevisionHandler user
  :<|> activationHandler user
  :<|> reorderHandler user
  :<|> mergeHandler user
  :<|> usageHandler user
  :<|> exportCsvHandler user
  :<|> importCsvHandler user
  :<|> securityAdminServer user
  where
    adminContentTypes locale = do
      requireCatalogCapability user "catalog.read"
      listContentTypes locale
    adminWorkflowStates workflowCode locale = do
      requireCatalogCapability user "catalog.read"
      sensitiveRequest <- requestedWorkflowIsSensitive workflowCode
      when sensitiveRequest (requireSecurityCapability user "security.read")
      listWorkflowStates sensitiveRequest workflowCode locale
    adminAuthoredContents locale = do
      requireCatalogCapability user "catalog.read"
      listAuthoredContents locale
    adminDefinitions locale = do
      requireCatalogCapability user "catalog.read"
      listCatalogDefinitions False locale
    adminBatch codes locale query page pageSize includeInactive ifNoneMatch = do
      requireCatalogCapability user "catalog.read"
      loadCatalogBatch False (fromMaybe False includeInactive) codes locale query page pageSize >>= addBatchEtagIfChanged ifNoneMatch
    adminPage catalogCode locale query page pageSize includeInactive ifNoneMatch = do
      requireCatalogCapability user "catalog.read"
      loadCatalogPageByCode False (fromMaybe False includeInactive) catalogCode locale query page pageSize >>= addPageEtagIfChanged ifNoneMatch
    adminItem catalogCode itemId locale = do
      requireCatalogCapability user "catalog.read"
      loadCatalogItemById False catalogCode itemId locale

listContentTypes :: Maybe Text -> AppM [ContentTypeDTO]
listContentTypes requestedLocale = do
  locale <- normalizeLocale requestedLocale
  contentTypeRows <- runDB $ selectList [M.ContentTypeActive ==. True] [Asc M.ContentTypeCode]
  let workflowStateKeys = nub (map (M.contentTypeWorkflowStateId . entityVal) contentTypeRows)
  workflowStates <- runDB $ selectList [M.WorkflowStateId <-. workflowStateKeys, M.WorkflowStateActive ==. True] []
  let workflowStateCodes = Map.fromList
        [ (entityKey state, M.workflowStateCode (entityVal state))
        | state <- workflowStates
        ]
  forM contentTypeRows $ \contentTypeRow@(Entity _ value) -> do
    workflowState <- maybe
      (throwError err503 { errBody = "Content type references an unavailable workflow state" })
      pure
      (Map.lookup (M.contentTypeWorkflowStateId value) workflowStateCodes)
    pure (contentTypeDTO locale workflowState contentTypeRow)

contentTypeDTO :: Text -> Text -> Entity M.ContentType -> ContentTypeDTO
contentTypeDTO locale workflowState (Entity key value) =
  ContentTypeDTO
    { ctId = persistKeyText key
    , ctCode = M.contentTypeCode value
    , ctEntityKind = M.contentTypeEntityKind value
    , ctName = chooseLocale locale (M.contentTypeNameEs value) (M.contentTypeNameEn value)
    , ctNameEs = M.contentTypeNameEs value
    , ctNameEn = M.contentTypeNameEn value
    , ctDescription = chooseLocaleMaybe locale (M.contentTypeDescriptionEs value) (M.contentTypeDescriptionEn value)
    , ctDescriptionEs = M.contentTypeDescriptionEs value
    , ctDescriptionEn = M.contentTypeDescriptionEn value
    , ctSchema = unAesonValue (M.contentTypeSchemaJson value)
    , ctSchemaVersion = M.contentTypeSchemaVersion value
    , ctPublicRoutePattern = M.contentTypePublicRoutePattern value
    , ctAdminRoutePattern = M.contentTypeAdminRoutePattern value
    , ctPublicRead = M.contentTypePublicRead value
    , ctActive = M.contentTypeActive value
    , ctWorkflowState = workflowState
    , ctVersion = M.contentTypeVersion value
    }

requestedWorkflowIsSensitive :: Maybe Text -> AppM Bool
requestedWorkflowIsSensitive Nothing = pure False
requestedWorkflowIsSensitive (Just rawWorkflowCode) = do
  let workflowCode = T.toLower (T.strip rawWorkflowCode)
  workflowRow <- runDB $ selectFirst
    [ M.WorkflowDefinitionCode ==. workflowCode
    , M.WorkflowDefinitionActive ==. True
    ]
    []
  pure (maybe False (M.workflowDefinitionSensitive . entityVal) workflowRow)

listWorkflowStates :: Bool -> Maybe Text -> Maybe Text -> AppM [WorkflowStateDTO]
listWorkflowStates includeSensitive requestedWorkflowCode requestedLocale = do
  locale <- normalizeLocale requestedLocale
  let normalizedWorkflowCode = T.toLower . T.strip <$> requestedWorkflowCode
  when (maybe False T.null normalizedWorkflowCode) $
    throwError err400 { errBody = "workflowCode must be omitted or non-empty" }
  workflowRows <- runDB $ selectList
    ([M.WorkflowDefinitionActive ==. True]
      <> [M.WorkflowDefinitionSensitive ==. False | not includeSensitive]
      <> maybe [] (\code -> [M.WorkflowDefinitionCode ==. code]) normalizedWorkflowCode)
    [Asc M.WorkflowDefinitionCode]
  workflowStateDTOs locale workflowRows

loadPublicWorkflowStates :: Text -> Maybe Text -> AppM WorkflowStatesDTO
loadPublicWorkflowStates rawWorkflowCode requestedLocale = do
  locale <- normalizeLocale requestedLocale
  let workflowCode = T.toLower (T.strip rawWorkflowCode)
  when (T.null workflowCode) $
    throwError err400 { errBody = "workflowCode must be non-empty" }
  workflowRow <- runDB $ selectFirst
    [ M.WorkflowDefinitionCode ==. workflowCode
    , M.WorkflowDefinitionActive ==. True
    , M.WorkflowDefinitionPublicRead ==. True
    , M.WorkflowDefinitionSensitive ==. False
    ]
    []
  row@(Entity _ workflow) <- maybe
    (throwError err404 { errBody = "Public workflow not found" })
    pure
    workflowRow
  states <- workflowStateDTOs locale [row]
  pure WorkflowStatesDTO
    { wseWorkflowCode = workflowCode
    , wseLocale = locale
    , wseRevision = M.workflowDefinitionCacheRevision workflow
    , wseStates = states
    }

workflowStateDTOs :: Text -> [Entity M.WorkflowDefinition] -> AppM [WorkflowStateDTO]
workflowStateDTOs locale workflowRows = do
  let workflowKeys = map entityKey workflowRows
      workflowMap = Map.fromList [(entityKey row, entityVal row) | row <- workflowRows]
  stateRows <- runDB $ selectList
    [ M.WorkflowStateWorkflowId <-. workflowKeys
    , M.WorkflowStateActive ==. True
    ]
    [Asc M.WorkflowStateSortOrder, Asc M.WorkflowStateCode]
  let stateKeys = map entityKey stateRows
  defaultRows <- runDB $ selectList
    [ M.WorkflowDefaultStateWorkflowId <-. workflowKeys
    , M.WorkflowDefaultStateStateId <-. stateKeys
    , M.WorkflowDefaultStateActive ==. True
    ]
    [Asc M.WorkflowDefaultStateContext]
  capabilityRows <- runDB $ selectList
    [ M.WorkflowStateCapabilityStateId <-. stateKeys
    , M.WorkflowStateCapabilityEnabled ==. True
    ]
    [Asc M.WorkflowStateCapabilityCapabilityCode]
  transitionRows <- runDB $ selectList
    [ M.WorkflowTransitionWorkflowId <-. workflowKeys
    , M.WorkflowTransitionFromStateId <-. stateKeys
    , M.WorkflowTransitionToStateId <-. stateKeys
    , M.WorkflowTransitionActive ==. True
    ]
    []
  let defaultsByState = Map.fromListWith (<>)
        [ (M.workflowDefaultStateStateId value, [M.workflowDefaultStateContext value])
        | Entity _ value <- defaultRows
        ]
      capabilitiesByState = Map.fromListWith (<>)
        [ (M.workflowStateCapabilityStateId value, [M.workflowStateCapabilityCapabilityCode value])
        | Entity _ value <- capabilityRows
        ]
      transitionsByState = Map.fromListWith (<>)
        [ (M.workflowTransitionFromStateId value, [workflowTransitionDTO value])
        | Entity _ value <- transitionRows
        ]
  forM stateRows $ \stateRow@(Entity _ stateValue) -> do
    workflow <- maybe
      (throwError err503 { errBody = "Workflow state references an unavailable workflow" })
      pure
      (Map.lookup (M.workflowStateWorkflowId stateValue) workflowMap)
    pure (workflowStateDTO locale workflow defaultsByState capabilitiesByState transitionsByState stateRow)

workflowStateDTO
  :: Text
  -> M.WorkflowDefinition
  -> Map.Map M.WorkflowStateId [Text]
  -> Map.Map M.WorkflowStateId [Text]
  -> Map.Map M.WorkflowStateId [WorkflowTransitionDTO]
  -> Entity M.WorkflowState
  -> WorkflowStateDTO
workflowStateDTO locale workflow defaultsByState capabilitiesByState transitionsByState (Entity key value) =
  WorkflowStateDTO
    { wsId = persistKeyText key
    , wsWorkflowId = persistKeyText (M.workflowStateWorkflowId value)
    , wsWorkflowCode = M.workflowDefinitionCode workflow
    , wsCode = M.workflowStateCode value
    , wsName = chooseLocale locale (M.workflowStateNameEs value) (M.workflowStateNameEn value)
    , wsNameEs = M.workflowStateNameEs value
    , wsNameEn = M.workflowStateNameEn value
    , wsSortOrder = M.workflowStateSortOrder value
    , wsTerminal = M.workflowStateTerminal value
    , wsActive = M.workflowStateActive value
    , wsInitialContexts = sortOn id (Map.findWithDefault [] key defaultsByState)
    , wsCapabilities = sortOn id (Map.findWithDefault [] key capabilitiesByState)
    , wsTransitions = sortOn wtrToStateId (Map.findWithDefault [] key transitionsByState)
    , wsVersion = M.workflowStateVersion value
    }

workflowTransitionDTO :: M.WorkflowTransition -> WorkflowTransitionDTO
workflowTransitionDTO value =
  WorkflowTransitionDTO
    { wtrToStateId = persistKeyText (M.workflowTransitionToStateId value)
    , wtrDirectExecutionAllowed =
        isNothing (M.workflowTransitionRequiredPermissionId value)
          && not (M.workflowTransitionRequiresReview value)
          && not (M.workflowTransitionRequiresDistinctApprover value)
    , wtrRequiresReview = M.workflowTransitionRequiresReview value
    , wtrRequiresDistinctApprover = M.workflowTransitionRequiresDistinctApprover value
    , wtrEffectiveFrom = M.workflowTransitionEffectiveFrom value
    , wtrEffectiveUntil = M.workflowTransitionEffectiveUntil value
    , wtrVersion = M.workflowTransitionVersion value
    }

listAuthoredContents :: Maybe Text -> AppM [AuthoredContentDTO]
listAuthoredContents requestedLocale = do
  locale <- normalizeLocale requestedLocale
  authoredRows <- runDB $ selectList [M.AuthoredContentActive ==. True] [Asc M.AuthoredContentSortOrder, Asc M.AuthoredContentCode]
  let contentTypeKeys = nub (map (M.authoredContentContentTypeId . entityVal) authoredRows)
      workflowStateKeys = nub (map (M.authoredContentWorkflowStateId . entityVal) authoredRows)
  contentTypeRows <- runDB $ selectList [M.ContentTypeId <-. contentTypeKeys, M.ContentTypeActive ==. True] []
  workflowStateRows <- runDB $ selectList [M.WorkflowStateId <-. workflowStateKeys, M.WorkflowStateActive ==. True] []
  let contentTypeMap = Map.fromList [(entityKey row, row) | row <- contentTypeRows]
      workflowStateMap = Map.fromList
        [ (entityKey row, M.workflowStateCode (entityVal row))
        | row <- workflowStateRows
        ]
  forM authoredRows $ \authoredRow@(Entity _ value) -> do
    contentTypeRow <- maybe
      (throwError err503 { errBody = "Authored content references an unavailable content type" })
      pure
      (Map.lookup (M.authoredContentContentTypeId value) contentTypeMap)
    workflowState <- maybe
      (throwError err503 { errBody = "Authored content references an unavailable workflow state" })
      pure
      (Map.lookup (M.authoredContentWorkflowStateId value) workflowStateMap)
    pure (authoredContentDTO locale workflowState contentTypeRow authoredRow)

authoredContentDTO :: Text -> Text -> Entity M.ContentType -> Entity M.AuthoredContent -> AuthoredContentDTO
authoredContentDTO locale workflowState (Entity contentTypeKey contentTypeValue) (Entity authoredKey authoredValue) =
  AuthoredContentDTO
    { acId = persistKeyText authoredKey
    , acCode = M.authoredContentCode authoredValue
    , acContentTypeId = persistKeyText contentTypeKey
    , acContentTypeCode = M.contentTypeCode contentTypeValue
    , acEntityKind = M.contentTypeEntityKind contentTypeValue
    , acName = chooseLocale locale (M.authoredContentNameEs authoredValue) (M.authoredContentNameEn authoredValue)
    , acNameEs = M.authoredContentNameEs authoredValue
    , acNameEn = M.authoredContentNameEn authoredValue
    , acDescription = chooseLocaleMaybe locale (M.authoredContentDescriptionEs authoredValue) (M.authoredContentDescriptionEn authoredValue)
    , acDescriptionEs = M.authoredContentDescriptionEs authoredValue
    , acDescriptionEn = M.authoredContentDescriptionEn authoredValue
    , acCurrentSlug = M.authoredContentCurrentSlug authoredValue
    , acPublicRoute = M.authoredContentPublicRoute authoredValue
    , acSchema = unAesonValue (M.contentTypeSchemaJson contentTypeValue)
    , acSchemaVersion = M.contentTypeSchemaVersion contentTypeValue
    , acSortOrder = M.authoredContentSortOrder authoredValue
    , acActive = M.authoredContentActive authoredValue
    , acWorkflowState = workflowState
    , acRevision = M.authoredContentPublishedRevision authoredValue
    , acVersion = M.authoredContentVersion authoredValue
    }

requireCatalogAccess :: AuthedUser -> AppM a -> AppM a
requireCatalogAccess user action = do
  requireCatalogCapability user "catalog.read"
  action

requireCatalogCapability :: AuthedUser -> Text -> AppM ()
requireCatalogCapability user permissionCode = do
  either throwError pure (validateModuleAccess ModuleCatalog user)
  rows <- runDB $ rawSql
    "SELECT EXISTS (SELECT 1 FROM party_security_role psr JOIN security_role r ON r.id=psr.role_id JOIN role_permission rp ON rp.role_id=r.id JOIN security_permission p ON p.id=rp.permission_id JOIN security_action a ON a.id=p.action_id JOIN security_module m ON m.id=p.module_id WHERE psr.party_id=? AND psr.active=TRUE AND r.active=TRUE AND rp.active=TRUE AND p.active=TRUE AND a.active=TRUE AND m.active=TRUE AND p.code=?)"
    [toPersistValue (auPartyId user), PersistText permissionCode]
  case rows of
    [Single True] -> pure ()
    _ -> throwError err403
      { errBody = BL.fromStrict (TE.encodeUtf8 ("Missing catalog capability: " <> permissionCode))
      }

requireCatalogApprovalAccess :: AuthedUser -> AppM ()
requireCatalogApprovalAccess user = do
  requireCatalogCapability user "catalog.approve"
  requireCatalogCapability user "catalog.publish"

runDB :: SqlPersistT IO a -> AppM a
runDB action = do
  pool <- asks envPool
  liftIO (runSqlPool action pool)

normalizeLocale :: Maybe Text -> AppM Text
normalizeLocale Nothing = do
  defaultEnablement <- runDB $ selectFirst
    [ M.DeploymentLocaleEnablementDeploymentCode ==. "default"
    , M.DeploymentLocaleEnablementEnabled ==. True
    , M.DeploymentLocaleEnablementDefaultLocale ==. True
    ]
    []
  case defaultEnablement of
    Nothing -> throwError err500 { errBody = "Persisted default locale is unavailable" }
    Just (Entity _ enablement) -> do
      localeReference <- runDB $ get (M.deploymentLocaleEnablementLocaleId enablement)
      case localeReference of
        Just locale
          | M.localeReferenceActive locale
          , isNothing (M.localeReferenceDeprecatedAt locale) -> pure (M.localeReferenceCode locale)
        _ -> throwError err500 { errBody = "Persisted default locale is unavailable" }
normalizeLocale (Just requested) = do
  let candidate = T.toLower (T.strip requested)
  when (T.null candidate || T.length candidate > 35) $
    throwError err400 { errBody = "Invalid locale" }
  localeReference <- runDB $ selectFirst
    [ M.LocaleReferenceCode ==. candidate
    , M.LocaleReferenceActive ==. True
    , M.LocaleReferenceDeprecatedAt ==. Nothing
    ]
    []
  maybe (throwError err400 { errBody = "Unsupported locale" }) (const (pure candidate)) localeReference

listCatalogDefinitions :: Bool -> Maybe Text -> AppM [CatalogDefinitionDTO]
listCatalogDefinitions publicOnly requestedLocale = do
  locale <- normalizeLocale requestedLocale
  definitions <- runDB $ selectList filters [Asc M.CatalogDefinitionCode]
  pure (map (definitionDTO locale) definitions)
  where
    filters =
      [M.CatalogDefinitionActive ==. True]
        <> [M.CatalogDefinitionPublicRead ==. True | publicOnly]

definitionDTO :: Text -> Entity M.CatalogDefinition -> CatalogDefinitionDTO
definitionDTO locale (Entity key value) =
  CatalogDefinitionDTO
    { cdId = persistKeyText key
    , cdCode = M.catalogDefinitionCode value
    , cdClassification = M.catalogDefinitionClassification value
    , cdEntityKind = M.catalogDefinitionEntityKind value
    , cdName = chooseLocale locale (M.catalogDefinitionNameEs value) (M.catalogDefinitionNameEn value)
    , cdDescription = chooseLocaleMaybe locale (M.catalogDefinitionDescriptionEs value) (M.catalogDefinitionDescriptionEn value)
    , cdPublicRead = M.catalogDefinitionPublicRead value
    , cdSensitive = M.catalogDefinitionSensitive value
    , cdOrderingMode = M.catalogDefinitionOrderingMode value
    , cdSourceName = M.catalogDefinitionSourceName value
    , cdSourceVersion = M.catalogDefinitionSourceVersion value
    , cdSourceEffectiveDate = M.catalogDefinitionSourceEffectiveDate value
    , cdLastSyncedAt = M.catalogDefinitionLastSyncedAt value
    , cdCacheRevision = M.catalogDefinitionCacheRevision value
    , cdActive = M.catalogDefinitionActive value
    , cdVersion = M.catalogDefinitionVersion value
    }

chooseLocale :: Text -> Text -> Text -> Text
chooseLocale locale spanish english
  | "en" `T.isPrefixOf` locale = english
  | otherwise = spanish

chooseLocaleMaybe :: Text -> Maybe Text -> Maybe Text -> Maybe Text
chooseLocaleMaybe locale spanish english
  | "en" `T.isPrefixOf` locale = english <|> spanish
  | otherwise = spanish <|> english

loadCatalogBatch :: Bool -> Bool -> [Text] -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM CatalogBatchDTO
loadCatalogBatch publicOnly includeInactive requestedCodes requestedLocale query requestedPage requestedPageSize = do
  locale <- normalizeLocale requestedLocale
  allDefinitions <- runDB $ selectList filters [Asc M.CatalogDefinitionCode]
  let normalizedCodes = nub (map (T.toLower . T.strip) requestedCodes)
      selected =
        if null normalizedCodes
          then allDefinitions
          else filter ((`elem` normalizedCodes) . T.toLower . M.catalogDefinitionCode . entityVal) allDefinitions
      unknownCodes = normalizedCodes >>= \catalogCode -> [catalogCode | all ((/= catalogCode) . T.toLower . M.catalogDefinitionCode . entityVal) allDefinitions]
  unless (null unknownCodes) $
    throwError err404 { errBody = BL.fromStrict (TE.encodeUtf8 ("Unknown catalog codes: " <> T.intercalate ", " unknownCodes)) }
  pages <- forM selected $ \definition -> loadCatalogPage publicOnly includeInactive definition locale query requestedPage requestedPageSize
  -- Catalog revisions are monotonic. Summing them makes the batch token change
  -- when any requested catalog changes; using only the maximum could hide an
  -- update to a catalog whose revision was below another page's revision.
  let revision = sum (map cpRevision pages)
  pure CatalogBatchDTO { cbCatalogs = pages, cbRevision = revision, cbLocale = locale }
  where
    filters =
      [M.CatalogDefinitionActive ==. True]
        <> [M.CatalogDefinitionPublicRead ==. True | publicOnly]

loadCatalogPageByCode :: Bool -> Bool -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM CatalogPageDTO
loadCatalogPageByCode publicOnly includeInactive rawCode requestedLocale query requestedPage requestedPageSize = do
  locale <- normalizeLocale requestedLocale
  definition <- findCatalogDefinition publicOnly rawCode
  loadCatalogPage publicOnly includeInactive definition locale query requestedPage requestedPageSize

findCatalogDefinition :: Bool -> Text -> AppM (Entity M.CatalogDefinition)
findCatalogDefinition publicOnly rawCode = do
  let catalogCode = T.toLower (T.strip rawCode)
  when (T.null catalogCode || T.length catalogCode > 100) $
    throwError err400 { errBody = "Invalid catalog code" }
  result <- runDB $ selectFirst filters []
  maybe (throwError err404 { errBody = "Catalog not found" }) pure result
  where
    filters =
      [ M.CatalogDefinitionCode ==. T.toLower (T.strip rawCode)
      , M.CatalogDefinitionActive ==. True
      ] <> [M.CatalogDefinitionPublicRead ==. True | publicOnly]

loadCatalogPage :: Bool -> Bool -> Entity M.CatalogDefinition -> Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM CatalogPageDTO
loadCatalogPage publicOnly includeInactive definition@(Entity definitionKey definitionValue) locale rawQuery requestedPage requestedPageSize = do
  spec <- maybe (throwError err501 { errBody = "Catalog has no registered typed query adapter" }) pure (catalogTableSpec (M.catalogDefinitionEntityKind definitionValue))
  now <- liftIO getCurrentTime
  let page = max 1 (fromMaybe 1 requestedPage)
      pageSize = min 500 (max 1 (fromMaybe 50 requestedPageSize))
      query = T.strip (fromMaybe "" rawQuery)
  when (T.length query > 200) $
    throwError err400 { errBody = "Catalog search is limited to 200 characters" }
  (items, total) <- runDB (loadCatalogRows spec definitionKey definitionValue locale query page pageSize publicOnly includeInactive)
  scopedDefaults <- runDB $ selectList
    [ M.CatalogScopedDefaultCatalogId ==. definitionKey
    , M.CatalogScopedDefaultActive ==. True
    ]
    [Asc M.CatalogScopedDefaultScopeKind, Asc M.CatalogScopedDefaultScopeId]
  let currentDefaults =
        filter
          (\(Entity _ value) ->
            maybe True (<= now) (M.catalogScopedDefaultEffectiveFrom value)
              && maybe True (> now) (M.catalogScopedDefaultEffectiveUntil value))
          scopedDefaults
  (deploymentDefaults, deploymentRevision) <- loadRegionalDeploymentDefaults definitionValue
  pure CatalogPageDTO
    { cpCatalog = definitionDTO locale definition
    , cpItems = items
    , cpDefaults = map catalogDefaultDTO currentDefaults <> deploymentDefaults
    , cpPage = page
    , cpPageSize = pageSize
    , cpTotal = total
    , cpRevision = M.catalogDefinitionCacheRevision definitionValue + deploymentRevision
    , cpLocale = locale
    }

catalogDefaultDTO :: Entity M.CatalogScopedDefault -> CatalogDefaultDTO
catalogDefaultDTO (Entity _ value) = CatalogDefaultDTO
  { cdfEntityId = UUID.toText (M.catalogScopedDefaultEntityId value)
  , cdfScopeKind = M.catalogScopedDefaultScopeKind value
  , cdfScopeId = M.catalogScopedDefaultScopeId value
  , cdfLocaleId = UUID.toText <$> M.catalogScopedDefaultLocaleId value
  , cdfEffectiveFrom = M.catalogScopedDefaultEffectiveFrom value
  , cdfEffectiveUntil = M.catalogScopedDefaultEffectiveUntil value
  , cdfVersion = M.catalogScopedDefaultVersion value
  }

loadRegionalDeploymentDefaults :: M.CatalogDefinition -> AppM ([CatalogDefaultDTO], Int64)
loadRegionalDeploymentDefaults definition =
  case M.catalogDefinitionEntityKind definition of
    "locale_reference" -> do
      rows <- runDB $ selectList
        [ M.DeploymentLocaleEnablementDeploymentCode ==. "default"
        , M.DeploymentLocaleEnablementEnabled ==. True
        ]
        [Asc M.DeploymentLocaleEnablementLocaleId]
      let revision = sum [fromIntegral (M.deploymentLocaleEnablementVersion value) | Entity _ value <- rows]
          defaults =
            [ CatalogDefaultDTO
                { cdfEntityId = persistKeyText (M.deploymentLocaleEnablementLocaleId value)
                , cdfScopeKind = "deployment"
                , cdfScopeId = M.deploymentLocaleEnablementDeploymentCode value
                , cdfLocaleId = Nothing
                , cdfEffectiveFrom = Nothing
                , cdfEffectiveUntil = Nothing
                , cdfVersion = M.deploymentLocaleEnablementVersion value
                }
            | Entity _ value <- rows
            , M.deploymentLocaleEnablementDefaultLocale value
            ]
      pure (defaults, revision)
    "currency_reference" -> do
      rows <- runDB $ selectList
        [ M.DeploymentCurrencyEnablementDeploymentCode ==. "default"
        , M.DeploymentCurrencyEnablementEnabled ==. True
        ]
        [Asc M.DeploymentCurrencyEnablementCurrencyId]
      let revision = sum [fromIntegral (M.deploymentCurrencyEnablementVersion value) | Entity _ value <- rows]
          defaults =
            [ CatalogDefaultDTO
                { cdfEntityId = persistKeyText (M.deploymentCurrencyEnablementCurrencyId value)
                , cdfScopeKind = "deployment"
                , cdfScopeId = M.deploymentCurrencyEnablementDeploymentCode value
                , cdfLocaleId = Nothing
                , cdfEffectiveFrom = Nothing
                , cdfEffectiveUntil = Nothing
                , cdfVersion = M.deploymentCurrencyEnablementVersion value
                }
            | Entity _ value <- rows
            , M.deploymentCurrencyEnablementDefaultCurrency value
            ]
      pure (defaults, revision)
    _ -> pure ([], 0)

loadCatalogRows
  :: CatalogTableSpec
  -> M.CatalogDefinitionId
  -> M.CatalogDefinition
  -> Text
  -> Text
  -> Int
  -> Int
  -> Bool
  -> Bool
  -> SqlPersistT IO ([CatalogItemDTO], Int64)
loadCatalogRows spec catalogKey definition locale query page pageSize publicOnly includeInactive = do
  jsonRows <- rawSql (catalogItemsSql spec publicOnly includeInactive) (catalogQueryParams spec catalogKey locale query page pageSize)
  countRows <- rawSql (catalogCountSql spec publicOnly includeInactive) (catalogCountParams spec catalogKey query)
  let decodedRows = map decodeRawCatalogItem jsonRows
      rows = catMaybes decodedRows
      total = case countRows of
        [Single value] -> value
        _ -> 0
  pure (map (rawItemDTO locale catalogKey definition) rows, total)

decodeRawCatalogItem :: Single Text -> Maybe RawCatalogItem
decodeRawCatalogItem (Single raw) = either (const Nothing) Just (eitherDecodeStrict' (TE.encodeUtf8 raw))

rawItemDTO :: Text -> M.CatalogDefinitionId -> M.CatalogDefinition -> RawCatalogItem -> CatalogItemDTO
rawItemDTO locale catalogKey definition raw =
  CatalogItemDTO
    { ciId = riId raw
    , ciCatalogId = persistKeyText catalogKey
    , ciCatalogCode = M.catalogDefinitionCode definition
    , ciKind = M.catalogDefinitionEntityKind definition
    , ciCode = riCode raw
    , ciName = chooseLocale locale (riNameEs raw) (riNameEn raw)
    , ciNameEs = riNameEs raw
    , ciNameEn = riNameEn raw
    , ciDescription = chooseLocaleMaybe locale (riDescriptionEs raw) (riDescriptionEn raw)
    , ciDescriptionEs = riDescriptionEs raw
    , ciDescriptionEn = riDescriptionEn raw
    , ciSearchAliases = riSearchAliases raw
    , ciCurrentSlug = riCurrentSlug raw
    , ciParentId = riParentId raw
    , ciSortOrder = riSortOrder raw
    , ciActive = riActive raw
    , ciWorkflowState = riWorkflowState raw
    , ciDeprecatedAt = riDeprecatedAt raw
    , ciReplacementId = riReplacementId raw
    , ciExternalCode = riExternalCode raw
    , ciSourceVersion = riSourceVersion raw
    , ciDisplaySymbol = riDisplaySymbol raw
    , ciUsageCount = riUsageCount raw
    , ciVersion = riVersion raw
    }

catalogItemsSql :: CatalogTableSpec -> Bool -> Bool -> Text
catalogItemsSql spec publicOnly includeInactive =
  "SELECT json_build_object("
    <> "'id', i.id::text, 'code', " <> itemExpr spec (ctsCodeExpr spec)
    <> ", 'nameEs', " <> itemExpr spec (ctsNameEsExpr spec)
    <> ", 'nameEn', " <> itemExpr spec (ctsNameEnExpr spec) <> ", "
    <> "'descriptionEs', " <> itemExpr spec (ctsDescriptionEsExpr spec)
    <> ", 'descriptionEn', " <> itemExpr spec (ctsDescriptionEnExpr spec) <> ", "
    <> "'searchAliases', COALESCE((SELECT json_agg(a.term ORDER BY a.normalized_term) FROM catalog_search_alias a WHERE a.catalog_id = ? AND a.entity_id = i.id), '[]'::json), "
    <> "'currentSlug', " <> itemExpr spec (ctsSlugExpr spec) <> ", "
    <> "'parentId', " <> uuidTextExpr (itemExpr spec (ctsParentExpr spec)) <> ", "
    <> "'sortOrder', i.sort_order, 'active', i.active, "
    <> "'workflowState', " <> workflowExpr spec <> ", "
    <> "'deprecatedAt', " <> itemExpr spec (ctsDeprecatedExpr spec) <> ", "
    <> "'replacementId', " <> uuidTextExpr (itemExpr spec (ctsReplacementExpr spec)) <> ", "
    <> "'externalCode', " <> itemExpr spec (ctsExternalCodeExpr spec) <> ", "
    <> "'sourceVersion', " <> itemExpr spec (ctsSourceVersionExpr spec) <> ", "
    <> "'displaySymbol', " <> itemExpr spec (ctsDisplaySymbolExpr spec) <> ", "
    <> "'usageCount', " <> itemExpr spec (ctsUsageExpr spec) <> ", 'version', i.version)::text "
    <> baseFromSql spec
    <> baseWhereSql spec publicOnly includeInactive
    <> catalogSearchSql spec
    <> " ORDER BY i.sort_order ASC, lower(CASE WHEN ? = 'en' THEN " <> itemExpr spec (ctsNameEnExpr spec) <> " ELSE " <> itemExpr spec (ctsNameEsExpr spec) <> " END) ASC, i.id ASC LIMIT ? OFFSET ?"

catalogCountSql :: CatalogTableSpec -> Bool -> Bool -> Text
catalogCountSql spec publicOnly includeInactive =
  "SELECT COUNT(*) " <> baseFromSql spec <> baseWhereSql spec publicOnly includeInactive
    <> catalogSearchSql spec

catalogSearchSql :: CatalogTableSpec -> Text
catalogSearchSql spec =
  " AND (? = '' OR lower(concat_ws(' ', "
    <> itemExpr spec (ctsCodeExpr spec) <> ", "
    <> itemExpr spec (ctsNameEsExpr spec) <> ", "
    <> itemExpr spec (ctsNameEnExpr spec) <> ", COALESCE("
    <> itemExpr spec (ctsDescriptionEsExpr spec) <> ", ''), COALESCE("
    <> itemExpr spec (ctsDescriptionEnExpr spec) <> ", ''))) LIKE ? ESCAPE '\\\\')"

itemExpr :: CatalogTableSpec -> Text -> Text
itemExpr _ expression
  | "::" `T.isInfixOf` expression = expression
  | otherwise = "i." <> expression

baseFromSql :: CatalogTableSpec -> Text
baseFromSql spec =
  " FROM " <> ctsTable spec <> " i"
    <> if ctsWorkflowScoped spec then " JOIN workflow_state ws ON ws.id = i.workflow_state_id" else ""

baseWhereSql :: CatalogTableSpec -> Bool -> Bool -> Text
baseWhereSql spec publicOnly includeInactive =
  " WHERE TRUE"
    <> if ctsCatalogScoped spec then " AND i.catalog_id = ?" else ""
    <> if includeInactive then "" else " AND i.active = TRUE"
    <> if publicOnly && ctsWorkflowScoped spec then " AND ws.code = 'published'" else ""
    <> deploymentEnablementSql spec publicOnly

deploymentEnablementSql :: CatalogTableSpec -> Bool -> Text
deploymentEnablementSql spec publicOnly
  | not publicOnly = ""
  | ctsTable spec == "locale_reference" =
      " AND EXISTS (SELECT 1 FROM deployment_locale_enablement deployment WHERE deployment.deployment_code='default' AND deployment.locale_id=i.id AND deployment.enabled)"
  | ctsTable spec == "currency_reference" =
      " AND EXISTS (SELECT 1 FROM deployment_currency_enablement deployment WHERE deployment.deployment_code='default' AND deployment.currency_id=i.id AND deployment.enabled)"
  | otherwise = ""

workflowExpr :: CatalogTableSpec -> Text
workflowExpr spec = if ctsWorkflowScoped spec then "ws.code" else "'published'"

uuidTextExpr :: Text -> Text
uuidTextExpr expression
  | "NULL" `T.isPrefixOf` expression = "NULL::text"
  | otherwise = expression <> "::text"

catalogQueryParams :: CatalogTableSpec -> M.CatalogDefinitionId -> Text -> Text -> Int -> Int -> [PersistValue]
catalogQueryParams spec catalogKey locale query page pageSize =
  [toPersistValue catalogKey]
    <> [toPersistValue catalogKey | ctsCatalogScoped spec]
    <> [ PersistText query
       , PersistText (searchPattern query)
       , PersistText locale
       , PersistInt64 (fromIntegral pageSize)
       , PersistInt64 (fromIntegral ((page - 1) * pageSize))
       ]

catalogCountParams :: CatalogTableSpec -> M.CatalogDefinitionId -> Text -> [PersistValue]
catalogCountParams spec catalogKey query =
  [toPersistValue catalogKey | ctsCatalogScoped spec]
    <> [PersistText query, PersistText (searchPattern query)]

searchPattern :: Text -> Text
searchPattern raw = "%" <> T.concatMap escapeLike (T.toLower raw) <> "%"
  where
    escapeLike '%' = "\\%"
    escapeLike '_' = "\\_"
    escapeLike '\\' = "\\\\"
    escapeLike character = T.singleton character

loadCatalogItemById :: Bool -> Text -> Text -> Maybe Text -> AppM CatalogItemDTO
loadCatalogItemById publicOnly catalogCode rawItemId requestedLocale = do
  itemId <- validateUuidText "itemId" rawItemId
  page <- loadCatalogPageByCode publicOnly (not publicOnly) catalogCode requestedLocale Nothing (Just 1) (Just 200)
  case filter ((== itemId) . ciId) (cpItems page) of
    [item] -> pure item
    _ -> do
      Entity definitionKey definitionValue <- findCatalogDefinition publicOnly catalogCode
      spec <- maybe (throwError err501) pure (catalogTableSpec (M.catalogDefinitionEntityKind definitionValue))
      locale <- normalizeLocale requestedLocale
      rows <- runDB $ rawSql (catalogSingleItemSql spec publicOnly) (catalogSingleItemParams spec definitionKey itemId)
      case catMaybes (map decodeRawCatalogItem rows) of
        [raw] -> pure (rawItemDTO locale definitionKey definitionValue raw)
        _ -> throwError err404 { errBody = "Catalog item not found" }

catalogSingleItemSql :: CatalogTableSpec -> Bool -> Text
catalogSingleItemSql spec publicOnly =
  let full = catalogItemsSql spec publicOnly False
      orderClause = " ORDER BY i.sort_order ASC, lower(CASE WHEN ? = 'en' THEN " <> itemExpr spec (ctsNameEnExpr spec) <> " ELSE " <> itemExpr spec (ctsNameEsExpr spec) <> " END) ASC, i.id ASC LIMIT ? OFFSET ?"
  in T.replace orderClause " AND i.id = ?::uuid" (T.replace (catalogSearchSql spec) "" full)

catalogSingleItemParams :: CatalogTableSpec -> M.CatalogDefinitionId -> Text -> [PersistValue]
catalogSingleItemParams spec catalogKey itemId =
  [toPersistValue catalogKey]
    <> [toPersistValue catalogKey | ctsCatalogScoped spec]
    <> [PersistText itemId]

addBatchEtag :: CatalogBatchDTO -> Headers '[Header "ETag" Text] CatalogBatchDTO
addBatchEtag payload = addHeader (etagValue (cbRevision payload)) payload

addPageEtag :: CatalogPageDTO -> Headers '[Header "ETag" Text] CatalogPageDTO
addPageEtag payload = addHeader (etagValue (cpRevision payload)) payload

addBatchEtagIfChanged :: Maybe Text -> CatalogBatchDTO -> AppM (Headers '[Header "ETag" Text] CatalogBatchDTO)
addBatchEtagIfChanged ifNoneMatch payload = do
  rejectNotModified ifNoneMatch (etagValue (cbRevision payload))
  pure (addBatchEtag payload)

addPageEtagIfChanged :: Maybe Text -> CatalogPageDTO -> AppM (Headers '[Header "ETag" Text] CatalogPageDTO)
addPageEtagIfChanged ifNoneMatch payload = do
  rejectNotModified ifNoneMatch (etagValue (cpRevision payload))
  pure (addPageEtag payload)

addWorkflowStatesEtagIfChanged
  :: Maybe Text
  -> WorkflowStatesDTO
  -> AppM (Headers '[Header "ETag" Text] WorkflowStatesDTO)
addWorkflowStatesEtagIfChanged ifNoneMatch payload = do
  let etag = workflowEtagValue (wseWorkflowCode payload) (wseRevision payload)
  rejectNotModified ifNoneMatch etag
  pure (addHeader etag payload)

rejectNotModified :: Maybe Text -> Text -> AppM ()
rejectNotModified supplied current =
  when (maybe False (etagHeaderMatches current) supplied) $
    throwError err304 { errHeaders = [("ETag", TE.encodeUtf8 current)] }

etagHeaderMatches :: Text -> Text -> Bool
etagHeaderMatches current supplied =
  any matches (T.splitOn "," supplied)
  where
    matches candidate =
      let normalized = fromMaybe (T.strip candidate) (T.stripPrefix "W/" (T.strip candidate))
      in normalized == "*" || normalized == current

etagValue :: Int64 -> Text
etagValue revision = "\"catalog-" <> T.pack (show revision) <> "\""

workflowEtagValue :: Text -> Int64 -> Text
workflowEtagValue workflowCode revision =
  "\"workflow-" <> workflowCode <> "-" <> T.pack (show revision) <> "\""

listRevisionsHandler :: AuthedUser -> Text -> Maybe Int -> Maybe Int -> AppM [CatalogRevisionDTO]
listRevisionsHandler user catalogCode requestedPage requestedPageSize = requireCatalogAccess user $ do
  Entity catalogKey catalog <- findCatalogDefinition False catalogCode
  let page = max 1 (fromMaybe 1 requestedPage)
      pageSize = min 200 (max 1 (fromMaybe 50 requestedPageSize))
  rows <- runDB $ selectList [M.CatalogRevisionCatalogId ==. catalogKey] [Desc M.CatalogRevisionCreatedAt, LimitTo pageSize, OffsetBy ((page - 1) * pageSize)]
  forM rows (revisionDTO catalog)

createRevisionHandler :: AuthedUser -> Text -> CatalogDraftRequest -> AppM CatalogRevisionDTO
createRevisionHandler user catalogCode draft = do
  requireCatalogCapability user (if isJust (cdrEntityId draft) then "catalog.update" else "catalog.create")
  validateDraft draft
  Entity catalogKey catalog <- findCatalogDefinition False catalogCode
  spec <- maybe (throwError err422 { errBody = "No typed adapter exists for this catalog" }) pure (catalogTableSpec (M.catalogDefinitionEntityKind catalog))
  when (ctsFamily spec == ReadOnlyFamily) $
    throwError err403 { errBody = "This governed or domain-specific catalog requires its controlled import/API" }
  validateCatalogSpecificDraft spec draft
  validateDraftBaseVersion spec catalogKey draft
  entityUuid <- case cdrEntityId draft of
    Nothing -> liftIO nextRandom
    Just raw -> UUID.fromText <$> validateUuidText "entityId" raw >>= maybe (throwError err400) pure
  draftState <- loadWorkflowStateKey (M.catalogDefinitionWorkflowId catalog) "draft"
  now <- liftIO getCurrentTime
  let baseVersion = fromMaybe 0 (cdrBaseVersion draft)
      revision = M.CatalogRevision
        { M.catalogRevisionCatalogId = catalogKey
        , M.catalogRevisionEntityId = entityUuid
        , M.catalogRevisionWorkflowStateId = draftState
        , M.catalogRevisionBaseVersion = baseVersion
        , M.catalogRevisionProposedVersion = baseVersion + 1
        , M.catalogRevisionPreviousValues = Nothing
        , M.catalogRevisionNewValues = AesonValue (toJSON draft)
        , M.catalogRevisionCreatedBy = auPartyId user
        , M.catalogRevisionCreatedAt = now
        , M.catalogRevisionSubmittedAt = Nothing
        , M.catalogRevisionReviewedBy = Nothing
        , M.catalogRevisionReviewedAt = Nothing
        , M.catalogRevisionApprovedBy = Nothing
        , M.catalogRevisionApprovedAt = Nothing
        , M.catalogRevisionReviewerNotes = Nothing
        , M.catalogRevisionRejectionReason = Nothing
        , M.catalogRevisionScheduledPublishAt = Nothing
        , M.catalogRevisionPublishedAt = Nothing
        , M.catalogRevisionSourcePlatform = cdrSourcePlatform draft
        , M.catalogRevisionCorrelationId = cdrCorrelationId draft
        , M.catalogRevisionReason = Just (cdrReason draft)
        , M.catalogRevisionResult = Just "draft-created"
        , M.catalogRevisionImportJobId = Nothing
        }
  revisionKey <- runDB $ insert revision
  writeAudit catalogKey entityUuid (Just revisionKey) "draft-created" (Just user) Nothing Nothing (cdrSourcePlatform draft) (cdrCorrelationId draft) (Just (cdrReason draft)) "success" Nothing (Just (toJSON draft))
  revisionDTO catalog (Entity revisionKey revision)

submitRevisionHandler :: AuthedUser -> Text -> AppM CatalogRevisionDTO
submitRevisionHandler user rawRevisionId = do
  requireCatalogCapability user "catalog.update"
  (revisionKey, revision, catalog) <- loadRevision rawRevisionId
  stateCode <- workflowStateCode (M.catalogRevisionWorkflowStateId revision)
  unless (stateCode `elem` ["draft", "rejected"]) $
    throwError err409 { errBody = "Only draft or rejected revisions can be submitted" }
  reviewState <- loadWorkflowStateKey (M.catalogDefinitionWorkflowId catalog) "review"
  now <- liftIO getCurrentTime
  runDB $ update revisionKey
    [ M.CatalogRevisionWorkflowStateId =. reviewState
    , M.CatalogRevisionSubmittedAt =. Just now
    , M.CatalogRevisionReviewedBy =. Nothing
    , M.CatalogRevisionReviewedAt =. Nothing
    , M.CatalogRevisionRejectionReason =. Nothing
    , M.CatalogRevisionResult =. Just "submitted"
    ]
  updated <- runDB (getJust revisionKey)
  revisionDTO catalog (Entity revisionKey updated)

approveRevisionHandler :: AuthedUser -> Text -> CatalogReviewRequest -> AppM CatalogRevisionDTO
approveRevisionHandler user rawRevisionId review = do
  requireCatalogApprovalAccess user
  (revisionKey, revision, catalog) <- loadRevision rawRevisionId
  stateCode <- workflowStateCode (M.catalogRevisionWorkflowStateId revision)
  unless (stateCode == "review") $
    throwError err409 { errBody = "Only revisions in review can be approved" }
  when (M.catalogDefinitionSensitive catalog && M.catalogRevisionCreatedBy revision == auPartyId user && not (crrEmergencyOverride review)) $
    throwError err403 { errBody = "The author of a sensitive change cannot approve it" }
  when (crrEmergencyOverride review && not (M.catalogDefinitionSensitive catalog)) $
    throwError err400 { errBody = "Emergency override is valid only for sensitive catalogs" }
  now <- liftIO getCurrentTime
  let publishNow = maybe True (<= now) (crrScheduledPublishAt review)
      nextStateCode = if publishNow then "published" else "approved"
  nextState <- loadWorkflowStateKey (M.catalogDefinitionWorkflowId catalog) nextStateCode
  when publishNow (publishRevision user revisionKey revision catalog now)
  runDB $ update revisionKey
    [ M.CatalogRevisionWorkflowStateId =. nextState
    , M.CatalogRevisionReviewedBy =. Just (auPartyId user)
    , M.CatalogRevisionReviewedAt =. Just now
    , M.CatalogRevisionApprovedBy =. Just (auPartyId user)
    , M.CatalogRevisionApprovedAt =. Just now
    , M.CatalogRevisionReviewerNotes =. Just (crrNotes review)
    , M.CatalogRevisionScheduledPublishAt =. crrScheduledPublishAt review
    , M.CatalogRevisionPublishedAt =. if publishNow then Just now else Nothing
    , M.CatalogRevisionResult =. Just nextStateCode
    ]
  updated <- runDB (getJust revisionKey)
  revisionDTO catalog (Entity revisionKey updated)

rejectRevisionHandler :: AuthedUser -> Text -> CatalogReviewRequest -> AppM CatalogRevisionDTO
rejectRevisionHandler user rawRevisionId review = do
  requireCatalogCapability user "catalog.review"
  when (T.null (T.strip (crrNotes review))) $
    throwError err400 { errBody = "A rejection reason is required" }
  (revisionKey, revision, catalog) <- loadRevision rawRevisionId
  stateCode <- workflowStateCode (M.catalogRevisionWorkflowStateId revision)
  unless (stateCode == "review") $
    throwError err409 { errBody = "Only revisions in review can be rejected" }
  rejectedState <- loadWorkflowStateKey (M.catalogDefinitionWorkflowId catalog) "rejected"
  now <- liftIO getCurrentTime
  runDB $ update revisionKey
    [ M.CatalogRevisionWorkflowStateId =. rejectedState
    , M.CatalogRevisionReviewedBy =. Just (auPartyId user)
    , M.CatalogRevisionReviewedAt =. Just now
    , M.CatalogRevisionReviewerNotes =. Just (crrNotes review)
    , M.CatalogRevisionRejectionReason =. Just (crrNotes review)
    , M.CatalogRevisionResult =. Just "rejected"
    ]
  updated <- runDB (getJust revisionKey)
  revisionDTO catalog (Entity revisionKey updated)

publishRevision :: AuthedUser -> M.CatalogRevisionId -> M.CatalogRevision -> M.CatalogDefinition -> UTCTime -> AppM ()
publishRevision user revisionKey revision catalog now = do
  draft <- decodeDraft (M.catalogRevisionNewValues revision)
  spec <- maybe (throwError err422) pure (catalogTableSpec (M.catalogDefinitionEntityKind catalog))
  when (ctsFamily spec == ReadOnlyFamily) $
    throwError err403 { errBody = "This catalog cannot be published through the generic adapter" }
  publishedState <- loadWorkflowStateKey (M.catalogDefinitionWorkflowId catalog) "published"
  parentUuid <- traverse (parseUuid "parentId") (cdrParentId draft)
  let catalogIdText = persistKeyText (M.catalogRevisionCatalogId revision)
      entityIdText = UUID.toText (M.catalogRevisionEntityId revision)
      workflowIdText = persistKeyText publishedState
      commonParams =
        [ PersistText entityIdText
        , PersistText catalogIdText
        , PersistText (cdrCode draft)
        , PersistText (cdrNameEs draft)
        , PersistText (cdrNameEn draft)
        , maybe PersistNull PersistText (cdrDescriptionEs draft)
        , maybe PersistNull PersistText (cdrDescriptionEn draft)
        , PersistInt64 (fromIntegral (cdrSortOrder draft))
        , PersistText workflowIdText
        ]
      lifecycleParams = case ctsFamily spec of
        FlatReferenceFamily -> [PersistUTCTime now, PersistUTCTime now, PersistInt64 (fromIntegral (M.catalogRevisionProposedVersion revision))]
        HierarchyFamily -> [toPersistValue (auPartyId user), PersistUTCTime now, PersistUTCTime now, PersistInt64 (fromIntegral (M.catalogRevisionProposedVersion revision))]
        FlatCatalogFamily -> [PersistUTCTime now, PersistUTCTime now, PersistInt64 (fromIntegral (M.catalogRevisionProposedVersion revision))]
        ServiceOfferingFamily ->
          [ toPersistValue (auPartyId user)
          , toPersistValue (auPartyId user)
          , toPersistValue (auPartyId user)
          , PersistUTCTime now
          , PersistUTCTime now
          , PersistInt64 (fromIntegral (M.catalogRevisionProposedVersion revision))
          ]
        RadioAutoStopFamily ->
          [ toPersistValue (auPartyId user)
          , toPersistValue (auPartyId user)
          , toPersistValue (auPartyId user)
          , PersistUTCTime now
          , PersistUTCTime now
          , PersistInt64 (fromIntegral (M.catalogRevisionProposedVersion revision))
          ]
        AppearanceModeFamily ->
          [ toPersistValue (auPartyId user)
          , toPersistValue (auPartyId user)
          , toPersistValue (auPartyId user)
          , PersistUTCTime now
          , PersistUTCTime now
          , PersistInt64 (fromIntegral (M.catalogRevisionProposedVersion revision))
          ]
        ReadOnlyFamily -> []
  when (ctsFamily spec == RadioAutoStopFamily) $
    validateRadioAutoStopDefaultChange
      (M.catalogRevisionCatalogId revision)
      (M.catalogRevisionEntityId revision)
      draft
  when (ctsFamily spec == AppearanceModeFamily) $
    validateAppearanceModeDefaultChange
      (M.catalogRevisionCatalogId revision)
      (M.catalogRevisionEntityId revision)
      draft
  case ctsDefaultScopeKind spec of
    Nothing -> pure ()
    Just scopeKind -> validateGlobalDefaultChange scopeKind (M.catalogRevisionCatalogId revision) (M.catalogRevisionEntityId revision) draft
  affected <- runDB
    ( rawSql
        (publishSql spec)
        ( commonParams
            <> lifecycleParams
            <> publishFamilyParams spec draft parentUuid
            <> [PersistInt64 (fromIntegral (M.catalogRevisionBaseVersion revision))]
        )
        :: SqlPersistT IO [Single Int]
    )
  when (null affected) $
    throwError err409 { errBody = "Catalog item version changed after this revision was created; reload and create a new revision" }
  when (ctsFamily spec == ServiceOfferingFamily) $
    publishServiceOfferingResources (M.catalogRevisionEntityId revision) draft
  when (ctsFamily spec == RadioAutoStopFamily) $
    publishRadioAutoStopDefault
      user
      revisionKey
      (M.catalogRevisionCatalogId revision)
      (M.catalogRevisionEntityId revision)
      draft
      now
  when (ctsFamily spec == AppearanceModeFamily) $
    publishAppearanceModeDefault
      user
      revisionKey
      (M.catalogRevisionCatalogId revision)
      (M.catalogRevisionEntityId revision)
      draft
      now
  case (ctsDefaultScopeKind spec, cdrGlobalDefault draft) of
    (Just scopeKind, Just True) ->
      publishScopedDefault user revisionKey (M.catalogRevisionCatalogId revision) (M.catalogRevisionEntityId revision) scopeKind draft now
    _ -> pure ()
  replaceSearchAliases (M.catalogRevisionCatalogId revision) (M.catalogRevisionEntityId revision) draft now
  runDB $ update (M.catalogRevisionCatalogId revision)
    [ M.CatalogDefinitionCacheRevision +=. 1
    , M.CatalogDefinitionUpdatedAt =. now
    , M.CatalogDefinitionVersion +=. 1
    ]
  writeAudit (M.catalogRevisionCatalogId revision) (M.catalogRevisionEntityId revision) (Just revisionKey) "published" (Just user) (Just user) (Just user) (cdrSourcePlatform draft) (cdrCorrelationId draft) (Just (cdrReason draft)) "success" Nothing (Just (toJSON draft))

publishSql :: CatalogTableSpec -> Text
publishSql spec =
  case ctsFamily spec of
    FlatReferenceFamily ->
      "INSERT INTO " <> ctsTable spec <> " (id, catalog_id, code, name_es, name_en, description_es, description_en, sort_order, workflow_state_id, created_at, updated_at, version, active, external_code, source_version) "
        <> "VALUES (?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?::uuid, ?, ?, ?, TRUE, ?, ?) "
        <> "ON CONFLICT (id) DO UPDATE SET code=EXCLUDED.code, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, sort_order=EXCLUDED.sort_order, workflow_state_id=EXCLUDED.workflow_state_id, updated_at=EXCLUDED.updated_at, version=EXCLUDED.version, external_code=EXCLUDED.external_code, source_version=EXCLUDED.source_version WHERE " <> ctsTable spec <> ".version=? RETURNING 1"
    HierarchyFamily ->
      "INSERT INTO " <> ctsTable spec <> " (id, catalog_id, code, name_es, name_en, description_es, description_en, sort_order, workflow_state_id, created_by, created_at, updated_at, version, active, parent_id, current_slug) "
        <> "VALUES (?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?::uuid, ?, ?, ?, ?, TRUE, ?::uuid, ?) "
        <> "ON CONFLICT (id) DO UPDATE SET code=EXCLUDED.code, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, sort_order=EXCLUDED.sort_order, workflow_state_id=EXCLUDED.workflow_state_id, updated_at=EXCLUDED.updated_at, version=EXCLUDED.version, parent_id=EXCLUDED.parent_id, current_slug=EXCLUDED.current_slug WHERE " <> ctsTable spec <> ".version=? RETURNING 1"
    FlatCatalogFamily
      | ctsTable spec `elem` ["reaction_type", "content_reaction_type"] ->
          "INSERT INTO " <> ctsTable spec <> " (id, catalog_id, code, name_es, name_en, description_es, description_en, sort_order, workflow_state_id, created_at, updated_at, version, active, current_slug, emoji) "
            <> "VALUES (?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?::uuid, ?, ?, ?, TRUE, ?, ?) "
            <> "ON CONFLICT (id) DO UPDATE SET code=EXCLUDED.code, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, sort_order=EXCLUDED.sort_order, workflow_state_id=EXCLUDED.workflow_state_id, updated_at=EXCLUDED.updated_at, version=EXCLUDED.version, current_slug=EXCLUDED.current_slug, emoji=EXCLUDED.emoji WHERE " <> ctsTable spec <> ".version=? RETURNING 1"
      | otherwise ->
          "INSERT INTO " <> ctsTable spec <> " (id, catalog_id, code, name_es, name_en, description_es, description_en, sort_order, workflow_state_id, created_at, updated_at, version, active, current_slug) "
            <> "VALUES (?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?::uuid, ?, ?, ?, TRUE, ?) "
            <> "ON CONFLICT (id) DO UPDATE SET code=EXCLUDED.code, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, sort_order=EXCLUDED.sort_order, workflow_state_id=EXCLUDED.workflow_state_id, updated_at=EXCLUDED.updated_at, version=EXCLUDED.version, current_slug=EXCLUDED.current_slug WHERE " <> ctsTable spec <> ".version=? RETURNING 1"
    ServiceOfferingFamily ->
      "INSERT INTO service_offering (id, catalog_id, code, name_es, name_en, description_es, description_en, sort_order, workflow_state_id, created_by, updated_by, approved_by, created_at, updated_at, version, category_id, current_slug, pricing_model_id, pricing_model_code, default_rate_cents, tax_rate_id, tax_rate_code, currency_id, billing_unit_es, billing_unit_en, default_duration_minutes, requires_engineer, active) "
        <> "VALUES (?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?::uuid, ?, ?, ?, ?, ?, ?, ?::uuid, ?, ?::uuid, NULL, ?, ?::uuid, NULL, ?::uuid, ?, ?, ?, ?, TRUE) "
        <> "ON CONFLICT (id) DO UPDATE SET code=EXCLUDED.code, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, sort_order=EXCLUDED.sort_order, workflow_state_id=EXCLUDED.workflow_state_id, updated_by=EXCLUDED.updated_by, approved_by=EXCLUDED.approved_by, updated_at=EXCLUDED.updated_at, version=EXCLUDED.version, category_id=EXCLUDED.category_id, current_slug=EXCLUDED.current_slug, pricing_model_id=EXCLUDED.pricing_model_id, pricing_model_code=NULL, default_rate_cents=EXCLUDED.default_rate_cents, tax_rate_id=EXCLUDED.tax_rate_id, tax_rate_code=NULL, currency_id=EXCLUDED.currency_id, billing_unit_es=EXCLUDED.billing_unit_es, billing_unit_en=EXCLUDED.billing_unit_en, default_duration_minutes=EXCLUDED.default_duration_minutes, requires_engineer=EXCLUDED.requires_engineer WHERE service_offering.version=? RETURNING 1"
    RadioAutoStopFamily ->
      "INSERT INTO radio_auto_stop_option (id, catalog_id, code, name_es, name_en, description_es, description_en, sort_order, workflow_state_id, created_by, updated_by, approved_by, created_at, updated_at, version, current_slug, duration_minutes, active) "
        <> "VALUES (?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?::uuid, ?, ?, ?, ?, ?, ?, ?, ?, TRUE) "
        <> "ON CONFLICT (id) DO UPDATE SET code=EXCLUDED.code, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, sort_order=EXCLUDED.sort_order, workflow_state_id=EXCLUDED.workflow_state_id, updated_by=EXCLUDED.updated_by, approved_by=EXCLUDED.approved_by, updated_at=EXCLUDED.updated_at, version=EXCLUDED.version, current_slug=EXCLUDED.current_slug, duration_minutes=EXCLUDED.duration_minutes WHERE radio_auto_stop_option.version=? RETURNING 1"
    AppearanceModeFamily ->
      "INSERT INTO appearance_mode_option (id, catalog_id, code, name_es, name_en, description_es, description_en, sort_order, workflow_state_id, created_by, updated_by, approved_by, created_at, updated_at, version, current_slug, active) "
        <> "VALUES (?::uuid, ?::uuid, ?, ?, ?, ?, ?, ?, ?::uuid, ?, ?, ?, ?, ?, ?, ?, TRUE) "
        <> "ON CONFLICT (id) DO UPDATE SET code=EXCLUDED.code, name_es=EXCLUDED.name_es, name_en=EXCLUDED.name_en, description_es=EXCLUDED.description_es, description_en=EXCLUDED.description_en, sort_order=EXCLUDED.sort_order, workflow_state_id=EXCLUDED.workflow_state_id, updated_by=EXCLUDED.updated_by, approved_by=EXCLUDED.approved_by, updated_at=EXCLUDED.updated_at, version=EXCLUDED.version, current_slug=EXCLUDED.current_slug WHERE appearance_mode_option.version=? RETURNING 1"
    ReadOnlyFamily -> "SELECT 1"

publishFamilyParams :: CatalogTableSpec -> CatalogDraftRequest -> Maybe UUID.UUID -> [PersistValue]
publishFamilyParams spec draft parentUuid =
  case ctsFamily spec of
    FlatReferenceFamily -> [maybe PersistNull PersistText (cdrExternalCode draft), maybe PersistNull PersistText (cdrSourceVersion draft)]
    HierarchyFamily -> [maybe PersistNull (PersistText . UUID.toText) parentUuid, maybe PersistNull PersistText (cdrCurrentSlug draft)]
    FlatCatalogFamily ->
      [maybe PersistNull PersistText (cdrCurrentSlug draft)]
        <> [maybe PersistNull PersistText (cdrDisplaySymbol draft) | ctsTable spec `elem` ["reaction_type", "content_reaction_type"]]
    ServiceOfferingFamily ->
      case cdrServiceOffering draft of
        Nothing -> []
        Just serviceDraft ->
          [ PersistText (sodCategoryId serviceDraft)
          , maybe PersistNull PersistText (cdrCurrentSlug draft)
          , PersistText (sodPricingModelId serviceDraft)
          , maybe PersistNull (PersistInt64 . fromIntegral) (sodRateCents serviceDraft)
          , maybe PersistNull PersistText (sodTaxRateId serviceDraft)
          , PersistText (sodCurrencyId serviceDraft)
          , maybe PersistNull PersistText (sodBillingUnitEs serviceDraft)
          , maybe PersistNull PersistText (sodBillingUnitEn serviceDraft)
          , maybe PersistNull (PersistInt64 . fromIntegral) (sodDefaultDurationMinutes serviceDraft)
          , PersistBool (sodRequiresEngineer serviceDraft)
          ]
    RadioAutoStopFamily ->
      case cdrRadioAutoStop draft of
        Nothing -> []
        Just radioDraft ->
          [ maybe PersistNull PersistText (cdrCurrentSlug draft)
          , PersistInt64 (fromIntegral (rasdDurationMinutes radioDraft))
          ]
    AppearanceModeFamily -> [maybe PersistNull PersistText (cdrCurrentSlug draft)]
    ReadOnlyFamily -> []

validateRadioAutoStopDefaultChange
  :: M.CatalogDefinitionId
  -> UUID.UUID
  -> CatalogDraftRequest
  -> AppM ()
validateRadioAutoStopDefaultChange catalogKey entityUuid draft = do
  radioDraft <- maybe
    (throwError err500 { errBody = "Stored radio auto-stop revision is missing its typed payload" })
    pure
    (cdrRadioAutoStop draft)
  currentIsEntity <- isCurrentRadioAutoStopDefault catalogKey entityUuid
  when
    (not (rasdDefaultForBroadcast radioDraft) && currentIsEntity) $
    throwError err409
      { errBody = "The active broadcast default cannot be removed without publishing another default" }

isCurrentRadioAutoStopDefault :: M.CatalogDefinitionId -> UUID.UUID -> AppM Bool
isCurrentRadioAutoStopDefault = isCurrentScopedDefault "radio-broadcast"

validateAppearanceModeDefaultChange
  :: M.CatalogDefinitionId
  -> UUID.UUID
  -> CatalogDraftRequest
  -> AppM ()
validateAppearanceModeDefaultChange catalogKey entityUuid draft = do
  appearanceDraft <- maybe
    (throwError err500 { errBody = "Stored appearance mode revision is missing its typed payload" })
    pure
    (cdrAppearanceMode draft)
  currentIsEntity <- isCurrentScopedDefault "appearance-mode" catalogKey entityUuid
  when
    (not (amdDefaultForApplication appearanceDraft) && currentIsEntity) $
    throwError err409
      { errBody = "The active application appearance default cannot be removed without publishing another default" }

validateGlobalDefaultChange
  :: Text
  -> M.CatalogDefinitionId
  -> UUID.UUID
  -> CatalogDraftRequest
  -> AppM ()
validateGlobalDefaultChange scopeKind catalogKey entityUuid draft = do
  currentIsEntity <- isCurrentScopedDefault scopeKind catalogKey entityUuid
  when (cdrGlobalDefault draft == Just False && currentIsEntity) $
    throwError err409
      { errBody = "The active global default cannot be removed without publishing another default" }

isCurrentScopedDefault :: Text -> M.CatalogDefinitionId -> UUID.UUID -> AppM Bool
isCurrentScopedDefault scopeKind catalogKey entityUuid = do
  current <- runDB $ selectFirst
    [ M.CatalogScopedDefaultCatalogId ==. catalogKey
    , M.CatalogScopedDefaultScopeKind ==. scopeKind
    , M.CatalogScopedDefaultScopeId ==. "global"
    , M.CatalogScopedDefaultLocaleId ==. Nothing
    , M.CatalogScopedDefaultActive ==. True
    ]
    []
  pure (maybe False ((== entityUuid) . M.catalogScopedDefaultEntityId . entityVal) current)

publishRadioAutoStopDefault
  :: AuthedUser
  -> M.CatalogRevisionId
  -> M.CatalogDefinitionId
  -> UUID.UUID
  -> CatalogDraftRequest
  -> UTCTime
  -> AppM ()
publishRadioAutoStopDefault user revisionKey catalogKey entityUuid draft now =
  case cdrRadioAutoStop draft of
    Just radioDraft | rasdDefaultForBroadcast radioDraft ->
      publishScopedDefault user revisionKey catalogKey entityUuid "radio-broadcast" draft now
    _ -> pure ()

publishAppearanceModeDefault
  :: AuthedUser
  -> M.CatalogRevisionId
  -> M.CatalogDefinitionId
  -> UUID.UUID
  -> CatalogDraftRequest
  -> UTCTime
  -> AppM ()
publishAppearanceModeDefault user revisionKey catalogKey entityUuid draft now =
  case cdrAppearanceMode draft of
    Just appearanceDraft | amdDefaultForApplication appearanceDraft ->
      publishScopedDefault user revisionKey catalogKey entityUuid "appearance-mode" draft now
    _ -> pure ()

publishScopedDefault
  :: AuthedUser
  -> M.CatalogRevisionId
  -> M.CatalogDefinitionId
  -> UUID.UUID
  -> Text
  -> CatalogDraftRequest
  -> UTCTime
  -> AppM ()
publishScopedDefault user revisionKey catalogKey entityUuid scopeKind draft now = runDB $ do
  previous <- selectFirst
    [ M.CatalogScopedDefaultCatalogId ==. catalogKey
    , M.CatalogScopedDefaultScopeKind ==. scopeKind
    , M.CatalogScopedDefaultScopeId ==. "global"
    , M.CatalogScopedDefaultLocaleId ==. Nothing
    , M.CatalogScopedDefaultActive ==. True
    ]
    []
  updateWhere
    [ M.CatalogScopedDefaultCatalogId ==. catalogKey
    , M.CatalogScopedDefaultScopeKind ==. scopeKind
    , M.CatalogScopedDefaultScopeId ==. "global"
    , M.CatalogScopedDefaultLocaleId ==. Nothing
    , M.CatalogScopedDefaultActive ==. True
    ]
    [ M.CatalogScopedDefaultActive =. False
    , M.CatalogScopedDefaultVersion +=. 1
    ]
  insert_ M.CatalogScopedDefault
    { M.catalogScopedDefaultCatalogId = catalogKey
    , M.catalogScopedDefaultEntityId = entityUuid
    , M.catalogScopedDefaultScopeKind = scopeKind
    , M.catalogScopedDefaultScopeId = "global"
    , M.catalogScopedDefaultLocaleId = Nothing
    , M.catalogScopedDefaultEffectiveFrom = Just now
    , M.catalogScopedDefaultEffectiveUntil = Nothing
    , M.catalogScopedDefaultActive = True
    , M.catalogScopedDefaultCreatedBy = Just (auPartyId user)
    , M.catalogScopedDefaultCreatedAt = now
    , M.catalogScopedDefaultVersion = 1
    }
  let previousEntityId = UUID.toText . M.catalogScopedDefaultEntityId . entityVal <$> previous
      operation = if previousEntityId == Just (UUID.toText entityUuid)
        then "default-confirmed"
        else "default-reassigned"
      affected = Aeson.object
        [ "scopeKind" .= scopeKind
        , "scopeId" .= ("global" :: Text)
        , "previousEntityId" .= previousEntityId
        , "newEntityId" .= UUID.toText entityUuid
        ]
  insert_ M.CatalogAuditEvent
    { M.catalogAuditEventCatalogId = catalogKey
    , M.catalogAuditEventEntityId = entityUuid
    , M.catalogAuditEventRevisionId = Just revisionKey
    , M.catalogAuditEventOperation = operation
    , M.catalogAuditEventPreviousValues = Nothing
    , M.catalogAuditEventNewValues = Nothing
    , M.catalogAuditEventActorId = Just (auPartyId user)
    , M.catalogAuditEventReviewerId = Just (auPartyId user)
    , M.catalogAuditEventApproverId = Just (auPartyId user)
    , M.catalogAuditEventOccurredAt = now
    , M.catalogAuditEventSourcePlatform = cdrSourcePlatform draft
    , M.catalogAuditEventImportJobId = Nothing
    , M.catalogAuditEventReason = Just (cdrReason draft)
    , M.catalogAuditEventCorrelationId = cdrCorrelationId draft
    , M.catalogAuditEventResult = "success"
    , M.catalogAuditEventAffectedRelationships = Just (AesonValue affected)
    }

publishServiceOfferingResources :: UUID.UUID -> CatalogDraftRequest -> AppM ()
publishServiceOfferingResources offeringUuid draft = do
  serviceDraft <- maybe
    (throwError err500 { errBody = "Stored service offering revision is missing its typed payload" })
    pure
    (cdrServiceOffering draft)
  requireActiveReference "service category" "service_category" (sodCategoryId serviceDraft)
  requireActiveReference "service pricing model" "service_pricing_model" (sodPricingModelId serviceDraft)
  requireActiveReference "currency" "currency_reference" (sodCurrencyId serviceDraft)
  forM_ (sodTaxRateId serviceDraft) (requireActiveReference "tax rate" "tax_rate_reference")
  resources <- forM (sodDefaultResources serviceDraft) $ \resource -> do
    resourceId <- validatePositiveInt64Text "serviceOffering.defaultResources.resourceId" (sordResourceId resource)
    selectionModeId <- validateUuidText
      "serviceOffering.defaultResources.selectionModeId"
      (sordSelectionModeId resource)
    requireActiveReference
      "service resource selection mode"
      "service_resource_selection_mode"
      selectionModeId
    rows <- runDB
      ( rawSql
          "SELECT COUNT(*) FROM resource WHERE id=? AND active=TRUE"
          [PersistInt64 resourceId]
          :: SqlPersistT IO [Single Int64]
      )
    case rows of
      [Single 1] -> pure (resourceId, selectionModeId, resource)
      _ -> throwError err409 { errBody = "A default service resource is missing or inactive" }
  runDB $ rawExecute
    "UPDATE service_offering_default_resource SET active=FALSE, version=version+1 WHERE service_offering_id=?::uuid AND active=TRUE"
    [PersistText (UUID.toText offeringUuid)]
  forM_ resources $ \(resourceId, selectionModeId, resource) -> runDB $ rawExecute
    "INSERT INTO service_offering_default_resource (id, service_offering_id, resource_id, selection_mode_id, selection_mode, sort_order, active, version) VALUES (gen_random_uuid(), ?::uuid, ?, ?::uuid, NULL, ?, TRUE, 1) ON CONFLICT (service_offering_id, resource_id) DO UPDATE SET selection_mode_id=EXCLUDED.selection_mode_id, selection_mode=NULL, sort_order=EXCLUDED.sort_order, active=TRUE, version=service_offering_default_resource.version+1"
    [ PersistText (UUID.toText offeringUuid)
    , PersistInt64 resourceId
    , PersistText selectionModeId
    , PersistInt64 (fromIntegral (sordSortOrder resource))
    ]

requireActiveReference :: Text -> Text -> Text -> AppM ()
requireActiveReference label tableName rawId = do
  referenceId <- validateUuidText label rawId
  rows <- runDB
    ( rawSql
        ("SELECT COUNT(*) FROM " <> tableName <> " WHERE id=?::uuid AND active=TRUE")
        [PersistText referenceId]
        :: SqlPersistT IO [Single Int64]
    )
  case rows of
    [Single 1] -> pure ()
    _ -> throwError err409 { errBody = BL.fromStrict (TE.encodeUtf8 ("The selected " <> label <> " is missing or inactive")) }

replaceSearchAliases :: M.CatalogDefinitionId -> UUID.UUID -> CatalogDraftRequest -> UTCTime -> AppM ()
replaceSearchAliases catalogKey entityUuid draft now = do
  localeRows <- runDB $ selectList [M.LocaleReferenceCode <-. ["es", "en"], M.LocaleReferenceActive ==. True] []
  let localeByCode wanted = entityKey <$> safeHead (filter ((== wanted) . M.localeReferenceCode . entityVal) localeRows)
      aliases = [("es", cdrSearchAliasesEs draft), ("en", cdrSearchAliasesEn draft)]
  runDB $ deleteWhere [M.CatalogSearchAliasCatalogId ==. catalogKey, M.CatalogSearchAliasEntityId ==. entityUuid]
  forM_ aliases $ \(localeCode, terms) ->
    forM_ (nub (filter (not . T.null) (map T.strip terms))) $ \term ->
      case localeByCode localeCode of
        Nothing -> pure ()
        Just localeKey -> runDB $ insert_ M.CatalogSearchAlias
          { M.catalogSearchAliasCatalogId = catalogKey
          , M.catalogSearchAliasEntityKind = "catalog-item"
          , M.catalogSearchAliasEntityId = entityUuid
          , M.catalogSearchAliasLocaleId = keyUuid localeKey
          , M.catalogSearchAliasTerm = term
          , M.catalogSearchAliasNormalizedTerm = T.toCaseFold term
          , M.catalogSearchAliasSource = "manual"
          , M.catalogSearchAliasCreatedAt = now
          }

activationHandler :: AuthedUser -> Text -> Text -> CatalogActivationRequest -> AppM CatalogItemDTO
activationHandler user catalogCode rawItemId request = do
  requireCatalogCapability user (if caActive request then "catalog.restore" else "catalog.deactivate")
  itemId <- validateUuidText "itemId" rawItemId
  Entity catalogKey catalog <- findCatalogDefinition False catalogCode
  spec <- maybe (throwError err422) pure (catalogTableSpec (M.catalogDefinitionEntityKind catalog))
  when (ctsFamily spec == ReadOnlyFamily) $
    throwError err403 { errBody = "Controlled reference data cannot be activated manually" }
  replacement <- traverse (validateUuidText "replacementId") (caReplacementId request)
  when (ctsFamily spec == RadioAutoStopFamily && not (caActive request)) $ do
    currentDefault <- isCurrentRadioAutoStopDefault catalogKey (fromMaybe UUID.nil (UUID.fromText itemId))
    when currentDefault $
      throwError err409 { errBody = "Publish another Radio auto-stop default before deactivating this option" }
  when (ctsFamily spec == AppearanceModeFamily && not (caActive request)) $ do
    currentDefault <- isCurrentScopedDefault "appearance-mode" catalogKey (fromMaybe UUID.nil (UUID.fromText itemId))
    when currentDefault $
      throwError err409 { errBody = "Publish another appearance default before deactivating this option" }
  case ctsDefaultScopeKind spec of
    Just scopeKind | not (caActive request) -> do
      currentDefault <- isCurrentScopedDefault scopeKind catalogKey (fromMaybe UUID.nil (UUID.fromText itemId))
      when currentDefault $
        throwError err409 { errBody = "Publish another global default before deactivating this option" }
    _ -> pure ()
  when (not (caActive request) && replacement == Just itemId) $
    throwError err400 { errBody = "Replacement must be a different active item" }
  affected <- runDB
    ( rawSql
        ("UPDATE " <> ctsTable spec <> " SET active=?, replacement_id=?::uuid, deprecated_at=CASE WHEN ? THEN NULL ELSE now() END, version=version+1, updated_at=now() WHERE id=?::uuid AND catalog_id=?::uuid AND version=? RETURNING 1")
        [ PersistBool (caActive request)
        , maybe PersistNull PersistText replacement
        , PersistBool (caActive request)
        , PersistText itemId
        , PersistText (persistKeyText catalogKey)
        , PersistInt64 (fromIntegral (caExpectedVersion request))
        ] :: SqlPersistT IO [Single Int]
    )
  when (null affected) $
    throwError err409 { errBody = "Item version changed or item does not belong to this catalog" }
  now <- liftIO getCurrentTime
  runDB $ update catalogKey [M.CatalogDefinitionCacheRevision +=. 1, M.CatalogDefinitionUpdatedAt =. now]
  writeAudit catalogKey (fromMaybe UUID.nil (UUID.fromText itemId)) Nothing (if caActive request then "activated" else "deactivated") (Just user) Nothing Nothing "admin" (caCorrelationId request) (Just (caReason request)) "success" Nothing Nothing
  loadCatalogItemById False catalogCode itemId (Just "es")

reorderHandler :: AuthedUser -> Text -> CatalogReorderRequest -> AppM NoContent
reorderHandler user catalogCode request = do
  requireCatalogCapability user "catalog.update"
  let rawIds = croOrderedItemIds request
  when (null rawIds || length rawIds /= length (nub rawIds)) $
    throwError err400 { errBody = "orderedItemIds must be non-empty and unique" }
  ids <- mapM (validateUuidText "orderedItemIds") rawIds
  Entity catalogKey catalog <- findCatalogDefinition False catalogCode
  when (M.catalogDefinitionCacheRevision catalog /= croExpectedCatalogRevision request) $
    throwError err409 { errBody = "Catalog revision changed; reload before reordering" }
  spec <- maybe (throwError err422) pure (catalogTableSpec (M.catalogDefinitionEntityKind catalog))
  when (ctsFamily spec == ReadOnlyFamily) $
    throwError err403 { errBody = "Controlled reference data cannot be reordered manually" }
  counts <- forM (zip [0 :: Int ..] ids) $ \(position, itemId) -> runDB
    ( rawSql
        ("UPDATE " <> ctsTable spec <> " SET sort_order=?, updated_at=now(), version=version+1 WHERE id=?::uuid AND catalog_id=?::uuid RETURNING 1")
        [PersistInt64 (fromIntegral position), PersistText itemId, PersistText (persistKeyText catalogKey)] :: SqlPersistT IO [Single Int]
    )
  unless (all ((== 1) . length) counts) $
    throwError err409 { errBody = "One or more reorder items do not belong to the catalog" }
  now <- liftIO getCurrentTime
  runDB $ update catalogKey [M.CatalogDefinitionCacheRevision +=. 1, M.CatalogDefinitionUpdatedAt =. now]
  writeAudit catalogKey UUID.nil Nothing "reordered" (Just user) Nothing Nothing "admin" (croCorrelationId request) (Just (croReason request)) "success" Nothing Nothing
  pure NoContent

mergeHandler :: AuthedUser -> Text -> CatalogMergeRequest -> AppM CatalogRevisionDTO
mergeHandler user catalogCode request = do
  requireCatalogCapability user "catalog.merge"
  sourceId <- validateUuidText "sourceItemId" (cmSourceItemId request)
  targetId <- validateUuidText "targetItemId" (cmTargetItemId request)
  when (sourceId == targetId) $
    throwError err400 { errBody = "Merge source and target must differ" }
  source <- loadCatalogItemById False catalogCode sourceId (Just "es")
  target <- loadCatalogItemById False catalogCode targetId (Just "es")
  unless (ciActive target) $
    throwError err409 { errBody = "Merge target must be active" }
  let draft = CatalogDraftRequest
        { cdrEntityId = Just sourceId
        , cdrBaseVersion = Just (ciVersion source)
        , cdrCode = ciCode source
        , cdrNameEs = ciNameEs source
        , cdrNameEn = ciNameEn source
        , cdrDescriptionEs = ciDescriptionEs source
        , cdrDescriptionEn = ciDescriptionEn source
        , cdrSearchAliasesEs = ciSearchAliases source
        , cdrSearchAliasesEn = []
        , cdrCurrentSlug = ciCurrentSlug source
        , cdrParentId = ciParentId source
        , cdrSortOrder = ciSortOrder source
        , cdrExternalCode = ciExternalCode source
        , cdrSourceVersion = ciSourceVersion source
        , cdrServiceOffering = Nothing
        , cdrRadioAutoStop = Nothing
        , cdrAppearanceMode = Nothing
        , cdrDisplaySymbol = ciDisplaySymbol source
        , cdrGlobalDefault = Nothing
        , cdrReason = cmReason request <> "; merge target=" <> targetId
        , cdrSourcePlatform = "admin"
        , cdrCorrelationId = cmCorrelationId request
        }
  revision <- createRevisionHandler user catalogCode draft
  Entity catalogKey _ <- findCatalogDefinition False catalogCode
  now <- liftIO getCurrentTime
  _ <- runDB $ insert M.CatalogMergeOperation
    { M.catalogMergeOperationCatalogId = catalogKey
    , M.catalogMergeOperationSourceEntityId = fromMaybe UUID.nil (UUID.fromText sourceId)
    , M.catalogMergeOperationTargetEntityId = fromMaybe UUID.nil (UUID.fromText targetId)
    , M.catalogMergeOperationStatus = "draft"
    , M.catalogMergeOperationReason = cmReason request
    , M.catalogMergeOperationAffectedReferences = Nothing
    , M.catalogMergeOperationRequestedBy = auPartyId user
    , M.catalogMergeOperationApprovedBy = Nothing
    , M.catalogMergeOperationExecutedAt = Nothing
    , M.catalogMergeOperationReversedAt = Nothing
    , M.catalogMergeOperationReversedBy = Nothing
    , M.catalogMergeOperationCreatedAt = now
    , M.catalogMergeOperationCorrelationId = cmCorrelationId request
    }
  pure revision

usageHandler :: AuthedUser -> Text -> Maybe Day -> Maybe Day -> AppM [CatalogUsageDTO]
usageHandler user catalogCode fromDay toDay = do
  requireCatalogCapability user "catalog.read"
  Entity catalogKey _ <- findCatalogDefinition False catalogCode
  let filters = [M.CatalogUsageDailyCatalogId ==. catalogKey]
        <> maybe [] (\day -> [M.CatalogUsageDailyDay >=. day]) fromDay
        <> maybe [] (\day -> [M.CatalogUsageDailyDay <=. day]) toDay
  rows <- runDB $ selectList filters [Desc M.CatalogUsageDailyDay]
  pure (map usageDTO rows)

usageDTO :: Entity M.CatalogUsageDaily -> CatalogUsageDTO
usageDTO (Entity _ usage) = CatalogUsageDTO
  { cuItemId = UUID.toText <$> M.catalogUsageDailyEntityId usage
  , cuDay = M.catalogUsageDailyDay usage
  , cuSelectionCount = M.catalogUsageDailySelectionCount usage
  , cuHistoricalReferenceCount = M.catalogUsageDailyHistoricalReferenceCount usage
  , cuReplacementCount = M.catalogUsageDailyReplacementCount usage
  , cuNoResultSearchCount = M.catalogUsageDailyNoResultSearchCount usage
  , cuFormFailureCount = M.catalogUsageDailyFormFailureCount usage
  }

exportCsvHandler :: AuthedUser -> Text -> AppM Text
exportCsvHandler user catalogCode = do
  requireCatalogCapability user "catalog.export"
  page <- loadCatalogPageByCode False True catalogCode (Just "es") Nothing (Just 1) (Just 200)
  when (cpTotal page > 200) $
    throwError err413 { errBody = "Use paginated export job for catalogs larger than 200 rows" }
  pure $ T.unlines ("id,code,name_es,name_en,description_es,description_en,slug,parent_id,sort_order,active,version" : map itemCsv (cpItems page))

itemCsv :: CatalogItemDTO -> Text
itemCsv item = T.intercalate "," (map csvField
  [ ciId item
  , ciCode item
  , ciNameEs item
  , ciNameEn item
  , fromMaybe "" (ciDescriptionEs item)
  , fromMaybe "" (ciDescriptionEn item)
  , fromMaybe "" (ciCurrentSlug item)
  , fromMaybe "" (ciParentId item)
  , T.pack (show (ciSortOrder item))
  , if ciActive item then "true" else "false"
  , T.pack (show (ciVersion item))
  ])

csvField :: Text -> Text
csvField value = "\"" <> T.replace "\"" "\"\"" value <> "\""

importCsvHandler :: AuthedUser -> Text -> Maybe Bool -> Text -> AppM CatalogImportResultDTO
importCsvHandler user catalogCode requestedDryRun csvBody = do
  requireCatalogCapability user "catalog.import"
  Entity catalogKey catalog <- findCatalogDefinition False catalogCode
  when (M.catalogDefinitionSensitive catalog) $
    requireCatalogApprovalAccess user
  let dryRun = fromMaybe True requestedDryRun
      contentLines = filter (not . T.null . T.strip) (T.lines csvBody)
      totalRows = max 0 (length contentLines - 1)
      headerOk = case contentLines of
        header : _ -> map T.toLower (parseCsvLine header) == ["code", "name_es", "name_en", "description_es", "description_en", "slug", "parent_id", "sort_order", "external_code", "source_version"]
        [] -> False
      parsedRows = map parseCsvLine (drop 1 contentLines)
      rowErrors = ["row " <> T.pack (show rowNumber) <> ": expected 10 columns" | (rowNumber, row) <- zip [2 :: Int ..] parsedRows, length row /= 10]
  unless headerOk $
    throwError err400 { errBody = "CSV header does not match the catalog import contract" }
  digestRows <- runDB $ rawSql "SELECT encode(digest(?, 'sha256'), 'hex')" [PersistText csvBody]
  digest <- case digestRows of
    [Single value] -> pure value
    _ -> throwError err500 { errBody = "Unable to calculate import digest" }
  now <- liftIO getCurrentTime
  let status = if null rowErrors then if dryRun then "validated" else "drafts-created" else "rejected"
      job = M.CatalogImportJob
        { M.catalogImportJobCatalogId = catalogKey
        , M.catalogImportJobSourceName = "csv-admin"
        , M.catalogImportJobSourceVersion = ""
        , M.catalogImportJobOriginalFilename = Nothing
        , M.catalogImportJobContentSha256 = digest
        , M.catalogImportJobDryRun = dryRun
        , M.catalogImportJobStatus = status
        , M.catalogImportJobTotalRows = totalRows
        , M.catalogImportJobAcceptedRows = totalRows - length rowErrors
        , M.catalogImportJobRejectedRows = length rowErrors
        , M.catalogImportJobAmbiguousRows = 0
        , M.catalogImportJobErrorReport = Nothing
        , M.catalogImportJobRequestedBy = auPartyId user
        , M.catalogImportJobReviewedBy = Nothing
        , M.catalogImportJobCreatedAt = now
        , M.catalogImportJobCompletedAt = Just now
        , M.catalogImportJobCorrelationId = "csv:" <> digest
        }
  jobKey <- runDB $ upsertBy (M.UniqueCatalogImportDigest catalogKey digest "") job
    [ M.CatalogImportJobDryRun =. dryRun
    , M.CatalogImportJobStatus =. status
    , M.CatalogImportJobTotalRows =. totalRows
    , M.CatalogImportJobAcceptedRows =. totalRows - length rowErrors
    , M.CatalogImportJobRejectedRows =. length rowErrors
    , M.CatalogImportJobCompletedAt =. Just now
    ]
  unless (dryRun || not (null rowErrors)) $
    forM_ (zip [2 :: Int ..] parsedRows) $ \(rowNumber, row) ->
      case row of
        [draftCode, draftNameEs, draftNameEn, draftDescriptionEs, draftDescriptionEn, draftSlug, draftParentId, draftSortOrder, draftExternalCode, draftSourceVersion] -> do
          sortValue <- maybe (throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid sort_order at row " <> T.pack (show rowNumber))) }) pure (readInt draftSortOrder)
          _ <- createRevisionHandler user catalogCode CatalogDraftRequest
            { cdrEntityId = Nothing
            , cdrBaseVersion = Nothing
            , cdrCode = draftCode
            , cdrNameEs = draftNameEs
            , cdrNameEn = draftNameEn
            , cdrDescriptionEs = nonEmpty draftDescriptionEs
            , cdrDescriptionEn = nonEmpty draftDescriptionEn
            , cdrSearchAliasesEs = []
            , cdrSearchAliasesEn = []
            , cdrCurrentSlug = nonEmpty draftSlug
            , cdrParentId = nonEmpty draftParentId
            , cdrSortOrder = sortValue
            , cdrExternalCode = nonEmpty draftExternalCode
            , cdrSourceVersion = nonEmpty draftSourceVersion
            , cdrServiceOffering = Nothing
            , cdrRadioAutoStop = Nothing
            , cdrAppearanceMode = Nothing
            , cdrDisplaySymbol = Nothing
            , cdrGlobalDefault = Nothing
            , cdrReason = "CSV import row " <> T.pack (show rowNumber)
            , cdrSourcePlatform = "csv-import"
            , cdrCorrelationId = "csv:" <> digest <> ":" <> T.pack (show rowNumber)
            }
          pure ()
        _ -> pure ()
  pure CatalogImportResultDTO
    { cirImportJobId = persistKeyText (entityKey jobKey)
    , cirDryRun = dryRun
    , cirStatus = status
    , cirTotalRows = totalRows
    , cirAcceptedRows = totalRows - length rowErrors
    , cirRejectedRows = length rowErrors
    , cirAmbiguousRows = 0
    , cirErrors = rowErrors
    }

parseCsvLine :: Text -> [Text]
parseCsvLine = finalize . T.foldl' step (False, "", [])
  where
    step (quoted, current, fields) character
      | character == '"' = (not quoted, current, fields)
      | character == ',' && not quoted = (quoted, "", fields <> [T.strip current])
      | otherwise = (quoted, T.snoc current character, fields)
    finalize (_, current, fields) = fields <> [T.strip current]

readInt :: Text -> Maybe Int
readInt raw = case reads (T.unpack (T.strip raw)) of
  [(value, "")] -> Just value
  _ -> Nothing

nonEmpty :: Text -> Maybe Text
nonEmpty raw = let value = T.strip raw in if T.null value then Nothing else Just value

validateDraft :: CatalogDraftRequest -> AppM ()
validateDraft draft = do
  when (T.null (T.strip (cdrCode draft)) || T.length (cdrCode draft) > 100) $
    throwError err400 { errBody = "code is required and limited to 100 characters" }
  when (T.null (T.strip (cdrNameEs draft)) || T.null (T.strip (cdrNameEn draft))) $
    throwError err400 { errBody = "Complete Spanish and English names are required" }
  when (T.null (T.strip (cdrReason draft)) || T.null (T.strip (cdrCorrelationId draft))) $
    throwError err400 { errBody = "reason and correlationId are required" }
  when (cdrSortOrder draft < 0) $
    throwError err400 { errBody = "sortOrder cannot be negative" }
  _ <- traverse (validateUuidText "entityId") (cdrEntityId draft)
  _ <- traverse (validateUuidText "parentId") (cdrParentId draft)
  case (cdrEntityId draft, cdrBaseVersion draft) of
    (Nothing, Nothing) -> pure ()
    (Nothing, Just 0) -> pure ()
    (Nothing, Just _) -> throwError err400 { errBody = "New catalog items must start at baseVersion 0" }
    (Just _, Nothing) -> throwError err400 { errBody = "Existing catalog items require baseVersion" }
    (Just _, Just version) | version > 0 -> pure ()
    (Just _, Just _) -> throwError err400 { errBody = "Existing catalog item baseVersion must be positive" }
  pure ()

validateDraftBaseVersion
  :: CatalogTableSpec
  -> M.CatalogDefinitionId
  -> CatalogDraftRequest
  -> AppM ()
validateDraftBaseVersion spec catalogKey draft =
  case cdrEntityId draft of
    Nothing -> pure ()
    Just entityId -> do
      rows <- runDB
        ( rawSql
            ("SELECT version FROM " <> ctsTable spec <> " WHERE id=?::uuid AND catalog_id=?::uuid")
            [PersistText entityId, PersistText (persistKeyText catalogKey)]
            :: SqlPersistT IO [Single Int]
        )
      case rows of
        [Single currentVersion]
          | Just currentVersion == cdrBaseVersion draft -> pure ()
          | otherwise -> throwError err409 { errBody = "Catalog item version changed; reload before creating a revision" }
        _ -> throwError err404 { errBody = "Catalog item does not exist in this catalog" }

validateCatalogSpecificDraft :: CatalogTableSpec -> CatalogDraftRequest -> AppM ()
validateCatalogSpecificDraft spec draft =
  case ctsFamily spec of
    ServiceOfferingFamily -> do
      serviceDraft <- maybe
        (throwError err400 { errBody = "serviceOffering is required for the services catalog" })
        pure
        (cdrServiceOffering draft)
      when (isJust (cdrParentId draft) || isJust (cdrExternalCode draft) || isJust (cdrSourceVersion draft)) $
        throwError err400 { errBody = "Service offerings use typed category and pricing relations, not generic parent or external fields" }
      _ <- validateUuidText "serviceOffering.categoryId" (sodCategoryId serviceDraft)
      _ <- validateUuidText "serviceOffering.pricingModelId" (sodPricingModelId serviceDraft)
      _ <- validateUuidText "serviceOffering.currencyId" (sodCurrencyId serviceDraft)
      _ <- traverse (validateUuidText "serviceOffering.taxRateId") (sodTaxRateId serviceDraft)
      let resources = sodDefaultResources serviceDraft
          resourceIds = map sordResourceId resources
          invalidRate = maybe False (\value -> value < 0 || value > 1000000000) (sodRateCents serviceDraft)
          invalidDuration = maybe False (\value -> value < 15 || value > 840) (sodDefaultDurationMinutes serviceDraft)
          invalidBilling value = T.length (T.strip value) > 80 || T.any isControl value
      when invalidRate $
        throwError err400 { errBody = "serviceOffering.rateCents must be between 0 and 1000000000" }
      when invalidDuration $
        throwError err400 { errBody = "serviceOffering.defaultDurationMinutes must be between 15 and 840" }
      when (maybe False invalidBilling (sodBillingUnitEs serviceDraft) || maybe False invalidBilling (sodBillingUnitEn serviceDraft)) $
        throwError err400 { errBody = "Service offering billing units are limited to 80 visible characters" }
      when (length resources > 50 || length resourceIds /= length (nub resourceIds)) $
        throwError err400 { errBody = "serviceOffering.defaultResources must contain at most 50 unique resource IDs" }
      forM_ resources $ \resource -> do
        _ <- validatePositiveInt64Text "serviceOffering.defaultResources.resourceId" (sordResourceId resource)
        _ <- validateUuidText
          "serviceOffering.defaultResources.selectionModeId"
          (sordSelectionModeId resource)
        when (sordSortOrder resource < 0) $
          throwError err400 { errBody = "Default resource sortOrder cannot be negative" }
      when (isJust (cdrRadioAutoStop draft)) $
        throwError err400 { errBody = "radioAutoStop fields are only valid for the radio auto-stop catalog" }
      when (isJust (cdrAppearanceMode draft)) $
        throwError err400 { errBody = "appearanceMode fields are only valid for the appearance modes catalog" }
      when (isJust (cdrDisplaySymbol draft)) $
        throwError err400 { errBody = "displaySymbol is only valid for reaction types" }
      when (isJust (cdrGlobalDefault draft)) $
        throwError err400 { errBody = "globalDefault is only valid for catalogs with an explicit global default scope" }
    RadioAutoStopFamily -> do
      radioDraft <- maybe
        (throwError err400 { errBody = "radioAutoStop is required for the radio auto-stop catalog" })
        pure
        (cdrRadioAutoStop draft)
      when
        ( isJust (cdrParentId draft)
            || isJust (cdrExternalCode draft)
            || isJust (cdrSourceVersion draft)
            || isJust (cdrServiceOffering draft)
            || isJust (cdrAppearanceMode draft)
            || isJust (cdrDisplaySymbol draft)
            || isJust (cdrGlobalDefault draft)
        ) $
        throwError err400
          { errBody = "Radio auto-stop options use their typed duration field, not generic parent, external, or service fields" }
      when (rasdDurationMinutes radioDraft < 0 || rasdDurationMinutes radioDraft > 1440) $
        throwError err400 { errBody = "radioAutoStop.durationMinutes must be between 0 and 1440" }
    AppearanceModeFamily -> do
      _ <- maybe
        (throwError err400 { errBody = "appearanceMode is required for the appearance modes catalog" })
        pure
        (cdrAppearanceMode draft)
      when
        ( isJust (cdrParentId draft)
            || isJust (cdrExternalCode draft)
            || isJust (cdrSourceVersion draft)
            || isJust (cdrServiceOffering draft)
            || isJust (cdrRadioAutoStop draft)
            || isJust (cdrDisplaySymbol draft)
            || isJust (cdrGlobalDefault draft)
        ) $
        throwError err400
          { errBody = "Appearance modes use their typed default field, not generic parent, external, service, or Radio fields" }
      unless (cdrCode draft `elem` ["system", "light", "dark"]) $
        throwError err400 { errBody = "appearance mode code is not recognized by the application renderer" }
    FlatCatalogFamily | ctsTable spec `elem` ["reaction_type", "content_reaction_type"] -> do
      symbol <- maybe
        (throwError err400 { errBody = "displaySymbol is required for reaction types" })
        pure
        (cdrDisplaySymbol draft)
      when
        ( T.null (T.strip symbol)
            || T.length (T.strip symbol) > 16
            || T.any isControl symbol
            || isJust (cdrParentId draft)
            || isJust (cdrExternalCode draft)
            || isJust (cdrSourceVersion draft)
            || isJust (cdrServiceOffering draft)
            || isJust (cdrRadioAutoStop draft)
            || isJust (cdrAppearanceMode draft)
            || isJust (cdrGlobalDefault draft)
        ) $
        throwError err400 { errBody = "Reaction types require one visible displaySymbol and no unrelated typed fields" }
    _ -> do
      when (isJust (cdrServiceOffering draft)) $
        throwError err400 { errBody = "serviceOffering fields are only valid for the services catalog" }
      when (isJust (cdrRadioAutoStop draft)) $
        throwError err400 { errBody = "radioAutoStop fields are only valid for the radio auto-stop catalog" }
      when (isJust (cdrAppearanceMode draft)) $
        throwError err400 { errBody = "appearanceMode fields are only valid for the appearance modes catalog" }
      when (isJust (cdrDisplaySymbol draft)) $
        throwError err400 { errBody = "displaySymbol is only valid for catalogs with a specialized symbol adapter" }
      when (isJust (cdrGlobalDefault draft) && isNothing (ctsDefaultScopeKind spec)) $
        throwError err400 { errBody = "globalDefault is only valid for catalogs with an explicit global default scope" }

validatePositiveInt64Text :: Text -> Text -> AppM Int64
validatePositiveInt64Text fieldName raw =
  case reads (T.unpack (T.strip raw)) of
    [(value, "")] | value > 0 -> pure value
    _ -> throwError err400
      { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a positive integer ID")) }

loadRevision :: Text -> AppM (M.CatalogRevisionId, M.CatalogRevision, M.CatalogDefinition)
loadRevision rawRevisionId = do
  revisionUuid <- parseUuid "revisionId" rawRevisionId
  let revisionKey = M.CatalogRevisionKey revisionUuid
  revision <- runDB (get revisionKey) >>= maybe (throwError err404 { errBody = "Catalog revision not found" }) pure
  catalog <- runDB (getJust (M.catalogRevisionCatalogId revision))
  pure (revisionKey, revision, catalog)

revisionDTO :: M.CatalogDefinition -> Entity M.CatalogRevision -> AppM CatalogRevisionDTO
revisionDTO catalog (Entity revisionKey revision) = do
  draft <- decodeDraft (M.catalogRevisionNewValues revision)
  state <- workflowStateCode (M.catalogRevisionWorkflowStateId revision)
  pure CatalogRevisionDTO
    { crId = persistKeyText revisionKey
    , crCatalogId = persistKeyText (M.catalogRevisionCatalogId revision)
    , crCatalogCode = M.catalogDefinitionCode catalog
    , crEntityId = UUID.toText (M.catalogRevisionEntityId revision)
    , crWorkflowState = state
    , crBaseVersion = M.catalogRevisionBaseVersion revision
    , crProposedVersion = M.catalogRevisionProposedVersion revision
    , crDraft = draft
    , crCreatedBy = keyInt64 (M.catalogRevisionCreatedBy revision)
    , crCreatedAt = M.catalogRevisionCreatedAt revision
    , crSubmittedAt = M.catalogRevisionSubmittedAt revision
    , crReviewedBy = keyInt64 <$> M.catalogRevisionReviewedBy revision
    , crReviewedAt = M.catalogRevisionReviewedAt revision
    , crApprovedBy = keyInt64 <$> M.catalogRevisionApprovedBy revision
    , crApprovedAt = M.catalogRevisionApprovedAt revision
    , crReviewerNotes = M.catalogRevisionReviewerNotes revision
    , crRejectionReason = M.catalogRevisionRejectionReason revision
    , crScheduledPublishAt = M.catalogRevisionScheduledPublishAt revision
    , crPublishedAt = M.catalogRevisionPublishedAt revision
    }

decodeDraft :: AesonValue -> AppM CatalogDraftRequest
decodeDraft (AesonValue value) =
  case Aeson.fromJSON value of
    Aeson.Success draft -> pure draft
    Aeson.Error _ -> throwError err500 { errBody = "Stored catalog revision does not match its typed schema" }

loadWorkflowStateKey :: M.WorkflowDefinitionId -> Text -> AppM M.WorkflowStateId
loadWorkflowStateKey workflowKey stateCode = do
  result <- runDB $ getBy (M.UniqueWorkflowStateCode workflowKey stateCode)
  maybe (throwError err503 { errBody = "Required persisted catalog workflow state is missing" }) (pure . entityKey) result

workflowStateCode :: M.WorkflowStateId -> AppM Text
workflowStateCode key = M.workflowStateCode <$> runDB (getJust key)

writeAudit
  :: M.CatalogDefinitionId
  -> UUID.UUID
  -> Maybe M.CatalogRevisionId
  -> Text
  -> Maybe AuthedUser
  -> Maybe AuthedUser
  -> Maybe AuthedUser
  -> Text
  -> Text
  -> Maybe Text
  -> Text
  -> Maybe Aeson.Value
  -> Maybe Aeson.Value
  -> AppM ()
writeAudit catalogKey entityUuid revisionKey operation actor reviewer approver sourcePlatform correlationId reason result previousValue newValue = do
  now <- liftIO getCurrentTime
  runDB $ insert_ M.CatalogAuditEvent
    { M.catalogAuditEventCatalogId = catalogKey
    , M.catalogAuditEventEntityId = entityUuid
    , M.catalogAuditEventRevisionId = revisionKey
    , M.catalogAuditEventOperation = operation
    , M.catalogAuditEventPreviousValues = AesonValue <$> previousValue
    , M.catalogAuditEventNewValues = AesonValue <$> newValue
    , M.catalogAuditEventActorId = auPartyId <$> actor
    , M.catalogAuditEventReviewerId = auPartyId <$> reviewer
    , M.catalogAuditEventApproverId = auPartyId <$> approver
    , M.catalogAuditEventOccurredAt = now
    , M.catalogAuditEventSourcePlatform = sourcePlatform
    , M.catalogAuditEventImportJobId = Nothing
    , M.catalogAuditEventReason = reason
    , M.catalogAuditEventCorrelationId = correlationId
    , M.catalogAuditEventResult = result
    , M.catalogAuditEventAffectedRelationships = Nothing
    }

securityAdminServer :: AuthedUser -> ServerT SecurityAdminAPI AppM
securityAdminServer user =
       listSecurityRolesHandler user
  :<|> listPartySecurityRolesHandler user
  :<|> listSecurityGrantRevisionsHandler user
  :<|> listSecurityAuditHandler user
  :<|> createPartyRoleGrantRevisionHandler user
  :<|> createRolePermissionGrantRevisionHandler user
  :<|> submitSecurityGrantRevisionHandler user
  :<|> approveSecurityGrantRevisionHandler user
  :<|> rejectSecurityGrantRevisionHandler user

requireSecurityCapability :: AuthedUser -> Text -> AppM ()
requireSecurityCapability user permissionCode = do
  either throwError pure (validateModuleAccess ModuleAdmin user)
  rows <- runDB $ rawSql
    "SELECT EXISTS (SELECT 1 FROM party_security_role psr JOIN security_role r ON r.id=psr.role_id JOIN role_permission rp ON rp.role_id=r.id JOIN security_permission p ON p.id=rp.permission_id JOIN security_action a ON a.id=p.action_id JOIN security_module m ON m.id=p.module_id WHERE psr.party_id=? AND psr.active=TRUE AND r.active=TRUE AND rp.active=TRUE AND p.active=TRUE AND a.active=TRUE AND m.active=TRUE AND p.code=?)"
    [toPersistValue (auPartyId user), PersistText permissionCode]
  case rows of
    [Single True] -> pure ()
    _ -> throwError err403
      { errBody = BL.fromStrict (TE.encodeUtf8 ("Missing security capability: " <> permissionCode))
      }

listSecurityRolesHandler :: AuthedUser -> AppM [SecurityRoleDTO]
listSecurityRolesHandler user = do
  requireSecurityCapability user "security.read"
  roles <- runDB $ selectList [M.SecurityRoleActive ==. True] [Asc M.SecurityRoleSortOrder, Asc M.SecurityRoleCode]
  permissionRows <- runDB $ rawSql
    "SELECT r.id::text, p.code, CASE WHEN p.resource_scope='module' AND a.code='access' THEN m.code ELSE NULL END FROM security_role r JOIN role_permission rp ON rp.role_id=r.id JOIN security_permission p ON p.id=rp.permission_id JOIN security_action a ON a.id=p.action_id JOIN security_module m ON m.id=p.module_id WHERE r.active=TRUE AND rp.active=TRUE AND p.active=TRUE AND a.active=TRUE AND m.active=TRUE ORDER BY r.sort_order, r.code, m.sort_order, p.code"
    [] :: AppM [(Single Text, Single Text, Single (Maybe Text))]
  let permissionsByRole = Map.fromListWith (flip (++))
        [ (roleId, [permissionCode])
        | (Single roleId, Single permissionCode, _) <- permissionRows
        ]
      modulesByRole = Map.fromListWith (flip (++))
        [ (roleId, [moduleCode])
        | (Single roleId, _, Single (Just moduleCode)) <- permissionRows
        ]
  pure
    [ SecurityRoleDTO
        { srId = persistKeyText roleKey
        , srCode = M.securityRoleCode roleValue
        , srNameEs = M.securityRoleNameEs roleValue
        , srNameEn = M.securityRoleNameEn roleValue
        , srDescriptionEs = M.securityRoleDescriptionEs roleValue
        , srDescriptionEn = M.securityRoleDescriptionEn roleValue
        , srEmergencyAdministrator = M.securityRoleEmergencyAdministrator roleValue
        , srSystemRole = M.securityRoleSystemRole roleValue
        , srSelfAssignable = M.securityRoleSelfAssignable roleValue
        , srAutomaticAssignable = M.securityRoleAutomaticAssignable roleValue
        , srActive = M.securityRoleActive roleValue
        , srModuleCodes = nub (Map.findWithDefault [] (persistKeyText roleKey) modulesByRole)
        , srPermissionCodes = nub (Map.findWithDefault [] (persistKeyText roleKey) permissionsByRole)
        , srVersion = M.securityRoleVersion roleValue
        }
    | Entity roleKey roleValue <- roles
    ]

listPartySecurityRolesHandler :: AuthedUser -> Maybe Int64 -> AppM [SecurityPartyRoleAssignmentDTO]
listPartySecurityRolesHandler user requestedPartyId = do
  requireSecurityCapability user "security.read"
  partyFilter <- case requestedPartyId of
    Nothing -> pure []
    Just rawPartyId
      | rawPartyId > 0 -> pure [M.PartySecurityRolePartyId ==. toSqlKey rawPartyId]
      | otherwise -> throwError err400 { errBody = "partyId must be a positive integer" }
  assignments <- runDB $ selectList partyFilter [Asc M.PartySecurityRolePartyId, Asc M.PartySecurityRoleRoleId]
  roleRows <- runDB $ selectList [] [Asc M.SecurityRoleSortOrder, Asc M.SecurityRoleCode]
  let roleMap = Map.fromList [(roleKey, roleValue) | Entity roleKey roleValue <- roleRows]
  forM assignments $ \(Entity assignmentKey assignment) ->
    case Map.lookup (M.partySecurityRoleRoleId assignment) roleMap of
      Nothing -> throwError err500 { errBody = "Party security role references a missing role" }
      Just roleValue -> pure SecurityPartyRoleAssignmentDTO
        { spaId = persistKeyText assignmentKey
        , spaPartyId = fromSqlKey (M.partySecurityRolePartyId assignment)
        , spaRoleId = persistKeyText (M.partySecurityRoleRoleId assignment)
        , spaRoleCode = M.securityRoleCode roleValue
        , spaRoleNameEs = M.securityRoleNameEs roleValue
        , spaActive = M.partySecurityRoleActive assignment
        , spaGrantedBy = fromSqlKey <$> M.partySecurityRoleGrantedBy assignment
        , spaApprovedBy = fromSqlKey <$> M.partySecurityRoleApprovedBy assignment
        , spaApprovalMode = M.partySecurityRoleApprovalMode assignment
        , spaSourceRevisionId = persistKeyText <$> M.partySecurityRoleSourceRevisionId assignment
        , spaSourcePolicyId = persistKeyText <$> M.partySecurityRoleSourcePolicyId assignment
        , spaCreatedAt = M.partySecurityRoleCreatedAt assignment
        , spaRevokedAt = M.partySecurityRoleRevokedAt assignment
        , spaVersion = M.partySecurityRoleVersion assignment
        }

listSecurityGrantRevisionsHandler
  :: AuthedUser
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> AppM [SecurityGrantRevisionDTO]
listSecurityGrantRevisionsHandler user requestedState requestedPage requestedPageSize = do
  requireSecurityCapability user "security.read"
  stateFilters <- case T.strip <$> requestedState of
    Nothing -> pure []
    Just "" -> throwError err400 { errBody = "workflowState must not be blank" }
    Just stateCode -> do
      stateKey <- loadSecurityWorkflowStateKey stateCode
      pure [M.SecurityGrantRevisionWorkflowStateId ==. stateKey]
  let page = max 1 (fromMaybe 1 requestedPage)
      pageSize = min 200 (max 1 (fromMaybe 50 requestedPageSize))
  revisions <- runDB $ selectList stateFilters
    [Desc M.SecurityGrantRevisionCreatedAt, LimitTo pageSize, OffsetBy ((page - 1) * pageSize)]
  securityRevisionDTOs revisions

listSecurityAuditHandler
  :: AuthedUser
  -> Maybe Int64
  -> Maybe Int
  -> Maybe Int
  -> AppM [SecurityAuditEventDTO]
listSecurityAuditHandler user requestedPartyId requestedPage requestedPageSize = do
  requireSecurityCapability user "security.read"
  partyFilters <- case requestedPartyId of
    Nothing -> pure []
    Just rawPartyId
      | rawPartyId > 0 -> pure [M.SecurityAuditEventPartyId ==. Just (toSqlKey rawPartyId)]
      | otherwise -> throwError err400 { errBody = "partyId must be a positive integer" }
  let page = max 1 (fromMaybe 1 requestedPage)
      pageSize = min 200 (max 1 (fromMaybe 50 requestedPageSize))
  rows <- runDB $ selectList partyFilters
    [Desc M.SecurityAuditEventOccurredAt, LimitTo pageSize, OffsetBy ((page - 1) * pageSize)]
  pure (map securityAuditDTO rows)

createPartyRoleGrantRevisionHandler
  :: AuthedUser
  -> PartyRoleGrantDraftRequest
  -> AppM SecurityGrantRevisionDTO
createPartyRoleGrantRevisionHandler user request = do
  requireSecurityCapability user "security.create"
  when (prgPartyId request <= 0) $
    throwError err400 { errBody = "partyId must be a positive integer" }
  roleKey <- securityRoleKeyFromText (prgRoleId request)
  let partyKey = toSqlKey (prgPartyId request)
  partyExists <- runDB $ isJust <$> (get partyKey :: SqlPersistT IO (Maybe Core.Party))
  unless partyExists $ throwError err404 { errBody = "Target party not found" }
  credentialCount <- runDB $ count
    [ Core.UserCredentialPartyId ==. partyKey
    , Core.UserCredentialActive ==. True
    ]
  when (credentialCount /= 1) $
    throwError err409 { errBody = "Security role targets require exactly one active user credential" }
  createSecurityGrantRevision
    user
    "party-role"
    (Just partyKey)
    roleKey
    Nothing
    (prgDesiredActive request)
    (prgExpectedVersion request)
    (prgReason request)
    (prgSourcePlatform request)
    (prgCorrelationId request)

createRolePermissionGrantRevisionHandler
  :: AuthedUser
  -> RolePermissionGrantDraftRequest
  -> AppM SecurityGrantRevisionDTO
createRolePermissionGrantRevisionHandler user request = do
  requireSecurityCapability user "security.create"
  roleKey <- securityRoleKeyFromText (rpgRoleId request)
  permissionKey <- securityPermissionKeyFromText (rpgPermissionId request)
  createSecurityGrantRevision
    user
    "role-permission"
    Nothing
    roleKey
    (Just permissionKey)
    (rpgDesiredActive request)
    (rpgExpectedVersion request)
    (rpgReason request)
    (rpgSourcePlatform request)
    (rpgCorrelationId request)

createSecurityGrantRevision
  :: AuthedUser
  -> Text
  -> Maybe PartyId
  -> M.SecurityRoleId
  -> Maybe M.SecurityPermissionId
  -> Bool
  -> Int
  -> Text
  -> Text
  -> Text
  -> AppM SecurityGrantRevisionDTO
createSecurityGrantRevision user changeKind partyKey roleKey permissionKey desiredActive expectedVersion rawReason rawSource rawCorrelation = do
  when (expectedVersion < 0) $
    throwError err400 { errBody = "expectedVersion cannot be negative" }
  reason <- validateSecurityAuditText "reason" 1 2000 rawReason
  sourcePlatform <- validateSecurityIdentifier "sourcePlatform" 2 50 rawSource
  correlationId <- validateSecurityIdentifier "correlationId" 8 200 rawCorrelation
  current <- loadSecurityGrantCurrent changeKind partyKey roleKey permissionKey
  let currentVersion = maybe 0 snd current
      currentActive = maybe False fst current
  when (expectedVersion /= currentVersion) $
    throwError err409 { errBody = "Security grant version conflict" }
  when (desiredActive == currentActive) $
    throwError err409 { errBody = "Security grant revision would not change the current assignment" }
  duplicateCorrelation <- runDB $ getBy (M.UniqueSecurityGrantCorrelation correlationId)
  when (isJust duplicateCorrelation) $
    throwError err409 { errBody = "Security grant correlationId already exists" }
  draftState <- loadSecurityWorkflowStateKey "draft"
  now <- liftIO getCurrentTime
  let revision = M.SecurityGrantRevision
        { M.securityGrantRevisionChangeKind = changeKind
        , M.securityGrantRevisionPartyId = partyKey
        , M.securityGrantRevisionRoleId = roleKey
        , M.securityGrantRevisionPermissionId = permissionKey
        , M.securityGrantRevisionDesiredActive = desiredActive
        , M.securityGrantRevisionExpectedVersion = expectedVersion
        , M.securityGrantRevisionWorkflowStateId = draftState
        , M.securityGrantRevisionCreatedBy = auPartyId user
        , M.securityGrantRevisionCreatedAt = now
        , M.securityGrantRevisionSubmittedAt = Nothing
        , M.securityGrantRevisionReviewedBy = Nothing
        , M.securityGrantRevisionReviewedAt = Nothing
        , M.securityGrantRevisionApprovedBy = Nothing
        , M.securityGrantRevisionApprovedAt = Nothing
        , M.securityGrantRevisionReviewerNotes = Nothing
        , M.securityGrantRevisionRejectionReason = Nothing
        , M.securityGrantRevisionApprovalMode = "normal"
        , M.securityGrantRevisionEmergencyReason = Nothing
        , M.securityGrantRevisionSourcePlatform = sourcePlatform
        , M.securityGrantRevisionCorrelationId = correlationId
        , M.securityGrantRevisionReason = reason
        , M.securityGrantRevisionResult = Just "draft-created"
        , M.securityGrantRevisionVersion = 1
        }
  revisionKey <- runDB $ do
    insertedKey <- insert revision
    insertSecurityAuditEvent insertedKey revision "draft-created" (Just currentActive) (Just desiredActive)
      (Just (auPartyId user)) Nothing Nothing "normal" "success" now
    pure insertedKey
  securityRevisionDTO (Entity revisionKey revision)

submitSecurityGrantRevisionHandler :: AuthedUser -> Text -> AppM SecurityGrantRevisionDTO
submitSecurityGrantRevisionHandler user rawRevisionId = do
  requireSecurityCapability user "security.assign"
  Entity revisionKey revision <- loadSecurityGrantRevision rawRevisionId
  stateCode <- workflowStateCode (M.securityGrantRevisionWorkflowStateId revision)
  unless (stateCode `elem` ["draft", "rejected"]) $
    throwError err409 { errBody = "Only draft or rejected security revisions can be submitted" }
  when (M.securityGrantRevisionCreatedBy revision /= auPartyId user) $
    throwError err403 { errBody = "Only the security revision author can submit it" }
  reviewState <- loadSecurityWorkflowStateKey "review"
  now <- liftIO getCurrentTime
  runDB $ do
    update revisionKey
      [ M.SecurityGrantRevisionWorkflowStateId =. reviewState
      , M.SecurityGrantRevisionSubmittedAt =. Just now
      , M.SecurityGrantRevisionReviewedBy =. Nothing
      , M.SecurityGrantRevisionReviewedAt =. Nothing
      , M.SecurityGrantRevisionApprovedBy =. Nothing
      , M.SecurityGrantRevisionApprovedAt =. Nothing
      , M.SecurityGrantRevisionReviewerNotes =. Nothing
      , M.SecurityGrantRevisionRejectionReason =. Nothing
      , M.SecurityGrantRevisionApprovalMode =. "normal"
      , M.SecurityGrantRevisionEmergencyReason =. Nothing
      , M.SecurityGrantRevisionResult =. Just "submitted"
      , M.SecurityGrantRevisionVersion +=. 1
      ]
    insertSecurityAuditEvent revisionKey revision "submitted" Nothing
      (Just (M.securityGrantRevisionDesiredActive revision)) (Just (auPartyId user)) Nothing Nothing "normal" "success" now
  updated <- runDB (getJust revisionKey)
  securityRevisionDTO (Entity revisionKey updated)

approveSecurityGrantRevisionHandler
  :: AuthedUser
  -> Text
  -> SecurityGrantReviewRequest
  -> AppM SecurityGrantRevisionDTO
approveSecurityGrantRevisionHandler user rawRevisionId review = do
  requireSecurityCapability user "security.approve"
  requireSecurityCapability user "security.assign"
  notes <- validateSecurityAuditText "notes" 1 2000 (sgrNotes review)
  Entity revisionKey revision <- loadSecurityGrantRevision rawRevisionId
  stateCode <- workflowStateCode (M.securityGrantRevisionWorkflowStateId revision)
  unless (stateCode == "review") $
    throwError err409 { errBody = "Only security revisions in review can be approved" }
  emergencyReason <- traverse
    (validateSecurityAuditText "emergencyOverrideReason" 20 2000)
    (sgrEmergencyOverrideReason review)
  let selfApproval = M.securityGrantRevisionCreatedBy revision == auPartyId user
  when (selfApproval && emergencyReason == Nothing) $
    throwError err403 { errBody = "Security change authors cannot approve their own change without an emergency override" }
  when (not selfApproval && isJust emergencyReason) $
    throwError err400 { errBody = "Emergency override is valid only for documented self-approval" }
  when selfApproval $ do
    requireSecurityCapability user "security.emergency-recover"
    emergencyActor <- isActiveEmergencyAdministrator (auPartyId user)
    unless emergencyActor $
      throwError err403 { errBody = "Emergency override requires an active emergency administrator" }
  current <- loadSecurityGrantCurrent
    (M.securityGrantRevisionChangeKind revision)
    (M.securityGrantRevisionPartyId revision)
    (M.securityGrantRevisionRoleId revision)
    (M.securityGrantRevisionPermissionId revision)
  let currentVersion = maybe 0 snd current
  when (currentVersion /= M.securityGrantRevisionExpectedVersion revision) $
    throwError err409 { errBody = "Security grant changed after this revision was drafted" }
  publishedState <- loadSecurityWorkflowStateKey "published"
  now <- liftIO getCurrentTime
  let approvalMode = if selfApproval then "emergency" else "normal"
  runDB $ do
    rawExecute
      "DO $$ BEGIN PERFORM pg_advisory_xact_lock(hashtextextended('tdf-security-grants-v1', 0)); END $$"
      []
    update revisionKey
      [ M.SecurityGrantRevisionWorkflowStateId =. publishedState
      , M.SecurityGrantRevisionReviewedBy =. Just (auPartyId user)
      , M.SecurityGrantRevisionReviewedAt =. Just now
      , M.SecurityGrantRevisionApprovedBy =. Just (auPartyId user)
      , M.SecurityGrantRevisionApprovedAt =. Just now
      , M.SecurityGrantRevisionReviewerNotes =. Just notes
      , M.SecurityGrantRevisionRejectionReason =. Nothing
      , M.SecurityGrantRevisionApprovalMode =. approvalMode
      , M.SecurityGrantRevisionEmergencyReason =. emergencyReason
      , M.SecurityGrantRevisionResult =. Just "published"
      , M.SecurityGrantRevisionVersion +=. 1
      ]
    applySecurityGrant revisionKey revision user approvalMode emergencyReason now
    insertSecurityAuditEvent revisionKey revision "published" (fst <$> current)
      (Just (M.securityGrantRevisionDesiredActive revision))
      (Just (M.securityGrantRevisionCreatedBy revision)) (Just (auPartyId user)) (Just (auPartyId user))
      approvalMode "success" now
  updated <- runDB (getJust revisionKey)
  securityRevisionDTO (Entity revisionKey updated)

rejectSecurityGrantRevisionHandler
  :: AuthedUser
  -> Text
  -> SecurityGrantReviewRequest
  -> AppM SecurityGrantRevisionDTO
rejectSecurityGrantRevisionHandler user rawRevisionId review = do
  requireSecurityCapability user "security.review"
  notes <- validateSecurityAuditText "notes" 1 2000 (sgrNotes review)
  when (isJust (sgrEmergencyOverrideReason review)) $
    throwError err400 { errBody = "Emergency override is not valid for rejection" }
  Entity revisionKey revision <- loadSecurityGrantRevision rawRevisionId
  stateCode <- workflowStateCode (M.securityGrantRevisionWorkflowStateId revision)
  unless (stateCode == "review") $
    throwError err409 { errBody = "Only security revisions in review can be rejected" }
  rejectedState <- loadSecurityWorkflowStateKey "rejected"
  now <- liftIO getCurrentTime
  runDB $ do
    update revisionKey
      [ M.SecurityGrantRevisionWorkflowStateId =. rejectedState
      , M.SecurityGrantRevisionReviewedBy =. Just (auPartyId user)
      , M.SecurityGrantRevisionReviewedAt =. Just now
      , M.SecurityGrantRevisionReviewerNotes =. Just notes
      , M.SecurityGrantRevisionRejectionReason =. Just notes
      , M.SecurityGrantRevisionApprovalMode =. "normal"
      , M.SecurityGrantRevisionEmergencyReason =. Nothing
      , M.SecurityGrantRevisionResult =. Just "rejected"
      , M.SecurityGrantRevisionVersion +=. 1
      ]
    insertSecurityAuditEvent revisionKey revision "rejected" Nothing
      (Just (M.securityGrantRevisionDesiredActive revision))
      (Just (M.securityGrantRevisionCreatedBy revision)) (Just (auPartyId user)) Nothing "normal" "success" now
  updated <- runDB (getJust revisionKey)
  securityRevisionDTO (Entity revisionKey updated)

applySecurityGrant
  :: M.SecurityGrantRevisionId
  -> M.SecurityGrantRevision
  -> AuthedUser
  -> Text
  -> Maybe Text
  -> UTCTime
  -> SqlPersistT IO ()
applySecurityGrant revisionKey revision user approvalMode emergencyReason now =
  case M.securityGrantRevisionChangeKind revision of
    "party-role" -> case M.securityGrantRevisionPartyId revision of
      Nothing -> liftIO . ioError . userError $ "party-role revision missing party"
      Just partyKey -> do
        current <- getBy (M.UniquePartySecurityRole partyKey (M.securityGrantRevisionRoleId revision))
        let currentVersion = maybe 0 (M.partySecurityRoleVersion . entityVal) current
        unless (currentVersion == M.securityGrantRevisionExpectedVersion revision) $
          liftIO . ioError . userError $ "security grant version conflict during approval"
        case current of
          Nothing -> insert_ M.PartySecurityRole
            { M.partySecurityRolePartyId = partyKey
            , M.partySecurityRoleRoleId = M.securityGrantRevisionRoleId revision
            , M.partySecurityRoleGrantedBy = Just (M.securityGrantRevisionCreatedBy revision)
            , M.partySecurityRoleApprovedBy = Just (auPartyId user)
            , M.partySecurityRoleApprovalMode = approvalMode
            , M.partySecurityRoleEmergencyReason = emergencyReason
            , M.partySecurityRoleSourceRevisionId = Just revisionKey
            , M.partySecurityRoleSourcePolicyId = Nothing
            , M.partySecurityRoleActive = M.securityGrantRevisionDesiredActive revision
            , M.partySecurityRoleCreatedAt = now
            , M.partySecurityRoleRevokedAt = if M.securityGrantRevisionDesiredActive revision then Nothing else Just now
            , M.partySecurityRoleVersion = 1
            }
          Just (Entity assignmentKey _) -> update assignmentKey
            [ M.PartySecurityRoleGrantedBy =. Just (M.securityGrantRevisionCreatedBy revision)
            , M.PartySecurityRoleApprovedBy =. Just (auPartyId user)
            , M.PartySecurityRoleApprovalMode =. approvalMode
            , M.PartySecurityRoleEmergencyReason =. emergencyReason
            , M.PartySecurityRoleSourceRevisionId =. Just revisionKey
            , M.PartySecurityRoleSourcePolicyId =. Nothing
            , M.PartySecurityRoleActive =. M.securityGrantRevisionDesiredActive revision
            , M.PartySecurityRoleRevokedAt =. if M.securityGrantRevisionDesiredActive revision then Nothing else Just now
            , M.PartySecurityRoleVersion +=. 1
            ]
    "role-permission" -> case M.securityGrantRevisionPermissionId revision of
      Nothing -> liftIO . ioError . userError $ "role-permission revision missing permission"
      Just permissionKey -> do
        current <- getBy (M.UniqueRolePermission (M.securityGrantRevisionRoleId revision) permissionKey)
        let currentVersion = maybe 0 (M.rolePermissionVersion . entityVal) current
        unless (currentVersion == M.securityGrantRevisionExpectedVersion revision) $
          liftIO . ioError . userError $ "security grant version conflict during approval"
        case current of
          Nothing -> insert_ M.RolePermission
            { M.rolePermissionRoleId = M.securityGrantRevisionRoleId revision
            , M.rolePermissionPermissionId = permissionKey
            , M.rolePermissionGrantedBy = Just (M.securityGrantRevisionCreatedBy revision)
            , M.rolePermissionApprovedBy = Just (auPartyId user)
            , M.rolePermissionApprovalMode = approvalMode
            , M.rolePermissionEmergencyReason = emergencyReason
            , M.rolePermissionSourceRevisionId = Just revisionKey
            , M.rolePermissionActive = M.securityGrantRevisionDesiredActive revision
            , M.rolePermissionCreatedAt = now
            , M.rolePermissionRevokedAt = if M.securityGrantRevisionDesiredActive revision then Nothing else Just now
            , M.rolePermissionVersion = 1
            }
          Just (Entity grantKey _) -> update grantKey
            [ M.RolePermissionGrantedBy =. Just (M.securityGrantRevisionCreatedBy revision)
            , M.RolePermissionApprovedBy =. Just (auPartyId user)
            , M.RolePermissionApprovalMode =. approvalMode
            , M.RolePermissionEmergencyReason =. emergencyReason
            , M.RolePermissionSourceRevisionId =. Just revisionKey
            , M.RolePermissionActive =. M.securityGrantRevisionDesiredActive revision
            , M.RolePermissionRevokedAt =. if M.securityGrantRevisionDesiredActive revision then Nothing else Just now
            , M.RolePermissionVersion +=. 1
            ]
    _ -> liftIO . ioError . userError $ "unknown security grant revision kind"

loadSecurityGrantCurrent
  :: Text
  -> Maybe PartyId
  -> M.SecurityRoleId
  -> Maybe M.SecurityPermissionId
  -> AppM (Maybe (Bool, Int))
loadSecurityGrantCurrent "party-role" (Just partyKey) roleKey Nothing = do
  row <- runDB $ getBy (M.UniquePartySecurityRole partyKey roleKey)
  pure $ (\value -> (M.partySecurityRoleActive value, M.partySecurityRoleVersion value)) . entityVal <$> row
loadSecurityGrantCurrent "role-permission" Nothing roleKey (Just permissionKey) = do
  row <- runDB $ getBy (M.UniqueRolePermission roleKey permissionKey)
  pure $ (\value -> (M.rolePermissionActive value, M.rolePermissionVersion value)) . entityVal <$> row
loadSecurityGrantCurrent _ _ _ _ =
  throwError err422 { errBody = "Invalid typed security grant target" }

loadSecurityGrantRevision :: Text -> AppM (Entity M.SecurityGrantRevision)
loadSecurityGrantRevision rawRevisionId = do
  revisionUuid <- parseUuid "revisionId" rawRevisionId
  let revisionKey = M.SecurityGrantRevisionKey revisionUuid
  maybe (throwError err404 { errBody = "Security grant revision not found" }) pure
    =<< runDB (getEntity revisionKey)

securityRoleKeyFromText :: Text -> AppM M.SecurityRoleId
securityRoleKeyFromText rawRoleId = do
  roleUuid <- parseUuid "roleId" rawRoleId
  let roleKey = M.SecurityRoleKey roleUuid
  role <- maybe (throwError err404 { errBody = "Security role not found" }) pure
    =<< runDB (get roleKey)
  unless (M.securityRoleActive role) $
    throwError err409 { errBody = "Inactive security roles cannot receive new changes" }
  pure roleKey

securityPermissionKeyFromText :: Text -> AppM M.SecurityPermissionId
securityPermissionKeyFromText rawPermissionId = do
  permissionUuid <- parseUuid "permissionId" rawPermissionId
  let permissionKey = M.SecurityPermissionKey permissionUuid
  permission <- maybe (throwError err404 { errBody = "Security permission not found" }) pure
    =<< runDB (get permissionKey)
  unless (M.securityPermissionActive permission) $
    throwError err409 { errBody = "Inactive security permissions cannot receive new grants" }
  pure permissionKey

loadSecurityWorkflowStateKey :: Text -> AppM M.WorkflowStateId
loadSecurityWorkflowStateKey stateCode = do
  workflow <- runDB $ getBy (M.UniqueWorkflowDefinitionCode "sensitive-publication")
  Entity workflowKey workflowValue <- maybe
    (throwError err500 { errBody = "Sensitive security workflow is missing" }) pure workflow
  unless (M.workflowDefinitionActive workflowValue) $
    throwError err500 { errBody = "Sensitive security workflow is inactive" }
  state <- runDB $ getBy (M.UniqueWorkflowStateCode workflowKey stateCode)
  Entity stateKey stateValue <- maybe
    (throwError err400 { errBody = "Unknown security workflow state" }) pure state
  unless (M.workflowStateActive stateValue) $
    throwError err409 { errBody = "Security workflow state is inactive" }
  pure stateKey

isActiveEmergencyAdministrator :: PartyId -> AppM Bool
isActiveEmergencyAdministrator partyKey = do
  rows <- runDB $ rawSql
    "SELECT security_is_coherent_emergency_administrator(?)"
    [toPersistValue partyKey]
  pure (rows == [Single True])

securityRevisionDTOs :: [Entity M.SecurityGrantRevision] -> AppM [SecurityGrantRevisionDTO]
securityRevisionDTOs revisions = do
  stateRows <- runDB $ selectList [] [Asc M.WorkflowStateSortOrder]
  let states = Map.fromList [(stateKey, M.workflowStateCode stateValue) | Entity stateKey stateValue <- stateRows]
  forM revisions $ \entity@(Entity _ revision) ->
    case Map.lookup (M.securityGrantRevisionWorkflowStateId revision) states of
      Nothing -> throwError err500 { errBody = "Security revision references a missing workflow state" }
      Just stateCode -> pure (securityRevisionDTOWithState stateCode entity)

securityRevisionDTO :: Entity M.SecurityGrantRevision -> AppM SecurityGrantRevisionDTO
securityRevisionDTO entity@(Entity _ revision) = do
  stateCode <- workflowStateCode (M.securityGrantRevisionWorkflowStateId revision)
  pure (securityRevisionDTOWithState stateCode entity)

securityRevisionDTOWithState :: Text -> Entity M.SecurityGrantRevision -> SecurityGrantRevisionDTO
securityRevisionDTOWithState stateCode (Entity revisionKey revision) = SecurityGrantRevisionDTO
  { sgrId = persistKeyText revisionKey
  , sgrChangeKind = M.securityGrantRevisionChangeKind revision
  , sgrPartyId = fromSqlKey <$> M.securityGrantRevisionPartyId revision
  , sgrRoleId = persistKeyText (M.securityGrantRevisionRoleId revision)
  , sgrPermissionId = persistKeyText <$> M.securityGrantRevisionPermissionId revision
  , sgrDesiredActive = M.securityGrantRevisionDesiredActive revision
  , sgrExpectedVersion = M.securityGrantRevisionExpectedVersion revision
  , sgrWorkflowState = stateCode
  , sgrCreatedBy = fromSqlKey (M.securityGrantRevisionCreatedBy revision)
  , sgrCreatedAt = M.securityGrantRevisionCreatedAt revision
  , sgrSubmittedAt = M.securityGrantRevisionSubmittedAt revision
  , sgrReviewedBy = fromSqlKey <$> M.securityGrantRevisionReviewedBy revision
  , sgrReviewedAt = M.securityGrantRevisionReviewedAt revision
  , sgrApprovedBy = fromSqlKey <$> M.securityGrantRevisionApprovedBy revision
  , sgrApprovedAt = M.securityGrantRevisionApprovedAt revision
  , sgrReviewerNotes = M.securityGrantRevisionReviewerNotes revision
  , sgrRejectionReason = M.securityGrantRevisionRejectionReason revision
  , sgrApprovalMode = M.securityGrantRevisionApprovalMode revision
  , sgrEmergencyReason = M.securityGrantRevisionEmergencyReason revision
  , sgrSourcePlatform = M.securityGrantRevisionSourcePlatform revision
  , sgrCorrelationId = M.securityGrantRevisionCorrelationId revision
  , sgrReason = M.securityGrantRevisionReason revision
  , sgrResult = M.securityGrantRevisionResult revision
  , sgrVersion = M.securityGrantRevisionVersion revision
  }

securityAuditDTO :: Entity M.SecurityAuditEvent -> SecurityAuditEventDTO
securityAuditDTO (Entity auditKey audit) = SecurityAuditEventDTO
  { saeId = persistKeyText auditKey
  , saeRevisionId = persistKeyText <$> M.securityAuditEventRevisionId audit
  , saeSourcePolicyId = persistKeyText <$> M.securityAuditEventSourcePolicyId audit
  , saeEntityKind = M.securityAuditEventEntityKind audit
  , saePartyId = fromSqlKey <$> M.securityAuditEventPartyId audit
  , saeRoleId = persistKeyText (M.securityAuditEventRoleId audit)
  , saePermissionId = persistKeyText <$> M.securityAuditEventPermissionId audit
  , saeOperation = M.securityAuditEventOperation audit
  , saePreviousActive = M.securityAuditEventPreviousActive audit
  , saeNewActive = M.securityAuditEventNewActive audit
  , saeActorId = fromSqlKey <$> M.securityAuditEventActorId audit
  , saeReviewerId = fromSqlKey <$> M.securityAuditEventReviewerId audit
  , saeApproverId = fromSqlKey <$> M.securityAuditEventApproverId audit
  , saeOccurredAt = M.securityAuditEventOccurredAt audit
  , saeSourcePlatform = M.securityAuditEventSourcePlatform audit
  , saeReason = M.securityAuditEventReason audit
  , saeCorrelationId = M.securityAuditEventCorrelationId audit
  , saeApprovalMode = M.securityAuditEventApprovalMode audit
  , saeResult = M.securityAuditEventResult audit
  }

insertSecurityAuditEvent
  :: M.SecurityGrantRevisionId
  -> M.SecurityGrantRevision
  -> Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe PartyId
  -> Maybe PartyId
  -> Maybe PartyId
  -> Text
  -> Text
  -> UTCTime
  -> SqlPersistT IO ()
insertSecurityAuditEvent revisionKey revision operation previousActive newActive actor reviewer approver approvalMode result now =
  insert_ M.SecurityAuditEvent
    { M.securityAuditEventRevisionId = Just revisionKey
    , M.securityAuditEventSourcePolicyId = Nothing
    , M.securityAuditEventEntityKind = M.securityGrantRevisionChangeKind revision
    , M.securityAuditEventPartyId = M.securityGrantRevisionPartyId revision
    , M.securityAuditEventRoleId = M.securityGrantRevisionRoleId revision
    , M.securityAuditEventPermissionId = M.securityGrantRevisionPermissionId revision
    , M.securityAuditEventOperation = operation
    , M.securityAuditEventPreviousActive = previousActive
    , M.securityAuditEventNewActive = newActive
    , M.securityAuditEventActorId = actor
    , M.securityAuditEventReviewerId = reviewer
    , M.securityAuditEventApproverId = approver
    , M.securityAuditEventOccurredAt = now
    , M.securityAuditEventSourcePlatform = M.securityGrantRevisionSourcePlatform revision
    , M.securityAuditEventReason = Just (M.securityGrantRevisionReason revision)
    , M.securityAuditEventCorrelationId = M.securityGrantRevisionCorrelationId revision
    , M.securityAuditEventApprovalMode = approvalMode
    , M.securityAuditEventResult = result
    }

validateSecurityIdentifier :: Text -> Int -> Int -> Text -> AppM Text
validateSecurityIdentifier fieldName minimumLength maximumLength raw = do
  let value = T.strip raw
      validChar character = isAlphaNum character || character `elem` ("-_.:" :: String)
  when (T.length value < minimumLength || T.length value > maximumLength || not (T.all validChar value)) $
    throwError err400
      { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " has an invalid length or character"))
      }
  pure value

validateSecurityAuditText :: Text -> Int -> Int -> Text -> AppM Text
validateSecurityAuditText fieldName minimumLength maximumLength raw = do
  let value = T.strip raw
  when (T.length value < minimumLength || T.length value > maximumLength || T.any isControl value) $
    throwError err400
      { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " has an invalid length or control character"))
      }
  pure value

validateUuidText :: Text -> Text -> AppM Text
validateUuidText fieldName raw = UUID.toText <$> parseUuid fieldName raw

parseUuid :: Text -> Text -> AppM UUID.UUID
parseUuid fieldName raw =
  case UUID.fromText (T.strip raw) of
    Just value | value /= UUID.nil && UUID.toText value == raw -> pure value
    _ -> throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 ("Invalid canonical UUID for " <> fieldName)) }

persistKeyText :: PersistField key => key -> Text
persistKeyText key =
  case toPersistValue key of
    PersistText value -> value
    PersistLiteral_ _ value -> TE.decodeUtf8 value
    PersistByteString value -> TE.decodeUtf8 value
    other -> T.pack (show other)

keyUuid :: PersistField key => key -> UUID.UUID
keyUuid = fromMaybe UUID.nil . UUID.fromText . persistKeyText

keyInt64 :: PartyId -> Int64
keyInt64 = fromSqlKey

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (value : _) = Just value
