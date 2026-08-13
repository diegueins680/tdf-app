{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.Catalog.Models where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Database.Persist.TH
import GHC.Generics (Generic)
import TDF.CMS.Models (AesonValue)
import TDF.Models (PartyId, ResourceId)
import TDF.UUIDInstances ()

-- Catalog entities keep domain-specific identities and foreign keys. The
-- shared tables below are limited to governance, translations/search aliases,
-- URL aliases, imports, audit, cache revisions, defaults, and usage metrics.
share [mkPersist sqlSettings, mkMigrate "migrateCatalogGovernance"] [persistLowerCase|
WorkflowDefinition sql=workflow_definition
    Id UUID default=gen_random_uuid()
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sensitive Bool default=False
    publicRead Bool default=False
    cacheRevision Int64 default=1
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueWorkflowDefinitionCode code
    deriving Show Generic
WorkflowState sql=workflow_state
    Id UUID default=gen_random_uuid()
    workflowId WorkflowDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sortOrder Int default=0
    terminal Bool default=False
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueWorkflowStateCode workflowId code
    deriving Show Generic
WorkflowTransition sql=workflow_transition
    Id UUID default=gen_random_uuid()
    workflowId WorkflowDefinitionId
    fromStateId WorkflowStateId
    toStateId WorkflowStateId
    requiredPermissionId UUID Maybe
    requiresReview Bool default=False
    requiresDistinctApprover Bool default=False
    effectiveFrom UTCTime Maybe
    effectiveUntil UTCTime Maybe
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    createdBy PartyId Maybe
    version Int default=1
    UniqueWorkflowTransition workflowId fromStateId toStateId
    deriving Show Generic
WorkflowDefaultState sql=workflow_default_state
    Id UUID default=gen_random_uuid()
    workflowId WorkflowDefinitionId
    stateId WorkflowStateId
    context Text default='initial'
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    deriving Show Generic
WorkflowStateCapability sql=workflow_state_capability
    Id UUID default=gen_random_uuid()
    stateId WorkflowStateId
    capabilityCode Text
    enabled Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueWorkflowStateCapability stateId capabilityCode
    deriving Show Generic
CatalogDefinition sql=catalog_definition
    Id UUID default=gen_random_uuid()
    code Text
    classification Text
    entityKind Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    publicRead Bool default=False
    sensitive Bool default=False
    orderingMode Text default='manual'
    workflowId WorkflowDefinitionId
    sourceName Text Maybe
    sourceVersion Text Maybe
    sourceEffectiveDate Day Maybe
    lastSyncedAt UTCTime Maybe
    cacheRevision Int64 default=0
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueCatalogDefinitionCode code
    UniqueCatalogDefinitionEntityKind entityKind
    deriving Show Generic
CatalogRevision sql=catalog_revision
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    entityId UUID
    workflowStateId WorkflowStateId
    baseVersion Int
    proposedVersion Int
    previousValues AesonValue Maybe
    newValues AesonValue
    createdBy PartyId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    submittedAt UTCTime Maybe
    reviewedBy PartyId Maybe
    reviewedAt UTCTime Maybe
    approvedBy PartyId Maybe
    approvedAt UTCTime Maybe
    reviewerNotes Text Maybe
    rejectionReason Text Maybe
    scheduledPublishAt UTCTime Maybe
    publishedAt UTCTime Maybe
    sourcePlatform Text
    correlationId Text
    reason Text Maybe
    result Text Maybe
    importJobId UUID Maybe
    UniqueCatalogEntityRevision catalogId entityId proposedVersion
    deriving Show Generic
CatalogAuditEvent sql=catalog_audit_event
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    entityId UUID
    revisionId CatalogRevisionId Maybe
    operation Text
    previousValues AesonValue Maybe
    newValues AesonValue Maybe
    actorId PartyId Maybe
    reviewerId PartyId Maybe
    approverId PartyId Maybe
    occurredAt UTCTime default=CURRENT_TIMESTAMP
    sourcePlatform Text
    importJobId UUID Maybe
    reason Text Maybe
    correlationId Text
    result Text
    affectedRelationships AesonValue Maybe
    deriving Show Generic
CatalogImportJob sql=catalog_import_job
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    sourceName Text
    sourceVersion Text default=''
    originalFilename Text Maybe
    contentSha256 Text
    dryRun Bool default=True
    status Text
    totalRows Int default=0
    acceptedRows Int default=0
    rejectedRows Int default=0
    ambiguousRows Int default=0
    errorReport AesonValue Maybe
    requestedBy PartyId
    reviewedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    completedAt UTCTime Maybe
    correlationId Text
    UniqueCatalogImportDigest catalogId contentSha256 sourceVersion
    deriving Show Generic
CatalogImportReviewEntry sql=catalog_import_review_entry
    Id UUID default=gen_random_uuid()
    importJobId CatalogImportJobId
    sourceRow Int
    originalValue AesonValue
    candidateMatches AesonValue
    status Text
    resolution AesonValue Maybe
    resolvedBy PartyId Maybe
    resolvedAt UTCTime Maybe
    evidence Text Maybe
    UniqueCatalogImportReviewRow importJobId sourceRow
    deriving Show Generic
CatalogBackfillRun sql=catalog_backfill_run
    Id UUID default=gen_random_uuid()
    runCode Text
    candidateRevision Text
    dryRun Bool default=True
    status Text
    safetyThreshold Int default=0
    scannedRows Int default=0
    mappedRows Int default=0
    ambiguousRows Int default=0
    rejectedRows Int default=0
    startedBy PartyId Maybe
    startedAt UTCTime default=CURRENT_TIMESTAMP
    completedAt UTCTime Maybe
    report AesonValue Maybe
    correlationId Text
    UniqueCatalogBackfillRun runCode candidateRevision dryRun
    deriving Show Generic
CatalogMigrationMapping sql=catalog_migration_mapping
    Id UUID default=gen_random_uuid()
    runId CatalogBackfillRunId
    sourceTable Text
    sourceColumn Text
    sourceRecordId Text default=''
    originalValue Text
    normalizedValue Text
    catalogId CatalogDefinitionId
    entityId UUID Maybe
    status Text
    evidence Text Maybe
    sourceCount Int64 default=0
    reviewedBy PartyId Maybe
    resolvedAt UTCTime Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    UniqueCatalogMigrationSource runId sourceTable sourceColumn sourceRecordId originalValue
    deriving Show Generic
WorkflowMigrationMapping sql=workflow_migration_mapping
    Id UUID default=gen_random_uuid()
    runId CatalogBackfillRunId
    workflowId WorkflowDefinitionId
    sourceTable Text
    sourceColumn Text
    sourceRecordId Text default=''
    originalValue Text
    normalizedValue Text
    stateId WorkflowStateId Maybe
    status Text
    evidence Text Maybe
    sourceCount Int64 default=0
    reviewedBy PartyId Maybe
    resolvedAt UTCTime Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    UniqueWorkflowMigrationSource runId sourceTable sourceColumn sourceRecordId originalValue
    deriving Show Generic
CatalogSlugAlias sql=catalog_slug_alias
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    entityKind Text
    entityId UUID
    scope Text
    slug Text
    current Bool default=False
    redirectStatus Int default=308
    createdBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    retiredAt UTCTime Maybe
    UniqueCatalogSlugScope scope slug
    deriving Show Generic
CatalogSearchAlias sql=catalog_search_alias
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    entityKind Text
    entityId UUID
    localeId UUID
    term Text
    normalizedTerm Text
    source Text default='manual'
    createdAt UTCTime default=CURRENT_TIMESTAMP
    UniqueCatalogSearchAlias catalogId entityId localeId normalizedTerm
    deriving Show Generic
CatalogScopedDefault sql=catalog_scoped_default
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    entityId UUID
    scopeKind Text
    scopeId Text
    localeId UUID Maybe
    effectiveFrom UTCTime Maybe
    effectiveUntil UTCTime Maybe
    active Bool default=True
    createdBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    deriving Show Generic
CatalogDependencyRule sql=catalog_dependency_rule
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    subjectEntityId UUID
    predicateKind Text
    relatedCatalogId CatalogDefinitionId Maybe
    relatedEntityId UUID Maybe
    contextKind Text
    contextId Text Maybe
    effectiveFrom UTCTime Maybe
    effectiveUntil UTCTime Maybe
    priority Int default=0
    active Bool default=True
    createdBy PartyId
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    deriving Show Generic
CatalogMergeOperation sql=catalog_merge_operation
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    sourceEntityId UUID
    targetEntityId UUID
    status Text
    reason Text
    affectedReferences AesonValue Maybe
    requestedBy PartyId
    approvedBy PartyId Maybe
    executedAt UTCTime Maybe
    reversedAt UTCTime Maybe
    reversedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    correlationId Text
    deriving Show Generic
CatalogUsageDaily sql=catalog_usage_daily
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    entityId UUID Maybe
    day Day
    selectionCount Int64 default=0
    historicalReferenceCount Int64 default=0
    replacementCount Int64 default=0
    noResultSearchCount Int64 default=0
    formFailureCount Int64 default=0
    UniqueCatalogUsageDay catalogId entityId day !force
    deriving Show Generic
|]

share [mkPersist sqlSettings, mkMigrate "migrateCatalogSecurity"] [persistLowerCase|
SecurityModule sql=security_module
    Id UUID default=gen_random_uuid()
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sortOrder Int default=0
    active Bool default=True
    internalOnly Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueSecurityModuleCode code
    deriving Show Generic
SecurityAction sql=security_action
    Id UUID default=gen_random_uuid()
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sensitive Bool default=False
    grantable Bool default=True
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueSecurityActionCode code
    deriving Show Generic
SecurityPermission sql=security_permission
    Id UUID default=gen_random_uuid()
    code Text
    moduleId SecurityModuleId
    actionId SecurityActionId
    resourceScope Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sensitive Bool default=False
    publicMetadata Bool default=False
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueSecurityPermissionCode code
    UniqueSecurityPermissionTuple moduleId actionId resourceScope
    deriving Show Generic
SecurityRole sql=security_role
    Id UUID default=gen_random_uuid()
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sortOrder Int default=0
    systemRole Bool default=False
    emergencyAdministrator Bool default=False
    selfAssignable Bool default=False
    automaticAssignable Bool default=False
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    publishedRevision Int default=1
    version Int default=1
    UniqueSecurityRoleCode code
    deriving Show Generic
SecurityRoleAssignmentPolicy sql=security_role_assignment_policy
    Id UUID default=gen_random_uuid()
    code Text
    triggerCode Text
    roleId SecurityRoleId
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    requiresVerifiedEmail Bool default=False
    active Bool default=True
    effectiveFrom UTCTime Maybe
    effectiveTo UTCTime Maybe
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueSecurityRoleAssignmentPolicyCode code
    UniqueSecurityRoleAssignmentPolicyTriggerRole triggerCode roleId
    deriving Show Generic
SecurityGrantRevision sql=security_grant_revision
    Id UUID default=gen_random_uuid()
    changeKind Text
    partyId PartyId Maybe
    roleId SecurityRoleId
    permissionId SecurityPermissionId Maybe
    desiredActive Bool
    expectedVersion Int
    workflowStateId WorkflowStateId
    createdBy PartyId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    submittedAt UTCTime Maybe
    reviewedBy PartyId Maybe
    reviewedAt UTCTime Maybe
    approvedBy PartyId Maybe
    approvedAt UTCTime Maybe
    reviewerNotes Text Maybe
    rejectionReason Text Maybe
    approvalMode Text default='normal'
    emergencyReason Text Maybe
    sourcePlatform Text
    correlationId Text
    reason Text
    result Text Maybe
    version Int default=1
    UniqueSecurityGrantCorrelation correlationId
    deriving Show Generic
RolePermission sql=role_permission
    Id UUID default=gen_random_uuid()
    roleId SecurityRoleId
    permissionId SecurityPermissionId
    grantedBy PartyId Maybe
    approvedBy PartyId Maybe
    approvalMode Text default='bootstrap'
    emergencyReason Text Maybe
    sourceRevisionId SecurityGrantRevisionId Maybe
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    revokedAt UTCTime Maybe
    version Int default=1
    UniqueRolePermission roleId permissionId
    deriving Show Generic
PartySecurityRole sql=party_security_role
    Id UUID default=gen_random_uuid()
    partyId PartyId
    roleId SecurityRoleId
    grantedBy PartyId Maybe
    approvedBy PartyId Maybe
    approvalMode Text default='bootstrap'
    emergencyReason Text Maybe
    sourceRevisionId SecurityGrantRevisionId Maybe
    sourcePolicyId SecurityRoleAssignmentPolicyId Maybe
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    revokedAt UTCTime Maybe
    version Int default=1
    UniquePartySecurityRole partyId roleId
    deriving Show Generic
SecurityAuditEvent sql=security_audit_event
    Id UUID default=gen_random_uuid()
    revisionId SecurityGrantRevisionId Maybe
    sourcePolicyId SecurityRoleAssignmentPolicyId Maybe
    entityKind Text
    partyId PartyId Maybe
    roleId SecurityRoleId
    permissionId SecurityPermissionId Maybe
    operation Text
    previousActive Bool Maybe
    newActive Bool Maybe
    actorId PartyId Maybe
    reviewerId PartyId Maybe
    approverId PartyId Maybe
    occurredAt UTCTime default=CURRENT_TIMESTAMP
    sourcePlatform Text
    reason Text Maybe
    correlationId Text
    approvalMode Text
    result Text
    deriving Show Generic
|]

share [mkPersist sqlSettings, mkMigrate "migrateCatalogReferences"] [persistLowerCase|
CountryReference sql=country_reference
    Id UUID default=gen_random_uuid()
    alpha2 Text
    alpha3 Text
    numericCode Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    standard Text default='ISO 3166-1'
    sourceVersion Text
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId CountryReferenceId Maybe
    lastSyncedAt UTCTime
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueCountryReferenceAlpha2 alpha2
    UniqueCountryReferenceAlpha3 alpha3
    UniqueCountryReferenceNumeric numericCode
    deriving Show Generic
SubdivisionReference sql=subdivision_reference
    Id UUID default=gen_random_uuid()
    countryId CountryReferenceId
    code Text
    subdivisionType Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sourceVersion Text
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId SubdivisionReferenceId Maybe
    lastSyncedAt UTCTime
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueSubdivisionReferenceCode countryId code
    deriving Show Generic
CityReference sql=city_reference
    Id UUID default=gen_random_uuid()
    countryId CountryReferenceId
    subdivisionId SubdivisionReferenceId Maybe
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    timezone Text Maybe
    latitude Double Maybe
    longitude Double Maybe
    sourceName Text
    sourceVersion Text Maybe
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId CityReferenceId Maybe
    lastSyncedAt UTCTime Maybe
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueCityReferenceCode countryId code
    deriving Show Generic
CurrencyReference sql=currency_reference
    Id UUID default=gen_random_uuid()
    code Text
    numericCode Text Maybe
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    symbol Text
    minorUnits Int
    standard Text default='ISO 4217'
    sourceVersion Text
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId CurrencyReferenceId Maybe
    lastSyncedAt UTCTime
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueCurrencyReferenceCode code
    deriving Show Generic
TaxRateReference sql=tax_rate_reference
    Id UUID default=gen_random_uuid()
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    rateBps Int
    countryId CountryReferenceId Maybe
    standard Text
    sourceVersion Text
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId TaxRateReferenceId Maybe
    lastSyncedAt UTCTime
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueTaxRateReferenceCode code
    deriving Show Generic
LanguageReference sql=language_reference
    Id UUID default=gen_random_uuid()
    iso6391 Text Maybe
    iso6392T Text
    iso6392B Text Maybe
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    standard Text default='ISO 639'
    sourceVersion Text
    deprecatedAt UTCTime Maybe
    replacementId LanguageReferenceId Maybe
    lastSyncedAt UTCTime
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueLanguageReference6391 iso6391 !force
    UniqueLanguageReference6392T iso6392T
    deriving Show Generic
LocaleReference sql=locale_reference
    Id UUID default=gen_random_uuid()
    code Text
    languageId LanguageReferenceId
    countryId CountryReferenceId Maybe
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    fallbackLocaleId LocaleReferenceId Maybe
    defaultForPlatform Bool default=False
    sourceVersion Text
    lastSyncedAt UTCTime
    deprecatedAt UTCTime Maybe
    replacementId LocaleReferenceId Maybe
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueLocaleReferenceCode code
    deriving Show Generic
DeploymentCurrencyEnablement sql=deployment_currency_enablement
    Id UUID default=gen_random_uuid()
    deploymentCode Text
    currencyId CurrencyReferenceId
    enabled Bool default=True
    defaultCurrency Bool default=False
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueDeploymentCurrency deploymentCode currencyId
    deriving Show Generic
DeploymentLocaleEnablement sql=deployment_locale_enablement
    Id UUID default=gen_random_uuid()
    deploymentCode Text
    localeId LocaleReferenceId
    enabled Bool default=True
    defaultLocale Bool default=False
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueDeploymentLocale deploymentCode localeId
    deriving Show Generic
ExternalProvider sql=external_provider
    Id UUID default=gen_random_uuid()
    internalCode Text sql=code
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    providerKind Text
    publicMetadata Bool default=False
    active Bool default=True
    sourceVersion Text Maybe
    lastSyncedAt UTCTime Maybe
    sortOrder Int default=0
    deprecatedAt UTCTime Maybe
    replacementId ExternalProviderId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueExternalProviderCode internalCode
    deriving Show Generic
ExternalProviderCode sql=external_provider_code
    Id UUID default=gen_random_uuid()
    providerId ExternalProviderId
    codeSet Text
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sourceVersion Text
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId ExternalProviderCodeId Maybe
    lastSyncedAt UTCTime
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueExternalProviderCodeValue providerId codeSet code
    deriving Show Generic
DdexStandardVersion sql=ddex_standard_version
    Id UUID default=gen_random_uuid()
    standardCode Text
    versionCode Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    namespaceUri Text Maybe
    schemaUri Text Maybe
    sourceUri Text
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId DdexStandardVersionId Maybe
    sourceVersion Text
    lastSyncedAt UTCTime
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueDdexStandardVersion standardCode versionCode
    deriving Show Generic
DdexStandardSupport sql=ddex_standard_support
    Id UUID default=gen_random_uuid()
    standardVersionId DdexStandardVersionId
    deploymentCode Text
    detectionEnabled Bool default=False
    validationEnabled Bool default=False
    importEnabled Bool default=False
    exportEnabled Bool default=False
    active Bool default=True
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueDdexStandardSupport standardVersionId deploymentCode
    deriving Show Generic
DdexMessageType sql=ddex_message_type
    Id UUID default=gen_random_uuid()
    standardVersionId DdexStandardVersionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sortOrder Int default=0
    active Bool default=True
    deprecatedAt UTCTime Maybe
    replacementId DdexMessageTypeId Maybe
    sourceVersion Text
    lastSyncedAt UTCTime
    runtimeSupported Bool default=False
    version Int default=1
    UniqueDdexMessageType standardVersionId code
    deriving Show Generic
DdexVocabulary sql=ddex_vocabulary
    Id UUID default=gen_random_uuid()
    standardVersionId DdexStandardVersionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sourceUri Text Maybe
    sourceVersion Text
    lastSyncedAt UTCTime
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId DdexVocabularyId Maybe
    active Bool default=True
    sortOrder Int default=0
    version Int default=1
    UniqueDdexVocabulary standardVersionId code
    deriving Show Generic
DdexCode sql=ddex_code
    Id UUID default=gen_random_uuid()
    vocabularyId DdexVocabularyId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sortOrder Int default=0
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId DdexCodeId Maybe
    sourceVersion Text
    lastSyncedAt UTCTime
    active Bool default=True
    version Int default=1
    UniqueDdexCode vocabularyId code
    deriving Show Generic
DdexCodeApplicability sql=ddex_code_applicability
    Id UUID default=gen_random_uuid()
    codeId DdexCodeId
    messageTypeId DdexMessageTypeId
    required Bool default=False
    active Bool default=True
    UniqueDdexCodeApplicability codeId messageTypeId
    deriving Show Generic
|]

share [mkPersist sqlSettings, mkMigrate "migrateCatalogDomains"] [persistLowerCase|
ReleaseTypeReference sql=release_type_reference
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    externalCode Text Maybe
    sourceVersion Text Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId ReleaseTypeReferenceId Maybe
    version Int default=1
    UniqueReleaseTypeReferenceCode code
    deriving Show Generic
RecordingTypeReference sql=recording_type_reference
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    externalCode Text Maybe
    sourceVersion Text Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId RecordingTypeReferenceId Maybe
    version Int default=1
    UniqueRecordingTypeReferenceCode code
    deriving Show Generic
RecordingSessionType sql=recording_session_type
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    externalCode Text Maybe
    sourceVersion Text Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId RecordingSessionTypeId Maybe
    version Int default=1
    UniqueRecordingSessionTypeCode code
    deriving Show Generic
ServiceCategory sql=service_category
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    parentId ServiceCategoryId Maybe
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId ServiceCategoryId Maybe
    version Int default=1
    UniqueServiceCategoryCode code
    UniqueServiceCategorySlug currentSlug !force
    deriving Show Generic
ServicePricingModel sql=service_pricing_model
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId ServicePricingModelId Maybe
    version Int default=1
    UniqueServicePricingModelCode code
    UniqueServicePricingModelSlug currentSlug !force
    deriving Show Generic
ServiceOffering sql=service_offering
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    categoryId ServiceCategoryId
    legacyServiceCatalogId Int64 Maybe
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    pricingModelId ServicePricingModelId Maybe
    legacyPricingModelCode Text Maybe sql=pricing_model_code
    defaultRateCents Int Maybe
    taxRateId TaxRateReferenceId Maybe
    legacyTaxRateCode Text Maybe sql=tax_rate_code
    currencyId CurrencyReferenceId
    billingUnitEs Text Maybe
    billingUnitEn Text Maybe
    defaultDurationMinutes Int Maybe
    requiresEngineer Bool default=False
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    deprecatedAt UTCTime Maybe
    replacementId ServiceOfferingId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueServiceOfferingCode code
    UniqueServiceOfferingSlug currentSlug !force
    UniqueServiceOfferingLegacyId legacyServiceCatalogId !force
    deriving Show Generic
PipelineWorkflowBinding sql=pipeline_workflow_binding
    Id UUID default=gen_random_uuid()
    serviceOfferingId ServiceOfferingId
    workflowId WorkflowDefinitionId
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniquePipelineWorkflowService serviceOfferingId
    deriving Show Generic
ServiceResourceSelectionMode sql=service_resource_selection_mode
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId ServiceResourceSelectionModeId Maybe
    version Int default=1
    UniqueServiceResourceSelectionModeCode code
    UniqueServiceResourceSelectionModeSlug currentSlug !force
    deriving Show Generic
RadioAutoStopOption sql=radio_auto_stop_option
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    durationMinutes Int
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId RadioAutoStopOptionId Maybe
    version Int default=1
    UniqueRadioAutoStopOptionCode code
    UniqueRadioAutoStopOptionDuration durationMinutes
    UniqueRadioAutoStopOptionSlug currentSlug !force
    deriving Show Generic
AppearanceModeOption sql=appearance_mode_option
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId AppearanceModeOptionId Maybe
    version Int default=1
    UniqueAppearanceModeOptionCode code
    UniqueAppearanceModeOptionSlug currentSlug !force
    deriving Show Generic
ServiceOfferingDefaultResource sql=service_offering_default_resource
    Id UUID default=gen_random_uuid()
    serviceOfferingId ServiceOfferingId
    resourceId ResourceId
    selectionModeId ServiceResourceSelectionModeId Maybe
    legacySelectionModeCode Text Maybe sql=selection_mode
    sortOrder Int default=0
    active Bool default=True
    version Int default=1
    UniqueServiceOfferingDefaultResource serviceOfferingId resourceId
    deriving Show Generic
Genre sql=genre
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    parentId GenreId Maybe
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    publishedRevision Int default=0
    deprecatedAt UTCTime Maybe
    replacementId GenreId Maybe
    externalCode Text Maybe
    externalSource Text Maybe
    sourceVersion Text Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueGenreCode code
    UniqueGenreSlug currentSlug !force
    deriving Show Generic
Instrument sql=instrument
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    parentId InstrumentId Maybe
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    publishedRevision Int default=0
    deprecatedAt UTCTime Maybe
    replacementId InstrumentId Maybe
    externalCode Text Maybe
    externalSource Text Maybe
    sourceVersion Text Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueInstrumentCode code
    UniqueInstrumentSlug currentSlug !force
    deriving Show Generic
EventType sql=event_type
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    publishedRevision Int default=0
    deprecatedAt UTCTime Maybe
    replacementId EventTypeId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueEventTypeCode code
    UniqueEventTypeSlug currentSlug !force
    deriving Show Generic
BookingType sql=booking_type
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    serviceOfferingId ServiceOfferingId Maybe
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    publishedRevision Int default=0
    deprecatedAt UTCTime Maybe
    replacementId BookingTypeId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueBookingTypeCode code
    UniqueBookingTypeSlug currentSlug !force
    deriving Show Generic
FeedbackCategory sql=feedback_category
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    publishedRevision Int default=0
    deprecatedAt UTCTime Maybe
    replacementId FeedbackCategoryId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueFeedbackCategoryCode code
    UniqueFeedbackCategorySlug currentSlug !force
    deriving Show Generic
FeedbackSeverity sql=feedback_severity
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    effectiveFrom Day Maybe
    effectiveUntil Day Maybe
    publishedRevision Int default=0
    deprecatedAt UTCTime Maybe
    replacementId FeedbackSeverityId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueFeedbackSeverityCode code
    UniqueFeedbackSeveritySlug currentSlug !force
    deriving Show Generic
ContentCategory sql=content_category
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    parentId ContentCategoryId Maybe
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId ContentCategoryId Maybe
    version Int default=1
    UniqueContentCategoryCode code
    UniqueContentCategorySlug currentSlug !force
    deriving Show Generic
Tag sql=tag
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId TagId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueTagCode code
    UniqueTagSlug currentSlug !force
    deriving Show Generic
ReactionType sql=reaction_type
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    emoji Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    deprecatedAt UTCTime Maybe
    replacementId ReactionTypeId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueReactionTypeCode code
    UniqueReactionTypeSlug currentSlug !force
    deriving Show Generic
ContentType sql=content_type
    Id UUID default=gen_random_uuid()
    code Text
    entityKind Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    schemaJson AesonValue
    schemaVersion Int
    publicRoutePattern Text Maybe
    adminRoutePattern Text Maybe
    publicRead Bool default=False
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueContentTypeCode code
    deriving Show Generic
AuthoredContent sql=authored_content
    Id UUID default=gen_random_uuid()
    contentTypeId ContentTypeId
    code Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text
    publicRoute Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    publishedRevision Int default=1
    version Int default=1
    UniqueAuthoredContentCode code
    UniqueAuthoredContentSlug currentSlug
    deriving Show Generic
RecordRelease sql=record_release
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    releaseTypeId ReleaseTypeReferenceId
    titleEs Text
    titleEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    releaseDate Day Maybe
    currentSlug Text Maybe
    coverAssetId UUID Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    publishedRevision Int default=0
    deprecatedAt UTCTime Maybe
    replacementId RecordReleaseId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueRecordReleaseCode code
    UniqueRecordReleaseSlug currentSlug !force
    deriving Show Generic
Recording sql=recording
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    recordingTypeId RecordingTypeReferenceId
    titleEs Text
    titleEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    durationMs Int Maybe
    languageId LanguageReferenceId Maybe
    explicitContent Bool default=False
    currentSlug Text Maybe
    imageAssetId UUID Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    publishedRevision Int default=0
    deprecatedAt UTCTime Maybe
    replacementId RecordingId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueRecordingCode code
    UniqueRecordingSlug currentSlug !force
    deriving Show Generic
RecordingSession sql=recording_session
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    sessionTypeId RecordingSessionTypeId
    titleEs Text
    titleEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    recordedAt UTCTime Maybe
    currentSlug Text Maybe
    venueId UUID Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    publishedRevision Int default=0
    deprecatedAt UTCTime Maybe
    replacementId RecordingSessionId Maybe
    usageCount Int64 default=0
    version Int default=1
    UniqueRecordingSessionCode code
    UniqueRecordingSessionSlug currentSlug !force
    deriving Show Generic
ReleaseRecording sql=release_recording
    Id UUID default=gen_random_uuid()
    releaseId RecordReleaseId
    recordingId RecordingId
    discNumber Int default=1
    sortOrder Int
    primaryRecording Bool default=False
    UniqueReleaseRecording releaseId recordingId
    UniqueReleaseRecordingOrder releaseId discNumber sortOrder
    deriving Show Generic
SessionRecording sql=session_recording
    Id UUID default=gen_random_uuid()
    sessionId RecordingSessionId
    recordingId RecordingId
    sortOrder Int
    primaryRecording Bool default=False
    UniqueSessionRecording sessionId recordingId
    UniqueSessionRecordingOrder sessionId sortOrder
    deriving Show Generic
EditorialCollection sql=editorial_collection
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    collectionType Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    currentSlug Text Maybe
    publicRoute Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    updatedBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    publishedRevision Int default=0
    version Int default=1
    UniqueEditorialCollectionCode code
    UniqueEditorialCollectionSlug currentSlug !force
    deriving Show Generic
CollectionRelease sql=collection_release
    Id UUID default=gen_random_uuid()
    collectionId EditorialCollectionId
    releaseId RecordReleaseId
    sortOrder Int
    featured Bool default=False
    UniqueCollectionRelease collectionId releaseId
    UniqueCollectionReleaseOrder collectionId sortOrder
    deriving Show Generic
CollectionRecording sql=collection_recording
    Id UUID default=gen_random_uuid()
    collectionId EditorialCollectionId
    recordingId RecordingId
    sortOrder Int
    featured Bool default=False
    UniqueCollectionRecording collectionId recordingId
    UniqueCollectionRecordingOrder collectionId sortOrder
    deriving Show Generic
CollectionSession sql=collection_session
    Id UUID default=gen_random_uuid()
    collectionId EditorialCollectionId
    sessionId RecordingSessionId
    sortOrder Int
    featured Bool default=False
    UniqueCollectionSession collectionId sessionId
    UniqueCollectionSessionOrder collectionId sortOrder
    deriving Show Generic
RecordContributor sql=record_contributor
    Id UUID default=gen_random_uuid()
    catalogId CatalogDefinitionId
    code Text
    contributorKind Text
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueRecordContributorCode code
    deriving Show Generic
ReleaseContributor sql=release_contributor
    Id UUID default=gen_random_uuid()
    releaseId RecordReleaseId
    contributorId RecordContributorId
    creditRole Text
    sortOrder Int default=0
    primaryCredit Bool default=False
    UniqueReleaseContributor releaseId contributorId creditRole
    UniqueReleaseContributorOrder releaseId creditRole sortOrder
    deriving Show Generic
RecordingContributor sql=recording_contributor
    Id UUID default=gen_random_uuid()
    recordingId RecordingId
    contributorId RecordContributorId
    creditRole Text
    sortOrder Int default=0
    primaryCredit Bool default=False
    UniqueRecordingContributor recordingId contributorId creditRole
    UniqueRecordingContributorOrder recordingId creditRole sortOrder
    deriving Show Generic
SessionContributor sql=session_contributor
    Id UUID default=gen_random_uuid()
    sessionId RecordingSessionId
    contributorId RecordContributorId
    creditRole Text
    sortOrder Int default=0
    primaryCredit Bool default=False
    UniqueSessionContributor sessionId contributorId creditRole
    UniqueSessionContributorOrder sessionId creditRole sortOrder
    deriving Show Generic
RecordExternalResource sql=record_external_resource
    Id UUID default=gen_random_uuid()
    providerId ExternalProviderId
    externalCode Text
    resourceKind Text
    canonicalUrl Text
    labelEs Text Maybe
    labelEn Text Maybe
    durationMs Int Maybe
    thumbnailUrl Text Maybe
    active Bool default=True
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueRecordExternalResource providerId resourceKind externalCode
    deriving Show Generic
ReleaseExternalResource sql=release_external_resource
    Id UUID default=gen_random_uuid()
    releaseId RecordReleaseId
    resourceId RecordExternalResourceId
    relationKind Text
    sortOrder Int default=0
    primaryResource Bool default=False
    UniqueReleaseExternalResource releaseId resourceId relationKind
    UniqueReleaseExternalResourceOrder releaseId relationKind sortOrder
    deriving Show Generic
RecordingExternalResource sql=recording_external_resource
    Id UUID default=gen_random_uuid()
    recordingId RecordingId
    resourceId RecordExternalResourceId
    relationKind Text
    sortOrder Int default=0
    primaryResource Bool default=False
    UniqueRecordingExternalResource recordingId resourceId relationKind
    UniqueRecordingExternalResourceOrder recordingId relationKind sortOrder
    deriving Show Generic
SessionExternalResource sql=session_external_resource
    Id UUID default=gen_random_uuid()
    sessionId RecordingSessionId
    resourceId RecordExternalResourceId
    relationKind Text
    sortOrder Int default=0
    primaryResource Bool default=False
    UniqueSessionExternalResource sessionId resourceId relationKind
    UniqueSessionExternalResourceOrder sessionId relationKind sortOrder
    deriving Show Generic
CollectionExternalResource sql=collection_external_resource
    Id UUID default=gen_random_uuid()
    collectionId EditorialCollectionId
    resourceId RecordExternalResourceId
    relationKind Text
    sortOrder Int default=0
    primaryResource Bool default=False
    UniqueCollectionExternalResource collectionId resourceId relationKind
    UniqueCollectionExternalResourceOrder collectionId relationKind sortOrder
    deriving Show Generic
NavigationItem sql=navigation_item
    Id UUID default=gen_random_uuid()
    code Text
    parentId NavigationItemId Maybe
    nameEs Text
    nameEn Text
    descriptionEs Text Maybe
    descriptionEn Text Maybe
    routePath Text
    iconCode Text Maybe
    featureCode Text Maybe
    requiredPermissionId SecurityPermissionId Maybe
    platformScope Text
    publicVisible Bool default=False
    sortOrder Int default=0
    active Bool default=True
    workflowStateId WorkflowStateId
    createdBy PartyId Maybe
    approvedBy PartyId Maybe
    createdAt UTCTime default=CURRENT_TIMESTAMP
    updatedAt UTCTime default=CURRENT_TIMESTAMP
    version Int default=1
    UniqueNavigationItemCode code
    UniqueNavigationItemRoute platformScope routePath
    deriving Show Generic
|]
