#!/usr/bin/env node

import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { resolve } from 'node:path';
import process from 'node:process';

const DEFAULT_INPUT = 'docs/catalog-persistence/reports/static-list-inventory.json';
const DEFAULT_OUTPUT = 'docs/catalog-persistence/catalog-list-decisions.json';

const SECURITY_PATTERN = /(?:action|capabilit|grant|module|permission|role|security)/i;
const GOVERNED_PATTERN = /(?:countr|currenc|ddex|external|identifier|iso|language|locale|payment|platform|provider|subdivision|tax|territor)/i;
const WORKFLOW_PATTERN = /(?:stage|state|status|transition)/i;

function parseArgs(argv) {
  const options = { input: DEFAULT_INPUT, output: DEFAULT_OUTPUT };
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === '--input') options.input = argv[++index];
    else if (arg === '--output') options.output = argv[++index];
    else if (arg === '--help') {
      process.stdout.write(
        'Usage: node scripts/build-catalog-decisions.mjs [--input PATH] [--output PATH]\n',
      );
      process.exit(0);
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }
  return options;
}

function context(candidate) {
  return `${candidate.file} ${candidate.name} ${candidate.values.join(' ')}`;
}

function isServiceResourceSelectionModeMirror(candidate) {
  return /selectionMode/i.test(candidate.name)
    && candidate.values.length === 2
    && candidate.values.includes('all')
    && candidate.values.includes('first-available');
}

function isAppearanceEmergencyBootstrap(candidate) {
  return candidate.name === 'EMERGENCY_THEME_OPTIONS'
    && candidate.values.length === 3
    && candidate.values.every((value) => /code=(?:system|light|dark)/.test(value));
}

function isSocialEventStateParserMirror(candidate) {
  return candidate.file.endsWith('/TDF/SocialEventLifecycle.hs')
    && candidate.name === 'recognizedSocialEventStateCodes';
}

function isSocialEventCapabilityMirror(candidate) {
  return candidate.file.endsWith('/TDF/SocialEventLifecycle.hs')
    && candidate.name === 'recognizedSocialEventCapabilityCodes';
}

function isServiceStorefrontOrderStatus(candidate) {
  return candidate.file.endsWith('/ServiceOrderTrackingPage.tsx')
    && candidate.kind === 'switch-cases'
    && candidate.name === 'status';
}

function technicalRule(candidate) {
  const value = context(candidate);
  const lowerName = candidate.name.toLowerCase();

  if (
    candidate.file === 'scripts/production-migrations.json'
    && candidate.kind === 'json-array'
    && candidate.name === 'migrations'
  ) {
    return 'Migration identifiers and their ordered script paths are deployment execution mechanics; each applied database revision is persisted separately in the migration ledger.';
  }
  if (
    candidate.file.endsWith('/TDF/App/Boot.hs')
    && candidate.kind === 'haskell-list'
    && candidate.name === 'automaticMigrationPlans'
  ) {
    return 'These labels identify the exhaustive compiled Persistent migration groups checked before startup writes; they are execution boundaries, not selectable business data, roles, permissions, labels, or assignments.';
  }
  if (
    candidate.file.endsWith('/MixingMasteringPage.test.tsx')
    && candidate.kind === 'object-registry'
    && candidate.name === 'packageDto'
  ) {
    return 'These are field names in a typed test DTO fixture, not selectable values; the fixture remains a consumer of the service-package contract.';
  }

  if (
    candidate.kind === 'object-registry' &&
    /\/api\//.test(candidate.file) &&
    /^(?:Bookings|Cms|DDEX|EventDiscoverySourcesAPI|Events|Payments|Rooms|ServiceStorefront|Services|SocialEventsAPI)$/.test(candidate.name) &&
    candidate.values.every((entry) => /^[A-Za-z][A-Za-z0-9]*$/.test(entry))
  ) {
    return 'Typed API-client method names are implementation mechanics; they are not selectable or reportable domain values.';
  }
  if (candidate.name === 'catalogIntegrityStatements' && candidate.kind === 'haskell-list') {
    return 'The list is an ordered set of database DDL and trigger-installation statements; persisted domain rows own all selectable values, labels, assignments, and workflow relationships.';
  }
  if (/_QUERY_KEY$/.test(candidate.name) && candidate.kind === 'array') {
    return 'React Query cache-key segments are client cache mechanics and do not enumerate business values.';
  }
  if (
    (candidate.file.endsWith('/ServiceTypesPage.tsx')
      || candidate.file.endsWith('/RadioAutoStopCatalogPage.tsx')
      || candidate.file.endsWith('/AppearanceModeCatalogPage.tsx')
      || candidate.file.endsWith('/FeedbackCatalogPage.tsx')
      || candidate.file.endsWith('/catalogEditor.tsx'))
    && candidate.kind === 'switch-cases'
    && candidate.values.every((value) => ['approved', 'draft', 'published', 'rejected', 'review'].includes(value))
  ) {
    return 'This switch maps persisted workflow identifiers to visual theme tones only; persisted workflow rows own labels, ordering, transitions, and availability.';
  }
  if (
    candidate.file.endsWith('/catalogs.tsx')
    && candidate.kind === 'switch-cases'
    && candidate.name === 'classification'
  ) {
    return 'This switch localizes persisted catalog-classification identifiers for presentation; catalog_definition rows remain authoritative for which catalogs exist and how they are classified.';
  }
  if (
    candidate.file.endsWith('/catalogAdmin.ts')
    && (
      candidate.name === 'CatalogEditorKind'
      || (candidate.kind === 'switch-cases' && candidate.name === 'entityKind')
    )
  ) {
    return 'These closed discriminants select strict native form components for code-recognized entity schemas; persisted catalog definitions and items remain authoritative and unsupported types are deny-by-default read-only.';
  }
  if (
    candidate.file.endsWith('/CatalogsPage.tsx')
    && candidate.kind === 'switch-cases'
    && candidate.name === 'entityKind'
  ) {
    return 'This closed dispatch selects strict web editor components for code-recognized entity schemas; protected catalog_definition rows remain authoritative for the visible catalog list and unsupported schemas stay read-only.';
  }
  if (
    candidate.file.endsWith('/FeedbackCatalogPage.tsx')
    && (candidate.name === 'FeedbackCatalogCode' || candidate.name === 'CATALOGS')
  ) {
    return 'These identifiers bind two persisted feedback entity schemas to their validated scoped-default adapters; catalog_definition and specialized item rows remain authoritative for existence, labels, ordering, lifecycle, and selectable values.';
  }
  if (
    candidate.file.endsWith('/catalogEditor.tsx')
    && candidate.kind === 'switch-cases'
    && candidate.name === 'editorKind'
  ) {
    return 'This switch maps a strict native editor discriminator to its persisted default-scope schema; protected definitions and specialized catalog rows remain authoritative, with unsupported kinds denied by default.';
  }
  if (/scripts\/(?:lib\/)?(?:continuous-improvement|production-release|ai-workflow|ui-static-audit|refresh-)/.test(candidate.file)) {
    return 'CLI commands, release steps, process state, and audit-rule identifiers are execution mechanics of repository automation.';
  }
  if (/^(?:LogoVariant|PdfAlign|HealthState|ThemePreference|ExperimentVariant|StreamingPlayerVariant|ViewMode|TabKey|SortOrder|RegistrationSortKey|BuyerField|CustomFieldValueType|CustomFieldType|SendMode|EventDetailTab)$/.test(candidate.name)) {
    return 'This closed union controls rendering, local view state, or serialization mechanics and is not a governed business catalog.';
  }
  if (/^(?:initial|timeOptions|WEB_PAYMENT_ERROR|sanitizedGhStatus|runnerState)/i.test(candidate.name)) {
    return 'The values are keys of an internal state/error object rather than selectable business data.';
  }
  if (candidate.kind === 'switch-cases' && /(?:action\.type|command|provider|quality)/i.test(candidate.name)) {
    if (/provider|quality/i.test(candidate.name)) return null;
    return 'The switch is an exhaustive implementation dispatch over reducer, CLI, or arithmetic branches.';
  }
  if (/(?:AllowedKeys|RequestFields|CanonicalReplyKeys|LegacyReplyKeys|MetadataAllowedKeys)$/i.test(candidate.name)) {
    return 'The list is a strict wire-payload field allowlist used to reject unknown JSON keys, not domain reference data.';
  }
  if (/(?:ContentTypes|Extensions|Sensitive.*Fields|reservedFutureStubSlugLabels)/i.test(candidate.name)) {
    return 'The list is a security/parser allowlist for MIME types, file extensions, reserved language properties, or secret-bearing fields.';
  }
  if (/^(?:LogoVariant|RefundStatusColor)$/.test(candidate.name) || /Color$/.test(candidate.name)) {
    return 'Visual style tokens are rendering constants; persisted catalog records own labels and ordering, not theme palette tokens.';
  }
  if (/PdfAlign/.test(value)) {
    return 'PDF alignment tokens are document-layout mechanics.';
  }
  if (/META_HUMAN_AGENT_TAG_MISSING_ERROR_SUBCODE/.test(candidate.name)) {
    return 'This is an external protocol error discriminator required for exact error handling.';
  }
  if (/^(?:DriveAuthStatus|InstagramAuthStatus)$/.test(candidate.name)) {
    return 'These states describe an in-memory OAuth handshake, not an administrable business workflow.';
  }
  if (candidate.name === 'Directive') {
    return 'This sum type is an internal control/result discriminator and is not offered as configurable domain data.';
  }
  if (/^update[A-Z].*Fields$/.test(candidate.name)) {
    return 'The list enumerates accepted record fields for strict decoding and patch semantics.';
  }
  if (candidate.file.includes('DDEX/ERN/') && /(?:buildReleaseId|XML|XSD)/i.test(value)) {
    return 'The value is required to parse or render a versioned DDEX wire protocol; persisted reference metadata is maintained separately.';
  }
  return null;
}

function classificationFor(candidate, technicalJustification) {
  if (isAppearanceEmergencyBootstrap(candidate)) return 'dynamic-business-catalog';
  if (isServiceResourceSelectionModeMirror(candidate)) return 'dynamic-business-catalog';
  if (isSocialEventStateParserMirror(candidate)) return 'dynamic-business-catalog';
  if (isSocialEventCapabilityMirror(candidate)) return 'security-system-registry';
  if (isServiceStorefrontOrderStatus(candidate)) return 'dynamic-business-catalog';
  if (technicalJustification) return 'genuine-technical-constant';
  const value = context(candidate);
  // "reaction" contains the substring "action". Reaction choices are
  // business catalog data unless the candidate also names an explicit
  // security concept; do not let the broad security heuristic misclassify
  // social reactions as grantable actions.
  if (/reaction/i.test(value) && !/(?:capabilit|grant|module|permission|role|security)/i.test(value)) {
    return 'dynamic-business-catalog';
  }
  if (SECURITY_PATTERN.test(value)) return 'security-system-registry';
  if (GOVERNED_PATTERN.test(value)) return 'governed-reference-data';
  return 'dynamic-business-catalog';
}

function specializedModel(candidate, classification) {
  const value = context(candidate);
  if (isAppearanceEmergencyBootstrap(candidate)) {
    return 'appearance_mode_option, catalog_scoped_default';
  }
  if (isServiceResourceSelectionModeMirror(candidate)) {
    return 'service_resource_selection_mode, service_offering_default_resource.selection_mode_id';
  }
  if (isSocialEventStateParserMirror(candidate) || isSocialEventCapabilityMirror(candidate)) {
    return 'workflow_definition, workflow_state, workflow_transition, workflow_default_state, workflow_state_capability';
  }
  if (isServiceStorefrontOrderStatus(candidate)) {
    return 'workflow_definition, workflow_state, workflow_transition, workflow_default_state';
  }
  if (classification === 'genuine-technical-constant') return 'technical_constant_allowlist';
  if (/navigation|menu|feature|sidebar|public_nav/i.test(value)) {
    return 'navigation_item, navigation_item_permission';
  }
  if (/reaction/i.test(value) && !/(?:capabilit|grant|module|permission|role|security)/i.test(value)) {
    return 'reaction_type, content_reaction_type, fan_club_post_reaction, fan_club_memory_reaction';
  }
  if (/creator.?badge|badge.?type|trendsetter/i.test(value)) {
    return 'creator_badge_type, creator_badge';
  }
  if (SECURITY_PATTERN.test(value)) {
    if (/role/i.test(value)) return 'security_role, party_security_role, role_permission';
    if (/module/i.test(value)) return 'security_module, security_permission';
    return 'security_action, security_permission, role_permission';
  }
  if (/genre/i.test(value)) return 'genre, genre_translation, artist_genre_membership';
  if (/instrument/i.test(value)) return 'instrument, instrument_translation, session_instrument';
  if (/currenc/i.test(value)) return 'currency_reference, deployment_currency_enablement';
  if (/locale|language/i.test(value)) return 'language_reference, locale_reference, deployment_locale_enablement';
  if (/countr|city|subdivision|territor/i.test(value)) return 'country_reference, subdivision_reference, city_reference';
  if (/ddex/i.test(value)) return 'ddex_standard_version, ddex_message_type, ddex_vocabulary, ddex_code';
  if (/provider|platform|payment method/i.test(value)) return 'external_provider, external_provider_code';
  if (/tax/i.test(value)) return 'tax_rate_reference';
  if (/cms|records|release|recording/i.test(value)) return 'content_type, record_release, recording, recording_session, editorial_collection';
  if (/service|pricing|booking|room/i.test(value)) return 'service_category, service_offering, booking_type';
  if (/event type/i.test(value)) return 'event_type';
  if (/reaction/i.test(value)) return 'reaction_type';
  if (WORKFLOW_PATTERN.test(value)) return 'workflow_definition, workflow_state, workflow_transition';
  if (/categor|tag/i.test(value)) return 'content_category, tag';
  return 'domain-specific catalog table selected during consumer refactor';
}

function priorityFor(candidate, classification) {
  if (candidate.sourceKind === 'test') return 'P3-consumer';
  if (classification === 'genuine-technical-constant') return 'P3-retain';
  if (classification === 'security-system-registry') return 'P0-security';
  if (candidate.domain === 'music-catalog-cms') return 'P0-cutover';
  if (candidate.exactDuplicateIds.length > 0 || candidate.consumerCount > 10) return 'P1-cross-platform';
  if (candidate.sourceKind === 'migration') return 'P2-schema-history';
  return 'P2-domain';
}

function dispositionFor(candidate, classification) {
  if (candidate.sourceKind === 'test') return 'consumer-update';
  if (candidate.sourceKind === 'generated-client') return 'regenerate-from-openapi';
  if (candidate.sourceKind === 'migration') return 'historical-evidence-replaced-by-forward-migration';
  if (isServiceResourceSelectionModeMirror(candidate)) {
    return 'persisted-registry-with-exhaustive-execution-mirror';
  }
  if (isAppearanceEmergencyBootstrap(candidate)) {
    return 'versioned-emergency-bootstrap-replaced-by-catalog-snapshot';
  }
  if (isSocialEventStateParserMirror(candidate)) {
    return 'persisted-registry-with-parser-adapter-mirror';
  }
  if (isSocialEventCapabilityMirror(candidate)) {
    return 'persisted-registry-with-deny-by-default-code-mirror';
  }
  if (classification === 'genuine-technical-constant') return 'retain-in-code';
  if (
    classification === 'security-system-registry' &&
    candidate.surface === 'backend' &&
    /(?:haskell-sum-type|haskell-list)/.test(candidate.kind)
  ) {
    return 'persisted-registry-with-deny-by-default-code-mirror';
  }
  if (
    classification === 'governed-reference-data' &&
    /DDEX|provider|protocol|payment/i.test(context(candidate)) &&
    candidate.surface === 'backend'
  ) {
    return 'persisted-reference-with-parser-adapter-mirror';
  }
  return 'migrate-and-remove-authority-from-code';
}

function riskFor(candidate, classification) {
  if (classification === 'security-system-registry') {
    return 'Privilege escalation, silent unknown grants, self-approval, or administrator lockout.';
  }
  if (classification === 'governed-reference-data') {
    return 'Standards drift, provider rejection, invalid external codes, or incompatible client caches.';
  }
  if (classification === 'dynamic-business-catalog') {
    return 'Cross-client divergence, invalid historical labels, ambiguous backfill, or broken filters/reports.';
  }
  return 'Changing the constant could break parsing, transport, security checks, or implementation dispatch.';
}

function justificationFor(candidate, classification, disposition, technicalJustification) {
  if (candidate.sourceKind === 'test') {
    return 'Test data is retained only as a consumer/negative fixture and must follow the canonical persisted contract.';
  }
  if (candidate.sourceKind === 'migration') {
    return 'Historical migration evidence is immutable; a forward migration removes string authority and adds canonical foreign keys.';
  }
  if (candidate.sourceKind === 'generated-client') {
    return 'Generated code is never authoritative and will be regenerated from the canonical OpenAPI contract.';
  }
  if (isServiceResourceSelectionModeMirror(candidate)) {
    return 'Database rows own selection, labels, ordering, lifecycle, and administration. Code/OpenAPI retain only the closed discriminants required to execute each persisted resource-allocation policy; startup rejects missing or unknown active rows.';
  }
  if (isSocialEventStateParserMirror(candidate)) {
    return 'Persisted workflow rows own existence, labels, ordering, defaults, transitions, activation, and API options. Code retains only the provider/parser outputs it can emit, and startup requires each recognized code to resolve while allowing additional administrable persisted states.';
  }
  if (isSocialEventCapabilityMirror(candidate)) {
    return 'Persisted workflow-state capability rows own assignments. Code recognizes only the two executable behavior boundaries, startup rejects unknown enabled capabilities, and missing rows deny access by default.';
  }
  if (isAppearanceEmergencyBootstrap(candidate)) {
    return 'The database remains authoritative for labels, ordering, availability, and the scoped default. This marked recovery snapshot is used only when no valid network/cache snapshot exists and is replaced after successful synchronization.';
  }
  if (technicalJustification) return technicalJustification;
  if (disposition.includes('code-mirror') || disposition.includes('adapter-mirror')) {
    return 'Code retains only stable exhaustive identifiers needed for deny-by-default enforcement or protocol parsing; persisted rows own labels, ordering, assignment, validation metadata, and administration.';
  }
  if (classification === 'security-system-registry') {
    return 'Assignments and selectable grants must be database-authoritative, audited, approval-gated, and validated against the exhaustive backend capability registry.';
  }
  if (classification === 'governed-reference-data') {
    return 'The dataset must be database-authoritative and updated through a versioned standards/provider import rather than independent code allowlists.';
  }
  return 'The values are selectable, displayable, reportable, governable, or relational business data and therefore require a specialized persisted model.';
}

function main() {
  const options = parseArgs(process.argv.slice(2));
  const inputPath = resolve(options.input);
  if (!existsSync(inputPath)) throw new Error(`Inventory not found: ${inputPath}`);
  const inventory = JSON.parse(readFileSync(inputPath, 'utf8'));
  const decisions = inventory.candidates.map((candidate) => {
    const technicalJustification = technicalRule(candidate);
    const classification = classificationFor(candidate, technicalJustification);
    const disposition = dispositionFor(candidate, classification);
    return {
      id: candidate.id,
      classification,
      disposition,
      specializedModel: specializedModel(candidate, classification),
      priority: priorityFor(candidate, classification),
      risk: riskFor(candidate, classification),
      justification: justificationFor(
        candidate,
        classification,
        disposition,
        technicalJustification,
      ),
      reviewed: true,
    };
  });
  const report = {
    schemaVersion: 1,
    baselineRevision: 'ce0c3bc19e2d9030e871480e9e93790940c9eb12',
    generatedFrom: DEFAULT_INPUT,
    decisions,
  };
  writeFileSync(resolve(options.output), `${JSON.stringify(report, null, 2)}\n`);
}

main();
