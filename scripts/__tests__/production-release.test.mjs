import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';

import {
  captureContextualReputationGate,
  captureEventDiscoveryGates,
  runtimeEnvBlockers,
} from '../production-release.mjs';
import {
  MUSIC_RUNTIME_FUNCTION_SIGNATURES,
  buildMigrationBatchSql,
  buildSchemaPreflightSql,
  buildSchemaVerificationSql,
  expandMigrationIncludes,
  normalizeFullSha,
  parseSecurityEmergencyReadinessOutput,
  requireMigrationIntroductionAncestor,
  securityEmergencyReadinessBlocker,
  validateProductionCompose,
  validateMigrationRelativePath,
} from '../lib/production-release.mjs';

const releaseSha = 'ABCDEF0123456789ABCDEF0123456789ABCDEF01';

test('retired release CLI rejects legacy commands before any provider invocation', () => {
  for (const mode of ['plan', 'preflight', 'release']) {
    const result = spawnSync(process.execPath, [
      new URL('../production-release.mjs', import.meta.url).pathname,
      mode, '--sha', releaseSha, '--execute', '--confirm', releaseSha,
    ], { encoding: 'utf8', env: { PATH: '/nonexistent' } });
    assert.equal(result.status, 1, result.stderr);
    assert.match(result.stderr, /release CLI is retired/);
    assert.match(result.stderr, /ops\/hetzner\/README.md/);
    assert.equal(result.stdout, '');
    assert.doesNotMatch(result.stderr, /ENOENT|\$ /);
  }
});
const normalizedReleaseSha = releaseSha.toLowerCase();

const ticketMigration = 'tdf-hq/sql/2026-07-12_ticket_checkout_idempotency.sql';
const discoveryMigration = 'tdf-hq/sql/2026-07-12_event_discovery_imports.sql';

test('normalizeFullSha trims and lowercases an immutable full commit SHA', () => {
  assert.equal(normalizeFullSha(`  ${releaseSha}\n`), normalizedReleaseSha);
});

test('normalizeFullSha rejects mutable tags, abbreviated SHAs, and malformed values', () => {
  for (const invalid of [
    '',
    'latest',
    'abcdef0',
    `${normalizedReleaseSha}0`,
    'g'.repeat(40),
    `${normalizedReleaseSha.slice(0, 20)}\n${normalizedReleaseSha.slice(20)}`,
  ]) {
    assert.throws(
      () => normalizeFullSha(invalid),
      /40|full|sha|commit|hex/i,
      `expected ${JSON.stringify(invalid)} to be rejected`,
    );
  }
});

test('production migration manifest uses immutable full commit SHAs', () => {
  const manifest = JSON.parse(readFileSync(
    new URL('../production-migrations.json', import.meta.url),
    'utf8',
  ));

  assert.equal(manifest.schemaVersion, 1);
  assert.ok(Array.isArray(manifest.migrations));
  assert.equal(
    new Set(manifest.migrations.map(({ id }) => id)).size,
    manifest.migrations.length,
    'migration IDs must be unique',
  );
  assert.equal(
    new Set(manifest.migrations.map(({ path }) => path)).size,
    manifest.migrations.length,
    'migration paths must be unique',
  );
  for (const migration of manifest.migrations) {
    assert.equal(normalizeFullSha(migration.introducedBy), migration.introducedBy);
    assert.equal(
      existsSync(new URL(`../../${migration.path}`, import.meta.url)),
      true,
      `${migration.path} must exist`,
    );
  }

  const migrationIndex = (id) => manifest.migrations.findIndex((migration) => migration.id === id);
  for (const [prerequisite, dependent] of [
    ['2026-09-01_contextual_reputation', '2026-09-04_contextual_reputation_integrity'],
    ['2026-09-28_interaction_integrity', '2026-09-29_interaction_review_repairs'],
    ['2026-09-29_interaction_review_repairs', '2026-09-29_interaction_publication_authority'],
    ['2026-09-29_interaction_publication_authority', '2026-09-29_interaction_reaction_withdrawal'],
    ['2026-09-29_interaction_reaction_withdrawal', '2026-09-29_interaction_moderation_block_boundary'],
    ['2026-09-29_interaction_moderation_block_boundary', '2026-09-29_interaction_moderation_access'],
    ['2026-09-06_user_onboarding_progress', '2026-09-07_user_experiment_assignment'],
    ['2026-09-07_directory_event_visibility_and_favorite_evidence', '2026-09-08_event_rsvp_identity_privacy_feed'],
    ['2026-09-07_directory_event_visibility_and_favorite_evidence', '2026-09-17_directory_event_privacy_composition'],
    ['2026-09-09_music_directory_suppressed_event_privacy', '2026-09-17_directory_event_privacy_composition'],
    ['2026-09-17_directory_event_privacy_composition', '2026-10-03_discovery_ownership_metadata_boundary'],
    ['2026-09-07_artist_merch_storefronts', '2026-09-08_merch_reputation'],
    ['2026-09-09_canonical_payment_lifecycle', '2026-09-10_payment_attempt_intent_binding'],
    ['2026-09-10_payment_attempt_intent_binding', '2026-09-11_payment_intent_runtime_sync'],
    ['2026-09-11_manual_bank_provider_activation', '2026-09-11_provider_capability_catalog'],
  ]) {
    assert.ok(migrationIndex(prerequisite) >= 0, `${prerequisite} must be registered`);
    assert.ok(
      migrationIndex(prerequisite) < migrationIndex(dependent),
      `${prerequisite} must precede ${dependent}`,
    );
  }

  const resumeIndex = manifest.migrations.findIndex(
    ({ id }) => id === '2026-08-16_catalog_locale_preference_resume',
  );
  const writerResumeIndex = manifest.migrations.findIndex(
    ({ id }) => id === '2026-08-16_catalog_transitional_writer_resume',
  );
  const backfillIndex = manifest.migrations.findIndex(
    ({ id }) => id === '2026-08-07_catalog_backfill_apply',
  );
  assert.ok(resumeIndex >= 0, 'resume migration must be registered');
  assert.equal(writerResumeIndex, resumeIndex + 1, 'writer resume must follow locale recovery');
  assert.equal(backfillIndex, writerResumeIndex + 1, 'writer resume must run immediately before backfill');
});

test('event schema prerequisites are registered once in dependency order', () => {
  const { migrations } = JSON.parse(readFileSync(
    new URL('../production-migrations.json', import.meta.url), 'utf8',
  ));
  const prerequisites = [
    '2026-09-06_user_onboarding_progress',
    '2026-09-07_artist_merch_storefronts',
    '2026-09-09_canonical_payment_lifecycle',
    '2026-09-10_payment_attempt_intent_binding',
  ];
  for (const id of prerequisites) {
    const entries = migrations.filter((entry) => entry.id === id);
    assert.equal(entries.length, 1, `${id} must be registered exactly once`);
    assert.equal(entries[0].path, `tdf-hq/sql/${id}.sql`);
  }
  for (const [prerequisite, consumer] of [
    ['2026-08-13_unified_checkout_core', '2026-09-07_artist_merch_storefronts'],
    ['2026-08-14_checkout_event_refund_runtime', '2026-09-07_artist_merch_storefronts'],
    ['2026-08-14_music_directory_core', '2026-09-07_artist_merch_storefronts'],
    ['2026-09-07_artist_merch_storefronts', '2026-09-08_merch_reputation'],
    ['2026-08-14_checkout_event_refund_runtime', '2026-09-09_canonical_payment_lifecycle'],
    ['2026-09-09_canonical_payment_lifecycle', '2026-09-10_payment_attempt_intent_binding'],
  ]) {
    const before = migrations.findIndex(({ id }) => id === prerequisite);
    const after = migrations.findIndex(({ id }) => id === consumer);
    assert.ok(before >= 0 && after > before, `${prerequisite} must precede ${consumer}`);
  }
});

test('suppressed-event privacy migration is anchored to its released squash commit', () => {
  const manifest = JSON.parse(readFileSync(
    new URL('../production-migrations.json', import.meta.url),
    'utf8',
  ));
  const migration = manifest.migrations.find(
    ({ id }) => id === '2026-09-09_music_directory_suppressed_event_privacy',
  );

  assert.deepEqual(migration, {
    id: '2026-09-09_music_directory_suppressed_event_privacy',
    path: 'tdf-hq/sql/2026-09-09_music_directory_suppressed_event_privacy.sql',
    introducedBy: '849444cdcce0254c3091293edce8a4cc5f8175fa',
  });
});

test('Party selector rate-limit compatibility is anchored to its migration commit', () => {
  const manifest = JSON.parse(readFileSync(
    new URL('../production-migrations.json', import.meta.url),
    'utf8',
  ));
  const migration = manifest.migrations.find(
    ({ id }) => id === '2026-09-10_party_selector_rate_limit_scopes',
  );

  assert.deepEqual(migration, {
    id: '2026-09-10_party_selector_rate_limit_scopes',
    path: 'tdf-hq/sql/2026-09-10_party_selector_rate_limit_scopes.sql',
    introducedBy: '565ed7d0a1c5381719538c6e2bcae83ac30c3a92',
  });
});

test('already-applied production migrations retain their recorded checksums', () => {
  const expected = new Map([
    [
      '../../tdf-hq/sql/2026-08-13_service_storefront_phase0_hardening.sql',
      '7541ba81474ae699a6de2849ace0baad3623bc7dbffb285d70d611b17ea50b0b',
    ],
    [
      '../../tdf-hq/sql/2026-08-14_catalog_integrity.sql',
      '4a856fb813417e2e87aed286b1b664d54cbda004ffb1c44baed89f4b4901571b',
    ],
    [
      '../../tdf-hq/sql/2026-08-13_unified_checkout_core.sql',
      '79a940399d6d93f1d3ec7da0fb009e45aade993c0c2a645e674799a298e0abd0',
    ],
    [
      '../../tdf-hq/sql/2026-08-13_distribution_accounting_core.sql',
      'ab983f7af3b192e6425b29096e800f7095ef185d288a0f639a280d1a5e361278',
    ],
    [
      '../../tdf-hq/sql/2026-08-14_catalog_integrity.sql',
      '4a856fb813417e2e87aed286b1b664d54cbda004ffb1c44baed89f4b4901571b',
    ],
    [
      '../../tdf-hq/sql/2026-08-18_music_directory_profile_images.sql',
      '63dd690b595584c057c87278d8ee9e3d22607b59d127100b0267b736cb3c5312',
    ],
    [
      '../../tdf-hq/sql/2026-09-06_contextual_reputation_staging_worker.sql',
      '5ec11f453dddf2b167c79f4d54ac7ec677591f8c2f9fa7bb2a9bc389878a6959',
    ],
    [
      '../../tdf-hq/sql/2026-09-04_access_request_notification_types.sql',
      'a2eede1722ecfa4decd054755a3f871902808ec2bca5dcf8413d9b9b7c644d1e',
    ],
    [
      '../../tdf-hq/sql/2026-09-08_notification_notif_type_not_null_repair.sql',
      'f83049fb3f27f4d57ac2c5594849ec10cef83b847363a25e76712f9369d2cf5d',
    ],
    [
      '../../tdf-hq/sql/2026-09-08_notification_notif_type_text_compatibility.sql',
      'e495d71e1f6553735351d58edd65afbb4cb0a5aaa841112937c3b068faeec204',
    ],
    [
      '../../tdf-hq/sql/2026-09-09_music_directory_suppressed_event_privacy.sql',
      '33f89f61c2552cacc6c4842ac8513d6b518a7b342115a9b1cde3f01a76926712',
    ],
  ]);

  for (const [relativePath, checksum] of expected) {
    const actual = createHash('sha256')
      .update(readFileSync(new URL(relativePath, import.meta.url)))
      .digest('hex');
    assert.equal(actual, checksum, `${relativePath} must remain byte-for-byte immutable`);
  }
});

test('notification repair checksum history advances through a new forward migration', () => {
  const manifest = JSON.parse(readFileSync(
    new URL('../production-migrations.json', import.meta.url),
    'utf8',
  ));
  const accessIndex = manifest.migrations.findIndex(
    ({ id }) => id === '2026-09-04_access_request_notification_types',
  );
  const nullabilityIndex = manifest.migrations.findIndex(
    ({ id }) => id === '2026-09-08_notification_notif_type_not_null_repair',
  );
  const compatibilityIndex = manifest.migrations.findIndex(
    ({ id }) => id === '2026-09-08_notification_notif_type_text_compatibility',
  );

  assert.deepEqual(manifest.migrations[accessIndex].compatibleAppliedChecksums, [
    '200c8405c62fecfcd2479d5d889da511830f39a4ac08d6612c51abbbfbabceab',
    'ad04cc74dae4f937932cc50772d8f2bcb0be8f8cf6652c0b6d1162d85fc7c0e9',
    '90a2e34be1feb89975baa608a6a07a1491e4632fd0dd5bca3ac1edf387c8670c',
  ]);
  assert.deepEqual(manifest.migrations[nullabilityIndex].compatibleAppliedChecksums, [
    '7942288682d50a655dd81f072b0ccd91d262d070b493cacad2602565e8e42347',
  ]);
  assert.equal(compatibilityIndex, nullabilityIndex + 1);
});

test('production release refuses to omit a migration outside the release ancestry', () => {
  const migration = {
    id: '2026-08-14_catalog_canonical_schema',
    introducedBy: '1'.repeat(40),
  };

  assert.doesNotThrow(() => {
    requireMigrationIntroductionAncestor(migration, '2'.repeat(40), true);
  });
  assert.throws(
    () => requireMigrationIntroductionAncestor(migration, '2'.repeat(40), false),
    /not an ancestor.*refusing to omit/u,
  );
});

test('validateMigrationRelativePath accepts a dated SQL migration inside tdf-hq/sql', () => {
  assert.equal(validateMigrationRelativePath(ticketMigration), ticketMigration);
});

test('validateMigrationRelativePath rejects traversal, absolute, nested, and non-SQL paths', () => {
  for (const invalid of [
    '/tmp/migration.sql',
    '../tdf-hq/sql/migration.sql',
    'tdf-hq/sql/../Dockerfile',
    'tdf-hq/sql/nested/migration.sql',
    'tdf-hq/sql/migration.txt',
    'tdf-hq/sql/migration.sql\n\\quit',
  ]) {
    assert.throws(
      () => validateMigrationRelativePath(invalid),
      /migration|path|sql|relative|unsafe/i,
      `expected ${JSON.stringify(invalid)} to be rejected`,
    );
  }
});

test('expandMigrationIncludes embeds same-directory SQL and rejects recursive includes', async () => {
  const files = new Map([
    ['tdf-hq/sql/2026-08-14_source.sql', 'SELECT 42 AS included;'],
    ['tdf-hq/sql/2026-08-14_cycle.sql', '\\ir 2026-08-14_cycle.sql'],
  ]);
  const readFile = async (relativePath) => {
    if (!files.has(relativePath)) throw new Error(`missing ${relativePath}`);
    return files.get(relativePath);
  };

  const expanded = await expandMigrationIncludes({
    path: 'tdf-hq/sql/2026-08-14_apply.sql',
    content: 'BEGIN;\n\\ir 2026-08-14_source.sql\nCOMMIT;',
  }, readFile);

  assert.match(expanded, /begin inlined migration include/i);
  assert.match(expanded, /SELECT 42 AS included/);
  assert.doesNotMatch(expanded, /^\s*\\ir\s+/mu);
  await assert.rejects(
    expandMigrationIncludes({
      path: 'tdf-hq/sql/2026-08-14_cycle.sql',
      content: files.get('tdf-hq/sql/2026-08-14_cycle.sql'),
    }, readFile),
    /recursive/i,
  );
});

test('security emergency readiness parser accepts only aggregate read-only gate reports', () => {
  const report = parseSecurityEmergencyReadinessOutput([
    'psql connection notice',
    JSON.stringify({
      kind: 'security-emergency-readiness',
      schemaMode: 'legacy',
      transactionReadOnly: 'on',
      requiredIndependentPaths: 2,
      activeEmergencyAssignments: 2,
      distinctAssignedParties: 2,
      authenticatableParties: 1,
      databaseCoherentPaths: null,
      preMigrationReady: false,
      databaseReady: false,
    }),
  ].join('\n'));

  assert.equal(report.schemaMode, 'legacy');
  assert.equal(report.authenticatableParties, 1);
  assert.equal(
    securityEmergencyReadinessBlocker(report),
    'emergency recovery has 1 independently authenticatable parties; 2 are required before migration',
  );
});

test('security emergency readiness requires canonical coherence after migration', () => {
  const report = parseSecurityEmergencyReadinessOutput(JSON.stringify({
    kind: 'security-emergency-readiness',
    schemaMode: 'canonical',
    transactionReadOnly: 'on',
    requiredIndependentPaths: 2,
    activeEmergencyAssignments: 2,
    distinctAssignedParties: 2,
    authenticatableParties: 2,
    legacyAuthenticatableParties: 0,
    coherentLegacyTargetRoles: 1,
    databaseCoherentPaths: 2,
    preMigrationReady: true,
    databaseReady: true,
  }));

  assert.equal(securityEmergencyReadinessBlocker(report), undefined);
  assert.equal(
    securityEmergencyReadinessBlocker(report, { requireCanonical: true }),
    undefined,
  );
  assert.match(
    securityEmergencyReadinessBlocker(
      { ...report, schemaMode: 'legacy', databaseReady: false },
      { requireCanonical: true },
    ),
    /post-migration.*legacy/i,
  );
  assert.match(
    securityEmergencyReadinessBlocker(
      { ...report, databaseCoherentPaths: 1, databaseReady: false },
      { requireCanonical: true },
    ),
    /1 coherent paths; 2 are required/i,
  );
});

test('partial canonical security may resume from legacy paths but cannot pass the post-migration gate', () => {
  const report = parseSecurityEmergencyReadinessOutput(JSON.stringify({
    kind: 'security-emergency-readiness',
    schemaMode: 'canonical',
    transactionReadOnly: 'on',
    requiredIndependentPaths: 2,
    activeEmergencyAssignments: 0,
    distinctAssignedParties: 0,
    authenticatableParties: 0,
    legacyAuthenticatableParties: 2,
    coherentLegacyTargetRoles: 1,
    databaseCoherentPaths: 0,
    preMigrationReady: true,
    databaseReady: false,
  }));

  assert.equal(securityEmergencyReadinessBlocker(report), undefined);
  assert.match(
    securityEmergencyReadinessBlocker(report, { requireCanonical: true }),
    /0 coherent paths; 2 are required/i,
  );
});

test('partial canonical security cannot claim readiness without a coherent mapped target role', () => {
  assert.throws(
    () => parseSecurityEmergencyReadinessOutput(JSON.stringify({
      kind: 'security-emergency-readiness',
      schemaMode: 'canonical',
      transactionReadOnly: 'on',
      requiredIndependentPaths: 2,
      activeEmergencyAssignments: 0,
      distinctAssignedParties: 0,
      authenticatableParties: 0,
      legacyAuthenticatableParties: 2,
      coherentLegacyTargetRoles: 0,
      databaseCoherentPaths: 0,
      preMigrationReady: true,
      databaseReady: false,
    })),
    /inconsistent gate evidence/i,
  );
});

test('catalog locale resume migration only relaxes copied preference evidence columns', () => {
  const sql = readFileSync(
    new URL('../../tdf-hq/sql/2026-08-16_catalog_locale_preference_resume.sql', import.meta.url),
    'utf8',
  );

  assert.match(sql, /locale_id[\s\S]*data_type = 'uuid'/i);
  assert.match(sql, /currency_id[\s\S]*data_type = 'uuid'/i);
  assert.match(sql, /ALTER COLUMN locale DROP NOT NULL/i);
  assert.match(sql, /ALTER COLUMN currency DROP NOT NULL/i);
  assert.doesNotMatch(sql, /\b(?:UPDATE|DELETE|INSERT)\b/i);
});

test('catalog writer resume migration only removes premature canonical triggers', () => {
  const sql = readFileSync(
    new URL('../../tdf-hq/sql/2026-08-16_catalog_transitional_writer_resume.sql', import.meta.url),
    'utf8',
  );

  assert.match(sql, /DROP TRIGGER IF EXISTS catalog_pipeline_card_integrity ON pipeline_card/i);
  assert.match(sql, /DROP TRIGGER IF EXISTS social_event_type_integrity ON social_event/i);
  assert.match(sql, /DROP TRIGGER IF EXISTS social_event_workflow_state_integrity ON social_event/i);
  assert.doesNotMatch(sql, /\b(?:UPDATE|DELETE|INSERT)\b/i);
});

test('catalog backfill locks pipeline writers around the transitional mapping', () => {
  const sql = readFileSync(
    new URL('../../tdf-hq/sql/2026-08-07_catalog_backfill_apply.sql', import.meta.url),
    'utf8',
  );

  const lockIndex = sql.indexOf(
    'LOCK TABLE pipeline_card IN SHARE ROW EXCLUSIVE MODE;',
  );
  const updateIndex = sql.indexOf(
    'UPDATE pipeline_card target SET service_offering_id=offering.id',
  );
  assert.ok(lockIndex >= 0, 'pipeline writers must be locked');
  assert.match(
    sql,
    /FROM pipeline_card source\s+WHERE source\.service_kind IS NOT NULL/i,
    'reruns must skip pipeline rows whose legacy evidence was already cleared',
  );
  assert.ok(updateIndex > lockIndex, 'pipeline mapping must run after the writer lock');
});

test('pending canonical cutovers relax legacy evidence columns before clearing them', () => {
  const pipelineSql = readFileSync(
    new URL('../../tdf-hq/sql/2026-08-11_pipeline_workflow_cutover_apply.sql', import.meta.url),
    'utf8',
  );
  const ddexSql = readFileSync(
    new URL('../../tdf-hq/sql/2026-08-12_ddex_operational_cutover_apply.sql', import.meta.url),
    'utf8',
  );

  for (const column of ['service_kind', 'stage']) {
    assert.match(pipelineSql, new RegExp(`ALTER COLUMN ${column} DROP NOT NULL`, 'i'));
  }
  for (const clause of [
    'ddex_import_plan ALTER COLUMN status',
    'ddex_import_run ALTER COLUMN status',
    'ddex_job ALTER COLUMN job_type',
    'ddex_job ALTER COLUMN status',
    'ddex_import_change ALTER COLUMN operation',
  ]) {
    assert.match(ddexSql, new RegExp(`ALTER TABLE ${clause} DROP NOT NULL`, 'i'));
  }
});

test('social event cutovers remove transitional triggers before canonical updates', () => {
  const typeSql = readFileSync(
    new URL('../../tdf-hq/sql/2026-08-11_social_event_type_cutover_apply.sql', import.meta.url),
    'utf8',
  );
  const workflowSql = readFileSync(
    new URL('../../tdf-hq/sql/2026-08-11_social_event_workflow_cutover_apply.sql', import.meta.url),
    'utf8',
  );

  assert.ok(typeSql.indexOf('DROP TRIGGER IF EXISTS social_event_type_integrity ON social_event;')
    < typeSql.indexOf('UPDATE social_event target SET'));
  assert.ok(typeSql.indexOf('DROP TRIGGER IF EXISTS social_event_workflow_state_integrity ON social_event;')
    < typeSql.indexOf('UPDATE social_event target SET'));
  assert.ok(workflowSql.indexOf('DROP TRIGGER IF EXISTS social_event_workflow_state_integrity ON social_event;')
    < workflowSql.indexOf('UPDATE social_event target SET'));
});

test('security readiness SQL keeps legacy recovery only as a pre-migration fallback', () => {
  const sql = readFileSync(
    new URL('../../tdf-hq/sql/preflight_security_emergency_readiness.sql', import.meta.url),
    'utf8',
  );

  assert.match(sql, /legacy_authenticatable_party/i);
  assert.match(sql, /role\.code = 'admin'/i);
  assert.match(
    sql,
    /preMigrationReady',[\s\S]*legacy_authenticatable_parties >= 2[\s\S]*coherent_legacy_target_roles = 1/i,
  );
  assert.match(sql, /databaseReady', database_coherent_paths >= 2/i);
});

test('security emergency readiness parser rejects missing, writable, and malformed reports', () => {
  assert.throws(
    () => parseSecurityEmergencyReadinessOutput('not-json'),
    /no report/i,
  );
  assert.throws(
    () => parseSecurityEmergencyReadinessOutput(JSON.stringify({
      kind: 'security-emergency-readiness',
      schemaMode: 'legacy',
      transactionReadOnly: 'off',
      requiredIndependentPaths: 2,
      preMigrationReady: false,
      databaseReady: false,
    })),
    /not read-only/i,
  );
  assert.throws(
    () => parseSecurityEmergencyReadinessOutput(JSON.stringify({
      kind: 'security-emergency-readiness',
      schemaMode: 'canonical',
      transactionReadOnly: 'on',
      requiredIndependentPaths: 2,
      activeEmergencyAssignments: -1,
      preMigrationReady: true,
      databaseReady: true,
    })),
    /activeEmergencyAssignments/i,
  );
});

const productionCompose = readFileSync(
  new URL('../../ops/hetzner/compose.production.yaml', import.meta.url),
  'utf8',
);
function composeWith(search, replacement) {
  // Only the api service follows the edge-facing CORS override, so anchoring on
  // it keeps each mutation inside the service the validator reads.
  const start = productionCompose.indexOf("      CORS_DISABLE_DEFAULTS: 'true'");
  assert.ok(start > 0, 'compose fixture anchor');
  const tail = productionCompose.slice(start);
  assert.ok(tail.includes(search), `compose fixture must contain ${search}`);
  return productionCompose.slice(0, start) + tail.replace(search, replacement);
}

test('validateProductionCompose accepts the reviewed production api service', () => {
  assert.deepEqual(validateProductionCompose(productionCompose), {
    runMigrations: false,
    autoApplyProductionMigrations: true,
    reputationAggregationWorkerEnabled: false,
    eventDiscoveryEnabled: false,
    eventDiscoveryAutoPublish: false,
    internalFeedbackUploadRoot: '/data/assets/.internal-feedback',
  });
});

test('validateProductionCompose fails closed on unsafe or missing staged flags', () => {
  for (const [name, safe, unsafe] of [
    ['RUN_MIGRATIONS', "'false'", "'true'"],
    ['AUTO_APPLY_PRODUCTION_MIGRATIONS', "'true'", "'false'"],
    ['EVENT_DISCOVERY_ENABLED', "'false'", "'true'"],
    ['EVENT_DISCOVERY_AUTO_PUBLISH', "'false'", "'true'"],
    ['REPUTATION_AGGREGATION_WORKER_ENABLED', "'false'", "'true'"],
  ]) {
    const line = `      ${name}: ${safe}\n`;
    assert.throws(
      () => validateProductionCompose(composeWith(line, `      ${name}: ${unsafe}\n`)),
      new RegExp(name),
      `${name} changed`,
    );
    assert.throws(() => validateProductionCompose(composeWith(line, '')), new RegExp(name), `${name} missing`);
  }
});

test('validateProductionCompose reads flags only from the api service environment', () => {
  // The canary service legitimately disables the automatic runner; its value
  // must neither satisfy nor fail the api check.
  assert.match(productionCompose, /AUTO_APPLY_PRODUCTION_MIGRATIONS: 'false'/);
  const duplicated = composeWith("      RUN_MIGRATIONS: 'false'\n", "      RUN_MIGRATIONS: 'true'\n      RUN_MIGRATIONS: 'false'\n");
  assert.throws(() => validateProductionCompose(duplicated), /Duplicate compose environment key/);
  assert.throws(() => validateProductionCompose(productionCompose.replace('\n  api:\n', '\n  api-v2:\n')), /exactly one api block/);
  assert.throws(() => validateProductionCompose(''), /exactly one services block/);
});

test('validateProductionCompose requires durable private internal feedback storage', () => {
  const root = '      TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT: /data/assets/.internal-feedback\n';
  for (const replacement of [
    '',
    '      TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT: /data/assets/public-feedback\n',
    '      TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT: /tmp/assets/.internal-feedback\n',
  ]) {
    assert.throws(
      () => validateProductionCompose(composeWith(root, replacement)),
      /TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT/,
    );
  }
  assert.throws(
    () => validateProductionCompose(composeWith('      - ./assets:/data/assets\n', '')),
    /mounted volume/,
  );
});

test('runtime preflight preserves a coherent captured contextual reputation gate', () => {
  const values = {
    RUN_MIGRATIONS: 'false',
    AUTO_APPLY_PRODUCTION_MIGRATIONS: 'true',
    CONTEXTUAL_REPUTATION_ENABLED: 'false',
    REPUTATION_AGGREGATION_WORKER_ENABLED: 'false',
    REPUTATION_AGGREGATION_ENVIRONMENT: 'production',
    REPUTATION_AGGREGATION_MODE: 'simulation',
    EVENT_DISCOVERY_ENABLED: 'false',
    EVENT_DISCOVERY_AUTO_PUBLISH: 'false',
    SINGLE_FEATURE_ONBOARDING_EXPERIMENT_ENABLED: 'false',
    DEFAULT_LOCALE: 'es',
  };
  const rows = [
    { machineId: 'machine-a', values },
    { machineId: 'machine-b', values: { ...values } },
  ];

  assert.equal(captureContextualReputationGate(rows), false);
  assert.deepEqual(runtimeEnvBlockers(rows), []);
  assert.match(runtimeEnvBlockers([{ machineId: 'machine-a', values: { ...values, SINGLE_FEATURE_ONBOARDING_EXPERIMENT_ENABLED: 'true' } }])[0], /SINGLE_FEATURE_ONBOARDING_EXPERIMENT_ENABLED/);
  assert.deepEqual(runtimeEnvBlockers(rows, { contextualReputationEnabled: false }), []);
  assert.match(
    runtimeEnvBlockers(rows, { contextualReputationEnabled: true })[0],
    /CONTEXTUAL_REPUTATION_ENABLED/,
  );
  const enabledRows = rows.map(({ machineId, values: rowValues }) => ({
    machineId,
    values: { ...rowValues, CONTEXTUAL_REPUTATION_ENABLED: 'true' },
  }));
  assert.equal(captureContextualReputationGate(enabledRows), true);
  assert.match(runtimeEnvBlockers(enabledRows)[0], /CONTEXTUAL_REPUTATION_ENABLED/);
  assert.throws(
    () => captureContextualReputationGate([
      rows[0],
      { machineId: 'machine-b', values: { ...values, CONTEXTUAL_REPUTATION_ENABLED: 'true' } },
    ]),
    /same boolean CONTEXTUAL_REPUTATION_ENABLED/i,
  );
});

test('buildMigrationBatchSql enables psql fail-fast behavior and preserves migration order', () => {
  const sql = buildMigrationBatchSql([ticketMigration, discoveryMigration]);

  assert.match(sql, /\\set\s+ON_ERROR_STOP\s+(?:on|1)/i);
  assert.match(sql, /\\set\s+candidate_revision\s+0{40}/i);
  assert.equal((sql.match(/\\unset\s+run_code/g) ?? []).length, 2);
  assert.equal((sql.match(/\\unset\s+safety_threshold/g) ?? []).length, 2);
  assert.match(sql, /pg_try_advisory_lock/i);
  // A held lock must end the batch with a non-zero psql status: \quit cannot.
  assert.doesNotMatch(sql, /\\quit/i);
  assert.match(sql, /\\else\n\n\\echo Another production schema migration is active[^\n]*\n\nDO \$lock\$ BEGIN RAISE EXCEPTION 'Another production schema migration is active[^']*'; END \$lock\$;/);
  assert.doesNotMatch(sql, /SELECT\s+pg_advisory_lock\s*\(/i);
  assert.ok(sql.indexOf(ticketMigration) >= 0, 'ticket migration must be included');
  assert.ok(sql.indexOf(discoveryMigration) > sql.indexOf(ticketMigration), 'migration order must be preserved');
  assert.equal(sql.match(new RegExp(ticketMigration.replaceAll('.', '\\.'), 'g'))?.length, 1);
  assert.equal(sql.match(new RegExp(discoveryMigration.replaceAll('.', '\\.'), 'g'))?.length, 1);
});

test('buildMigrationBatchSql validates every path before rendering psql input', () => {
  assert.throws(
    () => buildMigrationBatchSql([ticketMigration, 'tdf-hq/sql/../secrets.sql']),
    /migration|path|unsafe/i,
  );
});

test('buildMigrationBatchSql commits each ledger row together with its migration', () => {
  const migration = { id: 'atomic-test', path: 'tdf-hq/sql/2026-10-12_atomic_test.sql', checksum: 'a'.repeat(64),
    content: 'CREATE TABLE atomic_test ();' };
  const sql = buildMigrationBatchSql([migration]);
  const block = sql.slice(sql.indexOf('\\if :apply_migration_1'), sql.indexOf('\\else'));
  const order = ['BEGIN;', 'INSERT INTO public.tdf_schema_migration', migration.content, 'COMMIT;'].map(part => block.indexOf(part));
  assert.ok(order.every(index => index >= 0) && order.every((index, n) => n === 0 || index > order[n - 1]),
    'the ledger insert must sit inside the transaction, before the migration source');
  assert.equal((block.match(/INSERT INTO public\.tdf_schema_migration/g) ?? []).length, 1);
});

test('every manifest migration can run inside the batch transaction', async () => {
  // Precondition of the atomic ledger row: a file with its own transaction ends
  // with its COMMIT, so nothing escapes the transaction opened by the batch, and
  // no statement refuses a transaction block. Checked on the expanded SQL the
  // batch runs, includes resolved.
  const manifest = JSON.parse(readFileSync(new URL('../production-migrations.json', import.meta.url), 'utf8'));
  const read = relative => readFileSync(new URL(`../../${relative}`, import.meta.url), 'utf8');
  for (const entry of manifest.migrations) {
    const content = await expandMigrationIncludes({ path: entry.path, content: read(entry.path) }, read);
    const lines = content.split('\n').map(line => line.trim()).filter(line => line && !line.startsWith('--'));
    assert.doesNotMatch(content, /\bCONCURRENTLY\b|^\s*(?:VACUUM|REINDEX|CLUSTER|DISCARD|LISTEN|UNLISTEN)\b|ALTER\s+SYSTEM|(?:CREATE|DROP)\s+(?:DATABASE|TABLESPACE|SUBSCRIPTION)|ALTER\s+TYPE[^;]*\bADD\s+VALUE|^\s*(?:START\s+TRANSACTION|SET\s+TRANSACTION)\b|^\s*BEGIN\s+(?:ISOLATION|READ|DEFERRABLE)|\r/im,
      `${entry.id} uses a statement that cannot run inside the batch transaction`);
    const begins = content.match(/^\s*BEGIN(?:\s+(?:TRANSACTION|WORK))?\s*;/gim) ?? [];
    // END is not counted as COMMIT: PL/pgSQL blocks end with it. A file that
    // used END as the commit synonym would fail the tail check below.
    const commits = content.match(/^\s*COMMIT(?:\s+(?:TRANSACTION|WORK))?\s*;/gim) ?? [];
    assert.ok(begins.length <= 1 && commits.length <= 1 && !lines.some(line => /^(?:ROLLBACK|SAVEPOINT|PREPARE\s+TRANSACTION)\b/i.test(line)),
      `${entry.id} must hold at most one transaction and never roll it back`);
    if (begins.length) {
      assert.equal(commits.length, 1, `${entry.id} opens a transaction it does not commit`);
      assert.match(lines.at(-1), /^COMMIT(?:\s+(?:TRANSACTION|WORK))?\s*;$/i, `${entry.id} has statements after its COMMIT`);
    }
  }
});

test('buildMigrationBatchSql verifies the pre-designation social shape only on explicit opt-out', () => {
  const before = {
    id: '2026-10-25_event_memory_invitations',
    path: 'tdf-hq/sql/2026-10-25_event_memory_invitations.sql',
    checksum: '1'.repeat(64),
    content: 'SELECT 1;',
  };
  const designation = {
    id: '2026-11-01_social_sync_account_delivery_designation',
    path: 'tdf-hq/sql/2026-11-01_social_sync_account_delivery_designation.sql',
    checksum: '2'.repeat(64),
    content: 'SELECT 2;',
  };
  const prefix = buildMigrationBatchSql([before], { socialDesignationIncluded: false });
  const full = buildMigrationBatchSql([before, designation]);
  // Without the explicit opt-out a batch verifies the current production shape.
  assert.match(buildMigrationBatchSql([before]), /table_name = 'social_sync_account'\n\s*\) <> 17 OR/);
  const socialCount = (sql) => sql.match(/table_name = 'social_sync_account'\n\s*\) <> (\d+) OR/)[1];
  const socialForeignKeys = (sql) => sql.match(/<> (\d+) THEN\n\s*RAISE EXCEPTION 'A social-sync foreign key/)[1];
  assert.equal(socialCount(prefix), '13');
  assert.equal(socialForeignKeys(prefix), '7');
  assert.doesNotMatch(prefix, /'social_sync_account', 'delivery_designated'/);
  assert.equal(socialCount(full), '17');
  assert.equal(socialForeignKeys(full), '9');
  assert.match(full, /'social_sync_account', 'designated_by_party_id', 'bigint', 'YES'/);
  // Production verification outside a batch always expects the full shape.
  assert.equal(socialCount(buildSchemaVerificationSql()), '17');
});

test('buildMigrationBatchSql verifies the ADR 0134 marketplace offer schema unless a prefix opts out', () => {
  const before = {
    id: '2026-10-25_event_memory_invitations',
    path: 'tdf-hq/sql/2026-10-25_event_memory_invitations.sql',
    checksum: '1'.repeat(64),
    content: 'SELECT 1;',
  };
  const offers = {
    id: '2026-11-08_marketplace_offers',
    path: 'tdf-hq/sql/2026-11-08_marketplace_offers.sql',
    checksum: '3'.repeat(64),
    content: 'SELECT 3;',
  };
  const marketplaceCheck = /Marketplace offer schema \(ADR 0134\) is missing/;
  // Without the explicit opt-out every batch verifies the current production shape.
  assert.match(buildMigrationBatchSql([before]), marketplaceCheck);
  assert.match(buildMigrationBatchSql([before, offers]), marketplaceCheck);
  const prefix = buildMigrationBatchSql([before], { socialDesignationIncluded: false, marketplaceOffersIncluded: false });
  assert.doesNotMatch(prefix, marketplaceCheck);
  assert.doesNotMatch(prefix, /classified_slots_remaining_check is missing/);
  assert.match(prefix, /ADR 0134: marketplace offers verified only when the batch applies 2026-11-08_marketplace_offers/);
  // Production verification outside a batch always expects the marketplace schema.
  assert.match(buildSchemaVerificationSql(), marketplaceCheck);
  assert.match(buildSchemaVerificationSql(), /classified_application rows without an initial negotiation round/);
});

test('buildMigrationBatchSql verifies the ADR 0135 marketplace agreement schema unless a prefix opts out', () => {
  const before = {
    id: '2026-10-25_event_memory_invitations',
    path: 'tdf-hq/sql/2026-10-25_event_memory_invitations.sql',
    checksum: '1'.repeat(64),
    content: 'SELECT 1;',
  };
  const agreementCheck = /Marketplace agreement schema \(ADR 0135\) is missing/;
  assert.match(buildMigrationBatchSql([before]), agreementCheck);
  assert.match(buildMigrationBatchSql([before]), /Marketplace agreement default pricing rule is missing/);
  const prefix = buildMigrationBatchSql([before], { socialDesignationIncluded: false, marketplaceOffersIncluded: false, marketplaceAgreementsIncluded: false });
  assert.doesNotMatch(prefix, agreementCheck);
  assert.match(prefix, /ADR 0135: marketplace agreements verified only when the batch applies 2026-11-12_marketplace_agreements/);
  // Opting out of agreements alone keeps the offers verification.
  const offersOnly = buildMigrationBatchSql([before], { marketplaceAgreementsIncluded: false });
  assert.match(offersOnly, /Marketplace offer schema \(ADR 0134\) is missing/);
  assert.doesNotMatch(offersOnly, agreementCheck);
  assert.match(buildSchemaVerificationSql(), agreementCheck);
});

test('buildMigrationBatchSql rejects unexpanded include directives', () => {
  assert.throws(
    () => buildMigrationBatchSql([{
      id: 'include-test',
      path: 'tdf-hq/sql/2026-08-14_include_test.sql',
      checksum: '0'.repeat(64),
      content: '\\ir 2026-08-14_source.sql',
    }]),
    /unexpanded include/i,
  );
});

test('buildSchemaVerificationSql fails closed over every registered runtime schema contract', () => {
  const sql = buildSchemaVerificationSql();
  assert.match(sql, /interaction_report_reasons/);
  assert.match(sql, /mention_party_ids/);
  assert.match(sql, /last_event_id/);

  assert.match(sql, /\\set\s+ON_ERROR_STOP\s+(?:on|1)/i);
  for (const triggerMarker of [
    'checkout_environment',
    'attempt_provider',
    'checkout_status',
    'package_release_version_id',
    'delivery_environment',
    'evidence_delivery_attempt_id',
    'statement_net_minor',
  ]) {
    assert.match(sql, new RegExp(triggerMarker));
  }
  for (const requiredObject of [
    'event_ticket_order',
    'checkout_idempotency_key',
    'unique_event_ticket_checkout',
    'uq_event_ticket_order_stripe_payment_intent',
    'index_chat_message_thread',
    'index_course_registration_party',
    'index_course_registration_follow_up_next',
    'index_course_registration_follow_up_party',
    'index_course_registration_follow_up_registration',
    'index_course_registration_receipt_party',
    'index_course_registration_receipt_registration',
    'index_whats_app_message_party',
    'index_whats_app_message_phone',
    'A declared index is unique, missing or invalid',
    'external_venue_ref',
    'external_artist_ref',
    'external_event_ref',
    'external_event_discovery_run',
    'idx_external_event_ref_city',
    'idx_external_event_ref_event_id',
    'event_city',
    'event_city_subscription',
    'event_discovery_source',
    'unique_external_event_discovery_slot',
    'social_sync_account',
    'social_sync_post',
    'social_sync_run',
    'social_discovery_review',
    'unique_social_sync_account',
    'unique_social_sync_post',
    'unique_social_discovery_review',
    'artist_profile_enrichment',
    'artist_inventory_reference',
    'artist_research_source',
    'artist_enrichment_suggestion',
    'artist_field_change',
    'artist_enrichment_run',
    'artist_identity_candidate',
    'artist_media_asset',
    'uq_artist_profile_slug_ci',
    'uq_artist_enrichment_active_full_run',
    'unique_artist_media_drive_file',
    'catalog_release',
    'ddex_document',
    'message_id',
    'sender_id',
    'recipient_id',
    'campaign_automation',
    'campaign_delivery',
    'notification_notif_type_check',
    'notification_navigation_backfill',
    'notification_navigation_constraint_history',
    "column_name='target_key'",
    'access_request_submitted',
    'access_request_review',
    'access_request_decided',
    'feature_access_requests',
    'feature_access_request_history',
    'feature_navigation_preferences',
    'commerce_checkout_session',
    'commerce_payment_attempt',
    'commerce_provider_binding',
    'commerce_provider_event_inbox',
    'commerce_provider_operation',
    'evidence_type',
    'ck_commerce_provider_event_evidence',
    'commerce_guard_provider_operation_immutable',
    'trg_commerce_provider_operation_immutable',
    'idx_commerce_provider_event_untrusted_work',
    'idx_commerce_provider_operation_reconciliation',
    'uq_commerce_provider_operation_create',
    'commerce_refund',
    'commerce_refund_allocation',
    'commerce_refund_reason_code',
    'commerce_reconciliation_exception',
    'commerce_manual_payment_evidence',
    'submitted_by',
    'commerce_manual_payment_evidence_review_report',
    'fk_commerce_manual_evidence_submitted_by',
    'trg_commerce_validate_manual_payment_evidence',
    'checkout.bank_transfer',
    'checkout.cash',
    'checkout.pos',
    'service_storefront_order',
    'checkout_id',
    'fk_service_storefront_order_checkout',
    'service_storefront_checkout_backfill_report',
    'uq_commerce_succeeded_attempt_checkout',
    'marketplace_sale_order_runtime',
    'marketplace_sale_fulfillment_event',
    'marketplace_sale_checkout_backfill_report',
    'marketplace_rental_listing_terms',
    'marketplace_rental_cart_selection',
    'marketplace_rental_listing_terms_history',
    'marketplace_rental_order_runtime',
    'marketplace_rental_event',
    'marketplace_order_checkout_runtime',
    'idx_marketplace_sale_runtime_status',
    'idx_marketplace_rental_runtime_status',
    'idx_marketplace_rental_terms_history',
    'trg_marketplace_validate_sale_runtime',
    'trg_marketplace_validate_rental_runtime',
    'trg_marketplace_record_rental_terms_history',
    'trg_marketplace_validate_rental_transition',
    'trg_marketplace_record_rental_transition',
    'trg_marketplace_sync_verified_checkout',
    'trg_marketplace_protect_canonical_payment_state',
    'commerce.marketplace_sales',
    'commerce.marketplace_rentals',
    'service_booking_commerce_policy',
    'service_booking_checkout_runtime',
    'service_booking_resource_allocation',
    'service_booking_event',
    'idx_service_booking_runtime_status',
    'trg_service_booking_validate_runtime',
    'trg_service_booking_require_verified_payment',
    'service_booking_expire_holds',
    'commerce.service_bookings',
    'course_checkout_policy',
    'course_checkout_policy_history',
    'course_registration_checkout_runtime',
    'course_enrollment_event',
    'idx_course_registration_runtime_capacity',
    'trg_course_checkout_validate_runtime',
    'trg_course_checkout_require_verified_payment',
    'trg_course_registration_require_canonical_payment',
    'course_checkout_expire_holds',
    'commerce.courses',
    'commerce.course_recurring_billing',
    'fk_commerce_provider_event_checkout',
    'idx_commerce_provider_event_work',
    'trg_commerce_validate_refund_write',
    'uq_commerce_credit_note_refund',
    'checkout.paypal.webhooks',
    'checkout.paypal.refunds',
    'checkout.datafast.webhooks',
    'checkout.datafast.refunds',
    'checkout.placetopay.webhooks',
    'checkout.payphone.notifications',
    'catalog_definition',
    'catalog_backfill_run',
    'security_permission',
    'party_security_role',
    'country_reference',
    'record_release',
    'ddex_standard_version',
    'standard_version_id',
    'workflow_state_id',
    'social_event_time_order',
    'end_time',
    'allowed_versions',
    'records-cms-cutover-2026-08-07',
    'ddex-operational-cutover-2026-08-12',
    'user_onboarding_progress',
    'user_onboarding_progress_eligible_idx',
    'user_experiment_assignment',
    'user_experiment_assignment_pending_exposure_idx',
  ]) {
    assert.match(sql, new RegExp(requiredObject), `verification must inspect ${requiredObject}`);
  }
  assert.match(
    sql,
    /service_booking_expire_holds[\s\S]*failed/,
    'schema verification must prove failed booking payment attempts expire',
  );
  assert.match(
    sql,
    /course_checkout_expire_holds[\s\S]*target_course_id[\s\S]*failed/,
    'schema verification must prove course-scoped failed payment attempt expiry',
  );
  assert.match(sql, /RAISE\s+EXCEPTION|\\quit/i, 'schema drift must terminate verification');
  assert.match(sql, /social_event[\s\S]*end_time[\s\S]*is_nullable\s*=\s*'YES'/i);
  assert.match(sql, /social_event_time_order[\s\S]*convalidated/i);
  assert.match(
    sql,
    /IF EXISTS \([\s\S]*notification_notif_type_check[\s\S]*\) AND NOT EXISTS \([\s\S]*access_request_submitted/i,
    'the historical unconstrained notification baseline must remain valid while a named allowlist fails closed',
  );
  assert.match(
    sql,
    /conname = 'notification_notif_type_check'[\s\S]*OR \(contype = 'c' AND 3 = ANY \(conkey\)\)/i,
    'unvalidated named and differently named notification allowlists must not be mistaken for no allowlist',
  );
});

test('buildSchemaPreflightSql is read-only and accepts unapplied release tables', () => {
  const sql = buildSchemaPreflightSql();

  assert.match(sql, /BEGIN READ ONLY/i);
  assert.match(sql, /default_transaction_read_only/i);
  assert.match(sql, /information_schema\.columns[\s\S]*checkout_idempotency_key/i);
  assert.match(sql, /social_sync_account[\s\S]*social_sync_post[\s\S]*social_sync_run/i);
  assert.match(sql, /campaign_automation[\s\S]*campaign_delivery/i);
  assert.match(sql, /feature_access_requests[\s\S]*feature_navigation_preferences/i);
  assert.match(sql, /course_checkout_policy[\s\S]*course_enrollment_event/i);
  assert.match(sql, /ROLLBACK/i);
  assert.doesNotMatch(sql, /ALTER\s+TABLE|CREATE\s+TABLE|INSERT\s+INTO|UPDATE\s+|DELETE\s+FROM/i);
});


test('discovery gates are captured from a coherent fleet without enabling or disabling either flag', () => {
  for (const enabled of [false, true]) {
    for (const autoPublish of [false, true]) {
      const values = {
        EVENT_DISCOVERY_ENABLED: String(enabled),
        EVENT_DISCOVERY_AUTO_PUBLISH: String(autoPublish),
      };
      const rows = [{ machineId: 'a', values }, { machineId: 'b', values: { ...values } }];
      const gates = captureEventDiscoveryGates(rows);
      assert.deepEqual(gates, { eventDiscoveryEnabled: enabled, eventDiscoveryAutoPublish: autoPublish });
    }
  }
});

test('discovery preflight rejects inconsistent, unset or malformed fleet gates', () => {
  const values = { EVENT_DISCOVERY_ENABLED: 'true', EVENT_DISCOVERY_AUTO_PUBLISH: 'true' };
  assert.throws(() => captureEventDiscoveryGates([]), /same boolean EVENT_DISCOVERY_ENABLED/);
  assert.throws(() => captureEventDiscoveryGates([{ values: {} }]), /same boolean EVENT_DISCOVERY_ENABLED/);
  assert.throws(() => captureEventDiscoveryGates([{ values }, { values: { ...values, EVENT_DISCOVERY_ENABLED: 'false' } }]), /same boolean EVENT_DISCOVERY_ENABLED/);
  assert.throws(() => captureEventDiscoveryGates([{ values }, { values: { ...values, EVENT_DISCOVERY_AUTO_PUBLISH: 'false' } }]), /same boolean EVENT_DISCOVERY_AUTO_PUBLISH/);
  assert.throws(() => captureEventDiscoveryGates([{ values: { ...values, EVENT_DISCOVERY_AUTO_PUBLISH: 'yes' } }]), /same boolean EVENT_DISCOVERY_AUTO_PUBLISH/);
});

test('runtime checks accept captured discovery settings and reject subsequent drift', () => {
  const values = {
    RUN_MIGRATIONS: 'false', AUTO_APPLY_PRODUCTION_MIGRATIONS: 'true',
    CONTEXTUAL_REPUTATION_ENABLED: 'false', REPUTATION_AGGREGATION_WORKER_ENABLED: 'false',
    REPUTATION_AGGREGATION_ENVIRONMENT: 'production', REPUTATION_AGGREGATION_MODE: 'simulation',
    EVENT_DISCOVERY_ENABLED: 'true', EVENT_DISCOVERY_AUTO_PUBLISH: 'true', DEFAULT_LOCALE: 'es',
    SINGLE_FEATURE_ONBOARDING_EXPERIMENT_ENABLED: 'false',
  };
  const rows = [{ machineId: 'a', values }];
  const gates = captureEventDiscoveryGates(rows);
  assert.deepEqual(runtimeEnvBlockers(rows, gates), []);
  assert.equal(runtimeEnvBlockers(rows).length, 2);
  const changed = [{ machineId: 'a', values: { ...values, EVENT_DISCOVERY_AUTO_PUBLISH: 'false' } }];
  assert.match(runtimeEnvBlockers(changed, gates)[0], /EVENT_DISCOVERY_AUTO_PUBLISH/);
  assert.throws(() => runtimeEnvBlockers(rows, { eventDiscoveryEnabled: 'true' }), /must be a boolean/);
  assert.throws(() => runtimeEnvBlockers(rows, { eventDiscoveryAutoPublish: 'false' }), /must be a boolean/);
});

test('schema verification covers every music SQL function called at runtime', () => {
  const sqlDirectory = new URL('../../tdf-hq/sql/', import.meta.url);
  const defined = new Set();
  for (const name of readdirSync(sqlDirectory)) {
    if (!name.endsWith('.sql') || name.includes('rollback')) continue;
    const source = readFileSync(new URL(name, sqlDirectory), 'utf8');
    for (const match of source.matchAll(/CREATE\s+(?:OR\s+REPLACE\s+)?FUNCTION\s+(?:public\.)?(music_[a-z0-9_]+)/gi)) {
      defined.add(match[1].toLowerCase());
    }
  }
  const sourceRoots = [
    ['../../tdf-hq/src/', /\.hs$/],
    ['../', /\.(?:sh|mjs)$/],
  ];
  const called = new Set();
  for (const [root, pattern] of sourceRoots) {
    const directory = new URL(root, import.meta.url);
    for (const entry of readdirSync(directory, { recursive: true })) {
      const relative = String(entry);
      if (!pattern.test(relative) || relative.includes('__tests__') || relative.includes('node_modules')) continue;
      const source = readFileSync(new URL(relative, directory), 'utf8');
      for (const match of source.matchAll(/\b(music_[a-z0-9_]+)\s*\(/g)) {
        if (defined.has(match[1])) called.add(match[1]);
      }
    }
  }
  const verified = new Set(MUSIC_RUNTIME_FUNCTION_SIGNATURES.map((signature) => signature.slice(0, signature.indexOf('('))));
  assert.ok(called.size > 0, 'expected to find music function calls in source');
  assert.deepEqual([...called].filter((name) => !verified.has(name)).sort(), [],
    'music functions called at runtime but not verified by the release schema gate');
  assert.deepEqual([...verified].filter((name) => !defined.has(name)).sort(), [],
    'verified music functions that no forward migration defines');
  const sql = buildSchemaVerificationSql();
  for (const signature of MUSIC_RUNTIME_FUNCTION_SIGNATURES) {
    assert.ok(sql.includes(`'${signature}'`), `schema verification omits ${signature}`);
  }
});
