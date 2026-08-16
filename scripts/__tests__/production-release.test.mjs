import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';

import {
  buildDatabaseSqlInvocation,
  buildDeployPlan,
  buildMigrationBatchSql,
  buildReleaseSteps,
  buildSchemaPreflightSql,
  buildSchemaVerificationSql,
  expandMigrationIncludes,
  normalizeFullSha,
  parseSecurityEmergencyReadinessOutput,
  requireMigrationIntroductionAncestor,
  securityEmergencyReadinessBlocker,
  validateFlyConfig,
  validateMigrationRelativePath,
} from '../lib/production-release.mjs';

const releaseSha = 'ABCDEF0123456789ABCDEF0123456789ABCDEF01';
const normalizedReleaseSha = releaseSha.toLowerCase();
const releaseImage = `diegueins680/tdf-hq:${normalizedReleaseSha}`;

const ticketMigration = 'tdf-hq/sql/2026-07-12_ticket_checkout_idempotency.sql';
const discoveryMigration = 'tdf-hq/sql/2026-07-12_event_discovery_imports.sql';

const safeFlyConfig = `
app = "tdf-hq"
primary_region = "gru"

[env]
  APP_PORT = "8080"
  RUN_MIGRATIONS = "false"
  EVENT_DISCOVERY_ENABLED = "false"

[deploy]
  strategy = "rolling"
  max_unavailable = 1
  wait_timeout = "10m"

[[services]]
  protocol = "tcp"
  internal_port = 8080

  [[services.http_checks]]
    interval = "10s"
    grace_period = "15s"
    method = "get"
    path = "/health"
    protocol = "http"
    timeout = "2s"
`;

function releaseOptions(overrides = {}) {
  return {
    app: 'tdf-hq',
    canaryMachineId: 'canary-machine',
    dryRun: false,
    flyConfig: safeFlyConfig,
    image: releaseImage,
    migrations: [ticketMigration, discoveryMigration],
    priorImages: {
      'canary-machine': 'registry.fly.io/tdf-hq:deployment-old-canary',
      'remaining-machine': 'registry.fly.io/tdf-hq:deployment-old-remaining',
    },
    priorShas: {
      'canary-machine': '1111111111111111111111111111111111111111',
      'remaining-machine': '2222222222222222222222222222222222222222',
    },
    remainingMachineIds: ['remaining-machine'],
    sha: releaseSha,
    ...overrides,
  };
}

function commandText(step) {
  assert.ok(Array.isArray(step.command), `${step.id} command must be an argv array`);
  return step.command.join(' ');
}

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
  for (const migration of manifest.migrations) {
    assert.equal(normalizeFullSha(migration.introducedBy), migration.introducedBy);
  }

  const resumeIndex = manifest.migrations.findIndex(
    ({ id }) => id === '2026-08-16_catalog_locale_preference_resume',
  );
  const backfillIndex = manifest.migrations.findIndex(
    ({ id }) => id === '2026-08-07_catalog_backfill_apply',
  );
  assert.ok(resumeIndex >= 0, 'resume migration must be registered');
  assert.equal(backfillIndex, resumeIndex + 1, 'resume migration must run immediately before backfill');
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

test('production database SQL streams outside the flyctl argument vector', () => {
  const sql = `SELECT '${'x'.repeat(1_500_000)}';`;
  const invocation = buildDatabaseSqlInvocation({
    dbApp: 'tdf-hq-db',
    database: 'tdf_hq',
  }, sql, { tuplesOnly: true });

  assert.equal(invocation.input, sql);
  assert.ok(invocation.argv.every((argument) => argument.length < 1_024));
  assert.doesNotMatch(invocation.argv.join(' '), /base64|SELECT 'x/u);
  assert.match(invocation.argv.at(-1), /psql -X -v ON_ERROR_STOP=1 -qAt -p 5433 -d tdf_hq/u);
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

test('catalog resume migration only relaxes copied preference evidence columns', () => {
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

test('validateFlyConfig accepts an explicit migration-free staged rolling release', () => {
  const validation = validateFlyConfig(safeFlyConfig);

  assert.equal(validation.runMigrations, false);
  assert.equal(validation.eventDiscoveryEnabled, false);
  assert.equal(validation.healthCheckPath, '/health');
  assert.equal(validation.strategy, 'rolling');
  assert.equal(validation.maxUnavailable, 1);
});

test('validateFlyConfig fails closed when startup migrations are enabled', () => {
  assert.throws(
    () => validateFlyConfig(safeFlyConfig.replace('RUN_MIGRATIONS = "false"', 'RUN_MIGRATIONS = "true"')),
    /RUN_MIGRATIONS|migration/i,
  );
});

test('validateFlyConfig fails closed when event discovery would start during the initial release', () => {
  assert.throws(
    () => validateFlyConfig(
      safeFlyConfig.replace('EVENT_DISCOVERY_ENABLED = "false"', 'EVENT_DISCOVERY_ENABLED = "true"'),
    ),
    /EVENT_DISCOVERY_ENABLED|discovery/i,
  );
});

test('validateFlyConfig requires an HTTP readiness check on /health', () => {
  const withoutHealthCheck = safeFlyConfig.replace(/\n  \[\[services\.http_checks\]\][\s\S]*$/, '\n');

  assert.throws(() => validateFlyConfig(withoutHealthCheck), /health|http.*check|readiness/i);
  assert.throws(
    () => validateFlyConfig(safeFlyConfig.replace('path = "/health"', 'path = "/version"')),
    /health|readiness/i,
  );
});

test('validateFlyConfig requires a one-at-a-time rolling deployment', () => {
  assert.throws(
    () => validateFlyConfig(safeFlyConfig.replace('strategy = "rolling"', 'strategy = "canary"')),
    /rolling|canary|volume|strategy/i,
  );
  assert.throws(
    () => validateFlyConfig(safeFlyConfig.replace('max_unavailable = 1', 'max_unavailable = 2')),
    /max_unavailable|unavailable|one/i,
  );
});

test('validateFlyConfig reads safety keys from their actual TOML sections', () => {
  const spoofedEnv = safeFlyConfig
    .replace('RUN_MIGRATIONS = "false"', 'RUN_MIGRATIONS = "true"')
    .replace('[env]', '[build.args]\nRUN_MIGRATIONS = "false"\n\n[env]');
  const spoofedDeploy = safeFlyConfig
    .replace('strategy = "rolling"', 'strategy = "bluegreen"')
    .replace('max_unavailable = 1', 'max_unavailable = 2')
    .replace('[deploy]', '[release.metadata]\nstrategy = "rolling"\nmax_unavailable = 1\n\n[deploy]');

  assert.throws(() => validateFlyConfig(spoofedEnv), /RUN_MIGRATIONS|migration/i);
  assert.throws(() => validateFlyConfig(spoofedDeploy), /rolling|strategy|max_unavailable/i);
});

test('validateFlyConfig refuses a different Fly application', () => {
  assert.throws(
    () => validateFlyConfig(safeFlyConfig.replace('app = "tdf-hq"', 'app = "not-production"')),
    /tdf-hq|production|app/i,
  );
});

test('buildMigrationBatchSql enables psql fail-fast behavior and preserves migration order', () => {
  const sql = buildMigrationBatchSql([ticketMigration, discoveryMigration]);

  assert.match(sql, /\\set\s+ON_ERROR_STOP\s+(?:on|1)/i);
  assert.match(sql, /\\set\s+candidate_revision\s+0{40}/i);
  assert.equal((sql.match(/\\unset\s+run_code/g) ?? []).length, 2);
  assert.equal((sql.match(/\\unset\s+safety_threshold/g) ?? []).length, 2);
  assert.match(sql, /pg_try_advisory_lock/i);
  assert.match(sql, /\\quit\s+3/i);
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

  assert.match(sql, /\\set\s+ON_ERROR_STOP\s+(?:on|1)/i);
  for (const requiredObject of [
    'event_ticket_order',
    'checkout_idempotency_key',
    'unique_event_ticket_checkout',
    'uq_event_ticket_order_stripe_payment_intent',
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
    'feature_access_requests',
    'feature_access_request_history',
    'feature_navigation_preferences',
    'catalog_definition',
    'catalog_backfill_run',
    'security_permission',
    'party_security_role',
    'country_reference',
    'record_release',
    'ddex_standard_version',
    'standard_version_id',
    'workflow_state_id',
    'allowed_versions',
    'records-cms-cutover-2026-08-07',
    'ddex-operational-cutover-2026-08-12',
  ]) {
    assert.match(sql, new RegExp(requiredObject), `verification must inspect ${requiredObject}`);
  }
  assert.match(sql, /RAISE\s+EXCEPTION|\\quit/i, 'schema drift must terminate verification');
});

test('buildSchemaPreflightSql is read-only and accepts unapplied release tables', () => {
  const sql = buildSchemaPreflightSql();

  assert.match(sql, /BEGIN READ ONLY/i);
  assert.match(sql, /default_transaction_read_only/i);
  assert.match(sql, /information_schema\.columns[\s\S]*checkout_idempotency_key/i);
  assert.match(sql, /social_sync_account[\s\S]*social_sync_post[\s\S]*social_sync_run/i);
  assert.match(sql, /campaign_automation[\s\S]*campaign_delivery/i);
  assert.match(sql, /feature_access_requests[\s\S]*feature_navigation_preferences/i);
  assert.match(sql, /ROLLBACK/i);
  assert.doesNotMatch(sql, /ALTER\s+TABLE|CREATE\s+TABLE|INSERT\s+INTO|UPDATE\s+|DELETE\s+FROM/i);
});

test('buildReleaseSteps orders schema work before a single-machine canary and fleet rollout', () => {
  const steps = buildReleaseSteps(releaseOptions());
  const ids = steps.map(({ id }) => id);

  for (const requiredId of [
    'local-preflight',
    'remote-preflight',
    'apply-migrations',
    'verify-schema',
    'deploy-canary',
    'smoke-canary',
    'deploy-remaining-1',
    'smoke-remaining-1',
    'verify-fleet',
  ]) {
    assert.ok(ids.includes(requiredId), `missing ${requiredId} release step`);
  }

  assert.ok(ids.indexOf('remote-preflight') < ids.indexOf('apply-migrations'));
  assert.ok(ids.indexOf('apply-migrations') < ids.indexOf('verify-schema'));
  assert.ok(ids.indexOf('verify-schema') < ids.indexOf('deploy-canary'));
  assert.ok(ids.indexOf('deploy-canary') < ids.indexOf('smoke-canary'));
  assert.ok(ids.indexOf('smoke-canary') < ids.indexOf('deploy-remaining-1'));
  assert.ok(ids.indexOf('deploy-remaining-1') < ids.indexOf('smoke-remaining-1'));
  assert.ok(ids.indexOf('smoke-remaining-1') < ids.indexOf('verify-fleet'));

  const canaryCommand = commandText(steps.find(({ id }) => id === 'deploy-canary'));
  assert.match(canaryCommand, /--only-machines canary-machine(?:\s|$)/);
  assert.match(canaryCommand, new RegExp(`--image ${releaseImage}`));
  assert.match(canaryCommand, /RUN_MIGRATIONS=false/);
  assert.match(canaryCommand, /EVENT_DISCOVERY_ENABLED=false/);
  assert.doesNotMatch(canaryCommand, /--strategy canary(?:\s|$)/);

  const remainingCommand = commandText(steps.find(({ id }) => id === 'deploy-remaining-1'));
  assert.match(remainingCommand, /--only-machines remaining-machine(?:\s|$)/);
  assert.doesNotMatch(remainingCommand, /--exclude-machines/);
  assert.match(remainingCommand, /--strategy rolling(?:\s|$)/);
  assert.match(remainingCommand, /--max-unavailable 1(?:\s|$)/);
});

test('buildReleaseSteps rolls the canary back to its captured image before any remaining-machine rollout', () => {
  const steps = buildReleaseSteps(releaseOptions());
  const smoke = steps.find(({ id }) => id === 'smoke-canary');
  const rolloutIndex = steps.findIndex(({ id }) => id === 'deploy-remaining-1');

  assert.ok(smoke, 'missing canary smoke step');
  assert.ok(Array.isArray(smoke.onFailure), 'canary smoke must declare failure actions');

  const rollback = smoke.onFailure.find(({ id }) => id === 'rollback-canary');
  assert.ok(rollback, 'canary smoke failure must declare a rollback step');
  assert.equal(rollback.mutating, true);
  assert.equal(rollback.beforeStep, 'deploy-remaining');
  assert.ok(steps.indexOf(smoke) < rolloutIndex, 'smoke must run before remaining-machine rollout');

  const rollbackCommand = commandText(rollback);
  assert.match(rollbackCommand, /--only-machines canary-machine(?:\s|$)/);
  assert.match(rollbackCommand, /--image registry\.fly\.io\/tdf-hq:deployment-old-canary(?:\s|$)/);
  assert.doesNotMatch(rollbackCommand, new RegExp(releaseImage));
});

test('buildReleaseSteps refuses executable placeholders without rollback metadata', () => {
  assert.throws(
    () => buildReleaseSteps({
      app: 'tdf-hq',
      dryRun: false,
      flyConfig: safeFlyConfig,
      image: releaseImage,
      migrations: [ticketMigration],
      sha: releaseSha,
    }),
    /canary|explicit|machine/i,
  );
  assert.throws(
    () => buildReleaseSteps(releaseOptions({ priorImages: {}, priorShas: {} })),
    /rollback|previous|image|commit/i,
  );
});

test('buildReleaseSteps refuses a canary when no untouched Machine remains', () => {
  assert.throws(
    () => buildReleaseSteps(releaseOptions({ remainingMachineIds: [] })),
    /canary|remaining|machine|two/i,
  );
});

test('buildDeployPlan dry-run is descriptive but contains no mutating commands', () => {
  const plan = buildDeployPlan(releaseOptions({ dryRun: true }));

  assert.equal(plan.dryRun, true);
  assert.equal(plan.mode, 'dry-run');
  assert.ok(plan.steps.length > 0, 'dry-run should still explain the release sequence');
  assert.deepEqual(plan.commands, []);
  assert.ok(plan.steps.every((step) => step.mutating !== true));
  assert.ok(plan.steps.every((step) => step.command === undefined));
  assert.match(JSON.stringify(plan), new RegExp(normalizedReleaseSha));
  assert.match(JSON.stringify(plan), /ticket_checkout_idempotency/);
  assert.match(JSON.stringify(plan), /event_discovery_imports/);
});

test('buildDeployPlan validates Fly safety settings before producing a release plan', () => {
  assert.throws(
    () => buildDeployPlan(
      releaseOptions({
        dryRun: true,
        flyConfig: safeFlyConfig.replace('RUN_MIGRATIONS = "false"', 'RUN_MIGRATIONS = "true"'),
      }),
    ),
    /RUN_MIGRATIONS|migration/i,
  );
});
