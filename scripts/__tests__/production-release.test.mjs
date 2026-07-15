import test from 'node:test';
import assert from 'node:assert/strict';

import {
  buildDeployPlan,
  buildMigrationBatchSql,
  buildReleaseSteps,
  buildSchemaPreflightSql,
  buildSchemaVerificationSql,
  normalizeFullSha,
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

test('buildSchemaVerificationSql fails closed over the ticketing and discovery schema contract', () => {
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
  ]) {
    assert.match(sql, new RegExp(requiredObject), `verification must inspect ${requiredObject}`);
  }
  assert.match(sql, /RAISE\s+EXCEPTION|\\quit/i, 'schema drift must terminate verification');
});

test('buildSchemaPreflightSql is read-only and accepts an unapplied ticket idempotency column', () => {
  const sql = buildSchemaPreflightSql();

  assert.match(sql, /BEGIN READ ONLY/i);
  assert.match(sql, /default_transaction_read_only/i);
  assert.match(sql, /information_schema\.columns[\s\S]*checkout_idempotency_key/i);
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
