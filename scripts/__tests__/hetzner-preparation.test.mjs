import assert from 'node:assert/strict';
import { test } from 'node:test';
import { loadMigrationContract, reconcileMigrationLedger, sha256 } from '../lib/migration-contract.mjs';
import { prepareHetznerRelease } from '../lib/hetzner-preparation.mjs';

const revision = 'a'.repeat(40), introducedBy = 'b'.repeat(40), oldChecksum = 'c'.repeat(64);
const manifest = { schemaVersion: 1, migrations: [
  { id: 'first', path: 'tdf-hq/sql/first.sql', introducedBy },
  { id: 'second', path: 'tdf-hq/sql/second.sql', introducedBy, compatibleAppliedChecksums: [oldChecksum] },
] };
function sourceFixture(changed = manifest, overrides = {}) {
  const blobs = { 'scripts/production-migrations.json': JSON.stringify(changed),
    'tdf-hq/sql/first.sql': '\\ir included.sql\nSELECT 1;',
    'tdf-hq/sql/included.sql': 'SELECT 42;', 'tdf-hq/sql/second.sql': 'SELECT 2;', ...overrides };
  return (sha, name) => {
    assert.equal(sha, revision, 'Every include must use the immutable source revision');
    assert.ok(Object.hasOwn(blobs, name), `Unexpected source blob: ${name}`);
    return blobs[name];
  };
}
const load = () => loadMigrationContract(revision, sourceFixture(), async () => true);
const applied = (entry, checksum = entry.checksum) => ({ migration_id: entry.id, checksum, source_commit: introducedBy });

test('migration contract binds every expanded SQL include and introduction to one revision', async () => {
  const contract = await load();
  assert.equal(contract.migrations.length, 2);
  assert.match(contract.migrations[0].content, /SELECT 42/);
  assert.notEqual(contract.migrations[0].checksum, sha256(sourceFixture()(revision, 'tdf-hq/sql/first.sql')));
  const changed = await loadMigrationContract(revision,
    sourceFixture(manifest, { 'tdf-hq/sql/included.sql': 'SELECT 43;' }), async () => true);
  assert.notEqual(changed.migrations[0].checksum, contract.migrations[0].checksum);
  assert.equal(changed.migrations[1].checksum, contract.migrations[1].checksum);
});

test('migration source negative controls reject missing ancestry, duplicates, bad checksums and escaped includes', async () => {
  await assert.rejects(loadMigrationContract(revision, sourceFixture(), async () => false), /not an ancestor/);
  for (const mutate of [
    x => { x.migrations.push(x.migrations[0]); },
    x => { x.migrations[1].path = x.migrations[0].path; },
    x => { x.migrations[0].introducedBy = 'main'; },
    x => { x.migrations[0].path = 'tdf-hq/sql/../secret.sql'; },
    x => { x.migrations[1].compatibleAppliedChecksums = ['no']; },
    x => { x.migrations[1].compatibleAppliedChecksums = oldChecksum; },
    x => { x.migrations = []; },
    x => { x.schemaVersion = 2; },
  ]) {
    const input = structuredClone(manifest); mutate(input);
    await assert.rejects(loadMigrationContract(revision, sourceFixture(input), async () => true));
  }
  await assert.rejects(loadMigrationContract(revision,
    sourceFixture(manifest, { 'tdf-hq/sql/included.sql': '\\ir first.sql' }), async () => true), /Recursive/);
  await assert.rejects(loadMigrationContract(revision,
    sourceFixture(manifest, { 'tdf-hq/sql/included.sql': '\\ir ../private.sql' }), async () => true), /Unsupported/);
});

test('ledger reconciliation retains manifest order and does not skip an unapplied hole', async () => {
  const contract = await load();
  const ledger = [applied(contract.migrations[1], oldChecksum)];
  const result = reconcileMigrationLedger(contract, ledger);
  assert.deepEqual(result.map(x => [x.id, x.state]), [['first', 'pending'], ['second', 'applied']]);
  assert.equal(result[1].appliedChecksum, oldChecksum);
  assert.equal(result[1].checksum, contract.migrations[1].checksum);
  assert.equal(result[0].content, undefined, 'Plans contain hashes, not SQL or data');
  assert.equal(reconcileMigrationLedger(contract, [applied(contract.migrations[1]), applied(contract.migrations[0])])
    .filter(x => x.state === 'pending').length, 0);
});

test('ledger negative controls reject unknown, changed, duplicate and malformed history', async () => {
  const contract = await load(), row = applied(contract.migrations[0]);
  for (const ledger of [null, [row, row], [{ ...row, migration_id: 'unreviewed' }],
    [{ ...row, checksum: 'd'.repeat(64) }], [{ ...row, checksum: 'invalid' }],
    [{ ...row, source_commit: 'main' }]]) {
    assert.throws(() => reconcileMigrationLedger(contract, ledger));
  }
});

async function input() {
  const contract = await load();
  const container = { containerId: '1'.repeat(64), image: `vendor/image@sha256:${'2'.repeat(64)}`,
    imageId: `sha256:${'3'.repeat(64)}`, running: true };
  const provenance = { inspectorSha256: '4'.repeat(64), launcherSha256: '5'.repeat(64) };
  return { contract, provenance, now: Date.parse('2026-10-05T01:00:00Z'), receipt: {
    startedAt: '2026-10-05T00:59:50Z', observedAt: '2026-10-05T00:59:55Z', sourceRevision: revision,
    sourceWorktreeDirty: false, ...provenance, snapshot: {
      schemaVersion: 1, project: 'tdf-production', directory: '/opt/tdf/production',
      publicBackend: { name: 'tdf-hq', commit: introducedBy, version: '0.1.0.0' },
      database: { database: 'tdf_hq', role: 'tdf_catalog_inventory', readOnly: true, localConnection: true,
        migrations: [applied(contract.migrations[0])] },
      containers: { api: { ...container, booleanConfiguration: { RUN_MIGRATIONS: 'false', RESET_DB: 'false',
        SEED_DB: 'false', ALLOW_ALL_ORIGINS: 'true', ARTIST_ENRICHMENT_ENABLED: 'true' },
      missingBooleanConfiguration: ['CORS_DISABLE_DEFAULTS'] },
      db: { ...container, volume: 'tdf_production_postgres_data' }, edge: { ...container } },
    },
  } };
}

test('preparation records current drift and required CORS repair without enabling release or experiments', async () => {
  const args = await input(), before = structuredClone(args);
  const plan = prepareHetznerRelease(args);
  assert.deepEqual(args, before, 'Observational input is immutable');
  assert.equal(plan.executionAllowed, false);
  assert.equal(plan.status, 'prepared-not-authorized');
  assert.deepEqual(plan.counts, { applied: 1, pending: 1 });
  assert.equal(plan.observedFlags.ALLOW_ALL_ORIGINS, 'true');
  assert.equal(plan.requiredCorsConfiguration.ALLOW_ALL_ORIGINS, 'false');
  assert.equal(plan.observedFlags.ARTIST_ENRICHMENT_ENABLED, 'true');
  assert.equal(plan.requiredCorsConfiguration.ARTIST_ENRICHMENT_ENABLED, undefined);
  assert.ok(plan.remainingGates.some(x => x.includes('actual isolated restore')));
  assert.ok(plan.remainingGates.some(x => x.includes('all old writers')));
});

test('preparation negative controls reject stale/provenance/target/identity/destructive flag changes', async () => {
  const mutations = [
    x => { x.receipt.startedAt = '2026-10-04T00:00:00Z'; },
    x => { x.receipt.observedAt = '2026-10-05T01:00:01Z'; },
    x => { x.receipt.observedAt = '2026-10-05T00:59:49Z'; },
    x => { x.receipt.startedAt = 'invalid'; },
    x => { x.receipt.sourceRevision = 'main'; },
    x => { x.receipt.sourceWorktreeDirty = true; },
    x => { x.receipt.inspectorSha256 = '6'.repeat(64); },
    x => { x.receipt.launcherSha256 = '6'.repeat(64); },
    x => { x.receipt.snapshot.project = 'tdf-restore'; },
    x => { x.receipt.snapshot.directory = '/opt/tdf/restore'; },
    x => { x.receipt.snapshot.database.database = 'tdf_test'; },
    x => { x.receipt.snapshot.database.role = 'postgres'; },
    x => { x.receipt.snapshot.database.readOnly = false; },
    x => { x.receipt.snapshot.database.localConnection = false; },
    x => { x.receipt.snapshot.containers.db.volume = 'other'; },
    x => { x.receipt.snapshot.publicBackend.commit = 'main'; },
    x => { x.receipt.snapshot.publicBackend.name = 'other'; },
    x => { x.receipt.snapshot.containers.api.image = 'vendor/image:latest'; },
    x => { x.receipt.snapshot.containers.db.running = false; },
    x => { x.receipt.snapshot.containers.edge.containerId = 'short'; },
    x => { x.receipt.snapshot.containers.api.booleanConfiguration.RESET_DB = 'true'; },
    x => { x.receipt.snapshot.containers.api.booleanConfiguration.RUN_MIGRATIONS = 'true'; },
    x => { x.receipt.snapshot.containers.api.booleanConfiguration.SEED_DB = 'true'; },
    x => { x.receipt.snapshot.containers.api.booleanConfiguration.ALLOW_ALL_ORIGINS = 'unknown'; },
  ];
  for (const mutate of mutations) {
    const args = await input(); mutate(args); assert.throws(() => prepareHetznerRelease(args));
  }
});
