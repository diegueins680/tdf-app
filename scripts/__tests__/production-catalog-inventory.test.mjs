import test from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { validateProvenance } from '../production-catalog-inventory.mjs';
const health = { status: 'ok', db: 'ok' };
const version = { commit: 'a'.repeat(40) };
const deployment = { provider: 'hetzner', project: 'tdf-production', database: 'tdf_hq', health, version,
  apiContainer: 'api', databaseContainer: 'db', apiImage: 'digest', configuredImage: 'digest', databaseImage: 'pg17' };
const metadata = { database: 'tdf_hq', transactionReadOnly: 'on' };
test('accepts a stable current API/database deployment', () => {
  assert.doesNotThrow(() => validateProvenance(health, version, deployment, deployment, metadata));
});
test('refuses mismatched public origin, unhealthy database, stale target and deployment races', () => {
  for (const change of [{ version: { commit: 'b'.repeat(40) } }, { health: { status: 'ok', db: 'down' } },
    { database: 'trader' }, { provider: 'fly' }, { apiContainer: 'replaced' }, { databaseImage: 'replaced' }]) {
    assert.throws(() => validateProvenance(health, version, deployment, { ...deployment, ...change }, metadata));
  }
  assert.throws(() => validateProvenance(health, version, deployment, deployment, { ...metadata, transactionReadOnly: 'off' }));
  assert.throws(() => validateProvenance(health, version, deployment, deployment, { ...metadata, database: 'trader' }));
});
test('dry-run SQL keeps bounded read-only and sensitive-column exclusions', () => {
  const sql = execFileSync(process.execPath, ['scripts/production-catalog-inventory.mjs', '--dry-run'], { encoding: 'utf8' });
  assert.match(sql, /BEGIN TRANSACTION READ ONLY/);
  assert.match(sql, /statement_timeout = '120s'/);
  assert.match(sql, /LIMIT 200/);
  assert.match(sql, /token\|secret\|password\|credential\|email\|phone/);
  assert.match(sql, /ROLLBACK;/);
});
