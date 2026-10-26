#!/usr/bin/env node
// Owned empty database only. Reuse the production batch builder and the shipped
// SQL source extractor; no provider adapter or production connection is used.
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { disposablePostgresUrl } from './lib/disposable-postgres-url.mjs';
import { buildMigrationBatchSql, expandMigrationIncludes } from './lib/production-release.mjs';

const root = fileURLToPath(new URL('../', import.meta.url));
const database = process.env.TDF_DIRECTORY_ALERT_PARITY_DATABASE_URL;
disposablePostgresUrl(database, { ci: process.env.CI === 'true' });
const environment = { ...process.env, PGOPTIONS: '-c statement_timeout=60000 -c lock_timeout=10000' };
const migrationId = '2026-10-25_directory_saved_search_parity';
const forward = `tdf-hq/sql/${migrationId}.sql`;
const rollback = `tdf-hq/sql/${migrationId}_rollback.sql`;
const sourceRevision = execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' }).trim();
const manifest = JSON.parse(readFileSync(new URL('production-migrations.json', import.meta.url), 'utf8')).migrations;
const position = manifest.findIndex((entry) => entry.id === migrationId);
assert.ok(position > 0, 'Register the reviewed additive migration before this qualification');

function sql(input) {
  return execFileSync('psql', ['-XqAt', '-v', 'ON_ERROR_STOP=1', '-d', database], {
    input, encoding: 'utf8', env: environment, cwd: root,
    maxBuffer: 64 * 1024 * 1024, stdio: ['pipe', 'pipe', 'pipe'],
  }).trim();
}
function apply(path) { return sql(readFileSync(new URL(path, new URL('../', import.meta.url)), 'utf8')); }
async function batch(entries) {
  const migrations = await Promise.all(entries.map(async (entry) => {
    const content = await expandMigrationIncludes({ path: entry.path, content: readFileSync(new URL(entry.path, new URL('../', import.meta.url)), 'utf8') },
      (path) => Promise.resolve(readFileSync(new URL(path, new URL('../', import.meta.url)), 'utf8')));
    return { ...entry, content, checksum: createHash('sha256').update(content).digest('hex') };
  }));
  return buildMigrationBatchSql(migrations, { sourceCommit: sourceRevision });
}
assert.equal(sql("SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p','v','m','f','S');"), '0', 'Use a new empty disposable database');
apply('scripts/__tests__/fixtures/production-schema-20260814.sql');
apply('scripts/__tests__/fixtures/catalog-production-source-fixture.sql');
sql(await batch(manifest.slice(0, position)));
const originalTrigger = sql("SELECT pg_get_functiondef('directory_enqueue_saved_search_alerts()'::regprocedure);");
sql(`INSERT INTO party(id,display_name,is_org,created_at) VALUES(900050,'Synthetic historical alert owner',false,now());
 INSERT INTO directory_saved_search(account_party_id,name,canonical_query,query_hash,alerts_enabled)
 VALUES(900050,'Historical JSON and hash sentinel','{"entityType":"all","q":"sentinel"}',repeat('9',64),false);`);
const savedRowsSql = 'SELECT jsonb_agg(to_jsonb(saved) ORDER BY id) FROM directory_saved_search saved;';
const originalRows = sql(savedRowsSql);
const complete = await batch(manifest);
sql(complete);
assert.equal(sql('SELECT count(*) FROM tdf_schema_migration;'), String(manifest.length));
const ledgerSql = 'SELECT jsonb_agg(to_jsonb(ledger) ORDER BY migration_id) FROM tdf_schema_migration ledger;';
const originalLedger = sql(ledgerSql);
sql(complete);
apply(forward);
assert.equal(sql(ledgerSql), originalLedger, 'Replay cannot rewrite the migration ledger');
assert.equal(sql(savedRowsSql), originalRows, 'Forward/replay cannot rewrite saved JSON, names or hashes');
apply('tdf-hq/test/integration/directory_saved_search_parity_postgres.sql');
execFileSync('python3', ['scripts/test-community-search.py'], {
  cwd: root, env: { ...environment, TDF_DISCOVERY_TEST_DATABASE_URL: database }, stdio: 'inherit',
});
execFileSync(process.execPath, ['scripts/__tests__/directory-search-plan-parity.mjs'], {
  cwd: root, env: environment, stdio: 'inherit',
});

apply(rollback);
apply(rollback);
assert.equal(sql("SELECT pg_get_functiondef('directory_enqueue_saved_search_alerts()'::regprocedure);"), originalTrigger, 'Rollback restores the exact previous trigger');
assert.equal(sql("SELECT count(*) FROM pg_proc WHERE proname IN ('directory_query_alias_entity_ids','directory_matches_non_geo_filters','directory_matches_entity_and_derivation','directory_parse_saved_search');"), '0');
assert.equal(sql(savedRowsSql), originalRows, 'Rollback does not delete/rewrite saved searches');
assert.equal(sql(ledgerSql), originalLedger, 'Rollback does not rewrite historical migration objects');
apply(forward);
assert.equal(sql(savedRowsSql), originalRows);
apply('tdf-hq/test/integration/directory_saved_search_parity_postgres.sql');
assert.equal(sql(ledgerSql), originalLedger);
console.log('PASS: full prefix, forward/replay, shared predicate regressions, exact rollback/reapply, saved JSON/hash preservation');

if (process.env.TDF_DIRECTORY_ALERT_PARITY_SERVER_BIN) {
  execFileSync(process.execPath, ['scripts/__tests__/directory-saved-search-http.mjs'], {
    cwd: root, env: environment, stdio: 'inherit',
  });
} else {
  assert.notEqual(process.env.CI, 'true', 'CI must execute the real HTTP lane with its compiled backend');
  console.log('HTTP lane not executed: supply TDF_DIRECTORY_ALERT_PARITY_SERVER_BIN; SQL is not an HTTP pass');
}
