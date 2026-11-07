#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { disposablePostgresUrl } from './lib/disposable-postgres-url.mjs';

const root = fileURLToPath(new URL('../', import.meta.url));
const database = process.env.TDF_MUSIC_REHEARSAL_DATABASE_URL;
if (!database) throw new Error('Set TDF_MUSIC_REHEARSAL_DATABASE_URL to a new empty test database');
disposablePostgresUrl(database, { ci: process.env.CI === 'true' });
if (process.env.PGHOSTADDR || process.env.PGSERVICE || process.env.PGSERVICEFILE) {
  throw new Error('Inherited libpq routing overrides are forbidden');
}

function sql(input) {
  return execFileSync('psql', ['-XqAt', '-v', 'ON_ERROR_STOP=1', '-d', database], {
    input, encoding: 'utf8', maxBuffer: 64 * 1024 * 1024,
    env: { ...process.env, PGOPTIONS: '-c statement_timeout=60000 -c lock_timeout=10000' },
    stdio: ['pipe', 'pipe', 'pipe'],
  }).trim();
}
function apply(relative) {
  sql(readFileSync(new URL(relative, new URL('../', import.meta.url)), 'utf8'));
}
function requireEqual(actual, expected, message) {
  if (actual !== expected) throw new Error(message);
}

// A pre-existing schema must never be used as a rehearsal target. The CI step
// creates this database in its own isolated PostgreSQL service, without reuse.
requireEqual(sql("SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p','v','m','f','S');"),
  '0', 'Music integration rehearsal requires an empty test database');
apply('scripts/__tests__/fixtures/production-schema-20260814.sql');
apply('scripts/__tests__/fixtures/catalog-production-source-fixture.sql');
const revision = execFileSync('git', ['rev-parse', 'HEAD'], { cwd: root, encoding: 'utf8' }).trim();
const batch = execFileSync(process.execPath, ['scripts/render-production-migration-batch.mjs'], {
  cwd: root, env: { ...process.env, SOURCE_COMMIT: revision },
  encoding: 'utf8', maxBuffer: 64 * 1024 * 1024,
});
const expectedCount = String(JSON.parse(readFileSync(new URL('production-migrations.json', import.meta.url),
  'utf8')).migrations.length);
sql(batch);
requireEqual(sql('SELECT count(*) FROM tdf_schema_migration;'), expectedCount,
  'The complete manifest must apply');
const ledgerSql = 'SELECT jsonb_agg(to_jsonb(m) ORDER BY migration_id) FROM tdf_schema_migration m;';
const ledger = sql(ledgerSql);
const snapshotSql = `SELECT jsonb_build_object(
  'functions', (SELECT jsonb_agg(pg_get_functiondef(p.oid) ORDER BY p.proname,
      pg_get_function_identity_arguments(p.oid))
    FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
    WHERE n.nspname='public' AND p.proname IN
      ('music_publish_due','music_withdraw_due','music_retire_live_versions',
       'music_release_live_version','music_rebind_upload_asset')),
  'triggers', (SELECT jsonb_agg(pg_get_triggerdef(t.oid) ORDER BY t.tgname)
    FROM pg_trigger t JOIN pg_class c ON c.oid=t.tgrelid
    JOIN pg_namespace n ON n.oid=c.relnamespace
    WHERE n.nspname='public' AND t.tgname IN
      ('trg_music_release_live_version','trg_music_rebind_upload_asset')));`;
const expected = sql(snapshotSql);
sql(batch);
requireEqual(sql(ledgerSql), ledger, 'Manifest replay must leave the applied ledger unchanged');
requireEqual(sql(snapshotSql), expected, 'Manifest replay must leave music functions and triggers unchanged');

for (let cycle = 1; cycle <= 2; cycle += 1) {
  apply('tdf-hq/sql/2026-10-12_music_upload_rebind_rollback.sql');
  apply('tdf-hq/sql/2026-10-11_music_publication_isolation_rollback.sql');
  if (sql(snapshotSql) === expected) throw new Error('Rollback control did not remove the music guarantees');
  requireEqual(sql("SELECT count(*) FROM pg_trigger WHERE tgname IN ('trg_music_release_live_version','trg_music_rebind_upload_asset');"),
    '0', 'Both music triggers must be absent after rollback');
  apply('tdf-hq/sql/2026-10-11_music_publication_isolation.sql');
  apply('tdf-hq/sql/2026-10-12_music_upload_rebind.sql');
  requireEqual(sql(snapshotSql), expected, 'Reapply must restore exact function and trigger definitions');
  requireEqual(sql(ledgerSql), ledger, 'Rollback/reapply must not rewrite migration history');
  console.log(`Music rollback/reapply cycle ${cycle} passed`);
}
console.log(`Complete ${expectedCount}-entry manifest apply/replay and music rollback/reapply passed`);
