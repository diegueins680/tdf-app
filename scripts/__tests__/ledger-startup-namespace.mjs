// Exercise the actual startup SQL on a fully migrated, owned synthetic database.
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { buildSchemaVerificationSql } from '../lib/production-release.mjs';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';

const database = process.env.TDF_LEDGER_POSTING_DATABASE_URL;
disposablePostgresUrl(database, { ci: process.env.CI === 'true' });
assert.ok(!process.env.PGHOSTADDR && !process.env.PGSERVICE && !process.env.PGSERVICEFILE,
  'Inherited libpq routing overrides are forbidden');
const run = sql => spawnSync('psql', ['-XqAt', '-v', 'ON_ERROR_STOP=1', '-d', database], {
  input: sql, encoding: 'utf8', timeout: 60000,
  env: { ...process.env, PGOPTIONS: '-c statement_timeout=30000 -c lock_timeout=10000' },
});
const pass = (sql, label) => {
  const result = run(sql);
  assert.equal(result.status, 0, `${label}: ${result.error ?? result.stderr}`);
  return result.stdout.trim();
};
const snapshot = () => pass(`SELECT jsonb_build_object(
  'guards', (SELECT jsonb_agg(pg_get_functiondef(oid) ORDER BY proname)
    FROM pg_proc WHERE oid IN (
      'public.commerce_validate_ledger_posting()'::regprocedure,
      'public.commerce_protect_posted_ledger()'::regprocedure)),
  'receipts', (SELECT jsonb_agg(to_jsonb(m) ORDER BY migration_id)
    FROM public.tdf_schema_migration m));`, 'snapshot installed guards and receipts');
const before = snapshot();
const verification = buildSchemaVerificationSql({ includePsqlHeader: false });
pass(verification, 'trusted full-schema baseline');
let rejected = 0;
for (const signature of [
  'public.commerce_validate_ledger_posting()',
  'public.commerce_protect_posted_ledger()',
]) {
  for (const mutation of [
    `ALTER FUNCTION ${signature} RESET search_path;`,
    `ALTER FUNCTION ${signature} SET search_path = pg_temp, pg_catalog, public;`,
    `SET LOCAL search_path = pg_temp, public; ALTER FUNCTION ${signature} SET search_path FROM CURRENT;`,
    `ALTER FUNCTION ${signature} SECURITY DEFINER;`,
    `ALTER FUNCTION ${signature} RENAME TO synthetic_ledger_startup_shadow;`,
  ]) {
    // A failed DO aborts the transaction; connection close rolls all DDL back.
    const result = run(`BEGIN;\n${mutation}\n${verification}\nROLLBACK;`);
    assert.notEqual(result.status, 0, `startup accepted ${mutation}`);
    assert.match(result.stderr, /Ledger trigger trusted lookup settings are missing or changed/,
      `startup failed for an unrelated reason: ${result.error ?? result.stderr}`);
    assert.equal(snapshot(), before, 'negative control changed guards or receipts');
    rejected += 1;
  }
  pass(`BEGIN; ALTER FUNCTION ${signature} SET statement_timeout = '5s';\n${verification}\nROLLBACK;`,
    'compatible extra function setting');
  assert.equal(snapshot(), before, 'positive control changed guards or receipts');
}
pass(verification, 'restored full-schema baseline');
assert.equal(snapshot(), before);
console.log(`ledger-startup-namespace: passed (${rejected} rejected mutations, 2 compatible settings; exact restoration)`);
