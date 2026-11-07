// DEPLOY-MIGRATION-001 at the PostgreSQL boundary: the production migration
// batch rendered by buildMigrationBatchSql, run through psql exactly as the
// release and the entrypoint run it, against the fully migrated disposable
// fixture. Synthetic migrations live in their own schema so the batch's schema
// verification still passes.
import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { createHash, randomInt } from 'node:crypto';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';
import { buildMigrationBatchSql } from '../lib/production-release.mjs';
import { reconcileMigrationLedger } from '../lib/migration-contract.mjs';

const db = process.env.TDF_MIGRATION_BATCH_DATABASE_URL;
disposablePostgresUrl(db, { ci: process.env.CI === 'true' });
const args = [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'];
const run = sql => spawnSync('psql', args, { input: sql, encoding: 'utf8' });
const pass = (sql, label) => { const r = run(sql); assert.equal(r.status, 0, `${label}: ${r.stderr}`); return r.stdout.trim(); };
const reject = (sql, pattern, label) => { const r = run(sql); assert.notEqual(r.status, 0, label); assert.match(r.stderr, pattern, label); };
const dir = mkdtempSync(join(tmpdir(), 'migration-batch-'));
const sha256 = value => createHash('sha256').update(value).digest('hex');
const source = '1'.repeat(40);
const migration = (id, content) => ({ id, path: `tdf-hq/sql/${id}.sql`, content, checksum: sha256(content) });
const batch = (migrations, label) => {
  const file = join(dir, `${label}.sql`);
  writeFileSync(file, buildMigrationBatchSql(migrations, { sourceCommit: source }));
  const r = spawnSync('psql', [db, '-X', '-v', 'ON_ERROR_STOP=1', '-f', file], { encoding: 'utf8' });
  return { status: r.status, stdout: r.stdout, stderr: r.stderr };
};
const ledger = id => pass(`SELECT coalesce((SELECT checksum||','||source_commit FROM tdf_schema_migration WHERE migration_id='${id}'), 'none');`, `ledger ${id}`);
const table = name => pass(`SELECT to_regclass('synthetic_batch.${name}') IS NOT NULL;`, `table ${name}`);
const ledgerRows = () => pass(`SELECT string_agg(migration_id, ',' ORDER BY applied_at, migration_id) FROM tdf_schema_migration WHERE migration_id LIKE 'synthetic_batch_%';`, 'rows');
const contract = migrations => ({ migrations: migrations.map(({ id, checksum, compatibleAppliedChecksums = [] }) => ({ id, checksum, compatibleAppliedChecksums })) });
const observedLedger = () => JSON.parse(pass(`SELECT coalesce(json_agg(json_build_object('migration_id', migration_id, 'checksum', checksum, 'source_commit', source_commit)), '[]') FROM tdf_schema_migration WHERE migration_id LIKE 'synthetic_batch_%';`, 'observed'));

pass(`DROP SCHEMA IF EXISTS synthetic_batch CASCADE; DELETE FROM tdf_schema_migration WHERE migration_id LIKE 'synthetic_batch_%'; CREATE SCHEMA synthetic_batch;`, 'setup');
try {
  const a = migration('synthetic_batch_a', 'BEGIN;\nCREATE TABLE synthetic_batch.a (id integer PRIMARY KEY);\nCOMMIT;');
  const b = migration('synthetic_batch_b', 'CREATE TABLE synthetic_batch.b (id integer PRIMARY KEY);');

  // Fresh application in manifest order, ledger rows bound to the candidate.
  let r = batch([a, b], 'fresh');
  assert.equal(r.status, 0, r.stderr);
  assert.ok(r.stdout.indexOf('Applying migration synthetic_batch_a') < r.stdout.indexOf('Applying migration synthetic_batch_b'));
  assert.equal(ledger(a.id), `${a.checksum},${source}`); assert.equal(ledger(b.id), `${b.checksum},${source}`);
  assert.equal(table('a') + table('b'), 'tt');
  const appliedAt = pass(`SELECT applied_at FROM tdf_schema_migration WHERE migration_id='${a.id}';`, 'applied_at');

  // Re-running is a no-op.
  r = batch([a, b], 'rerun');
  assert.equal(r.status, 0, r.stderr);
  assert.equal((r.stdout.match(/Already applied synthetic_batch_/g) ?? []).length, 2);
  assert.equal(pass(`SELECT applied_at FROM tdf_schema_migration WHERE migration_id='${a.id}';`, 'applied_at'), appliedAt);

  // Changed applied history fails closed; an explicitly compatible checksum passes and keeps the recorded one.
  const changed = migration(a.id, a.content + '\n-- edited');
  r = batch([changed, b], 'changed');
  assert.notEqual(r.status, 0); assert.match(r.stderr, /Checksum mismatch for migration synthetic_batch_a/);
  assert.equal(ledger(a.id), `${a.checksum},${source}`);
  r = batch([{ ...changed, compatibleAppliedChecksums: [a.checksum] }, b], 'compatible');
  assert.equal(r.status, 0, r.stderr);
  assert.equal(ledger(a.id), `${a.checksum},${source}`);
  assert.throws(() => reconcileMigrationLedger(contract([changed, b]), observedLedger()), /differs from the reviewed candidate/);
  assert.equal(reconcileMigrationLedger(contract([{ ...changed, compatibleAppliedChecksums: [a.checksum] }, b]), observedLedger())
    .every(row => row.state === 'applied'), true);

  // A ledger row the candidate does not know (an older image after a forward
  // release): the batch tolerates it so the image rollback can start; release
  // preparation rejects it.
  pass(`INSERT INTO tdf_schema_migration (migration_id, checksum, source_commit) VALUES ('synthetic_batch_unknown', '${'e'.repeat(64)}', '${source}');`, 'unknown row');
  r = batch([a, b], 'unknown');
  assert.equal(r.status, 0, r.stderr);
  assert.throws(() => reconcileMigrationLedger(contract([a, b]), observedLedger()), /absent from the candidate manifest/);
  pass(`DELETE FROM tdf_schema_migration WHERE migration_id='synthetic_batch_unknown';`, 'remove unknown');

  // A hole: an entry that precedes an applied one in manifest order is applied
  // now, after it, and preparation reports it pending rather than omitting it.
  const c = migration('synthetic_batch_c', 'BEGIN;\nCREATE TABLE synthetic_batch.c (id integer PRIMARY KEY);\nCOMMIT;');
  assert.deepEqual(reconcileMigrationLedger(contract([a, c, b]), observedLedger()).map(row => [row.id, row.state]),
    [[a.id, 'applied'], [c.id, 'pending'], [b.id, 'applied']]);
  r = batch([a, c, b], 'hole');
  assert.equal(r.status, 0, r.stderr);
  assert.equal(ledgerRows(), `${a.id},${b.id},${c.id}`);

  // The ledger row commits with the migration's effects or not at all, for a
  // file with its own transaction and for one without.
  for (const [label, content] of [
    ['own transaction', 'BEGIN;\nCREATE TABLE synthetic_batch.d (id integer PRIMARY KEY);\nSELECT 1/0;\nCOMMIT;'],
    ['no transaction', 'CREATE TABLE synthetic_batch.d (id integer PRIMARY KEY);\nSELECT 1/0;'],
  ]) {
    const d = migration('synthetic_batch_d', content);
    r = batch([a, b, c, d], `failing-${label.replace(' ', '-')}`);
    assert.notEqual(r.status, 0, label); assert.match(r.stderr, /division by zero/, label);
    assert.equal(table('d'), 'f', `${label}: no effects survive a failed migration`);
    assert.equal(ledger(d.id), 'none', `${label}: no ledger row survives a failed migration`);
  }
  // Negative control: the previous layout (effects, then the ledger row outside
  // the migration's transaction) leaves the effects of a file without its own
  // transaction committed and unrecorded.
  {
    const d = migration('synthetic_batch_d', 'CREATE TABLE synthetic_batch.d (id integer PRIMARY KEY);\nSELECT 1/0;');
    const legacy = buildMigrationBatchSql([a, b, c, d], { sourceCommit: source })
      .replace(/(\\echo Applying migration synthetic_batch_d)\nBEGIN;\n(INSERT INTO public\.tdf_schema_migration[^;]*;)\n([\s\S]*?)\nCOMMIT;\n\\else/,
        (_m, echo, insert, body) => `${echo}\n${body}\n${insert}\n\\else`);
    assert.notEqual(legacy, buildMigrationBatchSql([a, b, c, d], { sourceCommit: source }));
    const file = join(dir, 'legacy.sql'); writeFileSync(file, legacy);
    const legacyRun = spawnSync('psql', [db, '-X', '-v', 'ON_ERROR_STOP=1', '-f', file], { encoding: 'utf8' });
    assert.notEqual(legacyRun.status, 0);
    assert.equal(table('d'), 't', 'negative control: partial effects committed');
    assert.equal(ledger(d.id), 'none', 'negative control: and unrecorded');
    pass(`DROP TABLE synthetic_batch.d;`, 'remove partial effects');
  }

  // The window itself: the session dies right after the file's own COMMIT. With
  // the atomic row the ledger row is already part of that commit; the previous
  // layout lost it.
  {
    const g = migration('synthetic_batch_g', 'BEGIN;\nCREATE TABLE synthetic_batch.g (id integer PRIMARY KEY);\nCOMMIT;\nSELECT pg_terminate_backend(pg_backend_pid());');
    const rendered = buildMigrationBatchSql([a, b, c, g], { sourceCommit: source });
    const legacy = rendered.replace(/(\\echo Applying migration synthetic_batch_g)\nBEGIN;\n(INSERT INTO public\.tdf_schema_migration[^;]*;)\n([\s\S]*?)\nCOMMIT;\n\\else/,
      (_m, echo, insert, body) => `${echo}\n${body}\n${insert}\n\\else`);
    assert.notEqual(legacy, rendered);
    for (const [label, text, expectedLedger] of [['legacy', legacy, 'none'], ['atomic', rendered, `${g.checksum},${source}`]]) {
      const file = join(dir, `window-${label}.sql`); writeFileSync(file, text);
      const died = spawnSync('psql', [db, '-X', '-v', 'ON_ERROR_STOP=1', '-f', file], { encoding: 'utf8' });
      assert.notEqual(died.status, 0, `${label}: the session was terminated`);
      assert.equal(table('g'), 't', `${label}: the file's COMMIT persisted its effects`);
      assert.equal(ledger(g.id), expectedLedger, `${label}: ledger row after the crash`);
      pass(`DROP TABLE synthetic_batch.g; DELETE FROM tdf_schema_migration WHERE migration_id='${g.id}';`, `${label}: reset`);
    }
  }

  // Effects present without a ledger row (the window the atomic row closes, or
  // a manual application): a re-run re-applies the file. Only an idempotent
  // migration recovers by itself; a non-idempotent one needs a forward repair.
  pass(`CREATE TABLE synthetic_batch.e (id integer PRIMARY KEY);`, 'unrecorded effects');
  const eStrict = migration('synthetic_batch_e', 'BEGIN;\nCREATE TABLE synthetic_batch.e (id integer PRIMARY KEY);\nCOMMIT;');
  r = batch([a, b, c, eStrict], 'unrecorded-strict');
  assert.notEqual(r.status, 0); assert.match(r.stderr, /already exists/); assert.equal(ledger(eStrict.id), 'none');
  const eIdempotent = migration('synthetic_batch_e', 'BEGIN;\nCREATE TABLE IF NOT EXISTS synthetic_batch.e (id integer PRIMARY KEY);\nCOMMIT;');
  r = batch([a, b, c, eIdempotent], 'unrecorded-idempotent');
  assert.equal(r.status, 0, r.stderr); assert.equal(ledger(eIdempotent.id), `${eIdempotent.checksum},${source}`);

  // One batch at a time: a held migration lock makes the batch quit with status 3 before touching anything.
  {
    const holder = spawn('psql', args, { stdio: ['pipe', 'pipe', 'pipe'] });
    const ready = new Promise((resolve, rejectPromise) => {
      let output = ''; holder.stdout.on('data', data => { output += data; if (output.includes('lock_ready')) resolve(); });
      holder.on('error', rejectPromise);
      holder.on('close', status => rejectPromise(new Error(`lock holder exited: ${status}`)));
      setTimeout(() => rejectPromise(new Error('lock holder did not report')), 20_000).unref();
    });
    holder.stderr.resume();
    holder.stdin.write(`SELECT pg_advisory_lock(hashtextextended('tdf-production-schema-migrations', 0)); SELECT 'lock_ready';\n`);
    await ready;
    const f = migration('synthetic_batch_f', 'CREATE TABLE synthetic_batch.f (id integer PRIMARY KEY);');
    r = batch([a, b, c, eIdempotent, f], 'locked');
    holder.stdin.end('SELECT pg_advisory_unlock_all();\n');
    assert.equal(r.status, 3, `locked batch status: ${r.stderr}`);
    assert.match(r.stdout, /Another production schema migration is active/);
    assert.equal(table('f') + ledger(f.id), 'fnone');
  }

  // The ledger only records well-formed provenance.
  reject(`INSERT INTO tdf_schema_migration (migration_id, checksum, source_commit) VALUES ('synthetic_batch_bad', 'short', '${source}');`, /check constraint/, 'checksum shape');
  reject(`INSERT INTO tdf_schema_migration (migration_id, checksum, source_commit) VALUES ('synthetic_batch_bad', '${'f'.repeat(64)}', 'main');`, /check constraint/, 'source shape');
  reject(`INSERT INTO tdf_schema_migration (migration_id, checksum, source_commit) VALUES ('${a.id}', '${a.checksum}', '${source}');`, /duplicate key/, 'one row per migration');
} finally {
  pass(`DROP SCHEMA IF EXISTS synthetic_batch CASCADE; DELETE FROM tdf_schema_migration WHERE migration_id LIKE 'synthetic_batch_%';`, 'cleanup');
  rmSync(dir, { recursive: true, force: true });
}
console.log('migration-batch-postgres: ok');
