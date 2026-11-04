import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { readFile } from 'node:fs/promises';
import { randomBytes, randomUUID } from 'node:crypto';
import { after, before, test } from 'node:test';

const execute = promisify(execFile);
const host = process.env.TDF_TEST_POSTGRES_HOST ?? '127.0.0.1';
if (!['127.0.0.1', 'localhost', 'postgres'].includes(host)) throw new Error('Only an isolated local/CI PostgreSQL test host is supported');
const database = `tdf_content_storage_${randomBytes(5).toString('hex')}_test`;
const env = {
  ...Object.fromEntries(Object.entries(process.env).filter(([key]) => !key.startsWith('PG'))),
  PGHOST: host, PGPORT: process.env.TDF_TEST_POSTGRES_PORT ?? '5432',
  PGUSER: process.env.TDF_TEST_POSTGRES_USER ?? 'postgres',
  PGPASSWORD: process.env.TDF_TEST_POSTGRES_PASSWORD ?? '', PGSSLMODE: 'disable',
};
const psql = process.env.PSQL_BIN ?? 'psql';
async function sql(query, db = database) {
  const result = await execute(psql, ['-X', '-v', 'ON_ERROR_STOP=1', '-At', '-d', db, '-c', query], { env, maxBuffer: 1024 * 1024 });
  return result.stdout.trim();
}
let created = false;
before(async () => {
  await sql(`CREATE DATABASE ${database}`, 'postgres');
  created = true;
  await sql('CREATE TABLE party(id bigint PRIMARY KEY); INSERT INTO party SELECT generate_series(1,20)');
  await sql(await readFile(new URL('../tdf-hq/sql/2026-11-04_content_storage.sql', import.meta.url), 'utf8'));
  await sql(await readFile(new URL('../tdf-hq/sql/2026-11-04_content_storage_drive.sql', import.meta.url), 'utf8'));
  await sql(await readFile(new URL('../tdf-hq/sql/2026-11-04_content_storage_deletion.sql', import.meta.url), 'utf8'));
  await sql(await readFile(new URL('../tdf-hq/sql/2026-11-04_content_storage_accounting.sql', import.meta.url), 'utf8'));
  await sql(await readFile(new URL('../tdf-hq/sql/2026-11-04_content_storage_delete_capacity.sql', import.meta.url), 'utf8'));
  // These synthetic fixture accounts have no pre-existing objects.
  await sql('INSERT INTO content_storage_account(party_id,legacy_usage_verified) SELECT id,true FROM party ON CONFLICT(party_id) DO UPDATE SET legacy_usage_verified=true');
});
after(async () => { if (created) await sql(`DROP DATABASE ${database}`, 'postgres'); });

const hash = (letter) => letter.repeat(64);
async function reserve(owner, bytes, { key = hash('a'), fingerprint = hash('b'), destination = 'tdf' } = {}) {
  return JSON.parse(await sql(`SELECT content_storage_reserve(${owner},'${key}','${fingerprint}',${bytes},'${destination}','private','synthetic.bin','application/octet-stream','${hash('c')}','${randomUUID()}','${randomUUID()}')`));
}

test('allows exactly 5 decimal GB and refuses one additional byte', async () => {
  const first = await reserve(1, 5_000_000_000);
  assert.equal(first.status, 'uploading');
  assert.equal((await reserve(1, 1, { key: hash('d') })).code, 'storage_quota_exceeded');
});
test('serializes simultaneous 3 GB reservations so only one can consume free capacity', async () => {
  const outcomes = await Promise.all([reserve(2, 3_000_000_000), reserve(2, 3_000_000_000, { key: hash('d') })]);
  assert.equal(outcomes.filter((value) => value.status === 'uploading').length, 1);
  assert.equal(outcomes.filter((value) => value.code === 'storage_quota_exceeded').length, 1);
  assert.equal(await sql("SELECT count(*) FROM content_storage_upload WHERE owner_party_id=2"), '1');
});
test('never double reserves a concurrent replay of the same request', async () => {
  const outcomes = await Promise.all([reserve(3, 40), reserve(3, 40)]);
  assert.equal(outcomes.filter((value) => value.status === 'uploading').length, 1);
  assert.equal(outcomes.filter((value) => value.code === 'storage_upload_in_progress').length, 1);
  assert.equal(await sql("SELECT sum(byte_size) FROM content_storage_upload WHERE owner_party_id=3"), '40');
});
test('replays completed files and refuses a changed request under the same key', async () => {
  const first = await reserve(4, 10);
  await sql(`UPDATE content_storage_upload SET status='completed',completed_at=now() WHERE id='${first.id}'`);
  const replay = await reserve(4, 10);
  assert.equal(replay.id, first.id);
  assert.equal(replay.status, 'completed');
  assert.equal((await reserve(4, 10, { fingerprint: hash('d') })).code, 'storage_upload_conflict');
});
test('does not charge external bytes and preserves a larger existing entitlement', async () => {
  await reserve(5, 6_000_000_000, { destination: 'google_drive' });
  assert.equal((await reserve(5, 5_000_000_000, { key: hash('d') })).status, 'uploading');
  await sql('UPDATE content_storage_account SET allowance_bytes=10000000000 WHERE party_id=6');
  assert.equal((await reserve(6, 6_000_000_000)).status, 'uploading');
});
test('rotates expired leases without allocating a new file or releasing uncertain bytes', async () => {
  const first = await reserve(7, 5_000_000_000);
  await sql(`UPDATE content_storage_upload SET lease_until=now()-interval '1 second' WHERE id='${first.id}'`);
  assert.equal((await reserve(7, 1, { key: hash('d') })).code, 'storage_quota_exceeded');
  const retry = await reserve(7, 5_000_000_000);
  assert.equal(retry.id, first.id);
  assert.notEqual(retry.lease, first.lease);
  assert.equal(await sql(`SELECT count(*) FROM content_storage_upload WHERE id='${first.id}' AND lease_id='${first.lease}' AND lease_until>now()`), '0');
});
test('keeps keys isolated by owner and refuses resurrection after explicit cancellation', async () => {
  const first = await reserve(8, 10);
  const other = await reserve(9, 10);
  assert.notEqual(first.id, other.id);
  await sql(`UPDATE content_storage_upload SET status='cancelled' WHERE id='${first.id}'`);
  assert.equal((await reserve(8, 10)).code, 'storage_upload_cancelled');
});
test('reapplies the ordered migration batch and refuses rollback while uploads exist', async () => {
  for (const name of ['content_storage', 'content_storage_drive', 'content_storage_deletion', 'content_storage_accounting', 'content_storage_delete_capacity']) {
    await sql(await readFile(new URL(`../tdf-hq/sql/2026-11-04_${name}.sql`, import.meta.url), 'utf8'));
  }
  await assert.rejects(sql(await readFile(new URL('../tdf-hq/sql/2026-11-04_content_storage_rollback.sql', import.meta.url), 'utf8')));
  assert.equal(await sql('SELECT count(*)>0 FROM content_storage_upload'), 't');
});

test('persists one Google-generated identity before concurrent provider creation', async () => {
  const insert = (providerId) => sql(`INSERT INTO content_storage_drive_attempt(owner_party_id,request_key,fingerprint,provider_account_id,provider_file_id) VALUES(10,'${hash('a')}','${hash('b')}','synthetic-google-owner','${providerId}') ON CONFLICT(owner_party_id,request_key) DO NOTHING; SELECT provider_file_id FROM content_storage_drive_attempt WHERE owner_party_id=10 AND request_key='${hash('a')}'`);
  const outputs = await Promise.all([insert('synthetic-google-a'), insert('synthetic-google-b')]);
  assert.equal(outputs[0].split('\n').at(-1), outputs[1].split('\n').at(-1));
  assert.equal(await sql('SELECT count(*) FROM content_storage_drive_attempt'), '1');
  assert.equal(await sql(`SELECT count(*) FROM content_storage_drive_attempt WHERE owner_party_id=10 AND request_key='${hash('a')}' AND provider_account_id='different-google-owner'`), '0');
});

test('does not pretend legacy usage is zero, while new accounts receive the measured free allowance', async () => {
  await sql('UPDATE content_storage_account SET legacy_usage_verified=false WHERE party_id=20');
  assert.equal((await reserve(20, 1)).code, 'storage_inventory_required');
  await sql(`SELECT content_storage_reconcile(20,4000000000,'${hash('d')}',0)`);
  assert.equal((await reserve(20, 2000000000)).code, 'storage_quota_exceeded');
  assert.equal((await reserve(20, 1000000000)).status, 'uploading');
  await sql('INSERT INTO party(id) VALUES(21)');
  assert.equal((await reserve(21, 5000000000)).status, 'uploading');
});
test('reconciliation keeps its audit identity and rejects conflicting replay or stale updates', async () => {
  const repeated = JSON.parse(await sql(`SELECT content_storage_reconcile(20,4000000000,'${hash('d')}',0)`));
  assert.equal(repeated.status, 'already_recorded');
  assert.equal(JSON.parse(await sql(`SELECT content_storage_reconcile(20,0,'${hash('d')}',1)`)).code, 'reconciliation_conflict');
  assert.equal(JSON.parse(await sql(`SELECT content_storage_reconcile(20,0,'${hash('e')}',0)`)).code, 'reconciliation_conflict');
});

test('explicit deletion preserves completion evidence, fences stale publication and retains capacity until physical deletion is confirmed', async () => {
  const first = await reserve(19, 2000000000);
  await sql(`UPDATE content_storage_upload SET status='completed',completed_at=now() WHERE id='${first.id}'`);
  await sql(`UPDATE content_storage_upload SET status='cancelled',original_completed_at=COALESCE(original_completed_at,completed_at),completed_at=NULL WHERE id='${first.id}'`);
  assert.equal(await sql(`SELECT original_completed_at IS NOT NULL AND status='cancelled' FROM content_storage_upload WHERE id='${first.id}'`), 't');
  assert.equal(await sql(`SELECT count(*) FROM content_storage_upload WHERE id='${first.id}' AND status='uploading' AND lease_id='${first.lease}'`), '0');
  assert.equal((await reserve(19, 2000000000)).code, 'storage_upload_cancelled');
  assert.equal((await reserve(19, 5000000000, { key: hash('d') })).code, 'storage_quota_exceeded');
  await sql(`UPDATE content_storage_upload SET deleted_at=now() WHERE id='${first.id}'`);
  assert.equal((await reserve(19, 5000000000, { key: hash('d') })).status, 'uploading');
});
