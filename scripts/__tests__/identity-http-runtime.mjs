import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { mkdtempSync, openSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';
const db = process.env.TDF_IDENTITY_HTTP_DATABASE_URL;
const binary = process.env.TDF_IDENTITY_HTTP_SERVER_BIN;
assert.ok(binary && db, 'isolated database and tested backend binary required');
const url = new URL(db);
assert.ok(['127.0.0.1', 'localhost'].includes(url.hostname) || (process.env.CI === 'true' && url.hostname === 'postgres'));
assert.match(url.pathname, /_test$/);
const port = Number(process.env.TDF_IDENTITY_HTTP_PORT ?? 18631);
assert.ok(Number.isInteger(port) && port > 1024 && port < 65536);
const base = `http://127.0.0.1:${port}`;
const sql = query => {
  const result = spawnSync('psql', [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-c', query], { encoding: 'utf8' });
  assert.equal(result.status, 0, result.stderr);
  return result.stdout.trim();
};
assert.equal(sql("SELECT count(*) FROM party WHERE display_name LIKE 'Identity HTTP %'"), '0', 'requires unused synthetic fixtures');
let occupied = false;
try { await fetch(`${base}/health`); occupied = true; } catch { /* unoccupied */ }
assert.equal(occupied, false, 'refusing occupied test port');
const runtime = mkdtempSync(join(tmpdir(), 'tdf-identity-http-'));
const logPath = join(runtime, 'backend.log');
const log = openSync(logPath, 'w', 0o600);
const server = spawn(binary, [], { env: {
  PATH: process.env.PATH, TMPDIR: runtime, APP_ENV: 'test', DATABASE_URL: db, APP_PORT: String(port),
  RESET_DB: 'false', RUN_MIGRATIONS: 'false', SEED_DB: 'false', DEFAULT_LOCALE: 'es',
  HQ_ASSETS_DIR: runtime, EVENT_DISCOVERY_ENABLED: 'false', ARTIST_ENRICHMENT_ENABLED: 'false', EVENT_LOGISTICS_RECHECK_ENABLED: 'false',
}, stdio: ['ignore', log, log] });
try {
  let healthy = false;
  for (let i = 0; i < 90; i++) {
    try { healthy = (await (await fetch(`${base}/health`)).json()).db === 'ok'; } catch { /* starting */ }
    if (healthy) break;
    assert.equal(server.exitCode, null, readFileSync(logPath, 'utf8'));
    await new Promise(resolve => setTimeout(resolve, 500));
  }
  assert.ok(healthy, readFileSync(logPath, 'utf8'));
  sql(`INSERT INTO party(display_name,is_org,created_at) VALUES ('Identity HTTP operator A',false,now()),('Identity HTTP operator B',false,now()),('Identity HTTP denied',false,now());
    INSERT INTO party_security_role(party_id,role_id,approval_mode,active) SELECT p.id,r.id,'bootstrap',true FROM party p CROSS JOIN security_role r WHERE p.display_name IN ('Identity HTTP operator A','Identity HTTP operator B') AND r.code='admin' AND r.active;
    INSERT INTO api_token(token,party_id,label,active) SELECT 'synthetic-identity-'||CASE display_name WHEN 'Identity HTTP operator A' THEN 'a' WHEN 'Identity HTTP operator B' THEN 'b' ELSE 'denied' END,id,'Synthetic identity HTTP fixture',true FROM party WHERE display_name LIKE 'Identity HTTP %';`);
  const actor = Number(sql("SELECT id FROM party WHERE display_name='Identity HTTP operator A'"));
  const headers = who => who ? { Authorization: `Bearer synthetic-identity-${who}` } : {};
  const body = { cDisplayName: 'Identity HTTP contact', cIsOrg: false, cPrimaryEmail: 'shared@example.test' };
  const key = 'identity-http-request-0001';
  const post = (who, requestKey = key, payload = body) => fetch(`${base}/parties`, { method: 'POST', headers: { ...headers(who), 'Content-Type': 'application/json', 'Idempotency-Key': requestKey }, body: JSON.stringify(payload) });
  assert.equal((await post(null)).status, 401);
  assert.equal((await post('denied')).status, 403);
  const created = await Promise.all(Array.from({ length: 8 }, async () => {
    const response = await post('a'); assert.equal(response.status, 200); return response.json();
  }));
  const survivor = created[0].partyId;
  assert.equal(new Set(created.map(p => p.partyId)).size, 1, 'concurrent HTTP retries must create one contact');
  assert.equal((await post('a', key, { ...body, cDisplayName: 'changed payload' })).status, 409);
  const otherActor = await post('b'); assert.equal(otherActor.status, 200);
  assert.notEqual((await otherActor.json()).partyId, survivor, 'request keys must be actor-scoped');
  const distinct = await post('a', 'identity-http-request-0002'); assert.equal(distinct.status, 200);
  const retired = (await distinct.json()).partyId;
  assert.notEqual(retired, survivor, 'shared contact details do not establish identity');
  const operation = randomUUID(), caseId = randomUUID();
  sql(`INSERT INTO identity_reconciliation_case(id,member_ids,status,evidence,before_parties,reason,reviewed_by,reviewed_at)
    SELECT '${caseId}',ARRAY[${survivor},${retired}],'confirmed',jsonb_build_object('basis','verified-source-subject','issuer','synthetic-http','scope','synthetic-only','subject','synthetic-contact','evidence_reference','synthetic-http-fixture-only','external_reference_review','no-unresolved-references','member_ids',ARRAY[${survivor},${retired}]),jsonb_agg(to_jsonb(p) ORDER BY p.id),'Synthetic whole-group identity fixture',${actor},now() FROM party p WHERE id IN (${survivor},${retired});`);
  const plan = JSON.parse(sql(`SELECT identity_merge_plan('${caseId}');`));
  assert.equal(plan.can_execute, true, JSON.stringify(plan.blockers));
  assert.equal(plan.survivor, survivor);
  const execute = () => JSON.parse(sql(`SELECT identity_execute_merge('${operation}','${caseId}','${plan.fingerprint}');`));
  assert.equal(execute().status, 'applied'); assert.equal(execute().status, 'already-applied');
  assert.equal((await post('a', 'identity-http-request-0002')).status, 409, 'retired request cannot silently reuse canonical identity');
  const edit = (id, who, notes) => fetch(`${base}/parties/${id}`, { method: 'PUT', headers: { ...headers(who), 'Content-Type': 'application/json' }, body: JSON.stringify({ uNotes: notes }) });
  assert.equal((await edit(retired, 'a', 'must not write')).status, 409, 'archived edits need an understandable conflict');
  assert.equal((await edit(retired, 'denied', 'must not write')).status, 403);

  const listed = await (await fetch(`${base}/parties?limit=200`, { headers: headers('a') })).json();
  assert.ok(!listed.some(p => p.partyId === retired));
  assert.equal((await (await fetch(`${base}/parties/${retired}`, { headers: headers('a') })).json()).partyId, survivor);
  assert.equal((await fetch(`${base}/parties/${retired}`, { headers: headers('denied') })).status, 403);
  assert.equal((await fetch(`${base}/parties/${retired}`)).status, 401);
  assert.equal((await edit(survivor, 'a', 'unrelated later edit')).status, 200);
  assert.equal(JSON.parse(sql(`SELECT identity_rollback_merge('${operation}');`)).status, 'reverted');
  assert.equal(JSON.parse(sql(`SELECT identity_rollback_merge('${operation}');`)).status, 'already-reverted');
  assert.equal(sql(`SELECT notes FROM party WHERE id=${survivor}`), 'unrelated later edit');
  assert.equal((await (await fetch(`${base}/parties/${retired}`, { headers: headers('a') })).json()).partyId, retired);
  assert.equal(sql(`SELECT count(*) FROM identity_party_archive WHERE party_id=${retired}`), '0');
  console.log('Identity HTTP: authorization, concurrent replay, actor scope, shared details, archival, canonical access and rollback passed.');
} finally {
  server.kill('SIGTERM');
  await new Promise(resolve => { if (server.exitCode !== null) resolve(); else server.once('exit', resolve); });
}
