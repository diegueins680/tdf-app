// PRIV-WHATSAPP-001 at the PostgreSQL boundary. The statements mirror what
// claimWhatsAppConfirmationRequest, releaseWhatsAppConfirmationClaim,
// applyWhatsAppConsentConfirmation and the opt-out upsert in TDF.Server issue
// (captured from the server's statement log on 2026-10-12); the HTTP harness
// covers the handlers themselves. Each race runs two psql sessions whose
// commits wait behind one advisory gate, so the second writer always blocks on
// the first writer's uncommitted row.
import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { randomInt } from 'node:crypto';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';

const db = process.env.TDF_WHATSAPP_CONSENT_DATABASE_URL;
disposablePostgresUrl(db, { ci: process.env.CI === 'true' });
const args = [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'];
const run = sql => spawnSync('psql', args, { input: sql, encoding: 'utf8' });
const pass = (sql, label) => { const r = run(sql); assert.equal(r.status, 0, `${label}: ${r.stderr}`); return r.stdout.trim(); };
const reject = (sql, pattern, label) => { const r = run(sql); assert.notEqual(r.status, 0, label); assert.match(r.stderr, pattern, label); };

let serial = 0;
const freshPhone = () => `+5939900${String(randomInt(10000, 99999))}${String(++serial).padStart(2, '0')}`;
const columns = 'phone_e164,display_name,consent,source,note,consented_at,revoked_at,confirmation_requested_at,created_at,updated_at';
const values = (phone, name) => `('${phone}','${name}',false,'public','pending_confirmation',null,null,null,now(),now())`;
// Fixed shape: the row is created or left alone; the claim is the conditional update.
const createFixed = (phone, name) => `INSERT INTO whats_app_consent(${columns}) VALUES ${values(phone, name)} ON CONFLICT (phone_e164) DO NOTHING;`;
// Pre-fix shape (insertUnique): a SELECT then a plain INSERT.
const createUnique = (phone, name) => `INSERT INTO whats_app_consent(${columns}) SELECT ${values(phone, name).slice(1, -1)} WHERE NOT EXISTS (SELECT 1 FROM whats_app_consent WHERE phone_e164='${phone}');`;
const claim = (phone, at = 'now()') => `WITH c AS (UPDATE whats_app_consent SET confirmation_requested_at=${at},display_name='Claim',source='public',note='pending_confirmation',updated_at=${at}
  WHERE phone_e164='${phone}' AND consent=false AND (confirmation_requested_at IS NULL OR confirmation_requested_at < (${at})::timestamptz - interval '24 hours') RETURNING 1)
  SELECT 'claimed:'||count(*) FROM c;`;
const release = (phone, claimedAt) => `WITH r AS (UPDATE whats_app_consent SET confirmation_requested_at=null WHERE phone_e164='${phone}' AND consent=false AND confirmation_requested_at=${claimedAt} RETURNING 1) SELECT 'released:'||count(*) FROM r;`;
const confirm = (phone, sentAt, at = 'now()') => `WITH k AS (UPDATE whats_app_consent SET consent=true,consented_at=${at},revoked_at=null,confirmation_requested_at=null,note='confirmed_by_reply',updated_at=${at}
  WHERE phone_e164='${phone}' AND consent=false AND confirmation_requested_at >= (${at})::timestamptz - interval '7 days' AND confirmation_requested_at <= (${sentAt})::timestamptz RETURNING 1)
  SELECT 'confirmed:'||count(*) FROM k;`;
const optOut = phone => `INSERT INTO whats_app_consent(${columns}) VALUES ('${phone}',null,false,'opt-out',null,null,now(),null,now(),now())
  ON CONFLICT (phone_e164) DO UPDATE SET consent=false,source='opt-out',note=null,consented_at=null,revoked_at=now(),confirmation_requested_at=null,updated_at=now();`;
const state = phone => pass(`SELECT consent||','||coalesce(note,'')||','||(confirmation_requested_at IS NOT NULL)||','||(revoked_at IS NOT NULL) FROM whats_app_consent WHERE phone_e164='${phone}';`, 'state');

// Single-session semantics of the mirrored statements.
{
  const phone = freshPhone();
  pass(`BEGIN; ${createFixed(phone, 'A')} ${createFixed(phone, 'B')} COMMIT;`, 'repeated creation is a no-op');
  assert.equal(pass(`SELECT count(*)||','||display_name FROM whats_app_consent WHERE phone_e164='${phone}' GROUP BY display_name;`, 'one row'), '1,A');
  assert.equal(pass(claim(phone, `'2026-10-12T12:00:00Z'`), 'first claim'), 'claimed:1');
  assert.equal(pass(claim(phone, `'2026-10-13T11:59:59Z'`), 'claim inside the interval'), 'claimed:0');
  assert.equal(pass(release(phone, `'2026-10-12T11:00:00Z'`), 'release of a different claim'), 'released:0');
  assert.equal(pass(release(phone, `'2026-10-12T12:00:00Z'`), 'release of the own claim'), 'released:1');
  assert.equal(pass(claim(phone, `'2026-10-12T12:00:01Z'`), 'claim right after a release'), 'claimed:1');
  assert.equal(pass(claim(phone, `'2026-10-13T12:00:02Z'`), 'claim after the interval'), 'claimed:1');
  assert.equal(pass(confirm(phone, `'2026-10-13T12:00:01Z'`, `'2026-10-13T12:00:03Z'`), 'reply sent before the request'), 'confirmed:0');
  assert.equal(pass(confirm(phone, `'2026-10-13T12:00:02Z'`, `'2026-10-20T12:00:03Z'`), 'request older than the window'), 'confirmed:0');
  assert.equal(pass(confirm(phone, `'2026-10-13T12:00:02Z'`, `'2026-10-20T12:00:02Z'`), 'reply at the exact request time inside the window'), 'confirmed:1');
  assert.equal(state(phone), 'true,confirmed_by_reply,false,false');
  assert.equal(pass(confirm(phone, `'2026-10-13T12:00:02Z'`, `'2026-10-20T12:00:02Z'`), 'redelivered reply'), 'confirmed:0');
  assert.equal(pass(claim(phone, `'2026-11-01T00:00:00Z'`), 'no claim on active consent'), 'claimed:0');
  assert.equal(pass(release(phone, `'2026-10-13T12:00:02Z'`), 'release never touches active consent'), 'released:0');
  pass(optOut(phone), 'opt-out');
  assert.equal(state(phone), 'false,,false,true');
  assert.equal(pass(confirm(phone, `'2026-11-01T00:00:00Z'`), 'no request after opt-out'), 'confirmed:0');
  assert.equal(pass(claim(phone), 'claim after opt-out'), 'claimed:1');
  assert.equal(state(phone), 'false,pending_confirmation,true,true');
  assert.equal(pass(confirm(phone, `now()`), 'reply confirms the new request'), 'confirmed:1');
  assert.equal(state(phone), 'true,confirmed_by_reply,false,false');
  assert.equal(pass(`SELECT count(*) FROM whats_app_consent WHERE phone_e164='${phone}';`, 'still one row'), '1');
}

const session = sql => new Promise((resolve, rejectPromise) => {
  const child = spawn('psql', args, { stdio: ['pipe', 'pipe', 'pipe'] });
  let stdout = '', stderr = '';
  child.stdout.on('data', data => { stdout += data; });
  child.stderr.on('data', data => { stderr += data; });
  child.on('error', rejectPromise);
  child.on('close', status => resolve({ status, stdout: stdout.trim(), stderr }));
  child.stdin.end(sql);
});
// Runs two scripts in concurrent transactions that both reach the commit gate
// before either commits; `second` is released from the gate after `first`.
const race = async (first, second) => {
  const gate = randomInt(1, 2147483647), application = `whatsapp_consent_${gate}`;
  const holder = spawn('psql', args, { stdio: ['pipe', 'pipe', 'pipe'] });
  const ready = new Promise((resolve, rejectPromise) => {
    let output = '';
    holder.stdout.on('data', data => { output += data; if (output.includes('gate_ready')) resolve(); });
    holder.on('error', rejectPromise);
  });
  holder.stderr.resume();
  holder.stdin.write(`SELECT pg_advisory_lock(20261012,${gate}); SELECT 'gate_ready';\n`);
  await ready;
  const script = body => `SET application_name='${application}'; BEGIN; SET LOCAL statement_timeout='20s'; ${body} SELECT pg_advisory_xact_lock_shared(20261012,${gate}); COMMIT;`;
  const a = session(script(first));
  // The second writer blocks on the first writer's row until the first commits.
  let blocked = false;
  for (let poll = 0; poll < 200 && !blocked; poll++) {
    blocked = pass(`SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND application_name='${application}' AND wait_event_type='Lock' AND wait_event='advisory';`, 'first at gate') === '1';
    if (!blocked) await new Promise(resolve => setTimeout(resolve, 25));
  }
  assert.ok(blocked, 'first writer reaches the gate');
  const b = session(script(second));
  let waiting = false;
  for (let poll = 0; poll < 200 && !waiting; poll++) {
    waiting = pass(`SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND application_name='${application}' AND wait_event_type='Lock' AND wait_event IN ('transactionid','tuple');`, 'second blocked') === '1';
    if (!waiting) await new Promise(resolve => setTimeout(resolve, 25));
  }
  holder.stdin.end(`SELECT pg_advisory_unlock(20261012,${gate});\n`);
  const [ra, rb] = await Promise.all([a, b]);
  return { a: ra, b: rb, waiting };
};

// Concurrent first requests: with the fixed creation both are answered and
// exactly one claims; with the pre-fix shape the second insert fails.
{
  const phone = freshPhone();
  const { a, b, waiting } = await race(`${createFixed(phone, 'First')} ${claim(phone)}`, `${createFixed(phone, 'Second')} ${claim(phone)}`);
  assert.ok(waiting, 'second creation waits on the first uncommitted row');
  assert.equal(a.status, 0, a.stderr); assert.equal(b.status, 0, b.stderr);
  assert.deepEqual([a.stdout, b.stdout], ['claimed:1', 'claimed:0']);
  // The winner's claim rewrote the display name; the loser's creation was a no-op.
  assert.equal(pass(`SELECT count(*)||','||display_name FROM whats_app_consent WHERE phone_e164='${phone}' GROUP BY display_name;`, 'one row'), '1,Claim');
}
{
  const phone = freshPhone();
  const { a, b, waiting } = await race(`${createUnique(phone, 'First')} ${claim(phone)}`, `${createUnique(phone, 'Second')} ${claim(phone)}`);
  assert.ok(waiting, 'negative control: second insert waits on the first uncommitted row');
  assert.equal(a.status, 0, a.stderr);
  assert.notEqual(b.status, 0, 'negative control: check-then-insert loses the race');
  assert.match(b.stderr, /duplicate key value violates unique constraint "unique_whats_app_consent"/);
}

// A withdrawal and a confirmation of the same pending request, in both
// orders: consent ends inactive and revoked either way.
for (const confirmFirst of [false, true]) {
  const phone = freshPhone();
  pass(`${createFixed(phone, 'Pending')} UPDATE whats_app_consent SET confirmation_requested_at=now()-interval '1 hour' WHERE phone_e164='${phone}';`, 'pending request');
  const reply = confirm(phone, `now()`), withdraw = optOut(phone);
  const { a, b, waiting } = await race(confirmFirst ? reply : withdraw, confirmFirst ? withdraw : reply);
  assert.ok(waiting, `withdrawal race (confirm first: ${confirmFirst}) blocks the second writer`);
  assert.equal(a.status, 0, a.stderr); assert.equal(b.status, 0, b.stderr);
  assert.equal(confirmFirst ? a.stdout : b.stdout, confirmFirst ? 'confirmed:1' : 'confirmed:0');
  assert.equal(state(phone), 'false,,false,true', `withdrawal wins (confirm first: ${confirmFirst})`);
}

// A new claim after the interval and a confirmation of the old request: the
// reply either confirms the old request or the new claim supersedes it, never
// both, and consent is never active alongside a pending request.
for (const confirmFirst of [false, true]) {
  const phone = freshPhone();
  pass(`${createFixed(phone, 'Old')} UPDATE whats_app_consent SET confirmation_requested_at=now()-interval '25 hours' WHERE phone_e164='${phone}';`, 'old request');
  const reply = confirm(phone, `now()-interval '1 hour'`), reclaim = claim(phone);
  const { a, b, waiting } = await race(confirmFirst ? reply : reclaim, confirmFirst ? reclaim : reply);
  assert.ok(waiting, `reclaim race (confirm first: ${confirmFirst}) blocks the second writer`);
  assert.equal(a.status, 0, a.stderr); assert.equal(b.status, 0, b.stderr);
  if (confirmFirst) { assert.deepEqual([a.stdout, b.stdout], ['confirmed:1', 'claimed:0']); assert.equal(state(phone), 'true,confirmed_by_reply,false,false'); }
  else { assert.deepEqual([a.stdout, b.stdout], ['claimed:1', 'confirmed:0']); assert.equal(state(phone), 'false,pending_confirmation,true,false'); }
}

pass(`DELETE FROM whats_app_consent WHERE phone_e164 LIKE '+5939900%';`, 'cleanup');
console.log('whatsapp-consent-postgres: ok');
