import assert from 'node:assert/strict';
import { spawn, execFileSync } from 'node:child_process';
import { mkdtempSync, mkdirSync, openSync, closeSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';

// Real HTTP/PostgreSQL contract. Never accepts a production host or database.
const database = process.env.TDF_EXPERIMENT_TEST_DATABASE_URL;
const binary = process.env.TDF_EXPERIMENT_SERVER_BIN;
assert.ok(database && binary, 'isolated database and tested executable required');
const url = new URL(database);
assert.ok(['127.0.0.1', 'localhost'].includes(url.hostname) || (process.env.CI === 'true' && url.hostname === 'postgres'));
assert.match(url.pathname, /_test$/);
const sql = (query) => {
  try { return execFileSync('psql', [database, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-c', query], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim(); }
  catch { throw new Error('Isolated experiment SQL failed; connection details omitted'); }
};
const prefix = `experiment-${process.pid}-${Date.now()}`;
const runtime = mkdtempSync(join(tmpdir(), 'tdf-experiment-http-'));
mkdirSync(join(runtime, 'assets'));
const children = [];
const files = [];
const experiment = '/session/experiments/single-feature-onboarding-v1';
const pause = (ms) => new Promise(resolve => setTimeout(resolve, ms));
const request = async (base, token, path = '/assignment', method = 'GET', status = 200) => {
  const response = await fetch(base + experiment + path, { method, headers: token ? { Authorization: `Bearer ${token}` } : {}, signal: AbortSignal.timeout(15000) });
  assert.equal(response.status, status, `unexpected experiment status ${response.status}`);
  return status === 200 ? response.json() : null;
};
const start = async (port, enabled) => {
  const base = `http://127.0.0.1:${port}`;
  const occupied = await fetch(base + '/health', { signal: AbortSignal.timeout(1000) }).then(() => true, () => false);
  assert.equal(occupied, false, 'refusing occupied port');
  const fd = openSync(join(runtime, `${enabled}.log`), 'w', 0o600); files.push(fd);
  const child = spawn(binary, [], { env: {
    PATH: process.env.PATH, TMPDIR: runtime, APP_ENV: 'test', DATABASE_URL: database,
    APP_PORT: String(port), RESET_DB: 'false', RUN_MIGRATIONS: 'false', SEED_DB: 'false',
    DEFAULT_LOCALE: 'es', HQ_ASSETS_DIR: join(runtime, 'assets'), EVENT_DISCOVERY_ENABLED: 'false',
    ARTIST_ENRICHMENT_ENABLED: 'false', EVENT_LOGISTICS_RECHECK_ENABLED: 'false',
    SINGLE_FEATURE_ONBOARDING_EXPERIMENT_ENABLED: String(enabled),
  }, stdio: ['ignore', fd, fd] });
  children.push(child);
  for (let attempt = 0; attempt < 90; attempt++) {
    assert.equal(child.exitCode, null, `test backend exited; inspect ${runtime}`);
    const health = await fetch(base + '/health', { signal: AbortSignal.timeout(1000) }).then(r => r.json(), () => null);
    if (health?.status === 'ok' && health?.db === 'ok') return base;
    await pause(250);
  }
  throw new Error(`isolated backend did not become ready; inspect ${runtime}`);
};
const fixture = (suffix, hours, completed = false) => {
  const token = `${prefix}-${suffix}`;
  const id = Number(sql(`INSERT INTO party(display_name,is_org,created_at) VALUES ('${token}',false,now()) RETURNING id`));
  assert.ok(Number.isSafeInteger(id));
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES ('${token}',${id},'isolated experiment test',true)`);
  if (hours !== null) sql(`INSERT INTO user_onboarding_progress(party_id,signup_completed_at,intent,completed_at,updated_at) VALUES (${id},now()-interval '${hours} hours','events',${completed ? 'now()' : 'NULL'},now())`);
  return { id, token };
};
// Hold the exact progress row until the HTTP handler is observed waiting for its
// lock, then commit completion/expiry. No probabilistic scheduling assumption.
const lockedRequest = async (base, account, update, settleDelay = 0) => {
  const holder = spawn('psql', [database, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], { stdio: ['pipe', 'pipe', 'pipe'] });
  children.push(holder);
  let output = '';
  const ready = new Promise((resolve, reject) => {
    holder.stdout.on('data', chunk => { output += chunk; if (output.includes('LOCKED')) resolve(); });
    holder.once('exit', code => { if (!output.includes('LOCKED')) reject(new Error(`lock holder exited ${code}`)); });
  });
  holder.stdin.write(`BEGIN; ${update};\n\\echo LOCKED\n`);
  await ready;
  const result = request(base, account.token, '/exposure', 'POST');
  try {
    let waiting = false;
    for (let attempt = 0; attempt < 100; attempt++) {
      waiting = Number(sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND wait_event_type='Lock' AND query LIKE 'UPDATE user_onboarding_progress SET party_id = party_id%'") || '0') > 0;
      if (waiting) break;
      await pause(20);
    }
    assert.ok(waiting, 'experiment handler must wait on authoritative progress row');
    if (settleDelay) await pause(settleDelay);
  } finally {
    holder.stdin.end('COMMIT;\n');
    await once(holder, 'exit');
  }
  return result;
};

try {
  const disabled = await start(Number(process.env.TDF_EXPERIMENT_SERVER_PORT ?? 18673), false);
  const enabled = await start(Number(process.env.TDF_EXPERIMENT_SERVER_PORT ?? 18673) + 2, true);
  const a = fixture('a', 1), b = fixture('b', 1);
  const old = fixture('old', 25), returning = fixture('returning', null), done = fixture('done', 1, true);
  const assignmentCount = () => Number(sql(`SELECT count(*) FROM user_experiment_assignment WHERE party_id IN (${a.id},${b.id})`));
  const before = sql(`SELECT row_to_json(p) FROM user_onboarding_progress p WHERE party_id=${a.id}`);
  const paused = await request(disabled, a.token);
  const pausedExposure = await request(disabled, a.token, '/exposure', 'POST');
  assert.equal(paused.experimentEnabled, false); assert.equal(paused.assignedAt, null);
  assert.equal(pausedExposure.newlyExposed, false); assert.equal(assignmentCount(), 0);
  assert.equal(sql(`SELECT row_to_json(p) FROM user_onboarding_progress p WHERE party_id=${a.id}`), before);
  await request(enabled, null, '/assignment', 'GET', 401);
  await request(enabled, 'invalid-synthetic-token', '/exposure', 'POST', 401);
  for (const account of [old, returning, done]) {
    const value = await request(enabled, account.token);
    assert.equal(value.experimentEligible, false); assert.equal(value.assignedAt, null);
  }
  const assignments = await Promise.all(Array.from({ length: 16 }, () => request(enabled, a.token)));
  assert.equal(assignments.filter(x => x.newlyAssigned).length, 1);
  assert.equal(new Set(assignments.map(x => `${x.variant}/${x.assignedAt}/${x.eligibleUntil}`)).size, 1);
  const exposures = await Promise.all(Array.from({ length: 16 }, () => request(enabled, a.token, '/exposure', 'POST')));
  assert.equal(exposures.filter(x => x.newlyExposed).length, 1);
  assert.equal(new Set(exposures.map(x => x.assignment.exposedAt)).size, 1);
  const second = await request(enabled, b.token);
  assert.equal(second.newlyAssigned, true); assert.equal(second.exposedAt, null);
  // Distinct accounts may legitimately share a variant; persistence stays isolated.
  assert.equal(assignmentCount(), 2);
  const completed = await lockedRequest(enabled, b, `UPDATE user_onboarding_progress SET completed_at=now() WHERE party_id=${b.id}`);
  assert.equal(completed.newlyExposed, false); assert.equal(completed.assignment.experimentEligible, false);
  const expires = fixture('expires', 1);
  const expired = await lockedRequest(enabled, expires, `UPDATE user_onboarding_progress SET signup_completed_at=now()-interval '24 hours'+interval '500 milliseconds' WHERE party_id=${expires.id}`, 700);
  assert.equal(expired.newlyExposed, false); assert.equal(expired.assignment.experimentEligible, false);
  assert.equal(Number(sql(`SELECT count(*) FROM user_experiment_assignment WHERE party_id=${expires.id}`)), 0);
  sql(`UPDATE api_token SET active=false WHERE token='${a.token}'`);
  await request(enabled, a.token, '/assignment', 'GET', 401);
  const pausedAgain = await request(disabled, b.token);
  assert.equal(pausedAgain.experimentEnabled, false); assert.equal(pausedAgain.assignedAt, null);
  assert.equal(Number(sql(`SELECT count(*) FROM party_security_role WHERE party_id IN (${a.id},${b.id})`)), 0);
  console.log('Experiment HTTP/PostgreSQL: paused/no-write, signup eligibility, account isolation, 16 concurrent assignments/exposures, completion/expiry lock contention and revocation passed.');
  console.log(`Isolated evidence: ${runtime}`);
} finally {
  for (const child of children) if (child.exitCode === null) child.kill('SIGTERM');
  for (const child of children) if (child.exitCode === null) await once(child, 'exit');
  files.forEach(closeSync);
}
