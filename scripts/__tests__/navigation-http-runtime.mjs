import assert from 'node:assert/strict';
import { spawn, execFileSync } from 'node:child_process';
import { mkdtempSync, mkdirSync, openSync, closeSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { once } from 'node:events';

const database = process.env.TDF_NAVIGATION_TEST_DATABASE_URL;
const binary = process.env.TDF_NAVIGATION_SERVER_BIN;
assert.ok(database && binary, 'isolated database and tested executable required');
const url = new URL(database);
assert.ok(['127.0.0.1', 'localhost'].includes(url.hostname) || (process.env.CI === 'true' && url.hostname === 'postgres'));
assert.match(url.pathname, /_test$/);
const sql = query => {
  try { return execFileSync('psql', [database, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-c', query], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim(); }
  catch { throw new Error('Isolated navigation SQL failed; connection details omitted'); }
};
const runtime = mkdtempSync(join(tmpdir(), 'tdf-navigation-http-'));
mkdirSync(join(runtime, 'assets'));
const prefix = `navigation-${process.pid}-${Date.now()}`;
const pause = ms => new Promise(resolve => setTimeout(resolve, ms));
const port = Number(process.env.TDF_NAVIGATION_SERVER_PORT ?? 18683);
const base = `http://127.0.0.1:${port}`;
const path = '/navigation/preferences/studio.pipelines';
const children = [];
let fd;
const request = async (token, method = 'POST', suffix = '/visit', body) => {
  const response = await fetch(base + path + suffix, { method,
    headers: { Authorization: `Bearer ${token}`, 'Content-Type': 'application/json' },
    body: body ? JSON.stringify(body) : undefined, signal: AbortSignal.timeout(20000),
  });
  return { status: response.status, value: response.status === 200 ? await response.json() : null };
};
const fixture = (suffix, admin = true) => {
  const token = `${prefix}-${suffix}`;
  const id = Number(sql(`INSERT INTO party(display_name,is_org,created_at) VALUES ('${token}',false,now()) RETURNING id`));
  assert.ok(Number.isSafeInteger(id));
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES ('${token}',${id},'isolated navigation test',true)`);
  if (admin) sql(`INSERT INTO party_security_role(party_id,role_id) SELECT ${id},id FROM security_role WHERE code='admin'`);
  return { id, token };
};
try {
  assert.equal(await fetch(base + '/health', { signal: AbortSignal.timeout(1000) }).then(() => true, () => false), false, 'refusing occupied port');
  fd = openSync(join(runtime, 'backend.log'), 'w', 0o600);
  const child = spawn(binary, [], { env: {
    PATH: process.env.PATH, TMPDIR: runtime, APP_ENV: 'test', DATABASE_URL: database,
    APP_PORT: String(port), RESET_DB: 'false', RUN_MIGRATIONS: 'false', SEED_DB: 'false',
    DEFAULT_LOCALE: 'es', HQ_ASSETS_DIR: join(runtime, 'assets'), EVENT_DISCOVERY_ENABLED: 'false',
    ARTIST_ENRICHMENT_ENABLED: 'false', EVENT_LOGISTICS_RECHECK_ENABLED: 'false',
  }, stdio: ['ignore', fd, fd] });
  children.push(child);
  let ready = false;
  for (let attempt = 0; attempt < 90; attempt++) {
    assert.equal(child.exitCode, null, `test backend exited; inspect ${runtime}`);
    const health = await fetch(base + '/health', { signal: AbortSignal.timeout(1000) }).then(r => r.json(), () => null);
    if (health?.status === 'ok' && health?.db === 'ok') { ready = true; break; }
    await pause(250);
  }
  assert.ok(ready, `backend not ready; inspect ${runtime}`);
  const a = fixture('a'), b = fixture('b'), denied = fixture('denied', false);
  assert.equal((await request('invalid-local-token')).status, 401);
  assert.equal((await request(denied.token)).status, 404);
  // EXCLUSIVE allows both legacy SELECTs to see no row, while blocking their
  // INSERTs. Releasing after two observed waiters deterministically exercises
  // concurrent first visits; the old read-then-insert implementation returns500.
  const holder = spawn('psql', [database, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], { stdio: ['pipe', 'pipe', 'pipe'] });
  children.push(holder);
  let output = '';
  const locked = new Promise((resolve, reject) => {
    holder.stdout.on('data', chunk => { output += chunk; if (output.includes('LOCKED')) resolve(); });
    holder.once('exit', () => { if (!output.includes('LOCKED')) reject(new Error('test lock holder exited')); });
  });
  holder.stdin.write('BEGIN; LOCK TABLE feature_navigation_preferences IN EXCLUSIVE MODE;\n\\echo LOCKED\n');
  await locked;
  const pending = Promise.all(Array.from({ length: 16 }, () => request(a.token)));
  try {
    let waiting = 0;
    for (let attempt = 0; attempt < 100; attempt++) {
      waiting = Number(sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND wait_event_type='Lock' AND query ILIKE '%feature_navigation_preferences%'"));
      if (waiting >= 2) break;
      await pause(20);
    }
    assert.ok(waiting >= 2, 'at least two first visits must wait for the controlled write lock');
  } finally { holder.stdin.end('COMMIT;\n'); await once(holder, 'exit'); }
  const responses = await pending;
  assert.deepEqual(responses.map(r => r.status), Array(16).fill(200), 'all concurrent first visits succeed');
  assert.equal(sql(`SELECT count(*)||':'||sum(use_count) FROM feature_navigation_preferences WHERE party_id=${a.id}`), '1:16');
  assert.equal((await request(b.token)).value.useCount, 1, 'separate account starts its own count');
  const settings = { favorite: true, pinned: true, pinOrder: 3 };
  assert.equal((await request(a.token, 'PUT', '', settings)).status, 200);
  assert.equal((await request(a.token, 'PUT', '', { npuFavorite: true, npuPinned: true, npuPinOrder: 3 })).status, 200, 'legacy wire contract remains accepted');
  assert.equal((await request(a.token, 'PUT', '', { ...settings, npuFavorite: false })).status, 400, 'mixed schemas are rejected');
  assert.equal((await request(a.token, 'PUT', '', { ...settings, partyId: b.id })).status, 400, 'caller cannot choose another account');
  const mixed = await Promise.all(Array.from({ length: 16 }, (_, i) => i % 2 ? request(a.token) : request(a.token, 'PUT', '', settings)));
  assert.ok(mixed.every(r => r.status === 200));
  const stored = JSON.parse(sql(`SELECT row_to_json(p) FROM feature_navigation_preferences p WHERE party_id=${a.id}`));
  assert.equal(stored.use_count, 24); assert.equal(stored.favorite, true);
  assert.equal(stored.pinned, true); assert.equal(stored.pin_order, 3);
  sql(`UPDATE api_token SET active=false WHERE token='${a.token}'`);
  assert.equal((await request(a.token)).status, 401);
  assert.equal(sql(`SELECT use_count FROM feature_navigation_preferences WHERE party_id=${a.id}`), '24');
  assert.equal(sql(`SELECT count(*) FROM feature_navigation_preferences WHERE party_id=${denied.id}`), '0');
  console.log('PASS: controlled concurrent first visits; exact counts; settings preserved; account isolation; denial and revocation.');
} finally {
  for (const child of children.reverse()) if (child.exitCode === null) {
    child.kill('SIGTERM'); await Promise.race([once(child, 'exit'), pause(3000)]);
    if (child.exitCode === null) child.kill('SIGKILL');
  }
  if (fd !== undefined) closeSync(fd);
}
