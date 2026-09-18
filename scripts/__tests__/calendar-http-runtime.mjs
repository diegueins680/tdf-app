import assert from 'node:assert/strict';
import { spawn, execFileSync } from 'node:child_process';
import { mkdtempSync, mkdirSync, openSync, closeSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

const database = process.env.TDF_CALENDAR_TEST_DATABASE_URL;
const binary = process.env.TDF_CALENDAR_SERVER_BIN;
assert.ok(database && binary, 'isolated database and tested executable required');
const url = new URL(database);
assert.ok(['127.0.0.1', 'localhost'].includes(url.hostname) || (process.env.CI === 'true' && url.hostname === 'postgres'));
assert.match(url.pathname, /_test$/);
const sql = query => {
  try { return execFileSync('psql', [database, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-c', query], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim(); }
  catch (error) { writeFileSync('/tmp/tdf-ux-calendar-sql-error.txt', error.stderr ?? '', {mode:0o600}); throw new Error('Isolated calendar SQL failed; private diagnostics saved, connection details omitted'); }
};
const migration = readFileSync(new URL('../../tdf-hq/sql/2026-09-18_calendar_runtime_schema.sql', import.meta.url), 'utf8');
const runtime = mkdtempSync(join(tmpdir(), 'tdf-calendar-http-'));
mkdirSync(join(runtime, 'assets'));
const prefix = `calendar-${process.pid}-${Date.now()}`;
const port = Number(process.env.TDF_CALENDAR_SERVER_PORT ?? 18692);
const base = `http://127.0.0.1:${port}`;
const fixtures = [];
let child, fd, schemaReady = false;
const fixture = admin => {
  const token = `${prefix}-${admin ? 'admin' : 'reader'}`;
  const id = Number(sql(`INSERT INTO party(display_name,is_org,created_at) VALUES ('${token}',false,now()) RETURNING id`));
  assert.ok(Number.isSafeInteger(id)); fixtures.push({id, token});
  sql(`INSERT INTO api_token(token,party_id,label,active) VALUES ('${token}',${id},'isolated calendar test',true)`);
  if (admin) sql(`INSERT INTO party_security_role(party_id,role_id) SELECT ${id},id FROM security_role WHERE code='admin'`);
  return token;
};
const request = async (path, token) => {
  const response = await fetch(base + '/calendar/v1/' + path, {
    headers: token ? { Authorization: `Bearer ${token}` } : {}, signal: AbortSignal.timeout(15000),
  });
  return { status: response.status, body: response.status === 200 ? await response.json() : null };
};
try {
  assert.equal(await fetch(base + '/health', { signal: AbortSignal.timeout(1000) }).then(() => true, () => false), false, 'refusing occupied port');
  sql(migration); schemaReady = true;
  const admin = fixture(true), reader = fixture(false);
  fd = openSync(join(runtime, 'backend.log'), 'w', 0o600);
  child = spawn(binary, [], { env: {
    PATH: process.env.PATH, TMPDIR: runtime, APP_ENV: 'test', DATABASE_URL: database,
    APP_PORT: String(port), RESET_DB: 'false', RUN_MIGRATIONS: 'false', AUTO_APPLY_PRODUCTION_MIGRATIONS: 'false', SEED_DB: 'false',
    DEFAULT_LOCALE: 'es', HQ_ASSETS_DIR: join(runtime, 'assets'), EVENT_DISCOVERY_ENABLED: 'false',
    ARTIST_ENRICHMENT_ENABLED: 'false', EVENT_LOGISTICS_RECHECK_ENABLED: 'false',
  }, stdio: ['ignore', fd, fd] });
  let ready = false;
  for (let attempt = 0; attempt < 120; attempt++) {
    assert.equal(child.exitCode, null, `test backend exited; private diagnostics ${runtime}`);
    const health = await fetch(base + '/health', { signal: AbortSignal.timeout(1000) }).then(r => r.json(), () => null);
    if (health?.status === 'ok' && health?.db === 'ok') { ready = true; break; }
    await new Promise(resolve => setTimeout(resolve, 250));
  }
  assert.ok(ready, 'actual backend readiness required');
  for (const path of ['config', 'events']) {
    assert.equal((await request(path)).status, 401);
    assert.equal((await request(path, reader)).status, 403);
    const empty = await request(`${path}?calendarId=${prefix}`, admin);
    assert.equal(empty.status, 200);
    assert.deepEqual(empty.body, path === 'config' ? null : []);
  }
  sql(`INSERT INTO google_calendar_config(owner_id,calendar_id,access_token,refresh_token)
    VALUES (${fixtures[0].id},'${prefix}','synthetic-access','synthetic-refresh');`);
  for (let index = 0; index < 12; index++) {
    sql(`INSERT INTO google_calendar_event(calendar_id,google_id,status,summary,attendees,raw_payload)
      VALUES ('${prefix}','event-${index}','${index % 2 ? 'cancelled' : 'confirmed'}','Synthetic ${index}','[]','{"test":true}');`);
  }
  const before = sql(`SELECT row_to_json(c) FROM google_calendar_config c WHERE calendar_id='${prefix}'`);
  const eventsBefore = sql(`SELECT json_agg(e ORDER BY e.id) FROM google_calendar_event e WHERE calendar_id='${prefix}'`);
  sql(migration); sql(migration);
  assert.equal(sql(`SELECT row_to_json(c) FROM google_calendar_config c WHERE calendar_id='${prefix}'`), before);
  assert.equal(sql(`SELECT json_agg(e ORDER BY e.id) FROM google_calendar_event e WHERE calendar_id='${prefix}'`), eventsBefore);
  const config = await request(`config?calendarId=${prefix}`, admin);
  assert.equal(config.status, 200); assert.equal(config.body.calendarId, prefix);
  assert.deepEqual(Object.keys(config.body).sort(), ['calendarId', 'configId', 'syncCursor', 'syncedAt']);
  assert.ok(!JSON.stringify(config.body).includes('synthetic-access'));
  const events = await request(`events?calendarId=${prefix}&status=confirmed`, admin);
  assert.equal(events.status, 200); assert.equal(events.body.length, 6);
  assert.ok(events.body.every(event => event.status === 'confirmed'));
  assert.equal(sql(`SELECT count(*) FROM pg_trigger WHERE tgrelid='google_calendar_config'::regclass AND tgname='identity_archive_reference_guard' AND tgfoid='identity_reject_archived_reference()'::regprocedure AND tgenabled='O'`), '1');
  assert.equal(sql(`SELECT count(*) FROM pg_constraint WHERE conrelid='google_calendar_config'::regclass AND confrelid='party'::regclass AND contype='f'`), '1');
  console.log(JSON.stringify({ actualPostgres: true, publicDenied: true, nonAdminDenied: true,
    emptyState: 'passed', populatedConfigAndFilteredEvents: 'passed', credentialsNotExposed: true,
    migrationRepeatPreservesRows: true, existingArchivedOwnerGuard: 'attached', generatedEvents: 12 }));
} finally {
  child?.kill('SIGTERM'); if (fd !== undefined) closeSync(fd);
  if (schemaReady) sql(`DELETE FROM google_calendar_event WHERE calendar_id='${prefix}'; DELETE FROM google_calendar_config WHERE calendar_id='${prefix}';`);
  if (fixtures.length) {
    const ids = fixtures.map(f => f.id).join(',');
    sql(`UPDATE party_security_role SET active=false WHERE party_id IN (${ids}); UPDATE api_token SET active=false WHERE party_id IN (${ids});`);
  }
}
