import assert from 'node:assert/strict';
import { createHash, randomUUID } from 'node:crypto';
import { execFileSync, spawn } from 'node:child_process';
import { mkdtempSync, openSync, closeSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { createServer } from 'node:net';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';

const database = process.env.TDF_DIRECTORY_ALERT_PARITY_DATABASE_URL;
disposablePostgresUrl(database, { ci: process.env.CI === 'true' });
const binary = process.env.TDF_DIRECTORY_ALERT_PARITY_SERVER_BIN;
assert.ok(binary, 'Supply the newly compiled backend');
const sql = (input) => execFileSync('psql', ['-XqAt', '-v', 'ON_ERROR_STOP=1', '-d', database], { input, encoding: 'utf8', maxBuffer: 16 * 1024 * 1024 }).trim();
const socket = createServer();
await new Promise((resolve, reject) => socket.once('error', reject).listen(0, '127.0.0.1', resolve));
const port = socket.address().port;
await new Promise((resolve) => socket.close(resolve));
const api = `http://127.0.0.1:${port}`;
const work = mkdtempSync(join(tmpdir(), 'tdf-directory-alert-http-'));
const log = openSync(join(work, 'api.log'), 'w', 0o600);
// Assert compiled provenance, rather than permitting deployment metadata to
// relabel an unrelated cached binary as this candidate.
const nativeEnvironment = { ...process.env };
for (const name of ['GIT_SHA', 'GIT_COMMIT', 'GIT_COMMIT_SHA', 'COMMIT_SHA', 'SOURCE_COMMIT', 'SOURCE_VERSION', 'SOURCE_SHA', 'GITHUB_SHA', 'RENDER_GIT_COMMIT', 'RENDER_GIT_COMMIT_SHA', 'VERCEL_GIT_COMMIT_SHA', 'FLY_GIT_SHA']) delete nativeEnvironment[name];
const sourceCommit = execFileSync('git', ['rev-parse', 'HEAD'], { encoding: 'utf8' }).trim();
const server = spawn(binary, [], { env: {
  ...nativeEnvironment, DATABASE_URL: database, APP_ENV: 'test', APP_PORT: String(port),
  RUN_MIGRATIONS: 'false', AUTO_APPLY_PRODUCTION_MIGRATIONS: 'false',
  RESET_DB: 'false', SEED_DB: 'false', DEFAULT_LOCALE: 'es',
  EVENT_DISCOVERY_ENABLED: 'false', ARTIST_ENRICHMENT_ENABLED: 'false',
  EVENT_LOGISTICS_RECHECK_ENABLED: 'false', OPERATIONS_WORKER_ENABLED: 'false',
}, stdio: ['ignore', log, log], detached: true });
const tokens = { owner: 'synthetic-directory-alert-owner', outsider: 'synthetic-directory-alert-outsider' };
let requests = 0;
async function request(path, { body, rawBody, method = body || rawBody ? 'POST' : 'GET', actor = 'owner', key } = {}) {
  assert.equal(server.exitCode, null, 'Owned backend must remain live');
  requests += 1;
  const response = await fetch(api + path, { method, headers: {
    ...(actor ? { Authorization: `Bearer ${tokens[actor]}` } : {}),
    ...(body || rawBody ? { 'Content-Type': 'application/json' } : {}),
    ...(key ? { 'Idempotency-Key': key } : {}),
  }, ...(rawBody ? { body: rawBody } : body ? { body: JSON.stringify(body) } : {}), signal: AbortSignal.timeout(10000) });
  const text = await response.text();
  let value; try { value = JSON.parse(text); } catch { value = text; }
  return { status: response.status, value };
}
const create = (name, query, alertsEnabled = true, key = randomUUID()) => request('/directory/saved-searches', {
  key, body: { name, canonicalQuery: query, alertsEnabled, alertFrequency: 'daily' },
});
try {
  for (let attempt = 0; attempt < 120; attempt += 1) {
    assert.equal(server.exitCode, null, 'Owned backend exited during startup');
    try { if ((await fetch(api + '/health')).ok) break; } catch { /* startup only */ }
    if (attempt === 119) throw new Error('Owned backend startup deadline');
    await delay(250);
  }
  const listener = execFileSync('lsof', ['-tiTCP:' + port, '-sTCP:LISTEN'], { encoding: 'utf8' }).trim().split(/\s+/);
  assert.deepEqual(listener, [String(server.pid)], 'Refuse to mutate a listener owned by another process');
  const version = await request('/version', { actor: null });
  assert.equal(version.status, 200);
  assert.match(version.value.commit, /^[0-9a-f]{40}$/);
  if (process.env.CI === 'true') {
    assert.equal(version.value.commit, sourceCommit, 'CI must compile this exact committed candidate');
  } else if (process.env.TDF_DIRECTORY_ALERT_PARITY_COMPILED_COMMIT) {
    assert.equal(version.value.commit, process.env.TDF_DIRECTORY_ALERT_PARITY_COMPILED_COMMIT);
  }
  console.log(JSON.stringify({ qualificationSourceCommit: sourceCommit, compiledCommit: version.value.commit,
    binarySha256: createHash('sha256').update(readFileSync(binary)).digest('hex'),
    sourceDirectorySha256: createHash('sha256').update(readFileSync('tdf-hq/src/TDF/Server/Directory.hs')).digest('hex'),
    scope: 'Compiled identity is measured without deployment overrides; local source/binary correspondence is bounded by recorded hashes and real HTTP assertions; CI requires exact HEAD' }));
  sql(`INSERT INTO party(id,display_name,is_org,created_at) VALUES
   (900055,'HTTP alert owner',false,now()),(900056,'HTTP alert outsider',false,now());
   INSERT INTO api_token(token,party_id,label,active) VALUES
   ('${tokens.owner}',900055,'synthetic owned alert test',true),('${tokens.outsider}',900056,'synthetic outsider test',true);
   INSERT INTO directory_age_assurance(account_party_id,assurance_status,verified_at,updated_at)
   VALUES(900055,'adult_attested',now(),now());`);
  assert.equal((await request('/directory/saved-searches', { actor: null })).status, 401);
  const denied = await create('Radio no soportado', { radiusKm: 10 });
  assert.equal(denied.status, 400);
  assert.match(denied.value, /Quita el radio/);
  for (const [query, enabled] of [[{ latitude: -0.18, longitude: -78.46 }, false], [{ location: { latitude: -0.18 } }, true], [{ remote: 'true' }, true], [{ genreId: 'invalid' }, true], [{ q: '\u0000' }, true]]) {
    assert.equal((await create('Invalid query', query, enabled)).status, 400);
  }
  for (const literal of ['1e100000', '1e-20000']) {
    const invalidNumeric = await request('/directory/saved-searches', {
      rawBody: '{"name":"Invalid numeric range","canonicalQuery":{"radiusKm":' + literal + '},"alertsEnabled":false,"alertFrequency":"daily"}', key: randomUUID(),
    });
    assert.equal(invalidNumeric.status, 400, 'Reject out-of-jsonb numeric range before casting canonical JSON');
  }
  for (const field of ['cityId', 'professionId', 'serviceId', 'instrumentId', 'genreId']) {
    for (const value of ['', '   ', '10000000000040008000000000000001', '{10000000-0000-4000-8000-000000000001}']) {
      const publicQuery = await request('/directory/search?' + new URLSearchParams({ [field]: value }), { actor: null });
      assert.equal(publicQuery.status, 400, 'Public UUID parser rejects noncanonical values');
      assert.equal((await create('Public UUID parity', { [field]: value })).status, 400);
    }
  }
  assert.equal((await create('Null optional UUID', { genreId: null })).status, 201);
  const canonicalUuid = 'AAAAAAAA-0000-F000-C000-000000000001';
  assert.equal((await request('/directory/search?' + new URLSearchParams({ genreId: canonicalUuid }), { actor: null })).status, 200);
  assert.equal((await create('Canonical nonstandard UUID bits', { genreId: canonicalUuid })).status, 201);
  const radiusOff = await create('Radio sin alertas', { radiusKm: 10 }, false);
  assert.equal(radiusOff.status, 201);
  const query = { q: 'HTTP Parity Person', entityType: 'all' };
  const spacedQuery = await create('Public whitespace parity', { q: '\t ' + query.q + '\u00a0' });
  assert.equal(spacedQuery.status, 201);
  assert.deepEqual(spacedQuery.value.canonicalQuery, { q: '\t ' + query.q + '\u00a0' });
  const replayKey = randomUUID();
  const all = await create('Alerta recuperada sin radio', query, true, replayKey);
  assert.equal(all.status, 201);
  assert.deepEqual(all.value.canonicalQuery, query, 'Keep the valid JSON and hash behavior');
  assert.equal((await create('Alerta recuperada sin radio', query, true, replayKey)).value.id, all.value.id);
  const remote = await create('Solo remoto', { q: query.q, remote: true });
  assert.equal(remote.status, 201);
  const listed = await request('/directory/saved-searches');
  assert.equal(listed.status, 200);
  assert.ok(listed.value.some((row) => row.id === all.value.id));
  assert.ok(!(await request('/directory/saved-searches', { actor: 'outsider' })).value.some((row) => row.id === all.value.id));
  const profile = {
    profileKind: 'person', publicName: query.q, slug: 'http-parity-person',
    bio: 'Perfil sintético para verificar filtros y entrega interna de alertas.',
    professionIds: ['21000000-0000-4000-8000-000000000001'], instrumentIds: [],
    genreIds: ['2109e4ab-c2b5-493a-aa5e-97c00dd6fa9c'], serviceOfferingIds: [],
    countryId: '1cb3600a-c7e3-4f5f-8e67-55db001de6d5',
    cityId: '24000000-0000-4000-8000-000000000002',
    onsite: true, remote: false, availableToTravel: false,
    coverImageUrl: 'https://cdn.example.test/alert-profile.jpg',
  };
  const made = await request('/directory/profiles', { body: profile, key: randomUUID() });
  assert.equal(made.status, 201, JSON.stringify(made.value));
  const id = made.value.id;
  const published = await request(`/directory/profiles/${id}/status`, { method: 'PATCH', body: { status: 'published' } });
  assert.equal(published.status, 200, JSON.stringify(published.value));
  const deliveries = (savedId) => Number(sql(`SELECT count(*) FROM directory_alert_delivery WHERE saved_search_id='${savedId}' AND result_kind='profile' AND result_id='${id}';`));
  assert.equal(deliveries(all.value.id), 1, 'all sentinel must match exactly once');
  assert.equal(deliveries(remote.value.id), 0, 'onsite profile must not alert a remote-only search');
  const remoteQuery = '/directory/search?' + new URLSearchParams({ q: query.q, remote: 'true' });
  assert.ok(!(await request(remoteQuery, { actor: null })).value.items.some((row) => row.id === id));
  assert.equal((await request(`/directory/profiles/${id}`, { method: 'PUT', body: { ...profile, remote: true } })).status, 200);
  assert.equal(deliveries(remote.value.id), 1);
  assert.ok((await request(remoteQuery, { actor: null })).value.items.some((row) => row.id === id));
  sql(`UPDATE directory_search_document SET source_status=source_status WHERE entity_kind='profile' AND entity_id='${id}';`);
  assert.equal(deliveries(remote.value.id), 1, 'same-version trigger replay stays deduplicated');
  assert.equal(Number(sql(`SELECT count(*) FROM directory_alert_delivery WHERE saved_search_id='${remote.value.id}' AND internal_notification_id IS NOT NULL;`)), 1);
  assert.equal(Number(sql(`SELECT count(*) FROM directory_alert_delivery WHERE email_status<>'disabled' OR push_status<>'disabled';`)), 0);
  const deliveryId = sql(`SELECT id FROM directory_alert_delivery WHERE saved_search_id='${remote.value.id}' AND result_id='${id}' ORDER BY matched_at LIMIT 1;`);
  const context = await request(`/directory/notification-context/alert/${deliveryId}`);
  assert.equal(context.status, 200);
  assert.equal(context.value.resultId, id);
  assert.equal(context.value.slug, profile.slug);
  assert.equal((await request(`/directory/notification-context/alert/${deliveryId}`, { actor: 'outsider' })).status, 404);
  for (const [raw, normalized] of [
    ['2026-10-25T12:00:00+23:59', '2026-10-24T12:01:00Z'],
    ['2026-10-25T12:00:60.5Z', '2026-10-25T12:01:00.5Z'],
  ]) {
    const dateAlert = await create('Exact UTC boundary ' + raw, { q: query.q, dateTo: raw });
    assert.equal(dateAlert.status, 201);
    const version = sql(`UPDATE directory_search_document SET effective_at='${normalized}'::timestamptz,source_version=source_version+1 WHERE entity_kind='profile' AND entity_id='${id}' RETURNING source_version;`);
    const pageAtBoundary = await request('/directory/search?' + new URLSearchParams({ q: query.q, dateTo: raw }), { actor: null });
    assert.ok(pageAtBoundary.value.items.some((row) => row.id === id), 'Public typed UTC must include exact boundary');
    assert.equal(Number(sql(`SELECT count(*) FROM directory_alert_delivery WHERE saved_search_id='${dateAlert.value.id}' AND result_id='${id}' AND result_version=${version};`)), 1);
    const nextVersion = sql(`UPDATE directory_search_document SET effective_at='${normalized}'::timestamptz+interval '1 microsecond',source_version=source_version+1 WHERE entity_kind='profile' AND entity_id='${id}' RETURNING source_version;`);
    const pageOutsideBoundary = await request('/directory/search?' + new URLSearchParams({ q: query.q, dateTo: raw }), { actor: null });
    assert.ok(!pageOutsideBoundary.value.items.some((row) => row.id === id));
    assert.equal(Number(sql(`SELECT count(*) FROM directory_alert_delivery WHERE saved_search_id='${dateAlert.value.id}' AND result_id='${id}' AND result_version=${nextVersion};`)), 0);
  }
  // The new save validator uses the public UTCTime parser. Compare acceptance,
  // rather than assuming PostgreSQL's permissive time grammar is equivalent.
  for (const date of ['2026-10-25T12:00:00Z', '2026-10-25T12:00:00.123456Z', '2026-10-25T12:00:00+05:30', '2026-10-25t12:00:00z', '2026-10-25 12:00:00Z', '2026-10-25T12:00:00+0530', '2026-10-25T12:00:00+05', '2026-10-25T12:00Z', '2026-10-25T12:00:00+23:59', '2026-10-25T12:00:60.5Z', '2026-02-30T12:00:00Z', '2026-10-25T24:00:00Z', 'tomorrow', 'infinity', '10/25/2026']) {
    const publicRead = await request('/directory/search?' + new URLSearchParams({ dateFrom: date }), { actor: null });
    const saved = await create('Date parity ' + date, { dateFrom: date });
    assert.equal(saved.status, publicRead.status === 200 ? 201 : 400, `UTCTime acceptance differs for ${date}`);
    if (saved.status === 201) assert.deepEqual(saved.value.canonicalQuery, { dateFrom: date });
  }
  assert.equal((await request(`/directory/profiles/${id}/status`, { method: 'PATCH', body: { status: 'paused' } })).status, 200);
  assert.equal((await request(`/directory/notification-context/alert/${deliveryId}`)).status, 404, 'A paused result no longer resolves through old notification');
  assert.equal((await request(`/directory/profiles/${id}/status`, { method: 'PATCH', body: { status: 'published' } })).status, 200);
  assert.equal((await request(`/directory/notification-context/alert/${deliveryId}`)).status, 200);
  console.log(`PASS: ${requests} real HTTP requests, owned actor, invalid/radio recovery, persistence/replay, query parity, outsider isolation, date acceptance; no external delivery`);
} finally {
  if (server.exitCode === null && server.signalCode === null) {
    process.kill(-server.pid, 'SIGTERM');
    await Promise.race([new Promise((resolve) => server.once('exit', resolve)), delay(10000)]);
    if (server.exitCode === null && server.signalCode === null) process.kill(-server.pid, 'SIGKILL');
  }
  closeSync(log);
  console.log(`Owned HTTP log retained at ${work}; backend stopped`);
}
