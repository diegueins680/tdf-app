import assert from 'node:assert/strict';
import net from 'node:net';
import { spawn, execFileSync } from 'node:child_process';
import { mkdtempSync, mkdirSync, openSync, closeSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { randomUUID } from 'node:crypto';

const database = process.env.TDF_PUBLIC_CATALOG_TEST_DATABASE_URL;
const binary = process.env.TDF_PUBLIC_CATALOG_SERVER_BIN;
assert.ok(database && binary, 'isolated database and tested executable required');
const upstream = new URL(database);
assert.ok(['127.0.0.1', 'localhost'].includes(upstream.hostname) || (process.env.CI === 'true' && upstream.hostname === 'postgres'));
assert.match(upstream.pathname, /_test$/);
const runtime = mkdtempSync(join(tmpdir(), 'tdf-public-catalog-http-'));
const sql = query => {
  try { return execFileSync('psql', [database, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-c', query], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim(); }
  catch (error) { writeFileSync(join(runtime, 'sql-error.txt'), error.stderr ?? '', { mode: 0o600 }); throw new Error(`Isolated public catalog SQL failed; private diagnostics ${runtime}, connection details omitted`); }
};
mkdirSync(join(runtime, 'assets'));
const prefix = `catalog-${process.pid}-${Date.now()}`;
const port = Number(process.env.TDF_PUBLIC_CATALOG_SERVER_PORT ?? 18691);
const base = `http://127.0.0.1:${port}`;
const fixtures = Array.from({ length: 80 }, (_, index) => ({
  index, assetId: randomUUID(), listingId: randomUUID(),
  active: index % 5 !== 0, approved: index % 4 !== 0,
  price: 10000 + index, daily: 2000 + index, weekly: index % 3 ? 10000 + index : null,
}));
const executions = { marketplace: 0, records: 0 };
let deactivateAfterSelection = false;
let concurrentDeactivation = false;
const sockets = new Set();
// Count actual PostgreSQL Execute/SimpleQuery messages in catalog transactions,
// including BEGIN/COMMIT. Classify statements in memory to exclude independent
// background-worker transactions; never print or retain SQL/credentials. TLS is disabled only
// on this explicitly guarded isolated test connection; production is untouched.
const proxy = net.createServer(client => {
  const server = net.connect(Number(upstream.port || 5432), upstream.hostname);
  sockets.add(client); sockets.add(server);
  let buffer = Buffer.alloc(0), startup = true;
  let transactionCount = 0, transactionKinds = new Set();
  const statements = new Map(), portals = new Map();
  const classify = text => ({
    assetRead: /\bFROM\s+"?asset"?\b/i.test(text),
    begin: /^BEGIN\b/i.test(text), end: /^(COMMIT|ROLLBACK)\b/i.test(text),
    marketplace: /\b(?:FROM|JOIN)\s+"?(?:marketplace_listing|marketplace_rental_listing_terms|asset)"?\b/i.test(text),
    records: /\b(?:FROM|JOIN)\s+"?(?:editorial_collection|collection_external_resource|collection_release|collection_recording|collection_session|record_release|recording|recording_session|release_contributor|recording_contributor|session_contributor|release_external_resource|recording_external_resource|session_external_resource|record_contributor|record_external_resource|external_provider|workflow_definition|workflow_state|locale_reference)"?\b/i.test(text),
  });
  const observe = kind => {
    assert.ok(kind, 'unclassified database execution');
    if (deactivateAfterSelection && kind.assetRead) {
      // The first listings response has already reached the actual handler.
      // Block forwarding the next asset response until another connection has
      // committed deactivation, reproducing READ COMMITTED interleaving.
      deactivateAfterSelection = false;
      sql(`UPDATE marketplace_listing SET active=FALSE WHERE id='${fixtures[1].listingId}'`);
      concurrentDeactivation = true;
    }
    if (kind.begin) { transactionCount = 1; transactionKinds = new Set(); return; }
    for (const domain of ['marketplace', 'records']) if (kind[domain]) transactionKinds.add(domain);
    if (transactionCount) {
      transactionCount++;
      if (kind.end) {
        for (const domain of transactionKinds) executions[domain] += transactionCount;
        transactionCount = 0; transactionKinds = new Set();
      }
    } else for (const domain of ['marketplace', 'records']) if (kind[domain]) executions[domain]++;
  };
  client.on('data', chunk => {
    server.write(chunk);
    buffer = Buffer.concat([buffer, chunk]);
    if (startup) {
      if (buffer.length < 4 || buffer.length < buffer.readUInt32BE(0)) return;
      buffer = buffer.subarray(buffer.readUInt32BE(0)); startup = false;
    }
    while (buffer.length >= 5) {
      const size = buffer.readUInt32BE(1) + 1;
      if (buffer.length < size) break;
      const payload = buffer.subarray(5, size);
      if (buffer[0] === 81) observe(classify(payload.toString('utf8'))); // SimpleQuery
      if (buffer[0] === 80) { // Parse: retain only statement name and classification.
        const end = payload.indexOf(0);
        statements.set(payload.subarray(0, end).toString(), classify(payload.subarray(end + 1).toString()));
      }
      if (buffer[0] === 66) { // Bind: never inspect or retain parameter bytes.
        const end = payload.indexOf(0), statementEnd = payload.indexOf(0, end + 1);
        portals.set(payload.subarray(0, end).toString(), statements.get(payload.subarray(end + 1, statementEnd).toString()));
      }
      if (buffer[0] === 69) observe(portals.get(payload.subarray(0, payload.indexOf(0)).toString()));
      buffer = buffer.subarray(size);
    }
  });
  server.pipe(client);
  client.on('error', () => server.destroy()); server.on('error', () => client.destroy());
  client.on('close', () => { sockets.delete(client); server.destroy(); });
  server.on('close', () => { sockets.delete(server); client.destroy(); });
});
let child, fd, failure;
try {
  assert.equal(await fetch(base + '/health', { signal: AbortSignal.timeout(1000) }).then(() => true, () => false), false, 'refusing occupied port');
  await new Promise(resolve => proxy.listen(0, '127.0.0.1', resolve));
  const proxied = new URL(database); proxied.hostname = '127.0.0.1'; proxied.port = String(proxy.address().port); proxied.searchParams.set('sslmode', 'disable');
  sql('BEGIN;\n' + fixtures.map(f => `
    INSERT INTO asset(id,name,category,condition,status,owner,maintenance_policy)
      VALUES ('${f.assetId}','${prefix}-${f.index}','synthetic','Good','Active','TDF','None');
    INSERT INTO marketplace_listing(id,asset_id,title,purpose,price_usd_cents,markup_pct,currency,active,created_at,updated_at)
      VALUES ('${f.listingId}','${f.assetId}','${prefix}-${String(f.index).padStart(3, '0')}','rent',${f.price},25,'USD',${f.active},now(),now());
    INSERT INTO marketplace_rental_listing_terms(listing_id,daily_rate_usd_cents,weekly_rate_usd_cents,security_deposit_usd_cents,late_fee_usd_cents,min_days,max_days,cancellation_window_hours,timezone,terms_version,terms_summary,active,approved_at)
      VALUES ('${f.listingId}',${f.daily},${f.weekly ?? 'NULL'},500,100,2,20,48,'America/Guayaquil','synthetic-v1','Synthetic read-only catalog test',${f.approved},${f.approved ? 'now()' : 'NULL'});`).join('\n') + '\nCOMMIT;');
  fd = openSync(join(runtime, 'backend.log'), 'w', 0o600);
  child = spawn(binary, [], { env: {
    PATH: process.env.PATH, TMPDIR: runtime, APP_ENV: 'test', DATABASE_URL: proxied.toString(),
    APP_PORT: String(port), RESET_DB: 'false', RUN_MIGRATIONS: 'false', AUTO_APPLY_PRODUCTION_MIGRATIONS: 'false', SEED_DB: 'false',
    DEFAULT_LOCALE: 'es', HQ_ASSETS_DIR: join(runtime, 'assets'), EVENT_DISCOVERY_ENABLED: 'false',
    ARTIST_ENRICHMENT_ENABLED: 'false', EVENT_LOGISTICS_RECHECK_ENABLED: 'false',
  }, stdio: ['ignore', fd, fd] });
  let ready = false;
  for (let attempt = 0; attempt < 120; attempt++) {
    assert.equal(child.exitCode, null, `backend exited; private diagnostics ${runtime}`);
    const health = await fetch(base + '/health', { signal: AbortSignal.timeout(1000) }).then(r => r.json(), () => null);
    if (health?.status === 'ok' && health?.db === 'ok') { ready = true; break; }
    await new Promise(resolve => setTimeout(resolve, 250));
  }
  assert.ok(ready, 'test backend readiness not established');
  const before = executions.marketplace;
  const started = performance.now();
  const response = await fetch(base + '/marketplace', { signal: AbortSignal.timeout(30000) });
  assert.equal(response.status, 200);
  const all = await response.json();
  const count = executions.marketplace - before;
  const actual = all.filter(row => row.miTitle.startsWith(prefix));
  assert.equal(actual.length, fixtures.filter(f => f.active).length);
  assert.deepEqual(actual.map(r => r.miTitle), actual.map(r => r.miTitle).sort());
  for (const f of fixtures) {
    const row = actual.find(r => r.miListingId === f.listingId);
    if (!f.active) { assert.equal(row, undefined); continue; }
    assert.ok(row);
    assert.equal(row.miPriceUsdCents, f.approved ? f.daily : f.price);
    assert.equal(row.miRentalWeeklyPriceUsdCents ?? null, f.approved ? f.weekly : null);
    assert.equal(row.miRentalTermsVersion ?? null, f.approved ? 'synthetic-v1' : null);
    assert.equal(row.miRentalSecurityDepositUsdCents ?? null, f.approved ? 500 : null);
    assert.equal(row.miRentalMinDays ?? null, f.approved ? 2 : null);
    assert.equal(row.miRentalMaxDays ?? null, f.approved ? 20 : null);
    assert.equal(row.miRentalTimezone ?? null, f.approved ? 'America/Guayaquil' : null);
  }
  assert.ok(count > 0, 'wire observer must see real database executions');
  console.log(JSON.stringify({ scenario: 'public marketplace batch', fixtures: fixtures.length, published: actual.length, totalPublished: all.length, executions: count, elapsedMs: Math.round(performance.now() - started), actualPostgres: true }));
  for (const index of [1, 3, 4, 7]) {
    const expected = actual.find(row => row.miListingId === fixtures[index].listingId);
    const detail = await fetch(`${base}/marketplace/${fixtures[index].listingId}`);
    assert.equal(detail.status, 200);
    assert.deepEqual(await detail.json(), expected, 'batched list and authoritative detail must agree');
  }
  deactivateAfterSelection = true;
  const racedResponse = await fetch(base + '/marketplace', { signal: AbortSignal.timeout(30000) });
  assert.equal(racedResponse.status, 200);
  const racedListing = (await racedResponse.json()).find(row => row.miListingId === fixtures[1].listingId);
  assert.ok(concurrentDeactivation, 'real concurrent listing update must occur between selected rows and terms');
  assert.ok(racedListing, 'listing selected before deactivation remains in that response');
  assert.equal(racedListing.miPriceUsdCents, fixtures[1].daily, 'selected rental must retain approved terms across concurrent deactivation');
  assert.equal(racedListing.miRentalTermsVersion, 'synthetic-v1');
  assert.equal(sql(`SELECT active FROM marketplace_listing WHERE id='${fixtures[1].listingId}'`), 'f');
  const afterDeactivation = await fetch(base + '/marketplace');
  assert.ok(!(await afterDeactivation.json()).some(row => row.miListingId === fixtures[1].listingId), 'next request excludes the deactivated listing');
  console.log(JSON.stringify({scenario:'selected rental survives concurrent deactivation without base-price fallback', actualPostgres:true, status:'PASS'}));
  const expectedCollections = JSON.parse(sql(`SELECT COALESCE(json_agg(c.id ORDER BY c.id),'[]')
    FROM editorial_collection c JOIN workflow_state s ON c.workflow_state_id=s.id
    JOIN workflow_definition w ON s.workflow_id=w.id
    WHERE c.active AND c.collection_type IN ('release','recording','session')
      AND w.code='catalog-publication' AND s.code='published'`));
  const beforeFeed = executions.records;
  const feedResponse = await fetch(base + '/records/feed?locale=es', { signal: AbortSignal.timeout(30000) });
  assert.equal(feedResponse.status, 200);
  const feed = await feedResponse.json();
  const feedCount = executions.records - beforeFeed;
  assert.equal(feed.locale, 'es');
  for (const key of ['collections', 'recordings', 'releases', 'sessions']) assert.ok(Array.isArray(feed[key]));
  assert.deepEqual(feed.collections.map(row => row.id).sort(), expectedCollections);
  console.log(JSON.stringify({ scenario: 'published records feed', executions: feedCount, actualPostgres: true, fieldContract: 'passed' }));
  assert.ok(count <= 16, `public list exceeded bounded query budget: ${count} executions with 80 generated fixtures`);
  assert.ok(feedCount > 0 && feedCount <= 35, `records read exceeded transaction/query budget: ${feedCount}`);

} catch (error) {
  failure = error;
  throw error;
} finally {
  child?.kill('SIGTERM');
  for (const socket of sockets) socket.destroy();
  await new Promise(resolve => proxy.close(resolve));
  if (fd !== undefined) closeSync(fd);
  const ids = fixtures.map(f => `'${f.listingId}'`).join(',');
  try { sql(`DELETE FROM marketplace_rental_listing_terms WHERE listing_id IN (${ids}); DELETE FROM marketplace_rental_listing_terms_history WHERE listing_id IN (${ids}); DELETE FROM marketplace_listing WHERE id IN (${ids}); DELETE FROM asset WHERE id IN (${fixtures.map(f => `'${f.assetId}'`).join(',')});`); } catch (error) {
    if (!failure) throw error;
    console.error(error.message);
  }
}
