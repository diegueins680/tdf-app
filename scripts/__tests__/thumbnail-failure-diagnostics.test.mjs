import test from 'node:test';
import assert from 'node:assert/strict';
import { beginThumbnailDiagnostics, captureThumbnailFailure,
  shouldCaptureThumbnailFailure, syntheticThumbnailPath,
  THUMBNAIL_DIAGNOSTIC_LIMIT } from '../../e2e/web/helpers/thumbnail-failure-diagnostics.mjs';

const url = 'https://i.ytimg.com/vi/ooPsIHsikYU/mqdefault.jpg';
const videoIds = ['ooPsIHsikYU'];

function pageFixture() {
  const calls = { evaluate: 0, init: 0, responses: null };
  return { calls,
    on(name, callback) { assert.equal(name, 'response'); calls.responses = callback; },
    async addInitScript(_callback, args) { calls.init++; assert.deepEqual(args.allowed, videoIds); },
    async evaluate() { calls.evaluate++; return { events: [], currentImages: [], resources: [] }; },
  };
}

test('thumbnail failure capture selects only unexpected failures and timeouts', () => {
  assert.equal(shouldCaptureThumbnailFailure({ status: 'passed', expectedStatus: 'passed' }), false);
  assert.equal(shouldCaptureThumbnailFailure({ status: 'skipped', expectedStatus: 'passed' }), false);
  assert.equal(shouldCaptureThumbnailFailure({ status: 'interrupted', expectedStatus: 'passed' }), false);
  assert.equal(shouldCaptureThumbnailFailure({ status: 'failed', expectedStatus: 'failed' }), false);
  assert.equal(shouldCaptureThumbnailFailure({ status: 'failed', expectedStatus: 'passed' }), true);
  assert.equal(shouldCaptureThumbnailFailure({ status: 'timedOut', expectedStatus: 'passed' }), true);
});

test('successful and expected-failure cases never evaluate or attach diagnostics', async () => {
  const page = pageFixture();
  const recorder = await beginThumbnailDiagnostics(page, videoIds);
  const attach = () => assert.fail('No successful/expected-failure attachment');
  assert.equal(await captureThumbnailFailure(page, { status: 'passed', expectedStatus: 'passed', attach }, recorder), false);
  assert.equal(await captureThumbnailFailure(page, { status: 'failed', expectedStatus: 'failed', attach }, recorder), false);
  assert.equal(page.calls.evaluate, 0);
});

test('unexpected failure attachment preserves the primary failed status', async () => {
  const page = pageFixture();
  const recorder = await beginThumbnailDiagnostics(page, videoIds);
  recorder.noteRequest(url);
  let attached;
  const info = { status: 'failed', expectedStatus: 'passed', async attach(name, payload) { attached = { name, ...payload }; } };
  assert.equal(await captureThumbnailFailure(page, info, recorder), true);
  assert.equal(info.status, 'failed');
  assert.equal(page.calls.evaluate, 1);
  assert.equal(attached.name, 'synthetic-thumbnail-failure');
  assert.equal(JSON.parse(attached.body).network[0].path, '/vi/ooPsIHsikYU/mqdefault.jpg');
});

test('unavailable browser snapshot cannot erase the primary failure or expose exception arguments', async () => {
  const page = pageFixture();
  const recorder = await beginThumbnailDiagnostics(page, videoIds);
  page.evaluate = async () => { throw new Error('synthetic-private-exception-argument'); };
  let body;
  const info = { status: 'failed', expectedStatus: 'passed', async attach(_name, payload) { body = payload.body; } };
  assert.equal(await captureThumbnailFailure(page, info, recorder), true);
  assert.equal(info.status, 'failed');
  assert.deepEqual(JSON.parse(body).browser, { unavailable: true });
  assert.equal(body.includes('synthetic-private-exception-argument'), false);
});

test('attachment errors propagate and cannot produce a successful test status', async () => {
  const page = pageFixture();
  const recorder = await beginThumbnailDiagnostics(page, videoIds);
  const error = new Error('synthetic-attachment-failure');
  const info = { status: 'failed', expectedStatus: 'passed', async attach() { throw error; } };
  await assert.rejects(captureThumbnailFailure(page, info, recorder), value => value === error);
  assert.equal(info.status, 'failed');
});

test('thumbnail diagnostics accept only exact synthetic HTTPS image identities', () => {
  const ids = new Set(videoIds);
  assert.equal(syntheticThumbnailPath(url, ids), '/vi/ooPsIHsikYU/mqdefault.jpg');
  assert.equal(syntheticThumbnailPath('http://i.ytimg.com/vi/ooPsIHsikYU/mqdefault.jpg', ids), null);
  assert.equal(syntheticThumbnailPath('https://i.ytimg.com/vi/Cb7VGZJ6apo/mqdefault.jpg', ids), null);
  assert.equal(syntheticThumbnailPath(`${url}?synthetic-private-query=not-recorded`, ids), null);
  assert.equal(syntheticThumbnailPath('https://synthetic-user:synthetic-password@i.ytimg.com/vi/ooPsIHsikYU/mqdefault.jpg', ids), null);
  assert.equal(syntheticThumbnailPath('https://core.persona.test/session', ids), null);
});

test('diagnostic request cap leaves the original request counter independent', async () => {
  const page = pageFixture();
  const recorder = await beginThumbnailDiagnostics(page, videoIds);
  let originalRequestCount = 0;
  for (let i = 0; i < THUMBNAIL_DIAGNOSTIC_LIMIT + 5; i++) { originalRequestCount++; recorder.noteRequest(url); }
  recorder.noteRequest('https://core.persona.test/session');
  assert.equal(originalRequestCount, THUMBNAIL_DIAGNOSTIC_LIMIT + 5);
  assert.equal(recorder.network.length, THUMBNAIL_DIAGNOSTIC_LIMIT);
  assert.equal(recorder.dropped, 5);
  assert.ok(recorder.network.every(event => event.type === 'request' && event.path === '/vi/ooPsIHsikYU/mqdefault.jpg'));
});

test('actual response metadata is filtered and bounded independently of browser events', async () => {
  const page = pageFixture();
  const recorder = await beginThumbnailDiagnostics(page, videoIds);
  page.calls.responses({ url: () => 'https://core.persona.test/session', status: () => 401 });
  page.calls.responses({ url: () => url, status: () => 404 });
  assert.equal(recorder.network.length, 1);
  assert.equal(recorder.network[0].type, 'response');
  assert.equal(recorder.network[0].status, 404);
});
