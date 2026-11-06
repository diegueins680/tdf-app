import test from 'node:test';
import assert from 'node:assert/strict';
import { JSDOM } from 'jsdom';
import { beginThumbnailDiagnostics, captureThumbnailFailure,
  shouldCaptureThumbnailFailure, syntheticThumbnailPath,
  THUMBNAIL_DIAGNOSTIC_LIMIT, THUMBNAIL_MUTATION_NODE_LIMIT } from '../../e2e/web/helpers/thumbnail-failure-diagnostics.mjs';

const url = 'https://i.ytimg.com/vi/ooPsIHsikYU/mqdefault.jpg';
const videoIds = ['ooPsIHsikYU'];

function pageFixture() {
  const calls = { evaluate: 0, init: 0, responses: null, listeners: {} };
  return { calls,
    on(name, callback) { calls.listeners[name] = callback; if (name === 'response') calls.responses = callback; },
    async addInitScript(callback, args) { calls.init++; assert.deepEqual(args.allowed, videoIds); calls.callback = callback; calls.args = args; },
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


test('request identities correlate separate same-URL completions and sanitize failures', async () => {
  const page = pageFixture();
  const recorder = await beginThumbnailDiagnostics(page, videoIds);
  const request = (error = 'net::ERR_ABORTED') => ({ url: () => url, resourceType: () => 'image',
    timing: () => ({ responseEnd: 12.5 }), failure: () => ({ errorText: error }) });
  const first = request(), second = request('secret https://private.example/session?token=not-recorded');
  recorder.noteRequest(first); recorder.noteRequest(second);
  page.calls.responses({ url: () => url, request: () => first, status: () => 404 });
  page.calls.listeners.requestfinished(first); page.calls.listeners.requestfailed(second);
  assert.deepEqual(recorder.network.map(row => row.requestId), [1, 2, 1, 1, 2]);
  assert.equal(recorder.network[3].responseEnd, 12.5);
  assert.equal(recorder.network[4].failureCode, 'unavailable');
  const third = request(); recorder.noteRequest(third); page.calls.listeners.requestfailed(third);
  assert.equal(recorder.network.at(-1).failureCode, 'net::ERR_ABORTED');
  page.calls.listeners.requestfinished({ timing() { throw Error('private-error'); } });
  assert.equal(recorder.errors, 1);
  assert.equal(JSON.stringify(recorder).includes('private'), false);
});

async function browserFixture(body) {
  const page = pageFixture(); await beginThumbnailDiagnostics(page, videoIds);
  const dom = new JSDOM('<!doctype html><html><body></body></html>', { runScripts: 'outside-only' });
  try {
    // JSDOM implements MutationObserver but not ResourceTiming; no requests run here.
    Object.defineProperty(dom.window.performance, 'getEntriesByType', { value: () => [] });
    dom.window.eval(`(${page.calls.callback.toString()})(${JSON.stringify(page.calls.args)})`);
    await body(dom.window, () => new Promise(resolve => setImmediate(resolve)));
  } finally { dom.window.close(); }
}

test('passive insertion, source change and removal keep one image identity before load', () => browserFixture(async (window, flush) => {
  const image = window.document.createElement('img'); image.src = url;
  window.document.body.append(image); await flush();
  image.dispatchEvent(new window.Event('load'));
  image.src = url.replace('mqdefault', 'hqdefault'); await flush();
  image.remove(); await flush();
  const observed = window.__tdfSyntheticThumbnailFailure();
  assert.deepEqual(Array.from(observed.mutations, x => x.type), ['insert', 'attribute', 'remove']);
  assert.equal(observed.mutations[0].firstSeen, 'insert');
  assert.equal(observed.events[0].firstSeen, 'insert');
  assert.ok(observed.mutations.every(x => x.id === observed.events[0].id));
  assert.equal(observed.mutations[1].previousPath, '/vi/ooPsIHsikYU/mqdefault.jpg');
  assert.equal(observed.mutations[1].path, '/vi/ooPsIHsikYU/hqdefault.jpg');
  assert.equal(observed.mutations[2].connected, false);
  const unrelated = window.document.createElement('img'); unrelated.src = `${url}?private=not-recorded`;
  window.document.body.append(unrelated); await flush();
  assert.equal(JSON.stringify(window.__tdfSyntheticThumbnailFailure()).includes('private'), false);
}));

test('observer traversal, record processing and mutation rings report bounded omissions', () => browserFixture(async (window, flush) => {
  const fragment = window.document.createDocumentFragment();
  for (let i = 0; i < THUMBNAIL_MUTATION_NODE_LIMIT + 50; i++) fragment.append(window.document.createElement('span'));
  window.document.body.append(fragment); await flush();
  let result = window.__tdfSyntheticThumbnailFailure();
  assert.ok(result.nodesVisited <= THUMBNAIL_MUTATION_NODE_LIMIT);
  assert.ok(result.recordsVisited <= THUMBNAIL_MUTATION_NODE_LIMIT);
  assert.ok(result.truncatedSubtrees > 0);
  const image = window.document.createElement('img'); image.src = url; window.document.body.append(image); await flush();
  for (let batch = 0; batch < 3; batch++) {
    for (let i = 0; i < THUMBNAIL_DIAGNOSTIC_LIMIT + 10; i++) image.setAttribute('src', url);
    await flush();
  }
  result = window.__tdfSyntheticThumbnailFailure();
  assert.equal(result.mutations.length, THUMBNAIL_DIAGNOSTIC_LIMIT);
  assert.ok(result.mutationDropped > 0); assert.ok(result.recordsDropped > 0);
  assert.equal(result.observerErrors, 0);
}));


test('failure snapshot retains bounded synthetic nodes without scanning unrelated images', () => browserFixture(async (window, flush) => {
  const host = window.document.createElement('div');
  for (let i = 0; i < 80; i++) { const image = window.document.createElement('img'); image.src = 'https://unrelated.example/not-recorded'; host.append(image); }
  for (let i = 0; i < THUMBNAIL_DIAGNOSTIC_LIMIT + 1; i++) { const image = window.document.createElement('img'); image.src = url; host.append(image); }
  window.document.body.append(host); await flush();
  const result = window.__tdfSyntheticThumbnailFailure();
  assert.equal(result.currentImages.length, THUMBNAIL_DIAGNOSTIC_LIMIT);
  assert.equal(result.imageSnapshotDropped, 1);
  assert.equal(result.imageSnapshotScope, 'observed-synthetic-nodes-only');
  assert.ok(result.currentImages.every(image => image.path === '/vi/ooPsIHsikYU/mqdefault.jpg'));
  assert.equal(JSON.stringify(result).includes('unrelated.example'), false);
}));
