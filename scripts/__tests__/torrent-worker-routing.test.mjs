import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const body = fs.readFileSync(path.join(root, 'tdf-hq-ui/public/webtorrent-sw.min.js'), 'utf8');
function route(url) {
  const listeners = new Map();
  vm.runInNewContext(body, {
    self: { registration: { scope: 'https://app.example/' }, addEventListener: (type, handler) => listeners.set(type, handler) },
    clients: { matchAll: async () => [] }, Response, ReadableStream, MessageChannel, setTimeout, clearTimeout,
  });
  const responses = [];
  listeners.get('fetch')({ request: new Request(url), respondWith: response => responses.push(response) });
  return responses;
}
for (const url of [
  'https://app.example/inicio',
  'https://app.example/api/catalog?next=https://app.example/webtorrent/cancel/',
  'https://app.example/inicio?next=https://app.example/webtorrent/keepalive/',
  'https://app.example/webtorrentish/cancel/',
  'https://other.example/webtorrent/cancel/',
]) {
  test(`unrelated request remains untouched: ${url}`, () => assert.equal(route(url).length, 0));
}
test('actual keepalive and cancel control routes still respond', () => {
  for (const name of ['keepalive', 'cancel']) {
    const responses = route(`https://app.example/webtorrent/${name}/`);
    assert.equal(responses.length, 1);
    assert.ok(responses[0] instanceof Response);
    assert.equal(responses[0].status, 200);
  }
});

function protocolFixture() {
  const handlers = new Map(); const windows = new Map(); const sent = []; const allPorts = [];
  const timers = new Map(); let next = 0;
  const clients = { get: async id => windows.get(id), matchAll: async () => [...windows.values()] };
  vm.runInNewContext(body, {
    self: { registration: { scope: 'https://app.example/' }, clients, addEventListener: (type, handler) => handlers.set(type, handler) },
    clients, Response, ReadableStream, MessageChannel,
    setTimeout: (handler, delay) => { const id = ++next; timers.set(id, { handler, delay }); return id; },
    clearTimeout: id => timers.delete(id),
  });
  function client(id, response) {
    const window = { postMessage(message, ports) {
      sent.push(id); const port = ports[0]; allPorts.push(port);
      port.onmessage = event => { if (event.data === false) port.close(); };
      if (response) port.postMessage(response);
    } };
    windows.set(id, window); return window;
  }
  function request(clientId) {
    let response;
    handlers.get('fetch')({ clientId, request: new Request('https://app.example/webtorrent/fixture/file.wav'), respondWith: value => { response = value; } });
    return response;
  }
  return { client, request, sent, timers, close: () => { for (const port of allPorts) port.close(); } };
}

test('a controlled request is sent only to its originating document', { timeout: 2_000 }, async t => {
  const fixture = protocolFixture(); t.after(fixture.close);
  fixture.client('unrelated', { status: 404, body: 'wrong document' });
  fixture.client('owner', { status: 200, body: 'owned payload' });
  const response = await fixture.request('owner');
  assert.equal(response.status, 200); assert.equal(await response.text(), 'owned payload');
  assert.deepEqual(fixture.sent, ['owner']); assert.equal(fixture.timers.size, 0);
});

test('a missing originating document fails closed without broadcasting', { timeout: 2_000 }, async t => {
  const fixture = protocolFixture(); t.after(fixture.close);
  fixture.client('unrelated', { status: 200, body: 'other data' });
  assert.equal((await fixture.request('closed-document')).status, 404);
  assert.deepEqual(fixture.sent, []);
});

test('a nonresponding document has a bounded first-reply wait', { timeout: 2_000 }, async t => {
  const fixture = protocolFixture(); t.after(fixture.close); fixture.client('owner');
  const response = fixture.request('owner');
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(fixture.timers.size, 1);
  const [{ handler, delay }] = fixture.timers.values(); assert.equal(delay, 15_000);
  handler(); assert.equal((await response).status, 404); assert.equal(fixture.timers.size, 0);
});

test('the explicitly limited empty-client legacy path still accepts a player response', { timeout: 2_000 }, async t => {
  const fixture = protocolFixture(); t.after(fixture.close);
  fixture.client('legacy', { status: 200, body: 'legacy payload' });
  const response = await fixture.request(''); assert.equal(await response.text(), 'legacy payload');
  assert.deepEqual(fixture.sent, ['legacy']);
});


test('only the explicit control message requests an eligible-client claim', { timeout: 2_000 }, async () => {
  const handlers = new Map(); let claims = 0; const waits = [];
  const clients = { claim: async () => { claims++; } };
  vm.runInNewContext(body, { self: { clients, registration: { scope: 'https://app.example/' }, addEventListener: (type, handler) => handlers.set(type, handler) }, clients, Response, ReadableStream, MessageChannel, setTimeout, clearTimeout });
  const send = data => handlers.get('message')({ data, waitUntil: promise => waits.push(promise) });
  send({ type: 'unrelated' }); assert.equal(claims, 0);
  send({ type: 'tdf-torrent-claim' }); assert.equal(claims, 1); assert.equal(waits.length, 1);
  await waits[0];
});
