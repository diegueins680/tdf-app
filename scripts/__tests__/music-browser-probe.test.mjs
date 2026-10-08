import assert from 'node:assert/strict';
import http from 'node:http';
import test from 'node:test';
import { startMusicTestEdge } from '../lib/music-browser-probe.mjs';

test('browser edge rejects non-loopback or ambiguous upstreams before listening', async () => {
  for (const value of ['https://api.tdf.test', 'http://localhost:8080', 'http://127.0.0.1:8080/path',
    'http://user@127.0.0.1:8080', 'http://127.0.0.1:8080?host=remote', 'http://127.0.0.1:8080\n']) {
    await assert.rejects(startMusicTestEdge(value));
  }
});

test('trusted local edge forwards bytes, status and CORS unchanged, overriding spoofed geography', async () => {
  let received;
  const upstream = http.createServer(async (req, res) => {
    const chunks = []; for await (const chunk of req) chunks.push(chunk);
    received = { headers: req.headers, path: req.url, body: Buffer.concat(chunks).toString() };
    res.writeHead(409, { 'Content-Type': 'application/json', 'Access-Control-Allow-Origin': 'http://127.0.0.1:4187' });
    res.end('{"real":"upstream conflict"}');
  });
  await new Promise((resolve) => upstream.listen(0, '127.0.0.1', resolve));
  let edge;
  try {
    edge = await startMusicTestEdge(`http://127.0.0.1:${upstream.address().port}`);
    const response = await fetch(`${edge.endpoint}/music/test?x=1`, { method: 'POST', body: '{"payload":1}',
      headers: { Origin: 'http://127.0.0.1:4187', 'CF-IPCountry': 'US', 'X-Forwarded-For': '203.0.113.1',
        Authorization: 'Bearer synthetic-test-token', 'Idempotency-Key': 'synthetic-test-key' } });
    assert.equal(response.status, 409);
    assert.equal(await response.text(), '{"real":"upstream conflict"}');
    assert.equal(response.headers.get('access-control-allow-origin'), 'http://127.0.0.1:4187');
    assert.equal(received.path, '/music/test?x=1'); assert.equal(received.body, '{"payload":1}');
    assert.equal(received.headers['cf-ipcountry'], 'EC');
    assert.equal(received.headers['x-forwarded-for'], undefined);
    assert.equal(received.headers.authorization, 'Bearer synthetic-test-token');
    assert.equal(received.headers['idempotency-key'], 'synthetic-test-key');
  } finally {
    await edge?.close();
    await new Promise((resolve) => { upstream.close(resolve); upstream.closeAllConnections(); });
  }
});
