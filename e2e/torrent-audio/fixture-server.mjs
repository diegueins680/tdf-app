import fs from 'node:fs';
import path from 'node:path';
import http from 'node:http';
import crypto from 'node:crypto';
import { fileURLToPath } from 'node:url';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const dist = path.join(root, 'artifacts/torrent-audio-fixture/dist');
// One second of synthetic PCM silence; no artist recordings or real torrents.
export const wav = Buffer.alloc(16044);
for (const [offset, value] of [[0, 'RIFF'], [8, 'WAVEfmt '], [36, 'data']]) wav.write(value, offset);
wav.writeUInt32LE(wav.length - 8, 4); wav.writeUInt32LE(16, 16); wav.writeUInt16LE(1, 20); wav.writeUInt16LE(1, 22);
wav.writeUInt32LE(8000, 24); wav.writeUInt32LE(16000, 28); wav.writeUInt16LE(2, 32); wav.writeUInt16LE(16, 34); wav.writeUInt32LE(wav.length - 44, 40);
function bencode(value) {
  if (Buffer.isBuffer(value) || typeof value === 'string') { const bytes = Buffer.from(value); return Buffer.concat([Buffer.from(`${bytes.length}:`), bytes]); }
  if (typeof value === 'number') return Buffer.from(`i${value}e`);
  if (Array.isArray(value)) return Buffer.concat([Buffer.from('l'), ...value.map(bencode), Buffer.from('e')]);
  return Buffer.concat([Buffer.from('d'), ...Object.keys(value).sort().flatMap(key => [bencode(key), bencode(value[key])]), Buffer.from('e')]);
}
export async function startFixture({ delayedClaim = false } = {}) {
  const requests = []; let origin;
  const server = http.createServer((request, response) => {
    response.setHeader('content-security-policy', "default-src 'self'; script-src 'self' 'unsafe-inline'; connect-src 'self'; media-src 'self' blob:; worker-src 'self' blob:; object-src 'none'; base-uri 'none'");
    const url = new URL(request.url, origin);
    requests.push({ pathname: url.pathname, search: url.search, range: request.headers.range });
    if (url.pathname === '/fixture.torrent') {
      const filename = url.searchParams.get('variant') === 'b' ? 'synthetic-b.wav' : 'synthetic.wav';
      const metadata = bencode({ 'announce-list': [], 'url-list': [origin + '/webseed/' + filename], info: { name: filename, length: wav.length, 'piece length': 16384, pieces: crypto.createHash('sha1').update(wav).digest(), private: 1 } });
      response.setHeader('content-type', 'application/x-bittorrent'); response.end(metadata); return;
    }
    if (/^\/webseed\/synthetic(?:-b)?\.wav$/.test(url.pathname)) {
      let start = 0; let end = wav.length - 1;
      if (request.headers.range) {
        const match = /^bytes=(\d+)-(\d*)$/.exec(request.headers.range);
        if (!match) { response.writeHead(416); response.end(); return; }
        start = Number(match[1]); end = match[2] ? Math.min(Number(match[2]), end) : end;
        if (start > end || start >= wav.length) { response.writeHead(416); response.end(); return; }
        response.statusCode = 206; response.setHeader('content-range', `bytes ${start}-${end}/${wav.length}`);
      }
      response.setHeader('content-type', 'audio/wav'); response.setHeader('accept-ranges', 'bytes'); response.setHeader('content-length', end - start + 1);
      response.end(request.method === 'HEAD' ? undefined : wav.subarray(start, end + 1)); return;
    }
    if (url.pathname === '/foreign-sw.js') {
      response.setHeader('content-type', 'application/javascript'); response.end('self.addEventListener("install",()=>self.skipWaiting());self.addEventListener("activate",e=>e.waitUntil(self.clients.claim()));'); return;
    }
    const file = path.resolve(dist, '.' + decodeURIComponent(url.pathname));
    if (file.startsWith(dist + path.sep) && fs.existsSync(file) && fs.statSync(file).isFile()) {
      response.setHeader('content-type', file.endsWith('.js') ? 'application/javascript' : file.endsWith('.html') ? 'text/html' : 'application/octet-stream');
      let body = fs.readFileSync(file);
      if (delayedClaim && url.pathname === '/webtorrent-sw.min.js') {
        const source = body.toString(); const needle = "self.addEventListener('activate', event => { event.waitUntil(self.clients.claim()); });";
        if (!source.includes(needle)) { response.writeHead(500); response.end('Unrecognized test claim fixture'); return; }
        body = Buffer.from(source.replace(needle, "self.addEventListener('activate', () => {}); self.addEventListener('message', event => { if (event.data === 'test-claim') event.waitUntil(self.clients.claim()); });").replace("event.data?.type === 'tdf-torrent-claim'", "event.data?.type === 'test-disabled-claim'"));
      }
      response.end(body); return;
    }
    response.setHeader('content-type', 'text/plain'); response.end('unrelated-response-sentinel');
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  origin = `http://127.0.0.1:${server.address().port}`;
  return { origin, requests, close: async () => { server.closeAllConnections(); await new Promise(resolve => server.close(resolve)); } };
}


export async function withFixture(options, run) {
  const fixture = await startFixture(options);
  try { return await run(fixture); }
  finally { await fixture.close(); }
}
