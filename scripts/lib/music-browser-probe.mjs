import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { mkdtempSync } from 'node:fs';
import http from 'node:http';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = dirname(dirname(dirname(fileURLToPath(import.meta.url))));

// Test-only trusted edge: no response substitution, CORS rewriting or storage
// proxying. Country is fixture data, never accepted from a browser header.
export async function startMusicTestEdge(apiBase) {
  assert.match(apiBase, /^http:\/\/127\.0\.0\.1:\d+$/);
  const target = new URL(apiBase);
  const server = http.createServer((req, res) => {
    if (!req.url?.startsWith('/') || req.url.startsWith('//')) {
      res.writeHead(400).end(); return;
    }
    const headers = { ...req.headers, host: target.host, 'cf-ipcountry': 'EC' };
    for (const name of ['forwarded', 'x-forwarded-host', 'x-forwarded-proto', 'x-forwarded-for']) delete headers[name];
    const upstream = http.request({ hostname: target.hostname, port: target.port,
      path: req.url, method: req.method, headers, timeout: 30000 }, (response) => {
      res.writeHead(response.statusCode, response.headers);
      response.on('error', () => res.destroy()); response.pipe(res);
    });
    upstream.on('timeout', () => upstream.destroy());
    upstream.on('error', () => { if (!res.headersSent) res.writeHead(502); res.end(); });
    req.on('aborted', () => upstream.destroy());
    res.on('close', () => upstream.destroy());
    req.pipe(upstream);
  });
  await new Promise((resolve, reject) => {
    server.once('error', reject); server.listen(0, '127.0.0.1', resolve);
  });
  return { endpoint: `http://127.0.0.1:${server.address().port}`,
    close: () => new Promise((resolve) => { server.close(resolve); server.closeAllConnections(); }) };
}

export async function runMusicBrowserProbe({ apiBase, storageEndpoint, password, masterAssetId, recordingId }) {
  assert.match(storageEndpoint, /^https:\/\/127\.0\.0\.1:\d+$/);
  const edge = await startMusicTestEdge(apiBase);
  const artifacts = mkdtempSync(join(tmpdir(), 'tdf-music-browser-results-'));
  console.log(`Real-browser evidence directory: ${artifacts}`);
  try {
    await new Promise((resolve, reject) => {
      // No DB/provider credentials reach Vite or the browser process. The only
      // password is the random, disposable persona login for this database.
      const child = spawn(process.execPath, [join(root, 'node_modules/@playwright/test/cli.js'),
        'test', '--config=playwright.music-integration.config.mjs'], {
        cwd: root, stdio: 'inherit', env: { PATH: process.env.PATH, HOME: process.env.HOME,
          PLAYWRIGHT_PORT: '4187', PLAYWRIGHT_ARTIFACT_DIR: artifacts,
          TDF_MUSIC_BROWSER_API: edge.endpoint, TDF_MUSIC_BROWSER_STORAGE: storageEndpoint,
          TDF_MUSIC_BROWSER_PASSWORD: password, TDF_MUSIC_BROWSER_MASTER: masterAssetId,
          TDF_MUSIC_BROWSER_RECORDING: recordingId },
      });
      child.once('error', reject);
      child.once('close', (code) => code === 0 ? resolve() : reject(new Error(`Real-browser integration failed (${code}); see ${artifacts}`)));
    });
  } finally { await edge.close(); }
}
