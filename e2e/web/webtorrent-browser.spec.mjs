import { test, expect } from '@playwright/test';
import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const digest = bytes => crypto.createHash('sha256').update(bytes).digest('hex');

test('@critical official WebTorrent browser module imports and releases its service worker without peers', async ({ page, context, baseURL, browserName }, testInfo) => {
  const manifest = JSON.parse(fs.readFileSync(path.join(root, 'third_party/webtorrent/browser.json'), 'utf8'));
  const dist = path.join(root, 'tdf-hq-ui/dist');
  const candidates = fs.readdirSync(path.join(dist, 'assets')).filter(name => /^webtorrent\.min-.*\.js$/.test(name));
  expect(candidates).toHaveLength(1);
  const asset = `/assets/${candidates[0]}`;
  expect(digest(fs.readFileSync(path.join(dist, asset)))).toBe(manifest.asset.sha256);
  expect(fs.readFileSync(path.join(dist, 'webtorrent-sw.min.js'))).toEqual(fs.readFileSync(path.join(root, 'tdf-hq-ui/public/webtorrent-sw.min.js')));
  const external = [];
  const origin = new URL(baseURL).origin;
  // No application API or real torrents are loaded. Abort and record any external attempt.
  await context.route('**/*', async route => {
    const url = new URL(route.request().url());
    if (url.origin !== origin) { external.push(url.origin); return route.abort(); }
    if (url.pathname === '/__webtorrent-browser-probe') return route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Isolated browser component probe</title>' });
    return route.continue();
  });
  await page.goto('/__webtorrent-browser-probe');
  const result = await page.evaluate(async ({ asset }) => {
    const { default: WebTorrent } = await import(asset);
    const nativeWebRTC = Boolean(globalThis.RTCPeerConnection || globalThis.mozRTCPeerConnection || globalThis.webkitRTCPeerConnection);
    if (Boolean(WebTorrent.WEBRTC_SUPPORT) !== nativeWebRTC) throw new Error('Browser WebRTC capability and library support disagree');
    const registration = await navigator.serviceWorker.register('/webtorrent-sw.min.js', { scope: '/torrent-stream/' });
    let client;
    try {
      if (registration.active?.state !== 'activated') {
        const worker = registration.installing ?? registration.waiting ?? registration.active;
        if (!worker) throw new Error('No service worker');
        await new Promise((resolve, reject) => {
          const check = () => {
            if (worker.state === 'activated') { clearTimeout(timer); worker.removeEventListener('statechange', check); resolve(); }
            else if (worker.state === 'redundant') { clearTimeout(timer); worker.removeEventListener('statechange', check); reject(new Error('Worker redundant')); }
          };
          const timer = setTimeout(() => { worker.removeEventListener('statechange', check); reject(new Error('Worker activation timed out')); }, 10_000);
          worker.addEventListener('statechange', check); check();
        });
      }
      if (!WebTorrent.WEBRTC_SUPPORT) return { moduleType: typeof WebTorrent, nativeWebRTC, webRTC: false, workerState: registration.active.state, scope: new URL(registration.scope).pathname, constructorLifecycle: 'NOT_APPLICABLE_NATIVE_WEBRTC_UNAVAILABLE' };
      client = new WebTorrent({ tracker: false, dht: false, lsd: false });
      const server = client.createServer({ controller: registration }, 'browser');
      const before = { moduleType: typeof WebTorrent, nativeWebRTC, webRTC: WebTorrent.WEBRTC_SUPPORT, torrents: client.torrents.length, workerState: registration.active.state, scope: new URL(registration.scope).pathname, server: Boolean(server) };
      await new Promise((resolve, reject) => client.destroy(error => error ? reject(error) : resolve()));
      return { ...before, destroyed: client.destroyed };
    } finally {
      if (client && !client.destroyed) client.destroy();
      await registration.unregister();
    }
  }, { asset });
  await testInfo.attach('browser-component-lifecycle', { body: JSON.stringify(result), contentType: 'application/json' });
  if (browserName === 'chromium' || browserName === 'firefox') expect(result.webRTC).toBe(true);
  if (result.webRTC) {
    expect(result).toEqual({ moduleType: 'function', nativeWebRTC: true, webRTC: true, torrents: 0, workerState: 'activated', scope: '/torrent-stream/', server: true, destroyed: true });
  } else {
    // Production explicitly refuses torrent playback without native WebRTC.
    // Module import and worker lifecycle still execute; constructor is not claimed.
    expect(result).toEqual({ moduleType: 'function', nativeWebRTC: false, webRTC: false, workerState: 'activated', scope: '/torrent-stream/', constructorLifecycle: 'NOT_APPLICABLE_NATIVE_WEBRTC_UNAVAILABLE' });
  }
  expect(await page.evaluate(async () => (await navigator.serviceWorker.getRegistrations()).length)).toBe(0);
  expect(external).toEqual([]);
  const notices = await page.request.get('/third-party/webtorrent-NOTICES.txt');
  expect(notices.ok()).toBeTruthy();
  expect(digest(await notices.body())).toBe(manifest.notices.sha256);
});
