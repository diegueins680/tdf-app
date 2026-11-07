import { test as base, expect } from '@playwright/test';
import { withFixture, wav } from './fixture-server.mjs';

const test = base.extend({
  delayedClaim: [false, { option: true }],
  fixture: async ({ context, page, delayedClaim }, use) => {
    await withFixture({ delayedClaim }, async fixture => {
      const external = []; const sockets = [];
    try {
    await context.route('**/*', route => {
      if (new URL(route.request().url()).origin !== fixture.origin) {
        external.push(route.request().url()); return route.abort();
      }
      return route.continue();
    });
    await context.routeWebSocket('**', socket => { sockets.push(socket.url()); socket.close(); });
    await context.addInitScript(() => { window.cspViolations = []; document.addEventListener('securitypolicyviolation', event => window.cspViolations.push({ blockedURI: event.blockedURI, directive: event.violatedDirective })); });
    await page.goto(fixture.origin + '/browser-test/torrent-audio.html');
    await page.waitForFunction(() => Boolean(window.tdfTorrentAudio));
    await use({ ...fixture, external, sockets });
    }
    finally {
      await page.evaluate(async () => {
        window.session?.destroy(); window.controller?.abort();
        const audio = document.getElementById('audio'); audio.pause(); audio.removeAttribute('src'); audio.load();
        for (const registration of await navigator.serviceWorker.getRegistrations()) await registration.unregister();
      }).catch(() => {});
    }
    });
  },
});

async function start(page, variant = '') {
  return page.evaluate(async (variant) => {
    const bytes = await (await fetch('/fixture.torrent' + (variant ? '?variant=' + variant : ''))).arrayBuffer();
    window.controller = new AbortController(); window.sessionState = 'pending'; window.progress = [];
    window.sessionPromise = window.tdfTorrentAudio.createTorrentAudioSession({
      source: new File([bytes], 'synthetic.torrent', { type: 'application/x-bittorrent' }),
      audio: document.getElementById('audio'), signal: window.controller.signal,
      onProgress: value => window.progress.push(value),
    }).then(session => { window.session = session; window.sessionState = 'ready'; return { state: 'ready', fileName: session.fileName }; }, error => { window.sessionState = 'error'; return { state: 'error', message: error.message }; });
    return { nativeWebRTC: Boolean(globalThis.RTCPeerConnection || globalThis.mozRTCPeerConnection || globalThis.webkitRTCPeerConnection) };
  }, variant);
}
async function result(page) { return page.evaluate(() => window.sessionPromise); }
async function assertStream(page, fixture, testInfo) {
  const capability = await start(page); const observed = await result(page);
  if (!capability.nativeWebRTC) {
    expect(observed).toMatchObject({ state: 'error' });
    expect(observed.message).toMatch(/no admite conexiones WebRTC/);
    await testInfo.attach('torrent-audio-evidence', { body: JSON.stringify({ status: 'NOT_APPLICABLE_NATIVE_WEBRTC_UNAVAILABLE', observed }), contentType: 'application/json' });
    return false;
  }
  expect(observed).toEqual({ state: 'ready', fileName: 'synthetic.wav' });
  const data = await page.evaluate(async () => {
    const audio = document.getElementById('audio');
    const response = await fetch(audio.src, { signal: AbortSignal.timeout(10_000) });
    const bytes = Array.from(new Uint8Array(await response.arrayBuffer()));
    const registration = await navigator.serviceWorker.getRegistration('/');
    const range = await fetch(audio.src, { headers: { Range: 'bytes=0-43' }, signal: AbortSignal.timeout(10_000) });
    return { status: response.status, contentType: response.headers.get('content-type'), bytes, rangeStatus: range.status, rangeHeader: range.headers.get('content-range'), range: Array.from(new Uint8Array(await range.arrayBuffer())), controlledByRegistration: navigator.serviceWorker.controller === registration.active, scope: new URL(registration.scope).pathname };
  });
  expect(data).toMatchObject({ status: 200, contentType: 'audio/wav', rangeStatus: 206, rangeHeader: `bytes 0-43/${wav.length}`, controlledByRegistration: true, scope: '/' });
  expect(data.bytes).toEqual(Array.from(wav)); expect(data.range).toEqual(Array.from(wav.subarray(0, 44)));
  expect(fixture.requests.some(r => r.pathname === '/webseed/synthetic.wav')).toBe(true);
  expect(fixture.external).toEqual([]); expect(fixture.sockets).toEqual([]);
  await testInfo.attach('torrent-audio-evidence', { body: JSON.stringify({ status: 'TESTED', wholeBytes: data.bytes.length, rangeBytes: data.range.length, controlledByRegistration: data.controlledByRegistration, loopbackWebseedRequests: fixture.requests.filter(r => r.pathname === '/webseed/synthetic.wav'), externalHttpAttempts: fixture.external, webSocketAttempts: fixture.sockets }), contentType: 'application/json' });
  return true;
}

test('actual loader streams synthetic audio and supports media metadata and playback', async ({ page, fixture }, testInfo) => {
  if (!await assertStream(page, fixture, testInfo)) return;
  await expect.poll(() => page.evaluate(() => document.getElementById('audio').duration)).toBe(1);
  await page.locator('#play').click();
  await expect.poll(() => page.evaluate(() => document.getElementById('audio').currentTime)).toBeGreaterThan(0.05);
  await page.evaluate(() => { window.session.destroy(); window.session.destroy(); });
});

test('legacy narrow registration is preserved while the ordinary app page gets root control', async ({ page, fixture }, testInfo) => {
  await page.evaluate(async () => {
    const reg = await navigator.serviceWorker.register('/webtorrent-sw.min.js', { scope: '/torrent-stream/' });
    if (reg.active?.state !== 'activated') await new Promise(resolve => { const worker = reg.installing ?? reg.waiting; const check = () => { if (worker.state === 'activated') { worker.removeEventListener('statechange', check); resolve(); } }; worker.addEventListener('statechange', check); check(); });
  });
  const supported = await assertStream(page, fixture, testInfo);
  expect(await page.evaluate(async () => (await navigator.serviceWorker.getRegistrations()).map(r => new URL(r.scope).pathname).sort())).toEqual(supported ? ['/', '/torrent-stream/'] : ['/torrent-stream/']);
});

test('unrelated requests with stream URLs in their query pass through the root worker', async ({ page, fixture }, testInfo) => {
  if (!await assertStream(page, fixture, testInfo)) return;
  const observed = await page.evaluate(async () => {
    try { const response = await fetch('/unrelated?next=' + location.origin + '/webtorrent/cancel/', { signal: AbortSignal.timeout(5_000) }); return { text: await response.text() }; }
    catch (error) { return { error: error.name }; }
  });
  expect(observed).toEqual({ text: 'unrelated-response-sentinel' });
});

test('foreign root registration remains unchanged when playback is refused', async ({ page, fixture }) => {
  await page.evaluate(async () => {
    const reg = await navigator.serviceWorker.register('/foreign-sw.js', { scope: '/' });
    if (reg.active?.state !== 'activated') await new Promise(resolve => { const worker = reg.installing ?? reg.waiting; const check = () => { if (worker.state === 'activated') { worker.removeEventListener('statechange', check); resolve(); } }; worker.addEventListener('statechange', check); check(); });
    window.foreignRegistration = reg;
  });
  const capability = await start(page);
  const observed = await result(page);
  expect(observed.state).toBe('error'); expect(observed.message).toMatch(capability.nativeWebRTC ? /servicio actual/ : /no admite conexiones WebRTC/);
  expect(await page.evaluate(async () => {
    const current = await navigator.serviceWorker.getRegistration('/');
    return { sameActive: current.active === window.foreignRegistration.active, scriptPath: new URL(current.active.scriptURL).pathname, audioSrc: document.getElementById('audio').getAttribute('src') };
  })).toEqual({ sameActive: true, scriptPath: '/foreign-sw.js', audioSrc: null });
  expect(fixture.requests.some(r => r.pathname === '/webtorrent-sw.min.js')).toBe(false);
});

test('owned root registration is reused across consecutive real loader sessions', async ({ page, fixture }, testInfo) => {
  if (!await assertStream(page, fixture, testInfo)) return;
  const before = await page.evaluate(async () => { window.session.destroy(); window.ownedRegistration = await navigator.serviceWorker.getRegistration('/'); return navigator.serviceWorker.controller === window.ownedRegistration.active; });
  expect(before).toBe(true);
  await assertStream(page, fixture, testInfo);
  expect(await page.evaluate(async () => navigator.serviceWorker.controller === window.ownedRegistration.active && (await navigator.serviceWorker.getRegistrations()).length === 1)).toBe(true);
});

test.describe('activation before control', () => {
  test.use({ delayedClaim: true });
  test('actual loader stays pending until the activated worker claims the page', async ({ page, fixture }, testInfo) => {
    const capability = await start(page);
    if (!capability.nativeWebRTC) {
      const unsupported = await result(page); expect(unsupported.message).toMatch(/no admite conexiones WebRTC/);
      expect(await page.evaluate(() => navigator.serviceWorker.getRegistration('/'))).toBeUndefined();
      await testInfo.attach('controller-handoff', { body: JSON.stringify({ status: 'NOT_APPLICABLE_NATIVE_WEBRTC_UNAVAILABLE', unsupported }), contentType: 'application/json' });
      return;
    }
    await expect.poll(() => page.evaluate(async () => (await navigator.serviceWorker.getRegistration('/'))?.active?.state)).toBe('activated');
    expect(await page.evaluate(() => ({ controlled: Boolean(navigator.serviceWorker.controller), state: window.sessionState, src: document.getElementById('audio').getAttribute('src') }))).toEqual({ controlled: false, state: 'pending', src: null });
    expect(fixture.requests.some(r => r.pathname === '/webseed/synthetic.wav')).toBe(false);
    await page.evaluate(async () => (await navigator.serviceWorker.getRegistration('/')).active.postMessage('test-claim'));
    const observed = await result(page);
    expect(observed.state).toBe(capability.nativeWebRTC ? 'ready' : 'error');
    if (!capability.nativeWebRTC) expect(observed.message).toMatch(/no admite conexiones WebRTC/);
    expect(await page.evaluate(async () => navigator.serviceWorker.controller === (await navigator.serviceWorker.getRegistration('/')).active)).toBe(true);
    await testInfo.attach('controller-handoff', { body: JSON.stringify({ nativeWebRTC: capability.nativeWebRTC, observed, pendingUntilClaim: true }), contentType: 'application/json' });
  });
});


test('simultaneous app tabs stream their own torrent instead of another tab’s 404', async ({ page, context, fixture }, testInfo) => {
  if (!await assertStream(page, fixture, testInfo)) return;
  const ownURL = await page.evaluate(() => document.getElementById('audio').src);
  const other = await context.newPage();
  try {
    await other.goto(fixture.origin + '/browser-test/torrent-audio.html');
    await other.waitForFunction(() => Boolean(window.tdfTorrentAudio));
    const diagnostic = await other.evaluate(async () => { const reg = await navigator.serviceWorker.getRegistration('/'); return { href: location.href, controller: navigator.serviceWorker.controller?.scriptURL, state: navigator.serviceWorker.controller?.state, active: reg?.active?.scriptURL, activeState: reg?.active?.state, same: navigator.serviceWorker.controller === reg?.active, scopes: (await navigator.serviceWorker.getRegistrations()).map(r => r.scope) }; });
    await testInfo.attach('second-document-control', { body: JSON.stringify(diagnostic), contentType: 'application/json' });
    await start(other, 'b'); const observed = await result(other);
    await testInfo.attach('second-document-after', { body: JSON.stringify(await other.evaluate(async () => { const r = await navigator.serviceWorker.getRegistration('/'); return { result: window.sessionState, ctrl: navigator.serviceWorker.controller?.scriptURL, active: r?.active?.scriptURL, same: navigator.serviceWorker.controller === r?.active, state: r?.active?.state }; })), contentType: 'application/json' });
    expect(observed).toEqual({ state: 'ready', fileName: 'synthetic-b.wav' });
    await other.bringToFront();
    const read = async (target, source) => target.evaluate(async (source) => {
      const response = await fetch(source || document.getElementById('audio').src, { signal: AbortSignal.timeout(10_000) });
      return { status: response.status, type: response.headers.get('content-type'), bytes: Array.from(new Uint8Array(await response.arrayBuffer())) };
    }, source);
    const first = await read(page, ownURL); const second = await read(other);
    await testInfo.attach('two-document-status', { body: JSON.stringify({ firstStatus: first.status, firstType: first.type, firstBytes: first.bytes.length, secondStatus: second.status, secondType: second.type, secondBytes: second.bytes.length }), contentType: 'application/json' });
    expect(first.status).toBe(200); expect(second.status).toBe(200);
    expect(first.type).toBe('audio/wav'); expect(second.type).toBe('audio/wav');
    expect(first.bytes).toEqual(Array.from(wav)); expect(second.bytes).toEqual(Array.from(wav));
    await testInfo.attach('two-document-streams', { body: JSON.stringify({ firstStatus: first.status, firstBytes: first.bytes.length, secondStatus: second.status, secondBytes: second.bytes.length, differentTorrentNames: true }), contentType: 'application/json' });
  } finally {
    await other.evaluate(() => window.session?.destroy()).catch(() => {});
    await other.close();
  }
});
