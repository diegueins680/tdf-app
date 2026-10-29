import { expect, test } from '@playwright/test';
import { gunzipSync } from 'node:zlib';

const sentinel = 'TDF-SYNTHETIC-PRIVATE-RESET';

function decode(request) {
  let body = request.postDataBuffer();
  if (!body?.length) return null;
  if (body[0] === 31 && body[1] === 139) body = gunzipSync(body);
  const text = body.toString();
  try { return JSON.parse(text); } catch {
    const data = new URLSearchParams(text).get('data');
    if (!data) throw new Error('Unknown analytics request encoding');
    return JSON.parse(Buffer.from(data, 'base64').toString());
  }
}

function events(payload) {
  if (Array.isArray(payload)) return payload.flatMap(events);
  if (!payload || typeof payload !== 'object') return [];
  return [...(payload.event ? [payload.event] : []), ...events(payload.batch)];
}

for (const identified of [false, true]) {
  test(`analytics excludes reset credentials before and after ${identified ? 'identification' : 'anonymous startup'} @critical @mobile-flow`, async ({ page, baseURL }) => {
    const base = new URL(baseURL).origin;
    const context = page.context();
    const packets = [], requests = [], decodeErrors = [];
    let releaseChunk;
    const chunkGate = new Promise(resolve => { releaseChunk = resolve; });
    // Delayed route loading leaves the sensitive URL present during SDK startup.
    const deadline = setTimeout(() => releaseChunk(), 8000);
    await context.addInitScript(() => {
      window.__ENV__ = { VITE_POSTHOG_KEY: 'phc_synthetic_privacy_test', VITE_POSTHOG_HOST: 'https://analytics.invalid' };
      // Exercise ordinary visitor capture: PostHog otherwise discards automated
      // browser events, which can make a privacy regression falsely pass.
      Object.defineProperty(navigator, 'webdriver', { get: () => false });
      Object.defineProperty(navigator, 'userAgent', { get: () => navigator.userAgentData?.mobile
        ? 'Mozilla/5.0 (Linux; Android 13) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Mobile Safari/537.36'
        : 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36' });
      Object.defineProperty(navigator, 'userAgentData', { get: () => ({ brands: [{ brand: 'Chromium', version: '130' }], mobile: false, platform: 'macOS' }) });
    });
    await context.route('**/*', async route => {
      const request = route.request(), url = new URL(request.url());
      const headers = { 'Access-Control-Allow-Origin': base, 'Access-Control-Allow-Credentials': 'true', 'Access-Control-Allow-Headers': '*' };
      if (url.hostname === 'analytics.invalid' || url.hostname.endsWith('.posthog.com')) {
        if (request.method() === 'OPTIONS') return route.fulfill({ status: 204, headers });
        requests.push(url.pathname);
        let packet;
        try { packet = decode(request); } catch { decodeErrors.push(url.pathname); }
        if (packet) {
          packets.push(packet);
          if (events(packet).includes(identified ? '$identify' : '$pageview')) releaseChunk();
        }
        if (url.pathname.endsWith('/config.js')) {
          const key = url.pathname.split('/')[2];
          return route.fulfill({ headers, contentType: 'application/javascript', body: `window._POSTHOG_REMOTE_CONFIG = {${JSON.stringify(key)}:{config:{hasFeatureFlags:true}}};` });
        }
        return route.fulfill({ headers, json: { status: 1, featureFlags: {}, featureFlagPayloads: {}, hasFeatureFlags: true } });
      }
      if (['fetch', 'xhr'].includes(request.resourceType()) && !url.pathname.startsWith('/assets/')) {
        if (url.pathname === '/session') return route.fulfill({ status: identified ? 200 : 401, json: identified ? { username: 'synthetic-fan', displayName: 'Synthetic Fan', partyId: 938421, roles: ['Fan'], modules: [], featureFlags: [] } : null });
        if (url.pathname === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
        return route.fulfill({ status: 404, json: null });
      }
      if (url.origin === base && request.method() === 'GET') {
        if (/\/ResetPasswordPage-[^/]+\.js$/.test(url.pathname)) await chunkGate;
        return route.continue();
      }
      return route.abort();
    });
    try {
      await page.goto(`/reset?token=${sentinel}&lang=es&utm_source=privacy-test`);
      await expect.poll(() => packets.flatMap(events)).toContain('$pageview');
      if (identified) await expect.poll(() => packets.flatMap(events)).toContain('$identify');
      await expect(page.getByLabel('Nueva contraseña', { exact: true })).toBeVisible();
      expect(new URL(page.url()).searchParams.has('token')).toBe(false);
      expect(decodeErrors).toEqual([]);
      expect(JSON.stringify(packets)).not.toContain(sentinel);
      // No /flags or remote-config request may bypass before_send. TDF's own
      // featureFlags come from the authoritative session API, not PostHog.
      expect(requests.filter(path => !['/e/', '/i/v0/e/'].includes(path))).toEqual([]);
      expect(JSON.stringify(packets)).toContain('privacy-test');
    } finally {
      clearTimeout(deadline);
      releaseChunk();
    }
  });
}
