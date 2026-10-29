import assert from 'node:assert/strict';
import test from 'node:test';
import { injectEventPreview, isSafeEventId, renderEventMetadata, safeAbsoluteImage } from '../../functions/_shared/event-preview.mjs';
import { onRequest } from '../../functions/eventos/[eventId].js';
import { canonicalRedirectLocation, onRequest as middleware } from '../../functions/_middleware.js';
import { onRequest as deployedReportRequest } from '../../functions/reportes/eventos/[token].js';
import { independentEnvelope, reportToken, syntheticReport } from './fixtures/event-report.mjs';
import { createEventReportHandler, renderEventReport, reportHeaders, resolveEventReport, tokenDigest } from '../../functions/_shared/event-report.mjs';

const publicEvent = {
  id: '42',
  title: 'Festival <script>alert(1)</script>',
  description: 'Una noche de música & comunidad',
  startTime: '2026-10-03T20:00:00Z',
  endTime: null,
  imageUrl: 'https://cdn.example.test/poster.jpg',
  isPublic: true,
  publicShareEligible: true,
  workflowStateCode: 'announced',
  venue: { name: 'Teatro Nacional' },
  location: { city: 'Quito' },
};

// Test-only capability: the actual report link is never stored in source or logs.
const reportRequest = createEventReportHandler(independentEnvelope());
const reportUrl = `https://www.tdfrecords.net/reportes/eventos/${reportToken}`;
const reportContext = async (overrides = {}) => ({
  params: { token: reportToken },
  request: new Request(reportUrl),
  env: { PATCH_CULTURE_REPORT_TOKEN_SHA256: await tokenDigest(reportToken) },
  next: () => { throw new Error('Report must not load the public SPA or API'); },
  ...overrides,
});

test('the report is readable without login only with the complete capability', async () => {
  const response = await reportRequest(await reportContext());
  assert.equal(response.status, 200);
  for (const [name, value] of Object.entries(reportHeaders)) assert.equal(response.headers.get(name), value);
  const html = await response.text();
  assert.match(html, /Synthetic report fixture/);
  assert.match(html, /120[,.]00/);
  assert.match(html, /24[,.]00/);
  assert.match(html, /96[,.]00/);
  assert.match(html, /USD48/);
  assert.match(html, /Diferencia ficticia pendiente de conciliar/);
  assert.match(html, /Solo lectura/);
  assert.doesNotMatch(html, /<script|https:\/\/chatgpt|posthog|<form|og:|test_capability_|a7fd4151/);
  assert.match(html, /href="\/eventos\/999"/);
});

test('deployed route cannot accept a public synthetic key through a digest override', async () => {
  assert.equal((await deployedReportRequest(await reportContext())).status, 404);
});

test('wrong, partial and named report identifiers return indistinguishable empty 404s', async () => {
  for (const token of [null, undefined, '', '141', 'patch-culture', reportToken.slice(1), `${reportToken}x`, 'z'.repeat(43), `${reportToken.slice(0, -1)}!`, [reportToken]]) {
    const response = await reportRequest(await reportContext({ params: { token } }));
    assert.equal(response.status, 404);
    assert.equal(await response.text(), 'Not found');
    for (const [name, value] of Object.entries(reportHeaders)) assert.equal(response.headers.get(name), value);
  }
});

test('revoking or rotating the capability takes effect on the very next request', async () => {
  const context = await reportContext();
  assert.equal((await reportRequest(context)).status, 200);
  for (const enabled of ['false', '', 'TRUE', '0', false]) {
    const response = await reportRequest({ ...context, env: { ...context.env, EVENT_REPORT_LINKS_ENABLED: enabled } });
    assert.equal(response.status, 404);
    assert.equal(await response.text(), 'Not found');
  }
  const rotated = Buffer.alloc(32, 9).toString('base64url');
  const env = { PATCH_CULTURE_REPORT_TOKEN_SHA256: await tokenDigest(rotated), EVENT_REPORT_LINKS_ENABLED: 'true' };
  assert.equal((await reportRequest({ ...context, env })).status, 404);
  assert.equal((await reportRequest({ ...context, env, params: { token: rotated } })).status, 404);
  const rotatedRequest = createEventReportHandler(independentEnvelope(syntheticReport, rotated));
  assert.equal((await rotatedRequest({ ...context, env, params: { token: rotated } })).status, 200);
  assert.equal((await rotatedRequest(context)).status, 404);
});

test('invalid configuration fails closed and query strings or sessions cannot confer report access', async () => {
  for (const digest of ['', 'garbage', 'A'.repeat(64), null, 123]) {
    const env = { PATCH_CULTURE_REPORT_TOKEN_SHA256: digest };
    assert.equal(await resolveEventReport(reportToken, env), null);
  }
  const response = await reportRequest(await reportContext({
    params: { token: 'x'.repeat(43) },
    request: new Request(`${reportUrl}?token=${reportToken}`, { headers: { Cookie: 'session=admin' } }),
  }));
  assert.equal(response.status, 404);
});

test('HEAD verifies access with no body and write methods are refused', async () => {
  const head = await reportRequest(await reportContext({ request: new Request(reportUrl, { method: 'HEAD' }) }));
  assert.equal(head.status, 200);
  assert.equal(await head.text(), '');
  const missing = await reportRequest(await reportContext({
    params: { token: 'z'.repeat(43) }, request: new Request(reportUrl, { method: 'HEAD' }),
  }));
  assert.equal(missing.status, 404);
  assert.equal(await missing.text(), '');
  for (const method of ['POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS']) {
    const response = await reportRequest(await reportContext({ request: new Request(reportUrl, { method }) }));
    assert.equal(response.status, 405);
    assert.equal(response.headers.get('allow'), 'GET, HEAD');
    assert.doesNotMatch(await response.text(), /PATCH CULTURE|USD/);
  }
});

test('snapshot rendering escapes every editorial value and preserves provisional arithmetic', () => {
  const editorial = Object.fromEntries(['title', 'date', 'venue', 'snapshotDate', 'venueAmountLabel', 'profitExplanation', 'beerCaption', 'beerExplanation', 'reconciliation', 'accounting', 'source'].map(field => [field, `<script>${field}</script>`]));
  const html = renderEventReport({ ...syntheticReport, ...editorial, beer: [{ label: '<img src=x onerror=bad()>', pints: 2, costCents: 2400 }], pending: ['<img src=x onerror=bad()>'] });
  assert.doesNotMatch(html, /<script>|<img src=x/);
  assert.match(html, /&lt;script&gt;/);
  assert.match(html, /&lt;img src=x/);
  assert.equal(syntheticReport.beer.reduce((sum, row) => sum + row.costCents, 0), 2400);
  assert.equal(syntheticReport.beer.reduce((sum, row) => sum + row.pints, 0), 2);
  assert.equal(syntheticReport.reportedGrossCents - syntheticReport.owedToVenueCents, 9600);
  assert.equal(syntheticReport.provisionalShareCents * 2, 9600);
});

test('report middleware keeps legacy redirects private and does not invoke downstream readers for malformed paths', async () => {
  const legacy = reportUrl.replace('www.tdfrecords.net', 'tdf-app.pages.dev');
  const response = await middleware({ request: new Request(legacy), next: () => assert.fail('unexpected reader') });
  assert.equal(response.status, 302);
  assert.equal(response.headers.get('location'), reportUrl);
  assert.equal(response.headers.get('cache-control'), reportHeaders['Cache-Control']);
  assert.equal(response.headers.get('referrer-policy'), 'no-referrer');
  for (const suffix of ['', '/', '/patch-culture', `/${reportToken}/extra`]) {
    const invalid = await middleware({ request: new Request(`https://www.tdfrecords.net/reportes/eventos${suffix}`), next: () => assert.fail('SPA fallback') });
    assert.equal(invalid.status, 404);
    assert.equal(await invalid.text(), 'Not found');
    assert.equal(invalid.headers.get('x-robots-tag'), reportHeaders['X-Robots-Tag']);
  }
  const context = await reportContext();
  const served = await middleware({ ...context, next: () => reportRequest(context) });
  assert.equal(served.status, 200);
  assert.equal(served.headers.get('referrer-policy'), 'no-referrer');
});

test('renders real crawler metadata and JSON-LD without executable event content', () => {
  const preview = renderEventMetadata(publicEvent, '42');
  const html = injectEventPreview('<html><head><title>TDF Records</title></head><body></body></html>', preview);
  assert.match(html, /Festival &lt;script&gt;alert\(1\)&lt;\/script&gt;/);
  assert.match(html, /property="og:image" content="https:\/\/cdn\.example\.test\/poster\.jpg"/);
  assert.match(html, /"@type":"Event"/);
  assert.match(html, /rel="canonical" href="https:\/\/www\.tdfrecords\.net\/eventos\/42"/);
  assert.doesNotMatch(html, /<script>alert\(1\)<\/script>/);
});

test('rejects private previews, malformed ids, and unsafe image protocols', () => {
  assert.equal(isSafeEventId('../42'), false);
  assert.equal(safeAbsoluteImage('javascript:alert(1)'), null);
  assert.equal(safeAbsoluteImage('https://user:password@cdn.example.test/poster.jpg'), null);
  assert.throws(() => renderEventMetadata({ ...publicEvent, isPublic: false }, '42'));
});

test('keeps an explicitly public cancelled event readable but marks its structured status', () => {
  const preview = renderEventMetadata({
    ...publicEvent,
    publicShareEligible: false,
    workflowStateCode: 'cancelled',
  }, '42');
  assert.match(preview.tags, /https:\/\/schema\.org\/EventCancelled/);
});

test('serves real values in the initial HTML response to a crawler request', async () => {
  const originalFetch = globalThis.fetch;
  globalThis.fetch = async () => Response.json(publicEvent);
  try {
    const response = await onRequest({
      params: { eventId: '42' },
      env: {
        PUBLIC_API_BASE: 'https://api.example.test',
      },
      next: async () => new Response('<html><head><title>TDF Records</title></head><body><div id="root"></div></body></html>', {
        headers: { 'Content-Type': 'text/html' },
      }),
      request: new Request('https://preview.example.test/eventos/42?utm_source=tdf_web', {
        headers: { 'User-Agent': 'facebookexternalhit/1.1' },
      }),
    });
    const html = await response.text();
    assert.equal(response.status, 200);
    assert.equal(response.headers.get('cache-control'), 'no-store');
    assert.match(html, /property="og:title" content="Festival &lt;script&gt;alert\(1\)&lt;\/script&gt;"/);
    // Previews always point at the canonical site, whatever host served them.
    assert.match(html, /rel="canonical" href="https:\/\/www\.tdfrecords\.net\/eventos\/42"/);
    assert.doesNotMatch(html, /preview\.example\.test|pages\.dev/);
    assert.doesNotMatch(html, /<script>alert\(1\)<\/script>/);
  } finally {
    globalThis.fetch = originalFetch;
  }
});


test('uses the preview image fallback when no image is published', () => {
  for (const imageUrl of [null, undefined, '', '   ']) {
    assert.equal(safeAbsoluteImage(imageUrl), null);
    const preview = renderEventMetadata({ ...publicEvent, imageUrl }, '42', 'https://www.tdfrecords.net');
    assert.equal(preview.image, 'https://www.tdfrecords.net/tdf-app-icon-1024.png');
  }
});

test('redirects the legacy production Pages host to the canonical site', async () => {
  assert.equal(
    canonicalRedirectLocation('https://tdf-app.pages.dev/eventos/141?utm_source=tdf_mobile&utm_campaign=event_rsvp'),
    'https://www.tdfrecords.net/eventos/141?utm_source=tdf_mobile&utm_campaign=event_rsvp',
  );
  assert.equal(
    canonicalRedirectLocation('https://tdf-app.pages.dev/oauth/google-drive/callback?code=abc&state=xyz'),
    'https://www.tdfrecords.net/oauth/google-drive/callback?code=abc&state=xyz',
  );
  const response = await middleware({ request: new Request('https://tdf-app.pages.dev/'), next: async () => new Response('page') });
  assert.equal(response.status, 301);
  assert.equal(response.headers.get('location'), 'https://www.tdfrecords.net/');
});

test('keeps legacy-host redirects on the canonical site for double-slash paths', async () => {
  for (const [legacy, canonical] of [
    ['https://tdf-app.pages.dev//evil.example/path?x=1', 'https://www.tdfrecords.net//evil.example/path?x=1'],
    ['https://tdf-app.pages.dev/\\evil.example/path', 'https://www.tdfrecords.net//evil.example/path'],
    ['https://tdf-app.pages.dev///evil.example', 'https://www.tdfrecords.net///evil.example'],
  ]) {
    const location = canonicalRedirectLocation(legacy);
    assert.equal(location, canonical, legacy);
    assert.equal(new URL(location).origin, 'https://www.tdfrecords.net', legacy);
  }
  const response = await middleware({ request: new Request('https://tdf-app.pages.dev//evil.example/path'), next: async () => new Response('page') });
  assert.equal(new URL(response.headers.get('location')).hostname, 'www.tdfrecords.net');
});

test('leaves the canonical site, branch previews and app-link files alone', async () => {
  for (const url of [
    'https://www.tdfrecords.net/eventos/141',
    'https://feature-x.tdf-app.pages.dev/eventos/141',
    'https://tdf-app.pages.dev/.well-known/apple-app-site-association',
    'https://tdf-app.pages.dev.evil.example/',
  ]) {
    assert.equal(canonicalRedirectLocation(url), null, url);
  }
  const response = await middleware({ request: new Request('https://www.tdfrecords.net/eventos/141'), next: async () => new Response('page') });
  assert.equal(await response.text(), 'page');
});
