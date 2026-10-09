import assert from 'node:assert/strict';
import test from 'node:test';
import { injectEventPreview, isSafeEventId, renderEventMetadata, safeAbsoluteImage } from '../../functions/_shared/event-preview.mjs';
import { onRequest } from '../../functions/eventos/[eventId].js';
import { canonicalRedirectLocation, onRequest as middleware } from '../../functions/_middleware.js';

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
