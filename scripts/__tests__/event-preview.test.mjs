import assert from 'node:assert/strict';
import test from 'node:test';
import { injectEventPreview, isSafeEventId, renderEventMetadata, safeAbsoluteImage } from '../../functions/_shared/event-preview.mjs';
import { onRequest } from '../../functions/eventos/[eventId].js';

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
  assert.match(html, /"@type":"MusicEvent"/);
  assert.match(html, /rel="canonical" href="https:\/\/tdf-app\.pages\.dev\/eventos\/42"/);
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
    assert.match(html, /rel="canonical" href="https:\/\/preview\.example\.test\/eventos\/42"/);
    assert.doesNotMatch(html, /<script>alert\(1\)<\/script>/);
  } finally {
    globalThis.fetch = originalFetch;
  }
});
