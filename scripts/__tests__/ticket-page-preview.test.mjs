import assert from 'node:assert/strict';
import test from 'node:test';
import { onRequest as checkout } from '../../functions/eventos/[eventId]/entradas.js';
import { onRequest as receipt } from '../../functions/eventos/[eventId]/orden/[orderId].js';

test('checkout and receipt suppress indexing/caching in the initial response', async () => {
  for (const handler of [checkout, receipt]) {
    const result = await handler({
      params: { eventId: '141', orderId: '92' },
      next: async () => new Response('<html><body>App shell</body></html>', {
        headers: { 'Content-Type': 'text/html', 'Cache-Control': 'public, max-age=3600' },
      }),
    });
    assert.equal(result.status, 200);
    assert.equal(result.headers.get('X-Robots-Tag'), 'noindex, follow');
    assert.equal(result.headers.get('Cache-Control'), 'private, no-store');
    assert.equal(result.headers.get('Referrer-Policy'), 'no-referrer');
    assert.equal(result.headers.get('Content-Type'), 'text/html');
    assert.equal(await result.text(), '<html><body>App shell</body></html>');
  }
});

test('invalid event or order paths do not invoke the app shell', async () => {
  for (const params of [{ eventId: '../141' }, { eventId: '141', orderId: 'not-an-order' }]) {
    const result = await receipt({ params, next: () => assert.fail('Invalid path reached asset') });
    assert.equal(result.status, 404);
    assert.equal(result.headers.get('X-Robots-Tag'), 'noindex, follow');
  }
});
