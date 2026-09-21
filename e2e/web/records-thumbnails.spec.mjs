import { readFileSync } from 'node:fs';
import { expect, test } from '@playwright/test';

// Synthetic content exists only inside this browser test; no provider/account writes.
const validImage = readFileSync(new URL('./fixtures/thumbnail-valid.png', import.meta.url));
const placeholderImage = readFileSync(new URL('./fixtures/thumbnail-placeholder.png', import.meta.url));
async function fixture(page, baseURL, verified) {
  const requests = [];
  const videos = [
    ['ooPsIHsikYU', 'Llama Este Pez @ Sereno Moreno Live Set Pt 1'],
    ['Cb7VGZJ6apo', 'Llama Este Pez @ Sereno Moreno Live Set Pt 2'],
    ['f2BabxM1Pjc', 'Federico Molinari @ TDF Electro Sessions'],
  ];
  const recordings = videos.map(([id, title], index) => ({
    id, code: `test-${id}`, title, contributors: [], sortOrder: index, revision: 1,
    resources: [{ id, providerCode: 'youtube', kind: 'video', externalCode: id,
      url: `https://www.youtube.com/watch?v=${id}`, thumbnailUrl: `https://i.ytimg.com/vi/${id}/hqdefault.jpg`,
      primary: true, relationKind: 'primary-media', sortOrder: 0,
      ...(verified && index < 2 ? { availability: 'unavailable', availabilityReason: 'removed_by_uploader' } : {}),
    }],
  }));
  await page.route('**/*', async route => {
    const request = route.request(); const url = new URL(request.url());
    if (url.hostname === 'i.ytimg.com') {
      requests.push(url.pathname);
      if (url.pathname.includes('f2BabxM1Pjc')) return route.fulfill({ contentType: 'image/png', body: validImage });
      if (url.pathname.includes('ooPsIHsikYU') && url.pathname.endsWith('hqdefault.jpg')) {
        return route.fulfill({ contentType: 'image/png', body: placeholderImage });
      }
      return route.fulfill({ status: 404, contentType: 'image/png', body: placeholderImage });
    }
    if (url.pathname === '/records/feed') return route.fulfill({ json: { locale: 'es', revision: 1, collections: [], releases: [], sessions: [], recordings } });
    if (['fetch', 'xhr'].includes(request.resourceType())) {
      if (url.pathname === '/session') return route.fulfill({ status: 401, json: {} });
      if (url.pathname.startsWith('/catalog')) return route.fulfill({ json: { catalogs: [], items: [], defaults: [] } });
      return route.fulfill({ json: [] });
    }
    return url.origin === new URL(baseURL).origin ? route.continue() : route.abort();
  });
  return requests;
}

test('Records handles failed images and decoded placeholders with bounded requests @critical', async ({ page, baseURL }) => {
  const requests = await fixture(page, baseURL, false);
  await page.goto('/records#releases');
  // WebKit defers offscreen lazy images: exercise compact previews before cards.
  await page.getByText('Videos recientes', { exact: true }).scrollIntoViewIfNeeded();
  await expect(page.getByRole('heading', { name: 'Videos recientes', exact: true }).locator('../..').getByRole('img', { name: 'Miniatura no disponible' })).toHaveCount(2);
  await page.getByText('Grabaciones recientes', { exact: true }).scrollIntoViewIfNeeded();
  await expect(page.getByRole('img', { name: 'Miniatura no disponible' })).toHaveCount(4);
  const control = page.locator('img[alt="Federico Molinari @ TDF Electro Sessions"]').last();
  await expect(control).toBeVisible();
  await expect.poll(() => control.evaluate(i => i.naturalWidth)).toBe(480);
  expect(requests.every(p => /\/vi\/(ooPsIHsikYU|Cb7VGZJ6apo|f2BabxM1Pjc)\/(hqdefault|mqdefault)\.jpg/.test(p))).toBe(true);
  // Compact + card instances can each request a candidate once; never retry endlessly.
  expect(requests.length).toBeLessThanOrEqual(10);
});

test('Records preserves source identities, keyboard links and responsive unavailable states @critical', async ({ page, baseURL }) => {
  const requests = await fixture(page, baseURL, true);
  await page.goto('/records#releases');
  const link = page.getByRole('link', { name: 'Consultar fuente de Llama Este Pez @ Sereno Moreno Live Set Pt 1' });
  await link.scrollIntoViewIfNeeded();
  await expect(link).toHaveAttribute('href', 'https://www.youtube.com/watch?v=ooPsIHsikYU');
  await link.focus(); await expect(link).toBeFocused();
  await expect(page.getByRole('img', { name: 'Video no disponible en la fuente' })).toHaveCount(4);
  expect(requests.some(p => p.includes('ooPsIHsikYU') || p.includes('Cb7VGZJ6apo'))).toBe(false);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
