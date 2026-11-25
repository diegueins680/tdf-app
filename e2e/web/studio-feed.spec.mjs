import { test, expect } from '@playwright/test';

test('el editor confirma seis fotos en la ficha del estudio', async ({ page }) => {
  const profile = '10000000-0000-4000-8000-000000000001';
  const slug = 'tdf-records-estudio';
  const session = { username: 'synthetic', displayName: 'Editor de prueba', partyId: 10, roles: ['Artist'], modules: [], apiToken: 'fixture' };
  await page.addInitScript(s => localStorage.setItem('tdf-hq-ui/session', JSON.stringify(s)), session);
  const files = [], feed = [];
  const png = Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+a53sAAAAASUVORK5CYII=', 'base64');
  await page.route('**/*', async route => {
    const req = route.request(), url = new URL(req.url()), p = url.pathname;
    if (!['localhost','127.0.0.1'].includes(url.hostname)) return route.abort();
    if (p === '/session') return route.fulfill({ json: session });
    if (p === '/directory/profiles') return route.fulfill({ json: [{ id: profile, capabilities: { edit: true, publish: true } }] });
    if (p === '/directory/profiles/' + slug) return route.fulfill({ json: { id: profile, name: 'TDF Estudio', slug, kind: 'studio', feed } });
    if (p === '/drive/upload') {
      const body = req.postDataBuffer().toString('latin1');
      expect(body).toContain('private');
      files.push(body.match(/filename="([^"]+)"/)[1]);
      return route.fulfill({ json: { duFileId: 'fixture-' + files.length } });
    }
    if (p.includes('/feed/') && req.method() === 'PUT') {
      feed.push({ ...req.postDataJSON(), id: p.split('/').pop() });
      return route.fulfill({ status: 204 });
    }
    if (p.includes('/feed/')) return route.fulfill({ contentType: 'application/json', body: JSON.stringify('data:image/png;base64,' + png.toString('base64')) });
    if (p.includes('/reviews')) return route.fulfill({ json: { items: [], summary: {}, nextCursor: null } });
    return route.continue();
  });
  await page.goto('/directorio/' + slug);
  await page.getByLabel('Texto de la publicación').fill('Ensayo de seis fotos');
  await page.getByLabel('Fotos en orden').setInputFiles(Array.from({ length: 6 }, (_, i) => ({ name: 'foto-' + (i+1) + '.png', mimeType: 'image/png', buffer: png })));
  await page.getByRole('button', { name: 'Mover antes' }).first().click();
  await page.getByRole('button', { name: 'Vista previa' }).click();
  expect(files).toHaveLength(0);
  await page.getByRole('button', { name: 'Confirmar publicación' }).click();
  await expect(page.getByRole('article').getByRole('img')).toHaveCount(6);
  expect(files).toEqual([2,1,3,4,5,6].map(i => 'foto-' + i + '.png'));
});
