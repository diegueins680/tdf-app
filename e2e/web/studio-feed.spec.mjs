import { readFileSync } from 'node:fs';
import { test, expect } from '@playwright/test';

test('el editor confirma seis fotos en la ficha del estudio', async ({ page }) => {
  const profile = '10000000-0000-4000-8000-000000000001';
  const slug = 'tdf-records-estudio';
  const session = { username: 'synthetic', displayName: 'Editor de prueba', partyId: 10, roles: ['Artist'], modules: [], apiToken: 'fixture' };
  await page.addInitScript(s => localStorage.setItem('tdf-hq-ui/session', JSON.stringify(s)), session);
  const files = [], feed = [];
  const photos = Array.from({ length: 6 }, (_, i) => readFileSync(new URL('./fixtures/studio-feed/' + (i + 1) + '.png', import.meta.url)));
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
    if (p.includes('/feed/') && req.method() === 'PATCH') {
      const edit = req.postDataJSON();
      if (edit.published) feed[0].caption = edit.caption;
      else feed.length = 0;
      return route.fulfill({ status: 204 });
    }
    if (p.includes('/feed/')) return route.fulfill({ contentType: 'application/json', body: JSON.stringify('data:image/png;base64,' + photos[Number(files[Number(p.split('-').pop()) - 1].match(/\d+/)[0]) - 1].toString('base64')) });
    if (p.includes('/reviews')) return route.fulfill({ json: { items: [], summary: {}, nextCursor: null } });
    if (['fetch', 'xhr'].includes(req.resourceType()) && !p.startsWith('/@') && !p.startsWith('/src/') && !p.includes('node_modules')) return route.fulfill({ status: 404, json: {} });
    return route.continue();
  });
  await page.goto('/directorio/' + slug);
  await page.getByLabel('Texto de la publicación').fill('Ensayo de seis fotos');
  await page.getByLabel('Fotos en orden').setInputFiles(Array.from({ length: 6 }, (_, i) => ({ name: 'foto-' + (i+1) + '.png', mimeType: 'image/png', buffer: photos[i] })));
  await page.getByRole('button', { name: 'Mover antes' }).first().click();
  await page.getByRole('button', { name: 'Vista previa' }).click();
  expect(files).toHaveLength(0);
  await page.getByRole('button', { name: 'Confirmar publicación' }).click();
  await expect(page.getByRole('article').getByRole('img')).toHaveCount(6);
  expect(files).toEqual([2,1,3,4,5,6].map(i => 'foto-' + i + '.png'));
  await page.getByRole('button', { name: 'Editar publicación' }).click();
  await page.getByLabel('Editar texto').fill('Texto editado');
  await page.getByRole('button', { name: 'Guardar texto' }).click();
  await expect(page.getByText('Texto editado', { exact: true })).toBeVisible();
  await page.getByRole('button', { name: 'Retirar publicación' }).click();
  await expect(page.getByText('Aún no hay publicaciones.')).toBeVisible();
});
