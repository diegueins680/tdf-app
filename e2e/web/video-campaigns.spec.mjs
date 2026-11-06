import { expect, test } from '@playwright/test';

// Browser/API fixtures only: no real accounts, storage, provider connections or publication.
test('artist can retry a draft save, reload and review manual publication @mobile-flow', async ({ page, baseURL }, info) => {
  const rows = new Map(); const saves = []; let reject = true;
  const artist = { apArtistId: 42, apDisplayName: 'Artista de prueba', apSlug: 'artista-de-prueba' };
  await page.route('**/*', async (route) => {
    const request = route.request(); const url = new URL(request.url());
    const path = url.pathname.replace(/^\/core-api/, '');
    if (['fetch', 'xhr'].includes(request.resourceType())) {
      if (path === '/session') return route.fulfill({ json: {
        username: 'synthetic-42', partyId: 42, displayName: 'Artista de prueba', roles: ['Artist'], modules: [],
        preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
      } });
      if (path === '/fans/me/artist-profile') return route.fulfill({ json: artist });
      if (path.startsWith('/artists/video-campaigns')) {
        if (request.method() === 'GET' && path.endsWith('/metrics')) return route.fulfill({ json: { registrations: 0, follows: 0, byPiece: [] } });
        if (request.method() === 'GET') return route.fulfill({ json: [...rows.values()] });
        saves.push({ path, method: request.method(), payload: request.postDataJSON() });
        if (request.method() !== 'PUT') return route.fulfill({ status: 403, json: { error: 'Publication forbidden in fixture' } });
        if (reject) { reject = false; return route.fulfill({ status: 400, json: { error: 'No se pudo guardar. Reintenta.' } }); }
        const id = path.split('/').at(-1); const { document, expectedRevision } = request.postDataJSON();
        const row = { id, artistId: 42, revision: expectedRevision + 1, content: document,
          createdAt: '2026-11-05T00:00:00Z', updatedAt: '2026-11-05T00:00:00Z' };
        rows.set(id, row); return route.fulfill({ json: row });
      }
      if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [] } });
      return route.fulfill({ status: 404, json: { error: 'No fixture for this API' } });
    }
    if (url.origin !== new URL(baseURL).origin) return route.abort('blockedbyclient');
    return route.continue();
  });
  await page.goto('/mi-artista/campanas');
  await page.getByRole('button', { name: 'Crear campaña', exact: true }).click();
  await page.getByLabel('Título de la sesión').fill('Sesión de prueba');
  await page.getByRole('button', { name: 'Guardar borrador', exact: true }).click();
  await expect(page.getByRole('alert').filter({ hasText: /Reintenta/ })).toBeVisible();
  await expect(page.getByLabel('Título de la sesión')).toHaveValue('Sesión de prueba');
  await page.getByRole('button', { name: 'Guardar borrador', exact: true }).click();
  await expect(page.getByText(/Borrador guardado/)).toBeVisible();
  expect(saves[0].path).toBe(saves[1].path);
  expect(rows.size).toBe(1);
  await page.reload();
  await page.getByRole('button', { name: 'Continuar', exact: true }).click();
  await expect(page.getByLabel('Título de la sesión')).toHaveValue('Sesión de prueba');
  await page.getByRole('button', { name: 'Siguiente', exact: true }).click();
  await expect(page.getByText(/Publicación manual disponible/)).toBeVisible();
  await page.getByLabel('Fecha propuesta (opcional)').fill('2026-12-01');
  await page.getByRole('button', { name: 'Preparar textos y calendario' }).click();
  await page.getByRole('button', { name: 'Siguiente', exact: true }).click();
  await expect(page.getByLabel('Texto de la publicación').first()).toBeVisible();
  expect(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 1)).toBe(false);
  await page.getByRole('button', { name: 'Siguiente', exact: true }).click();
  await expect(page.getByRole('button', { name: 'Guardar para revisión' })).toBeVisible();
  await expect(page.getByRole('button', { name: 'Publicar video en el perfil de TDF' })).toHaveCount(0);
  expect(saves.every((r) => r.method === 'PUT' && r.payload.document.status === 'draft')).toBe(true);
  await page.screenshot({ path: info.outputPath('launch-review.png'), fullPage: true });
});
