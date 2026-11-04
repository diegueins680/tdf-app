import { test, expect } from '@playwright/test';

const profile = { username: 'synthetic', displayName: 'Artista de prueba', partyId: 10, roles: ['Artist'], modules: ['Packages'], apiToken: 'synthetic-app-token' };
const png = Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+a53sAAAAASUVORK5CYII=', 'base64');
const uploads = [];
test.beforeEach(async ({ context }) => {
  uploads.length = 0;
  await context.addInitScript((session) => {
    if (location.hostname === '127.0.0.1') localStorage.setItem('tdf-hq-ui/session', JSON.stringify(session));
  }, profile);
  await context.route('**/*', async (route) => {
    const request = route.request(), url = new URL(request.url());
    if (url.hostname === 'accounts.google.com') {
      const callback = new URL(url.searchParams.get('redirect_uri'));
      callback.searchParams.set('code', 'synthetic-oauth-code'); callback.searchParams.set('state', url.searchParams.get('state'));
      return route.fulfill({ contentType: 'text/html', body: `<script>location.replace(${JSON.stringify(callback.toString())})</script>` });
    }
    if (url.hostname !== '127.0.0.1') return route.abort();
    if (url.pathname === '/session') return route.fulfill({ json: profile });
    if (url.pathname === '/storage/me') return route.fulfill({ json: { allowanceBytes: 5_000_000_000, usedBytes: 0, reservedBytes: 0, revision: 0, defaultDestination: null, legacyUsageVerified: true } });
    if (url.pathname === '/drive/token') return route.fulfill({ json: { accessToken: 'synthetic-provider-token', refreshToken: 'synthetic-refresh', expiresIn: 3600 } });
    if (url.pathname === '/drive/upload') {
      uploads.push(request.postDataBuffer().toString('utf8'));
      if (uploads.length === 1 && !uploads[0].includes('google_drive')) return route.fulfill({ status: 503, json: { code: 'storage_service_unavailable' } });
      return route.fulfill({ json: { duFileId: 'synthetic', duWebViewLink: null, duWebContentLink: null, duPublicUrl: 'https://assets.example.test/cover.png' } });
    }
    return route.continue();
  });
});

async function choose(page, label) {
  await page.getByRole('combobox', { name: 'Dónde guardar' }).click();
  await page.getByRole('option', { name: label }).click();
}
test('retries a TDF outage with the same file identity and keeps the biography', async ({ page }) => {
  await page.goto('/e2e/storage-upload.html');
  await page.getByLabel('Biografía').fill('Mi biografía permanece durante la carga.');
  await choose(page, 'TDF · hasta 5 GB gratis');
  await page.locator('input[type=file]').setInputFiles({ name: 'cover.png', mimeType: 'image/png', buffer: png });
  await expect(page.getByText(/almacenamiento de TDF no está disponible/)).toBeVisible();
  await page.getByRole('button', { name: /Reintentar cover.png/ }).click();
  await expect(page.getByRole('status')).toHaveText('Archivo listo');
  await expect(page.getByLabel('Biografía')).toHaveValue('Mi biografía permanece durante la carga.');
  const key = (body) => body.match(/name="idempotencyKey"\r\n\r\n([a-f0-9]{64})/)?.[1];
  expect(uploads).toHaveLength(2); expect(key(uploads[0])).toMatch(/^[a-f0-9]{64}$/); expect(key(uploads[1])).toBe(key(uploads[0]));
});
test('connects personal Drive in a popup without replacing the page or using TDF credentials', async ({ page }) => {
  await page.goto('/e2e/storage-upload.html');
  await page.getByLabel('Biografía').fill('Mi biografía y archivo sobreviven a OAuth.');
  await choose(page, 'Mi Google Drive · no consume espacio TDF');
  await page.locator('input[type=file]').setInputFiles({ name: 'cover.png', mimeType: 'image/png', buffer: png });
  await expect(page.getByRole('button', { name: /Reintentar cover.png/ })).toBeVisible();
  expect(uploads).toHaveLength(0);
  const popupPromise = page.waitForEvent('popup');
  await page.getByRole('button', { name: 'Conectar Drive', exact: true }).click();
  const popup = await popupPromise;
  await popup.waitForEvent('close');
  await expect(page.getByLabel('Biografía')).toHaveValue('Mi biografía y archivo sobreviven a OAuth.');
  await page.getByRole('button', { name: /Reintentar cover.png/ }).click();
  await expect(page.getByRole('status')).toHaveText('Archivo listo');
  expect(uploads).toHaveLength(1); expect(uploads[0]).toContain('google_drive');
});
