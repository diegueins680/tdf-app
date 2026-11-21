import { test, expect } from '@playwright/test';

const profile = { username: 'synthetic', displayName: 'Artista de prueba', partyId: 10, roles: ['Artist'], modules: ['Packages'], apiToken: 'synthetic-app-token' };
const png = Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+a53sAAAAASUVORK5CYII=', 'base64');
const connectionHandle = 'tdf-vault-v1_00000000-0000-4000-8000-000000000010';
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
    if (url.pathname === '/drive/token') return route.fulfill({ json: { accessToken: connectionHandle, refreshToken: connectionHandle, expiresIn: 3600, tokenType: 'TDFConnection' } });
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
  await page.locator('input[data-testid=storage-upload-select]').setInputFiles({ name: 'cover.png', mimeType: 'image/png', buffer: png });
  await expect(page.getByText(/almacenamiento de TDF no está disponible/)).toBeVisible();
  await page.getByRole('button', { name: /Reintentar cover.png/ }).click();
  await expect(page.getByRole('status').filter({ hasText: /^Archivo listo$/ })).toHaveText('Archivo listo');
  await expect(page.getByLabel('Biografía')).toHaveValue('Mi biografía permanece durante la carga.');
  const key = (body) => body.match(/name="idempotencyKey"\r\n\r\n([a-f0-9]{64})/)?.[1];
  expect(uploads).toHaveLength(2); expect(key(uploads[0])).toMatch(/^[a-f0-9]{64}$/); expect(key(uploads[1])).toBe(key(uploads[0]));
});
test('connects personal Drive in a popup without replacing the page or using TDF credentials', async ({ page }) => {
  await page.goto('/e2e/storage-upload.html');
  await page.getByLabel('Biografía').fill('Mi biografía y archivo sobreviven a OAuth.');
  await choose(page, 'Mi Google Drive · no consume espacio TDF');
  await page.locator('input[data-testid=storage-upload-select]').setInputFiles({ name: 'cover.png', mimeType: 'image/png', buffer: png });
  await expect(page.getByRole('button', { name: /Reintentar cover.png/ })).toBeVisible();
  expect(uploads).toHaveLength(0);
  const popupPromise = page.waitForEvent('popup');
  await page.getByRole('button', { name: 'Conectar Drive', exact: true }).click();
  const popup = await popupPromise;
  await popup.waitForEvent('close');
  await expect(page.getByLabel('Biografía')).toHaveValue('Mi biografía y archivo sobreviven a OAuth.');
  await page.getByRole('button', { name: /Reintentar cover.png/ }).click();
  await expect(page.getByRole('status').filter({ hasText: /^Archivo listo$/ })).toHaveText('Archivo listo');
  expect(uploads).toHaveLength(1); expect(uploads[0]).toContain('google_drive');
});

test('recovers the form after reload and reselects only the original file with the same request identity', async ({ page }) => {
  await page.goto('/e2e/storage-upload.html');
  await page.getByLabel('Biografía').fill('Este borrador sobrevive a una recarga.');
  await choose(page, 'TDF · hasta 5 GB gratis');
  await page.locator('input[data-testid=storage-upload-select]').setInputFiles({ name: 'cover.png', mimeType: 'image/png', buffer: png });
  await expect(page.getByRole('button', { name: /Reintentar cover.png/ })).toBeVisible();
  const firstKey = uploads[0].match(/name="idempotencyKey"\r\n\r\n([a-f0-9]{64})/)?.[1];
  await page.reload();
  await expect(page.getByLabel('Biografía')).toHaveValue('Este borrador sobrevive a una recarga.');
  const fileChooser = page.waitForEvent('filechooser');
  await page.getByRole('button', { name: 'Reseleccionar archivo', exact: true }).click();
  await (await fileChooser).setFiles({ name: 'cover.png', mimeType: 'image/png', buffer: png });
  await expect(page.getByRole('status').filter({ hasText: /^Archivo listo$/ })).toHaveText('Archivo listo');
  expect(uploads).toHaveLength(2);
  expect(uploads[1].match(/name="idempotencyKey"\r\n\r\n([a-f0-9]{64})/)?.[1]).toBe(firstKey);
});


test('waits for popup session bootstrap before consuming OAuth state or exchanging the code', async ({ page, context }) => {
  let releaseBootstrap;
  const bootstrapGate = new Promise(resolve => { releaseBootstrap = resolve; });
  let announceBootstrap;
  const bootstrapStarted = new Promise(resolve => { announceBootstrap = resolve; });
  let tokenRequests = 0;
  await context.route('**/session', async route => {
    if (await route.request().frame().page().opener()) { announceBootstrap(); await bootstrapGate; }
    return route.fulfill({ json: profile });
  });
  await context.route('**/drive/token', async route => {
    tokenRequests += 1;
    return route.fulfill({ json: { accessToken: 'synthetic-provider-token', refreshToken: 'synthetic-refresh', expiresIn: 3600 } });
  });
  try {
    await page.goto('/e2e/storage-upload.html');
    await page.getByLabel('Biografía').fill('Bootstrap preserves my file and biography.');
    await choose(page, 'Mi Google Drive · no consume espacio TDF');
    await page.locator('input[data-testid=storage-upload-select]').setInputFiles({ name: 'cover.png', mimeType: 'image/png', buffer: png });
    await expect(page.getByRole('button', { name: /Reintentar cover.png/ })).toBeVisible();
    const popupPromise = page.waitForEvent('popup');
    await page.getByRole('button', { name: 'Conectar Drive', exact: true }).click();
    const popup = await popupPromise;
    await bootstrapStarted;
    await expect(popup.getByText('Conectando con Google Drive...')).toBeVisible();
    expect(await popup.evaluate(() => localStorage.getItem('tdf-google-drive-state') === new URLSearchParams(location.search).get('state'))).toBe(true);
    expect(tokenRequests).toBe(0);
    const closed = popup.waitForEvent('close');
    releaseBootstrap();
    await closed;
    expect(tokenRequests).toBe(1);
    await expect(page.getByLabel('Biografía')).toHaveValue('Bootstrap preserves my file and biography.');
    await page.getByRole('button', { name: /Reintentar cover.png/ }).click();
    await expect(page.getByRole('status').filter({ hasText: /^Archivo listo$/ })).toHaveText('Archivo listo');
    expect(uploads).toHaveLength(1); expect(uploads[0]).toContain('google_drive');
  } finally { releaseBootstrap(); }
});
