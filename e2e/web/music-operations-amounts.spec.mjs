import { expect, test } from '@playwright/test';
import axe from 'axe-core';

async function fixture(page, baseURL) {
  const origin = new URL(baseURL).origin, writes = [], reads = [];
  const version = {
    id: 'version', releaseId: 'release', versionNumber: 1, state: 'draft', title: 'Borrador sintético',
    displayArtist: 'Artista sintético', titleLanguage: 'es', explicitContent: 'unknown',
    parties: [], credits: [], identifiers: [], rights: [], terms: [], comments: [], validation: { errors: [] },
    tracks: [{ trackId: 'synthetic-track', recordingId: 'synthetic-recording', discNumber: 1, trackNumber: 1,
      title: 'Pista sintética', titleLanguage: 'es', displayArtist: 'Artista sintético',
      isPrimaryResource: true, previewStartMs: null, previewDurationMs: null, durationMs: 60000, explicitContent: 'not_explicit' }],
    availability: [{ territory_mode: 'include', territories: ['Worldwide'], listening_policy: 'none',
      download_policy: 'purchase', purchasable: true, price_minor: '9007199254740993', currency: 'USD',
      downloadable_asset_id: 'synthetic-asset' }],
    assets: [{ id: 'synthetic-asset', recordingId: null, parentAssetId: 'synthetic-source', role: 'stream_audio',
      originalFilename: 'synthetic.mp3', mediaType: 'audio/mpeg', byteSize: 4, sha256: 'a'.repeat(64),
      processingState: 'ready', technicalMetadata: {}, createdAt: '2030-01-01T00:00:00Z' }],
    updatedAt: '2030-01-01T00:00:00Z',
  };
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path)
      || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      const body = request.postDataJSON();
      writes.push({ path, method: request.method(), body });
      // Only these two authoring writes are simulated. No terms, review,
      // publication, purchase or other mutation is allowed by the fixture.
      if (request.method() === 'PUT' && path === '/music/releases/release/versions/version/draft') {
        version.updatedAt = '2030-01-01T00:00:01Z';
        return route.fulfill({ json: version });
      }
      if (request.method() === 'PUT' && path === '/music/releases/release/versions/version/content') {
        const rule = body.availability[0];
        version.availability[0] = { ...version.availability[0], price_minor: rule.priceMinor, currency: rule.currency };
        version.updatedAt = '2030-01-01T00:00:02Z';
        return route.fulfill({ json: version });
      }
      return route.fulfill({ status: 409, json: { error: 'No synthetic action allowed' } });
    }
    const preferences = { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'synthetic-operator', displayName: 'Operador sintético', partyId: 101, roles: ['Admin'], modules: ['admin'], featureFlags: ['operations_enabled'], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/music/releases/release/versions/version') return route.fulfill({ json: version });
    if (path === '/music/analytics/releases/release') return route.fulfill({ json: {
      releaseId: 'release', disclaimer: 'Métricas sintéticas.', daily: [], tracks: [], territories: [],
      totals: { playStarts: 0, eligiblePlays: 0, completions: 0, skips: 0, listenedMs: 0, uniqueListeners: 0, purchases: 0, downloads: 0 },
    } });
    if (path === '/operations/work-items') {
      const filters = Object.fromEntries(url.searchParams);
      for (const key of ['minAmountMinor', 'maxAmountMinor']) if (filters[key] !== undefined) expect(Number.isSafeInteger(Number(filters[key]))).toBe(true);
      if (filters.minAmountMinor !== undefined && filters.maxAmountMinor !== undefined) expect(Number(filters.minAmountMinor)).toBeLessThanOrEqual(Number(filters.maxAmountMinor));
      reads.push(filters);
      return route.fulfill({ json: { items: [], nextCursor: null, hasMore: false } });
    }
    if (path === '/operations/saved-views') return route.fulfill({ json: [{ id: 'zero', name: 'Vista cero', filters: { minAmountMinor: 0, maxAmountMinor: 1005 } }] });
    if (path === '/operations/events') return route.fulfill({ json: { events: [], lastEventId: null, retryAfterMs: 15000 } });
    if (path === '/operations/metrics') return route.fulfill({ json: {
      newRegistrations: 0, registrationsRequiringAttention: 0, reservationsAwaitingConfirmation: 0,
      todaySessions: 0, schedulingConflicts: 0, unpaidInvoices: 0, overdueInvoices: 0,
      paymentsAwaitingVerification: 0, revenueReceivedTodayMinor: 0, unassignedWork: 0,
      slaBreaches: 0, averageFirstResponseSeconds: null, averageResolutionSeconds: null,
      integrationFailures: 0, currency: 'USD', calculatedAt: '2030-01-01T00:00:00Z',
    } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], states: [] } });
    return route.fulfill({ json: [] });
  });
  return { writes, reads };
}

async function checkError(page, input) {
  await expect(input).toHaveAttribute('inputmode', 'decimal');
  await expect(input).toHaveAttribute('aria-invalid', 'true');
  const helper = await input.getAttribute('aria-describedby');
  expect(helper).toBeTruthy();
  await expect(page.locator(`[id="${helper}"]`)).not.toBeEmpty();
  await page.addScriptTag({ content: axe.source });
  const violations = await input.evaluate(async element => (await globalThis.axe.run(element.closest('.MuiFormControl-root'), {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(v => ({ id: v.id, impact: v.impact })));
  expect(violations).toEqual([]);
}

async function showField(input) {
  await input.evaluate(element => element.scrollIntoView({ block: 'center', inline: 'nearest', behavior: 'instant' }));
  await expect(input).toBeInViewport({ ratio: 1 });
  expect(await input.evaluate(element => {
    const rect = element.getBoundingClientRect();
    return document.elementFromPoint(rect.x + rect.width / 2, rect.y + rect.height / 2) === element;
  })).toBe(true);
}

test('music authoring keeps exact 64-bit cents behind USD decimal entry and blocks malformed autosave @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const { writes } = await fixture(page, baseURL);
  await page.goto('/label/releases/release/versions/version');
  const price = page.getByRole('textbox', { name: 'Precio (USD)', exact: true });
  await expect(price).toHaveValue('90071992547409.93');
  await price.fill('4,999');
  await checkError(page, price);
  await expect(page.getByRole('button', { name: 'Guardar ahora', exact: true })).toBeDisabled();
  await expect(page.getByRole('button', { name: 'Enviar a revisión', exact: true })).toBeDisabled();
  // Exercise the real production debounce, without installing a fake clock.
  await page.waitForTimeout(1800);
  expect(writes).toEqual([]);
  await price.fill('92233720368547758,07');
  await page.getByRole('button', { name: 'Guardar ahora', exact: true }).click();
  await expect.poll(() => writes.length).toBe(2);
  expect(writes.map(write => write.path.split('/').pop())).toEqual(['draft', 'content']);
  expect(writes[1].body.availability[0].priceMinor).toBe('9223372036854775807');
  await expect(price).toHaveValue('92233720368547758.07');
  await page.reload();
  await expect(price).toHaveValue('92233720368547758.07');
  await showField(price);
  await page.screenshot({ path: testInfo.outputPath('music-usd-price.png') });
  expect(writes).toHaveLength(2);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});

test('operations filters preserve zero and exact bounds without querying invalid ranges @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const { reads, writes } = await fixture(page, baseURL);
  await page.goto('/dashboard/operations');
  await page.getByRole('combobox', { name: 'Vistas guardadas' }).click();
  await page.getByRole('option', { name: 'Vista cero', exact: true }).click();
  await expect.poll(() => reads.at(-1)?.minAmountMinor).toBe('0');
  await page.getByRole('button', { name: /^Filtros/ }).click();
  const minimum = page.getByRole('textbox', { name: 'Monto mínimo', exact: true });
  const maximum = page.getByRole('textbox', { name: 'Monto máximo', exact: true });
  await expect(minimum).toHaveValue('0.00'); await expect(maximum).toHaveValue('10.05');
  await showField(minimum);
  await page.screenshot({ path: testInfo.outputPath('operations-zero-filter.png') });
  await maximum.fill('40'); await minimum.fill('25,50');
  await expect.poll(() => reads.at(-1)?.minAmountMinor).toBe('2550');
  const before = reads.length;
  await minimum.fill('50');
  await checkError(page, minimum);
  await expect(page.getByRole('button', { name: 'Aplicar filtros', exact: true })).toBeDisabled();
  await page.waitForTimeout(120);
  expect(reads).toHaveLength(before);
  await maximum.fill('60');
  await expect.poll(() => reads.at(-1)?.minAmountMinor).toBe('5000');
  await expect.poll(() => reads.at(-1)?.maxAmountMinor).toBe('6000');
  await minimum.fill('-');
  await expect(minimum).toHaveAttribute('aria-invalid', 'true');
  await minimum.press('Escape');
  await page.getByRole('button', { name: /^Filtros/ }).click();
  await expect(minimum).toHaveValue('50.00'); await expect(maximum).toHaveValue('60.00');
  await page.getByRole('button', { name: 'Limpiar', exact: true }).click();
  await expect(minimum).toHaveValue(''); await expect(maximum).toHaveValue('');
  await page.getByRole('button', { name: 'Aplicar filtros', exact: true }).click();
  // The unfiltered result can be fresh in the app cache. Refresh explicitly
  // to verify the active query rather than assuming clearing must refetch.
  await page.getByRole('button', { name: 'Actualizar', exact: true }).click();
  await expect.poll(() => reads.at(-1)?.minAmountMinor).toBeUndefined();
  await expect.poll(() => reads.at(-1)?.maxAmountMinor).toBeUndefined();
  expect(writes).toEqual([]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
