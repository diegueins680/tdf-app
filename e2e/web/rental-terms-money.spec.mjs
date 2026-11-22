import { expect, test } from '@playwright/test';
import axe from 'axe-core';

test('rental terms use exact USD amounts with inline limits and preserve rejected edits for retry @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const writes = [], origin = new URL(baseURL).origin;
  const listing = { miListingId: 'synthetic-rental', miAssetId: 'synthetic-asset', miTitle: 'Equipo sintético',
    miBrand: 'Fixture', miModel: 'Modelo', miCategory: 'Audio', miCondition: 'used', miPurpose: 'rent',
    miStatus: 'En stock', miPriceUsdCents: 2550, miPriceDisplay: 'USD 25.50', miPhotoUrl: null,
    miRentalWeeklyPriceUsdCents: 12345, miRentalSecurityDepositUsdCents: 0, miRentalLateFeeUsdCents: 151,
    miRentalTermsVersion: 'synthetic-v1', miRentalTermsSummary: 'Política sintética sin publicación real.',
    miRentalMinDays: 2, miRentalMaxDays: 15, miRentalCancellationWindowHours: 48 };
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path)
      || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      const body = request.postDataJSON(); writes.push({ method: request.method(), path, body });
      expect(path).toBe('/marketplace/synthetic-rental/rental-terms'); expect(request.method()).toBe('PUT');
      expect(body).toEqual({ mrtuDailyRateUsdCents: 4099, mrtuWeeklyRateUsdCents: null, mrtuSecurityDepositUsdCents: 1205,
        mrtuLateFeeUsdCents: 0, mrtuMinDays: 2, mrtuMaxDays: 15, mrtuCancellationWindowHours: 48,
        mrtuTimezone: 'America/Guayaquil', mrtuTermsVersion: 'synthetic-v2', mrtuTermsSummary: listing.miRentalTermsSummary, mrtuActive: true });
      if (writes.length === 1) return route.fulfill({ status: 503, json: { error: 'Synthetic rejection; no version activated' } });
      return route.fulfill({ json: { ...listing, miPriceUsdCents: 4099, miRentalTermsVersion: 'synthetic-v2' } });
    }
    const preferences = { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'synthetic-operator', partyId: 42, roles: ['Admin'], modules: ['admin'], featureFlags: [], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/marketplace') return route.fulfill({ json: [listing] });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/marketplace');
  await page.getByRole('button', { name: 'Configurar renta', exact: true }).click();
  const dialog = page.getByRole('dialog', { name: 'Tarifa y términos de renta' });
  const daily = dialog.getByRole('textbox', { name: 'Tarifa diaria (USD)', exact: true });
  const weekly = dialog.getByRole('textbox', { name: 'Tarifa semanal (USD, opcional)', exact: true });
  const deposit = dialog.getByRole('textbox', { name: 'Depósito reembolsable (USD)', exact: true });
  const late = dialog.getByRole('textbox', { name: 'Cargo tardío (USD)', exact: true });
  const save = dialog.getByRole('button', { name: 'Guardar versión', exact: true });
  await expect(daily).toHaveValue('25.50'); await expect(weekly).toHaveValue('123.45');
  await expect(deposit).toHaveValue('0.00'); await expect(late).toHaveValue('1.51');
  await daily.fill('1.234'); await daily.press('Tab'); await expect(save).toBeDisabled();
  await expect(daily).toHaveAttribute('aria-invalid', 'true'); await expect(daily).toHaveAttribute('inputmode', 'decimal');
  const helper = await daily.getAttribute('aria-describedby'); expect(helper).toBeTruthy();
  await expect(page.locator(`[id="${helper}"]`)).toContainText('dos decimales');
  await daily.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
  await expect(daily).toBeInViewport({ ratio: 1 });
  expect(await daily.evaluate(node => { const r = node.getBoundingClientRect(); return node.contains(document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)); })).toBe(true);
  await expect.poll(() => daily.evaluate(node => node.closest('.MuiFormControl-root').getAnimations({ subtree: true }).filter(a => a.playState === 'running').length)).toBe(0);
  await page.addScriptTag({ content: axe.source });
  expect(await daily.evaluate(async node => (await globalThis.axe.run(node.closest('.MuiFormControl-root'), {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
  await page.screenshot({ path: info.outputPath('rental-price-inline-error.png') });
  await daily.fill('21474836.48'); await expect(save).toBeDisabled();
  await expect(page.locator(`[id="${helper}"]`)).toContainText('21474836.47');
  await daily.fill('40,99'); await weekly.fill('0'); await expect(save).toBeDisabled();
  await weekly.fill(''); await deposit.fill(''); await expect(save).toBeDisabled();
  await deposit.fill('12.05'); await late.fill('0');
  await dialog.getByRole('textbox', { name: 'Versión de términos', exact: true }).fill('synthetic-v2');
  expect(writes).toEqual([]); await save.click();
  await expect(dialog.getByRole('alert').filter({ hasText: 'Synthetic rejection' })).toBeVisible();
  await expect(daily).toHaveValue('40,99'); await save.click(); await expect(dialog).toBeHidden();
  expect(writes).toHaveLength(2); expect(writes[0]).toEqual(writes[1]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
