import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// All business/session APIs are fixtures. Only local static assets reach the
// preview server; no reservation, provider request or external traffic is sent.
for (const failure of ['error', 'empty']) {
  test(`public booking recovers an ${failure} catalog without losing contact details @critical @mobile-flow`, async ({ page, baseURL }, testInfo) => {
    const origin = new URL(baseURL).origin;
    const errors = [], writes = [];
    let requests = 0, recovered = false, releaseInitial, releaseRetry;
    const initial = new Promise(resolve => { releaseInitial = resolve; });
    const retry = new Promise(resolve => { releaseRetry = resolve; });
    page.on('pageerror', error => errors.push(error.message));
    await page.addInitScript(() => localStorage.setItem('tdf-hq-ui/locale', 'es'));
    await page.route('**/*', async route => {
      const request = route.request(), url = new URL(request.url());
      if (url.origin !== origin && !/(^|\.)tdfrecords\.net$/.test(url.hostname)) return route.abort();
      if (request.method() !== 'GET') {
        writes.push({ method: request.method(), path: url.pathname });
        return route.abort();
      }
      if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
      const path = url.pathname.replace(/^\/api(?=\/)/, '');
      if (path === '/session') return route.fulfill({ json: null });
      if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
      if (path === '/services/catalog/public') {
        requests++;
        if (!recovered) {
          await initial;
          return failure === 'error'
            ? route.fulfill({ status: 503, json: { error: 'Synthetic catalog outage' } })
            : route.fulfill({ json: { sceItems: [] } });
        }
        await retry;
        return route.fulfill({ json: { sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [{
          scId: '11111111-1111-4111-8111-111111111111', scCode: 'recording-fixture',
          scName: 'Grabación de prueba', scNameEs: 'Grabación de prueba', scNameEn: 'Test recording',
          scCategoryId: '33333333-3333-4333-8333-333333333333', scKind: 'recording',
          scPricingModelId: '44444444-4444-4444-8444-444444444444', scPricingModel: 'hourly',
          scRateCents: 10000, scCurrency: 'USD', scCurrencyId: '55555555-5555-4555-8555-555555555555',
          scBillingUnit: 'hour', scDefaultDurationMinutes: 60, scRequiresEngineer: false,
          scDefaultResources: [], scSortOrder: 0, scActive: true,
        }] } });
      }
      if (path.includes('/engineers')) return route.fulfill({ json: [] });
      if (path.includes('/reviews')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
      if (path.includes('/availability')) return route.fulfill({ json: { available: true } });
      return route.fulfill({ status: 404, json: { error: 'No fixture for this API' } });
    });
    await page.goto('/reservar?service=recording-fixture', { waitUntil: 'domcontentloaded' });
    const name = page.getByRole('textbox', { name: 'Nombre completo', exact: true });
    const email = page.getByRole('textbox', { name: 'Correo', exact: true });
    await name.fill('Visitante de prueba');
    await email.fill('visitante@example.test');
    releaseInitial();
    const warning = page.getByRole('alert').filter({ hasText: 'No pudimos cargar el catálogo de servicios' });
    await expect(warning).toBeVisible();
    await expect(name).toBeDisabled();
    const retryButton = warning.getByRole('button', { name: 'Reintentar catálogo', exact: true });
    await expect(retryButton).toBeEnabled();
    await page.addScriptTag({ content: axe.source });
    const violations = await warning.evaluate(async element => (await globalThis.axe.run(element, {
      runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
    })).violations.filter(v => ['serious', 'critical'].includes(v.impact)).map(v => ({ id: v.id, impact: v.impact })));
    expect(violations).toEqual([]);
    const before = requests;
    recovered = true;
    await retryButton.focus();
    await retryButton.press('Enter');
    await expect(warning.getByRole('button', { name: 'Cargando catálogo…', exact: true })).toBeDisabled();
    await expect(name).toBeDisabled();
    await expect.poll(() => requests).toBe(before + 1);
    expect(writes).toEqual([]);
    releaseRetry();
    await expect(warning).toHaveCount(0);
    await expect(name).toBeEnabled();
    await expect(name).toHaveValue('Visitante de prueba');
    await expect(email).toHaveValue('visitante@example.test');
    await expect(page.getByRole('combobox', { name: /^Servicio\b/ })).toContainText('Grabación de prueba');
    await page.getByRole('button', { name: 'Continuar', exact: true }).click();
    await expect(page.getByLabel(/^Fecha y hora/)).toBeVisible();
    await page.setViewportSize({ width: 320, height: 740 });
    expect(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth)).toBe(false);
    expect(errors).toEqual([]);
    expect(writes).toEqual([]);
    await page.screenshot({ path: testInfo.outputPath('booking-catalog-recovered.png'), fullPage: true });
  });
}
