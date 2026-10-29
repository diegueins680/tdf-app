import { expect, test } from '@playwright/test';
import axe from 'axe-core';

async function measureBookingViewport(page) {
  return page.evaluate(() => {
    const viewport = { at: performance.now(), compatMode: document.compatMode, innerWidth,
      clientWidth: document.documentElement.clientWidth,
      scrollWidth: document.documentElement.scrollWidth,
      visualViewport: visualViewport && { width: visualViewport.width, scale: visualViewport.scale } };
    // Mobile innerWidth can grow with overflowing content; clientWidth is the
    // visible content bound, including on a page whose layout viewport grows.
    return { ...viewport, overflow: viewport.scrollWidth > viewport.clientWidth };
  });
}

// Observe a failed measurement without changing its asserted result. In
// particular, a later fit must not turn the original failure into a pass.
async function failedViewportGeometry(page) {
  return page.evaluate(async () => {
    const describe = element => {
      if (!element) return null;
      const rect = element.getBoundingClientRect();
      return { tag: element.tagName, id: element.id, classes: element.getAttribute('class'),
        text: element.textContent?.slice(0, 100), x: rect.x, right: rect.right,
        width: rect.width, clientWidth: element.clientWidth, scrollWidth: element.scrollWidth,
        transform: element.style.transform };
    };
    const sample = label => ({ label, at: performance.now(), innerWidth,
      clientWidth: document.documentElement.clientWidth,
      scrollWidth: document.documentElement.scrollWidth,
      visualViewport: visualViewport && { width: visualViewport.width, scale: visualViewport.scale },
      bookingCard: describe(document.querySelector('[data-testid="public-booking-card"]')),
      serviceSelect: describe(document.querySelector('[role="combobox"]')),
      dateInput: describe(document.querySelector('input[type="datetime-local"]')),
      tooltip: describe(document.querySelector('[role="tooltip"]')),
      tooltipPositioner: describe(document.querySelector('[role="tooltip"]')?.closest('[data-popper-placement]')),
      beyondClientViewport: [...document.querySelectorAll('body *')].filter(element => {
        const rect = element.getBoundingClientRect();
        const style = getComputedStyle(element);
        return rect.width > 0 && rect.right > document.documentElement.clientWidth + 0.5
          && style.display !== 'none' && style.visibility !== 'hidden';
      }).slice(0, 15).map(describe),
    });
    const samples = [sample('after-failed-one-shot')];
    await Promise.race([new Promise(resolve => requestAnimationFrame(resolve)),
      new Promise(resolve => setTimeout(resolve, 250))]);
    samples.push(sample('next-frame-or-250ms'));
    await new Promise(resolve => setTimeout(resolve, 100));
    samples.push(sample('another-100ms'));
    await new Promise(resolve => setTimeout(resolve, 300));
    samples.push(sample('another-300ms'));
    return samples;
  });
}

test('booking viewport check detects known client-width overflow @critical @mobile-flow', async ({ page }) => {
  await page.setViewportSize({ width: 320, height: 740 });
  await page.setContent('<!doctype html><meta name="viewport" content="width=device-width, initial-scale=1"><style>html,body{margin:0}</style>');
  await expect.poll(() => page.evaluate(() => document.documentElement.clientWidth)).toBe(320);
  expect((await measureBookingViewport(page)).overflow).toBe(false);
  await page.evaluate(() => {
    const offender = document.createElement('div');
    offender.id = 'known-viewport-overflow';
    offender.style.cssText = 'position:absolute;left:0;top:0;width:calc(100vw + 80px);height:1px;background:red;';
    document.body.append(offender);
  });
  await expect.poll(async () => (await measureBookingViewport(page)).overflow).toBe(true);
  const wide = await measureBookingViewport(page);
  expect(wide.compatMode).toBe('CSS1Compat');
  expect(wide.clientWidth).toBe(320);
  expect(wide.scrollWidth).toBeGreaterThan(320);
  await page.locator('#known-viewport-overflow').evaluate(element => element.remove());
  await expect.poll(async () => (await measureBookingViewport(page)).overflow).toBe(false);
});

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
    // SessionQueryProvider retries 503s three times (1s, 2s, 4s). Begin the
    // UI assertion after the final synthetic response, rather than spending
    // its 8s budget on that unrelated backoff. The case keeps its 30s budget.
    const initialFailure = failure === 'error' ? page.waitForResponse(response =>
      new URL(response.url()).pathname.replace(/^\/api(?=\/)/, '') === '/services/catalog/public'
      && response.status() === 503 && requests === 4) : null;
    releaseInitial();
    if (initialFailure) await initialFailure;
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
    // The mobile marker exists at both390 and320px. Bind the measurement to
    // the requested content viewport, without waiting for overflow to vanish.
    await expect(page.getByText('Paso 2 de 3', { exact: true })).toBeVisible();
    await expect.poll(() => page.evaluate(() => {
      const width = document.documentElement.clientWidth;
      return width > 0 && width <= 320;
    })).toBe(true);
    const viewportMeasurement = await measureBookingViewport(page);
    if (viewportMeasurement.overflow) await testInfo.attach('booking-viewport-geometry', {
      body: Buffer.from(JSON.stringify({ requestedViewport: page.viewportSize(), initial: viewportMeasurement,
        samples: await failedViewportGeometry(page) }, null, 2)),
      contentType: 'application/json',
    });
    expect(viewportMeasurement.compatMode).toBe('CSS1Compat');
    expect(viewportMeasurement.overflow).toBe(false);
    expect(errors).toEqual([]);
    expect(writes).toEqual([]);
    await page.screenshot({ path: testInfo.outputPath('booking-catalog-recovered.png'), fullPage: true });
  });
}
