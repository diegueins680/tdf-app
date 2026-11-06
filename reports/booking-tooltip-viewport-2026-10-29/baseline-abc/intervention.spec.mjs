import { expect, test } from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs';
import axe from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/axe-core/axe.js';

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


async function interventionGeometry(page, anchor, label) {
  return page.evaluate(({ anchor, label }) => {
    const viewport = { at: performance.now(), compatMode: document.compatMode, innerWidth,
      clientWidth: document.documentElement.clientWidth, scrollWidth: document.documentElement.scrollWidth,
      visualViewport: visualViewport && { width: visualViewport.width, scale: visualViewport.scale } };
    const describe = element => {
      if (!element) return null;
      const r = element.getBoundingClientRect();
      return { tag: element.tagName, text: element.textContent?.slice(0, 100), x:r.x, y:r.y, right:r.right,
        width:r.width, height:r.height, clientWidth:element.clientWidth, scrollWidth:element.scrollWidth,
        transform:element.style.transform, placement:element.getAttribute('data-popper-placement') };
    };
    return { ...viewport, overflow:viewport.scrollWidth > viewport.clientWidth, label,
      anchor:describe(anchor), tooltip:describe(document.querySelector('[role="tooltip"]')),
      bookingCard:describe(document.querySelector('[data-testid="public-booking-card"]')),
      serviceSelect:describe(document.querySelector('[role="combobox"]')),
      dateInput:describe(document.querySelector('input[type="datetime-local"]')) };
  }, { anchor, label });
}

// All business/session APIs are fixtures. Only local static assets reach the
// preview server; no reservation, provider request or external traffic is sent.
for (const variant of ['A-open-before-resize', 'B-closed-before-resize', 'C-open-after-resize']) {
  const failure = 'error';
  test(`tooltip intervention ${variant} @critical @mobile-flow`, async ({ page, baseURL }, testInfo) => {
    const origin = new URL(baseURL).origin;
    const errors = [], writes = [];
    let requests = 0, recovered = false, releaseInitial, releaseRetry;
    const initialGate = new Promise(resolve => { releaseInitial = resolve; });
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
          await initialGate;
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
    await page.setViewportSize({ width: 390, height: 740 });
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
    const slot = page.getByText('Sugerencias rápidas', { exact: true }).locator('..').getByRole('button').nth(2);
    const tooltip = page.getByRole('tooltip');
    const slotText = await slot.innerText();
    // Normal pointer actions establish the intervention; no DOM removal or CSS edits.
    await page.mouse.move(0, 0);
    await expect(tooltip).toHaveCount(0);
    if (variant === 'A-open-before-resize') {
      await slot.hover();
      await expect(tooltip).toBeVisible();
      await expect(tooltip).toHaveAttribute('data-popper-placement', /^(top|bottom|left|right)/);
    }
    const anchor = await slot.elementHandle();
    const preResize = await interventionGeometry(page, anchor, 'pre-resize-390');
    await page.setViewportSize({ width: 320, height: 740 });
    await expect(page.getByText('Paso 2 de 3', { exact: true })).toBeVisible();
    await expect.poll(() => page.evaluate(() => {
      const width = document.documentElement.clientWidth;
      return width > 0 && width <= 320;
    })).toBe(true);
    if (variant === 'C-open-after-resize') {
      await slot.hover();
      await expect(tooltip).toBeVisible();
      await expect(tooltip).toHaveAttribute('data-popper-placement', /^(top|bottom|left|right)/);
    }
    // Keep the first result immutable; later fit never changes the assertion.
    const initial = await interventionGeometry(page, anchor, 'initial-after-intervention');
    const samples = await failedViewportGeometry(page);
    const evidence = { variant, slotText, requestedViewport: page.viewportSize(), preResize, initial, samples,
      browser: testInfo.project.name, hasTouch: testInfo.project.use.hasTouch ?? false,
      errors, writes, providerCalls: 0, sourceHead: '535b113e7b4c139be9ea570c18e8d29c6a34700d',
      scope: 'Controlled synthetic local UI intervention; no WebKit or production/provider execution.' };
    await testInfo.attach('tooltip-intervention-geometry', { body: Buffer.from(JSON.stringify(evidence, null, 2)), contentType: 'application/json' });
    await page.screenshot({ path: testInfo.outputPath('intervention.png'), fullPage: true });
    expect(initial.compatMode).toBe('CSS1Compat');
    expect(initial.overflow).toBe(false);
    expect(errors).toEqual([]);
    expect(writes).toEqual([]);
    await page.screenshot({ path: testInfo.outputPath('booking-catalog-recovered.png'), fullPage: true });
  });
}
