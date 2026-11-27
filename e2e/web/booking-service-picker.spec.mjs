import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const offering = {
  scId: '22222222-2222-4222-8222-222222222222', scCode: 'dj-booth-practice', scName: 'Práctica en cabina DJ con acompañamiento para preparación de una presentación en vivo',
  scNameEs: 'Práctica en DJ Booth', scNameEn: 'DJ Booth practice',
  scCategoryId: '77777777-7777-4777-8777-777777777777', scKind: 'dj-practice',
  scPricingModelId: '44444444-4444-4444-8444-444444444444', scPricingModel: 'hourly',
  scRateCents: 5000, scCurrency: 'USD', scCurrencyId: '55555555-5555-4555-8555-555555555555',
  scBillingUnit: 'hour', scTaxRateCode: 'ec-iva-standard', scDefaultDurationMinutes: 30,
  scRequiresEngineer: false, scDefaultResources: [], scSortOrder: 20, scActive: true,
};

const services = [
  offering,
  { ...offering, scId: '33333333-3333-4333-8333-333333333333', scCode: 'long-song', scName: 'Preparación' + 'musical'.repeat(18), scBillingUnit: 'song', scRateCents: 0, scSortOrder: 21 },
  { ...offering, scId: '44444444-4444-4444-8444-444444444444', scCode: 'custom-rate', scName: 'Sesión con unidad personalizada', scBillingUnit: 'jornada extendida', scRateCents: 12550, scSortOrder: 22 },
  { ...offering, scId: '55555555-5555-4555-8555-555555555555', scCode: 'unpriced', scName: 'Servicio sin precio de referencia', scRateCents: null, scBillingUnit: null, scSortOrder: 23 },
];

// Inspect actual text rectangles against the control and its containing column.
// Root overflow alone misses text clipped by a card or a select's hidden overflow.
async function containedText(locator, outer) {
  const bounds = await outer.boundingBox();
  expect(bounds).not.toBeNull();
  expect(await locator.evaluate((node, limit) => {
    const own = node.getBoundingClientRect(), walker = document.createTreeWalker(node, NodeFilter.SHOW_TEXT);
    let text;
    while ((text = walker.nextNode())) {
      if (!text.textContent.trim()) continue;
      const range = document.createRange(); range.selectNodeContents(text);
      for (const r of range.getClientRects()) {
        if (r.width && (r.left < Math.max(own.left, limit.x) - 1 || r.right > Math.min(own.right, limit.x + limit.width) + 1 || r.top < Math.max(own.top, limit.y) - 1 || r.bottom > Math.min(own.bottom, limit.y + limit.height) + 1)) return { text: text.textContent, rect: r.toJSON(), own: own.toJSON(), limit };
      }
    }
    return node.scrollWidth <= node.clientWidth + 1 ? true : { scrollWidth: node.scrollWidth, clientWidth: node.clientWidth };
  }, bounds)).toBe(true);
}

async function settledAxe(page, locator) {
  await expect.poll(() => locator.evaluate(node => {
    for (let e = node; e; e = e.parentElement) if (Number(getComputedStyle(e).opacity) !== 1) return false;
    return node.getAnimations({ subtree: true }).every(a => a.playState !== 'running');
  })).toBe(true);
  expect(await locator.evaluate(async node => (await globalThis.axe.run(node, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
}

test('service names and billing units stay readable inside the booking column @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const origin = new URL(baseURL).origin, writes = [], availability = [];
  await page.clock.setFixedTime(new Date('2029-12-01T12:00:00Z'));
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path) || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') { writes.push(path); return route.fulfill({ status: 409, json: { error: 'No real booking is allowed in this fixture' } }); }
    if (path === '/session') return route.fulfill({ status: 401, json: {} });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/services/catalog/public') return route.fulfill({ json: { sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: services } });
    if (path === '/bookings/public/availability') { availability.push(url.searchParams.get('serviceOfferingId')); return route.fulfill({ json: { available: true } }); }
    if (path === '/engineers' || path === '/rooms/public') return route.fulfill({ json: [] });
    if (path.startsWith('/reviews/')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: {} });
    return route.fulfill({ status: 404, json: {} });
  });
  await page.goto('/reservar?service=dj-booth-practice');
  const field = page.getByTestId('booking-service-field'), select = page.getByRole('combobox', { name: 'Servicio' });
  const card = page.getByTestId('public-booking-card');
  await expect(select).toHaveText(services[0].scName);
  await expect(field).toContainText('Estimado: USD 50 / hora');
  await containedText(select, field); await containedText(field, card);
  await page.getByLabel('Nombre completo').fill('Persona sintética');
  await page.getByRole('textbox', { name: 'Correo', exact: true }).fill('synthetic@example.test');
  await select.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
  await expect(select).toBeInViewport({ ratio: 1 });
  expect(await select.evaluate(node => { const r = node.getBoundingClientRect(); return node.contains(document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)); })).toBe(true);
  await page.addScriptTag({ content: axe.source }); await settledAxe(page, field);
  await page.screenshot({ path: info.outputPath('booking-service-selected.png') });
  await select.click();
  const options = page.getByRole('option'), list = page.getByRole('listbox');
  await expect(options).toHaveCount(services.length);
  await settledAxe(page, list);
  for (let i = 0; i < services.length; i++) {
    const option = options.nth(i);
    await expect(option).toHaveAttribute('data-value', services[i].scId);
    await expect(option).toContainText(services[i].scName);
    await option.scrollIntoViewIfNeeded();
    await expect(option).toBeInViewport();
    await containedText(option, list);
    const r = await option.boundingBox(); expect(r.x).toBeGreaterThanOrEqual(0);
    expect(r.x + r.width).toBeLessThanOrEqual(await page.evaluate(() => innerWidth));
    expect(r.y).toBeGreaterThanOrEqual(0);
    expect(r.y + r.height).toBeLessThanOrEqual(await page.evaluate(() => innerHeight));
    expect(await option.evaluate(node => { const r = node.getBoundingClientRect(); return [r.left + 2, r.right - 2].every(x => node.contains(document.elementFromPoint(x, r.y + r.height / 2))); })).toBe(true);
  }
  await expect(options.nth(0)).toContainText('USD 50 / hora');
  await expect(options.nth(1)).toContainText('USD 0 / canción');
  const decimalRate = await page.evaluate(() => (125.5).toLocaleString(undefined, { minimumFractionDigits: 0, maximumFractionDigits: 2 }));
  await expect(options.nth(2)).toContainText(`USD ${decimalRate} / jornada extendida`);
  await expect(options.nth(3)).toHaveText(services[3].scName);
  await options.first().scrollIntoViewIfNeeded(); await settledAxe(page, list);
  await page.screenshot({ path: info.outputPath('booking-service-options.png') });
  await page.keyboard.press('Escape'); await expect(select).toBeFocused();
  await page.keyboard.press('Enter'); await expect(options.first()).toBeFocused();
  await page.keyboard.press('ArrowDown'); await expect(options.nth(1)).toBeFocused();
  await page.keyboard.press('Enter'); await expect(select).toBeFocused();
  await expect(select).toHaveText(services[1].scName);
  await expect(field).toContainText('Estimado: USD 0 / canción');
  await containedText(select, field); await containedText(field, card);
  await expect(page.getByLabel('Nombre completo')).toHaveValue('Persona sintética');
  await expect(page.getByRole('textbox', { name: 'Correo', exact: true })).toHaveValue('synthetic@example.test');
  await page.getByRole('button', { name: 'Continuar', exact: true }).click();
  await expect(page.getByLabel(/^Fecha y hora/)).toBeVisible();
  await expect.poll(() => availability.at(-1)).toBe(services[1].scId);
  expect(writes).toEqual([]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
