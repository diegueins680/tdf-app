import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const offering = {
  scId: '22222222-2222-4222-8222-222222222222', scCode: 'dj-booth-practice', scName: 'Práctica en DJ Booth',
  scNameEs: 'Práctica en DJ Booth', scNameEn: 'DJ Booth practice',
  scCategoryId: '77777777-7777-4777-8777-777777777777', scKind: 'dj-practice',
  scPricingModelId: '44444444-4444-4444-8444-444444444444', scPricingModel: 'hourly',
  scRateCents: 5000, scCurrency: 'USD', scCurrencyId: '55555555-5555-4555-8555-555555555555',
  scBillingUnit: 'hour', scTaxRateCode: 'ec-iva-standard', scDefaultDurationMinutes: 30,
  scRequiresEngineer: false, scDefaultResources: [], scSortOrder: 20, scActive: true,
};

test('booking introduction is brief and practical details preserve edits @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const origin = new URL(baseURL).origin, writes = [];
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
    if (path === '/services/catalog/public') return route.fulfill({ json: { sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [offering] } });
    if (path === '/bookings/public/availability') return route.fulfill({ json: { available: true } });
    if (path === '/engineers' || path === '/rooms/public') return route.fulfill({ json: [] });
    if (path.startsWith('/reviews/')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: {} });
    return route.fulfill({ status: 404, json: {} });
  });
  await page.goto('/reservar?service=dj-booth-practice');
  const intro = page.getByTestId('booking-introduction'), details = page.getByTestId('booking-practical-details');
  const summary = details.locator('summary'), copy = page.getByTestId('booking-introduction-copy');
  await expect(details).not.toHaveAttribute('open', ''); await expect(copy).toHaveText('Elige el servicio y el horario. No necesitas cuenta.');
  await expect(summary).toHaveText('Horario y precios');
  const lines = await intro.evaluate(node => {
    const count = element => { const range = document.createRange(); range.selectNodeContents(element); return new Set([...range.getClientRects()].filter(rect => rect.height > 0).map(rect => Math.round(rect.top))).size; };
    return count(node.querySelector('[data-testid="booking-introduction-copy"]')) + count(node.querySelector('summary'));
  });
  expect(lines).toBeLessThanOrEqual(3);
  await page.getByLabel('Nombre completo').fill('Persona sintética'); await page.getByRole('textbox', { name: 'Correo', exact: true }).fill('synthetic@example.test');
  await expect(async () => {
    await summary.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
    await expect(summary).toBeInViewport({ ratio: 1, timeout: 500 });
    expect(await summary.evaluate(node => { const r = node.getBoundingClientRect(); return node.contains(document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)); })).toBe(true);
  }).toPass({ timeout: 8000 });
  await page.screenshot({ path: info.outputPath('booking-introduction-collapsed.png') });
  await summary.focus(); await page.keyboard.press('Enter'); await expect(details).toHaveAttribute('open', ''); await expect(summary).toBeFocused();
  await expect(details.getByText(/08:00–22:00, Quito \(GMT-5\)/)).toBeVisible(); await expect(details).toContainText('Precios de referencia en USD');
  await expect(details).not.toContainText('servidor');
  await expect.poll(() => intro.evaluate(node => node.getAnimations({ subtree: true }).filter(a => a.playState === 'running').length)).toBe(0);
  await page.addScriptTag({ content: axe.source });
  expect(await intro.evaluate(async node => (await globalThis.axe.run(node, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
  await page.screenshot({ path: info.outputPath('booking-introduction-expanded.png') });
  await page.keyboard.press('Enter'); await expect(details).not.toHaveAttribute('open', '');
  await expect(page.getByLabel('Nombre completo')).toHaveValue('Persona sintética'); await expect(page.getByRole('textbox', { name: 'Correo', exact: true })).toHaveValue('synthetic@example.test');
  await expect(page.getByRole('alert').filter({ hasText: 'Estimado para Práctica en DJ Booth' })).toBeVisible();
  await page.getByRole('button', { name: 'Continuar', exact: true }).click(); await expect(page.getByLabel(/^Fecha y hora/)).toBeVisible();
  await expect(page.getByLabel(/^Fecha y hora/)).toHaveAttribute('aria-describedby', /.+/);
  await expect(page.getByRole('button', { name: 'Confirmar reserva', exact: true })).toBeVisible();
  expect(writes).toEqual([]); expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
