import { expect, test } from '@playwright/test';
import axe from 'axe-core';

async function fixture(page, baseURL) {
  const writes = [], origin = new URL(baseURL).origin;
  await page.clock.setFixedTime(new Date('2030-11-20T01:15:00Z'));
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin || url.protocol === 'blob:' ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path)
      || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      const body = request.postDataJSON(); writes.push({ path, method: request.method(), body });
      expect(path).toBe('/payments'); expect(request.method()).toBe('POST');
      expect(body).toMatchObject({ pcAmountCents: 4099, pcCurrency: 'USD', pcPartyId: 11, pcPaidAt: '2026-11-14' });
      if (writes.length === 1) return route.fulfill({ status: 422, json: { error: 'Synthetic validation rejection; no payment recorded' } });
      return route.fulfill({ json: { payId: 77, payPartyId: 11, payPartyDisplayName: 'Contacto sintético', payAmountCents: 4099,
        payCurrency: 'USD', payMethod: body.pcMethod, payPaidAt: body.pcPaidAt } });
    }
    const preferences = { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'synthetic-admin', partyId: 42, roles: ['Admin'], modules: ['admin', 'invoicing'], featureFlags: [], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/parties/search') return route.fulfill({ json: { items: [{ partyId: 11, displayName: 'Contacto sintético', partyType: 'person', username: null, avatarUrl: null, secondaryLabel: null, accountStatus: 'active' }], nextCursor: null } });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], catalogs: [] } });
    return route.fulfill({ json: [] });
  });
  return writes;
}

async function inspect(page, field) {
  await expect(field).toHaveAttribute('aria-invalid', 'true'); await expect(field).toHaveAttribute('inputmode', 'decimal');
  const helper = await field.getAttribute('aria-describedby'); expect(helper).toBeTruthy();
  await expect(page.locator(`[id="${helper}"]`)).not.toBeEmpty();
  await expect(async () => {
    await field.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
    await expect(field).toBeInViewport({ ratio: 1, timeout: 500 });
    expect(await field.evaluate(node => { const r = node.getBoundingClientRect(); return node.contains(document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)); })).toBe(true);
  }).toPass({ timeout: 8000 });
  await expect.poll(() => field.evaluate(node => node.closest('.MuiFormControl-root').getAnimations({ subtree: true }).filter(a => a.playState === 'running').length)).toBe(0);
  await page.addScriptTag({ content: axe.source });
  expect(await field.evaluate(async node => (await globalThis.axe.run(node.closest('.MuiFormControl-root'), {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
}

test('manual payment requires an entire exact amount and retains invalid draft and rejected edits @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const writes = await fixture(page, baseURL);
  await page.goto('/finanzas/pagos');
  const card = page.locator('.MuiCard-root').filter({ has: page.getByText('Nuevo pago manual', { exact: true }) });
  const contact = card.getByRole('combobox', { name: /^Contacto/ });
  await contact.fill('Contacto'); await page.getByRole('option', { name: /Contacto sintético/ }).click();
  const amount = card.getByRole('textbox', { name: 'Monto (USD)', exact: true });
  const submit = card.getByRole('button', { name: 'Registrar pago', exact: true });
  await amount.fill('12abc'); await amount.press('Tab'); await expect(submit).toBeDisabled();
  await inspect(page, amount); await page.screenshot({ path: info.outputPath('manual-payment-inline-error.png') });
  await page.reload(); await expect(amount).toHaveValue('12abc'); await expect(submit).toBeDisabled();
  await amount.fill('1.234'); await expect(submit).toBeDisabled(); expect(writes).toEqual([]);
  await amount.fill('40,99'); await card.getByLabel(/^Fecha de pago/).fill('2026-11-14');
  await submit.click(); await expect(card.getByRole('alert').filter({ hasText: 'Synthetic validation rejection' })).toBeVisible();
  await expect(amount).toHaveValue('40,99'); await submit.click();
  await expect(amount).toHaveValue(''); expect(writes).toHaveLength(2); expect(writes[0]).toEqual(writes[1]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});

test('report hourly rate blocks invalid export and saves/restores exact cents in a local template @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const writes = await fixture(page, baseURL);
  await page.goto('/finanzas/creador-reporte-cuenta');
  await expect(page.getByLabel('Corte', { exact: true })).toHaveValue('2030-11-19');
  await page.getByRole('button', { name: 'Agregar bloque', exact: true }).click();
  await expect(page.getByLabel('Fecha', { exact: true })).toHaveValue('2030-11-19');
  await page.getByRole('button', { name: 'Eliminar bloque', exact: true }).click();
  const rate = page.getByRole('textbox', { name: 'Tarifa USD/h', exact: true });
  const save = page.getByRole('button', { name: 'Guardar plantilla', exact: true });
  const download = page.locator('a[download="reporte-cuenta.pdf"]');
  await rate.fill('40,99'); await expect(download).toHaveAttribute('href', /^blob:/);
  await rate.fill('25abc'); await rate.press('Tab'); await inspect(page, rate);
  await expect(save).toBeDisabled(); await expect(download).not.toHaveAttribute('href');
  await expect(page.getByRole('alert').filter({ hasText: 'La vista previa conserva la última tarifa válida' })).toBeVisible();
  expect(await page.evaluate(() => localStorage.getItem('tdf-work-account-report-builder-templates'))).toBeNull();
  await page.screenshot({ path: info.outputPath('report-hourly-rate-inline-error.png') });
  await rate.fill('0,29'); await page.getByRole('textbox', { name: 'Nombre de la plantilla', exact: true }).fill('Plantilla sintética');
  await save.click();
  expect(await page.evaluate(() => JSON.parse(localStorage.getItem('tdf-work-account-report-builder-templates'))[0].source.hourlyRateCents)).toBe(29);
  await page.reload(); await page.getByRole('button', { name: 'Cargar', exact: true }).click();
  await expect(rate).toHaveValue('0.29'); await expect(download).toHaveAttribute('href', /^blob:/);
  await rate.fill('90071992547409.91');
  await expect(page.locator('main')).toContainText('90.071.992.547.409,91');
  await save.click();
  expect(await page.evaluate(() => JSON.parse(localStorage.getItem('tdf-work-account-report-builder-templates'))[0].source.hourlyRateCents)).toBe(Number.MAX_SAFE_INTEGER);
  expect(writes).toEqual([]); expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
