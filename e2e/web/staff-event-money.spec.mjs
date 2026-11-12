import { expect, test } from '@playwright/test';
import axe from 'axe-core';

test('event money fields accept currency units and associate errors without a financial write @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const origin = new URL(baseURL).origin;
  const writes = [];
  const preferences = { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' };
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    // Signed-in shell lifecycle calls are synthetic too; they are independent
    // of submitting these forms. Every other non-GET is captured and refused.
    if (request.method() === 'POST' && [
      '/session/onboarding/reconcile', '/radio/presence', '/navigation/preferences/social.events/visit',
    ].includes(path)) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      writes.push({ path, body: request.postDataJSON() });
      return route.fulfill({ status: 409, json: { error: 'No synthetic write allowed' } });
    }
    if (path === '/session') return route.fulfill({ json: {
      username: 'synthetic-organizer', displayName: 'Organizador sintético', partyId: 42,
      roles: ['Admin'], modules: [], featureFlags: [], preferences,
    } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/social-events/events') return route.fulfill({ json: [{
      eventId: '7', eventOrganizerPartyId: '42', eventTitle: 'Importes sintéticos',
      eventStart: '2099-05-03T20:00:00Z', eventCurrency: 'EUR', eventArtists: [],
    }] });
    if (path === '/social-events/events/7/finance-summary') return route.fulfill({ json: {
      efsEventId: '7', efsCurrency: 'EUR', efsPlannedIncomeCents: 0, efsPlannedExpenseCents: 0,
      efsActualIncomeCents: 0, efsActualExpenseCents: 0, efsNetCents: 0,
      efsTicketPaidRevenueCents: 0, efsTicketRefundedRevenueCents: 0, efsTicketPendingRevenueCents: 0,
      efsGeneratedAt: '2099-05-01T12:00:00Z',
    } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], states: [] } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/social/eventos');
  const price = page.getByRole('textbox', { name: 'Precio (USD)', exact: true });
  const budget = page.getByRole('textbox', { name: 'Plan (EUR)', exact: true });
  const contract = page.getByRole('textbox', { name: 'Monto (EUR)', exact: true }).nth(0);
  const finance = page.getByRole('textbox', { name: 'Monto (EUR)', exact: true }).nth(1);
  const inputs = [price, budget, contract, finance];
  for (const input of inputs) {
    await expect(input).toHaveAttribute('inputmode', 'decimal');
    await input.fill('25,505');
    await expect(input).toHaveAttribute('aria-invalid', 'true');
    const helper = await input.getAttribute('aria-describedby');
    expect(helper).toBeTruthy();
    await expect(page.locator(`[id="${helper}"]`)).toHaveText('El precio debe tener como máximo dos decimales.');
  }
  await page.getByRole('textbox', { name: 'Nombre', exact: true }).fill('General');
  await page.getByRole('spinbutton', { name: 'Cantidad', exact: true }).fill('2');
  await page.getByRole('textbox', { name: 'Nombre línea', exact: true }).fill('Producción');
  for (const concept of await page.getByRole('textbox', { name: 'Concepto', exact: true }).all()) await concept.fill('Concepto sintético');
  for (const name of ['Crear tier', 'Agregar línea', 'Crear contrato', 'Registrar']) {
    await page.getByRole('button', { name, exact: true }).click();
    await expect(page.getByRole('alert').filter({ hasText: 'El precio debe tener como máximo dos decimales.' })).toBeVisible();
  }
  expect(writes).toEqual([]);
  await page.addScriptTag({ content: axe.source });
  for (const input of inputs) {
    const violations = await input.evaluate(async element => (await globalThis.axe.run(element.closest('.MuiFormControl-root'), {
      runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
    })).violations.map(v => ({ id: v.id, impact: v.impact })));
    expect(violations).toEqual([]);
    await input.fill('25,50');
    await expect(input).toHaveAttribute('aria-invalid', 'false');
    await expect(input).toHaveValue('25,50');
  }
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  await budget.scrollIntoViewIfNeeded();
  await page.screenshot({ path: testInfo.outputPath('event-budget-currency.png') });
  expect(writes).toEqual([]);
});
