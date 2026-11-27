import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Synthetic authenticated reads only. Shell lifecycle calls are mocked;
// every other non-GET is captured and refused, including every financial API.
async function fixture(page, baseURL) {
  const origin = new URL(baseURL).origin, writes = [];
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path)
      || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      writes.push({ method: request.method(), path });
      return route.fulfill({ status: 409, json: { error: 'No synthetic financial write allowed' } });
    }
    const preferences = { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'synthetic-operator', displayName: 'Operador sintético', partyId: 42, roles: ['Admin'], modules: ['admin'], featureFlags: [], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/merch/capabilities') return route.fulfill({ json: { features: { refundOperations: true } } });
    if (path === '/merch/seller/stores') return route.fulfill({ json: [{ id: 'synthetic-store', slug: 'synthetic-store', displayName: 'Tienda sintética', currency: 'USD', operationalStatus: 'active', permissions: { catalog: true, settings: true } }] });
    if (path === '/merch/admin/issues') return route.fulfill({ json: [{
      id: 'synthetic-issue', orderId: 'synthetic-order', storeId: 'synthetic-store', orderNumber: 'SYNTHETIC',
      storeName: 'Tienda sintética', customerName: 'Cliente sintético', customerEmail: 'synthetic@example.test',
      issueType: 'refund', status: 'staff_review', message: 'Solicitud sintética', currency: 'USD',
      totalMinor: 5000, refundedMinor: 1000, paymentStatus: 'partially_refunded',
      createdAt: '2030-01-01T12:00:00Z', updatedAt: '2030-01-01T12:00:00Z',
    }] });
    if (path === '/marketplace/orders') return route.fulfill({ json: [{
      moOrderId: 'synthetic-rental', moCartId: 'synthetic-cart', moCurrency: 'usd', moTotalUsdCents: 10000,
      moTotalDisplay: 'USD $100.00', moStatus: 'paid', moStatusHistory: [], moItems: [],
      moBuyerName: 'Cliente sintético', moBuyerEmail: 'synthetic@example.test', moPaymentProvider: 'paypal',
      moCreatedAt: '2030-01-01T12:00:00Z', moUpdatedAt: '2030-01-01T12:00:00Z',
      moOrderKind: 'rental', moCheckoutStatus: 'paid', moFulfillmentStatus: 'returned_pending_inspection',
      moSecurityDepositUsdCents: 5000, moDepositDeductionUsdCents: 1250, moConditionIn: 'Condición sintética verificada.',
    }] });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], states: [] } });
    return route.fulfill({ json: [] });
  });
  return writes;
}

async function errorAssociation(page, input) {
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

async function showAmount(input) {
  await input.evaluate(element => element.scrollIntoView({ block: 'center', inline: 'nearest', behavior: 'instant' }));
  await expect(input).toBeInViewport({ ratio: 1 });
  expect(await input.evaluate(element => {
    const rect = element.getBoundingClientRect();
    return document.elementFromPoint(rect.x + rect.width / 2, rect.y + rect.height / 2) === element;
  })).toBe(true);
}

test('merch prices and delivery accept currency units without submitting invalid money @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const writes = await fixture(page, baseURL);
  await page.goto('/mi-tienda');
  const price = page.getByRole('textbox', { name: /^Precio \(USD\)/ });
  await expect(price).toHaveValue('10.00');
  await price.fill('25,505');
  await errorAssociation(page, price);
  await expect(page.getByRole('button', { name: 'Guardar borrador' })).toBeDisabled();
  await price.fill('25,50');
  await expect(price).toHaveAttribute('aria-invalid', 'false');
  await showAmount(price);
  await page.screenshot({ path: testInfo.outputPath('merch-price-currency.png') });
  await page.getByRole('tab', { name: 'Políticas y entrega' }).click();
  const rate = page.getByRole('textbox', { name: /^Tarifa \(USD\)/ });
  await rate.fill('5,255');
  await errorAssociation(page, rate);
  await expect(page.getByRole('button', { name: 'Agregar zona' })).toBeDisabled();
  await rate.fill('0');
  await expect(rate).toHaveAttribute('aria-invalid', 'false');
  await expect(page.getByRole('button', { name: 'Agregar zona' })).toBeEnabled();
  expect(writes).toEqual([]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});

test('refund currency input keeps the verified balance and request-only meaning @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const writes = await fixture(page, baseURL);
  await page.goto('/admin/merch');
  await expect(page.getByText('Reembolso · En revisión del equipo TDF', { exact: true })).toBeVisible();
  await expect(page.getByText(/Pago: Reembolsado parcialmente.*Reembolso: Ninguno/)).toBeVisible();
  const amount = page.getByRole('textbox', { name: /^Monto \(USD;/ });
  await amount.fill('40.01');
  await errorAssociation(page, amount);
  await expect(page.getByRole('button', { name: 'Registrar solicitud de reembolso' })).toBeDisabled();
  await amount.fill('25,50');
  await expect(amount).toHaveAttribute('aria-invalid', 'false');
  await expect(page.getByRole('button', { name: 'Registrar solicitud de reembolso' })).toBeEnabled();
  await expect(page.getByText(/Nunca contacta al proveedor ni devuelve dinero/)).toBeVisible();
  await showAmount(amount);
  await page.screenshot({ path: testInfo.outputPath('refund-currency.png') });
  expect(writes).toEqual([]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});

test('rental deduction displays and validates currency without changing custody or payment @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const writes = await fixture(page, baseURL);
  await page.goto('/operacion/ordenes-marketplace');
  await page.getByRole('button', { name: /^Abrir orden/ }).first().click();
  const dialog = page.getByRole('dialog');
  const amount = dialog.getByRole('textbox', { name: 'Deducción propuesta del depósito (USD)', exact: true });
  await expect(amount).toHaveValue('12.50');
  await dialog.getByRole('combobox', { name: 'Siguiente estado de renta' }).click();
  await page.getByRole('option', { name: 'Daños en revisión', exact: true }).click();
  await amount.fill('50.01');
  await errorAssociation(page, amount);
  await expect(dialog.getByRole('button', { name: 'Guardar transición de renta' })).toBeDisabled();
  await amount.fill('25,50');
  await expect(amount).toHaveAttribute('aria-invalid', 'false');
  await expect(dialog.getByRole('button', { name: 'Guardar transición de renta' })).toBeEnabled();
  await expect(dialog.getByText(/La propuesta no ejecuta un cobro ni un reembolso/)).toBeVisible();
  await showAmount(amount);
  await page.screenshot({ path: testInfo.outputPath('rental-deduction-currency.png') });
  expect(writes).toEqual([]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
