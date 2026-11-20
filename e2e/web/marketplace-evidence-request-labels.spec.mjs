import { expect, test } from '@playwright/test';
import axe from 'axe-core';

async function fixture(page, baseURL, variant = 'manual') {
  const origin = new URL(baseURL).origin;
  const state = { writes: [], amount: 10000, unknown: false };
  const orderId = '66666666-6666-4666-8666-666666666666';
  const rental = variant === 'request';
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path)
      || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      state.writes.push({ path, method: request.method(), body: request.postDataJSON() });
      return route.fulfill({ status: 409, json: { error: 'No synthetic financial or domain write allowed' } });
    }
    const preferences = { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'synthetic-reviewer', displayName: 'Revisor sintético', partyId: 202, roles: ['Admin'], modules: ['admin'], featureFlags: [], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/marketplace/orders') return route.fulfill({ json: [{
      moOrderId: orderId, moCartId: null, moCurrency: 'USD', moTotalUsdCents: 10000, moTotalDisplay: 'USD $100.00',
      moStatus: rental ? 'paid' : 'bank_transfer_pending', moStatusHistory: [], moItems: [],
      moBuyerName: 'Cliente sintético', moBuyerEmail: 'synthetic@example.test', moPaymentProvider: rental ? 'paypal' : 'bank_transfer',
      moPaidAt: rental ? '2030-01-01T12:00:00Z' : null, moCreatedAt: '2030-01-01T12:00:00Z', moUpdatedAt: '2030-01-01T12:00:00Z',
      moOrderKind: rental ? 'rental' : 'sale', moCheckoutStatus: rental ? 'paid' : 'awaiting_payment',
      moFulfillmentMethod: 'pickup', moFulfillmentStatus: rental ? 'confirmed' : 'on_hold',
      moRentalStartDate: rental ? '2030-01-10' : null, moRentalEndDate: rental ? '2030-01-12' : null,
      moRentalDurationDays: rental ? 3 : null, moRentalChargeUsdCents: rental ? 5000 : null,
      moSecurityDepositUsdCents: rental ? 5000 : null, moDepositStatus: rental ? 'collected' : null,
      moRentalTermsVersion: rental ? 'synthetic-v1' : null, moRentalTimezone: rental ? 'America/Guayaquil' : null,
    }] });
    if (path === `/marketplace/orders/${orderId}/commerce`) return route.fulfill({ json: {
      mpcOrderId: orderId, mpcCheckoutId: 'synthetic-checkout', mpcPaymentStatus: 'awaiting_payment',
      mpcHoldExpiresAt: '2030-01-01T13:00:00Z', mpcOrderKind: 'sale', mpcManualEvidence: {
        mmeEvidenceId: 'synthetic-evidence', mmePaymentMethod: 'bank_transfer',
        mmeStatus: variant === 'approved' ? 'approved' : 'submitted', mmeCustomerReference: 'SYNTHETIC-REFERENCE',
        mmeSubmittedAmountMinor: state.amount, mmeCurrency: 'USD', mmeSubmittedBy: 101, mmeSubmittedAt: '2030-01-01T12:00:00Z',
      },
    } });
    if (path === `/marketplace/orders/${orderId}/customer-requests`) return route.fulfill({ json: rental ? [{
      mcrRequestId: 'synthetic-request', mcrOrderId: orderId, mcrOrderKind: 'rental',
      mcrRequestType: state.unknown ? 'future_request_' + 'x'.repeat(70) : 'rental_extension',
      mcrStatus: state.unknown ? 'future_request_state' : 'submitted', mcrReason: 'Extensión sintética sin enviar revisión.',
      mcrRequestedEndDate: '2030-01-20', mcrRequestedAt: '2030-01-01T12:00:00Z',
    }] : [] });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], states: [] } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/operacion/ordenes-marketplace');
  await page.getByRole('button', { name: /^Abrir orden/ }).first().click();
  return state;
}

async function inspect(page) {
  const dialog = page.getByRole('dialog');
  await page.addScriptTag({ content: axe.source });
  await expect.poll(() => dialog.evaluate(node => {
    for (let e = node; e; e = e.parentElement) if (Number(getComputedStyle(e).opacity) !== 1) return false;
    return !node.getAnimations({ subtree: true }).some(a => a.playState === 'running'
      && !(a.effect?.getTiming().iterations === Infinity && a.effect.target instanceof Element
        && a.effect.target.matches('.MuiTouchRipple-childPulsate')));
  })).toBe(true);
  expect(await dialog.evaluate(async node => (await globalThis.axe.run(node, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
  expect(await dialog.evaluate(node => node.scrollWidth <= node.clientWidth)).toBe(true);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
}

test('manual evidence uses readable status and exact currency units without confirming payment @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const state = await fixture(page, baseURL);
  const dialog = page.getByRole('dialog');
  const status = dialog.getByText('Referencia recibida', { exact: true });
  await expect(status).toBeVisible();
  await expect(dialog.getByText('Método: Transferencia bancaria', { exact: true })).toBeVisible();
  const amount = dialog.getByText('Monto declarado:', { exact: true }).locator('..');
  await expect(amount).toContainText(/100[.,]00/); await expect(amount).not.toContainText('10000');
  await expect(dialog.getByText('La evidencia del cliente no confirma pago por sí sola.')).toBeVisible();
  const approve = dialog.getByRole('button', { name: 'Aprobar pago verificado', exact: true });
  await expect(approve).toBeDisabled();
  await dialog.getByRole('textbox', { name: 'Notas de revisión', exact: true }).fill('Revisión sintética sin enviar.');
  await expect(approve).toBeEnabled();
  await expect(dialog.getByText(/^Pago manual verificado\./)).toHaveCount(0);
  await inspect(page); await status.scrollIntoViewIfNeeded();
  await page.screenshot({ path: info.outputPath('staff-manual-evidence-amount.png') });
  expect(state.writes).toEqual([]);
});

test('approved evidence and missing amounts never invent paid or free orders @critical @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL, 'approved');
  const dialog = page.getByRole('dialog');
  await expect(dialog.getByText('Evidencia aprobada', { exact: true })).toBeVisible();
  await expect(dialog.getByText(/^Pago manual verificado\./)).toHaveCount(0);
  await expect(dialog.getByRole('button', { name: 'Aprobar pago verificado', exact: true })).toHaveCount(0);
  state.amount = null;
  await page.reload(); await page.getByRole('button', { name: /^Abrir orden/ }).first().click();
  await expect(dialog.getByText('Monto declarado: Sin monto declarado', { exact: true })).toBeVisible();
  await expect(dialog.getByText(/^Monto declarado:.*Gratis/)).toHaveCount(0);
  await expect(dialog.getByText(/^Pago manual verificado\./)).toHaveCount(0);
  await inspect(page); expect(state.writes).toEqual([]);
});

test('staff request labels preserve quote-only extension review and unfamiliar codes @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const state = await fixture(page, baseURL, 'request');
  const dialog = page.getByRole('dialog');
  const type = dialog.getByText('Solicitar extensión de fechas', { exact: true });
  await expect(type).toBeVisible(); await expect(dialog.getByText('En revisión', { exact: true })).toBeVisible();
  const quote = dialog.getByRole('button', { name: 'Requiere cotización', exact: true });
  await expect(quote).toBeDisabled(); await expect(dialog.getByRole('button', { name: 'Aprobar transición', exact: true })).toHaveCount(0);
  await dialog.getByRole('textbox', { name: 'Notas para el cliente', exact: true }).fill('Cotización sintética pendiente, sin envío.');
  await expect(quote).toBeEnabled();
  await expect(dialog.getByText('Fechas:', { exact: true }).locator('..')).toContainText('2030-01-12');
  await inspect(page); await type.scrollIntoViewIfNeeded();
  await page.screenshot({ path: info.outputPath('staff-request-labels.png') });
  state.unknown = true;
  await page.reload(); await page.getByRole('button', { name: /^Abrir orden/ }).first().click();
  const unknown = dialog.getByText('future_request_' + 'x'.repeat(70), { exact: true });
  await expect(unknown).toBeVisible(); await expect(dialog.getByText('future_request_state', { exact: true })).toBeVisible();
  await expect(dialog.getByRole('button', { name: 'Requiere cotización', exact: true })).toHaveCount(0);
  await expect(dialog.getByRole('button', { name: 'Aprobar transición', exact: true })).toHaveCount(0);
  await inspect(page);
  const outer = await dialog.boundingBox(), inner = await unknown.boundingBox();
  expect(inner.x).toBeGreaterThanOrEqual(outer.x); expect(inner.x + inner.width).toBeLessThanOrEqual(outer.x + outer.width);
  expect(state.writes).toEqual([]);
});
