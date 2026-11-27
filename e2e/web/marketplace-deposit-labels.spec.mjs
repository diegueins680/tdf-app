import { expect, test } from '@playwright/test';
import axe from 'axe-core';

async function fixture(page, baseURL, terminal = false) {
  const origin = new URL(baseURL).origin;
  const state = { writes: [], unknown: false };
  const orderId = '44444444-4444-4444-8444-444444444444';
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path)
      || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      state.writes.push({ path, method: request.method(), body: request.postDataJSON() });
      return route.fulfill({ status: 409, json: { error: 'No synthetic financial write allowed' } });
    }
    const preferences = { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'synthetic-reviewer', displayName: 'Revisor sintético', partyId: 202, roles: ['Admin'], modules: ['admin'], featureFlags: [], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/marketplace/orders') return route.fulfill({ json: [{
      moOrderId: orderId, moCartId: null, moCurrency: 'USD', moTotalUsdCents: 10000, moTotalDisplay: 'USD $100.00',
      moStatus: 'paid', moStatusHistory: [], moItems: [], moBuyerName: 'Cliente sintético', moBuyerEmail: 'synthetic@example.test',
      moPaymentProvider: 'paypal', moPaidAt: '2030-01-01T12:00:00Z', moCreatedAt: '2030-01-01T12:00:00Z', moUpdatedAt: '2030-01-01T12:00:00Z',
      moOrderKind: 'rental', moCheckoutStatus: 'paid', moFulfillmentMethod: 'pickup', moFulfillmentStatus: terminal ? 'closed' : 'deposit_refund_due',
      moRentalStartDate: '2030-01-01', moRentalEndDate: '2030-01-03', moRentalDurationDays: 3, moRentalChargeUsdCents: 5000,
      moSecurityDepositUsdCents: 5000, moDepositDeductionUsdCents: terminal ? 5000 : 1250,
      moDepositStatus: state.unknown ? null : terminal ? 'forfeited' : 'partial_refund_due',
      moRentalTermsVersion: 'synthetic-v1', moRentalTimezone: 'America/Guayaquil',
    }] });
    if (path === `/marketplace/orders/${orderId}/deposit-settlements`) return route.fulfill({ json: [{
      mdsSettlementId: '55555555-5555-4555-8555-555555555555', mdsOrderId: orderId, mdsCheckoutId: 'synthetic-checkout',
      mdsCurrency: 'USD', mdsDepositAmountMinor: 5000, mdsDeductionAmountMinor: terminal ? 5000 : 1250,
      mdsRefundAmountMinor: terminal ? 0 : 3750,
      mdsStatus: state.unknown ? 'future_status_' + 'x'.repeat(80) : terminal ? 'verified' : 'submitted',
      mdsSettlementMethod: state.unknown ? 'future_method' : terminal ? 'forfeiture' : 'bank_transfer',
      mdsExternalReference: 'SYNTHETIC-REFERENCE', mdsEvidenceUrl: 'https://example.test/private/synthetic-evidence.pdf',
      mdsSubmittedBy: 101, mdsSubmittedAt: '2030-01-03T12:00:00Z',
    }] });
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
      && !(a.effect?.getTiming().iterations === Infinity
        && a.effect.target instanceof Element && a.effect.target.matches('.MuiTouchRipple-childPulsate')));
  })).toBe(true);
  expect(await dialog.evaluate(async node => (await globalThis.axe.run(node, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
  expect(await dialog.evaluate(node => node.scrollWidth <= node.clientWidth)).toBe(true);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
}

test('staff deposit evidence stays pending until independent review without display writes @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const state = await fixture(page, baseURL);
  const dialog = page.getByRole('dialog');
  await expect(dialog.getByText(/^Estado del depósito: Devolución parcial pendiente/)).toBeVisible();
  const status = dialog.getByText('Evidencia en revisión', { exact: true });
  await expect(status).toBeVisible();
  await expect(dialog.getByText(/^Transferencia bancaria · devolución/)).toBeVisible();
  await expect(dialog.getByText('Registra evidencia real. Esto no ejecuta ni afirma un reembolso de Datafast o PayPal.')).toBeVisible();
  const verify = dialog.getByRole('button', { name: 'Verificar evidencia real', exact: true });
  await expect(verify).toBeDisabled();
  await expect(dialog.getByRole('button', { name: 'Requiere conciliación', exact: true })).toBeDisabled();
  await dialog.getByRole('textbox', { name: 'Notas de revisión independiente' }).fill('Revisión sintética sin envío.');
  await expect(verify).toBeEnabled();
  await expect(dialog.getByRole('button', { name: 'Requiere conciliación', exact: true })).toBeEnabled();
  await expect(dialog.getByRole('link', { name: 'Abrir evidencia protegida' })).toHaveAttribute('href', 'https://example.test/private/synthetic-evidence.pdf');
  await inspect(page);
  await status.scrollIntoViewIfNeeded();
  await page.screenshot({ path: info.outputPath('staff-deposit-pending.png') });
  expect(state.writes).toEqual([]);
});

test('staff verified evidence keeps forfeiture and unfamiliar data distinct @critical @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL, true);
  const dialog = page.getByRole('dialog');
  await expect(dialog.getByText(/^Estado del depósito: Depósito retenido/)).toBeVisible();
  await expect(dialog.getByText('Evidencia verificada', { exact: true })).toBeVisible();
  await expect(dialog.getByText(/^Retención total documentada · devolución/)).toBeVisible();
  await expect(dialog.getByRole('button', { name: 'Verificar evidencia real', exact: true })).toHaveCount(0);
  await inspect(page);
  state.unknown = true;
  await page.reload();
  await page.getByRole('button', { name: /^Abrir orden/ }).first().click();
  await expect(dialog.getByText(/^Estado del depósito: Sin información/)).toBeVisible();
  const unknown = dialog.getByText('future_status_' + 'x'.repeat(80), { exact: true });
  await expect(unknown).toBeVisible();
  await expect(dialog.getByText(/^future_method · devolución/)).toBeVisible();
  await expect(dialog.getByRole('button', { name: 'Verificar evidencia real', exact: true })).toHaveCount(0);
  await inspect(page);
  const outer = await dialog.boundingBox(), inner = await unknown.boundingBox();
  expect(inner.x).toBeGreaterThanOrEqual(outer.x); expect(inner.x + inner.width).toBeLessThanOrEqual(outer.x + outer.width);
  expect(state.writes).toEqual([]);
});
