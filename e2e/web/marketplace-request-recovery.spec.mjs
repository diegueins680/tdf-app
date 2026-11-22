import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const firstId = '22222222-2222-4222-8222-222222222222';
const secondId = '33333333-3333-4333-8333-333333333333';
async function fixture(page, baseURL, rejected = false) {
  const origin = new URL(baseURL).origin;
  const state = { orderOverrides: {}, writes: [], reads: [], completed: [], release: null, statuses: new Map([[firstId, rejected ? 'rejected' : 'awaiting_evidence'], [secondId, 'awaiting_evidence']]) };
  await page.addInitScript(({ firstId, secondId }) => {
    localStorage.setItem('tdf-hq-ui/locale', 'es');
    sessionStorage.setItem(`tdf-marketplace-order-lookup:${firstId}`, 'synthetic-first-access');
    sessionStorage.setItem(`tdf-marketplace-order-lookup:${secondId}`, 'synthetic-second-access');
  }, { firstId, secondId });
  const order = id => ({
    moOrderId: id, moCartId: null, moCurrency: 'USD', moTotalUsdCents: 10000, moTotalDisplay: 'USD $100.00',
    moStatus: 'paid', moStatusHistory: [], moCreatedAt: '2030-01-01T15:00:00Z', moUpdatedAt: '2030-01-01T15:00:00Z',
    moBuyerName: '', moBuyerEmail: '', moBuyerPhone: null, moPaymentProvider: 'bank_transfer',
    moPaypalOrderId: null, moPaypalPayerEmail: null, moPaidAt: '2030-01-01T15:01:00Z', moLookupToken: null,
    moCheckoutStatus: 'paid', moManualPaymentStatus: state.statuses.get(id), moManualPaymentSubmittedAt: null,
    moOrderKind: 'rental', moFulfillmentMethod: 'pickup', moFulfillmentStatus: 'confirmed', moHoldExpiresAt: '2030-01-01T16:15:00Z',
    moTrackingReference: null, moFulfillmentHistory: [], moRentalStartDate: '2030-01-01', moRentalEndDate: '2030-01-31',
    moRentalDurationDays: null, moRentalChargeUsdCents: null, moSecurityDepositUsdCents: null, moDepositStatus: null,
    moDepositDeductionUsdCents: null, moRentalTermsVersion: null, moRentalTimezone: 'America/Guayaquil', moConditionOut: null, moConditionIn: null,
    ...state.orderOverrides,
    moItems: [{ moiListingId: 'synthetic-listing', moiTitle: 'Micrófono sintético', moiQuantity: 1, moiUnitPriceUsdCents: 10000, moiSubtotalCents: 10000, moiUnitPriceDisplay: 'USD $100.00', moiSubtotalDisplay: 'USD $100.00' }],
  });
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path) || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (path.startsWith('/marketplace/orders/')) {
      const id = path.split('/')[3];
      expect([firstId, secondId]).toContain(id);
      const token = request.headers()['x-order-lookup-token'];
      expect(token).toBe(id === firstId ? 'synthetic-first-access' : 'synthetic-second-access');
      if (request.method() === 'POST' && path.endsWith('/requests')) {
        state.writes.push({ id, body: request.postDataJSON(), token, key: request.headers()['idempotency-key'] });
        const outcome = await new Promise(resolve => { state.release = resolve; });
        state.release = null;
        if (outcome === 503) return route.fulfill({ status: 503, json: { error: 'Synthetic reference rejection' } });
        const body = request.postDataJSON();
        await route.fulfill({ json: { mcrRequestId: 'synthetic-request', mcrOrderId: id, mcrOrderKind: 'rental', mcrRequestType: body.mcrsRequestType, mcrStatus: 'submitted', mcrReason: body.mcrsReason, mcrRequestedEndDate: body.mcrsRequestedEndDate ?? null, mcrEvidenceUrl: null, mcrRequestedAt: '2030-01-01T15:10:00Z', mcrReviewedAt: null, mcrReviewNotes: null } }); state.completed.push(id); return;
      }
      if (request.method() !== 'GET') { state.writes.push({ unexpected: path }); return route.fulfill({ status: 409, json: {} }); }
      if (path.endsWith('/requests')) return route.fulfill({ json: [] });
      state.reads.push(id); return route.fulfill({ json: order(id) });
    }
    if (request.method() !== 'GET') { state.writes.push({ unexpected: path }); return route.fulfill({ status: 409, json: {} }); }
    if (path === '/session') return route.fulfill({ status: 401, json: {} });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
    return route.fulfill({ json: [] });
  });
  await page.goto(`/marketplace/orden/${firstId}`);
  await expect(page.getByTestId('marketplace-request-form')).toBeVisible();
  await page.addScriptTag({ content: axe.source });
  return state;
}
async function reachable(control) {
  await expect(async () => {
    await control.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
    await expect(control).toBeInViewport({ ratio: 1, timeout: 500 });
    expect(await control.evaluate(node => { const r = node.getBoundingClientRect(); return node.contains(document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)); })).toBe(true);
  }).toPass({ timeout: 8000 });
}
async function inspect(page) {
  const card = page.getByTestId('marketplace-order-card');
  await card.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
  await expect.poll(() => card.evaluate(node => {
    for (let e = node; e; e = e.parentElement) if (Number(getComputedStyle(e).opacity) !== 1) return [{ notOpaque: e.className }];
    // Keyboard focus has a persistent decorative ripple; wait for every finite
    // transition while keeping focus and all WCAG rules active.
    return node.getAnimations({ subtree: true }).filter(a => a.playState === 'running'
      && !(a.effect?.getTiming().iterations === Infinity
        && a.effect.target instanceof Element && a.effect.target.matches('.MuiTouchRipple-childPulsate')))
      .map(a => ({ animation: a.animationName, target: a.effect?.target?.className, iterations: a.effect?.getTiming().iterations }));
  })).toEqual([]);
  expect(await card.evaluate(async node => (await globalThis.axe.run(node, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  const form = page.getByTestId('marketplace-request-form');
  if (await form.count()) {
    const outer = await card.boundingBox(), inner = await form.boundingBox();
    expect(inner.x).toBeGreaterThanOrEqual(outer.x); expect(inner.x + inner.width).toBeLessThanOrEqual(outer.x + outer.width);
  }
}

test('rental requests preserve invalid text and recover the same uncertain attempt before changes @critical @mobile-flow', async ({ page, baseURL }, info) => {
  // Seven validation, recovery and axe passes take ~26s on emulated WebKit iPhone; keep headroom like the merch recovery specs.
  test.setTimeout(60_000);
  const state = await fixture(page, baseURL);
  const form = page.getByTestId('marketplace-request-form'), submit = form.getByRole('button', { name: 'Enviar solicitud', exact: true });
  const type = form.getByRole('combobox', { name: /^Tipo de solicitud/ });
  const reason = form.getByRole('textbox', { name: /^Motivo/ });
  await reachable(submit); await submit.click(); await expect(type).toBeFocused();
  await expect(type).toHaveAttribute('aria-invalid', 'true'); expect(state.writes).toEqual([]);
  await type.click(); await page.getByRole('option', { name: 'Solicitar extensión de fechas' }).click();
  const date = form.getByLabel(/^Nueva fecha de devolución/);
  await date.fill('2030-01-31'); await reachable(submit); await submit.click(); await expect(date).toBeFocused();
  await expect(date).toHaveAttribute('aria-invalid', 'true'); await expect(date).toHaveAttribute('min', '2030-02-01');
  await date.fill('2030-02-01'); await reason.fill('R'.repeat(1001)); await reason.press('Tab');
  await expect(reason).toHaveValue('R'.repeat(1001)); await expect(reason).not.toHaveAttribute('maxlength');
  const help = page.locator(`[id="${await reason.getAttribute('aria-describedby')}"]`);
  await expect(help).toContainText('3 a 1000');
  expect(state.writes).toEqual([]); await inspect(page); await reachable(reason);
  await page.screenshot({ path: info.outputPath('marketplace-request-errors.png') });
  await reason.fill('  Solicitud sintética de extensión  '); await reachable(submit); await submit.click();
  await expect.poll(() => state.writes.length).toBe(1); await expect(reason).toBeDisabled(); await expect(date).toBeDisabled();
  await expect(type).toHaveAttribute('aria-disabled', 'true');
  await form.evaluate(node => node.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true })));
  expect(state.writes).toHaveLength(1); state.release(503);
  await expect(form.getByText(/No pudimos confirmar la solicitud/)).toBeVisible(); await expect(reason).toBeEnabled();
  await expect(reason).toHaveValue('  Solicitud sintética de extensión  '); await expect(date).toHaveValue('2030-02-01');
  await reason.fill('Un cambio posterior'); await expect(submit).toBeDisabled();
  const restore = form.getByRole('button', { name: 'Usar datos del intento anterior' });
  await inspect(page); await reachable(restore); await page.screenshot({ path: info.outputPath('marketplace-request-recovery.png') });
  await restore.click(); await expect(reason).toBeFocused();
  await expect(reason).toHaveValue('  Solicitud sintética de extensión  '); expect(state.writes).toHaveLength(1);
  await reachable(submit); await submit.click(); await expect.poll(() => state.writes.length).toBe(2);
  expect(state.writes[1]).toEqual(state.writes[0]); expect(state.writes[0].key).toBeTruthy();
  expect(state.writes[0].body).toEqual({ mcrsRequestType: 'rental_extension', mcrsReason: 'Solicitud sintética de extensión', mcrsRequestedEndDate: '2030-02-01' });
  state.release(200); await expect(reason).toHaveValue('');
  await expect(page.getByText('Solicitar extensión de fechas · En revisión')).toBeVisible();
  await expect(page.getByText(/^Fechas:/)).toContainText('2030-01-31');
  await expect(page.getByText(/^Fechas:/)).not.toContainText('2030-02-01');
  await expect(form).toContainText('no cancela, devuelve ni extiende automáticamente'); await inspect(page);
});

test('old customer-request completion cannot clear the new order draft @critical @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  const form = page.getByTestId('marketplace-request-form');
  await form.getByRole('combobox', { name: /^Tipo de solicitud/ }).click();
  await page.getByRole('option', { name: 'Cancelar antes de la entrega del equipo' }).click();
  const reason = form.getByRole('textbox', { name: /^Motivo/ }); await reason.fill('Solicitud de la primera orden');
  const submit = form.getByRole('button', { name: 'Enviar solicitud', exact: true });
  await reachable(submit); await submit.click(); await expect.poll(() => state.writes.length).toBe(1);
  await expect(reason).toBeDisabled();
  await page.evaluate(id => { history.pushState({}, '', `/marketplace/orden/${id}`); dispatchEvent(new PopStateEvent('popstate')); }, secondId);
  const card = page.getByTestId('marketplace-order-card'); await expect(card).toContainText(secondId);
  await expect(reason).toBeEnabled(); await expect(reason).toHaveValue('');
  await reason.fill('Borrador de la segunda orden'); state.release(200);
  await expect.poll(() => state.completed).toEqual([firstId]);
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
  await expect(reason).toHaveValue('Borrador de la segunda orden'); await expect(card).not.toContainText('Solicitud de la primera orden');
  expect(state.writes).toHaveLength(1); expect(state.writes[0].id).toBe(firstId); await inspect(page);
});


test('order states distinguish delivery, rental custody and deposit settlement without writes @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const state = await fixture(page, baseURL);
  state.orderOverrides = {
    moFulfillmentStatus: 'deposit_refund_due', moDepositStatus: 'partial_refund_due',
    moDepositDeductionUsdCents: 1000,
    moFulfillmentHistory: [['returned_pending_inspection', '2030-01-31T15:00:00Z'], ['deposit_refund_due', '2030-01-31T15:01:00Z']],
  };
  const refresh = async () => {
    await page.reload();
    await expect(page.getByTestId('marketplace-order-card')).toBeVisible();
    await page.addScriptTag({ content: axe.source });
  };
  await refresh();
  await expect(page.getByText('Renta: Devolución de depósito pendiente', { exact: true })).toBeVisible();
  await expect(page.getByText('Devuelta; inspección pendiente', { exact: true })).toBeVisible();
  await expect(page.getByText(/^Estado del depósito: Devolución parcial pendiente/)).toBeVisible();
  await expect(page.getByText('El pago, la custodia del equipo y la devolución del depósito se confirman por separado.')).toBeVisible();
  await inspect(page);
  const history = page.getByText('Historial de renta', { exact: true }).locator('..');
  await reachable(history.getByText('Devolución de depósito pendiente', { exact: true }));
  const historyBounds = await history.boundingBox();
  for (const chip of await history.locator('.MuiChip-root').all()) {
    const bounds = await chip.boundingBox();
    expect(bounds.x).toBeGreaterThanOrEqual(historyBounds.x);
    expect(bounds.x + bounds.width).toBeLessThanOrEqual(historyBounds.x + historyBounds.width);
  }
  expect(await history.evaluate(async node => (await globalThis.axe.run(node, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => v.id))).toEqual([]);
  await page.screenshot({ path: info.outputPath('marketplace-rental-status.png') });
  state.orderOverrides.moDepositStatus = 'partially_refunded';
  await refresh();
  await expect(page.getByText(/^Estado del depósito: Devuelto parcialmente/)).toBeVisible();
  const futureStatus = 'future_state_' + 'x'.repeat(80);
  state.orderOverrides = { moFulfillmentStatus: futureStatus, moDepositStatus: null,
    moFulfillmentHistory: [[futureStatus, '2030-01-31T15:00:00Z']] };
  await refresh();
  await expect(page.getByText('Estado del depósito: Sin información', { exact: true })).toBeVisible();
  await expect(page.getByText(`Renta: ${futureStatus}`, { exact: true })).toBeVisible();
  await expect(page.getByText(futureStatus, { exact: true })).toBeVisible();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  state.orderOverrides = { moOrderKind: 'sale', moFulfillmentStatus: 'ready_for_pickup',
    moFulfillmentHistory: [['on_hold', '2030-01-01T15:00:00Z'], ['ready_for_pickup', '2030-01-01T15:01:00Z']] };
  await refresh();
  await expect(page.getByText('Entrega: Lista para retiro', { exact: true })).toBeVisible();
  await expect(page.getByText('Reserva activa', { exact: true })).toBeVisible();
  await expect(page.getByText('Lista para retiro', { exact: true })).toBeVisible();
  await inspect(page);
  expect(state.writes).toEqual([]);
});
