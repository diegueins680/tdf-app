import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const firstId = '22222222-2222-4222-8222-222222222222';
const secondId = '33333333-3333-4333-8333-333333333333';
async function fixture(page, baseURL, rejected = false) {
  const origin = new URL(baseURL).origin;
  const state = { writes: [], reads: [], completed: [], release: null, statuses: new Map([[firstId, rejected ? 'rejected' : 'awaiting_evidence'], [secondId, 'awaiting_evidence']]) };
  await page.addInitScript(({ firstId, secondId }) => {
    localStorage.setItem('tdf-hq-ui/locale', 'es');
    sessionStorage.setItem(`tdf-marketplace-order-lookup:${firstId}`, 'synthetic-first-access');
    sessionStorage.setItem(`tdf-marketplace-order-lookup:${secondId}`, 'synthetic-second-access');
  }, { firstId, secondId });
  const order = id => ({
    moOrderId: id, moCartId: null, moCurrency: 'USD', moTotalUsdCents: 10000, moTotalDisplay: 'USD $100.00',
    moStatus: 'bank_transfer_pending', moStatusHistory: [], moCreatedAt: '2030-01-01T15:00:00Z', moUpdatedAt: '2030-01-01T15:00:00Z',
    moBuyerName: '', moBuyerEmail: '', moBuyerPhone: null, moPaymentProvider: 'bank_transfer',
    moPaypalOrderId: null, moPaypalPayerEmail: null, moPaidAt: null, moLookupToken: null,
    moCheckoutStatus: 'awaiting_payment', moManualPaymentStatus: state.statuses.get(id), moManualPaymentSubmittedAt: null,
    moOrderKind: 'sale', moFulfillmentMethod: 'pickup', moFulfillmentStatus: 'on_hold', moHoldExpiresAt: '2030-01-01T16:15:00Z',
    moTrackingReference: null, moFulfillmentHistory: [], moRentalStartDate: null, moRentalEndDate: null,
    moRentalDurationDays: null, moRentalChargeUsdCents: null, moSecurityDepositUsdCents: null, moDepositStatus: null,
    moDepositDeductionUsdCents: null, moRentalTermsVersion: null, moRentalTimezone: null, moConditionOut: null, moConditionIn: null,
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
      if (request.method() === 'POST' && path.endsWith('/manual-payment/evidence')) {
        state.writes.push({ id, body: request.postDataJSON(), token });
        const outcome = await new Promise(resolve => { state.release = resolve; });
        state.release = null;
        if (outcome === 503) return route.fulfill({ status: 503, json: { error: 'Synthetic reference rejection' } });
        state.statuses.set(id, 'submitted'); await route.fulfill({ json: order(id) }); state.completed.push(id); return;
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
  await expect(page.getByTestId('marketplace-reference-form')).toBeVisible();
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
  const form = page.getByTestId('marketplace-reference-form');
  if (await form.count()) {
    const outer = await card.boundingBox(), inner = await form.boundingBox();
    expect(inner.x).toBeGreaterThanOrEqual(outer.x); expect(inner.x + inner.width).toBeLessThanOrEqual(outer.x + outer.width);
  }
}

test('bank references retain invalid and rejected text, lock pending sends and remain unpaid @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const state = await fixture(page, baseURL, true);
  const input = page.getByRole('textbox', { name: 'Referencia corregida' });
  const form = page.getByTestId('marketplace-reference-form'), submit = form.getByRole('button', { name: 'Reenviar evidencia' });
  await reachable(submit); await submit.click(); await expect(input).toBeFocused();
  await expect(input).toHaveAttribute('aria-invalid', 'true');
  const help = page.locator(`[id="${await input.getAttribute('aria-describedby')}"]`);
  await expect(help).toContainText('3 a 120 caracteres');
  await input.fill('R'.repeat(121)); await input.press('Tab');
  await expect(input).toHaveValue('R'.repeat(121)); await expect(input).not.toHaveAttribute('maxlength');
  await inspect(page); expect(state.writes).toEqual([]);
  await reachable(input); await page.screenshot({ path: info.outputPath('marketplace-reference-errors.png') });
  await input.fill('  BANK-REFERENCE-1  '); await expect(input).toHaveAttribute('aria-invalid', 'false');
  await reachable(submit); await submit.click(); await expect.poll(() => state.writes.length).toBe(1);
  await expect(input).toBeDisabled(); await expect(form.getByRole('button', { name: 'Enviando…' })).toBeDisabled();
  await form.evaluate(node => node.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true })));
  expect(state.writes).toHaveLength(1); await inspect(page);
  state.release(503); await expect(page.getByText('No pudimos registrar la referencia. El pedido no fue marcado como pagado.')).toBeVisible();
  await expect(input).toBeEnabled(); await expect(input).toHaveValue('  BANK-REFERENCE-1  ');
  expect(state.writes).toHaveLength(1); await inspect(page);
  await reachable(input); await page.screenshot({ path: info.outputPath('marketplace-reference-retry.png') });
  await reachable(submit); await submit.click(); await expect.poll(() => state.writes.length).toBe(2);
  expect(state.writes[1]).toEqual(state.writes[0]); expect(state.writes[0]).toEqual({ id: firstId, body: { mmesCustomerReference: 'BANK-REFERENCE-1' }, token: 'synthetic-first-access' });
  state.release(200); await expect(form).toHaveCount(0);
  await expect(page.getByText('Referencia recibida. Sigue pendiente de revisión; este pedido aún no está pagado.')).toBeVisible();
  await expect(page.getByTestId('marketplace-order-card')).not.toContainText('Pago verificado'); await inspect(page);
});

test('a new order owns its draft when an old bank-reference response finishes @critical @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  const input = page.getByRole('textbox', { name: 'Referencia de transferencia' });
  await input.fill('OLD-BANK-REFERENCE');
  const submit = page.getByTestId('marketplace-reference-form').getByRole('button', { name: 'Enviar evidencia para revisión' });
  await reachable(submit); await submit.click(); await expect.poll(() => state.writes.length).toBe(1);
  await expect(input).toBeDisabled();
  await page.evaluate(id => { history.pushState({}, '', `/marketplace/orden/${id}`); dispatchEvent(new PopStateEvent('popstate')); }, secondId);
  const card = page.getByTestId('marketplace-order-card');
  await expect(card).toContainText(`Pedido ${secondId}`); await expect(card).not.toContainText(firstId);
  await expect(input).toBeEnabled(); await expect(input).toHaveValue('');
  await input.fill('NEW-BANK-REFERENCE'); state.release(200);
  await expect.poll(() => state.completed).toEqual([firstId]);
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
  await expect(input).toHaveValue('NEW-BANK-REFERENCE');
  await expect(card).toContainText(`Pedido ${secondId}`); await expect(card).not.toContainText('Referencia recibida.');
  await inspect(page); expect(state.writes).toHaveLength(1);
});
