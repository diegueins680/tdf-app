import { expect, test } from '@playwright/test';
import axe from 'axe-core';

async function fixture(page, baseURL, language, paid) {
  const origin = new URL(baseURL).origin;
  const state = { paid, cancelled: false, failReads: false, reads: 0, writes: [], issues: [], release: undefined };
  let now = Date.now(); await page.clock.setFixedTime(now);
  await page.addInitScript(locale => {
    localStorage.setItem('tdf-hq-ui/locale', locale);
    sessionStorage.setItem('tdf-merch-order:synthetic-order', JSON.stringify({ id: 'synthetic-order', token: 'synthetic-private-order-access' }));
  }, language);
  const order = () => ({ id: 'synthetic-order', orderNumber: 'SYNTHETIC-001', commercialStatus: state.cancelled ? 'cancelled' : state.paid ? 'confirmed' : 'created', paymentStatus: state.paid ? 'paid' : 'pending', fulfillmentStatus: state.cancelled ? 'cancelled' : paid ? 'delivered' : 'pending', refundStatus: 'none', currency: 'USD', totalMinor: 3000, lines: [], timeline: [], issues: state.issues });
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path) || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      expect(request.method()).toBe('POST'); expect(path).toBe(`/merch/orders/synthetic-order/${paid ? 'issues' : 'cancel'}`);
      const body = request.postDataJSON(), key = request.headers()['idempotency-key'], token = request.headers()['x-order-lookup-token'];
      expect(token).toBe('synthetic-private-order-access'); expect(key).toBeTruthy(); state.writes.push({ path, body, key, token });
      if (paid) {
        expect(body).toEqual({ issueType: 'refund', message: 'Solicitud sintética de ayuda' });
        if (state.writes.length === 1) { await new Promise(resolve => { state.release = resolve; }); return route.fulfill({ status: 503, json: { error: 'Synthetic issue rejection' } }); }
        expect(state.writes).toHaveLength(2); const issue = { id: 'synthetic-issue', issueType: 'refund', status: 'open', message: body.message };
        state.issues.push(issue); return route.fulfill({ json: issue });
      }
      expect(body).toEqual({ reason: 'Motivo sintético para cancelar' });
      if (state.writes.length === 1) return route.fulfill({ status: 409, json: { error: 'Synthetic cancellation conflict' } });
      expect(state.writes).toHaveLength(2); await new Promise(resolve => { state.release = resolve; }); state.cancelled = true; return route.fulfill({ json: order() });
    }
    const preferences = { locale: language, currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'buyer@example.test', displayName: 'Compradora sintética', partyId: 42, roles: ['Customer'], modules: [], featureFlags: [], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/merch/orders/synthetic-order') {
      expect(request.headers()['x-order-lookup-token']).toBe('synthetic-private-order-access'); state.reads += 1;
      if (state.failReads) return route.fulfill({ status: 503, json: { error: 'Synthetic refresh failure' } });
      return route.fulfill({ json: order() });
    }
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], catalogs: [] } });
    return route.fulfill({ json: [] });
  });
  state.refresh = async () => { const before = state.reads; now += 31_000; await page.clock.setFixedTime(now); await page.evaluate(() => window.dispatchEvent(new Event('visibilitychange'))); await expect.poll(() => state.reads).toBeGreaterThan(before); };
  return state;
}
async function inspect(page, control, root) {
  await expect(async () => {
    await control.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
    await expect(control).toBeInViewport({ ratio: 1, timeout: 500 });
    expect(await control.evaluate(node => { const r = node.getBoundingClientRect(); return node.contains(document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)); })).toBe(true);
  }).toPass({ timeout: 8000 });
  await expect.poll(() => root.evaluate(node => (node.closest('.MuiModal-root') ?? node).getAnimations({ subtree: true }).filter(a => a.playState === 'running').length)).toBe(0);
  await page.addScriptTag({ content: axe.source });
  expect(await root.evaluate(async node => (await globalThis.axe.run(node, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
}
for (const language of ['es', 'en']) {
  const en = language === 'en';
  test(`order support retains requests without changing payment, ${language} @critical @mobile-flow`, async ({ page, baseURL }, info) => {
    test.setTimeout(60_000); const state = await fixture(page, baseURL, language, true);
    await page.goto('/tienda/orden/synthetic-order');
    await expect(page.getByRole('heading', { name: en ? 'Updates' : 'Novedades', exact: true })).toBeVisible();
    const form = page.getByRole('form', { name: en ? 'Report a problem' : 'Reportar un problema' });
    const input = form.getByRole('textbox', { name: en ? 'What happened?' : '¿Qué ocurrió?', exact: true });
    const submit = form.getByRole('button', { name: en ? 'Submit issue' : 'Enviar incidencia', exact: true });
    await submit.click(); await expect(input).toBeFocused(); await expect(input).toHaveAttribute('aria-invalid', 'true');
    await input.fill('x'.repeat(5001)); await submit.click(); await expect(input).toHaveValue('x'.repeat(5001)); expect(state.writes).toEqual([]);
    const helper = await input.getAttribute('aria-describedby'); expect(helper).toBeTruthy(); await expect(page.locator(`[id="${helper}"]`)).toHaveText(en ? 'Use between 10 and 5000 characters.' : 'Usa entre 10 y 5000 caracteres.');
    await inspect(page, input, form); await page.screenshot({ path: info.outputPath('merch-order-report-error.png') });
    await input.fill('Solicitud sintética de ayuda'); await form.getByRole('combobox').click(); await page.getByRole('option', { name: en ? 'Refund' : 'Reembolso', exact: true }).click();
    state.failReads = true; await state.refresh(); await expect(page.getByRole('alert').filter({ hasText: en ? 'last loaded values' : 'últimos valores cargados' })).toBeVisible();
    await expect(submit).toBeDisabled(); await form.evaluate(node => node.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }))); expect(state.writes).toEqual([]); await expect(input).toHaveValue('Solicitud sintética de ayuda');
    state.failReads = false; await page.getByRole('button', { name: en ? 'Try again' : 'Reintentar', exact: true }).click(); await expect(submit).toBeEnabled();
    await submit.click(); await expect.poll(() => state.writes.length).toBe(1); await expect(input).toBeDisabled(); await expect(form.getByRole('combobox')).toBeDisabled();
    await expect(page.getByRole('button', { name: en ? 'Review verified purchase' : 'Evaluar compra verificada', exact: true })).toBeDisabled();
    await form.evaluate(node => node.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }))); expect(state.writes).toHaveLength(1); state.release();
    await expect(form.getByRole('alert')).toContainText(en ? 'Your message was kept' : 'Conservamos tu mensaje'); await expect(input).toHaveValue('Solicitud sintética de ayuda');
    await submit.click(); await expect(form.getByRole('alert')).toContainText(en ? 'The issue was recorded.' : 'La incidencia quedó registrada.'); await expect(input).toHaveValue('');
    await expect(page.getByText(en ? 'Requests and issues' : 'Solicitudes e incidencias', { exact: true })).toBeVisible();
    await expect(page.getByText(en ? 'Payment: Paid' : 'Pago: Pagado', { exact: true })).toBeVisible(); await expect(page.getByText(en ? 'Refund: None' : 'Reembolso: Ninguno', { exact: true })).toBeVisible();
    expect(state.writes).toHaveLength(2); expect(state.writes[1]).toEqual(state.writes[0]);
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  });
  test(`unpaid cancellation requires a current named confirmation, ${language} @critical @mobile-flow`, async ({ page, baseURL }, info) => {
    test.setTimeout(60_000); const state = await fixture(page, baseURL, language, false);
    await page.goto('/tienda/orden/synthetic-order');
    const form = page.getByRole('form', { name: en ? 'Cancel unpaid order' : 'Cancelar orden sin pagar', exact: true });
    const input = form.getByRole('textbox', { name: en ? 'Reason for cancellation' : 'Motivo de cancelación', exact: true });
    const review = form.getByRole('button', { name: en ? 'Review cancellation' : 'Revisar cancelación', exact: true });
    await review.click(); await expect(input).toBeFocused(); await expect(input).toHaveAttribute('aria-invalid', 'true'); expect(state.writes).toEqual([]);
    await input.fill('x'.repeat(2001)); await form.evaluate(node => node.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }))); await expect(input).toHaveValue('x'.repeat(2001)); expect(state.writes).toEqual([]);
    await input.fill('  Motivo sintético para cancelar  '); await review.click();
    const dialog = page.getByRole('dialog', { name: en ? 'Cancel order SYNTHETIC-001?' : '¿Cancelar el pedido SYNTHETIC-001?', exact: true });
    const keep = dialog.getByRole('button', { name: en ? 'Keep order' : 'Conservar pedido', exact: true });
    const confirm = dialog.getByRole('button', { name: en ? 'Cancel unpaid order' : 'Cancelar orden sin pagar', exact: true });
    await expect(dialog).toContainText('Motivo sintético para cancelar'); await inspect(page, keep, dialog); await page.screenshot({ path: info.outputPath('merch-order-cancel-confirmation.png') }); expect(state.writes).toEqual([]);
    await keep.focus(); await page.keyboard.press('Enter'); await expect(dialog).not.toBeVisible(); await expect(input).toHaveValue('  Motivo sintético para cancelar  ');
    await review.click(); state.paid = true; await state.refresh(); await expect(confirm).toBeDisabled(); await expect(dialog).toContainText(en ? 'Check the current order status' : 'Revisa el estado actual');
    expect(state.writes).toEqual([]); await keep.click(); await expect(dialog).not.toBeVisible();
    state.paid = false; await state.refresh(); await expect(review).toBeEnabled(); await expect(input).toHaveValue('  Motivo sintético para cancelar  ');
    await review.click(); await confirm.click(); await expect(dialog).not.toBeVisible(); await expect(page.getByRole('alert').filter({ hasText: en ? 'Your reason was kept' : 'Conservamos tu motivo' })).toBeVisible();
    await expect(input).toHaveValue('  Motivo sintético para cancelar  '); await expect(review).toBeEnabled(); expect(state.writes).toHaveLength(1);
    await review.click(); expect(state.writes).toHaveLength(1); await confirm.click(); await expect.poll(() => state.writes.length).toBe(2);
    await expect(dialog.getByRole('button', { name: en ? 'Cancelling…' : 'Cancelando…', exact: true })).toBeDisabled(); await expect(keep).toBeDisabled();
    await page.keyboard.press('Escape'); await expect(dialog).toBeVisible();
    await page.locator('form').evaluateAll(nodes => nodes.forEach(node => node.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true })))); expect(state.writes).toHaveLength(2); state.release();
    await expect(dialog).not.toBeVisible(); await expect(page.getByRole('alert').filter({ hasText: en ? 'stock was released' : 'stock quedó liberado' })).toBeVisible();
    await expect(form).toHaveCount(0); expect(state.writes[1]).toEqual(state.writes[0]);
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  });
}
