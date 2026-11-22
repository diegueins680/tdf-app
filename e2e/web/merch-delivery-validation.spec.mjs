import { expect, test } from '@playwright/test';
import axe from 'axe-core';

for (const language of ['es', 'en']) test(`merch delivery errors and checkout guards, ${language} @critical @mobile-flow`, async ({ page, baseURL }, info) => {
  test.setTimeout(60_000);
  const en = language === 'en', origin = new URL(baseURL).origin, writes = [], reads = { cart: 0, store: 0, capabilities: 0 };
  let enabled = true, available = true, hasZone = true, release;
  let now = Date.now(); await page.clock.setFixedTime(now);
  await page.addInitScript(locale => {
    localStorage.setItem('tdf-hq-ui/locale', locale);
    sessionStorage.setItem('tdf-merch-cart:synthetic-store', JSON.stringify({ id: 'synthetic-cart', token: 'synthetic-cart-capability' }));
  }, language);
  const order = { id: 'synthetic-order', orderNumber: 'SYNTHETIC-001', paymentStatus: 'pending', commercialStatus: 'created', fulfillmentStatus: 'pending', refundStatus: 'none', currency: 'USD', totalMinor: 3000, lines: [], timeline: [], issues: [] };
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['xhr', 'fetch'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path) || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      expect(path).toBe('/merch/carts/synthetic-cart/checkout'); expect(request.method()).toBe('POST');
      const body = request.postDataJSON();
      writes.push({ body, key: request.headers()['idempotency-key'], token: request.headers()['x-cart-lookup-token'] });
      expect(writes.at(-1).key).toBeTruthy(); expect(writes.at(-1).token).toBe('synthetic-cart-capability');
      expect(body).toEqual({ recipient: { name: 'Compradora sintética', email: 'buyer@example.test', phone: null, countryCode: 'EC', subdivision: null, city: 'Quito', addressLine1: 'Dirección sintética 123', addressLine2: null, postalCode: null, deliveryNote: 'Nota sintética' }, shippingZoneId: 'synthetic-zone', createAccount: false, locale: language });
      if (writes.length === 1) {
        await new Promise(resolve => { release = resolve; });
        return route.fulfill({ status: 503, json: { error: 'Rechazo sintético de checkout' } });
      }
      expect(writes).toHaveLength(2); return route.fulfill({ json: order });
    }
    const preferences = { locale: language, currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'buyer@example.test', displayName: 'Compradora sintética', partyId: 42, roles: ['Customer'], modules: [], featureFlags: [], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/merch/capabilities') { reads.capabilities += 1; return route.fulfill({ json: { features: { publicCatalog: true, checkout: enabled } } }); }
    if (path === '/merch/storefronts/synthetic-store') { reads.store += 1; return route.fulfill({ json: { id: 'synthetic-store', displayName: 'Tienda sintética', shippingZones: hasZone ? [{ id: 'synthetic-zone', name: 'Entrega sintética', rateMinor: 500 }] : [] } }); }
    if (path === '/merch/carts/synthetic-cart') { reads.cart += 1; return route.fulfill({ json: { id: 'synthetic-cart', storeId: 'synthetic-store', currency: 'USD', productSubtotalMinor: 2500, items: [{ variantId: 'synthetic-variant', productName: 'Producto sintético', variantName: 'Talla M', sku: 'SYNTHETIC', quantity: 1, subtotalMinor: 2500, available }] } }); }
    if (path === '/merch/orders/synthetic-order') { expect(request.headers()['x-order-lookup-token']).toBe('synthetic-cart-capability'); return route.fulfill({ json: order }); }
    if (path.includes('/reputation')) return route.fulfill({ json: null });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], catalogs: [] } });
    return route.fulfill({ json: [] });
  });
  const refresh = async () => {
    const before = { ...reads }; now += 31_000; await page.clock.setFixedTime(now);
    await page.evaluate(() => window.dispatchEvent(new Event('visibilitychange')));
    for (const key of Object.keys(before)) await expect.poll(() => reads[key]).toBeGreaterThan(before[key]);
  };
  await page.goto('/tienda/synthetic-store/carrito');
  const form = page.getByRole('form', { name: en ? 'Delivery details' : 'Datos de entrega' });
  const name = form.getByRole('textbox', { name: en ? 'Full name' : 'Nombre completo', exact: true }), email = form.getByRole('textbox', { name: 'Email', exact: true });
  const city = form.getByRole('textbox', { name: en ? 'City' : 'Ciudad', exact: true }), address = form.getByRole('textbox', { name: en ? 'Address' : 'Dirección', exact: true });
  const note = form.getByRole('textbox', { name: en ? 'Delivery note (optional)' : 'Nota de entrega (opcional)', exact: true });
  const submit = form.getByRole('button', { name: en ? 'Review and continue to payment' : 'Revisar y continuar al pago', exact: true });
  const directSubmit = () => form.evaluate(node => node.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true })));
  await expect(name).toHaveValue('Compradora sintética'); await expect(email).toHaveValue('buyer@example.test');
  await name.fill(''); await email.fill('not-an-email'); await submit.click(); await expect(name).toBeFocused();
  expect(writes).toEqual([]); await name.fill('Compradora sintética'); await submit.click(); await expect(email).toBeFocused();
  await expect(email).toHaveValue('not-an-email'); await expect(email).toHaveAttribute('aria-invalid', 'true');
  const helper = await email.getAttribute('aria-describedby'); expect(helper).toBeTruthy(); await expect(page.locator(`[id="${helper}"]`)).toHaveText(en ? 'Enter a valid email.' : 'Ingresa un correo válido.');
  await expect(async () => {
    await email.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
    await expect(email).toBeInViewport({ ratio: 1, timeout: 500 });
    expect(await email.evaluate(node => { const r = node.getBoundingClientRect(); return node.contains(document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)); })).toBe(true);
  }).toPass({ timeout: 8000 });
  await expect.poll(() => form.evaluate(node => node.getAnimations({ subtree: true }).filter(a => a.playState === 'running').length)).toBe(0);
  await page.addScriptTag({ content: axe.source });
  expect(await form.evaluate(async node => (await globalThis.axe.run(node, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
  await page.screenshot({ path: info.outputPath('merch-delivery-inline-errors.png') });
  await email.fill('buyer@example.test'); await city.fill('Quito'); await address.fill('Dirección sintética 123');
  await note.fill('Una línea\notra línea'); await submit.click(); await expect(note).toBeFocused(); await expect(note).toHaveAttribute('aria-invalid', 'true'); expect(writes).toEqual([]);
  await note.fill('Nota sintética');
  available = false; await refresh(); await expect(submit).toBeDisabled(); await directSubmit(); expect(writes).toEqual([]);
  available = true; enabled = false; await refresh(); await expect(submit).toBeDisabled(); await directSubmit(); expect(writes).toEqual([]);
  enabled = true; hasZone = false; await refresh(); await expect(submit).toBeDisabled(); await directSubmit(); expect(writes).toEqual([]);
  hasZone = true; await refresh(); await expect(submit).toBeEnabled(); await expect(name).toHaveValue('Compradora sintética'); await expect(note).toHaveValue('Nota sintética');
  await expect(form.getByText(en ? 'Estimated total' : 'Total estimado', { exact: true })).toBeVisible();
  await form.getByRole('checkbox').check(); await submit.click(); await expect.poll(() => writes.length).toBe(1);
  await expect(name).toBeDisabled(); await expect(email).toBeDisabled(); await expect(form.getByRole('checkbox')).toBeDisabled(); await expect(form.getByRole('combobox')).toBeDisabled();
  await expect(page.getByRole('button', { name: en ? 'Remove Producto sintético' : 'Quitar Producto sintético' })).toBeDisabled();
  await directSubmit(); expect(writes).toHaveLength(1); release();
  await expect(form.getByRole('alert')).toContainText('Rechazo sintético de checkout'); await expect(email).toHaveValue('buyer@example.test'); await expect(note).toHaveValue('Nota sintética');
  await submit.click(); await expect(page).toHaveURL(/\/tienda\/orden\/synthetic-order$/); expect(writes).toHaveLength(2); expect(writes[1]).toEqual(writes[0]);
  await expect(page.getByText(en ? 'Payment is pending verification. This order is not paid yet.' : 'El pago está pendiente de verificación. Esta orden aún no está pagada.', { exact: true })).toBeVisible();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
