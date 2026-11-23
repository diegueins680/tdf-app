import { expect, test } from '@playwright/test';
import axe from 'axe-core';

async function fixture(page, baseURL, language) {
  const origin = new URL(baseURL).origin, writes = [], reads = [];
  await page.addInitScript(locale => localStorage.setItem('tdf-hq-ui/locale', locale), language);
  let draftAttempts = 0, stockAttempts = 0, version = 7, stock = 12, threshold = 3;
  const product = () => ({ id: 'synthetic-product', storeId: 'synthetic-store', storeName: 'Tienda sintética', slug: 'synthetic-product', name: 'Producto sintético', description: 'Producto sintético sin venta real.', category: 'limited_edition', status: 'draft', visibility: 'public', availabilityMode: 'in_stock', version: 2,
    variants: [{ id: 'synthetic-variant', sku: 'SKU-SYNTHETIC', name: 'Variante sintética', version, stockOnHand: stock, stockReserved: 2, stockSold: 1, reorderThreshold: threshold, active: true, stockMode: 'finite', weightGrams: 200, priceMinor: 2550, currency: 'USD', available: true }], images: [], related: [] });
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path) || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      const body = request.postDataJSON(), key = request.headers()['idempotency-key']; writes.push({ path, method: request.method(), body, key });
      if (path === '/merch/seller/stores/synthetic-store/products') {
        expect(request.method()).toBe('POST'); expect(body.category).toBe('limited_edition'); expect(key).toBeTruthy();
        expect(body.variants[0]).toMatchObject({ priceMinor: 2550, weightGrams: 100000, stockOnHand: 0 });
        draftAttempts += 1;
        if (draftAttempts === 1) return route.fulfill({ status: 422, json: { error: 'Synthetic draft validation rejection' } });
        return route.fulfill({ json: product() });
      }
      expect(path).toBe('/merch/seller/stores/synthetic-store/variants/synthetic-variant/stock'); expect(request.method()).toBe('PATCH');
      stockAttempts += 1;
      expect(body).toEqual({ stockOnHand: 9, reorderThreshold: 0, active: true, version: stockAttempts === 1 ? 7 : 8 });
      if (stockAttempts === 1) { version = 8; return route.fulfill({ status: 409, json: { error: 'Synthetic version conflict' } }); }
      version = 9; stock = 9; threshold = 0; return route.fulfill({ json: product().variants[0] });
    }
    reads.push(url);
    const preferences = { locale: language, currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'synthetic-admin', partyId: 42, roles: ['Admin'], modules: ['admin'], featureFlags: [], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/merch/capabilities') return route.fulfill({ json: { features: { storefronts: true, publicCatalog: true, refundOperations: false } } });
    if (path === '/merch/seller/stores') return route.fulfill({ json: [{ id: 'synthetic-store', slug: 'synthetic-store', displayName: 'Tienda sintética', operationalStatus: 'active', currency: 'USD', permissions: { catalog: true, stock: true } }] });
    if (path === '/merch/seller/stores/synthetic-store/products' || path === '/merch/admin/products') return route.fulfill({ json: [product()] });
    if (path === '/merch/storefronts/synthetic-store/products/synthetic-product') return route.fulfill({ json: product() });
    if (path.includes('/reputation')) return route.fulfill({ json: null });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], catalogs: [] } });
    return route.fulfill({ json: [] });
  });
  return { writes, reads };
}

async function inspect(page, field) {
  await expect(field).toHaveAttribute('inputmode', 'numeric'); await expect(field).toHaveAttribute('aria-invalid', 'true');
  const helper = await field.getAttribute('aria-describedby'); expect(helper).toBeTruthy(); await expect(page.locator(`[id="${helper}"]`)).not.toBeEmpty();
  await expect(async () => {
    await field.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
    await expect(field).toBeInViewport({ ratio: 1, timeout: 500 });
    expect(await field.evaluate(node => { const r = node.getBoundingClientRect(); return node.contains(document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)); })).toBe(true);
  }).toPass({ timeout: 8000 });
  await expect.poll(() => field.evaluate(node => node.closest('.MuiFormControl-root').getAnimations({ subtree: true }).filter(a => a.playState === 'running').length)).toBe(0);
  await page.addScriptTag({ content: axe.source });
  expect(await field.evaluate(async node => (await globalThis.axe.run(node.closest('.MuiFormControl-root'), { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
}

// DIAGNOSTIC ONLY (not for merge): repeat the English journey and dump page state when the Stock field is missing.
for (const [language, round] of [['es', 0], ...Array.from({ length: 12 }, (_, i) => ['en', i])]) {
  const en = language === 'en', category = en ? 'Category' : 'Categoría', limited = en ? 'Limited edition' : 'Edición limitada';
  test(`merch stock edits stay explicit and versioned, ${language} #${round} @critical @mobile-flow`, async ({ page, baseURL }, info) => {
    // Draft rejection/retry plus a stock conflict, reload and retry are one journey.
    test.setTimeout(60_000);
    const { writes } = await fixture(page, baseURL, language);
    await page.goto('/mi-tienda');
    const draft = page.locator('form').filter({ has: page.getByRole('heading', { name: en ? 'New product draft' : 'Nuevo borrador de producto', exact: true }) });
    await draft.getByRole('textbox', { name: en ? 'Product name' : 'Nombre del producto', exact: true }).fill('Producto sintético');
    await draft.getByRole('textbox', { name: 'Slug', exact: true }).fill('synthetic-product');
    await draft.getByRole('textbox', { name: en ? 'Description' : 'Descripción', exact: true }).fill('Descripción sintética');
    await draft.getByRole('textbox', { name: 'SKU', exact: true }).fill('SKU-SYNTHETIC-NEW');
    await draft.getByRole('textbox', { name: en ? 'Price (USD)' : 'Precio (USD)', exact: true }).fill('25,50');
    await draft.getByRole('combobox', { name: new RegExp('^'+category) }).click(); await page.getByRole('option', { name: limited, exact: true }).click();
    const quantity = draft.getByRole('textbox', { name: 'Stock', exact: true }), weight = draft.getByRole('textbox', { name: en ? 'Weight (grams)' : 'Peso (gramos)', exact: true });
    const save = draft.getByRole('button', { name: en ? 'Save draft' : 'Guardar borrador', exact: true });
    await quantity.waitFor({ timeout: 12000 }).catch(async error => {
      const state = await page.evaluate(() => ({
        htmlLang: document.documentElement.lang, storedLocale: localStorage.getItem('tdf-hq-ui/locale'),
        headings: [...document.querySelectorAll('h1,h2,h3')].map(h => h.textContent),
        listboxes: document.querySelectorAll('[role=listbox]').length, ariaHiddenRoot: document.getElementById('root')?.getAttribute('aria-hidden'),
        labels: [...document.querySelectorAll('form label')].map(l => l.textContent), active: document.activeElement?.outerHTML.slice(0, 200),
        categoryText: [...document.querySelectorAll('form [role=combobox]')].map(c => c.textContent),
      })).catch(e => ({ evaluateFailed: String(e) }));
      console.log('MERCH-DIAG ' + info.project.name + ' ' + JSON.stringify(state));
      throw error;
    });
    await quantity.fill(''); await expect(save).toBeDisabled(); await expect(quantity).toHaveValue(''); await inspect(page, quantity);
    await page.screenshot({ path: info.outputPath('merch-stock-inline-error.png') });
    await quantity.fill('21474836470'); await expect(save).toBeDisabled(); await expect(quantity).toHaveValue('21474836470');
    await quantity.fill('0'); await weight.fill('1000001'); await expect(weight).toHaveValue('1000001'); await expect(save).toBeDisabled();
    expect(writes).toEqual([]); await weight.fill('100000'); await save.click();
    await expect(draft.getByRole('alert')).toBeVisible(); await expect(weight).toHaveValue('100000'); await expect(quantity).toHaveValue('0');
    await save.click(); await expect(draft.getByRole('textbox', { name: en ? 'Product name' : 'Nombre del producto', exact: true })).toHaveValue('');
    expect(writes).toHaveLength(2); expect(writes[0]).toEqual(writes[1]);
    const form = page.locator('form').filter({ has: page.getByRole('button', { name: en ? 'Update stock' : 'Actualizar stock', exact: true }) });
    const stock = form.getByRole('textbox', { name: en ? 'On hand' : 'Existencia', exact: true }), threshold = form.getByRole('textbox', { name: en ? 'Low-stock alert' : 'Alerta de stock', exact: true });
    const update = form.getByRole('button', { name: en ? 'Update stock' : 'Actualizar stock', exact: true });
    await stock.fill(''); await threshold.fill('1e3'); await expect(update).toBeDisabled(); await expect(stock).toHaveValue(''); await inspect(page, stock);
    expect(writes).toHaveLength(2); await stock.fill('9'); await threshold.fill('0'); await update.click();
    await expect(form.getByRole('alert')).toBeVisible(); await expect(stock).toHaveValue('9'); await expect(threshold).toHaveValue('0'); expect(writes).toHaveLength(3);
    await page.reload(); await expect(stock).toHaveValue('12'); await stock.fill('9'); await threshold.fill('0'); await update.click();
    await expect.poll(() => writes.length).toBe(4); await expect(stock).toHaveValue('9');
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  });
  test(`merch category labels retain raw filter codes, ${language} @critical @mobile-flow`, async ({ page, baseURL }, info) => {
    const { writes, reads } = await fixture(page, baseURL, language);
    await page.goto('/tiendas');
    await page.getByRole('combobox', { name: new RegExp('^'+category) }).click(); await page.getByRole('option', { name: en ? 'Bundle' : 'Paquete', exact: true }).click();
    await expect.poll(() => reads.some(url => url.pathname.endsWith('/merch/storefronts') && url.searchParams.get('category') === 'bundle')).toBe(true);
    await page.goto('/tienda/synthetic-store/producto/synthetic-product');
    await expect(page.getByText(limited, { exact: true })).toBeVisible();
    await page.screenshot({ path: info.outputPath('merch-category-label.png') });
    await page.goto('/admin/merch'); await expect(page.getByText(`Tienda sintética · ${limited}`, { exact: true })).toBeVisible();
    expect(writes).toEqual([]);
  });
}
