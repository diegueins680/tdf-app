import { expect, test } from '@playwright/test';
import axe from 'axe-core';

for (const language of ['es', 'en']) {
  test(`stock draft survives background refresh and requires review, ${language} @critical @mobile-flow`, async ({ page, baseURL }, info) => {
    test.setTimeout(60_000);
    const en = language === 'en', origin = new URL(baseURL).origin, writes = [];
    let version = 7, stockOnHand = 12, reorderThreshold = 3, reads = 0;
    const variant = () => ({ id: 'synthetic-variant', sku: 'SYNTHETIC', name: 'Variante sintética', version, stockOnHand,
      reorderThreshold, active: true, stockReserved: 2, stockSold: 1, stockMode: 'finite', priceMinor: 2500, currency: 'USD' });
    await page.addInitScript(locale => localStorage.setItem('tdf-hq-ui/locale', locale), language);
    let now = Date.now(); await page.clock.setFixedTime(now);
    await page.route('**/*', async route => {
      const request = route.request(), url = new URL(request.url());
      if (!['xhr', 'fetch'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
      const path = url.pathname.replace(/^\/api(?=\/)/, '');
      if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path) || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
      if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
      if (request.method() !== 'GET') {
        expect(path).toBe('/merch/seller/stores/synthetic-store/variants/synthetic-variant/stock'); expect(request.method()).toBe('PATCH');
        const body = request.postDataJSON(); writes.push(body);
        if (writes.length === 1) {
          expect(body).toEqual({ stockOnHand: 9, reorderThreshold: 0, active: false, version: 8 });
          version = 9; stockOnHand = 18; reorderThreshold = 5;
          return route.fulfill({ status: 409, json: { error: 'Synthetic concurrent update' } });
        }
        expect(writes).toHaveLength(2); expect(body).toEqual({ stockOnHand: 17, reorderThreshold: 5, active: true, version: 9 });
        version = 10; stockOnHand = 17; return route.fulfill({ json: variant() });
      }
      const preferences = { locale: language, currency: 'USD', timezone: 'America/Guayaquil' };
      if (path === '/session') return route.fulfill({ json: { username: 'synthetic-admin', partyId: 42, roles: ['Admin'], modules: ['admin'], featureFlags: [], preferences } });
      if (path === '/session/preferences') return route.fulfill({ json: preferences });
      if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
      if (path === '/merch/capabilities') return route.fulfill({ json: { features: { storefronts: true } } });
      if (path === '/merch/seller/stores') return route.fulfill({ json: [{ id: 'synthetic-store', slug: 'synthetic-store', displayName: 'Tienda sintética', operationalStatus: 'active', permissions: { catalog: true, stock: true } }] });
      if (path === '/merch/seller/stores/synthetic-store/products') {
        reads += 1;
        return route.fulfill({ json: [{ id: 'synthetic-product', storeId: 'synthetic-store', name: 'Producto sintético', slug: 'synthetic-product', category: 'limited_edition', status: 'draft', visibility: 'public', availabilityMode: 'in_stock', version: 1, variants: [variant()] }] });
      }
      if (path.includes('/reputation')) return route.fulfill({ json: null });
      if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], catalogs: [] } });
      return route.fulfill({ json: [] });
    });
    const refresh = async () => {
      const before = reads; now += 31_000; await page.clock.setFixedTime(now);
      // Exercise the application's real refetchOnWindowFocus path after staleTime.
      await page.evaluate(() => window.dispatchEvent(new Event('visibilitychange')));
      await expect.poll(() => reads).toBeGreaterThan(before);
    };
    await page.goto('/mi-tienda');
    const form = page.getByRole('form', { name: en ? 'Stock for Variante sintética' : 'Stock de Variante sintética' });
    const stock = form.getByRole('textbox', { name: en ? 'On hand' : 'Existencia', exact: true });
    const threshold = form.getByRole('textbox', { name: en ? 'Low-stock alert' : 'Alerta de stock', exact: true });
    const active = form.getByRole('checkbox'), update = form.getByRole('button', { name: en ? 'Update stock' : 'Actualizar stock', exact: true });
    const keep = form.getByRole('button', { name: en ? 'Keep my edits' : 'Conservar mi edición', exact: true });
    const current = form.getByRole('button', { name: en ? 'Use current values' : 'Usar valores actuales', exact: true });
    await expect(stock).toHaveValue('12'); await stock.fill('09'); await threshold.fill(''); await active.uncheck();
    version = 8; stockOnHand = 15; reorderThreshold = 4; await refresh();
    await expect(form.getByRole('alert')).toContainText(en ? 'on hand 15' : 'existencia 15');
    await expect(stock).toHaveValue('09'); await expect(threshold).toHaveValue(''); await expect(active).not.toBeChecked(); await expect(update).toBeDisabled();
    expect(writes).toEqual([]);
    await expect(async () => {
      await keep.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
      await expect(keep).toBeInViewport({ ratio: 1, timeout: 500 });
      expect(await keep.evaluate(node => { const r = node.getBoundingClientRect(); return node.contains(document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)); })).toBe(true);
    }).toPass({ timeout: 8000 });
    await expect.poll(() => form.evaluate(node => node.getAnimations({ subtree: true }).filter(a => a.playState === 'running').length)).toBe(0);
    await page.addScriptTag({ content: axe.source });
    expect(await form.evaluate(async node => (await globalThis.axe.run(node, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } })).violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.failureSummary) })))).toEqual([]);
    await page.screenshot({ path: info.outputPath('merch-stock-background-review.png') });
    await keep.focus(); await page.keyboard.press('Enter'); await expect(stock).toBeFocused();
    await expect(stock).toHaveValue('09'); await expect(threshold).toHaveValue(''); await expect(update).toBeDisabled(); expect(writes).toEqual([]);
    await threshold.fill('0'); await update.click();
    await expect(current).toBeVisible(); await expect(stock).toHaveValue('09'); await expect(active).not.toBeChecked(); await expect(update).toBeDisabled();
    await expect(form).toContainText(en ? 'on hand 18' : 'existencia 18'); expect(writes).toHaveLength(1);
    await current.click(); await expect(stock).toBeFocused(); await expect(stock).toHaveValue('18'); await expect(threshold).toHaveValue('5'); await expect(active).toBeChecked(); expect(writes).toHaveLength(1);
    await stock.fill('17'); await update.click(); await expect.poll(() => writes.length).toBe(2); await expect(stock).toBeEnabled(); await expect(stock).toHaveValue('17');
    version = 11; stockOnHand = 22; reorderThreshold = 6; await refresh();
    await expect(stock).toHaveValue('22'); await expect(threshold).toHaveValue('6'); await expect(form.getByRole('alert')).toHaveCount(0); expect(writes).toHaveLength(2);
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  });
}
