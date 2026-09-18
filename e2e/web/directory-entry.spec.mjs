import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Browser/production-bundle conformance using synthetic read-only API fixtures.
// No request or mutation reaches a production service.
for (const denial of ['getter', 'getItem', 'setItem']) {
  test(`Directory remains searchable when city storage denies ${denial} @critical`, async ({ page, baseURL }) => {
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.addInitScript(operation => {
      localStorage.setItem('tdf.directory.cityId', 'old-city');
      const deny = () => { throw new DOMException('Optional preference denied', 'SecurityError'); };
      if (operation === 'getter') Object.defineProperty(window, 'localStorage', { configurable: true, get: deny });
      else Storage.prototype[operation] = deny;
    }, denial);
    const searches = [];
    const origin = new URL(baseURL).origin;
    await page.route('**/*', async route => {
      const request = route.request(), url = new URL(request.url());
      if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
      const path = url.pathname.replace(/^\/api(?=\/)/, '');
      if (path === '/session') return route.fulfill({ status: 401, json: { error: 'unauthenticated' } });
      if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
      if (path === '/directory/taxonomies') return route.fulfill({ json: {
        locale: 'es', professions: [], classifiedCategories: [], compensationTypes: [],
        serviceOfferings: [], currencies: [], instruments: [], genres: [],
        cities: [{ id: 'quito-fixture', code: 'quito-ec-p', name: 'Quito' }, { id: 'cuenca-fixture', code: 'cuenca-ec-p', name: 'Cuenca' }],
      } });
      if (path === '/directory/search') {
        searches.push(Object.fromEntries(url.searchParams));
        return route.fulfill({ json: { items: [], sponsoredItems: [], facets: { entityTypes: {}, cities: [], total: 0 }, nextCursor: null } });
      }
      if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [] } });
      return route.fulfill({ json: [] });
    });
    await page.goto('/buscar?q=music&cityId=cuenca-fixture');
    await expect(page.getByRole('heading', { name: 'Encuentra a la gente y las oportunidades que hacen música' })).toBeVisible();
    await expect(page.getByRole('combobox', { name: 'Ciudad', exact: true })).toHaveText('Cuenca');
    const search = page.getByRole('combobox', { name: '¿Qué necesitas?' });
    await expect(search).toHaveValue('music');
    await search.fill('concierto');
    await search.press('Enter');
    await expect.poll(() => searches.some(query => query.q === 'concierto' && query.cityId === 'cuenca-fixture')).toBe(true);
    await page.reload();
    await expect(search).toHaveValue('concierto');
    await expect(page.getByRole('combobox', { name: 'Ciudad', exact: true })).toHaveText('Cuenca');
    await page.addScriptTag({ content: axe.source });
    const violations = await page.evaluate(async () => (await window.axe.run(document)).violations.filter(v => ['serious', 'critical'].includes(v.impact)).map(v => v.id));
    expect(violations).toEqual([]);
    expect(errors).toEqual([]);
  });
}
