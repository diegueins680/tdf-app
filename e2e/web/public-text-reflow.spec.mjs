import { expect, test } from '@playwright/test';

// Synthetic read-only responses keep this layout regression independent of
// provider/network availability. Real Firefox font preferences are checked in
// the versioned audit receipt; this portable case changes the same root font.
for (const language of ['es', 'en']) {
  for (const path of ['/', '/comercio', '/distribucion', '/marketplace']) {
    test(`Public text resize preserves header and content ${language} ${path} @critical`, async ({ page, baseURL }) => {
      await page.setViewportSize({ width: 320, height: 900 });
      await page.addInitScript(locale => localStorage.setItem('tdf-hq-ui/locale', locale), language);
      const origin = new URL(baseURL).origin;
      await page.route('**/*', async route => {
        const request = route.request();
        const url = new URL(request.url());
        if (!['fetch', 'xhr'].includes(request.resourceType())) {
          return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
        }
        const apiPath = url.pathname.replace(/^\/api(?=\/)/, '');
        if (apiPath === '/session') return route.fulfill({ status: 401, json: { error: 'unauthenticated' } });
        if (apiPath === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
        if (apiPath === '/directory/search') return route.fulfill({ json: {
          items: [], sponsoredItems: [], facets: { entityTypes: {}, cities: [], total: 0 }, nextCursor: null,
        } });
        if (apiPath === '/directory/taxonomies') return route.fulfill({ json: {
          locale: language, professions: [], classifiedCategories: [], compensationTypes: [],
          serviceOfferings: [], currencies: [], instruments: [], genres: [], cities: [],
        } });
        if (apiPath.startsWith('/catalogs/')) return route.fulfill({ json: {} });
        return route.fulfill({ json: [] });
      });
      await page.goto(path);
      await expect(page.locator('html')).toHaveAttribute('lang', language);
      await expect(page.getByRole('heading', { level: 1 })).toBeVisible();
      await page.evaluate(() => { document.documentElement.style.fontSize = '200%'; });
      const fits = () => page.evaluate(() => document.documentElement.scrollWidth <= innerWidth);
      await expect.poll(fits).toBe(true);
      const more = page.getByRole('banner').getByRole('button').last();
      await more.focus();
      await expect(more).toBeFocused();
      const bounds = await more.boundingBox();
      expect(bounds.x).toBeGreaterThanOrEqual(0);
      expect(bounds.x + bounds.width).toBeLessThanOrEqual(320);
      await page.keyboard.press('Enter');
      await expect(page.getByRole('menu')).toBeVisible();
      await page.keyboard.press('Escape');
      await expect(more).toBeFocused();
      if (path === '/marketplace') {
        const sort = page.getByRole('combobox', { name: /Ordenar por/ });
        await sort.click();
        const option = page.getByRole('option', { name: 'Relevancia (disponible primero)', exact: true });
        await expect(option).toBeVisible();
        expect(await option.evaluate(element => element.scrollWidth <= element.clientWidth)).toBe(true);
        await page.keyboard.press('Escape');
      }
      await expect.poll(fits).toBe(true);
    });
  }
}
