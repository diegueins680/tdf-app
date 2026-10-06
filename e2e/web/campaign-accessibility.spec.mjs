import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// The campaign tracker is local-only. Shell responses are synthetic; no campaigns,
// invitations, budgets or provider settings are changed by this regression.
for (const width of [320, 1280]) {
  test(`Campaign controls expose row and column names at ${width}px @critical`, async ({ page, baseURL }) => {
    await page.setViewportSize({ width, height: 900 });
    const origin = new URL(baseURL).origin;
    await page.route('**/*', async (route) => {
      const request = route.request();
      const url = new URL(request.url());
      if (!['fetch', 'xhr'].includes(request.resourceType())) {
        return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
      }
      const path = url.pathname.replace(/^\/api(?=\/)/, '');
      const preferences = { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' };
      if (path === '/session') return route.fulfill({ json: {
        username: 'campaign-audit', displayName: 'Operador sintético', partyId: 42,
        roles: ['Admin'], modules: ['scheduling'], featureFlags: [], preferences,
      } });
      if (path === '/session/preferences') return route.fulfill({ json: preferences });
      if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
      if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
      return route.fulfill({ json: [] });
    });
    await page.goto('/estudio/campanas/tdf-sessions-domo');
    for (const tab of ['Control', 'Calendario', 'Creativos', 'Bandas', 'Copys']) {
      await page.getByRole('tab', { name: tab, exact: true }).click();
      if (tab === 'Calendario') {
        await expect(page.getByRole('progressbar', { name: 'Avance del calendario de campaña' })).toBeVisible();
      }
      if (tab === 'Creativos') {
        await expect(page.getByRole('progressbar', { name: 'Videos de campaña publicados' })).toBeVisible();
        const region = page.getByRole('region', { name: 'Seguimiento de creativos', exact: true });
        const rows = region.getByRole('row');
        expect(await rows.count()).toBeGreaterThan(1);
        for (const row of await rows.all()) {
          const status = row.getByRole('combobox');
          if (!(await status.count())) continue;
          const label = (await row.locator('[id$="-label"]').innerText()).replace(/\s+/g, ' ').trim();
          await expect(status).toHaveAccessibleName(`${label} Estado ${await status.innerText()}`);
          await expect(row.getByRole('textbox')).toHaveAccessibleName(`${label} Notas`);
        }
        await region.focus();
        await page.keyboard.press('Tab');
        await expect(region.getByRole('combobox').first()).toBeFocused();
        const focusIsVisible = () => page.evaluate(() => {
          const element = document.activeElement;
          const bounds = element.getBoundingClientRect();
          const hit = document.elementFromPoint(bounds.x + bounds.width / 2, bounds.y + bounds.height / 2);
          return hit === element || element.contains(hit);
        });
        await expect.poll(focusIsVisible).toBe(true);
        await page.keyboard.press('Enter');
        await expect(page.getByRole('listbox')).toBeVisible();
        await page.keyboard.press('Escape');
        await expect(region.getByRole('combobox').first()).toBeFocused();
        await page.keyboard.press('Tab');
        await expect(region.getByRole('textbox').first()).toBeFocused();
        await expect.poll(focusIsVisible).toBe(true);
      }
      await page.addScriptTag({ content: axe.source });
      const violations = await page.evaluate(async () => (await globalThis.axe.run(document, {
        runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
      })).violations.map(({ id, nodes }) => ({ id, targets: nodes.map(({ target }) => target) })));
      expect(violations, tab).toEqual([]);
      expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), tab).toBe(true);
    }
  });
}
