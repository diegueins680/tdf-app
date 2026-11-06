import { expect, test } from '@playwright/test';
import axe from 'axe-core';
import { readFile } from 'node:fs/promises';
import { inspectCampaignTabs, recordCampaignAccessibilityFailure } from './helpers/campaign-accessibility.mjs';

// The campaign tracker is local-only. Shell responses are synthetic; no campaigns,
// invitations, budgets or provider settings are changed by this regression.
for (const width of [320, 1280]) {
  test(`Campaign controls expose row and column names at ${width}px @critical`, async ({ page, baseURL }, testInfo) => {
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
      const before = await page.evaluate(inspectCampaignTabs);
      const fullViolations = await page.evaluate(async () => (await globalThis.axe.run(document, {
        runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
      })).violations);
      await recordCampaignAccessibilityFailure(page, testInfo, tab, fullViolations, before);
      const violations = fullViolations.map(({ id, nodes }) => ({ id, targets: nodes.map(({ target }) => target) }));
      expect(violations, tab).toEqual([]);
      expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), tab).toBe(true);
    }
  });
}

// Exercise the real axe engine and Playwright attachment, including an actually
// clipped unselected tab. This control does not assert a cause for hosted WebKit.
test('Campaign diagnostics retain contrast counterexamples and clipped-tab geometry @critical', async ({ page }, testInfo) => {
  await page.setViewportSize({ width: 320, height: 900 });
  await page.setContent(`<!doctype html><html lang="es"><head><title>Campaign evidence control</title>
    <style>body { background: white; color: black; }
    .scroller { width: 120px; overflow: hidden; }
    [role=tablist] { display: flex; width: max-content; }
    [role=tab] { width: 200px; flex-shrink: 0; height: 40px; }
    #contrast-control { color: #eee; font-size: 16px; }</style></head>
    <body><main><p id="contrast-control">Known bad contrast</p>
    <div class="scroller"><div role="tablist" aria-label="Campaign control">
    <button role="tab" aria-selected="true">Control</button>
    <button role="tab" aria-selected="false">Creativos</button>
    </div></div></main></body></html>`);
  await page.addScriptTag({ content: axe.source });
  const before = await page.evaluate(inspectCampaignTabs);
  const violations = await page.evaluate(async () => (await globalThis.axe.run(document, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations);
  expect(violations.some(({ id }) => id === 'color-contrast')).toBe(true);
  const attachmentCount = testInfo.attachments.length;
  await recordCampaignAccessibilityFailure(page, testInfo, 'negative-control', violations, before);
  expect(testInfo.attachments.length).toBe(attachmentCount + 1);
  const attachment = testInfo.attachments.at(-1);
  expect(attachment.contentType).toBe('application/json');
  const evidence = JSON.parse(attachment.body?.toString() ?? await readFile(attachment.path, 'utf8'));
  const contrast = evidence.violations.find(({ id }) => id === 'color-contrast');
  expect(contrast.nodes).toEqual(expect.any(Array));
  const node = contrast.nodes.find(({ target }) => target.includes('#contrast-control'));
  expect(node).toMatchObject({ any: expect.any(Array), failureSummary: expect.any(String) });
  const check = node.any.find(({ id }) => id === 'color-contrast');
  expect(check.data.contrastRatio).toBeLessThan(4.5);
  expect(check.data.fgColor).toBeTruthy();
  expect(check.data.bgColor).toBeTruthy();
  expect(node.failureSummary).toContain('contrast');
  expect(evidence.violations).toEqual(violations);
  for (const sample of [evidence.before, evidence.after]) {
    const clipped = sample.tabs.find(({ text }) => text === 'Creativos');
    expect(clipped.selected).toBe('false');
    expect(clipped.bounds.width).toBeGreaterThan(0);
    expect(clipped.clipIntersection.width).toBe(0);
    expect(clipped.ancestors.some(({ overflowX }) => overflowX === 'hidden')).toBe(true);
    expect(clipped.ancestors[0].color).toBeTruthy();
    expect(sample.sampledAt).toBeGreaterThan(0);
  }
});
