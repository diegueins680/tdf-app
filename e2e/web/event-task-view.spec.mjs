import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Synthetic browser/API fixtures, not real server authorization or database E2E.
async function fixture(page, baseURL, locale = 'es') {
  const origin = new URL(baseURL).origin;
  const state = { status: 200, reads: 0, parentReads: [], task: {
    eventId: 80, activityId: 8000, status: 'planned', version: 1,
    policy: { requiresAccountability: true, dependenciesGateCompletion: true, version: 2 },
    raci: [{ partyId: 11, role: 'accountable' }, { partyId: 12, role: 'responsible' }],
    accountabilityNeedsAttention: false,
  } };
  await page.addInitScript(value => localStorage.setItem('tdf-hq-ui/locale', value), locale);
  await page.route('**/*', async route => {
    const request = route.request(); const url = new URL(request.url());
    if (url.origin !== origin) return route.abort('blockedbyclient');
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    if (url.pathname === '/session') return route.fulfill({ json: {
      username: 'synthetic-task-reader', displayName: 'Lector Sintético', partyId: 42,
      roles: ['customer'], modules: [], featureFlags: [],
      preferences: { locale, currency: 'USD', timeZone: 'America/Guayaquil' },
    } });
    if (url.pathname === '/event-operations/events/80/tasks/8000') {
      state.reads++;
      expect(request.method()).toBe('GET');
      return route.fulfill({ status: state.status, json: state.status === 200 ? state.task : { error: 'private diagnostic' } });
    }
    if (url.pathname.startsWith('/social-events/') || url.pathname.startsWith('/parties/')) state.parentReads.push(url.pathname);
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture' } });
  });
  return state;
}

test('task-only route renders RACI and recovers keyboard refresh without parent reads @critical', async ({ page, baseURL }, testInfo) => {
  const state = await fixture(page, baseURL);
  await page.goto('/social/eventos/80?tarea=8000');
  const table = page.getByRole('table', { name: 'Asignaciones RACI' });
  await expect(table).toBeVisible();
  await expect(table.getByText('Persona #11')).toBeVisible();
  expect(state.parentReads).toEqual([]);
  // Development StrictMode starts an aborted read before the mounted read.
  // The aborted request may or may not reach the route interceptor.
  const initialReads = state.reads;
  expect([1, 2]).toContain(initialReads);
  state.status = 403;
  await page.getByRole('button', { name: 'Actualizar', exact: true }).focus();
  await page.keyboard.press('Enter');
  await expect(page.getByRole('alert').filter({ hasText: 'No pudimos consultar esta tarea' })).toBeVisible();
  await expect(table).toHaveCount(0);
  await expect(page.getByText('private diagnostic')).toHaveCount(0);
  expect(state.reads).toBe(initialReads + 1);
  state.status = 200;
  await page.getByRole('button', { name: 'Reintentar', exact: true }).focus();
  await page.keyboard.press('Enter');
  await expect(table).toBeVisible();
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await globalThis.axe.run(document)).violations
    .filter(({ impact }) => ['serious', 'critical'].includes(impact)).map(({ id }) => id));
  expect(violations).toEqual([]);
  await page.screenshot({ path: testInfo.outputPath('event-task-raci.png'), fullPage: true });
  expect(state.reads).toBe(initialReads + 2); expect(state.parentReads).toEqual([]);
});

test('malformed successful task cannot produce a RACI view', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL); state.task.activityId = 9000;
  await page.goto('/social/eventos/80?tarea=8000');
  await expect(page.getByRole('alert').filter({ hasText: 'No pudimos consultar esta tarea' })).toBeVisible();
  await expect(page.getByRole('table')).toHaveCount(0);
  expect(state.parentReads).toEqual([]);
});

test('invalid or repeated task selectors never fall back to loading the event', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  for (const query of ['tarea=', 'tarea=8000&tarea=9000', 'tarea=1e3']) {
    await page.goto(`/social/eventos/80?${query}`);
    await expect(page.getByRole('alert').filter({ hasText: 'El enlace de la tarea no es válido' })).toBeVisible();
  }
  expect(state.reads).toBe(0); expect(state.parentReads).toEqual([]);
});

test('English task view honestly reports unconfigured responsibility', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL, 'en');
  delete state.task.policy; state.task.raci = [];
  await page.goto('/social/eventos/80?tarea=8000');
  await expect(page.getByText('No responsibility policy is recorded.')).toBeVisible();
  await expect(page.getByText('No RACI assignments are recorded.')).toBeVisible();
  expect(state.parentReads).toEqual([]);
});
