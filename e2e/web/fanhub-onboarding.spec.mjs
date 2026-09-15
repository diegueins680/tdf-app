import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Synthetic API/browser integration, not real signup or server-persistence E2E.
async function fixture(page, baseURL, authenticated = true) {
  const origin = new URL(baseURL).origin;
  const state = { authenticated, failRead: false, failExit: false, malformedExit: false, reads: 0, exits: [], progress: {
    eligible: true, signupCompletedAt: '2026-09-15T00:00:00Z', onboardingIntent: 'follow_artists',
    completedAt: null, firstValue: null, firstValueCompletedAt: null, updatedAt: '2026-09-15T00:00:00Z',
  } };
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (url.origin !== origin) return route.abort('blockedbyclient');
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    const path = url.pathname;
    if (path === '/session') return state.authenticated
      ? route.fulfill({ json: { username: 'synthetic-fan', displayName: 'Cuenta Sintética', partyId: 42,
        roles: ['customer'], modules: [], featureFlags: [],
        preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' } } })
      : route.fulfill({ status: 401, json: { error: 'unauthenticated' } });
    if (path === '/session/onboarding') {
      state.reads++;
      if (state.failRead) {
        state.failRead = false;
        return route.fulfill({ status: 503, json: { error: 'synthetic read failure' } });
      }
      return route.fulfill({ json: state.progress });
    }
    if (path === '/session/onboarding/complete') {
      state.exits.push(request.postDataJSON());
      if (state.failExit) {
        state.failExit = false;
        return route.fulfill({ status: 503, json: { error: 'synthetic exit failure' } });
      }
      if (state.malformedExit) return route.fulfill({ json: { newlyCompleted: true } });
      state.progress = { ...state.progress, eligible: false, completedAt: '2026-09-15T00:05:00Z' };
      return route.fulfill({ json: { newlyCompleted: true, progress: state.progress } });
    }
    if (path === '/session/onboarding/reconcile') return route.fulfill({ json: { newlyCompleted: false, progress: state.progress } });
    if (['/fans/artists', '/fans/me/follows', '/fans/me/clubs'].includes(path)) return route.fulfill({ json: [] });
    if (path === '/records/feed') return route.fulfill({ json: { collections: [], releases: [], recordings: [], sessions: [] } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [] } });
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture for this API' } });
  });
  return state;
}

test('FanHub recovers reads and explicit keyboard exit without fake first value @critical', async ({ page, baseURL }, testInfo) => {
  const state = await fixture(page, baseURL);
  state.failRead = true;
  state.failExit = true;
  await page.goto('/fans', { waitUntil: 'domcontentloaded' });
  const loadError = page.getByRole('alert').filter({ hasText: 'No pudimos cargar tus primeros pasos.' });
  await expect(loadError).toBeVisible();
  expect(state.exits).toEqual([]);
  await expect(page.getByText('Primeros pasos', { exact: true })).toHaveCount(0);
  await loadError.getByRole('button', { name: 'Reintentar' }).click();
  await expect(page.getByText('Primeros pasos', { exact: true })).toBeVisible();
  const close = page.getByRole('button', { name: 'Cerrar primeros pasos', exact: true });
  await close.focus();
  await page.keyboard.press('Enter');
  const saveError = page.getByRole('alert').filter({ hasText: 'No pudimos guardar que terminaste estos primeros pasos.' });
  await expect(saveError).toBeVisible();
  await expect(page.getByText('Primeros pasos', { exact: true })).toBeVisible();
  expect(state.exits).toEqual([{}]);
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await globalThis.axe.run(document)).violations
    .filter(({ impact }) => ['serious', 'critical'].includes(impact)).map(({ id }) => id));
  await testInfo.attach('axe-recovery.json', { body: JSON.stringify(violations), contentType: 'application/json' });
  expect(violations).toEqual([]);
  await page.screenshot({ path: testInfo.outputPath('fanhub-exit-recovery.png'), fullPage: true });
  await saveError.getByRole('button', { name: 'Reintentar' }).focus();
  await page.keyboard.press('Enter');
  await expect(page.getByText('Primeros pasos', { exact: true })).toHaveCount(0);
  await expect(saveError).toHaveCount(0);
  expect(state.exits).toEqual([{}, {}]);
  expect(state.progress.firstValue).toBeNull();
  await page.reload({ waitUntil: 'domcontentloaded' });
  await expect(page.getByRole('heading', { name: 'Comunidad — Conecta con tus artistas' })).toBeVisible();
  await expect.poll(() => state.reads).toBeGreaterThanOrEqual(3);
  await expect(close).toHaveCount(0);
  expect(state.exits).toHaveLength(2);
});

test('FanHub does not show already-completed onboarding or submit another exit', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  state.progress = { ...state.progress, eligible: false, completedAt: '2026-09-15T00:05:00Z' };
  await page.goto('/fans', { waitUntil: 'domcontentloaded' });
  await expect(page.getByRole('heading', { name: 'Comunidad — Conecta con tus artistas' })).toBeVisible();
  await expect.poll(() => state.reads).toBe(1);
  await expect(page.getByRole('button', { name: 'Cerrar primeros pasos' })).toHaveCount(0);
  expect(state.exits).toEqual([]);
});

test('FanHub rejects a successful HTTP response with no authoritative progress', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  state.malformedExit = true;
  await page.goto('/fans', { waitUntil: 'domcontentloaded' });
  await page.getByRole('button', { name: 'Cerrar primeros pasos' }).click();
  await expect(page.getByText('No pudimos guardar que terminaste estos primeros pasos.', { exact: false })).toBeVisible();
  await expect(page.getByText('Primeros pasos', { exact: true })).toBeVisible();
  expect(state.exits).toEqual([{}]);
});

test('FanHub guest English tips are local presentation, never account completion', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL, false);
  await page.addInitScript(() => localStorage.setItem('tdf-hq-ui/locale', 'en'));
  await page.goto('/fans', { waitUntil: 'domcontentloaded' });
  await expect(page.getByText('First steps', { exact: true })).toBeVisible();
  await page.getByRole('button', { name: 'Close first steps' }).click();
  await expect(page.getByText('First steps', { exact: true })).toHaveCount(0);
  expect(state.reads).toBe(0);
  expect(state.exits).toEqual([]);
});
