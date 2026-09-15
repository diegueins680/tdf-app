import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Browser integration with synthetic API fixtures, NOT real-server/authentication E2E.
async function fixture(page, baseURL, authenticated) {
  const origin = new URL(baseURL).origin;
  const state = { authenticated, followed: false, failNextFollow: false, writes: [], external: [] };
  const artist = {
    apArtistId: 17, apDisplayName: 'Artista Sintética', apSlug: 'artista-sintetica',
    apBio: 'Perfil ficticio de verificación local.', apFollowerCount: 0,
    apHasUserAccount: true, apGenreIds: [], apCity: 'Quito',
  };
  const follow = { ffArtistId: 17, ffArtistName: artist.apDisplayName, ffStartedAt: '2026-09-14T12:00:00Z' };
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (url.origin !== origin) {
      state.external.push(url.origin);
      return route.abort('blockedbyclient');
    }
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    // Vite source modules are script resources, not API fixtures.
    const path = url.pathname;
    if (path === '/session') return state.authenticated
      ? route.fulfill({ json: { username: 'synthetic-42', displayName: 'Cuenta Sintética', partyId: 42,
        roles: ['customer'], modules: [], featureFlags: [],
        preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' } } })
      : route.fulfill({ status: 401, json: { error: 'unauthenticated' } });
    if (path === '/artists/artista-sintetica/public') return route.fulfill({ json: artist });
    if (path === '/fans/artists/17/releases') return route.fulfill({ json: [] });
    if (path === '/fans/me/follows' && request.method() === 'GET') return route.fulfill({ json: state.followed ? [follow] : [] });
    if (path === '/fans/me/follows/17' && request.method() === 'POST') {
      state.writes.push(path);
      if (state.failNextFollow) {
        state.failNextFollow = false;
        return route.fulfill({ status: 503, json: { error: 'synthetic failure' } });
      }
      state.followed = true;
      return route.fulfill({ json: follow });
    }
    if (path.startsWith('/session/onboarding/')) return route.fulfill({ json: {
      newlyCompleted: false, progress: { eligible: false, firstValue: null, completedAt: null },
    } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [] } });
    return route.fulfill({ status: 404, json: { error: 'No synthetic API fixture for this path' } });
  });
  return state;
}

test('artist follow guest return stays local and never mutates @critical', async ({ page, baseURL }, testInfo) => {
  const state = await fixture(page, baseURL, false);
  await page.goto('/a/artista-sintetica', { waitUntil: 'domcontentloaded' });
  const cta = page.getByRole('link', { name: 'Crear cuenta o ingresar para seguir', exact: true });
  await expect(cta).toBeVisible();
  const authUrl = new URL(await cta.getAttribute('href'), baseURL);
  expect(authUrl.origin).toBe(new URL(baseURL).origin);
  expect(authUrl.searchParams.get('redirect')).toBe('/a/artista-sintetica?resume=follow&artistId=17');
  expect(authUrl.searchParams.get('signup')).toBe('1');
  expect(state.writes).toEqual([]);
  await page.screenshot({ path: testInfo.outputPath('artist-follow-guest.png'), fullPage: true });
});

test('artist follow requires keyboard consent and keeps intent on failure @critical', async ({ page, baseURL }, testInfo) => {
  const state = await fixture(page, baseURL, true);
  state.failNextFollow = true;
  await page.goto('/a/artista-sintetica?source=event&resume=follow&artistId=17#artist-hero', { waitUntil: 'domcontentloaded' });
  const confirm = page.getByRole('button', { name: 'Seguir ahora', exact: true });
  await expect(confirm).toBeEnabled();
  expect(state.writes).toEqual([]);
  await confirm.focus();
  await page.keyboard.press('Enter');
  await expect(page.getByRole('alert').filter({ hasText: 'No pudimos seguir a Artista Sintética' })).toBeVisible();
  expect(new URL(page.url()).searchParams.get('resume')).toBe('follow');
  await confirm.focus();
  await page.keyboard.press('Enter');
  await expect(page).toHaveURL(`${baseURL}/a/artista-sintetica?source=event#artist-hero`);
  expect(state.writes).toHaveLength(2); // one failed request, one explicit retry; no automatic retry
  await expect(page.getByRole('button', { name: 'Dejar de seguir a Artista Sintética' })).toBeEnabled();
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await globalThis.axe.run(document)).violations
    .filter((violation) => ['serious', 'critical'].includes(violation.impact))
    .map(({ id, nodes }) => ({ id, targets: nodes.map((node) => node.target) })));
  await testInfo.attach('axe.json', { body: JSON.stringify(violations), contentType: 'application/json' });
  expect(violations).toEqual([]);
  await page.screenshot({ path: testInfo.outputPath('artist-follow-confirmed.png'), fullPage: true });
});

test('artist follow already-confirmed state consumes only the resume keys', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL, true);
  state.followed = true;
  await page.goto('/a/artista-sintetica?source=event&resume=follow&artistId=17#artist-hero', { waitUntil: 'domcontentloaded' });
  await expect(page.getByRole('button', { name: 'Dejar de seguir a Artista Sintética' })).toBeEnabled();
  await expect(page).toHaveURL(`${baseURL}/a/artista-sintetica?source=event#artist-hero`);
  expect(state.writes).toEqual([]);
});

test('artist follow guest CTA supports the English locale', async ({ page, baseURL }) => {
  await fixture(page, baseURL, false);
  await page.addInitScript(() => localStorage.setItem('tdf-hq-ui/locale', 'en'));
  await page.goto('/a/artista-sintetica', { waitUntil: 'domcontentloaded' });
  await expect(page.getByRole('link', { name: 'Create an account or sign in to follow', exact: true })).toBeVisible();
});
