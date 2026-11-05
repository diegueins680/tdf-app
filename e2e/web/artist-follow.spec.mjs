import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Browser integration with synthetic API fixtures, NOT real-server/authentication E2E.
async function fixture(page, baseURL, authenticated, withVideo = false) {
  const origin = new URL(baseURL).origin;
  const state = { authenticated, followed: false, failNextFollow: false, failNextLogin: false, failNextSignup: false, authAttempts: [], writes: [], external: [] };
  const artist = {
    apArtistId: 17, apDisplayName: 'Artista Sintética', apSlug: 'artista-sintetica',
    apBio: 'Perfil ficticio de verificación local.', apFollowerCount: 0,
    apHasUserAccount: true, apGenreIds: [], apCity: 'Quito',
    ...(withVideo ? { apFeaturedVideoUrl: 'https://youtu.be/uRNyIMLiqU8' } : {}),
  };
  const follow = { ffArtistId: 17, ffArtistName: artist.apDisplayName, ffStartedAt: '2026-09-14T12:00:00Z' };
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (withVideo && url.pathname === '/campaign-platform/public/videos') return route.fulfill({ json: [{
      id: 'a'.repeat(32), title: 'Sesión compartida de prueba', description: 'Créditos de prueba',
      url: 'https://youtu.be/uRNyIMLiqU8', poster: '',
    }] });
    if (url.origin !== origin) {
      state.external.push(url.origin);
      return route.abort('blockedbyclient');
    }
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    // Vite source modules are script resources, not API fixtures.
    const path = url.pathname;
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (['/login', '/signup'].includes(path) && request.method() === 'POST') {
      state.authAttempts.push({ path, body: request.postDataJSON() });
      const fail = path === '/login' ? state.failNextLogin : state.failNextSignup;
      state.failNextLogin = false; state.failNextSignup = false;
      if (fail) return route.fulfill({ status: path === '/login' ? 401 : 400, json: { error: 'Synthetic authentication failure' } });
      state.authenticated = true;
      return route.fulfill({ json: { token: 'synthetic-token', partyId: 42, roles: ['Customer'], modules: [] } });
    }
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

for (const mode of ['signup', 'login']) {
  test(`shared artist session survives failed ${mode}, retry and confirmed follow @mobile-flow @critical`, async ({ page, baseURL }, testInfo) => {
    const state = await fixture(page, baseURL, false, true);
    state[mode === 'signup' ? 'failNextSignup' : 'failNextLogin'] = true;
    const destination = '/a/artista-sintetica?video=youtube%3AuRNyIMLiqU8&utm_source=instagram#artist-session';
    await page.goto(destination, { waitUntil: 'domcontentloaded' });
    await expect(page.getByText('Puedes ver sus videos sin crear una cuenta.', { exact: false })).toBeVisible();
    await page.getByRole('button', { name: 'Ver video aquí', exact: true }).click();
    await expect(page.locator('iframe[title="Sesión compartida de prueba"]')).toHaveAttribute('src', /\/embed\/uRNyIMLiqU8/);
    expect(state.authAttempts).toHaveLength(0);
    expect(state.writes).toHaveLength(0);
    await page.getByRole('link', { name: 'Crear cuenta o ingresar para seguir', exact: true }).click();
    let authUrl = page.url();
    const target = new URL(new URL(authUrl).searchParams.get('redirect'), baseURL);
    expect(target.searchParams.get('video')).toBe('youtube:uRNyIMLiqU8');
    expect(target.searchParams.get('utm_source')).toBe('instagram');
    expect(target.hash).toBe('#artist-session');
    const dialog = page.getByRole('dialog');
    await expect(dialog).toBeVisible();
    let submit;
    if (mode === 'signup') {
      await dialog.getByLabel('Correo').fill('synthetic@example.test');
      await dialog.getByLabel('Contraseña', { exact: false }).first().fill('synthetic-password-123');
      submit = dialog.getByRole('button', { name: 'Crear e ingresar', exact: true });
    } else {
      await dialog.getByRole('button', { name: 'Ya tengo una cuenta', exact: true }).click();
      await page.getByLabel('Usuario o correo').fill('synthetic@example.test');
      await page.getByLabel('Contraseña', { exact: false }).first().fill('synthetic-password-123');
      submit = page.getByRole('button', { name: 'Ingresar', exact: true });
    }
    authUrl = page.url();
    await submit.click();
    await expect.poll(() => state.authAttempts.length).toBe(1);
    await expect(submit).toBeEnabled();
    await expect(page.getByRole('alert').first()).toBeVisible();
    expect(page.url()).toBe(authUrl);
    expect(state.writes).toHaveLength(0);
    await submit.click();
    await expect(page.getByRole('button', { name: 'Seguir ahora', exact: true })).toBeEnabled();
    expect(state.authAttempts).toHaveLength(2);
    expect(new URL(page.url()).searchParams.get('video')).toBe('youtube:uRNyIMLiqU8');
    expect(new URL(page.url()).hash).toBe('#artist-session');
    expect(state.writes).toHaveLength(0);
    state.failNextFollow = true;
    await page.getByRole('button', { name: 'Seguir ahora', exact: true }).click();
    await expect(page.getByRole('alert').filter({ hasText: 'No pudimos confirmar el cambio de seguimiento.' })).toBeVisible();
    expect(new URL(page.url()).searchParams.get('resume')).toBe('follow');
    await page.getByRole('button', { name: 'Seguir ahora', exact: true }).click();
    await expect(page.getByRole('button', { name: 'Dejar de seguir a Artista Sintética' })).toBeEnabled();
    expect(new URL(page.url()).searchParams.has('resume')).toBe(false);
    expect(new URL(page.url()).searchParams.get('video')).toBe('youtube:uRNyIMLiqU8');
    expect(new URL(page.url()).searchParams.get('utm_source')).toBe('instagram');
    await page.reload({ waitUntil: 'domcontentloaded' });
    await expect(page.getByRole('button', { name: 'Dejar de seguir a Artista Sintética' })).toBeEnabled();
    expect(state.writes).toHaveLength(2);
    await expect(page.getByRole('heading', { name: 'Sesión compartida de prueba' })).toBeVisible();
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
    await page.screenshot({ path: testInfo.outputPath(`artist-session-${mode}-confirmed.png`), fullPage: true });
  });
}

test('artist follow requires keyboard consent and keeps intent on failure @critical', async ({ page, baseURL }, testInfo) => {
  const state = await fixture(page, baseURL, true);
  state.failNextFollow = true;
  await page.goto('/a/artista-sintetica?source=event&resume=follow&artistId=17#artist-hero', { waitUntil: 'domcontentloaded' });
  const confirm = page.getByRole('button', { name: 'Seguir ahora', exact: true });
  await expect(confirm).toBeEnabled();
  expect(state.writes).toEqual([]);
  await confirm.focus();
  await page.keyboard.press('Enter');
  await expect(page.getByRole('alert').filter({ hasText: 'No pudimos confirmar el cambio de seguimiento. Comprueba tu conexión y vuelve a intentarlo.' })).toBeVisible();
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
