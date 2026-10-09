import { expect, test } from '@playwright/test';

// Synthetic API/browser integration for the account-entry flows reported as
// "white screen after signup/Google" on a Redmi Android phone (2026-10-07).
// No request reaches a real service.

const ARTISTS = [
  { apArtistId: 11, apDisplayName: 'Verde 70', apGenreIds: [], apGenres: 'Rock', apFollowerCount: 3 },
  { apArtistId: 12, apDisplayName: 'Diego Saa', apGenreIds: [], apGenres: 'Pop', apFollowerCount: 5 },
];

async function fixture(page, baseURL) {
  const origin = new URL(baseURL).origin;
  const state = { authenticated: false, signups: [], requests: [] };
  const session = () => ({
    username: 'llamaestepez@gmail.com', displayName: 'Llamaestepez', partyId: 77,
    roles: ['customer'], modules: [], featureFlags: [],
    preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
  });
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) {
      return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
    }
    const path = url.pathname;
    state.requests.push(`${request.method()} ${path}`);
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/session') return state.authenticated
      ? route.fulfill({ json: session() })
      : route.fulfill({ status: 401, json: { error: 'unauthenticated' } });
    if (path === '/signup' && request.method() === 'POST') {
      state.signups.push(request.postDataJSON());
      state.authenticated = true;
      return route.fulfill({ json: { token: 'synthetic-token', partyId: 77, roles: ['Customer'], modules: [] } });
    }
    if (path === '/login' && request.method() === 'POST') {
      state.authenticated = true;
      return route.fulfill({ json: { token: 'synthetic-token', partyId: 77, roles: ['Customer'], modules: [] } });
    }
    if (path === '/fans/artists') return route.fulfill({ json: ARTISTS });
    if (['/fans/me/follows', '/fans/me/clubs'].includes(path)) return route.fulfill({ json: [] });
    if (path === '/marketplace') return route.fulfill({ json: [] });
    if (path === '/records/feed') return route.fulfill({ json: { collections: [], releases: [], recordings: [], sessions: [] } });
    if (path.startsWith('/session/onboarding')) {
      return route.fulfill({ json: { newlyCompleted: false, progress: { eligible: false, completedAt: '2026-10-07T00:00:00Z' }, eligible: false, completedAt: '2026-10-07T00:00:00Z' } });
    }
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture for this API' } });
  });
  return state;
}

async function expectRenderedApp(page) {
  await expect.poll(async () => (await page.locator('#root').innerText()).trim().length).toBeGreaterThan(40);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
}

test('email signup asks only for email and password and lands on artist-first Comunidad @mobile-flow @critical', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  await page.goto('/login?signup=1', { waitUntil: 'domcontentloaded' });
  const dialog = page.getByRole('dialog');
  await expect(dialog.getByLabel('Correo')).toBeVisible();
  await expect(dialog.getByLabel('Nombre')).toHaveCount(0);
  await expect(dialog.getByRole('checkbox')).toHaveCount(0);
  await expect(page.locator('#signup-consent-notice')).toContainText('Al crear tu cuenta aceptas');

  await dialog.getByLabel('Correo').fill('llamaestepez@gmail.com');
  await dialog.getByLabel('Contraseña', { exact: false }).first().fill('secreta-larga-1');
  await dialog.getByRole('button', { name: 'Crear e ingresar' }).click();

  await expect(page).toHaveURL(/\/fans$/);
  await expect(page.getByRole('heading', { name: 'Comunidad — Conecta con tus artistas' })).toBeVisible();
  await expectRenderedApp(page);
  expect(state.signups).toHaveLength(1);
  expect(state.signups[0]).toMatchObject({
    email: 'llamaestepez@gmail.com', firstName: 'Llamaestepez', lastName: '', termsAccepted: true,
    termsVersion: 'tdf-account-terms-v1',
  });
  expect(state.signups[0]).not.toHaveProperty('phone');

  // Artist discovery comes before the promotional shortcut cards.
  const artist = page.getByText('Verde 70', { exact: true }).first();
  await expect(artist).toBeVisible();
  const artistTop = (await artist.boundingBox()).y;
  const bookingsCard = page.getByText('Experiencias y reservas', { exact: true });
  if (await bookingsCard.count()) {
    expect(artistTop).toBeLessThan((await bookingsCard.first().boundingBox()).y);
  }
});

test('login returns to the interrupted action and rejects external redirects @mobile-flow', async ({ page, baseURL }) => {
  await fixture(page, baseURL);
  await page.goto('/login?redirect=%2Fmarketplace', { waitUntil: 'domcontentloaded' });
  await page.getByLabel('Usuario o correo').fill('llamaestepez@gmail.com');
  await page.getByLabel('Contraseña', { exact: false }).first().fill('secreta-larga-1');
  await page.getByRole('button', { name: 'Ingresar', exact: true }).click();
  await expect(page).toHaveURL(/\/marketplace$/);
  await expectRenderedApp(page);

  await page.context().clearCookies();
  await page.evaluate(() => { localStorage.clear(); sessionStorage.clear(); });
  await fixture(page, baseURL);
  await page.goto('/login?returnTo=https%3A%2F%2Fevil.example%2Fphish', { waitUntil: 'domcontentloaded' });
  await page.getByLabel('Usuario o correo').fill('llamaestepez@gmail.com');
  await page.getByLabel('Contraseña', { exact: false }).first().fill('secreta-larga-1');
  await page.getByRole('button', { name: 'Ingresar', exact: true }).click();
  await expect(page).toHaveURL(/127\.0\.0\.1:4173\/fans$/);
  await expectRenderedApp(page);
});

test('a returning session opening the site root lands on Comunidad @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  state.authenticated = true;
  await page.addInitScript(() => localStorage.setItem('tdf-hq-ui/session', JSON.stringify({
    username: 'llamaestepez@gmail.com', displayName: 'Llamaestepez', roles: ['customer'], modules: [], partyId: 77,
  })));
  await page.goto('/', { waitUntil: 'domcontentloaded' });
  await expect(page).toHaveURL(/\/fans$/);
  await expectRenderedApp(page);
});

test('the app shell never stays blank: a failed bundle shows recovery actions @mobile-flow @critical', async ({ page, baseURL }) => {
  await fixture(page, baseURL);
  // Simulate the entry bundle failing to load (flaky network / stale deploy).
  await page.route(/\/assets\/index-[^/]+\.js$/, (route) => route.abort('failed'));
  await page.goto('/fans', { waitUntil: 'domcontentloaded' });
  const recovery = page.locator('#tdf-blank-screen-recovery');
  await expect(recovery).toBeVisible({ timeout: 20_000 });
  await expect(recovery.getByRole('heading', { name: 'La página no terminó de cargar' })).toBeVisible();
  for (const name of ['Recargar', 'Ir a Comunidad', 'Iniciar sesión']) {
    const button = recovery.getByRole('button', { name });
    await expect(button).toBeVisible();
    const box = await button.boundingBox();
    expect(box.height).toBeGreaterThanOrEqual(44);
  }
});

test('a healthy page never shows the blank-screen recovery @mobile-flow', async ({ page, baseURL }) => {
  await fixture(page, baseURL);
  await page.goto('/fans', { waitUntil: 'domcontentloaded' });
  await expectRenderedApp(page);
  await page.waitForTimeout(12_000);
  await expect(page.locator('#tdf-blank-screen-recovery')).toHaveCount(0);
});
