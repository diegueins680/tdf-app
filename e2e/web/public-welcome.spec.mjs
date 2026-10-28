import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Synthetic API/browser integration. Only local documents and static assets
// may reach the preview server; every API is fulfilled here and external
// resources are blocked. This does not certify provider, persistence or human
// comprehension outcomes.
async function fixture(page, baseURL, { searchFails = false } = {}) {
  const origin = new URL(baseURL).origin;
  const state = { writes: [], searches: [], sessions: [], blockedExternal: [], errors: [], geoCalls: 0 };
  page.on('pageerror', error => state.errors.push(error.message));
  await page.exposeFunction('__welcomeGeoRequested', () => { state.geoCalls++; });
  await page.addInitScript(() => {
    localStorage.setItem('tdf-hq-ui/locale', 'es');
    Object.defineProperty(navigator, 'geolocation', { configurable: true, value: {
      getCurrentPosition: (_success, failure) => {
        void globalThis.__welcomeGeoRequested();
        failure?.({ code: 1, message: 'Synthetic permission denial' });
      },
      watchPosition: (_success, failure) => {
        void globalThis.__welcomeGeoRequested();
        failure?.({ code: 1, message: 'Synthetic permission denial' });
        return 1;
      },
      clearWatch: () => {},
    } });
  });
  await page.route('**/*', async route => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['GET', 'HEAD', 'OPTIONS'].includes(request.method())) {
      state.writes.push({ method: request.method(), path: url.pathname });
      return route.abort('blockedbyclient');
    }
    if (request.method() === 'OPTIONS') return route.fulfill({ status: 204 });
    if (!['fetch', 'xhr'].includes(request.resourceType())) {
      if (url.origin === origin) return route.continue();
      state.blockedExternal.push(request.url());
      return route.abort('blockedbyclient');
    }
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (path === '/session') {
      state.sessions.push({ method: request.method(), status: 200, body: null });
      // The actual optional-session contract is HTTP 200 with JSON null.
      return route.fulfill({ status: 200, json: null });
    }
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/directory/taxonomies') return route.fulfill({ json: {
      locale: 'es', professions: [], classifiedCategories: [], compensationTypes: [],
      serviceOfferings: [], currencies: [], instruments: [], genres: [],
      cities: [{ id: 'quito-fixture', code: 'quito-ec-p', name: 'Quito' }],
    } });
    if (path === '/directory/search') {
      state.searches.push(Object.fromEntries(url.searchParams));
      if (searchFails) return route.fulfill({ status: 503, json: { error: 'Synthetic search outage' } });
      return route.fulfill({ json: {
        items: [], sponsoredItems: [], facets: { entityTypes: {}, cities: [], total: 0 }, nextCursor: null,
      } });
    }
    if (path === '/directory/suggestions' || path === '/fans/artists') return route.fulfill({ json: [] });
    if (path === '/records/feed') return route.fulfill({ json: {
      locale: 'es', revision: 1, collections: [], releases: [], sessions: [],
      recordings: [{
        id: 'welcome-recording', code: 'welcome-recording', title: 'Sesión pública de prueba',
        description: 'Contenido sintético para comprobar el acceso público.',
        contributors: [], sortOrder: 0, revision: 1,
        resources: [{
          id: 'welcome-video', providerCode: 'youtube', kind: 'video', externalCode: 'Welc0me1234',
          url: 'https://www.youtube.com/watch?v=Welc0me1234',
          thumbnailUrl: new URL('/event-fallback.svg', origin).href,
          primary: true, relationKind: 'primary-media', sortOrder: 0,
        }],
      }],
    } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], catalogs: [], defaults: [] } });
    if (path === '/service-catalog' || path === '/service-catalog/public' || path === '/services/public') {
      return route.fulfill({ json: [] });
    }
    // Unknown API reads cannot escape to a live service or fabricate success.
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture for this API' } });
  });
  return state;
}

function expectReadOnly(state) {
  expect(state.writes).toEqual([]);
  expect(state.geoCalls).toBe(0);
  expect(state.errors).toEqual([]);
}

async function expectAccessible(page, testInfo, name) {
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await globalThis.axe.run(document, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.filter(({ impact }) => ['serious', 'critical'].includes(impact))
    .map(({ id, impact, nodes }) => ({ id, impact, nodes: nodes.map(({ target, failureSummary }) => ({ target, failureSummary })) })));
  await testInfo.attach(`${name}-axe.json`, { body: JSON.stringify(violations), contentType: 'application/json' });
  expect(violations).toEqual([]);
}

test('anonymous root welcomes before search without registration or location access @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const state = await fixture(page, baseURL);
  await page.goto('/', { waitUntil: 'domcontentloaded' });
  await expect(page).toHaveURL(/\/inicio(?:\?|$)/);
  const welcome = page.getByRole('region', { name: 'Bienvenido a TDF Records' });
  await expect(welcome).toBeVisible();
  await expect(page.getByRole('heading', { level: 1 })).toHaveCount(1);
  const photo = welcome.getByRole('img');
  await expect(photo).toBeVisible();
  await expect.poll(() => photo.evaluate(image => image.complete && image.naturalWidth > 0)).toBe(true);
  const form = page.getByRole('search', { name: 'Buscar artistas y profesionales' });
  await expect(form).toBeAttached();
  expect(await welcome.evaluate((element, search) => Boolean(element.compareDocumentPosition(search) & Node.DOCUMENT_POSITION_FOLLOWING), await form.elementHandle())).toBe(true);
  await expect(welcome.locator('input, select, textarea')).toHaveCount(0);
  await expect(welcome.locator('a[href*="/login"]')).toHaveCount(0);
  await expect(page.getByRole('dialog')).toHaveCount(0);
  await expect.poll(() => state.sessions.length).toBeGreaterThan(0);
  expect(state.sessions.every(session => session.status === 200 && session.body === null)).toBe(true);
  expectReadOnly(state);
  await page.screenshot({ path: testInfo.outputPath('welcome-public.png'), fullPage: true });
});

test('welcome leads to public studio and services before account choices @critical @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  await page.goto('/inicio', { waitUntil: 'domcontentloaded' });
  const welcome = page.getByRole('region', { name: 'Bienvenido a TDF Records' });
  const explore = welcome.getByRole('link', { name: 'Conoce TDF y sus servicios', exact: true });
  await explore.focus();
  await expect(explore).toBeFocused();
  await explore.press('Enter');
  await expect(page).toHaveURL(/\/tdf$/);
  const main = page.getByRole('main');
  const studioLink = main.getByRole('link', { name: 'Conoce el estudio', exact: true });
  const servicesLink = main.getByRole('link', { name: 'Explora nuestros servicios', exact: true });
  await expect(studioLink).toHaveAttribute('href', '#estudio');
  await expect(servicesLink).toHaveAttribute('href', '#servicios');
  await studioLink.click();
  await expect(page).toHaveURL(/\/tdf#estudio$/);
  await expect(main.locator('#estudio').getByRole('heading')).toBeVisible();
  await servicesLink.click();
  await expect(page).toHaveURL(/\/tdf#servicios$/);
  await expect(main.locator('#servicios').getByRole('heading', { level: 2 })).toBeVisible();
  const order = await main.evaluate(element => {
    const studio = element.querySelector('#estudio');
    const services = element.querySelector('#servicios');
    const signup = element.querySelector('a[href*="signup=1"]');
    return {
      studioBeforeSignup: Boolean(studio && signup && (studio.compareDocumentPosition(signup) & Node.DOCUMENT_POSITION_FOLLOWING)),
      servicesBeforeSignup: Boolean(services && signup && (services.compareDocumentPosition(signup) & Node.DOCUMENT_POSITION_FOLLOWING)),
    };
  });
  expect(order).toEqual({ studioBeforeSignup: true, servicesBeforeSignup: true });
  await expect(page.getByRole('dialog')).toHaveCount(0);
  expectReadOnly(state);
});

test('welcome opens published work as a guest without an account gate @critical @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  await page.goto('/inicio', { waitUntil: 'domcontentloaded' });
  await page.getByRole('region', { name: 'Bienvenido a TDF Records' })
    .getByRole('link', { name: 'Mira sesiones y lanzamientos', exact: true }).click();
  await expect(page).toHaveURL(/\/records$/);
  await expect(page.getByRole('main')).toBeVisible();
  await expect(page.getByRole('main').getByText('Sesión pública de prueba', { exact: true }).first()).toBeVisible();
  await expect(page.getByRole('dialog')).toHaveCount(0);
  await expect(page.locator('input[type="password"]')).toHaveCount(0);
  expectReadOnly(state);
});

test('studio photos open with keyboard, fit small screens and restore focus @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const state = await fixture(page, baseURL);
  const enlarged = [];
  page.on('request', request => { if (/\/assets\/tdf-studio\/.*-1280\.webp/.test(request.url())) enlarged.push(request.url()); });
  await page.goto('/tdf#estudio', { waitUntil: 'domcontentloaded' });
  const gallery = page.getByRole('group', { name: 'Fotos del estudio TDF' });
  await gallery.scrollIntoViewIfNeeded();
  const photos = gallery.getByRole('img');
  await expect(photos).toHaveCount(6);
  for (const photo of await photos.all()) {
    await photo.scrollIntoViewIfNeeded();
    await expect.poll(() => photo.evaluate(image => image.complete && image.naturalWidth > 0)).toBe(true);
  }
  expect(enlarged).toHaveLength(0);
  await gallery.screenshot({ path: testInfo.outputPath('studio-gallery.png') });
  const trigger = gallery.getByRole('button', { name: 'Ampliar foto: Sala de control', exact: true });
  await trigger.focus();
  await trigger.press('Enter');
  const dialog = page.getByRole('dialog', { name: 'Sala de control', exact: true });
  await expect(dialog).toBeVisible();
  await expect.poll(() => dialog.getByRole('img').evaluate(image => image.complete && image.naturalWidth === 1280)).toBe(true);
  await expect(dialog.getByRole('status')).toHaveCount(0);
  await expectAccessible(page, testInfo, 'studio-photo-dialog');
  await page.keyboard.press('Escape');
  await expect(dialog).toHaveCount(0);
  await expect(trigger).toBeFocused();
  await page.setViewportSize({ width: 320, height: 740 });
  await gallery.scrollIntoViewIfNeeded();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  await expectAccessible(page, testInfo, 'studio-gallery-320');
  await trigger.click();
  await expect(dialog).toBeVisible();
  expect(await dialog.evaluate(element => { const r = element.getBoundingClientRect(); return r.left >= 0 && r.right <= innerWidth; })).toBe(true);
  await dialog.screenshot({ path: testInfo.outputPath('studio-photo-320.png') });
  await dialog.getByRole('button', { name: 'Cerrar foto', exact: true }).click();
  await expect(trigger).toBeFocused();
  expectReadOnly(state);
});

test('failed studio images retain public navigation and recover on retry @critical @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  let attempts = 0;
  await page.route('**/assets/tdf-studio/control-room-640.webp', route => route.abort('failed'));
  await page.route('**/assets/tdf-studio/control-room-1280.webp*', route => {
    attempts++;
    return attempts === 1 ? route.abort('failed') : route.continue();
  });
  await page.goto('/tdf#estudio', { waitUntil: 'domcontentloaded' });
  const gallery = page.getByRole('group', { name: 'Fotos del estudio TDF' });
  const trigger = gallery.getByRole('button', { name: 'Ampliar foto: Sala de control', exact: true });
  await trigger.scrollIntoViewIfNeeded();
  await expect(trigger).toContainText('Foto no disponible. Abre para reintentar.');
  await trigger.click();
  const dialog = page.getByRole('dialog', { name: 'Sala de control', exact: true });
  await expect(dialog.getByRole('alert')).toContainText('No se pudo cargar esta foto');
  await dialog.getByRole('button', { name: 'Reintentar foto', exact: true }).click();
  await expect.poll(() => dialog.getByRole('img').evaluate(image => image.complete && image.naturalWidth > 0)).toBe(true);
  expect(attempts).toBe(2);
  await dialog.getByRole('button', { name: 'Cerrar foto', exact: true }).click();
  await expect(trigger).toBeFocused();
  await expect(gallery.getByRole('img')).toHaveCount(5);
  await expect(page.locator('#estudio').getByRole('link', { name: 'Reservar experiencia', exact: true })).toBeEnabled();
  expectReadOnly(state);
});

test('direct search preserves query and city across changes and browser history @critical @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL);
  await page.goto('/buscar?q=bass&cityId=quito-fixture', { waitUntil: 'domcontentloaded' });
  await expect(page.getByRole('region', { name: 'Bienvenido a TDF Records' })).toHaveCount(0);
  const query = page.getByRole('combobox', { name: 'Buscar artistas, profesionales o eventos', exact: true });
  await expect(query).toHaveValue('bass');
  await expect(page.getByRole('combobox', { name: 'Ciudad', exact: true })).toHaveText('Quito');
  await expect.poll(() => state.searches.some(search => search.q === 'bass' && search.cityId === 'quito-fixture')).toBe(true);
  await query.fill('piano');
  await query.press('Enter');
  await expect.poll(() => state.searches.some(search => search.q === 'piano' && search.cityId === 'quito-fixture')).toBe(true);
  await page.goBack();
  await expect(query).toHaveValue('bass');
  expect(new URL(page.url()).searchParams.get('cityId')).toBe('quito-fixture');
  await page.reload({ waitUntil: 'domcontentloaded' });
  await expect(query).toHaveValue('bass');
  await expect(page.getByRole('combobox', { name: 'Ciudad', exact: true })).toHaveText('Quito');
  await expect(page.getByRole('combobox', { name: 'Profesión que buscas', exact: true })).toBeVisible();
  expectReadOnly(state);
});

test('a directory outage keeps welcome exploration useful and truthful @critical @mobile-flow', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL, { searchFails: true });
  await page.goto('/inicio', { waitUntil: 'domcontentloaded' });
  const failure = page.getByRole('alert').filter({ hasText: 'No se pudo completar la búsqueda.' });
  // Match the existing directory recovery suite: allow its real query retry cycle to settle.
  await expect(failure).toBeVisible({ timeout: 20000 });
  await expect(failure.getByRole('button', { name: 'Reintentar', exact: true })).toBeEnabled();
  const welcome = page.getByRole('region', { name: 'Bienvenido a TDF Records' });
  await expect(welcome).toBeVisible();
  await welcome.getByRole('link', { name: 'Conoce TDF y sus servicios', exact: true }).click();
  await expect(page).toHaveURL(/\/tdf$/);
  await expect(page.getByRole('main').getByRole('link', { name: 'Conoce el estudio', exact: true })).toBeVisible();
  await expect(page.getByRole('dialog')).toHaveCount(0);
  expectReadOnly(state);
});

test('welcome and direct search stay accessible without overflow at 320 pixels @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  await page.setViewportSize({ width: 320, height: 740 });
  const state = await fixture(page, baseURL);
  await page.goto('/inicio', { waitUntil: 'domcontentloaded' });
  await expect(page.getByRole('region', { name: 'Bienvenido a TDF Records' })).toBeVisible();
  await expect(page.getByText('Todavía no hay coincidencias', { exact: true })).toBeVisible();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  await expectAccessible(page, testInfo, 'welcome-320');
  const search = page.getByRole('region', { name: 'Bienvenido a TDF Records' })
    .getByRole('link', { name: 'Buscar artistas y profesionales', exact: true });
  await search.focus();
  await expect(search).toBeFocused();
  await search.press('Enter');
  await expect(page).toHaveURL(/\/buscar(?:\?|$)/);
  await expect(page.getByRole('region', { name: 'Bienvenido a TDF Records' })).toHaveCount(0);
  await expect(page.getByText('Todavía no hay coincidencias', { exact: true })).toBeVisible();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  await expectAccessible(page, testInfo, 'search-320');
  expectReadOnly(state);
});
