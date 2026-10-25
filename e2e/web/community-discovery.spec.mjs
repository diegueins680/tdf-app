import { expect, test } from '@playwright/test';
import axe from 'axe-core';
const genre = '11111111-1111-4111-8111-111111111111';
const profiles = Array.from({ length: 20 }, (_, index) => ({
  id: `synthetic-${index}`, type: 'profile', slug: `luna-${index}`, title: `Lúna ${index}`,
  profileKind: 'artist', summary: 'Proyecto musical de prueba. Jazz en Quito.',
  location: { city: 'Quito', countryCode: 'EC' }, taxonomy: { genreIds: [genre], professionIds: [], serviceIds: [], instrumentIds: [] }, score: 1, sponsored: false,
}));
async function fixture(page, baseURL, authenticated = false) {
  const requests = [];
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === new URL(baseURL).origin ? route.continue() : route.abort();
    if (url.pathname === '/session') return route.fulfill(authenticated ? { json: { username: 'fixture-admin', displayName: 'Synthetic Admin', partyId: 42, roles: ['Admin'], modules: ['admin', 'crm', 'scheduling'], featureFlags: [], preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' } } } : { status: 401, json: {} });
    if (url.pathname === '/directory/taxonomies') return route.fulfill({ json: { locale: 'es', cities: [], professions: [], serviceOfferings: [], instruments: [], genres: [{ id: genre, name: 'Jazz' }] } });
    if (url.pathname === '/directory/search') {
      requests.push(Object.fromEntries(url.searchParams));
      const items = url.searchParams.get('q') === 'missing' ? [] : profiles;
      return route.fulfill({ json: { items, sponsoredItems: [], facets: { total: items.length, entityTypes: { profile: items.length }, cities: [] } } });
    }
    if (/^\/directory\/profiles\/[^/]+\/reviews$/.test(url.pathname)) return route.fulfill({ json: { items: [], nextCursor: null } });
    if (/^\/directory\/profiles\/[^/]+$/.test(url.pathname)) return route.fulfill({ json: { id: 'synthetic-12', slug: 'luna-12', name: 'Lúna 12', kind: 'artist', bio: 'Perfil sintético de prueba. '.repeat(200), location: { city: 'Quito' }, links: [], portfolio: [] } });
    if (url.pathname === '/directory/suggestions') return route.fulfill({ json: [] });
    if (url.pathname === '/fans/artists') return route.fulfill({ json: [] });
    if (url.pathname === '/parties') return route.fulfill({ json: [] });
    if (url.pathname === '/records/feed') return route.fulfill({ json: { collections: [], releases: [], recordings: [], sessions: [] } });
    if (url.pathname.startsWith('/catalogs/')) return route.fulfill({ json: { items: [] } });
    return route.fulfill({ status: 404, json: {} });
  });
  return requests;
}

test('community discovery preserves query, genre and scroll through profile/history, then finds the artist globally @critical', async ({ page, baseURL }) => {
  const requests = await fixture(page, baseURL);
  await page.goto('/fans');
  const search = page.getByRole('search', { name: 'Buscar en Comunidad' });
  await expect(search).toBeVisible();
  await search.getByLabel('Buscar artistas y perfiles').fill('Luna');
  await expect.poll(() => requests.some((request) => request.q === 'Luna')).toBe(true);
  await search.getByLabel('Género musical').click();
  await page.getByRole('option', { name: 'Jazz', exact: true }).click();
  await expect(page).toHaveURL(new RegExp(`genreId=${genre}`));
  const artist = page.getByRole('link').filter({ has: page.getByRole('heading', { name: 'Lúna 12', exact: true }) });
  await artist.scrollIntoViewIfNeeded();
  // Capture the departure viewport after the browser's focus/actionability
  // scrolling, rather than before Playwright positions the click target.
  await artist.evaluate((element) => element.addEventListener('click', () => { window.discoveryDepartureY = window.scrollY; }, { capture: true, once: true }));
  await artist.click({ position: { x: 12, y: 12 } });
  const before = await page.evaluate(() => window.discoveryDepartureY);
  expect(before).toBeGreaterThan(100);

  await expect(page).toHaveURL(/\/directorio\/luna-12$/);
  await expect(page.getByRole('heading', { name: 'Lúna 12', exact: true })).toBeVisible();
  await expect.poll(() => page.evaluate(() => window.scrollY)).toBeLessThan(3);
  await page.goBack();
  await expect(search.getByLabel('Buscar artistas y perfiles')).toHaveValue('Luna');
  await expect(page).toHaveURL(new RegExp(`genreId=${genre}`));
  await expect.poll(async () => Math.abs(await page.evaluate(() => window.scrollY) - before)).toBeLessThan(8);
  await page.goForward();
  await expect(page).toHaveURL(/\/directorio\/luna-12$/);
  await expect.poll(() => page.evaluate(() => window.scrollY)).toBeLessThan(3);
  await page.goBack();
  // A visitor scrolls or tabs to this below-the-fold entry point after Back.
  await expect(search.getByLabel('Buscar artistas y perfiles')).toHaveValue('Luna');
  await page.keyboard.press('Tab');
  await page.getByRole('link', { name: 'Abrir búsqueda global' }).click();
  await expect(page.getByRole('tab', { name: 'Artistas y proyectos' })).toBeVisible();
  await page.getByRole('tab', { name: 'Artistas y proyectos' }).click();
  await expect.poll(() => requests.some((request) => request.entityType === 'artist' && request.q === 'Luna')).toBe(true);
  await expect(page.locator('a[href="/directorio/luna-12"]').first()).toBeVisible();
});

test('debounces typing, clears empty results, preserves inline scroll and exposes accessible controls @critical', async ({ page, baseURL }) => {
  const requests = await fixture(page, baseURL);
  await page.goto('/fans');
  const input = page.getByLabel('Buscar artistas y perfiles');
  await expect(input).toBeVisible();
  await input.fill('m'); await input.fill('mi'); await input.fill('missing');
  await expect(page.getByText('No encontramos perfiles con esta búsqueda.', { exact: false })).toBeVisible();
  expect(requests.filter((request) => request.q && request.q !== 'missing')).toEqual([]);
  await page.getByRole('button', { name: 'Limpiar búsqueda' }).click();
  await expect(page.getByRole('heading', { name: 'Lúna 12', exact: true })).toBeVisible();
  await page.evaluate(() => window.scrollTo(0, 800));
  const before = await page.evaluate(() => window.scrollY);
  await page.evaluate(() => { const button = document.createElement('button'); button.textContent = 'Inline'; button.onclick = () => { button.textContent = 'Done'; }; document.querySelector('main').append(button); button.click(); });
  expect(await page.evaluate(() => window.scrollY)).toBe(before);
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await globalThis.axe.run(document.querySelector('[aria-labelledby="community-discovery-heading"]'))).violations.filter(({ impact }) => ['serious', 'critical'].includes(impact)).map(({ id, nodes }) => ({ id, nodes: nodes.map(({ html, failureSummary }) => ({ html, failureSummary })) })));
  expect(violations).toEqual([]);
});

test('refresh restores document position and a late anchor is reached @critical', async ({ page, baseURL }) => {
  await fixture(page, baseURL);
  await page.goto('/fans');
  await expect(page.getByRole('heading', { name: 'Lúna 12', exact: true })).toBeVisible();
  await page.evaluate(() => window.scrollTo(0, 800));
  await page.reload();
  await expect.poll(() => page.evaluate(() => window.scrollY)).toBeGreaterThan(700);
  await page.goto('/fans#late-section');
  await expect(page.getByLabel('Buscar artistas y perfiles')).toBeVisible();
  await page.evaluate(() => { const section = document.createElement('section'); section.id = 'late-section'; section.textContent = 'Late anchor'; section.style.height = '1000px'; document.querySelector('main').append(section); });
  await expect.poll(() => page.locator('#late-section').evaluate((element) => Math.abs(element.getBoundingClientRect().top - 16))).toBeLessThan(4);
  await page.evaluate(() => { const header = document.createElement('header'); header.dataset.navigationStickyHeader = 'true'; header.style.cssText = 'height:64px;position:fixed;top:0'; document.body.prepend(header); });
  await expect.poll(() => page.locator('#late-section').evaluate((element) => Math.abs(element.getBoundingClientRect().top - 80))).toBeLessThan(4);
  await page.goto('/buscar');
  await expect(page.getByRole('tab', { name: 'Artistas y proyectos' })).toBeVisible();
  await expect.poll(() => page.evaluate(() => window.scrollY)).toBeLessThan(3);
});

// Real AppShell route transitions, with delayed list geometry controlled by the fixture.
test('protected scrolling container restores after async layout and public navigation @critical', async ({ page, baseURL }) => {
  await fixture(page, baseURL, true);
  await page.goto('/crm/contactos');
  const main = page.locator('[data-navigation-scroll-container]');
  await expect(main).toBeVisible();
  await expect(page.getByRole('heading', { name: 'Contactos', exact: true })).toBeVisible();
  await expect(page.getByText('Sin contactos aún', { exact: true })).toBeVisible();
  const addContent = () => page.evaluate(() => {
    const element = document.createElement('div'); element.style.minHeight = '4000px'; element.style.flexShrink = '0'; element.textContent = 'Synthetic async list';
    document.querySelector('[data-navigation-scroll-container]').append(element);
  });
  await addContent();
  await expect.poll(() => main.evaluate((element) => element.scrollHeight - element.clientHeight)).toBeGreaterThan(2000);
  await main.dispatchEvent('wheel', { deltaY: 1200, bubbles: true });
  await main.evaluate((element) => new Promise((resolve) => requestAnimationFrame(() => { element.scrollTo({ top: 1200, behavior: 'instant' }); resolve(); })));
  await expect.poll(() => main.evaluate((element) => element.scrollTop)).toBeGreaterThan(100);
  const searchLink = page.getByRole('link', { name: 'Buscar artistas y comunidad' });
  await searchLink.evaluate((element) => element.addEventListener('click', () => { window.discoveryMainDepartureY = document.querySelector('[data-navigation-scroll-container]').scrollTop; }, { once: true, capture: true }));
  await searchLink.click();
  const before = await page.evaluate(() => window.discoveryMainDepartureY);
  expect(before).toBeGreaterThan(100);
  await expect(page).toHaveURL(/\/buscar/);
  await expect(page.getByRole('tab', { name: 'Artistas y proyectos' })).toBeVisible();
  await expect.poll(() => page.evaluate(() => window.scrollY)).toBeLessThan(3);
  await page.goBack();
  await expect(main).toBeVisible();
  await expect(page.getByRole('heading', { name: 'Contactos', exact: true })).toBeVisible();
  await expect(page.getByText('Sin contactos aún', { exact: true })).toBeVisible();
  await addContent();
  await expect.poll(async () => Math.abs(await main.evaluate((element) => element.scrollTop) - before)).toBeLessThan(8);
});

// Wait for the lazy sidebar, rather than testing only its loading placeholder.
// A fixed-position mobile sidebar must not leave the drawer paper at zero width
// and move the close control outside the viewport after the chunk loads.
test('loaded feature menu stays dismissible from the public header @critical', async ({ page, baseURL }) => {
  await fixture(page, baseURL, true);
  await page.goto('/fans');
  const trigger = page.getByRole('button', { name: 'Menú de funciones', exact: true });
  await trigger.click();
  const menu = page.getByRole('dialog', { name: 'Menú de funciones', exact: true });
  await expect(menu.getByLabel('Buscar sección')).toBeVisible();
  const close = menu.getByRole('button', { name: 'Cerrar menú de funciones', exact: true });
  await expect(close).toBeInViewport({ ratio: 1 });
  await close.click();
  await expect(menu).toBeHidden();
  await expect(trigger).toBeFocused();

  await trigger.click();
  await expect(menu.getByLabel('Buscar sección')).toBeVisible();
  await page.keyboard.press('Escape');
  await expect(menu).toBeHidden();
  await expect(trigger).toBeFocused();
});
