// TAX-FILTER-001 / TAX-GENRE-SUGGESTION-001: shared location and genre
// filters across surfaces, in Spanish and English, and the administrator
// genre suggestion queue. The API is a synthetic fixture on the preview origin.
import { expect, test } from '@playwright/test';
import { mkdirSync } from 'node:fs';

const EC = 'aaaaaaaa-0000-4000-8000-000000000001';
const PICHINCHA = 'bbbbbbbb-0000-4000-8000-000000000001';
const QUITO = 'cccccccc-0000-4000-8000-000000000001';
const GUAYAQUIL = 'cccccccc-0000-4000-8000-000000000002';
const LATIN = 'dddddddd-0000-4000-8000-000000000001';
const CUMBIA = 'dddddddd-0000-4000-8000-000000000002';
const ROCK = 'dddddddd-0000-4000-8000-000000000003';
const SUGGESTION = 'eeeeeeee-0000-4000-8000-000000000001';
const evidenceDir = 'artifacts/discovery-playwright/taxonomy-filters';

const filterOptions = {
  countries: [{ id: EC, code: 'EC', nameEs: 'Ecuador', nameEn: 'Ecuador' }],
  regions: [{ id: PICHINCHA, countryId: EC, code: 'EC-P', type: 'province', nameEs: 'Pichincha', nameEn: 'Pichincha' }],
  cities: [
    { id: QUITO, countryId: EC, regionId: PICHINCHA, code: 'quito', nameEs: 'Quito', nameEn: 'Quito', latitude: -0.18, longitude: -78.47 },
    { id: GUAYAQUIL, countryId: EC, regionId: null, code: 'gye', nameEs: 'Guayaquil', nameEn: 'Guayaquil', latitude: -2.19, longitude: -79.89 },
  ],
  genres: [
    { id: LATIN, code: 'latin', nameEs: 'Latina', nameEn: 'Latin', parentId: null },
    { id: CUMBIA, code: 'cumbia', nameEs: 'Cumbia', nameEn: 'Cumbia', parentId: LATIN },
    { id: ROCK, code: 'rock', nameEs: 'Rock', nameEn: 'Rock', parentId: null },
  ],
};
const facets = { genres: [{ id: LATIN, count: 3 }, { id: CUMBIA, count: 2 }], countries: [{ id: EC, count: 3 }], regions: [{ id: PICHINCHA, count: 2 }], cities: [{ id: QUITO, count: 2 }, { id: GUAYAQUIL, count: 1 }] };
const profile = { id: 'synthetic-1', type: 'profile', slug: 'luna-1', title: 'Lúna', profileKind: 'artist', location: { city: 'Quito', countryCode: 'EC' }, taxonomy: { genreIds: [CUMBIA], professionIds: [], serviceIds: [], instrumentIds: [] }, score: 1, sponsored: false };
const admin = { username: 'fixture-admin', displayName: 'Synthetic Admin', partyId: 42, roles: ['Admin'], modules: ['admin', 'crm', 'scheduling'], featureFlags: [], preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' } };

async function fixture(page, baseURL, { authenticated = false } = {}) {
  const calls = [];
  const decisions = [];
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === new URL(baseURL).origin ? route.continue() : route.abort();
    const record = () => calls.push({ path: url.pathname, params: url.searchParams });
    if (url.pathname === '/session') return route.fulfill(authenticated ? { json: admin } : { status: 401, json: {} });
    if (url.pathname === '/fans/me/profile' && authenticated) return route.fulfill({ json: { fpArtistId: 42, fpCity: 'Quito' } });
    if (url.pathname === '/taxonomy/filter-options') return route.fulfill({ json: filterOptions });
    if (url.pathname === '/directory/search') { record(); return route.fulfill({ json: { items: [profile], sponsoredItems: [], facets: { total: 1, entityTypes: { profile: 1 }, cities: [], taxonomy: facets } } }); }
    if (url.pathname === '/music/releases') { record(); return route.fulfill({ json: [] }); }
    if (url.pathname === '/music/facets/releases') { record(); return route.fulfill({ json: facets }); }
    if (url.pathname === '/social-events/events') { record(); return route.fulfill({ json: [] }); }
    if (url.pathname === '/social-events/events/facets') { record(); return route.fulfill({ json: facets }); }
    if (url.pathname === '/admin/genre-suggestions') {
      return route.fulfill({ json: decisions.length ? [] : [{ id: SUGGESTION, status: 'pending', submittedName: 'cumbia villera', nameEs: 'Cumbia villera', nameEn: 'Cumbia Villera', parentGenreId: null, reviewReasons: ['unknown_word:villera'], suggestedBy: { partyId: 7, displayName: 'Artista Sintético' }, automatic: false, createdAt: '2026-11-21T12:00:00Z' }] });
    }
    if (url.pathname === `/admin/genre-suggestions/${SUGGESTION}/approve`) {
      decisions.push(request.postDataJSON());
      return route.fulfill({ json: { id: SUGGESTION, status: 'approved', submittedName: 'cumbia villera', nameEs: 'Cumbia villera', nameEn: 'Cumbia Villera', reviewReasons: [], suggestedBy: { partyId: 7 }, automatic: false, createdAt: '2026-11-21T12:00:00Z', resolvedGenre: { id: 'ffffffff-0000-4000-8000-000000000001', nameEs: 'Cumbia villera', nameEn: 'Cumbia Villera' } } });
    }
    if (url.pathname === '/directory/taxonomies') return route.fulfill({ json: { locale: 'es', cities: [], professions: [], serviceOfferings: [], instruments: [], genres: [] } });
    if (url.pathname === '/directory/suggestions') return route.fulfill({ json: [] });
    if (url.pathname === '/fans/artists' || url.pathname === '/parties') return route.fulfill({ json: [] });
    if (url.pathname === '/records/feed') return route.fulfill({ json: { collections: [], releases: [], recordings: [], sessions: [] } });
    if (url.pathname.startsWith('/catalogs/')) return route.fulfill({ json: { items: [] } });
    return route.fulfill({ status: 404, json: {} });
  });
  const lastCall = (path) => calls.filter((call) => call.path === path).at(-1)?.params;
  return { calls, decisions, lastCall };
}

const snapshot = async (page, testInfo, name) => {
  mkdirSync(evidenceDir, { recursive: true });
  await page.screenshot({ path: `${evidenceDir}/${testInfo.project.name}-${name}.png`, fullPage: false });
};

test('shared filters reach community search and music, show counts and chips, and can stay local to one section @critical', async ({ page, baseURL }, testInfo) => {
  const api = await fixture(page, baseURL);
  await page.goto('/fans');
  const filters = page.getByTestId('community-taxonomy-filters');
  await filters.getByRole('button', { name: 'Filtrar por lugar y género' }).click();
  await filters.getByRole('combobox', { name: 'Género' }).click();
  // Subgenres follow their parent and every option shows its count.
  await expect(page.getByRole('option', { name: 'Latina (3)' })).toBeVisible();
  await expect(page.getByRole('option', { name: 'Cumbia (2)' })).toBeVisible();
  await page.getByRole('option', { name: 'Latina (3)' }).click();
  await page.keyboard.press('Escape');
  await filters.getByRole('combobox', { name: 'Ciudad' }).click();
  await page.getByRole('option', { name: /^Quito \(2\)/ }).click();
  await page.keyboard.press('Escape');
  await expect.poll(() => api.lastCall('/directory/search')?.getAll('genreIds')).toEqual([LATIN]);
  await expect.poll(() => api.lastCall('/directory/search')?.getAll('cityIds')).toEqual([QUITO]);
  await expect(page).toHaveURL(new RegExp(`f\\.genre=${LATIN}.*f\\.city=${QUITO}|f\\.city=${QUITO}.*f\\.genre=${LATIN}`));
  await expect(filters.getByTestId('community-taxonomy-chip')).toHaveText(['Quito', 'Latina']);
  await snapshot(page, testInfo, 'community-es');

  // The same shared set applies to music without choosing again.
  await page.goto('/musica');
  await expect.poll(() => api.lastCall('/music/releases')?.getAll('genreIds')).toEqual([LATIN]);
  await expect.poll(() => api.lastCall('/music/releases')?.getAll('cityIds')).toEqual([QUITO]);
  const music = page.getByTestId('music-taxonomy-filters');
  await expect(music.getByTestId('music-taxonomy-chip')).toHaveText(['Quito', 'Latina']);

  // "Only in this section": removing the city here leaves the shared set alone.
  await music.getByRole('button', { name: /filtros activos/ }).click();
  await music.getByRole('checkbox', { name: 'Solo en esta sección' }).check();
  await music.getByRole('button', { name: 'Quitar Ciudad: Quito' }).click();
  await expect(music.getByText('Esta sección usa sus propios filtros.')).toBeVisible();
  await expect.poll(() => api.lastCall('/music/releases')?.getAll('cityIds')).toEqual([]);
  await snapshot(page, testInfo, 'music-override-es');
  await page.goto('/fans');
  await expect(page.getByTestId('community-taxonomy-chip')).toHaveText(['Quito', 'Latina']);

  // Clearing applies to the shared set.
  await page.getByTestId('community-taxonomy-filters').getByRole('button', { name: 'Limpiar filtros' }).click();
  await expect(page.getByTestId('community-taxonomy-chip')).toHaveCount(0);
  await expect.poll(() => api.lastCall('/directory/search')?.getAll('genreIds')).toEqual([]);
});

test('a mirrored page follows a second tab and can clear only its own filters @critical', async ({ page, context, baseURL }) => {
  const api = await fixture(page, baseURL);
  await page.goto(`/fans?q=luna&profileKind=band&f.genre=${LATIN}`);
  const filters = page.getByTestId('community-taxonomy-filters');
  await filters.getByRole('button', { name: 'Usar estos filtros en todas partes' }).click();
  const second = await context.newPage();
  await fixture(second, baseURL);
  await second.goto(`/musica?f.genre=${CUMBIA}`);
  await second.getByRole('button', { name: 'Usar estos filtros en todas partes' }).click();
  await expect.poll(() => api.lastCall('/directory/search')?.getAll('genreIds')).toEqual([CUMBIA]);
  await expect(page).toHaveURL(new RegExp(CUMBIA));
  await filters.getByRole('button', { name: /filtros? activos?/ }).click();
  await filters.getByRole('checkbox', { name: 'Solo en esta sección' }).check();
  await filters.getByRole('button', { name: 'Limpiar filtros' }).click();
  await expect(filters.getByTestId('community-taxonomy-chip')).toHaveCount(0);
  await expect(second.getByTestId('music-taxonomy-chip')).toHaveText('Cumbia');
  await page.getByRole('button', { name: 'Limpiar búsqueda' }).click();
  await expect.poll(() => Array.from(new URL(page.url()).searchParams.keys()).sort()).toEqual(['f.scope']);
  await second.close();
});

test('English viewers see English names, and a shared link opens as this section\'s own filters @critical', async ({ page, baseURL }, testInfo) => {
  const api = await fixture(page, baseURL);
  await page.addInitScript(() => window.localStorage.setItem('tdf-hq-ui/locale', 'en'));
  await page.goto(`/fans?f.genre=${LATIN}&f.region=${PICHINCHA}`);
  const filters = page.getByTestId('community-taxonomy-filters');
  await expect(filters.getByTestId('community-taxonomy-chip')).toHaveText(['Pichincha', 'Latin']);
  await expect.poll(() => api.lastCall('/directory/search')?.getAll('regionIds')).toEqual([PICHINCHA]);
  await expect(filters.getByText('This section uses its own filters.')).toBeVisible();
  await snapshot(page, testInfo, 'community-link-en');
  await filters.getByRole('button', { name: 'Use these filters everywhere' }).click();
  await expect(filters.getByText('This section uses its own filters.')).toHaveCount(0);
  await page.goto('/musica');
  await expect.poll(() => api.lastCall('/music/releases')?.getAll('genreIds')).toEqual([LATIN]);
  await expect(page.getByTestId('music-taxonomy-chip')).toHaveText(['Pichincha', 'Latin']);
});

test('events follow the shared filters for signed-in viewers @critical', async ({ page, baseURL }, testInfo) => {
  const api = await fixture(page, baseURL, { authenticated: true });
  await page.goto(`/social/eventos?state=${ROCK}&f.genre=${CUMBIA}&f.city=${GUAYAQUIL}`);
  await expect.poll(() => api.lastCall('/social-events/events')?.getAll('genreIds')).toEqual([CUMBIA]);
  await expect.poll(() => api.lastCall('/social-events/events')?.getAll('cityIds')).toEqual([GUAYAQUIL]);
  await expect.poll(() => api.lastCall('/social-events/events/facets')?.getAll('genreIds')).toEqual([CUMBIA]);
  await expect(page.getByTestId('events-taxonomy-chip')).toHaveText(['Guayaquil', 'Cumbia']);
  await expect.poll(() => api.lastCall('/social-events/events/facets')?.get('workflow_state_id')).toBe(ROCK);
  await snapshot(page, testInfo, 'events-es');
  const clear = page.getByRole('button', { name: 'Limpiar filtros', exact: true }).first();
  // On phones the docked radio overlays the bottom edge of the scroll area.
  await clear.evaluate((button) => button.scrollIntoView({ block: 'center', behavior: 'instant' }));
  if (testInfo.project.use.hasTouch) await clear.tap();
  else await clear.click();
  await expect.poll(() => new URL(page.url()).searchParams.has('state')).toBe(false);
  const filters = page.getByTestId('events-taxonomy-filters');
  await filters.getByRole('button', { name: 'Filtrar por lugar y género' }).click();
  await filters.getByRole('button', { name: 'Cerca de mí' }).click();
  await expect.poll(() => api.lastCall('/social-events/events')?.getAll('cityIds')).toEqual([QUITO]);
});

test('administrators approve a pending genre suggestion with reviewed names @critical', async ({ page, baseURL }, testInfo) => {
  const api = await fixture(page, baseURL, { authenticated: true });
  await page.goto('/admin/generos');
  const card = page.getByTestId('genre-suggestion-card');
  await expect(card.getByText('palabra nueva: «villera»')).toBeVisible();
  await card.getByLabel('Género principal').click();
  await page.getByRole('option', { name: 'Cumbia' }).click();
  await snapshot(page, testInfo, 'admin-queue-es');
  await card.getByRole('button', { name: 'Aprobar' }).last().click();
  await expect.poll(() => api.decisions).toEqual([{ approvedNameEs: 'Cumbia villera', approvedNameEn: 'Cumbia Villera', approvedParentGenreId: CUMBIA }]);
  await expect(page.getByText('No hay sugerencias en este estado.')).toBeVisible();
});
