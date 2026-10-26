import { expect, test } from '@playwright/test';

test.use({ serviceWorkers: 'block' });

// This suite exercises the rendered app with synthetic metadata/API replies.
// PostgreSQL search relevance/authorization and real audio decoding have their
// own suites. No media methods, clocks, or browser prototypes are replaced here.
const summaries = Array.from({ length: 49 }, (_, index) => ({
  id: `10000000-0000-4000-8000-${String(index + 1).padStart(12, '0')}`,
  versionId: `20000000-0000-4000-8000-${String(index + 1).padStart(12, '0')}`,
  versionNumber: 1, artistPartyId: 101, slug: `release-sintetico-${index + 1}`,
  kind: 'single', title: `Álbum sintético ${index + 1}`, displayArtist: 'Artista sintético',
  releaseAtUtc: '2026-01-01T00:00:00Z', publishedAt: '2026-01-01T00:00:01Z', labelName: null, coverAssetId: null,
}));
const trackTitle = 'Canción del horizonte';
const detail = (summary) => ({ ...summary, subtitle: null, versionTitle: null, catalogNumber: null,
  explicitContent: 'not_explicit', originalReleaseDate: '2026-01-01', coverAssets: [], availability: [],
  tracks: [{ trackId: '30000000-0000-4000-8000-000000000025', recordingId: '40000000-0000-4000-8000-000000000025',
    discNumber: 1, trackNumber: 1, title: trackTitle, displayArtist: summary.displayArtist,
    durationMs: 60000, explicitContent: 'not_explicit', sources: [] }],
});

async function installCatalogFixture(page, baseURL) {
  const boundary = { catalogRequests: [], detailRequests: [], deniedExternal: [], unexpectedMutations: [], loadedEntries: [],
    limitations: 'API/metadata fixtures; no PostgreSQL, production authorization, or audio decoding claim.' };
  const appOrigin = new URL(baseURL).origin;
  page.on('response', (response) => {
    if (new URL(response.url()).origin === appOrigin && /\/assets\/index-[^/]+\.js$/.test(new URL(response.url()).pathname)) boundary.loadedEntries.push(response.url());
  });
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['http:', 'https:'].includes(url.protocol)) return route.continue();
    if (['fetch', 'xhr'].includes(request.resourceType())) {
      if (url.pathname === '/session' && request.method() === 'GET') return route.fulfill({ json: null });
      if (url.pathname === '/catalogs/batch') return route.fulfill({ json: {} });
      if (request.method() !== 'GET') {
        boundary.unexpectedMutations.push({ method: request.method(), path: url.pathname });
        return route.fulfill({ status: 405, json: { error: 'No business mutations permitted by catalog fixture' } });
      }
      if (url.pathname === '/music/releases') {
        const q = url.searchParams.get('q') ?? '';
        const limit = Number(url.searchParams.get('limit') ?? 24);
        const offset = Number(url.searchParams.get('offset') ?? 0);
        boundary.catalogRequests.push({ q, limit, offset });
        // The server's track-title lookup is represented explicitly in this
        // fixture; the separate SQL suite verifies its implementation.
        const rows = q === 'horizonte' ? [summaries[24]]
          : q === '' || q === 'Artista sintético' ? summaries : [];
        return route.fulfill({ json: rows.slice(offset, offset + limit) });
      }
      if (url.pathname.startsWith('/music/releases/')) {
        const slug = decodeURIComponent(url.pathname.slice('/music/releases/'.length));
        boundary.detailRequests.push(slug);
        const summary = summaries.find((item) => item.slug === slug);
        return summary ? route.fulfill({ json: detail(summary) }) : route.fulfill({ status: 404, json: {} });
      }
      if (url.pathname === '/fans/artists') return route.fulfill({ json: [] });
      return route.fulfill({ status: 404, json: { error: 'Unconfigured synthetic API route' } });
    }
    if (url.origin === appOrigin) return route.continue();
    boundary.deniedExternal.push(url.origin);
    return route.abort('blockedbyclient');
  });
  return boundary;
}

const catalogHeading = (page, index) => page.getByRole('heading', { name: `Álbum sintético ${index}`, exact: true });
const releaseLink = (page, index) => page.getByRole('link').filter({ has: catalogHeading(page, index) });
const searchField = (page) => page.getByRole('textbox', { name: 'Buscar por pista, lanzamiento o artista' });
const pager = (page) => page.getByRole('navigation', { name: 'Páginas del catálogo musical' });

async function assertBoundary(boundary, testInfo) {
  expect(boundary.unexpectedMutations).toEqual([]);
  expect(boundary.loadedEntries.length).toBeGreaterThan(0);
  await testInfo.attach('catalog-discovery-fixture-boundary.json', { body: JSON.stringify(boundary, null, 2), contentType: 'application/json' });
}

test('@critical @mobile-flow PW-MUSIC-DISCOVERY-01 reaches releases 25 and 49 with keyboard pagination and preserves page through detail/back', async ({ page, baseURL }, testInfo) => {
  const boundary = await installCatalogFixture(page, baseURL);
  await page.goto('/musica');
  await expect(catalogHeading(page, 1)).toBeVisible();
  await expect(catalogHeading(page, 25)).toHaveCount(0);
  const next = pager(page).getByRole('button', { name: 'Página siguiente' });
  await expect(next).toBeEnabled();
  await next.scrollIntoViewIfNeeded();
  const controls = await pager(page).getByRole('button').evaluateAll((buttons) => buttons.map((button) => {
    const rect = button.getBoundingClientRect();
    return { name: button.getAttribute('aria-label'), width: rect.width, height: rect.height };
  }));
  for (const control of controls) {
    expect(control.width, control.name).toBeGreaterThanOrEqual(44);
    expect(control.height, control.name).toBeGreaterThanOrEqual(44);
  }
  await next.focus();
  await expect(next).toBeFocused();
  await next.press('Enter');
  await expect(page).toHaveURL(/\/musica\?page=2$/);
  await expect(catalogHeading(page, 25)).toBeVisible();
  await expect(catalogHeading(page, 1)).toHaveCount(0);
  expect(boundary.catalogRequests.some((request) => request.offset === 24 && request.limit === 25)).toBe(true);
  await releaseLink(page, 25).click();
  await expect(page).toHaveURL(/\/musica\/release-sintetico-25$/);
  await expect(page.getByRole('heading', { name: 'Álbum sintético 25', exact: true, level: 1 })).toBeVisible();
  await expect(page.getByText(trackTitle, { exact: true })).toBeVisible();
  await page.goBack();
  await expect(page).toHaveURL(/\/musica\?page=2$/);
  await expect(catalogHeading(page, 25)).toBeVisible();
  await page.goBack();
  await expect(page).toHaveURL(/\/musica$/);
  await expect(catalogHeading(page, 1)).toBeVisible();
  await expect(pager(page).getByRole('button', { name: 'Página anterior' })).toBeDisabled();
  await next.click();
  await expect(catalogHeading(page, 25)).toBeVisible();
  await expect(next).toBeEnabled();
  await next.click();
  await expect(page).toHaveURL(/\/musica\?page=3$/);
  await expect(catalogHeading(page, 49)).toBeVisible();
  await expect(next).toBeDisabled();
  await expect(pager(page).getByRole('status')).toHaveText('Página 3');
  expect(boundary.detailRequests).toContain('release-sintetico-25');
  await assertBoundary(boundary, testInfo);
});

test('@critical @mobile-flow PW-MUSIC-DISCOVERY-02 track-title search resets the page, preserves URL query and opens its matching release', async ({ page, baseURL }, testInfo) => {
  const boundary = await installCatalogFixture(page, baseURL);
  await page.goto('/musica?q=Artista%20sint%C3%A9tico&page=2&context=fixture');
  await expect(catalogHeading(page, 25)).toBeVisible();
  await expect(searchField(page)).toHaveValue('Artista sintético');
  await searchField(page).fill('horizonte');
  await expect.poll(() => new URL(page.url()).searchParams.get('q')).toBe('horizonte');
  await expect.poll(() => new URL(page.url()).searchParams.has('page')).toBe(false);
  expect(new URL(page.url()).searchParams.get('context')).toBe('fixture');
  await expect(catalogHeading(page, 25)).toBeVisible();
  await expect(catalogHeading(page, 26)).toHaveCount(0);
  await expect(pager(page).getByRole('status')).toHaveText('Página 1');
  await expect(pager(page).getByRole('button', { name: 'Página siguiente' })).toBeDisabled();
  expect(boundary.catalogRequests.some((request) => request.q === 'horizonte' && request.offset === 0 && request.limit === 25)).toBe(true);
  await releaseLink(page, 25).click();
  await expect(page.getByRole('heading', { name: 'Álbum sintético 25', exact: true, level: 1 })).toBeVisible();
  await expect(page.getByText(trackTitle, { exact: true })).toBeVisible();
  await page.goBack();
  await expect(searchField(page)).toHaveValue('horizonte');
  await expect(catalogHeading(page, 25)).toBeVisible();
  expect(new URL(page.url()).searchParams.has('page')).toBe(false);
  await page.goto('/musica/release-sintetico-25');
  await expect(page.getByRole('heading', { name: 'Álbum sintético 25', exact: true, level: 1 })).toBeVisible();
  await expect(page.getByText(trackTitle, { exact: true })).toBeVisible();
  await assertBoundary(boundary, testInfo);
});
