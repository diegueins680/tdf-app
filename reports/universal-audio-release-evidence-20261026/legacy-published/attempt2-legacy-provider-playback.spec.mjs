import { expect, test } from '/private/tmp/tdf-legacy-audio-links-20261026/node_modules/@playwright/test/index.mjs';

test.use({ serviceWorkers: 'block' });

// Render the real FanHub/ReleaseFeed using synthetic account and release data.
// Context-level routing also intercepts provider iframes and new-tab navigation:
// no request reaches a provider, and no external playback/decoder claim is made.
async function fixture(page, context, baseURL, artistFallback = false) {
  const appOrigin = new URL(baseURL).origin;
  const providerUrl = artistFallback
    ? 'https://www.youtube.com/channel/synthetic-artist'
    : 'https://open.spotify.com/track/synthetic-release';
  const entryVerifications = [];
  const state = { providerUrl, releaseReads: 0, unexpectedMutations: [], deniedExternal: [], providerNavigations: [], declaredReconciliations: [], loadedEntries: [] };
  const follow = { ffArtistId: 7, ffArtistName: 'Artista Sintético', ffStartedAt: '2026-01-01T00:00:00Z',
    ffSpotifyUrl: null, ffYoutubeUrl: artistFallback ? providerUrl : null };
  const release = { arArtistId: 7, arReleaseId: 42, arTitle: 'Lanzamiento sintético', arReleaseDate: '2026-01-01',
    arDescription: 'Fixture aislado de navegación de proveedor.', arCoverImageUrl: null,
    arSpotifyUrl: artistFallback ? null : providerUrl, arYoutubeUrl: null };
  await page.addInitScript(() => {
    globalThis.__legacyProviderCommands = [];
    globalThis.__legacyProviderTakeovers = [];
    window.addEventListener('tdf-player-load-track', (event) => globalThis.__legacyProviderCommands.push(event.detail));
    window.addEventListener('tdf-radio-takeover', () => globalThis.__legacyProviderTakeovers.push(performance.now()));
  });
  page.on('response', (response) => {
    const url = new URL(response.url());
    if (url.origin === appOrigin && /\/assets\/index-[^/]+\.js$/.test(url.pathname)) {
      state.loadedEntries.push(response.url());
      entryVerifications.push(response.body().then(bytes => {
        expect(bytes.includes(Buffer.from('2e01bbe1263e8761fa2692bd951f828c01142449'))).toBe(true);
      }));
    }
  });
  await context.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['http:', 'https:'].includes(url.protocol)) return route.continue();
    // Published analytics bootstrap is disabled in local build:e2e. Block it
    // before synthetic business-API dispatch; never transmit telemetry.
    if (url.origin !== appOrigin && url.origin !== 'https://api.tdfrecords.net' && ['fetch', 'xhr'].includes(request.resourceType())) {
      state.deniedExternal.push({ origin: url.origin, resourceType: request.resourceType(), method: request.method(), path: url.pathname });
      return route.abort('blockedbyclient');
    }
    if (['fetch', 'xhr'].includes(request.resourceType())) {
      const path = url.pathname;
      if (path === '/catalogs/batch') return route.fulfill({ json: {} });
      // The existing app shell reconciles prior durable onboarding actions on
      // session restore. Fulfill that bootstrap protocol locally without
      // inventing a new first value or permitting any business write.
      if (path === '/session/onboarding/reconcile' && request.method() === 'POST') {
        state.declaredReconciliations.push(path);
        return route.fulfill({ json: { newlyCompleted: false, progress: { eligible: false,
          signupCompletedAt: '2026-01-01T00:00:00Z', onboardingIntent: 'follow_artists', completedAt: '2026-01-01T00:01:00Z',
          firstValue: null, firstValueCompletedAt: null, updatedAt: '2026-01-01T00:01:00Z' } } });
      }
      if (request.method() !== 'GET') {
        state.unexpectedMutations.push({ method: request.method(), path });
        return route.fulfill({ status: 405, json: { error: 'No business writes in provider fixture' } });
      }
      if (path === '/session') return route.fulfill({ json: { username: 'synthetic-fan', displayName: 'Cuenta Sintética', partyId: 42,
        roles: ['customer'], modules: [], featureFlags: [], preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' } } });
      if (path === '/session/onboarding') return route.fulfill({ json: { eligible: false, signupCompletedAt: '2026-01-01T00:00:00Z',
        onboardingIntent: 'follow_artists', completedAt: '2026-01-01T00:01:00Z', firstValue: null, firstValueCompletedAt: null, updatedAt: '2026-01-01T00:01:00Z' } });
      if (path === '/fans/me/follows') return route.fulfill({ json: [follow] });
      if (path === '/fans/artists/7/releases') { state.releaseReads++; return route.fulfill({ json: [release] }); }
      if (path === '/fans/me/profile') return route.fulfill({ json: { fpArtistId: 42, fpDisplayName: 'Cuenta Sintética', fpBio: '', fpCity: '', fpFavoriteGenreIds: [], fpAvatarUrl: null } });
      if (['/fans/artists', '/fans/me/clubs', '/music/releases'].includes(path)) return route.fulfill({ json: [] });
      if (path === '/records/feed') return route.fulfill({ json: { collections: [], releases: [], recordings: [], sessions: [] } });
      if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [] } });
      if (path === '/directory/taxonomies') return route.fulfill({ json: { locale: 'es', cities: [], genres: [], professions: [], serviceOfferings: [], instruments: [] } });
      if (path === '/directory/search') return route.fulfill({ json: { items: [], sponsoredItems: [], facets: { total: 0, entityTypes: {}, cities: [] } } });
      if (path.startsWith('/interactions/targets/')) return route.fulfill({ json: { id: 'synthetic-target', reactions: [], commentCount: 0, rootCount: 0, canReact: false, canComment: false, canShare: false } });
      return route.fulfill({ status: 404, json: { error: 'No synthetic fixture for this read' } });
    }
    if (url.origin === appOrigin && ['GET', 'HEAD'].includes(request.method())) return route.continue();
    if (!['GET', 'HEAD'].includes(request.method())) {
      state.unexpectedMutations.push({ method: request.method(), path: url.pathname });
      return route.abort('blockedbyclient');
    }
    if (request.isNavigationRequest() && url.href === providerUrl) {
      state.providerNavigations.push({ url: url.href, referer: request.headers()['referer'] ?? null });
      return route.fulfill({ contentType: 'text/html', body: '<!doctype html><html><head><title>Synthetic provider destination</title></head><body>Locally fulfilled provider navigation; no external playback.</body></html>' });
    }
    state.deniedExternal.push({ origin: url.origin, resourceType: request.resourceType() });
    return route.abort('blockedbyclient');
  });
  state.verifyEntry = async () => { await Promise.all(entryVerifications); };
  return state;
}

const observed = (page) => page.evaluate(() => ({ commands: globalThis.__legacyProviderCommands, takeovers: globalThis.__legacyProviderTakeovers }));
async function verifyLink(link, href) {
  await expect(link).toHaveAttribute('href', href);
  await expect(link).toHaveAttribute('target', '_blank');
  await expect(link).toHaveAttribute('rel', 'noopener noreferrer');
  const geometry = await link.evaluate((element) => { const rect = element.getBoundingClientRect(); return { width: rect.width, height: rect.height }; });
  expect(geometry.width).toBeGreaterThanOrEqual(44);
  expect(geometry.height).toBeGreaterThanOrEqual(44);
}
async function attach(state, page, testInfo) {
  await state.verifyEntry();
  const commands = await observed(page);
  expect(commands.commands).toEqual([]);
  expect(state.unexpectedMutations).toEqual([]);
  expect(state.loadedEntries.length).toBeGreaterThan(0);
  await testInfo.attach('legacy-provider-boundary.json', { body: JSON.stringify({ ...state, observed: commands,
    limitation: 'Synthetic API/provider-destination responses; no external network, external playback, or native decoder evidence.' }, null, 2), contentType: 'application/json' });
}

test('@critical @mobile-flow PW-LEGACY-PROVIDER-01 release provider dialog cancels cleanly and activates a safe keyboard link without native selection', async ({ page, context, baseURL }, testInfo) => {
  const state = await fixture(page, context, baseURL);
  await page.goto('/fans');
  const listen = page.getByRole('button', { name: 'Escuchar', exact: true });
  await expect(listen).toBeVisible();
  await listen.click();
  const dialog = page.getByRole('dialog', { name: 'Escuchar en Spotify', exact: true });
  await expect(dialog).toBeVisible();
  await expect(dialog.getByText('Lanzamiento sintético · Artista Sintético', { exact: true })).toBeVisible();
  await expect(dialog.getByText('La reproducción y sus controles estarán en la plataforma externa.')).toBeVisible();
  let link = dialog.getByRole('link', { name: 'Abrir en Spotify (nueva pestaña)' });
  await verifyLink(link, state.providerUrl);
  expect(await observed(page)).toEqual({ commands: [], takeovers: [] });
  await page.keyboard.press('Escape');
  await expect(dialog).toBeHidden();
  expect(await observed(page)).toEqual({ commands: [], takeovers: [] });
  await listen.click();
  await expect(dialog).toBeVisible();
  link = dialog.getByRole('link', { name: 'Abrir en Spotify (nueva pestaña)' });
  await link.focus();
  await expect(link).toBeFocused();
  const popupPromise = context.waitForEvent('page');
  await link.press('Enter');
  const popup = await popupPromise;
  try {
    await popup.waitForLoadState('domcontentloaded');
    await expect(popup).toHaveURL(state.providerUrl);
    await expect(popup.getByText('Locally fulfilled provider navigation; no external playback.')).toBeVisible();
    expect(await popup.evaluate(() => window.opener === null)).toBe(true);
    expect(state.providerNavigations).toEqual([{ url: state.providerUrl, referer: null }]);
    expect((await observed(page)).takeovers).toHaveLength(1);
    await expect(page).toHaveURL(/\/fans$/);
  } finally { await popup.close(); }
  expect(state.releaseReads).toBeGreaterThanOrEqual(1);
  await attach(state, page, testInfo);
});

test('@critical @mobile-flow PW-LEGACY-PROVIDER-02 artist fallback remains explicit profile navigation and emits no native media command', async ({ page, context, baseURL }, testInfo) => {
  const state = await fixture(page, context, baseURL, true);
  await page.goto('/fans');
  await page.getByRole('button', { name: 'Escuchar', exact: true }).click();
  const dialog = page.getByRole('dialog', { name: 'Perfil de Artista Sintético en YouTube', exact: true });
  await expect(dialog).toBeVisible();
  await expect(dialog.getByText('Este enlace abre el perfil del artista; no identifica una pista de este lanzamiento.')).toBeVisible();
  await verifyLink(dialog.getByRole('link', { name: 'Abrir en YouTube (nueva pestaña)' }), state.providerUrl);
  expect(await observed(page)).toEqual({ commands: [], takeovers: [] });
  await dialog.getByRole('button', { name: 'Cerrar', exact: true }).click();
  await expect(dialog).toBeHidden();
  expect(await observed(page)).toEqual({ commands: [], takeovers: [] });
  expect(state.providerNavigations).toEqual([]);
  await attach(state, page, testInfo);
});
