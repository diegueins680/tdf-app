import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Synthetic API conformance: no real accounts, provider calls or mutations.
for (const language of ['es', 'en']) {
  test(`Directory complete search and favorite recovery use ${language} @critical`, async ({ page, baseURL }, testInfo) => {
    const english = language === 'en';
    const copy = english ? {
      heading: 'Find the people and opportunities that make music', loading: 'Searching for results',
      query: 'What are you looking for?', searchError: 'The search could not be completed.', retry: 'Retry',
      save: 'Save Synthetic Event to your account', unsave: 'Remove Synthetic Event from your saved items',
      favoriteError: 'We could not confirm this change.', refresh: 'Refresh saved items',
      location: 'Use my location', locationError: 'Your location could not be obtained.',
      map: 'Map', mapTitle: 'OpenStreetMap of results', mapPrivacy: 'The map uses authorized approximate locations.',
    } : {
      heading: 'Encuentra a la gente y las oportunidades que hacen música', loading: 'Buscando resultados',
      query: '¿Qué necesitas?', searchError: 'No se pudo completar la búsqueda.', retry: 'Reintentar',
      save: 'Guardar Synthetic Event en tu cuenta', unsave: 'Quitar Synthetic Event de tus guardados',
      favoriteError: 'No pudimos confirmar el cambio', refresh: 'Consultar guardados',
      location: 'Usar mi ubicación', locationError: 'No se obtuvo tu ubicación.',
      map: 'Mapa', mapTitle: 'Mapa OpenStreetMap de resultados', mapPrivacy: 'El mapa usa ubicaciones aproximadas autorizadas.',
    };
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.addInitScript(locale => {
      if (window.top !== window) return; // Do not configure cross-origin map documents.
      localStorage.setItem('tdf-hq-ui/locale', locale);
      Object.defineProperty(navigator, 'geolocation', { configurable: true, value: {
        getCurrentPosition: (_success, failure) => failure({ code: 1, message: 'Synthetic denial' }),
      } });
    }, language);
    let releaseSearch;
    const searchGate = new Promise(resolve => { releaseSearch = resolve; });
    let failSearch = true, saved = false, writes = 0;
    const taxonomyLanguages = [];
    const origin = new URL(baseURL).origin;
    await page.route('**/*', async route => {
      const request = route.request(), url = new URL(request.url());
      if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
      const path = url.pathname.replace(/^\/api(?=\/)/, '');
      if (path === '/session') return route.fulfill({ json: {
        username: 'synthetic-fan', displayName: 'Cuenta Sintética', partyId: 42, roles: ['customer'], modules: [], featureFlags: [],
        preferences: { locale: language, currency: 'USD', timeZone: 'America/Guayaquil' },
      } });
      if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
      if (path === '/directory/taxonomies') {
        taxonomyLanguages.push(url.searchParams.get('locale'));
        return route.fulfill({ json: { locale: language, professions: [], classifiedCategories: [], compensationTypes: [], serviceOfferings: [], currencies: [], instruments: [], genres: [], cities: [{ id: 'quito-fixture', code: 'quito-ec-p', name: 'Quito' }] } });
      }
      if (path === '/directory/search') {
        await searchGate;
        if (failSearch) return route.fulfill({ status: 503, json: { error: 'Synthetic temporary failure' } });
        return route.fulfill({ json: { items: [{ id: '42', type: 'event', slug: '42', title: 'Synthetic Event', summary: 'Contenido público de prueba.', imageUrl: null, sponsored: false, score: 1, location: { city: 'Quito', countryCode: 'EC', precision: 'city', latitude: -0.18, longitude: -78.46 } }], sponsoredItems: [], facets: { entityTypes: { event: 1 }, cities: [], total: 1 }, nextCursor: null } });
      }
      if (path === '/directory/favorites/event/42') {
        writes++; saved = true; // Server may persist before the client loses its response.
        return route.fulfill({ status: 503, json: { error: 'Synthetic uncertain transport' } });
      }
      if (path === '/directory/favorites') return route.fulfill({ json: saved ? [{ targetKind: 'event', targetId: '42', createdAt: '2026-09-18T12:00:00Z', result: null }] : [] });
      if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [] } });
      return route.fulfill({ json: [] });
    });
    try {
      await page.goto('/buscar?q=music&cityId=quito-fixture');
      await expect(page.locator('html')).toHaveAttribute('lang', language);
      await expect(page.getByRole('heading', { name: copy.heading })).toBeVisible();
      await expect(page.getByRole('progressbar', { name: copy.loading })).toBeVisible();
      releaseSearch();
      const failure = page.getByRole('alert').filter({ hasText: copy.searchError });
      await expect(failure).toBeVisible({ timeout: 20000 });
      await expect(page.getByRole('combobox', { name: copy.query })).toHaveValue('music');
      failSearch = false;
      await failure.getByRole('button', { name: copy.retry }).click();
      await expect(page.getByRole('heading', { name: 'Synthetic Event' })).toBeVisible();
      await expect(page.getByText('Contenido público de prueba.', { exact: true })).toBeVisible();
      expect(taxonomyLanguages).toContain(language);
      await page.getByRole('button', { name: copy.save, exact: true }).click();
      const favoriteError = page.getByRole('alert').filter({ hasText: copy.favoriteError });
      await expect(favoriteError).toBeVisible();
      await favoriteError.getByRole('button', { name: copy.refresh }).click();
      await expect(page.getByRole('button', { name: copy.unsave, exact: true })).toHaveAttribute('aria-pressed', 'true');
      await expect(favoriteError).toHaveCount(0);
      expect(writes).toBe(1);
      await page.evaluate(() => Object.defineProperty(navigator, 'share', { configurable: true, value: async () => { throw new DOMException('Synthetic user cancellation', 'AbortError'); } }));
      await page.getByRole('button', { name: english ? 'Share' : 'Compartir', exact: true }).click();
      await expect(page.getByRole('status').filter({ hasText: english ? 'The content was not shared.' : 'El contenido no se compartió.' })).toBeVisible();
      expect(errors).toEqual([]);
      await page.evaluate(() => {
        Object.defineProperty(navigator, 'share', { configurable: true, value: undefined });
        Object.defineProperty(navigator, 'clipboard', { configurable: true, value: { writeText: async () => { throw new DOMException('Synthetic clipboard denial', 'NotAllowedError'); } } });
      });
      await page.getByRole('button', { name: english ? 'Share' : 'Compartir', exact: true }).click();
      await expect(page.getByRole('alert').filter({ hasText: english ? 'The link could not be shared.' : 'No se pudo compartir el enlace.' })).toBeVisible();
      await page.evaluate(() => Object.defineProperty(navigator, 'clipboard', { configurable: true, value: { writeText: async value => { window.__directoryCopiedUrl = value; } } }));
      await page.getByRole('button', { name: english ? 'Share' : 'Compartir', exact: true }).click();
      await expect(page.getByRole('status').filter({ hasText: english ? 'Link copied.' : 'Enlace copiado.' })).toBeVisible();
      expect(await page.evaluate(() => window.__directoryCopiedUrl)).toBe(origin + '/eventos/42');
      await page.screenshot({ path: testInfo.outputPath('directory-recovered.png'), fullPage: true });
      await page.getByRole('button', { name: copy.location, exact: true }).click();
      await expect(page.getByRole('alert').filter({ hasText: copy.locationError })).toBeVisible();
      await page.getByRole('button', { name: copy.map, exact: true }).click();
      await expect(page.getByTitle(copy.mapTitle, { exact: true })).toBeVisible();
      await expect(page.getByRole('alert').filter({ hasText: copy.mapPrivacy })).toBeVisible();
      await page.addScriptTag({ content: axe.source });
      const violations = await page.evaluate(async () => (await window.axe.run(document)).violations.filter(v => ['serious', 'critical'].includes(v.impact)).map(v => v.id));
      expect(violations).toEqual([]);
      expect(errors).toEqual([]);
    } finally { releaseSearch(); }
  });
}
