import { chromium, devices, expect } from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs';
import { readFileSync, writeFileSync } from 'node:fs';
const root = '/private/tmp/tdf-artist-639-qualification-20261029';
const quality = JSON.parse(readFileSync(`${root}/full-quality.json`, 'utf8'));
const baseURL = 'http://127.0.0.1:41439';
const browser = await chromium.launch({ headless: true, executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' });
const results = [];
try {
  for (const profile of ['desktop', 'phone']) for (const network of ['valid', 'broken']) {
    const context = await browser.newContext(profile === 'desktop'
      ? { viewport: { width: 1440, height: 1000 }, locale: 'es-EC', timezoneId: 'America/Guayaquil' }
      : { ...devices['iPhone 13'], defaultBrowserType: 'chromium', locale: 'es-EC', timezoneId: 'America/Guayaquil' });
    const page = await context.newPage();
    const errors = [], writes = [], reads = [];
    const label = `${profile}-${network}`;
    page.on('pageerror', error => errors.push(error.message));
    await page.addInitScript(() => localStorage.setItem('tdf-hq-ui/locale', 'es'));
    const overview = {
      aeoProfiles: [{ apeArtistId: 115, apeArtistName: 'Verde 70', apeReviewStatus: 'verified', apeMissingFields: ['achievements'], apeBrokenFields: ['heroImageUrl'], apeConfidence: 0.9 }],
      aeoSources: [
        { arsId: 1, arsArtistId: 115, arsSourceUrl: 'https://images.example.test/photo.jpg', arsSourceType: `link_validation_${network}`, arsSupportedFields: 'heroImageUrl', arsRetrievedAt: '2026-10-26T10:00:00Z' },
        { arsId: 2, arsArtistId: 115, arsSourceUrl: 'https://images.example.test/photo.jpg', arsSourceType: 'link_validation_broken', arsSupportedFields: 'heroImageUrl', arsRetrievedAt: '2026-10-26T10:00:01Z', arsAttribution: '{"reason":"third_party_hotlink_requires_authorized_ingestion"}' },
      ],
      aeoInventory: [{ airId: 1, airArtistId: 115, airSourceType: 'social_artist_profile', airSourceRecordId: '50', airOriginalName: 'Verde 70', airNormalizedName: 'verde 70', airDisposition: 'matched', airFirstSeenAt: '2026-10-26T10:00:00Z', airLastSeenAt: '2026-10-26T10:00:00Z' }],
      aeoIdentityCandidates: [
        { aicId: 1, aicArtistId: 116, aicInventoryReferenceId: 2 },
        { aicId: 2, aicArtistId: null, aicInventoryReferenceId: 1 },
        { aicId: 3, aicArtistId: 115, aicInventoryReferenceId: 3 },
        { aicId: 4, aicArtistId: 116, aicInventoryReferenceId: 1 },
      ].map(row => ({ ...row, aicProvider: 'external_research', aicConfidence: 0.5, aicStatus: 'pending', aicEvidence: '{}', aicCreatedAt: '2026-10-26T10:00:00Z', aicUpdatedAt: '2026-10-26T10:00:00Z' })),
      aeoSuggestions: [], aeoChanges: [], aeoRuns: [], aeoMedia: [],
    };
    try {
      await page.route('**/*', async route => {
        const request = route.request(), url = new URL(request.url());
        if (request.method() !== 'GET') { writes.push({ method: request.method(), path: url.pathname }); return route.abort(); }
        if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === baseURL ? route.continue() : route.abort();
        const path = url.pathname.replace(/^\/api(?=\/)/, '');
        reads.push(path);
        if (path === '/session') return route.fulfill({ json: { username: 'reviewer@example.test', displayName: 'Synthetic reviewer', roles: ['Admin'], modules: ['Admin', 'Settings', 'Artists'], featureFlags: [], partyId: 99001 } });
        if (path === '/session/preferences') return route.fulfill({ json: { localeId: '', locale: 'es', currencyId: '', currency: 'USD', timezone: 'America/Guayaquil', countryId: null, countryCode: 'EC' } });
        if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
        if (path === '/version') return route.fulfill({ json: { commit: quality.sourceCommit, version: 'synthetic' } });
        if (path === '/admin/artists/enrichment/overview') return route.fulfill({ json: overview });
        if (path === '/catalogs/batch') return route.fulfill({ json: { catalogs: [] } });
        return route.fulfill({ status: 404, json: { error: 'No synthetic fixture for this read' } });
      });
      await page.goto(`${baseURL}/admin/artistas-enriquecimiento`, { waitUntil: 'domcontentloaded' });
      await expect(page.getByRole('heading', { name: 'Verde 70', exact: true })).toBeVisible();
      await expect(page.getByText('Cómo verificar un artista', { exact: true })).toBeVisible();
      await expect(page.getByText('Verificado', { exact: true })).toBeVisible();
      await expect(page.getByText('Imagen externa: revisar autorización de uso', { exact: true })).toBeVisible();
      const broken = page.getByText('Link roto: heroImageUrl', { exact: true });
      if (network === 'broken') await expect(broken).toBeVisible();
      else await expect(broken).toHaveCount(0);
      await expect(page.getByRole('tab', { name: 'Identidades (4)', exact: true })).toBeVisible();
      await page.getByRole('combobox', { name: 'Artista', exact: true }).click();
      await page.getByRole('option', { name: 'Verde 70', exact: true }).click();
      await expect(page.getByRole('tab', { name: 'Identidades (3)', exact: true })).toBeVisible();
      await page.getByRole('button', { name: 'Filtrar links rotos', exact: true }).click();
      const card = page.getByRole('heading', { name: 'Verde 70', exact: true });
      if (network === 'valid') await expect(card).toHaveCount(0);
      else await expect(card).toBeVisible();
      const viewport = await page.evaluate(() => ({ compatMode: document.compatMode, client: document.documentElement.clientWidth, scroll: document.documentElement.scrollWidth, inner: innerWidth, scale: visualViewport?.scale }));
      if (viewport.compatMode !== 'CSS1Compat' || viewport.scroll > viewport.client || Math.abs(viewport.scale - 1) >= .0005) throw new Error(`Viewport contract failed: ${JSON.stringify(viewport)}`);
      const allowedBlockedAppIntents = new Set(['POST /session/onboarding/reconcile', 'DELETE /radio/presence', 'POST /navigation/preferences/admin.artist-enrichment/visit']);
      if (errors.length || writes.some(row => !allowedBlockedAppIntents.has(`${row.method} ${row.path}`))) throw new Error(`Unexpected browser errors/write intents: ${JSON.stringify({ errors, writes })}`);
      await page.screenshot({ path: `${root}/browser-${label}.png`, fullPage: true });
      results.push({ profile, network, sourceCommit: quality.sourceCommit, passed: true, viewport, errors, blockedAppWriteIntents: writes, allWriteIntentsAborted: true, artistWriteIntents: 0, interceptedReads: reads, assertions: 'Verification guide/status; network versus rights; selected/ref-linked competing identity3; broken-only filtering; viewport.' });
      console.log(`PASS ${label}`);
    } catch (error) {
      await page.screenshot({ path: `${root}/browser-${label}-failure.png`, fullPage: true }).catch(() => {});
      writeFileSync(`${root}/browser-${label}-failure.json`, JSON.stringify({ message: error.message, errors, writes, reads, url: page.url() }, null, 2));
      throw error;
    } finally { await context.close(); }
  }
} finally {
  await browser.close();
  writeFileSync(`${root}/browser-results.json`, JSON.stringify({ sourceCommit: quality.sourceCommit, cases: results, passed: results.length, scope: 'Four local Chrome desktop/phone-emulation cases; no real API/provider operations or physical iOS guarantee.' }, null, 2) + '\n');
}
