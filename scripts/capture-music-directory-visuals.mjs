#!/usr/bin/env node

import { createServer } from 'node:http';
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { extname, join, normalize, resolve } from 'node:path';
import { chromium } from '/app/node_modules/playwright/index.mjs';

const workspace = process.env.TDF_DIRECTORY_WORKSPACE ?? '/workspace';
const outputDir = join(workspace, 'docs/music-directory/screenshots');
const webRoot = join(workspace, 'tdf-hq-ui/dist');
const mobileRoot = join(workspace, 'tdf-mobile/dist');
const axePath = join(workspace, 'node_modules/axe-core/axe.min.js');

for (const required of [webRoot, mobileRoot, axePath]) {
  if (!existsSync(required)) throw new Error(`Missing visual-evidence prerequisite: ${required}`);
}
mkdirSync(outputDir, { recursive: true });

const contentTypes = {
  '.css': 'text/css; charset=utf-8',
  '.html': 'text/html; charset=utf-8',
  '.ico': 'image/x-icon',
  '.js': 'text/javascript; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.png': 'image/png',
  '.svg': 'image/svg+xml',
  '.webp': 'image/webp',
  '.woff': 'font/woff',
  '.woff2': 'font/woff2',
};

function staticServer(root, port) {
  const absoluteRoot = resolve(root);
  const server = createServer((request, response) => {
    const pathname = decodeURIComponent(new URL(request.url ?? '/', `http://127.0.0.1:${port}`).pathname);
    const relative = pathname === '/' ? 'index.html' : pathname.replace(/^\/+/, '');
    let candidate = resolve(absoluteRoot, normalize(relative));
    if (!candidate.startsWith(`${absoluteRoot}/`) && candidate !== absoluteRoot) {
      response.writeHead(400).end('Bad request');
      return;
    }
    if (!existsSync(candidate) && existsSync(`${candidate}.html`)) candidate = `${candidate}.html`;
    if (!existsSync(candidate) || statSync(candidate).isDirectory()) candidate = join(absoluteRoot, 'index.html');
    response.writeHead(200, {
      'Content-Type': contentTypes[extname(candidate)] ?? 'application/octet-stream',
      'Cache-Control': 'no-store',
    });
    response.end(readFileSync(candidate));
  });
  return new Promise((ready, reject) => {
    server.once('error', reject);
    server.listen(port, '127.0.0.1', () => ready(server));
  });
}

const ids = {
  quito: '11111111-1111-4111-8111-111111111111',
  guayaquil: '22222222-2222-4222-8222-222222222222',
  bassist: '33333333-3333-4333-8333-333333333333',
  producer: '44444444-4444-4444-8444-444444444444',
  bass: '55555555-5555-4555-8555-555555555555',
  guitar: '66666666-6666-4666-8666-666666666666',
  rock: '77777777-7777-4777-8777-777777777777',
  recordingService: '77777777-7777-4777-8777-777777777778',
  usd: '77777777-7777-4777-8777-777777777779',
};

const taxonomy = (id, code, name) => ({ id, code, slug: code, name, parentId: null, requirements: {} });
const taxonomies = {
  locale: 'es',
  professions: [taxonomy(ids.bassist, 'bajista', 'Bajista'), taxonomy(ids.producer, 'productor-musical', 'Productor musical')],
  classifiedCategories: [
    taxonomy('88888888-8888-4888-8888-888888888881', 'busco-musico', 'Busco músico'),
    taxonomy('88888888-8888-4888-8888-888888888882', 'trabajo-remunerado', 'Trabajo remunerado'),
  ],
  compensationTypes: [taxonomy('99999999-9999-4999-8999-999999999999', 'rango', 'Rango negociable')],
  serviceOfferings: [{ ...taxonomy(ids.recordingService, 'grabacion', 'Grabación'), currencyId: ids.usd }],
  currencies: [{ ...taxonomy(ids.usd, 'USD', 'Dólar estadounidense'), symbol: '$', minorUnits: 2 }],
  instruments: [taxonomy(ids.bass, 'bajo-electrico', 'Bajo eléctrico'), taxonomy(ids.guitar, 'guitarra', 'Guitarra')],
  genres: [taxonomy(ids.rock, 'rock', 'Rock')],
  cities: [
    { ...taxonomy(ids.quito, 'quito-ec-p', 'Quito'), countryId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', latitude: -0.180653, longitude: -78.467834 },
    { ...taxonomy(ids.guayaquil, 'guayaquil-ec-g', 'Guayaquil'), countryId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', latitude: -2.189412, longitude: -79.889066 },
  ],
};

const location = { cityId: ids.quito, city: 'Quito', countryCode: 'EC', sector: null, latitude: -0.180653, longitude: -78.467834, precision: 'city', distanceKm: 3.8 };
const item = (overrides) => ({
  id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
  type: 'profile',
  slug: 'perfil-sintetico-bajista',
  title: 'Perfil sintético · Bajista de sesión',
  subtitle: 'Bajista · Productor musical',
  summary: 'Fixture local para verificar búsqueda, filtros y privacidad. Disponible en Quito, remoto y para viajar.',
  imageUrl: null,
  location,
  modality: { onsite: true, remote: true, travel: true },
  taxonomy: { professionIds: [ids.bassist, ids.producer], serviceIds: [ids.recordingService], instrumentIds: [ids.bass], genreIds: [ids.rock] },
  score: 0.91,
  scoreBreakdown: { text: 0.4, semantic: 0.15, proximity: 0.13, completeness: 0.09, activity: 0.07, availability: 0.05, reputation: 0.02 },
  sponsored: false,
  sponsorDisclosure: null,
  effectiveAt: '2026-08-15T10:00:00Z',
  expiresAt: null,
  ...overrides,
});

const organicItems = [
  item({}),
  item({
    id: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc', type: 'classified', slug: 'anuncio-sintetico-busca-bajista',
    title: 'Proyecto busca bajista para fechas en Quito', subtitle: 'Colaboración remunerada',
    summary: 'Anuncio sintético vigente para demostrar postulaciones, varias profesiones y alcance por ciudad.',
    taxonomy: { professionIds: [ids.bassist], serviceIds: [], instrumentIds: [ids.bass], genreIds: [ids.rock] }, expiresAt: '2026-09-14T10:00:00Z', score: 0.84,
  }),
  item({
    id: 'dddddddd-dddd-4ddd-8ddd-dddddddddddd', type: 'event', slug: 'evento-sintetico',
    title: 'Encuentro musical sintético de Quito', subtitle: 'Evento publicado',
    summary: 'Evento local de prueba; no representa una actividad real ni una entrada disponible.', score: 0.76,
    taxonomy: { professionIds: [], serviceIds: [], instrumentIds: [], genreIds: [ids.rock] },
  }),
  item({
    id: 'eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee', type: 'venue', slug: 'venue-sintetico',
    title: 'Venue sintético del centro norte', subtitle: 'Venue publicado',
    summary: 'Ficha sintética con ubicación de ciudad; sin dirección, teléfono ni coordenadas residenciales.', score: 0.72,
    taxonomy: { professionIds: [], serviceIds: [], instrumentIds: [], genreIds: [] },
  }),
];

const searchResponse = {
  items: organicItems,
  sponsoredItems: [item({
    id: 'ffffffff-ffff-4fff-8fff-ffffffffffff', type: 'classified', slug: 'anuncio-sintetico-destacado',
    title: 'Anuncio sintético destacado', subtitle: 'Resultado de demostración',
    summary: 'Separado expresamente del ranking orgánico.', sponsored: true, sponsorDisclosure: 'Patrocinado', score: 0,
  })],
  facets: { entityTypes: { profile: 1, classified: 1, event: 1, venue: 1 }, cities: [{ id: ids.quito, name: 'Quito', count: 4 }], total: 4 },
  nextCursor: null,
};

const publicProfile = {
  id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
  kind: 'person',
  name: 'Perfil sintético · Bajista de sesión',
  slug: 'perfil-sintetico-bajista',
  bio: 'Fixture local para demostrar reputación verificable sin representar a una persona real.',
  experience: 'Interacciones sintéticas solo para evidencia visual aislada.',
  creditsSummary: null,
  portfolio: [],
  links: [],
  equipment: null,
  rates: null,
  availability: { status: 'available', onsite: true, remote: true, travel: true, radiusKm: 40 },
  locations: [location],
  professions: [{ id: ids.bassist, code: 'bajista', name: 'Bajista', headline: null, yearsExperience: 8 }],
  instruments: [{ id: ids.bass, code: 'bajo-electrico', name: 'Bajo eléctrico', proficiency: 'professional' }],
  genres: [{ id: ids.rock, code: 'rock', name: 'Rock' }],
  verification: [],
  reputation: { completeness: 0.91, responseRate: null, medianResponseMinutes: null, completed: 1, reviewAverage: 5, reviewCount: 1 },
  canonicalUrl: '/directorio/perfil-sintetico-bajista',
};

const publicReviews = {
  summary: { profileId: publicProfile.id, average: 5, count: 1 },
  items: [{
    id: 'aaaaaaaa-0000-4000-8000-000000000001',
    rating: 5,
    body: 'Reseña sintética de una colaboración completada; no describe una contratación real.',
    createdAt: '2026-08-16T15:00:00Z',
    verifiedInteractionType: 'confirmed_collaboration',
    authorProfile: { id: 'aaaaaaaa-0000-4000-8000-000000000002', name: 'Proyecto sintético verificado', slug: 'proyecto-sintetico-verificado' },
  }],
  nextCursor: null,
};

const json = (value, status = 200) => ({ status, contentType: 'application/json', body: JSON.stringify(value) });
const fixtureRequests = [];

async function installFixtures(page) {
  await page.route('**/*', async (route) => {
    const request = route.request();
    const pathname = new URL(request.url()).pathname;
    if (request.resourceType() === 'fetch' || request.resourceType() === 'xhr') fixtureRequests.push(pathname);
    if (pathname.endsWith('/session')) return route.fulfill(json({ error: 'authentication_required' }, 401));
    if (pathname.endsWith('/catalogs/batch')) return route.fulfill(json({ revision: 1, catalogs: [] }));
    if (pathname.endsWith('/directory/taxonomies')) return route.fulfill(json(taxonomies));
    if (pathname.endsWith('/directory/search')) return route.fulfill(json(searchResponse));
    if (pathname.endsWith('/directory/suggestions')) return route.fulfill(json([]));
    if (pathname.endsWith('/directory/profiles/perfil-sintetico-bajista/reviews')) return route.fulfill(json(publicReviews));
    if (pathname.endsWith('/directory/profiles/perfil-sintetico-bajista')) return route.fulfill(json(publicProfile));
    if (pathname.endsWith('/v1/latest')) return route.fulfill(json({ amount: 1, base: 'USD', date: '2026-08-15', rates: { USD: 1 } }));
    if (pathname.includes('/export/embed.html')) {
      return route.fulfill({ status: 200, contentType: 'text/html', body: '<!doctype html><html><body style="margin:0;background:#dbeafe;display:grid;place-items:center;font:18px sans-serif"><p>Mapa abierto · Quito (fixture local)</p></body></html>' });
    }
    if (request.resourceType() === 'fetch' || request.resourceType() === 'xhr') return route.fulfill(json([]));
    return route.continue();
  });
}

async function waitForResults(page) {
  try {
    await page.getByText('Resultados orgánicos · 4', { exact: true }).waitFor({ timeout: 30_000 });
  } catch (error) {
    const body = (await page.locator('body').innerText().catch(() => '')).slice(0, 4_000);
    throw new Error(`Directory results did not render at ${page.url()}. Body: ${body}. Requests: ${JSON.stringify(fixtureRequests)}. Browser errors: ${JSON.stringify(browserErrors)}`, { cause: error });
  }
  await page.waitForTimeout(750);
}

async function axeAudit(page, name) {
  await page.addScriptTag({ path: axePath });
  const audit = await page.evaluate(async () => {
    const result = await window.axe.run(document, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } });
    return {
      path: location.pathname,
      violations: result.violations.map(({ id, impact, help, nodes }) => ({ id, impact, help, nodes: nodes.length, targets: nodes.slice(0, 5).map((node) => node.target) })),
      passes: result.passes.length,
      incomplete: result.incomplete.length,
    };
  });
  return { name, ...audit };
}

const webServer = await staticServer(webRoot, 4184);
const mobileServer = await staticServer(mobileRoot, 4185);
const browser = await chromium.launch({
  headless: true,
  executablePath: process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE ?? '/ms-playwright/chromium-1219/chrome-linux64/chrome',
  args: ['--no-sandbox'],
});
const accessibility = [];
const browserErrors = [];

try {
  const desktop = await browser.newPage({ viewport: { width: 1440, height: 1000 }, reducedMotion: 'reduce' });
  desktop.on('pageerror', (error) => browserErrors.push({ surface: 'web-desktop', message: error.message }));
  desktop.on('console', (message) => {
    if (message.type() !== 'error' || message.text().includes('status of 401')) return;
    browserErrors.push({ surface: 'web-desktop-console', message: message.text() });
  });
  await installFixtures(desktop);
  await desktop.goto('http://127.0.0.1:4184/buscar?cityId=11111111-1111-4111-8111-111111111111', { waitUntil: 'domcontentloaded' });
  await waitForResults(desktop);
  await desktop.screenshot({ path: join(outputDir, 'web-desktop-search-list.png'), fullPage: true, animations: 'disabled' });
  accessibility.push(await axeAudit(desktop, 'web-desktop-search-list'));
  await desktop.getByRole('button', { name: 'Mapa' }).click();
  await desktop.getByLabel('Resultados en mapa').waitFor();
  await desktop.locator('iframe[title="Mapa OpenStreetMap de resultados"]').scrollIntoViewIfNeeded();
  await desktop.evaluate(() => { if (document.activeElement instanceof HTMLElement) document.activeElement.blur(); });
  await desktop.waitForTimeout(750);
  await desktop.screenshot({ path: join(outputDir, 'web-desktop-search-map.png'), fullPage: true, animations: 'disabled' });
  accessibility.push(await axeAudit(desktop, 'web-desktop-search-map'));
  await desktop.close();

  const profile = await browser.newPage({ viewport: { width: 1440, height: 1000 }, reducedMotion: 'reduce' });
  profile.on('pageerror', (error) => browserErrors.push({ surface: 'web-profile', message: error.message }));
  await installFixtures(profile);
  await profile.goto('http://127.0.0.1:4184/directorio/perfil-sintetico-bajista', { waitUntil: 'domcontentloaded' });
  const reviewsHeading = profile.getByRole('heading', { name: 'Reseñas verificadas' });
  await reviewsHeading.waitFor({ timeout: 30_000 });
  await reviewsHeading.evaluate((element) => window.scrollTo(0, element.getBoundingClientRect().top + window.scrollY - 120));
  await profile.waitForTimeout(500);
  await profile.screenshot({ path: join(outputDir, 'web-desktop-profile-reviews.png'), animations: 'disabled' });
  accessibility.push(await axeAudit(profile, 'web-desktop-profile-reviews'));
  await profile.close();

  const narrow = await browser.newPage({ viewport: { width: 390, height: 844 }, reducedMotion: 'reduce', isMobile: true, hasTouch: true });
  narrow.on('pageerror', (error) => browserErrors.push({ surface: 'web-narrow', message: error.message }));
  await installFixtures(narrow);
  await narrow.goto('http://127.0.0.1:4184/buscar?cityId=11111111-1111-4111-8111-111111111111', { waitUntil: 'domcontentloaded' });
  await waitForResults(narrow);
  await narrow.screenshot({ path: join(outputDir, 'web-mobile-search.png'), fullPage: true, animations: 'disabled' });
  accessibility.push(await axeAudit(narrow, 'web-mobile-search'));
  await narrow.close();

  const mobile = await browser.newPage({ viewport: { width: 390, height: 844 }, reducedMotion: 'reduce', isMobile: true, hasTouch: true });
  mobile.on('pageerror', (error) => browserErrors.push({ surface: 'expo-web', message: error.message }));
  await installFixtures(mobile);
  await mobile.goto('http://127.0.0.1:4185/directory', { waitUntil: 'domcontentloaded' });
  await waitForResults(mobile);
  await mobile.screenshot({ path: join(outputDir, 'mobile-expo-directory.png'), fullPage: true, animations: 'disabled' });
  accessibility.push(await axeAudit(mobile, 'mobile-expo-directory'));
  await mobile.evaluate(() => {
    const scrollable = [...document.querySelectorAll('*')].find((element) => {
      const style = getComputedStyle(element);
      return element.scrollHeight > element.clientHeight + 300 && ['auto', 'scroll'].includes(style.overflowY);
    });
    if (scrollable) scrollable.scrollTop = 900;
    else window.scrollTo({ top: 900 });
  });
  await mobile.waitForTimeout(500);
  await mobile.screenshot({ path: join(outputDir, 'mobile-expo-directory-results.png'), animations: 'disabled' });
  await mobile.close();

  writeFileSync(join(outputDir, 'accessibility-results.json'), `${JSON.stringify(accessibility, null, 2)}\n`);
  writeFileSync(join(outputDir, 'browser-errors.json'), `${JSON.stringify(browserErrors, null, 2)}\n`);
  console.log(JSON.stringify({ screenshots: 6, accessibility, browserErrors }, null, 2));
} finally {
  await browser.close();
  await Promise.all([
    new Promise((resolveClose) => webServer.close(resolveClose)),
    new Promise((resolveClose) => mobileServer.close(resolveClose)),
  ]);
}
