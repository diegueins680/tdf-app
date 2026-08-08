#!/usr/bin/env node

import { createServer } from 'node:http';
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { extname, join, normalize, resolve } from 'node:path';
import { chromium } from '/app/node_modules/playwright/index.mjs';

const workspace = process.env.TDF_AUDIT_WORKSPACE ?? '/workspace';
const outputDir = join(workspace, 'docs/feature-discoverability-audit/2026-08-06/screenshots');
const axePath = join(workspace, 'node_modules/axe-core/axe.min.js');
const webRoot = join(workspace, 'tdf-hq-ui/dist');
const mobileRoot = join(workspace, 'tdf-mobile/dist');

for (const required of [webRoot, mobileRoot, axePath]) {
  if (!existsSync(required)) throw new Error(`Missing visual-audit prerequisite: ${required}`);
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
    const requestedPath = decodeURIComponent(new URL(request.url ?? '/', `http://127.0.0.1:${port}`).pathname);
    const relativePath = requestedPath === '/' ? 'index.html' : requestedPath.replace(/^\/+/, '');
    let candidate = resolve(absoluteRoot, normalize(relativePath));
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
  return new Promise((resolveReady, reject) => {
    server.once('error', reject);
    server.listen(port, '127.0.0.1', () => resolveReady(server));
  });
}

const adminRoles = ['admin', 'studio manager', 'manager', 'producer', 'reception', 'artist', 'professor', 'customer', 'fan', 'intern', 'maintenance', 'webmaster'];
const adminModules = ['crm', 'scheduling', 'packages', 'invoicing', 'admin', 'internships', 'ops', 'catalog'];

function sessionFixture(locale, profile = 'admin') {
  const admin = profile === 'admin';
  return {
    username: `audit-${profile}`,
    displayName: admin ? 'Usuario de verificación' : 'Fan de verificación',
    roles: admin ? adminRoles : ['fan', 'customer'],
    modules: admin ? adminModules : ['packages'],
    featureFlags: ['EVENT_DISCOVERY_ENABLED'],
    partyId: admin ? 999999 : 999998,
    preferences: {
      locale,
      currency: 'USD',
      timezone: 'America/Guayaquil',
      countryCode: 'EC',
      supportedLocales: ['es', 'en'],
      supportedCurrencies: ['USD'],
    },
  };
}

const documents = [
  { ddexDocumentId: 101, ddexDocumentFileName: 'fixture-validacion-error.xml', ddexDocumentSha256: 'synthetic-sha-101', ddexDocumentFamily: 'ERN', ddexDocumentVersion: '4.3', ddexDocumentStatus: 'invalid', ddexDocumentMessageId: 'SYNTHETIC-101', ddexDocumentSenderId: 'DPID-SYNTH-A', ddexDocumentRecipientId: 'DPID-SYNTH-TDF', ddexDocumentCreatedAt: '2026-08-06T12:00:00Z' },
  { ddexDocumentId: 102, ddexDocumentFileName: 'fixture-pendiente.xml', ddexDocumentSha256: 'synthetic-sha-102', ddexDocumentFamily: 'ERN', ddexDocumentVersion: '4.3', ddexDocumentStatus: 'validating', ddexDocumentMessageId: 'SYNTHETIC-102', ddexDocumentSenderId: 'DPID-SYNTH-B', ddexDocumentRecipientId: 'DPID-SYNTH-TDF', ddexDocumentCreatedAt: '2026-08-06T12:05:00Z' },
  { ddexDocumentId: 103, ddexDocumentFileName: 'fixture-listo.xml', ddexDocumentSha256: 'synthetic-sha-103', ddexDocumentFamily: 'ERN', ddexDocumentVersion: '4.3', ddexDocumentStatus: 'ready_to_import', ddexDocumentMessageId: 'SYNTHETIC-103', ddexDocumentSenderId: 'DPID-SYNTH-C', ddexDocumentRecipientId: 'DPID-SYNTH-TDF', ddexDocumentCreatedAt: '2026-08-06T12:10:00Z' },
];

const navigationPreferences = [
  { featureId: 'home.dashboard', favorite: true, pinned: true, pinOrder: 0, lastVisitedAt: '2026-08-06T12:00:00Z', useCount: 8, updatedAt: '2026-08-06T12:00:00Z' },
  { featureId: 'label.ddex.inbox', favorite: true, pinned: true, pinOrder: 1, lastVisitedAt: '2026-08-06T12:05:00Z', useCount: 5, updatedAt: '2026-08-06T12:05:00Z' },
  { featureId: 'crm.contacts', favorite: true, pinned: false, pinOrder: null, lastVisitedAt: '2026-08-06T11:00:00Z', useCount: 3, updatedAt: '2026-08-06T11:00:00Z' },
];

const jsonResponse = (value, status = 200) => ({
  status,
  contentType: 'application/json',
  body: JSON.stringify(value),
});

async function installFixtures(page, { locale = 'es', profile = 'admin', mobile = false } = {}) {
  const session = sessionFixture(locale, profile);
  await page.addInitScript(({ storedSession, activeLocale, isMobile }) => {
    localStorage.setItem('tdf-hq-ui/session', JSON.stringify(storedSession));
    localStorage.setItem('tdf-hq-ui/locale', activeLocale);
    localStorage.setItem('tdf-hq-ui/locale-preferences', JSON.stringify(storedSession.preferences));
    if (isMobile) localStorage.setItem('tdf-auth-token', 'synthetic-test-token');
  }, { storedSession: session, activeLocale: locale, isMobile: mobile });

  await page.route('**/*', async (route) => {
    const request = route.request();
    const path = new URL(request.url()).pathname;
    if (path === '/session') return route.fulfill(jsonResponse(session));
    if (path === '/session/preferences') return route.fulfill(jsonResponse(session.preferences));
    if (path === '/navigation/preferences') return route.fulfill(jsonResponse(navigationPreferences));
    if (path.startsWith('/navigation/preferences/') && request.method() !== 'GET') return route.fulfill(jsonResponse(navigationPreferences[0]));
    if (path === '/ddex/documents') return route.fulfill(jsonResponse(documents));
    if (path === '/ddex/documents/101') return route.fulfill(jsonResponse(documents[0]));
    if (path === '/ddex/documents/101/validation-runs/latest') {
      return route.fulfill(jsonResponse({
        reportRunId: 1,
        reportIsValid: false,
        reportIssues: [{ issueSeverity: 'Error', issueLayer: 'XSD', issueCode: 'SYNTH-001', issueMessage: 'El identificador de lanzamiento requiere revisión.', issueLine: 12, issueColumn: 8 }],
      }));
    }
    if (path === '/ddex/partners') return route.fulfill(jsonResponse([{ ddexPartnerId: 1, ddexPartnerName: 'Partner sintético', ddexPartnerDpid: 'DPID-SYNTH-A', ddexPartnerAllowedVersions: ['4.3'] }]));
    if (path === '/access-requests' || path === '/access-requests/reviewable' || path === '/notifications') return route.fulfill(jsonResponse([]));
    if (path.startsWith('/notifications/') || path === '/analytics/events') return route.fulfill({ status: 204, body: '' });
    if (request.url().startsWith('https://api.frankfurter.app/')) return route.fulfill(jsonResponse({ amount: 1, base: 'USD', date: '2026-08-06', rates: { USD: 1 } }));
    if (request.resourceType() === 'fetch' || request.resourceType() === 'xhr') return route.fulfill(jsonResponse([]));
    return route.continue();
  });
}

async function waitForApplication(page) {
  try {
    await page.locator('main, [role="main"]').first().waitFor({ state: 'attached', timeout: 20_000 });
  } catch (error) {
    const body = (await page.locator('body').innerText().catch(() => '')).slice(0, 1_000);
    await page.screenshot({ path: join(outputDir, 'visual-audit-failure.png'), fullPage: true }).catch(() => undefined);
    throw new Error(`Application shell did not render at ${page.url()}. Body: ${body}`, { cause: error });
  }
  await page.waitForTimeout(1_000);
}

async function capture(page, name, { fullPage = false } = {}) {
  await page.screenshot({ path: join(outputDir, `${name}.png`), fullPage, animations: 'disabled' });
}

async function axeAudit(page, name) {
  await page.addScriptTag({ path: axePath });
  const result = await page.evaluate(async () => {
    const axe = window.axe;
    const audit = await axe.run(document, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] } });
    return {
      url: location.pathname,
      violations: audit.violations.map(({ id, impact, help, nodes }) => ({ id, impact, help, nodes: nodes.length, targets: nodes.slice(0, 5).map((node) => node.target), html: nodes.slice(0, 5).map((node) => node.html) })),
      passes: audit.passes.length,
      incomplete: audit.incomplete.length,
    };
  });
  return { name, ...result };
}

const webServer = await staticServer(webRoot, 4174);
const mobileServer = await staticServer(mobileRoot, 4175);
const browser = await chromium.launch({
  headless: true,
  executablePath: process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE ?? '/ms-playwright/chromium-1219/chrome-linux64/chrome',
  args: ['--no-sandbox'],
});
const accessibility = [];
const manualChecks = {};

try {
  const desktop = await browser.newPage({ viewport: { width: 1440, height: 1000 }, reducedMotion: 'reduce' });
  desktop.on('console', (message) => {
    if (message.type() === 'error') console.error(`[browser console] ${message.text()}`);
  });
  desktop.on('pageerror', (error) => console.error(`[browser pageerror] ${error.message}`));
  await installFixtures(desktop, { locale: 'es' });
  await desktop.goto('http://127.0.0.1:4174/label/ddex', { waitUntil: 'domcontentloaded' });
  await waitForApplication(desktop);
  await capture(desktop, 'web-desktop-expanded-ddex-es');
  accessibility.push(await axeAudit(desktop, 'web-desktop-expanded-ddex-es'));

  await desktop.getByRole('button', { name: 'Ocultar menú lateral' }).click();
  await capture(desktop, 'web-desktop-collapsed-ddex-es');
  await desktop.getByRole('button', { name: 'Mostrar menú lateral' }).click();

  const paletteTrigger = desktop.getByRole('button', { name: 'Buscar sección' });
  await paletteTrigger.click();
  const paletteInput = desktop.getByRole('textbox', { name: 'Buscar secciones' });
  await paletteInput.fill('importar ddex');
  await capture(desktop, 'web-command-palette-bilingual-search');
  await desktop.keyboard.press('Escape');
  manualChecks.paletteFocusRestored = await paletteTrigger.evaluate((element) => element === document.activeElement);

  await desktop.locator('header button[aria-haspopup="menu"]').click();
  await desktop.getByRole('menu').waitFor();
  await capture(desktop, 'web-global-quick-create');
  await desktop.keyboard.press('Escape');

  await desktop.goto('http://127.0.0.1:4174/label/ddex/documents/101', { waitUntil: 'domcontentloaded' });
  await waitForApplication(desktop);
  await capture(desktop, 'web-ddex-detail-breadcrumbs');
  accessibility.push(await axeAudit(desktop, 'web-ddex-detail-breadcrumbs'));

  const english = await browser.newPage({ viewport: { width: 1440, height: 1000 }, reducedMotion: 'reduce' });
  await installFixtures(english, { locale: 'en' });
  await english.goto('http://127.0.0.1:4174/label/ddex', { waitUntil: 'domcontentloaded' });
  await waitForApplication(english);
  await capture(english, 'web-desktop-expanded-ddex-en');
  await english.close();

  const tablet = await browser.newPage({ viewport: { width: 768, height: 1024 }, reducedMotion: 'reduce' });
  await installFixtures(tablet, { locale: 'es' });
  await tablet.goto('http://127.0.0.1:4174/label/ddex', { waitUntil: 'domcontentloaded' });
  await waitForApplication(tablet);
  await capture(tablet, 'web-tablet-ddex-es');
  accessibility.push(await axeAudit(tablet, 'web-tablet-ddex-es'));
  await tablet.close();

  const mobileWeb = await browser.newPage({ viewport: { width: 390, height: 844 }, reducedMotion: 'reduce', isMobile: true, hasTouch: true });
  await installFixtures(mobileWeb, { locale: 'es' });
  await mobileWeb.goto('http://127.0.0.1:4174/label/ddex', { waitUntil: 'domcontentloaded' });
  await waitForApplication(mobileWeb);
  await mobileWeb.getByRole('button', { name: /Mostrar menú lateral|Ocultar menú lateral/ }).click();
  await capture(mobileWeb, 'web-mobile-drawer-ddex-es');
  accessibility.push(await axeAudit(mobileWeb, 'web-mobile-drawer-ddex-es'));
  await mobileWeb.close();

  const narrow = await browser.newPage({ viewport: { width: 320, height: 800 }, reducedMotion: 'reduce', isMobile: true, hasTouch: true });
  await installFixtures(narrow, { locale: 'es' });
  await narrow.goto('http://127.0.0.1:4174/label/ddex', { waitUntil: 'domcontentloaded' });
  await waitForApplication(narrow);
  await capture(narrow, 'web-320px-ddex-es');
  manualChecks.narrowViewportHasBodyOverflow = await narrow.evaluate(() => document.documentElement.scrollWidth > document.documentElement.clientWidth);
  manualChecks.changedSurfaceTargetExceptions = await narrow.evaluate(() => {
    const targets = [...document.querySelectorAll('header button, nav button, main button, main a')].filter((element) => {
      const style = getComputedStyle(element);
      return style.visibility !== 'hidden' && style.display !== 'none';
    });
    return targets.flatMap((element) => {
      const rect = element.getBoundingClientRect();
      const size = Math.min(rect.width, rect.height);
      if (size >= 44) return [];
      return [{
        name: element.getAttribute('aria-label') ?? element.textContent?.trim().slice(0, 80) ?? element.tagName,
        tag: element.tagName,
        width: Math.round(rect.width),
        height: Math.round(rect.height),
      }];
    });
  });
  accessibility.push(await axeAudit(narrow, 'web-320px-ddex-es'));
  await narrow.close();

  const locked = await browser.newPage({ viewport: { width: 1280, height: 900 }, reducedMotion: 'reduce' });
  await installFixtures(locked, { locale: 'es', profile: 'fan' });
  await locked.goto('http://127.0.0.1:4174/admin/users', { waitUntil: 'domcontentloaded' });
  await waitForApplication(locked);
  await capture(locked, 'web-locked-feature-403');
  accessibility.push(await axeAudit(locked, 'web-locked-feature-403'));

  await locked.goto('http://127.0.0.1:4174/access-requests/new?feature=admin.users&action=view', { waitUntil: 'domcontentloaded' });
  await waitForApplication(locked);
  await capture(locked, 'web-internal-access-request');
  accessibility.push(await axeAudit(locked, 'web-internal-access-request'));
  await locked.close();

  const mobileApp = await browser.newPage({ viewport: { width: 390, height: 844 }, reducedMotion: 'reduce', isMobile: true, hasTouch: true });
  mobileApp.on('console', (message) => {
    if (message.type() === 'error') console.error(`[mobile browser console] ${message.text()}`);
  });
  mobileApp.on('pageerror', (error) => console.error(`[mobile browser pageerror] ${error.message}`));
  await installFixtures(mobileApp, { locale: 'es', mobile: true });
  await mobileApp.goto('http://127.0.0.1:4175/more', { waitUntil: 'domcontentloaded' });
  await mobileApp.waitForTimeout(5_000);
  await capture(mobileApp, 'mobile-app-feature-explorer');
  accessibility.push(await axeAudit(mobileApp, 'mobile-app-feature-explorer'));
  await mobileApp.close();

  writeFileSync(join(outputDir, 'accessibility-results.json'), `${JSON.stringify(accessibility, null, 2)}\n`);
  writeFileSync(join(outputDir, 'manual-checks.json'), `${JSON.stringify(manualChecks, null, 2)}\n`);
  console.log(JSON.stringify({ screenshots: 12, accessibilitySurfaces: accessibility.length, manualChecks }, null, 2));
} finally {
  await browser.close();
  await Promise.all([
    new Promise((resolveClose) => webServer.close(resolveClose)),
    new Promise((resolveClose) => mobileServer.close(resolveClose)),
  ]);
}
