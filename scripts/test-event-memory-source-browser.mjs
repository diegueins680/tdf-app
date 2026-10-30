import { chromium, devices, expect } from '@playwright/test';
import { spawn } from 'node:child_process';
import { createServer } from 'node:net';
import { readFileSync, writeFileSync, openSync, closeSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
import { verifyNewAccountPublication } from './test-event-memory-signup-browser.mjs';
import { verifyRecoveredPublication } from './test-event-memory-recovery-browser.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const config = JSON.parse(readFileSync(process.argv[2], 'utf8'));
if (config.api !== 'http://127.0.0.1:18886' || config.eventId !== '919143001' || config.accounts.length !== 2
  || config.accounts.some((account) => !account.email.endsWith('@persona.test'))) throw new Error('Only owned synthetic browser fixtures are allowed');
const origin = 'http://127.0.0.1:5206'; const output = config.output;
const result = { status: 'NOT_EXECUTED', api: config.api, sourceRevision: 'recorded by the owning HTTP runner', sourceApiResponses: [], blockedExternalRequests: 0, assertions: [] };
let server, browser, log;
const cleanup = async () => {
  if (server && server.exitCode === null && server.signalCode === null) {
    try { process.kill(-server.pid, 'SIGTERM'); } catch {}
    await Promise.race([new Promise((resolve) => server.once('exit', resolve)), new Promise((resolve) => setTimeout(resolve, 5000))]);
    if (server.exitCode === null && server.signalCode === null) {
      try { process.kill(-server.pid, 'SIGKILL'); } catch {}
      await Promise.race([new Promise((resolve) => server.once('exit', resolve)), new Promise((resolve) => setTimeout(resolve, 5000))]);
    }
  }
  if (browser) { await browser.close().catch(() => {}); browser = null; }
  if (log !== undefined) { closeSync(log); log = undefined; }
};
process.on('SIGTERM', () => { void cleanup().finally(() => process.exit(143)); });
process.on('SIGINT', () => { void cleanup().finally(() => process.exit(130)); });
try {
  await new Promise((resolve, reject) => { const probe = createServer(); probe.once('error', reject); probe.listen(5206, '127.0.0.1', () => probe.close(resolve)); });
  log = openSync(path.join(output, 'browser-vite.log'), 'wx', 0o600);
  server = spawn('npm', ['run', 'dev', '--workspace=tdf-hq-ui', '--', '--host', '127.0.0.1', '--port', '5206', '--strictPort'],
    { cwd: root, detached: true, stdio: ['ignore', log, log], env: { ...process.env, VITE_API_BASE: config.api, VITE_POSTHOG_KEY: '', VITE_GOOGLE_CLIENT_ID: '' } });
  for (let attempt = 0; attempt < 120; attempt += 1) {
    if (server.exitCode !== null) throw new Error('Owned frontend exited before readiness');
    try { if ((await fetch(origin)).ok) break; } catch {}
    if (attempt === 119) throw new Error('Owned frontend readiness deadline');
    await new Promise((resolve) => setTimeout(resolve, 250));
  }
  const bundledBrowser = process.env.TDF_TEST_BUNDLED_BROWSER === '1';
  browser = await chromium.launch({ ...(bundledBrowser ? {} : { channel: 'chrome' }), headless: true });
  result.browserRuntime = { channel: bundledBrowser ? 'playwright-chromium' : 'chrome', version: browser.version() };
  const contexts = [await browser.newContext({ viewport: { width: 1280, height: 900 }, locale: 'es-EC', timezoneId: 'America/Guayaquil', reducedMotion: 'reduce' }),
    await browser.newContext({ ...devices['Pixel 7'], locale: 'es-EC', timezoneId: 'America/Guayaquil', reducedMotion: 'reduce' }),
    await browser.newContext({ ...devices['Pixel 7'], locale: 'es-EC', timezoneId: 'America/Guayaquil', reducedMotion: 'reduce' }),
    await browser.newContext({ ...devices['Pixel 7'], locale: 'es-EC', timezoneId: 'America/Guayaquil', reducedMotion: 'reduce' })];
  for (const context of contexts) await context.route('**/*', async (route) => {
    const url = new URL(route.request().url());
    if ([origin, config.api].includes(url.origin)) return route.continue();
    // The backend's fixture-only HTTPS asset origin maps to the actual local
    // durable asset route. No bytes are fabricated and no external host is called.
    if (url.origin === 'https://assets.persona.test') {
      const response = await route.fetch({ url: config.api + url.pathname });
      return route.fulfill({ response });
    }
    result.blockedExternalRequests += 1; return route.abort('blockedbyclient');
  });
  const pages = [await contexts[0].newPage(), await contexts[1].newPage()];
  for (const page of pages) page.on('response', (response) => {
    const url = new URL(response.url());
    if (url.origin === config.api && (url.pathname.includes('/commands') || url.pathname.endsWith('/moments') || url.pathname === '/login'))
      result.sourceApiResponses.push({ method: response.request().method(), path: url.pathname, status: response.status() });
  });
  const guestDestination = `/social/eventos/${config.eventId}?utm_source=synthetic-invitation#momentos`;
  const destination = `/social/eventos/${config.eventId}?utm_source=synthetic-invitation&compose=1#momentos`;
  const publicResponse = await contexts[0].request.get(`${config.api}/directory/events/${config.eventId}`);
  expect(publicResponse.status()).toBe(200);
  const publicEvent = await publicResponse.json();
  expect(String(publicEvent.id)).toBe(config.eventId);
  expect(publicEvent.workflowStateCode).toBe('completed');
  expect(typeof publicEvent.title).toBe('string');
  result.publicEntry = { publicSnapshotStatus: publicResponse.status(), completed: true, injectedNetworkFailures: 0, protectedGuestMomentReads: 0 };
  const privateDestination = '/social/eventos/919143002?utm_source=synthetic-private-invitation#momentos';
  await pages[0].goto(origin + privateDestination, { waitUntil: 'domcontentloaded' });
  await expect(pages[0].locator('input[autocomplete="username"]')).toBeVisible({ timeout: 30000 });
  expect(new URL(pages[0].url()).searchParams.get('redirect')).toBe(privateDestination);
  await expect(pages[0].getByRole('combobox', { name: 'Tipo de recuerdo' })).toHaveCount(0);
  result.assertions.push('Unavailable public metadata retains private-event authentication and the exact original target');
  for (const [index, page] of pages.entries()) {
    const guestReads = (request) => {
      const url = new URL(request.url());
      if (url.origin === config.api && url.pathname === `/social-events/events/${config.eventId}/moments`) result.publicEntry.protectedGuestMomentReads += 1;
    };
    page.on('request', guestReads);
    if (index === 0) await page.route(`${config.api}/directory/events/${config.eventId}`, async (route) => {
      if (result.publicEntry.injectedNetworkFailures === 0) {
        result.publicEntry.injectedNetworkFailures += 1;
        return route.abort('internetdisconnected');
      }
      return route.continue();
    });
    await page.goto(origin + guestDestination, { waitUntil: 'domcontentloaded' });
    if (index === 0) {
      await expect(page.getByRole('button', { name: 'Reintentar', exact: true })).toBeVisible({ timeout: 30000 });
      await expect(page).toHaveURL(origin + guestDestination);
      await expect(page.locator('input[autocomplete="username"]')).toHaveCount(0);
      await page.getByRole('button', { name: 'Reintentar', exact: true }).click();
    }
    await expect(page.getByRole('heading', { name: publicEvent.title, exact: true })).toBeVisible({ timeout: 30000 });
    await expect(page).toHaveURL(origin + guestDestination);
    await expect(page.locator('input[autocomplete="username"]')).toHaveCount(0);
    const signupLinks = page.locator('a[href*="signup=1"]');
    expect(await signupLinks.count()).toBeGreaterThan(0);
    for (const link of await signupLinks.all()) {
      const signup = new URL(await link.getAttribute('href'), origin);
      expect(signup.searchParams.get('redirect')).toBe(guestDestination);
    }
    expect(result.publicEntry.protectedGuestMomentReads).toBe(0);
    page.off('request', guestReads);
    await page.getByRole('link', { name: 'Compartir mis momentos', exact: true }).scrollIntoViewIfNeeded();
    if (index === 1) await page.screenshot({ path: path.join(output, 'source-public-entry-phone.png'), fullPage: true });
    await page.getByRole('link', { name: 'Compartir mis momentos', exact: true }).click();
    await expect(page.locator('input[autocomplete="username"]')).toBeVisible({ timeout: 30000 });
    expect(new URL(page.url()).searchParams.get('redirect')).toBe(destination);
    await page.locator('input[autocomplete="username"]').fill(config.accounts[index].email);
    await page.locator('input[autocomplete="current-password"]').fill(config.accounts[index].password);
    await page.locator('button[type="submit"]').first().click();
    await expect(page).toHaveURL(new URL(destination, origin).href, { timeout: 30000 });
    await expect(page.getByRole('combobox', { name: 'Tipo de recuerdo' })).toBeVisible({ timeout: 30000 });
  }
  expect(result.publicEntry.injectedNetworkFailures).toBe(1);
  result.assertions.push('Anonymous desktop/phone render completed-event metadata without private moment reads; explicit publication retains attribution through login');
  result.assertions.push('One injected metadata network failure remains on the public event; explicit retry receives real metadata');
  result.assertions.push('Two real isolated sessions sign in and retain event/composer context');
  const [owner, participant] = pages; const caption = 'Prueba sintética de revisión y restauración';
  const card = (page) => page.getByRole('region', { name: /^Publicación de / }).filter({ hasText: caption });
  await owner.getByRole('combobox', { name: 'Tipo de recuerdo' }).click();
  await owner.getByRole('option', { name: 'Solo texto' }).click();
  await owner.getByRole('textbox', { name: 'Describe tu publicación' }).fill(caption);
  await owner.getByRole('button', { name: 'Publicar', exact: true }).click();
  await expect(card(owner)).toBeVisible({ timeout: 30000 });
  await participant.reload(); await expect(card(participant)).toBeVisible();
  // Locator.click scrolls and re-resolves a detached target. A separate scroll
  // can fail when the real session/feed refresh replaces the card after the
  // visibility assertion, before the report action even reaches the backend.
  await card(participant).getByRole('button', { name: 'Denunciar recuerdo' }).click();
  await participant.getByRole('textbox', { name: 'Motivo' }).fill('Revisión sintética del consentimiento de imagen');
  await participant.getByRole('button', { name: 'Confirmar', exact: true }).click();
  await expect(participant.getByRole('dialog')).toHaveCount(0);
  await expect(card(participant)).toBeVisible();
  result.assertions.push('Desktop text publication persists and a phone reader reports it without automatically hiding it');
  await owner.reload();
  await owner.getByRole('button', { name: 'Revisar recuerdos', exact: true }).click();
  const review = owner.getByRole('region', { name: 'Revisión de recuerdos', exact: true });
  await expect(review.getByText('Revisión sintética del consentimiento de imagen', { exact: true })).toBeVisible();
  await review.getByRole('button', { name: 'Cerrar denuncia sin ocultar' }).click();
  await owner.getByRole('textbox', { name: 'Motivo' }).fill('Revisión sintética completada sin retirar el recuerdo');
  await owner.getByRole('button', { name: 'Confirmar', exact: true }).click();
  await expect(owner.getByRole('dialog')).toHaveCount(0);
  await expect(review.getByText(caption, { exact: true })).toHaveCount(0);
  await expect(card(owner)).toBeVisible();
  result.assertions.push('Organizer closes the actual source report without removing the publication');
  let injectedStaleVersion = 0;
  let injectionError;
  await owner.route(`${config.api}/interactions/targets/*/commands`, async (route) => {
    const payload = route.request().postDataJSON();
    if (payload?.command?.operation === 'source.hide' && injectedStaleVersion === 0) {
      const version = Number(payload.command.expectedVersion);
      if (!Number.isSafeInteger(version) || version < 1 || version > Number.MAX_SAFE_INTEGER - 1000000) {
        injectionError = 'The real fixture must have a valid source version';
        return route.continue();
      }
      injectedStaleVersion += 1;
      payload.command.expectedVersion = version + 1000000;
      // Fault injection changes one outgoing version. The real API/DB decides
      // the conflict; no response, entitlement or moderation result is mocked.
      return route.continue({ postData: JSON.stringify(payload) });
    }
    return route.continue();
  });
  const commandResponse = (page, operation) => page.waitForResponse((response) => {
    const request = response.request();
    if (request.method() !== 'POST' || !new URL(response.url()).pathname.endsWith('/commands')) return false;
    return request.postDataJSON()?.command?.operation === operation;
  });
  const hideWithKeyboard = async () => {
    await card(owner).scrollIntoViewIfNeeded();
    await card(owner).getByRole('button', { name: 'Ocultar recuerdo' }).focus(); await owner.keyboard.press('Enter');
    await owner.getByRole('textbox', { name: 'Motivo' }).fill('Ocultación sintética reversible');
    const response = commandResponse(owner, 'source.hide');
    await owner.getByRole('button', { name: 'Confirmar', exact: true }).click();
    return response;
  };
  let hidden = await hideWithKeyboard();
  expect(injectionError).toBeUndefined();
  expect(injectedStaleVersion).toBe(1);
  expect(hidden.status()).toBe(409);
  if (hidden.status() === 409) {
    // An offscreen cached summary may still carry the previous report version.
    // Exercise the explicit recovery path; never replay a stale decision as if
    // it succeeded or count a transient feed refetch as confirmed hiding.
    await expect(owner.getByRole('dialog').getByRole('alert')).toContainText('El recuerdo cambió');
    // MUI correctly makes the background aria-hidden while the dialog is open.
    // Check retained DOM and the real authenticated API, rather than mistaking that
    // accessibility isolation for source deletion or a successful hide.
    await expect(owner.getByRole('region', { name: /^Publicación de /, includeHidden: true }).filter({ hasText: caption })).toHaveCount(1);
    // APIRequestContext shares this isolated browser's HttpOnly session cookie.
    // Cookie-only sessions deliberately need no JS-readable bearer token.
    const unchanged = await owner.request.get(`${config.api}/social-events/events/${config.eventId}/moments?extended`);
    expect(unchanged.status()).toBe(200);
    expect(await unchanged.json()).toEqual(expect.arrayContaining([expect.objectContaining({ emCaption: caption })]));
    await owner.getByRole('button', { name: 'Cancelar', exact: true }).click();
    await owner.reload(); await expect(card(owner)).toBeVisible();
    hidden = await hideWithKeyboard();
    result.assertions.push('One injected mismatched version receives a real 409; explicit cancel/reload/review creates a fresh decision');
  }
  expect(hidden.status()).toBe(200);
  await expect(owner.getByRole('dialog')).toHaveCount(0);
  await expect(card(owner)).toHaveCount(0);
  await participant.reload(); await expect(card(participant)).toHaveCount(0);
  const openReview = owner.getByRole('button', { name: 'Revisar recuerdos', exact: true });
  if (await openReview.count()) await openReview.click();
  await owner.getByRole('button', { name: 'Actualizar revisión' }).click();
  await review.getByRole('button', { name: 'Restaurar recuerdo' }).click();
  await owner.getByRole('textbox', { name: 'Motivo' }).fill('Restauración sintética verificada');
  const restored = commandResponse(owner, 'source.restore');
  await owner.getByRole('button', { name: 'Confirmar', exact: true }).click();
  expect((await restored).status()).toBe(200);
  await expect(owner.getByRole('dialog')).toHaveCount(0);
  await expect(card(owner)).toBeVisible();
  await participant.reload(); await expect(card(participant)).toBeVisible();
  result.assertions.push('Keyboard hide denies both sessions after reload; scoped restoration restores the persisted source');
  for (const [index, page] of pages.entries()) {
    if (!(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1))) throw new Error('Horizontal overflow');
    await card(page).screenshot({ path: path.join(output, index === 0 ? 'source-desktop.png' : 'source-phone.png') });
  }
  await verifyNewAccountPublication({ context: contexts[2], owner, config, origin, output, result });
  // Recovery revokes this participant's old interactive credentials. Its earlier
  // context has completed its assertions; keep the new recovery session isolated.
  await contexts[1].close();
  await verifyRecoveredPublication({ context: contexts[3], owner, config, origin, output, result });
  result.status = 'PASS';
} catch (error) {
  result.status = 'FAIL'; result.error = String(error);
  for (const [index, context] of (browser?.contexts() ?? []).entries()) for (const page of context.pages())
    await page.screenshot({ path: path.join(output, `source-failure-${index}.png`), fullPage: true }).catch(() => {});
  process.exitCode = 1;
} finally {
  await cleanup(); result.frontendStopped = !server || server.exitCode !== null || server.signalCode !== null;
  if (!result.frontendStopped) { result.status = 'FAIL'; result.error = 'Owned frontend did not stop'; process.exitCode = 1; }
  writeFileSync(path.join(output, 'browser-results.json'), `${JSON.stringify(result, null, 2)}\n`, { mode: 0o600 });
}
console.log(JSON.stringify({ status: result.status, assertions: result.assertions.length }));
