import assert from 'node:assert/strict';
import { writeFileSync } from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const source = process.env.TDF_PILOT_SOURCE;
const out = process.env.TDF_PILOT_OUT;
const { chromium, devices, expect } = await import(pathToFileURL(path.join(source, 'node_modules/@playwright/test/index.mjs')));
const base = 'http://127.0.0.1:4257';
const api = 'http://127.0.0.1:18714';
const password = 'Synthetic-response-42!';
const changeError = 'No pudimos confirmar el cambio de seguimiento. Comprueba tu conexión y vuelve a intentarlo.';
const report = { status: 'running', humanParticipants: 0, productionWrites: false, fanIds: [], cases: [], scope: 'Actual isolated API/PostgreSQL221 and candidate web. Response loss, pre-commit failure and read503 are explicit browser fault adapters; no real providers or human usability claim.' };

async function request(endpoint, { method = 'GET', body, token, status = 200 } = {}) {
  const r = await fetch(api + endpoint, { method, headers: { ...(body ? { 'Content-Type': 'application/json' } : {}), ...(token ? { Authorization: `Bearer ${token}` } : {}) }, ...(body ? { body: JSON.stringify(body) } : {}) });
  const text = await r.text();
  assert.equal(r.status, status, `${method} ${endpoint}: ${text}`);
  return text ? JSON.parse(text) : null;
}
async function signup(email, firstName) {
  await request('/signup', { method: 'POST', body: { firstName, lastName: 'Fixture', email, password, termsAccepted: true, termsVersion: 'tdf-account-terms-v1' } });
  return request('/login', { method: 'POST', body: { username: email, password } });
}
async function login(page, email, redirect) {
  await page.goto(base + '/login?redirect=' + encodeURIComponent(redirect));
  await page.locator('input[autocomplete="username"]').fill(email);
  await page.locator('input[autocomplete="current-password"]').fill(password);
  await page.locator('button[type="submit"]').click();
  await expect(page).not.toHaveURL(/\/login/, { timeout: 30_000 });
}
const artist = await signup('artist@response.example.test', 'Artista Respuesta');
await request('/artists/me/activate', { method: 'POST', body: {}, token: artist.token });
const profile = await request('/fans/me/artist-profile', { token: artist.token });
report.artistId = artist.partyId;
const browser = await chromium.launch();
const contexts = [];
let currentPage = null;
try {
  for (const surface of ['public', 'community']) {
    for (const scenario of ['lost-response', 'lost-response-read-fails', 'before-commit']) {
      const mobile = scenario === 'lost-response-read-fails';
      const email = `${surface}-${scenario}@response.example.test`;
      const fan = await signup(email, 'Fan Respuesta');
      report.fanIds.push(fan.partyId);
      const context = await browser.newContext({ ...(mobile ? devices['Pixel 7'] : {}), locale: 'es-EC', serviceWorkers: 'block' });
      contexts.push(context);
      const row = { surface, scenario, formFactor: mobile ? 'Pixel7-emulated-web' : 'desktop-web', postCount: 0, deleteCount: 0, followReads: 0, droppedCommittedResponses: [], firstValueReceipts: [] };
      report.cases.push(row);
      let writeFault = scenario === 'before-commit' ? 'before' : 'after';
      let unavailableRead = false;
      let failNextRead = scenario === 'lost-response-read-fails';
      await context.route('**/*', async route => {
        const r = route.request();
        const u = new URL(r.url());
        if (![base, api].includes(u.origin)) return route.abort('blockedbyclient');
        if (u.origin === api && u.pathname === '/fans/me/follows' && r.method() === 'GET') {
          row.followReads++;
          if (unavailableRead) return route.fulfill({ status: 503, contentType: 'application/json', body: JSON.stringify({ error: 'Synthetic confirmation read unavailable' }) });
        }
        if (u.origin === api && u.pathname === `/fans/me/follows/${artist.partyId}` && ['POST', 'DELETE'].includes(r.method())) {
          if (r.method() === 'POST') row.postCount++; else row.deleteCount++;
          const fault = writeFault;
          writeFault = null;
          if (fault === 'before') return route.abort('failed');
          if (fault === 'after') {
            const response = await route.fetch();
            assert.equal(response.status(), 200); // Existing Servant Delete '[JSON] NoContent contract.
            row.droppedCommittedResponses.push({ method: r.method(), status: response.status() });
            if (failNextRead) { unavailableRead = true; failNextRead = false; }
            return route.abort('failed');
          }
          return route.continue();
        }
        if (!['GET', 'HEAD', 'OPTIONS'].includes(r.method()) && (u.origin !== api || !['/login', '/session/logout', '/session/onboarding/reconcile', '/session/onboarding/complete', '/radio/presence'].includes(u.pathname))) {
          throw new Error(`Unexpected browser write: ${r.method()} ${u.pathname}`);
        }
        return route.continue();
      });
      const page = await context.newPage();
      currentPage = page;
      page.on('response', async response => {
        if (response.url() === api + '/session/onboarding/complete' && response.ok()) {
          try { const value = await response.json(); row.firstValueReceipts.push({ newlyCompleted: value.newlyCompleted }); } catch { /* SQL and read-only API assertions remain mandatory. */ }
        }
      });
      const redirect = surface === 'public'
        ? `/artista/${artist.partyId}?resume=follow&artistId=${artist.partyId}&source=fixture#music`
        : '/fans';
      await login(page, email, redirect);
      const button = following => page.getByRole('button', { name: surface === 'public' ? (following ? `Dejar de seguir a ${profile.apDisplayName}` : new RegExp(`^(Seguir ahora|Seguir a ${profile.apDisplayName})$`)) : following ? 'Siguiendo' : 'Seguir', exact: true }).last();
      await expect(button(false)).toBeEnabled();
      const before = row.followReads;
      await button(false).click();
      if (scenario === 'before-commit') {
        await expect(page.getByText(changeError, { exact: true })).toBeVisible();
        await expect(button(false)).toBeEnabled();
        await expect.poll(() => row.followReads).toBe(before + 1);
        assert.equal(row.postCount, 1);
        assert.deepEqual(await request('/fans/me/follows', { token: fan.token }), []);
        const progress = await request('/session/onboarding', { token: fan.token });
        assert.equal(progress.firstValue, null);
        row.noRelationshipOrFirstValueAfterPreCommitFailure = true;
        await button(false).click(); // Explicit user retry, never automatic recovery.
      } else if (scenario === 'lost-response-read-fails') {
        await expect(page.getByText(changeError, { exact: true })).toBeVisible();
        await expect(button(false)).toBeDisabled();
        assert.equal(row.postCount, 1);
        assert.equal((await request('/fans/me/follows', { token: fan.token })).length, 1);
        unavailableRead = false;
        await page.getByRole('button', { name: 'Reintentar', exact: true }).click();
      }
      await expect(button(true)).toBeEnabled();
      await expect(page.getByText(changeError, { exact: true })).toHaveCount(0);
      assert.equal(row.postCount, scenario === 'before-commit' ? 2 : 1);
      const follows = await request('/fans/me/follows', { token: fan.token });
      assert.equal(follows.length, 1); assert.equal(follows[0].ffArtistId, artist.partyId);
      await expect.poll(async () => (await request('/session/onboarding', { token: fan.token })).firstValue).toBe('artist_followed');
      row.serverFirstValueConfirmed = true;
      if (surface === 'public') {
        const u = new URL(page.url()); assert.equal(u.searchParams.get('resume'), null); assert.equal(u.searchParams.get('artistId'), null);
        assert.equal(u.searchParams.get('source'), 'fixture'); assert.equal(u.hash, '#music'); row.unrelatedDeepLinkPreserved = true;
      }
      if (scenario === 'lost-response') {
        const priorReads = row.followReads;
        writeFault = 'after'; await button(true).click();
        await expect(button(false)).toBeEnabled();
        await expect(page.getByText(changeError, { exact: true })).toHaveCount(0);
        assert.equal(row.deleteCount, 1); assert.equal(row.followReads, priorReads + 1);
        assert.deepEqual(await request('/fans/me/follows', { token: fan.token }), []);
        row.committedUnfollowRecoveredWithoutReplay = true;
        await button(false).click(); await expect(button(true)).toBeEnabled();
        assert.equal(row.postCount, 2); assert.equal(row.deleteCount, 1);
      }
      const logoutResponse = page.waitForResponse(r => r.url() === api + '/session/logout' && r.request().method() === 'POST');
      await page.getByRole('button', { name: 'Abrir menú de sesión', exact: true }).click();
      await page.getByRole('menuitem', { name: 'Cerrar sesión', exact: true }).click();
      assert.ok([200, 204].includes((await logoutResponse).status()));
      await expect(page).toHaveURL(/\/login/);
      const sessionAfterLogout = await context.request.get(api + '/session');
      assert.equal(sessionAfterLogout.status(), 200);assert.equal(await sessionAfterLogout.json(), null);
      row.actualLogoutAndCookieAbsence = true;
      // A new browser context authenticates normally, proving persistence beyond cache.
      const later = await browser.newContext({ ...(!mobile ? devices['Pixel 7'] : {}), locale: 'es-EC', serviceWorkers: 'block' });
      contexts.push(later);
      await later.route('**/*', route => [base, api].includes(new URL(route.request().url()).origin) ? route.continue() : route.abort('blockedbyclient'));
      const returned = await later.newPage();
      await login(returned, email, `/artista/${artist.partyId}`);
      await expect(returned.getByRole('button', { name: `Dejar de seguir a ${profile.apDisplayName}`, exact: true })).toBeEnabled();
      row.persistedAfterIndependentWebLogin = true;
      await later.close(); await context.close();
      row.status = 'VERIFIED-isolated-only';
      writeFileSync(path.join(out, 'follow-response.json'), JSON.stringify(report, null, 2) + '\n');
      console.log(`${surface} ${scenario}: real API state and independent login PASS`);
    }
  }
  report.status = 'VERIFIED-isolated-only';
} catch (error) {
  report.status = 'FAILED';
  report.failure = String(error);
  // Capture only fictional fixture state, after an assertion fails. No extra
  // waits/listeners are introduced into successful user actions.
  if (currentPage) {
    try {
      report.failureSnapshot = await currentPage.evaluate(() => ({
        url: location.href, historyKey: history.state?.key, body: document.body.innerText,
        buttons: Array.from(document.querySelectorAll('button')).map(button => ({
          label: button.getAttribute('aria-label') ?? button.textContent, disabled: button.disabled,
        })),
      }));
      await currentPage.screenshot({ path: path.join(out, 'failure.png'), fullPage: true });
      const response = await currentPage.context().request.get(api + '/fans/me/follows');
      report.failureServerFollows = { status: response.status(), rows: response.ok() ? await response.json() : null };
    } catch (captureError) { report.captureError = String(captureError); }
  }
  throw error;
} finally {
  await Promise.allSettled(contexts.map(c => c.close()));
  await browser.close();
  report.browserClosed = true;
  writeFileSync(path.join(out, 'follow-response.json'), JSON.stringify(report, null, 2) + '\n');
}
