import { expect, test } from '@playwright/test';
import { createRequire } from 'node:module';
const require = createRequire(import.meta.url);

// Browser integration only: all API calls are synthetic and all external assets
// are blocked. Database/authorization/media delivery have a separate HTTP lane.
async function fixture(page, baseURL, anonymousStatus) {
  const origin = new URL(baseURL).origin;
  const state = { authenticated: false, signups: [], moments: [], failNextPublication: true };
  const session = () => ({ username: 'memory.new@persona.test', displayName: 'Synthetic participant', partyId: 9,
    roles: ['customer'], modules: [], featureFlags: [], preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' } });
  await page.route('**/*', async (route) => {
    const request = route.request(); const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
    const path = url.pathname;
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/session') return state.authenticated ? route.fulfill({ json: session() })
      : anonymousStatus === 401 ? route.fulfill({ status: 401, json: {} }) : route.fulfill({ status: 200, body: 'null', contentType: 'application/json' });
    if (path === '/signup') { state.signups.push(request.postDataJSON()); state.authenticated = true; return route.fulfill({ json: { token: 'synthetic-memory-token', partyId: 9, roles: ['Customer'], modules: [] } }); }
    if (path.startsWith('/session/onboarding')) return route.fulfill({ json: { newlyCompleted: false, eligible: false, completedAt: '2026-10-25T00:00:00Z', progress: { eligible: false, completedAt: '2026-10-25T00:00:00Z' } } });
    if (path === '/social-events/events/141') return route.fulfill({ json: { eventId: '141', eventOrganizerPartyId: '1', eventTitle: 'Synthetic memory community', eventStart: '2026-10-01T19:00:00Z', eventEnd: '2026-10-02T02:00:00Z', eventWorkflowStateCode: 'completed', eventIsPublic: true, eventPublicListable: true, eventRsvpEligible: false, eventTicketPurchaseEnabled: false, eventArtists: [] } });
    if (path === '/social-events/me/memory-invitations') return route.fulfill({ json: { inApp: false, supportsExtendedMoments: true } });
    if (path === '/social-events/events/141/moments') {
      if (request.method() === 'POST') {
        if (state.failNextPublication) { state.failNextPublication = false; return route.fulfill({ status: 503, body: 'Synthetic service unavailable' }); }
        const payload = request.postDataJSON();
        state.moments.push({ emId: '77', emEventId: '141', emAuthorPartyId: '9', emAuthorName: 'Synthetic participant', emCaption: payload.emCreateCaption, emMediaUrl: payload.emCreateMediaUrl, emMediaType: payload.emCreateMediaType, emCreatedAt: '2026-10-25T08:00:00Z', emUpdatedAt: '2026-10-25T08:00:00Z' });
        return route.fulfill({ json: state.moments[0] });
      }
      expect(url.searchParams.has('extended')).toBe(true);
      return route.fulfill({ json: state.moments });
    }
    if (path === '/social-events/events/141/rsvp') return route.fulfill({ json: null });
    if (path === '/social-events/events/141/rsvp-summary') return route.fulfill({ json: { rsvpAcceptedCount: 3, rsvpMaybeCount: 0 } });
    if (path === '/social-events/events/141/ticket-tiers') return route.fulfill({ json: [] });
    if (path.startsWith('/fans/') && path.includes('notifications')) return route.fulfill({ json: path.includes('count') ? 0 : [] });
    return route.fulfill({ status: 404, json: { error: 'Synthetic fixture unavailable' } });
  });
  return state;
}

// GET /session normally returns 200/null for an anonymous visitor. Retain the
// expired-session 401 boundary as a separate scenario, including its redirect.
for (const entry of [{ name: 'anonymous', status: 200 }, { name: 'expired', status: 401 }]) {
test(`new participant (${entry.name} session) preserves event intent, recovers a failed text publication and finds it after reload @mobile-flow @critical`, async ({ page, baseURL }, testInfo) => {
  const state = await fixture(page, baseURL, entry.status);
  const destination = '/social/eventos/141?compose=1#momentos';
  await page.goto(destination, { waitUntil: 'domcontentloaded' });
  await expect(page).toHaveURL(/\/login\?/);
  const login = new URL(page.url());
  expect(login.searchParams.get('redirect')).toBe(destination);
  // The URL can change before the protected-route redirect finishes. Wait
  // for the usable login screen, then follow the same account action as a user.
  await page.getByRole('button', { name: 'Crear cuenta general', exact: true }).click();
  const dialog = page.getByRole('dialog');
  await expect(dialog).toBeVisible();
  const signup = new URL(page.url());
  expect(signup.searchParams.get('redirect')).toBe(destination);
  expect(signup.searchParams.get('signup')).toBe('1');
  await dialog.getByLabel('Correo').fill('memory.new@persona.test');
  await dialog.getByLabel('Contraseña', { exact: false }).first().fill('synthetic-long-password-141');
  await dialog.getByRole('button', { name: 'Crear e ingresar' }).click();
  await expect(page).toHaveURL(new URL(destination, baseURL).href);
  await page.getByRole('combobox', { name: 'Tipo de recuerdo' }).click();
  await page.getByRole('option', { name: 'Solo texto' }).click();
  const caption = page.getByRole('textbox', { name: 'Describe tu publicación' });
  await caption.fill('Nuestro recuerdo sintético');
  await page.getByRole('button', { name: 'Publicar', exact: true }).click();
  await expect(page.getByText('Synthetic service unavailable', { exact: true })).toBeVisible();
  await expect(caption).toHaveValue('Nuestro recuerdo sintético');
  expect(state.moments).toHaveLength(0);
  await page.getByRole('button', { name: 'Publicar', exact: true }).focus();
  await page.keyboard.press('Enter');
  await expect(page.getByText('Nuestro recuerdo sintético', { exact: true })).toBeVisible();
  expect(state.moments).toHaveLength(1);
  await page.reload();
  await expect(page.getByText('Nuestro recuerdo sintético', { exact: true })).toBeVisible();
  expect(state.signups).toHaveLength(1);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1)).toBe(true);
  await page.addScriptTag({ path: require.resolve('axe-core/axe.min.js') });
  const violations = await page.evaluate(async () => (await window.axe.run(document.querySelector('#momentos'), { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa'] } })).violations.filter((v) => ['serious', 'critical'].includes(v.impact)).map((v) => v.id));
  expect(violations).toEqual([]);
  const screenshot = testInfo.outputPath('synthetic-event-memories.png');
  await page.locator('#momentos').screenshot({ path: screenshot });
  await testInfo.attach('Synthetic memories after reload', { path: screenshot, contentType: 'image/png' });
});
}
