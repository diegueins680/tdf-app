import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Real production-bundle routing and browser interaction, synthetic API only.
async function fixture(page, baseURL) {
  const state = { authenticated: true, failRead: false, reads: [], actions: [], status: 'pending', denied: false, profileReads: [], notifications: [
    { nId: 3, nType: 'artist_liked', nTitle: 'Nuevo fan', nBody: 'Nombre de presentación anterior empezó a seguir tu perfil.', nTargetType: 'party_profile', nTargetId: 7, nIsRead: false, nCreatedAt: '2026-09-16T10:00:00Z' },
    { nId: 4, nType: 'access_request_review', nTitle: 'Nueva solicitud de acceso', nBody: 'Una solicitud requiere revisión.', nTargetType: 'feature_access_request', nTargetId: 17, nIsRead: false, nCreatedAt: '2026-09-16T10:00:00Z' },
  ] };
  const origin = new URL(baseURL).origin;
  await page.context().route('**/*', async route => {
    const request = route.request(); const url = new URL(request.url()); const path = url.pathname;
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    if (path === '/session') return route.fulfill({ json: state.authenticated ? { username: 'synthetic-reviewer', displayName: 'Reviewer', partyId: 5, roles: ['Admin'], modules: ['Catalog', 'CRM', 'Admin'], featureFlags: [] } : null });
    if (path === '/session/preferences') return route.fulfill({ json: { localeId: '', locale: 'es', currencyId: '', currency: 'USD', timezone: 'America/Guayaquil', countryId: null, countryCode: 'EC' } });
    if (path.endsWith('/rsvp-feed')) return route.fulfill({ json: { feedItems: [], feedNextCursor: null } });
    if (path === '/health') return route.fulfill({ json: { status: 'ok' } });
    if (path === '/session/onboarding') return route.fulfill({ json: { eligible: false, completedAt: '2026-09-16T10:00:00Z' } });
    if (path === '/fans/me/notifications/count') return route.fulfill({ json: { ncUnread: state.notifications.filter(n => !n.nIsRead).length } });
    if (path === '/fans/me/notifications') return route.fulfill({ json: state.notifications });
    if (/^\/fans\/me\/notifications\/\d+\/read$/.test(path)) {
      state.reads.push(Number(path.split('/')[4]));
      if (state.failRead) return route.fulfill({ status: 503, json: { error: 'synthetic failure' } });
      state.notifications.find(n => n.nId === Number(path.split('/')[4])).nIsRead = true;
      return route.fulfill({ status: 204 });
    }
    if (path === '/fans/me/notifications/3') return route.fulfill({ json: state.notifications[0] });
    if (path === '/social/profiles/7') { state.profileReads.push(7); return route.fulfill({ json: { sppPartyId: 7, sppDisplayName: 'Actual follower', sppBio: 'Public follower biography', sppAvatarUrl: null, sppCity: 'Quito' } }); }
    if (path === '/access-requests/17' && request.method() === 'GET') return state.denied ? route.fulfill({ status: 404, json: {} }) : route.fulfill({ json: {
      canReview: true, canCancel: false, request: { id: 17, requesterPartyId: 7, requesterName: 'Synthetic requester', featureId: 'label.ddex.inbox', action: 'view', status: state.status, roleContext: [], moduleContext: [], reviewerGroup: 'label', justification: 'Review this specific workflow', reviewerNotes: state.status === 'approved' ? 'Already reviewed' : null, requestedAt: '2026-09-16T10:00:00Z', updatedAt: '2026-09-16T10:00:00Z', decidedAt: null, cancelledAt: null, expiresAt: null, history: [] },
    } });
    if (request.method() !== 'GET' && !path.startsWith('/navigation/')) state.actions.push({ method: request.method(), path });
    if (path.startsWith('/radio/presence')) return route.fulfill({ json: null });
    if (path.startsWith('/catalog')) return route.fulfill({ json: { catalogs: [], items: [], defaults: [] } });
    return route.fulfill({ json: [] });
  });
  return state;
}
async function openBell(page) {
  const bell = page.getByRole('button', { name: 'Notificaciones', exact: true });
  await expect(bell).toBeVisible(); await bell.click();
  await expect(page.getByRole('link', { name: /Nuevo fan/ })).toBeVisible();
}

test('notification follower keyboard/touch navigation and failed read persistence @critical', async ({ page, baseURL }, info) => {
  const state = await fixture(page, baseURL); state.failRead = true;
  await page.goto('/solicitudes-acceso?request=17'); await openBell(page); expect(state.reads).toEqual([]);
  const link = page.getByRole('link', { name: /Nuevo fan/ }); await expect(link).toHaveAttribute('href', '/notificaciones/3');
  await link.focus(); await expect(link).toBeFocused();
  if (info.project.use.hasTouch) await link.tap(); else await page.keyboard.press('Enter');
  await expect(page).toHaveURL(/\/perfil\/7$/); await expect(page.getByText('Actual follower', { exact: true })).toBeVisible();
  expect(state.profileReads).toContain(7); expect(state.reads).toEqual([3]); expect(state.notifications[0].nIsRead).toBe(false);
  await expect(page.getByRole('alert').filter({ hasText: 'No se pudo marcar' })).toBeVisible();
  await expect(page.getByRole('heading', { name: 'Notificaciones', exact: true })).toHaveCount(0);
  expect(state.actions.filter(a => /access-requests|social\/friends|follows/.test(a.path))).toEqual([]);
});

test('specific request navigation shows current state and never decides it @critical', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL); state.status = 'approved';
  await page.goto('/perfil/7'); await openBell(page); await page.getByRole('link', { name: /Nueva solicitud de acceso/ }).click();
  await expect(page).toHaveURL(/\/solicitudes-acceso\?request=17$/);
  await expect(page.getByRole('heading', { name: 'Solicitud #17' })).toBeVisible();
  await expect(page.getByText('Aprobada', { exact: true })).toBeVisible(); await expect(page.getByText('Already reviewed')).toBeVisible();
  await expect(page.getByRole('button', { name: 'Aprobar para provisión' })).toHaveCount(0);
  expect(state.actions.filter(a => a.path.startsWith('/access-requests'))).toEqual([]);
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await window.axe.run(document.querySelector('main') ?? document.body)).violations.filter(v => ['serious', 'critical'].includes(v.impact)));
  expect(violations.map(v => ({ id: v.id, nodes: v.nodes.map(n => n.target) }))).toEqual([]);
});

test('expired session retains notification destination and denied target stays private @critical', async ({ page, baseURL }) => {
  const state = await fixture(page, baseURL); state.authenticated = false;
  await page.goto('/solicitudes-acceso?request=17');
  await expect(page).toHaveURL(/\/login\?redirect=/);
  const redirect = new URL(page.url()).searchParams.get('redirect'); expect(redirect).toBe('/solicitudes-acceso?request=17');
  await expect(page.getByRole('heading', { name: 'Iniciar sesión', exact: true })).toBeVisible();
  state.authenticated = true; state.denied = true;
  await page.goto(redirect);
  await expect(page.getByRole('alert').filter({ hasText: 'no está disponible para tu cuenta' })).toBeVisible();
  await expect(page.getByText('Synthetic requester')).toHaveCount(0);
  await expect(page.getByRole('link', { name: 'Volver a mis solicitudes' })).toBeVisible();
  expect(state.actions.filter(a => a.path.startsWith('/access-requests'))).toEqual([]);
});

test('standard new-tab activation resolves the owned notification and preserves the original page @critical', async ({ page, baseURL, browserName }) => {
  const state = await fixture(page, baseURL);
  await page.goto('/solicitudes-acceso?request=17'); await openBell(page);
  const [target] = await Promise.all([page.context().waitForEvent('page'), page.getByRole('link', { name: /Nuevo fan/ }).click(browserName === 'webkit' ? { modifiers: ['ControlOrMeta'] } : { button: 'middle' })]);
  await expect(target).toHaveURL(/\/perfil\/7$/);
  await expect(target.getByText('Actual follower', { exact: true })).toBeVisible();
  await expect(page).toHaveURL(/\/solicitudes-acceso\?request=17$/);
  expect(state.reads).toEqual([3]); expect(state.actions.filter(a => a.path.startsWith('/access-requests'))).toEqual([]);
});
