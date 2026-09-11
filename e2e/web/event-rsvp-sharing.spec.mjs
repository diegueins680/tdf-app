import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const event = {
  id: '42',
  title: 'Festival Sintético RSVP',
  description: 'Evento ficticio para verificar RSVP sin servicios externos.',
  startTime: '2030-10-03T20:00:00Z',
  endTime: '2030-10-04T01:00:00Z',
  timezone: 'America/Guayaquil',
  imageUrl: '/tdf-app-icon-1024.png',
  isPublic: true,
  publicShareEligible: true,
  rsvpEligible: true,
  workflowStateCode: 'announced',
  canonicalUrl: '/eventos/42',
  venue: { name: 'Teatro Ficticio' },
  location: { city: 'Quito', countryCode: 'EC' },
  rsvpSummary: { acceptedCount: 2, maybeCount: 1 },
};

async function mockRsvpApi(page) {
  let currentRsvp = null;
  let upsertCount = 0;
  let authenticated = false;
  const bodies = [];

  await page.route('**/health', (route) => route.fulfill({ json: { status: 'ok' } }));
  await page.route('**/session', (route) => authenticated
    ? route.fulfill({
      json: {
        username: 'rsvp.persona@example.test',
        displayName: 'Persona Ficticia',
        roles: ['Fan', 'Customer'],
        modules: [],
        featureFlags: [],
        partyId: 101,
        preferences: { showEventRsvpsOnProfile: true },
      },
    })
    : route.fulfill({ status: 401, json: { error: 'unauthenticated' } }));
  await page.route('**/session/preferences', (route) => route.fulfill({
    json: {
      localeId: '', locale: 'es', currencyId: '', currency: 'USD',
      timezone: 'America/Guayaquil', countryId: null, countryCode: 'EC',
      showEventRsvpsOnProfile: true,
    },
  }));
  await page.route('**/catalogs/batch?*', (route) => route.fulfill({ json: {} }));
  await page.route('**/directory/events/42', (route) => route.fulfill({ json: event }));
  await page.route('**/signup', (route) => {
    authenticated = true;
    return route.fulfill({
      json: { token: 'fictional-rsvp-token', partyId: 101, roles: ['Fan', 'Customer'], modules: [] },
    });
  });
  await page.route('**/social-events/events/42/rsvp', async (route) => {
    if (route.request().method() === 'GET') return route.fulfill({ json: currentRsvp });
    if (route.request().method() === 'DELETE') {
      currentRsvp = null;
      return route.fulfill({ status: 204, body: '' });
    }
    upsertCount += 1;
    const body = route.request().postDataJSON();
    bodies.push(body);
    await new Promise((resolve) => setTimeout(resolve, 80));
    currentRsvp = {
      rsvpEventId: '42',
      rsvpStatus: body.rsvpStatus,
      rsvpShowOnProfile: body.rsvpStatus === 'declined' ? false : body.rsvpShowOnProfile,
      rsvpCreatedAt: '2030-01-01T00:00:00Z',
      rsvpUpdatedAt: '2030-01-02T00:00:00Z',
    };
    return route.fulfill({ json: currentRsvp });
  });
  await page.route('**/social-events/events/42/rsvp-summary', (route) => route.fulfill({
    json: {
      rsvpAcceptedCount: currentRsvp?.rsvpStatus === 'accepted' ? 3 : 2,
      rsvpMaybeCount: currentRsvp?.rsvpStatus === 'maybe' ? 2 : 1,
    },
  }));
  await page.route('**/social-events/profiles/101/rsvp-feed?*', (route) => route.fulfill({
    json: {
      feedItems: currentRsvp && ['accepted', 'maybe'].includes(currentRsvp.rsvpStatus) ? [{
        feedItemType: 'event_rsvp',
        feedEventId: '42',
        feedStatus: currentRsvp.rsvpStatus,
        feedShowOnProfile: true,
        feedEventTitle: event.title,
        feedEventStart: event.startTime,
        feedEventTimezone: event.timezone,
        feedEventImageUrl: null,
        feedVenueName: event.venue.name,
        feedCity: event.location.city,
        feedWorkflowStateCode: 'announced',
        feedActionAt: currentRsvp.rsvpUpdatedAt,
        feedCanonicalUrl: '/eventos/42',
        feedCanEdit: true,
        feedCanShare: true,
      }] : [],
      feedNextCursor: null,
    },
  }));
  await page.route('**/social/profiles/101', (route) => route.fulfill({
    json: { sppPartyId: 101, sppDisplayName: 'Persona RSVP Ficticia', sppBio: null, sppAvatarUrl: null, sppCity: 'Quito' },
  }));
  await page.route('**/social/friends', (route) => route.fulfill({ json: [] }));
  await page.route('**/social/followers', (route) => route.fulfill({ json: [] }));
  await page.route('**/social/following', (route) => route.fulfill({ json: [] }));
  await mockRadioShell(page);

  return { getState: () => ({ currentRsvp, upsertCount, bodies }) };
}

async function mockRadioShell(page) {
  await page.route('**/radio/streams*', (route) => route.fulfill({ json: [] }));
  await page.route('**/radio/presence*', (route) => route.fulfill({ json: null }));
  await page.route('**/radio/auto-stop-options*', (route) => route.fulfill({ json: { options: [] } }));
  await page.route('**/chat/threads', (route) => route.fulfill({ json: [] }));
  await page.route('**/fans/me/notifications/count', (route) => route.fulfill({ json: { ncUnread: 0 } }));
  await page.route('**/navigation/preferences', (route) => route.fulfill({ json: [] }));
  await page.route('**/navigation/preferences/*/visit', (route) => route.fulfill({
    json: { featureId: 'event-rsvp', favorite: false, pinned: false, pinOrder: null },
  }));
}

async function expectNoSeriousAxeViolations(page, testInfo, attachmentName) {
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => {
    const result = await globalThis.axe.run(document, { resultTypes: ['violations'] });
    return result.violations
      .filter((violation) => violation.impact === 'critical' || violation.impact === 'serious')
      .map((violation) => ({
        id: violation.id,
        impact: violation.impact,
        help: violation.help,
        nodes: violation.nodes.map((node) => node.target),
      }));
  });
  await testInfo.attach(attachmentName, {
    body: JSON.stringify(violations, null, 2),
    contentType: 'application/json',
  });
  expect(violations).toEqual([]);
}

test('@critical anonymous RSVP survives signup, appears once in the profile, shares publicly, and is withdrawn', async ({ page, context }, testInfo) => {
  test.skip(testInfo.project.name !== 'chromium-desktop', 'The complete mutation journey runs once on desktop Chromium.');
  const api = await mockRsvpApi(page);
  await context.grantPermissions(['clipboard-read', 'clipboard-write']);

  await page.goto('/eventos/42?utm_source=tdf_web&utm_medium=share&utm_campaign=event_rsvp');
  await expect(page.getByRole('heading', { name: event.title })).toBeVisible();
  await page.getByRole('button', { name: 'Voy' }).dblclick();

  await expect(page).toHaveURL(/\/login\?.*signup=1.*redirect=%2Feventos%2F42/);
  const signup = page.getByRole('dialog', { name: /crear cuenta/i });
  await expect(signup.getByRole('button', { name: 'Ya tengo una cuenta' })).toBeVisible();
  await signup.getByLabel('Nombre').fill('Persona');
  await signup.getByLabel('Apellido').fill('Ficticia');
  await signup.locator('input[name="email"]').fill('rsvp.persona@example.test');
  await signup.locator('input[name="newPassword"]').fill('fictional-password-not-a-secret');
  await page.getByLabel('Acepto los términos y la política de privacidad').check();
  await signup.getByRole('button', { name: 'Crear e ingresar' }).click();

  await expect(page).toHaveURL(/\/eventos\/42$/);
  await expect.poll(() => api.getState().upsertCount).toBe(1);
  expect(api.getState().bodies).toEqual([{ rsvpStatus: 'accepted', rsvpShowOnProfile: true }]);
  await expect(page.getByRole('button', { name: 'Voy' })).toHaveAttribute('aria-pressed', 'true');
  await expect(page.locator('section[aria-labelledby="event-rsvp-title-42"]')).toHaveAttribute('aria-busy', 'false');
  await expectNoSeriousAxeViolations(page, testInfo, 'event-rsvp-axe.json');
  await page.screenshot({ path: 'docs/social-events/evidence/web-rsvp-event.png', fullPage: true });

  await page.getByRole('button', { name: 'Copiar enlace' }).click();
  await expect.poll(() => page.evaluate(() => navigator.clipboard.readText())).toContain('/eventos/42');
  expect(await page.evaluate(() => navigator.clipboard.readText())).not.toContain('/social/eventos/');

  await page.goto('/perfil/101');
  await expect(page.getByText('Va a')).toBeVisible();
  await expect(page.getByText(event.title)).toBeVisible();
  await expectNoSeriousAxeViolations(page, testInfo, 'event-rsvp-profile-axe.json');
  await page.screenshot({ path: 'docs/social-events/evidence/web-rsvp-profile.png', fullPage: true });

  await page.goto('/eventos/42');
  await page.getByRole('button', { name: 'No iré' }).click();
  await expect.poll(() => api.getState().currentRsvp?.rsvpStatus).toBe('declined');
  await page.goto('/perfil/101');
  await expect(page.getByText('Todavía no hay actividad de RSVP visible.')).toBeVisible();
});

test('private events do not enumerate and cancelled public events disable RSVP and sharing', async ({ page }, testInfo) => {
  test.skip(testInfo.project.name !== 'chromium-desktop', 'Visibility boundary runs once.');
  await page.route('**/health', (route) => route.fulfill({ json: { status: 'ok' } }));
  await page.route('**/session', (route) => route.fulfill({ status: 401, json: { error: 'unauthenticated' } }));
  await page.route('**/catalogs/batch?*', (route) => route.fulfill({ json: {} }));
  await mockRadioShell(page);
  await page.route('**/directory/events/99', (route) => route.fulfill({ status: 404, json: { error: 'not found' } }));
  await page.goto('/eventos/99');
  await expect(page.getByText('Este contenido no está publicado, vigente o disponible.')).toBeVisible();
  await expect(page.getByText(/Festival|privado|Private/i)).toHaveCount(0);

  await page.route('**/directory/events/43', (route) => route.fulfill({
    json: { ...event, id: '43', title: 'Evento Cancelado Ficticio', canonicalUrl: '/eventos/43', workflowStateCode: 'cancelled', rsvpEligible: false, publicShareEligible: false },
  }));
  await page.goto('/eventos/43');
  await expect(page.getByText('Los eventos cancelados no aceptan nuevos RSVPs.')).toBeVisible();
  await expect(page.getByRole('button', { name: 'Voy' })).toBeDisabled();
  await expect(page.getByRole('button', { name: 'Compartir' })).toBeDisabled();
});
