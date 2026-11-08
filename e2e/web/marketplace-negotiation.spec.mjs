import { expect, test } from '@playwright/test';

// ADR 0132 — demand/offer marketplace, Phase 1 journey, as a synthetic
// API/browser integration (same approach as fanhub-onboarding.spec.mjs):
// author publishes a seeking classified with a suggested price → a band
// offers → the author counters → the band accepts → the classified is
// filled and the remaining open offer is rejected with slots_filled.
// Nothing here reaches a real backend; every API call is fulfilled below.

const CLASSIFIED_ID = '9c4a2f2e-0000-4000-8000-000000000001';
const AUTHOR_PROFILE = 'a1000000-0000-4000-8000-000000000001';
const BAND_PROFILE = 'b1000000-0000-4000-8000-000000000001';
const OTHER_BAND_PROFILE = 'b2000000-0000-4000-8000-000000000002';
const OFFER_ID = 'c1000000-0000-4000-8000-000000000001';
const OTHER_OFFER_ID = 'c2000000-0000-4000-8000-000000000002';
const USD = 'd1000000-0000-4000-8000-000000000001';

function makeFixture(page, baseURL, role) {
  const origin = new URL(baseURL).origin;
  const state = {
    classified: { id: CLASSIFIED_ID, authorProfileId: AUTHOR_PROFILE, sourceProfileId: null, title: 'Banda para boda en Cumbayá', slug: 'banda-para-boda-cumbaya', status: 'published', moderationStatus: 'allowed', expiresAt: '2026-12-20T00:00:00Z', version: 1, intent: 'seeking', slots: 1, slotsRemaining: 1, priceMode: 'suggested', offerDeadlineAt: null, budget: { minMinor: null, maxMinor: 120000, currencyId: USD, negotiable: true }, offerCount: 2, openOfferCount: 2 },
    offers: {
      [OFFER_ID]: { id: OFFER_ID, classifiedId: CLASSIFIED_ID, applicantProfileId: BAND_PROFILE, applicantProfile: { id: BAND_PROFILE, name: 'Los Sintéticos', slug: 'los-sinteticos', kind: 'band' }, message: 'Cuatro horas de show con backline incluido.', currencyId: USD, proposedAmountMinor: 130000, currentAmountMinor: 130000, currentRound: 1, lastActorRole: 'applicant', status: 'submitted', submittedAt: '2026-11-01T12:00:00Z', updatedAt: '2026-11-01T12:00:00Z', version: 1, reputation: { ratingAvg: 4.8, ratingCount: 6, completedCount: 9 }, terms: {}, rounds: [{ roundNumber: 1, actorRole: 'applicant', kind: 'initial', amountMinor: 130000, currencyId: USD, terms: {}, createdAt: '2026-11-01T12:00:00Z' }] },
      [OTHER_OFFER_ID]: { id: OTHER_OFFER_ID, classifiedId: CLASSIFIED_ID, applicantProfileId: OTHER_BAND_PROFILE, applicantProfile: { id: OTHER_BAND_PROFILE, name: 'Orquesta Prueba', slug: 'orquesta-prueba', kind: 'band' }, message: 'Repertorio tropical, tres sets.', currencyId: USD, proposedAmountMinor: 110000, currentAmountMinor: 110000, currentRound: 1, lastActorRole: 'applicant', status: 'submitted', submittedAt: '2026-11-02T12:00:00Z', updatedAt: '2026-11-02T12:00:00Z', version: 1, reputation: { ratingAvg: null, ratingCount: 0, completedCount: 0 }, terms: {}, rounds: [] },
    },
    counters: [],
    transitions: [],
  };
  const withRole = (offer) => ({ ...offer, classified: { ...state.classified }, participantRole: role });
  page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) {
      return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
    }
    const path = url.pathname;
    const method = request.method();
    if (path === '/session') return route.fulfill({ json: { username: role === 'author' ? 'synthetic-planner' : 'synthetic-band', displayName: 'Cuenta Sintética', partyId: role === 'author' ? 42 : 43, roles: ['customer'], modules: [], featureFlags: [], preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' } } });
    if (path === '/directory/taxonomies') return route.fulfill({ json: { locale: 'es', professions: [], classifiedCategories: [], compensationTypes: [], serviceOfferings: [], currencies: [{ id: USD, code: 'USD', name: 'Dólar', symbol: '$', minorUnits: 2 }], instruments: [], genres: [], languages: [], cities: [] } });
    if (path === '/directory/profiles' && method === 'GET') return route.fulfill({ json: [] });
    if (path === '/directory/classifieds' && method === 'GET') return route.fulfill({ json: role === 'author' ? [state.classified] : [] });
    if (path === '/directory/invitations') return route.fulfill({ json: [] });
    if (path === `/directory/classifieds/${CLASSIFIED_ID}/applications` && method === 'GET') {
      return route.fulfill({ json: Object.values(state.offers).map(withRole) });
    }
    if (path === '/directory/applications' && method === 'GET') {
      const filter = url.searchParams.get('status');
      const mine = Object.values(state.offers).filter((offer) => offer.applicantProfileId === BAND_PROFILE).filter((offer) => {
        if (filter === 'open') return ['submitted', 'viewed', 'shortlisted', 'countered_by_author', 'countered_by_applicant'].includes(offer.status);
        if (filter === 'accepted') return ['accepted', 'conversation_open', 'converted'].includes(offer.status);
        if (filter === 'closed') return ['rejected', 'withdrawn', 'expired'].includes(offer.status);
        return true;
      });
      return route.fulfill({ json: mine.map(withRole) });
    }
    const counterMatch = path.match(/^\/directory\/applications\/([^/]+)\/counter$/);
    if (counterMatch && method === 'POST') {
      const offer = state.offers[counterMatch[1]];
      const body = request.postDataJSON();
      state.counters.push({ id: counterMatch[1], role, body, idempotency: request.headers()['idempotency-key'] });
      const actor = role === 'author' ? 'author' : 'applicant';
      offer.currentRound += 1;
      offer.lastActorRole = actor;
      offer.currentAmountMinor = body.amountMinor;
      offer.status = actor === 'author' ? 'countered_by_author' : 'countered_by_applicant';
      offer.rounds.push({ roundNumber: offer.currentRound, actorRole: actor, kind: 'counter', amountMinor: body.amountMinor, currencyId: body.currencyId, terms: {}, message: body.message ?? null, createdAt: '2026-11-03T12:00:00Z' });
      return route.fulfill({ status: 201, json: withRole(offer) });
    }
    const statusMatch = path.match(/^\/directory\/applications\/([^/]+)\/status$/);
    if (statusMatch && method === 'PATCH') {
      const offer = state.offers[statusMatch[1]];
      const body = request.postDataJSON();
      state.transitions.push({ id: statusMatch[1], role, status: body.status });
      offer.status = body.status;
      if (body.status === 'accepted') {
        // Server-side slot accounting (I19): one slot consumed, classified filled,
        // every other open offer rejected with slots_filled.
        offer.acceptedAmountMinor = offer.currentAmountMinor;
        state.classified.slotsRemaining -= 1;
        if (state.classified.slotsRemaining === 0) {
          state.classified.status = 'filled';
          for (const other of Object.values(state.offers)) {
            if (other.id !== offer.id && ['submitted', 'viewed', 'shortlisted', 'countered_by_author', 'countered_by_applicant'].includes(other.status)) {
              other.status = 'rejected'; other.rejectionReason = 'slots_filled';
            }
          }
          state.classified.openOfferCount = 0;
        }
      }
      return route.fulfill({ json: withRole(offer) });
    }
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [] } });
    return route.fulfill({ status: 404, json: { error: `No synthetic fixture for ${method} ${path}` } });
  });
  return state;
}

test('author compares offers, counters one and the final acceptance fills the classified @critical', async ({ page, baseURL }) => {
  const state = makeFixture(page, baseURL, 'author');
  await page.goto('/mis-clasificados', { waitUntil: 'domcontentloaded' });
  await page.getByRole('tab', { name: /Anuncios/ }).click();
  await expect(page.getByText('Banda para boda en Cumbayá')).toBeVisible();
  await expect(page.getByText('Busca', { exact: true })).toBeVisible();
  await page.getByRole('button', { name: /^Ofertas \(2\)/ }).click();
  const board = page.getByTestId('offers-board');
  await expect(board.getByTestId('offer-card')).toHaveCount(2);

  // Sorting by lowest price puts the cheaper orchestra first; by reputation, the band.
  await board.getByRole('button', { name: 'Menor precio' }).click();
  await expect(board.getByTestId('offer-card').first()).toContainText('Orquesta Prueba');
  await board.getByRole('button', { name: 'Reputación' }).click();
  await expect(board.getByTestId('offer-card').first()).toContainText('Los Sintéticos');

  // Counter the band at 1 250 USD.
  const bandCard = board.getByTestId('offer-card').filter({ hasText: 'Los Sintéticos' });
  await bandCard.getByRole('button', { name: 'Contraofertar' }).click();
  await bandCard.getByLabel('Monto').fill('1250');
  await bandCard.getByRole('button', { name: 'Enviar contraoferta' }).click();
  await expect.poll(() => state.counters.length).toBe(1);
  expect(state.counters[0]).toMatchObject({ id: OFFER_ID, role: 'author', body: { amountMinor: 125000, currencyId: USD } });
  expect(state.counters[0].idempotency).toBeTruthy();
  await expect(bandCard.getByText('Contraoferta del autor')).toBeVisible();
  // After countering it is the band's move: the author cannot accept their own counter.
  await expect(bandCard.getByRole('button', { name: /^Aceptar/ })).toHaveCount(0);
  await expect(bandCard.getByText('Esperando a la otra parte')).toBeVisible();
});

test('offerer accepts the author counter from "Mis ofertas" and sees the negotiation rounds @critical', async ({ page, baseURL }) => {
  const state = makeFixture(page, baseURL, 'applicant');
  // The author already countered at 1 250 USD.
  const offer = state.offers[OFFER_ID];
  offer.status = 'countered_by_author'; offer.lastActorRole = 'author'; offer.currentRound = 2; offer.currentAmountMinor = 125000;
  offer.rounds.push({ roundNumber: 2, actorRole: 'author', kind: 'counter', amountMinor: 125000, currencyId: USD, terms: {}, message: 'Podemos llegar a 1250 si incluyen el backline.', createdAt: '2026-11-03T12:00:00Z' });

  await page.goto('/mis-clasificados', { waitUntil: 'domcontentloaded' });
  await page.getByRole('tab', { name: 'Mis ofertas' }).click();
  const card = page.getByTestId('my-offers').getByTestId('offer-card');
  await expect(card).toHaveCount(1);
  await expect(card).toContainText('Banda para boda en Cumbayá');
  await expect(card.getByText('Te toca responder')).toBeVisible();
  await card.getByRole('button', { name: /Ver negociación/ }).click();
  await expect(card.getByText(/contraoferta de quien publica/)).toBeVisible();
  await expect(card.getByText(/Podemos llegar a 1250/)).toBeVisible();

  await card.getByRole('button', { name: /^Aceptar/ }).click();
  await expect.poll(() => state.transitions.length).toBe(1);
  expect(state.transitions[0]).toEqual({ id: OFFER_ID, role: 'applicant', status: 'accepted' });
  expect(state.classified.status).toBe('filled');
  expect(state.offers[OTHER_OFFER_ID].status).toBe('rejected');
  expect(state.offers[OTHER_OFFER_ID].rejectionReason).toBe('slots_filled');
  await page.getByRole('tab', { name: 'Aceptadas' }).click();
  await expect(page.getByTestId('my-offers').getByTestId('offer-card')).toHaveCount(1);
  await expect(page.getByTestId('my-offers').getByText('Aceptada', { exact: true })).toBeVisible();
});
