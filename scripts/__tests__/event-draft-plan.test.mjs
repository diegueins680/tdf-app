import assert from 'node:assert/strict';
import test from 'node:test';
import { readFile } from 'node:fs/promises';
import { assertPrivateDraft, validateDraftPlan } from '../prepare-event-draft.mjs';

const plan = JSON.parse(await readFile(new URL('../../docs/events/patch-culture-vol-1/draft-plan.json', import.meta.url)));
test('approved event retains published price and twenty inactive places', () => {
  assert.doesNotThrow(() => validateDraftPlan(plan));
  assert.equal(plan.tier.ticketTierPriceCents, 2000);
  assert.equal(plan.tier.ticketTierQuantityTotal, 20);
  assert.equal(plan.event.eventIsPublic, false);
});
test('rejects selling or inconsistent plans before API access', () => {
  for (const mutate of [
    (x) => { x.event.eventIsPublic = true; },
    (x) => { x.tier.ticketTierActive = true; },
    (x) => { x.tier.ticketTierQuantitySold = 1; },
    (x) => { x.tier.ticketTierPriceCents = 2500; },
  ]) {
    const altered = structuredClone(plan);
    mutate(altered);
    assert.throws(() => validateDraftPlan(altered));
  }
});
test('fails closed on absent or enabled runtime visibility and purchase projections', () => {
  const draft = { eventId: 'fixture', eventIsPublic: false, eventPublicListable: false,
    eventTicketPurchaseEnabled: false, eventWorkflowStateCode: 'planning' };
  assert.doesNotThrow(() => assertPrivateDraft(draft));
  for (const field of ['eventIsPublic', 'eventPublicListable', 'eventTicketPurchaseEnabled']) {
    assert.throws(() => assertPrivateDraft({ ...draft, [field]: true }));
    assert.throws(() => assertPrivateDraft({ ...draft, [field]: undefined }));
  }
});

test('a private two-tier review keeps headroom for guests and rejects over-allocation', () => {
  const tier = {
    ticketTierCode: 'EARLY',
    ticketTierActive: false,
    ticketTierQuantitySold: 0,
    ticketTierQuantityTotal: 40,
    ticketTierPriceCents: 2000,
    ticketTierCurrency: 'USD',
    ticketTierSalesEnd: '2027-01-08T05:00:00Z',
  };
  const review = {
    schemaVersion: 2,
    approval: 'review-only',
    event: {
      eventIsPublic: false, eventArtists: [], eventCapacity: 100, eventPriceCents: 2000, eventCurrency: 'USD',
      eventStart: '2027-01-09T17:00:00Z', eventEnd: '2027-01-10T02:00:00Z',
    },
    tiers: [tier, {
      ...tier, ticketTierCode: 'DOOR', ticketTierQuantityTotal: 50, ticketTierPriceCents: 3000,
      ticketTierSalesStart: '2027-01-09T16:00:00Z', ticketTierSalesEnd: '2027-01-09T20:00:00Z',
    }],
  };
  assert.doesNotThrow(() => validateDraftPlan(review));
  for (const change of [
    { ticketTierQuantityTotal: 61 },
    { ticketTierCode: ' early ' },
    { ticketTierActive: true },
    { ticketTierSalesEnd: '2027-01-09T15:59:59Z' },
    { ticketTierCurrency: 'EUR' },
  ]) {
    assert.throws(() => validateDraftPlan({ ...review, tiers: [tier, { ...review.tiers[1], ...change }] }));
  }
  assert.throws(() => validateDraftPlan({ ...review, event: { ...review.event, eventTicketUrl: 'https://example.com' } }));
});
