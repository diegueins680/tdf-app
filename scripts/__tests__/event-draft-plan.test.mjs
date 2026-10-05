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
