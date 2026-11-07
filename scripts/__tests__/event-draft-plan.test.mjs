import assert from 'node:assert/strict';
import test from 'node:test';
import { mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
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

// EVT-DRAFT-REVIEW-001: synthetic offline boundaries, never an import authorization.
function reviewWithCurrency(currency) {
  const tier = {
    ticketTierCode: 'EARLY', ticketTierActive: false, ticketTierQuantitySold: 0,
    ticketTierQuantityTotal: 40, ticketTierPriceCents: 2000,
    ticketTierCurrency: currency, ticketTierSalesEnd: '2027-01-08T05:00:00Z',
  };
  return {
    schemaVersion: 2, approval: 'review-only', flyer: 'must-not-read.png',
    event: {
      eventIsPublic: false, eventArtists: [], eventCapacity: 100,
      eventPriceCents: 2000, eventCurrency: currency,
      eventStart: '2027-01-09T17:00:00Z', eventEnd: '2027-01-10T02:00:00Z',
    },
    tiers: [tier, { ...tier, ticketTierCode: 'DOOR', ticketTierQuantityTotal: 50, ticketTierPriceCents: 3000 }],
  };
}

const invalidReviewCurrencies = {
  missing: undefined, empty: '', arbitrary: 'bogus', short: 'US',
  padded: ' usd ', lowercase: 'usd', mixed: 'Usd', digit: 'US1',
  fullwidth: 'ＵＳＤ', null: null, numeric: 840, object: {},
};
for (const [name, currency] of Object.entries(invalidReviewCurrencies)) {
  test(`rejects matched ${name} review currency before offline approval`, () => {
    assert.throws(() => validateDraftPlan(reviewWithCurrency(currency)), /currency/i);
  });
}

test('accepts matching canonical currency strings without changing the submitted values', () => {
  for (const currency of ['USD', 'EUR', 'JPY']) {
    const review = reviewWithCurrency(currency);
    const before = structuredClone(review);
    assert.doesNotThrow(() => validateDraftPlan(review));
    assert.deepEqual(review, before);
  }
});

test('retains the original schema-one currency contract', () => {
  const legacy = structuredClone(plan);
  legacy.event.eventCurrency = ' usd ';
  legacy.tier.ticketTierCurrency = ' usd ';
  assert.doesNotThrow(() => validateDraftPlan(legacy));
});

const script = fileURLToPath(new URL('../prepare-event-draft.mjs', import.meta.url));
const blockedFetch = 'data:text/javascript,' + encodeURIComponent(
  "globalThis.fetch = () => { console.error('UNEXPECTED_FETCH'); throw new Error('Network is disabled in this fixture'); };",
);
async function runReviewCli(validateOnly) {
  const directory = await mkdtemp(join(tmpdir(), 'tdf-offline-review-'));
  try {
    const file = join(directory, 'review.json');
    await writeFile(file, JSON.stringify(reviewWithCurrency('USD')));
    const result = spawnSync(process.execPath, [
      '--import', blockedFetch, script, ...(validateOnly ? ['--validate-only'] : []), file,
    ], { cwd: directory, env: { ADMIN_TOKEN: 'synthetic-not-a-credential' }, encoding: 'utf8', timeout: 10000 });
    assert.ifError(result.error);
    assert.equal(existsSync(join(directory, 'ticketing-event-draft.json')), false);
    assert.doesNotMatch(result.stderr, /UNEXPECTED_FETCH/);
    return result;
  } finally {
    await rm(directory, { recursive: true, force: true });
  }
}

test('schema-two validate-only CLI succeeds without a flyer, credentials or network', async () => {
  const result = await runReviewCli(true);
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(JSON.parse(result.stdout), { valid: true, mode: 'offline', schemaVersion: 2, mutations: 0 });
});

test('schema-two CLI refuses import before flyer, token or API work', async () => {
  const result = await runReviewCli(false);
  assert.equal(result.status, 1);
  assert.match(result.stderr, /Review plans are offline only/);
  assert.doesNotMatch(result.stderr, /ENOENT|Authorized ADMIN_TOKEN/);
});
