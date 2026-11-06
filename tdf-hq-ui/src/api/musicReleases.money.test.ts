import { jest } from '@jest/globals';
import type { MusicReleaseContentInput } from './musicReleases';

const post = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const put = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ post, put, get: jest.fn(), del: jest.fn() }));
const { musicReleases } = await import('./musicReleases');

beforeEach(() => { post.mockReset().mockResolvedValue({}); put.mockReset().mockResolvedValue({}); });

function content(priceMinor: number | string): MusicReleaseContentInput {
  return { expectedUpdatedAt: '2035-01-01T00:00:00Z', tracks: [], parties: [], credits: [],
    identifiers: [], rightsDeclarations: [], availability: [{
      trackRef: null, territoryMode: 'include', territories: ['Worldwide'], startsAt: null, endsAt: null,
      listeningPolicy: 'none', downloadPolicy: 'purchase', purchasable: true,
      priceMinor, currency: 'USD', downloadableAssetId: 'synthetic-asset',
    }] };
}

it('submits the exact displayed purchase amount and currency', async () => {
  await musicReleases.createPurchase('rule', 'fixture-key', '9007199254740993', 'USD');
  expect(post).toHaveBeenCalledWith('/music/purchases', {
    availabilityRuleId: 'rule', territoryCode: 'ZZ', termsVersion: 'music-download-sale-2026-09-12',
    expectedAmountMinor: '9007199254740993', expectedCurrency: 'USD',
  }, { headers: { 'Idempotency-Key': 'fixture-key' } });
});

it('refuses a lossy legacy numeric offer before making a request', () => {
  expect(() => musicReleases.createPurchase('rule', 'fixture-key', Number('9007199254740993'), 'USD')).toThrow();
  expect(post).not.toHaveBeenCalled();
});

it('refuses an unsupported sale currency before making a request', () => {
  expect(() => musicReleases.createPurchase('rule', 'fixture-key', '250', 'EUR')).toThrow();
  expect(post).not.toHaveBeenCalled();
});

it.each([['250', 250], ['9007199254740993', '9007199254740993']] as const)(
  'preserves exact authoring input %s and compatible safe numeric encoding', async (input, encoded) => {
    await musicReleases.saveContent('release', 'version', content(input));
    expect(put).toHaveBeenCalledWith('/music/releases/release/versions/version/content',
      expect.objectContaining({ availability: [expect.objectContaining({ priceMinor: encoded })] }));
  },
);

it('refuses an inexact authoring value before making a request', () => {
  expect(() => musicReleases.saveContent('release', 'version', content('1e3'))).toThrow();
  expect(put).not.toHaveBeenCalled();
});
