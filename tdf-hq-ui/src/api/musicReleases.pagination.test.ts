import { jest } from '@jest/globals';
const get = jest.fn<(...args: unknown[]) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get, post: jest.fn(), put: jest.fn(), del: jest.fn() }));
const { musicReleases } = await import('./musicReleases');
beforeEach(() => { get.mockReset().mockResolvedValue([]); });

it('preserves existing public catalog callers without pagination options', async () => {
  await musicReleases.listPublic('  Árbol & mar  ', 7);
  expect(get).toHaveBeenCalledWith('/music/releases?q=%C3%81rbol+%26+mar&artistPartyId=7');
});
it('sends bounded pagination and the cancellation signal to the authorized public endpoint', async () => {
  const signal = new AbortController().signal;
  await musicReleases.listPublic(' pop ', undefined, { limit: 25, offset: 24, signal });
  expect(get).toHaveBeenCalledWith('/music/releases?q=pop&limit=25&offset=24', { signal });
});
it.each([{ limit: 0 }, { limit: 101 }, { limit: 1.5 }, { offset: -1 }, { offset: Infinity }, { offset: 1.5 }])(
  'rejects invalid pagination before requesting: %j', (page) => {
    expect(() => musicReleases.listPublic('', undefined, page)).toThrow(RangeError);
    expect(get).not.toHaveBeenCalled();
  },
);
