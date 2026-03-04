import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
}));

const { Engineers } = await import('./engineers');

const ENGINEERS_CACHE_KEY = 'tdf-engineers-cache-v1';

describe('Engineers.listPublic', () => {
  const warnSpy = jest.spyOn(console, 'warn').mockImplementation(() => undefined);

  beforeEach(() => {
    getMock.mockReset();
    warnSpy.mockClear();
    window.localStorage.clear();
  });

  afterAll(() => {
    warnSpy.mockRestore();
  });

  it('sanitizes live payloads and persists normalized cache', async () => {
    getMock.mockResolvedValueOnce([
      { peId: '2', peName: '  Ana  ' },
      { peId: 2, peName: 'ana' },
      { peId: 0, peName: 'Zero' },
      { peId: -1, peName: 'Negative' },
      { peId: 3.5, peName: 'Float' },
      { peId: '4x', peName: 'Invalid text' },
      { peId: 4, peName: ' Luis ' },
      { peId: 5, peName: '   ' },
    ]);

    const result = await Engineers.listPublic();

    expect(result).toEqual([
      { peId: 2, peName: 'Ana' },
      { peId: 4, peName: 'Luis' },
    ]);
    expect(JSON.parse(window.localStorage.getItem(ENGINEERS_CACHE_KEY) ?? '[]')).toEqual(result);
  });

  it('falls back to sanitized cache when the live request fails', async () => {
    window.localStorage.setItem(
      ENGINEERS_CACHE_KEY,
      JSON.stringify([
        { peId: '8', peName: '  Marta  ' },
        { peId: 8, peName: 'marta' },
        { peId: -10, peName: 'Invalid negative' },
        { peId: 'oops', peName: 'Invalid text' },
        { peId: 11, peName: ' Leo ' },
        { peId: 99, peName: '   ' },
      ]),
    );

    getMock.mockRejectedValueOnce(new Error('offline'));

    const result = await Engineers.listPublic();

    expect(result).toEqual([
      { peId: 11, peName: 'Leo' },
      { peId: 8, peName: 'Marta' },
    ]);
    expect(warnSpy).toHaveBeenCalledWith(
      'Engineer catalog unavailable, using cached engineer list',
      expect.any(Error),
    );
  });
});
