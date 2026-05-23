import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();
const warnMock = jest.fn<(...args: unknown[]) => void>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: {
    log: jest.fn(),
    warn: warnMock,
    error: jest.fn(),
  },
}));

const { Engineers } = await import('./engineers');

const ENGINEERS_CACHE_KEY = 'tdf-engineers-cache-v1';

const readEngineersCache = (): unknown => {
  const raw = window.localStorage.getItem(ENGINEERS_CACHE_KEY);
  if (!raw) return [];

  try {
    return JSON.parse(raw) as unknown;
  } catch {
    throw new Error('Engineer cache should contain valid JSON');
  }
};

describe('Engineers.listPublic', () => {
  beforeEach(() => {
    getMock.mockReset();
    warnMock.mockClear();
    window.localStorage.clear();
  });

  it('sanitizes live payloads and persists normalized cache', async () => {
    getMock.mockResolvedValueOnce([
      { peId: '2', peName: '  Ana  ' },
      { peId: 2, peName: 'ana' },
      { peId: 6, peName: 'Ana' },
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
      { peId: 6, peName: 'Ana' },
      { peId: 4, peName: 'Luis' },
    ]);
    expect(readEngineersCache()).toEqual(result);
  });

  it('falls back to sanitized cache when the live request fails', async () => {
    window.localStorage.setItem(
      ENGINEERS_CACHE_KEY,
      JSON.stringify([
        { peId: '8', peName: '  Marta  ' },
        { peId: 8, peName: 'marta' },
        { peId: 18, peName: 'Marta' },
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
      { peId: 18, peName: 'Marta' },
    ]);
    expect(warnMock).toHaveBeenCalledWith(
      'Engineer catalog unavailable, using cached engineer list',
      expect.any(Error),
    );
  });

  it('ignores malformed cached engineers when the live request fails', async () => {
    window.localStorage.setItem(ENGINEERS_CACHE_KEY, '{not valid json');
    getMock.mockRejectedValueOnce(new Error('offline'));

    await expect(Engineers.listPublic()).resolves.toEqual([]);
    expect(warnMock).toHaveBeenCalledWith(
      'Engineer catalog unavailable, falling back to manual entry',
      expect.any(Error),
    );
  });
});
