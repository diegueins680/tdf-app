import { jest } from '@jest/globals';

const getMock = jest.fn<(path: string) => Promise<unknown>>();
const warnMock = jest.fn();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
}));

jest.unstable_mockModule('../utils/logger', () => ({
  logger: {
    warn: warnMock,
  },
}));

const { Engineers, parseEngineersJson } = await import('./engineers');

describe('parseEngineersJson', () => {
  it('enforces the JSON string precondition for untyped callers', () => {
    expect(() => parseEngineersJson(42 as unknown as string)).toThrow(
      'parseEngineersJson precondition failed: raw must be a string',
    );
  });

  it('returns parsed values for valid JSON strings', () => {
    expect(parseEngineersJson('[{"peId":1,"peName":"Alpha"}]')).toEqual([{ peId: 1, peName: 'Alpha' }]);
  });

  it('returns null for malformed JSON instead of throwing', () => {
    expect(() => parseEngineersJson('{not valid json')).not.toThrow();
    expect(parseEngineersJson('{not valid json')).toBeNull();
  });
});

describe('Engineers.listPublic', () => {
  beforeEach(() => {
    getMock.mockReset();
    warnMock.mockReset();
    window.localStorage.clear();
  });

  afterEach(() => {
    window.localStorage.clear();
  });

  it('normalizes cached engineers when the live catalog fails', async () => {
    window.localStorage.setItem(
      'tdf-engineers-cache-v1',
      JSON.stringify([
        { peId: '2', peName: 'Beta' },
        { peId: 1, peName: ' Alpha ' },
        { peId: 2, peName: 'Duplicate Beta' },
        { peId: '0', peName: 'Invalid zero' },
        { peId: '3', peName: '   ' },
      ]),
    );
    getMock.mockRejectedValueOnce(new Error('offline'));

    await expect(Engineers.listPublic()).resolves.toEqual([
      { peId: 1, peName: 'Alpha' },
      { peId: 2, peName: 'Beta' },
    ]);
    expect(getMock).toHaveBeenCalledWith('/engineers');
    expect(warnMock).toHaveBeenCalledWith(
      'Engineer catalog unavailable, using cached engineer list',
      expect.any(Error),
    );
  });

  it('falls back to manual entry when the cache JSON is malformed', async () => {
    window.localStorage.setItem('tdf-engineers-cache-v1', '{not valid json');
    getMock.mockRejectedValueOnce(new Error('offline'));

    await expect(Engineers.listPublic()).resolves.toEqual([]);
    expect(warnMock).toHaveBeenCalledWith(
      'Engineer catalog unavailable, falling back to manual entry',
      expect.any(Error),
    );
  });
});
