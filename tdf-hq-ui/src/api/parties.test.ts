import { jest } from '@jest/globals';

const getMock = jest.fn<(...args: unknown[]) => Promise<unknown>>();

jest.unstable_mockModule('./client', () => ({
  get: getMock,
  post: jest.fn(),
  put: jest.fn(),
}));

const { Parties } = await import('./parties');

const page = (start: number, count: number) =>
  Array.from({ length: count }, (_, index) => ({ partyId: start + index }));

describe('Parties.list', () => {
  beforeEach(() => {
    getMock.mockReset();
  });

  it('requests one explicit page when the CRM has fewer contacts than a page', async () => {
    getMock.mockResolvedValueOnce(page(1, 278));

    await expect(Parties.list()).resolves.toHaveLength(278);
    expect(getMock).toHaveBeenCalledTimes(1);
    expect(getMock).toHaveBeenCalledWith('/parties?limit=500&offset=0');
  });

  it('keeps paging until the server returns a short page', async () => {
    getMock
      .mockResolvedValueOnce(page(1, 500))
      .mockResolvedValueOnce(page(501, 500))
      .mockResolvedValueOnce(page(1001, 3));

    const parties = await Parties.list();

    expect(parties).toHaveLength(1003);
    expect(parties.at(-1)).toEqual({ partyId: 1003 });
    expect(getMock.mock.calls.map((call) => call[0])).toEqual([
      '/parties?limit=500&offset=0',
      '/parties?limit=500&offset=500',
      '/parties?limit=500&offset=1000',
    ]);
  });

  it('stops at the server offset ceiling instead of looping forever', async () => {
    getMock.mockResolvedValue(page(1, 500));

    await Parties.list();

    expect(getMock).toHaveBeenCalledTimes(21);
    expect(getMock).toHaveBeenLastCalledWith('/parties?limit=500&offset=10000');
  });
});
