import { jest } from '@jest/globals';

const getMock = jest.fn();
jest.unstable_mockModule('./client', () => ({ get: getMock }));

const { searchPartiesForSelector } = await import('./partySelector');

describe('searchPartiesForSelector', () => {
  beforeEach(() => jest.clearAllMocks());

  it('uses the bounded selector endpoint and carries cancellation plus exclusions', async () => {
    const signal = new AbortController().signal;
    getMock.mockResolvedValueOnce({ items: [], nextCursor: null });

    await searchPartiesForSelector({
      query: 'Ána',
      kind: 'person',
      accountOnly: true,
      excludedPartyIds: [7, 9],
      cursor: 12,
      signal,
    });

    expect(getMock).toHaveBeenCalledWith(
      '/parties/search?q=%C3%81na&context=crm_assignment&kind=person&accountOnly=true&limit=15&cursor=12&excludePartyId=7&excludePartyId=9',
      { signal },
    );
  });

  it('forwards the authorized resource scope for contextual discovery', async () => {
    getMock.mockResolvedValueOnce({ items: [], nextCursor: null });

    await searchPartiesForSelector({
      query: 'Ana',
      context: 'event_logistics',
      scopeId: '42',
    });

    expect(getMock).toHaveBeenCalledWith(
      '/parties/search?q=Ana&context=event_logistics&kind=any&accountOnly=false&limit=15&scopeId=42',
      { signal: undefined },
    );
  });
});
