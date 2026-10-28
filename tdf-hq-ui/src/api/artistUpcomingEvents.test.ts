import { jest } from '@jest/globals';
import type { SocialEventDTO } from './socialEvents';

const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get: getMock }));
const { listArtistUpcomingEvents } = await import('./artistUpcomingEvents');
const startAfter = '2030-01-01T00:00:00.000Z';
const artist = { artistId: '50', artistPartyId: '115', artistName: 'Verde 70' };
const event = (id: string, overrides: Partial<SocialEventDTO> = {}): SocialEventDTO => ({
  eventId: id, eventTitle: `Concert ${id}`, eventStart: '2030-02-01T01:00:00.000Z',
  eventIsPublic: true, eventPublicListable: true, eventWorkflowStateCode: 'announced',
  eventArtists: [artist], ...overrides,
});

describe('listArtistUpcomingEvents', () => {
  beforeEach(() => getMock.mockReset());

  it('resolves the Party relationship, filters before ordering, and projects only public event data', async () => {
    getMock.mockImplementation(async (path) => {
      if (path.startsWith('/social-events/artists?')) return [
        { ...artist, artistId: '115', artistPartyId: '999' }, artist,
        { ...artist, artistId: '51' },
      ];
      if (path.startsWith('/social-events/venues/')) return { venueName: 'Vivo! Plaza', venueCity: 'Miami' };
      return [
        event('later', { eventStart: '2030-03-01T01:00:00.000Z', eventVenueId: '30' }),
        event('earlier', { eventVenueId: '30', eventBudgetCents: 100000, eventOrganizerPartyId: '9' }),
        event('private', { eventIsPublic: false }),
        event('draft', { eventPublicListable: false }),
        event('past', { eventStart: '2029-01-01T00:00:00Z' }),
        event('invalid', { eventStart: 'invalid' }),
        event('wrong-artist', { eventArtists: [{ ...artist, artistPartyId: '999' }] }),
      ];
    });
    const controller = new AbortController();
    const rows = await listArtistUpcomingEvents(115, startAfter, controller.signal);
    expect(rows.map((r) => r.publicUpcomingEventId)).toEqual(['earlier', 'later']);
    expect(rows[0]).toMatchObject({ publicUpcomingEventCity: 'Miami', publicUpcomingEventVenueName: 'Vivo! Plaza' });
    expect(JSON.stringify(rows)).not.toMatch(/eventBudgetCents|eventOrganizerPartyId/);
    expect(getMock.mock.calls.filter(([path]) => path.includes('/events?')).map(([path]) => path))
      .toEqual(['50', '51'].map((id) => `/social-events/events?artistId=${id}&start_after=${encodeURIComponent(startAfter)}&limit=500&offset=0`));
    expect(getMock.mock.calls.filter(([path]) => path.includes('/venues/'))).toHaveLength(1);
    expect(getMock.mock.calls.every(([, init]) => init?.signal === controller.signal)).toBe(true);
  });

  it('loads every artist event page rather than capping artist dates at the general carousel limit', async () => {
    getMock.mockImplementation(async (path) => {
      if (path.includes('/artists?')) return [artist];
      if (path.endsWith('offset=0')) return Array.from({ length: 500 }, (_, i) => event(String(i)));
      return [event('500')];
    });
    const rows = await listArtistUpcomingEvents(115, startAfter);
    expect(rows).toHaveLength(501);
    expect(rows.some((r) => r.publicUpcomingEventId === '500')).toBe(true);
    expect(getMock.mock.calls.some(([path]) => path.endsWith('offset=500'))).toBe(true);
  });

  it('finds a canonical artist beyond the first catalog page without matching names', async () => {
    getMock.mockImplementation(async (path) => {
      if (path.includes('/artists?')) return path.endsWith('offset=0')
        ? Array.from({ length: 500 }, (_, i) => ({ ...artist, artistPartyId: String(1000 + i) }))
        : [{ ...artist, artistName: 'A previous display name' }];
      return [event('artist-date')];
    });
    expect((await listArtistUpcomingEvents(115, startAfter)).map((r) => r.publicUpcomingEventId)).toEqual(['artist-date']);
  });

  it('does not guess a relationship when only the display name matches', async () => {
    getMock.mockResolvedValue([{ ...artist, artistPartyId: '999' }]);
    expect(await listArtistUpcomingEvents(115, startAfter)).toEqual([]);
    expect(getMock).toHaveBeenCalledTimes(1);
  });

  it('rejects malformed pages and propagates read failures for the carousel fallback', async () => {
    getMock.mockResolvedValue({ error: 'unexpected' });
    await expect(listArtistUpcomingEvents(115, startAfter)).rejects.toThrow('Expected an event catalog page');
    getMock.mockRejectedValue(new Error('offline'));
    await expect(listArtistUpcomingEvents(115, startAfter)).rejects.toThrow('offline');
  });
});
