import { jest } from '@jest/globals';
import type { PublicUpcomingEventDTO } from './socialEvents';

const getMock = jest.fn<(path: string, init?: RequestInit) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get: getMock }));
const { listArtistUpcomingEvents } = await import('./artistUpcomingEvents');
const startAfter = '2030-01-01T00:00:00.000Z';
const event = (id: string, overrides: Partial<PublicUpcomingEventDTO> = {}): PublicUpcomingEventDTO => ({
  publicUpcomingEventId: id, publicUpcomingEventTitle: `Concert ${id}`, publicUpcomingEventStart: '2030-02-01T01:00:00.000Z',
  publicUpcomingEventWorkflowStateCode: 'announced', ...overrides,
});

describe('listArtistUpcomingEvents', () => {
  beforeEach(() => getMock.mockReset());

  it('reads the public upcoming projection filtered by the artist Party', async () => {
    getMock.mockResolvedValue([
      event('earlier', { publicUpcomingEventCity: 'Miami', publicUpcomingEventVenueName: 'Vivo! Plaza' }),
      event('later', { publicUpcomingEventStart: '2030-03-01T01:00:00.000Z' }),
    ]);
    const controller = new AbortController();
    const rows = await listArtistUpcomingEvents(115, startAfter, controller.signal);
    expect(rows.map((r) => r.publicUpcomingEventId)).toEqual(['earlier', 'later']);
    expect(rows[0]).toMatchObject({ publicUpcomingEventCity: 'Miami', publicUpcomingEventVenueName: 'Vivo! Plaza' });
    expect(getMock.mock.calls).toEqual([[
      `/social-events/upcoming?artistPartyId=115&startAfter=${encodeURIComponent(startAfter)}&limit=200`,
      { signal: controller.signal },
    ]]);
  });

  it('never resolves the relationship on the client or reads the authenticated event list', async () => {
    getMock.mockResolvedValue([event('artist-date')]);
    await listArtistUpcomingEvents(115, startAfter);
    const paths = getMock.mock.calls.map(([path]) => path);
    expect(paths).toHaveLength(1);
    expect(paths.some((path) => path.startsWith('/social-events/artists') || path.startsWith('/social-events/events'))).toBe(false);
  });

  it('orders, de-duplicates and re-checks the window defensively', async () => {
    getMock.mockResolvedValue([
      event('later', { publicUpcomingEventStart: '2030-03-01T01:00:00.000Z' }),
      event('earlier'),
      event('earlier'),
      event('past', { publicUpcomingEventStart: '2029-01-01T00:00:00Z' }),
      event('invalid', { publicUpcomingEventStart: 'invalid' }),
      event('', { publicUpcomingEventId: '' }),
    ]);
    expect((await listArtistUpcomingEvents(115, startAfter)).map((r) => r.publicUpcomingEventId)).toEqual(['earlier', 'later']);
  });

  it('rejects malformed pages and propagates read failures for the carousel fallback', async () => {
    getMock.mockResolvedValue({ error: 'unexpected' });
    await expect(listArtistUpcomingEvents(115, startAfter)).rejects.toThrow('Expected a public upcoming events page');
    getMock.mockRejectedValue(new Error('offline'));
    await expect(listArtistUpcomingEvents(115, startAfter)).rejects.toThrow('offline');
  });
});
