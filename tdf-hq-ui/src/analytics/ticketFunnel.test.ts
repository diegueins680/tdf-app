import { jest } from '@jest/globals';
import { createTicketFunnelTracker } from './ticketFunnel';
import type { GrowthAttribution } from './growthAttribution';

describe('ticket funnel privacy and observation boundaries', () => {
  const capture = jest.fn();
  const analytics = { ready: true, capture };
  beforeEach(() => { capture.mockClear(); sessionStorage.clear(); });

  it('sends an explicit allowlist and keeps checkout/ticket scope local', () => {
    const attribution: GrowthAttribution = {
      source: 'artist', medium: 'social', campaign: 'patch_2026',
      content: 'PRIVATE-CONTENT', term: 'buyer@example.invalid', referralCode: 'PRIVATE-REFERRAL',
      landingPath: '/eventos/41/orden/92?token=PRIVATE-TOKEN', capturedAt: 'PRIVATE-DATE',
    };
    const track = createTicketFunnelTracker(analytics, sessionStorage, () => attribution);
    track('payment_completed', { eventId: 41, quantity: 2, privateScope: 'order:92:PRIVATE-TICKET' });
    expect(capture).toHaveBeenCalledWith('ticketing_payment_completed', {
      platform: 'web', event_id: 41, quantity: 2, evidence: 'browser_observation',
      attribution_source: 'artist', attribution_medium: 'social', attribution_campaign: 'patch_2026',
    });
    expect(JSON.stringify(capture.mock.calls)).not.toMatch(/PRIVATE|buyer|order:|92/);
  });

  it.each(['buyer@example.invalid', 'https://example.test/x', 'tdf-sensitive',
    'event-ticket-secret', 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', 'a'.repeat(65)])(
    'drops a sensitive or unbounded attribution value: %s', (source) => {
      createTicketFunnelTracker(analytics, null, () => ({ source, landingPath: '/', capturedAt: '' }))(
        'event_view', { eventId: 41 },
      );
      expect(capture.mock.calls[0]?.[1]).not.toHaveProperty('attribution_source');
    },
  );

  it('deduplicates refreshes and simultaneous component instances without losing other observations', () => {
    const first = createTicketFunnelTracker(analytics, sessionStorage, () => null);
    const second = createTicketFunnelTracker(analytics, sessionStorage, () => null);
    first('event_view', { eventId: 41 });
    second('event_view', { eventId: 42 });
    second('event_view', { eventId: 41 });
    createTicketFunnelTracker(analytics, sessionStorage, () => null)('event_view', { eventId: 42 });
    expect(capture).toHaveBeenCalledTimes(2);
    first('payment_completed', { eventId: 41, privateScope: 'order:1' });
    first('payment_completed', { eventId: 41, privateScope: 'order:2' });
    first('payment_completed', { eventId: 41, privateScope: 'order:2' });
    expect(capture).toHaveBeenCalledTimes(4);
  });

  it('does not capture or write storage when analytics is unavailable or the event is invalid', () => {
    const storage = { getItem: jest.fn<() => string | null>(), setItem: jest.fn() };
    createTicketFunnelTracker({ ...analytics, ready: false }, storage, () => null)('event_view', { eventId: 41 });
    const track = createTicketFunnelTracker(analytics, storage, () => null);
    for (const eventId of [0, -1, NaN, Infinity, 1.5]) track('event_view', { eventId });
    expect(capture).not.toHaveBeenCalled();
    expect(storage.setItem).not.toHaveBeenCalled();
    expect(storage.getItem).not.toHaveBeenCalled();
  });

  it('never lets blocked storage or a failing analytics SDK interrupt checkout', () => {
    const storage = { getItem: () => { throw new Error('blocked'); }, setItem: () => { throw new Error('quota'); } };
    const track = createTicketFunnelTracker(analytics, storage, () => null);
    expect(() => track('checkout_started', { eventId: 41 })).not.toThrow();
    expect(capture).toHaveBeenCalledTimes(1);
    const broken = createTicketFunnelTracker({ ready: true, capture: () => { throw new Error('SDK'); } }, null, () => null);
    expect(() => broken('checkout_started', { eventId: 41 })).not.toThrow();
  });

  it('keeps the observation cache bounded', () => {
    const track = createTicketFunnelTracker(analytics, sessionStorage, () => null);
    for (let eventId = 1; eventId <= 240; eventId += 1) track('event_view', { eventId });
    expect(JSON.parse(sessionStorage.getItem('tdf:ticket-funnel:observed:v1') ?? '[]')).toHaveLength(200);
  });
});
