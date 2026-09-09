import {
  clearEventRsvpIntent,
  eventRsvpIntentStorageKey,
  readEventRsvpIntent,
  saveEventRsvpIntent,
} from './eventRsvpIntent';

const memoryStorage = () => {
  const values = new Map<string, string>();
  return {
    getItem: (key: string) => values.get(key) ?? null,
    setItem: (key: string, value: string) => { values.set(key, value); },
    removeItem: (key: string) => { values.delete(key); },
  };
};

describe('event RSVP auth intent', () => {
  it('stores an anonymous, bounded, expiring intent and reads it only for its event', () => {
    const storage = memoryStorage();
    const intent = saveEventRsvpIntent({
      eventId: '42',
      status: 'accepted',
      showOnProfile: true,
      origin: 'public_event_detail',
      sharedAttribution: true,
    }, storage, 1_000);

    expect(intent.returnTo).toBe('/eventos/42');
    expect(intent.sharedAttribution).toBe(true);
    expect(JSON.parse(storage.getItem(eventRsvpIntentStorageKey) ?? '{}')).not.toHaveProperty('partyId');
    expect(readEventRsvpIntent('42', storage, 1_001)).toEqual(intent);
    expect(readEventRsvpIntent('43', storage, 1_001)).toBeNull();
    expect(readEventRsvpIntent('42', storage, intent.expiresAt)).toBeNull();
  });

  it('survives failed auth until explicitly cleared after success or cancellation', () => {
    const storage = memoryStorage();
    saveEventRsvpIntent({ eventId: '7', status: 'maybe', showOnProfile: false, origin: 'event_card' }, storage, 5_000);
    expect(readEventRsvpIntent('7', storage, 5_001)).not.toBeNull();
    clearEventRsvpIntent(storage);
    expect(readEventRsvpIntent('7', storage, 5_002)).toBeNull();
  });

  it('fails closed for tampered return routes and invalid status values', () => {
    const storage = memoryStorage();
    const intent = saveEventRsvpIntent({ eventId: '7', status: 'accepted', showOnProfile: true, origin: 'event_card' }, storage, 5_000);
    storage.setItem(eventRsvpIntentStorageKey, JSON.stringify({ ...intent, returnTo: '//evil.example', status: 'NONE' }));
    expect(readEventRsvpIntent('7', storage, 5_001)).toBeNull();
  });
});
