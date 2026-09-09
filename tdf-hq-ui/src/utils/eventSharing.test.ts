import {
  buildEventShareMessage,
  canonicalEventPath,
  canonicalEventUrl,
  safePublicImageUrl,
} from './eventSharing';

describe('event sharing', () => {
  it('always builds the public Spanish route and rejects malformed identifiers', () => {
    expect(canonicalEventPath(' 42 ')).toBe('/eventos/42');
    expect(() => canonicalEventPath('../social/eventos/42')).toThrow('Invalid public event identifier');
  });

  it('preserves only allow-listed anonymous attribution values', () => {
    expect(canonicalEventUrl('https://tdf.example', '42', {
      utm_source: 'tdf_web',
      utm_medium: 'share',
      utm_campaign: 'event_rsvp',
      partyId: '99',
      token: 'secret',
      utm_term: 'name@example.com',
    })).toBe('https://tdf.example/eventos/42?utm_source=tdf_web&utm_medium=share&utm_campaign=event_rsvp');
  });

  it('localizes RSVP share copy without exposing identity', () => {
    const spanish = buildEventShareMessage({
      eventId: '42',
      title: 'Festival TDF',
      start: '2030-03-02T20:00:00Z',
      timezone: 'UTC',
      venue: 'Teatro Sucre',
      status: 'accepted',
      locale: 'es-EC',
    });
    const english = buildEventShareMessage({ eventId: '42', title: 'Festival TDF', status: 'maybe', locale: 'en-US' });

    expect(spanish).toContain('Voy a Festival TDF');
    expect(spanish).toContain('Teatro Sucre');
    expect(english).toBe("I'm interested in Festival TDF. Take a look.");
    expect(`${spanish}${english}`).not.toMatch(/party|email|token/i);
  });

  it('accepts only credential-free HTTPS or same-origin public preview images', () => {
    expect(safePublicImageUrl('/poster.png', 'https://tdf.example')).toBe('https://tdf.example/poster.png');
    expect(safePublicImageUrl('/poster.png', 'http://localhost:5173')).toBe('http://localhost:5173/poster.png');
    expect(safePublicImageUrl('http://cdn.example/poster.png', 'https://tdf.example')).toBeUndefined();
    expect(safePublicImageUrl('https://user:password@cdn.example/poster.png', 'https://tdf.example')).toBeUndefined();
    expect(safePublicImageUrl('javascript:alert(1)', 'https://tdf.example')).toBeUndefined();
    expect(safePublicImageUrl('data:image/png;base64,abc', 'https://tdf.example')).toBeUndefined();
  });
});
