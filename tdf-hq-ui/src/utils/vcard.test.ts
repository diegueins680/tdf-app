import { buildVCardSharePayload, parseVCardPayload } from './vcard';

describe('vcard payload utils', () => {
  it('buildVCardSharePayload trims text fields and keeps only valid party ids', () => {
    const raw = buildVCardSharePayload({
      name: '  Diego  ',
      email: '  diego@example.com ',
      phone: '   ',
      partyId: 12.5,
    });
    const parsed = JSON.parse(raw) as Record<string, unknown>;
    expect(parsed['kind']).toBe('vcard-exchange');
    expect(parsed['name']).toBe('Diego');
    expect(parsed['email']).toBe('diego@example.com');
    expect(parsed['phone']).toBeUndefined();
    expect(parsed['partyId']).toBeNull();
    expect(typeof parsed['ts']).toBe('number');
  });

  it('parseVCardPayload accepts numeric party id strings and sanitizes fields', () => {
    const parsed = parseVCardPayload(
      JSON.stringify({
        kind: 'vcard-exchange',
        name: '  Persona  ',
        email: 'persona@example.com',
        phone: 12345,
        partyId: '42',
        ts: 1_700_000_000_000,
      }),
    );
    expect(parsed).toEqual({
      kind: 'vcard-exchange',
      name: 'Persona',
      email: 'persona@example.com',
      phone: null,
      partyId: 42,
      ts: 1_700_000_000_000,
    });
  });

  it('parseVCardPayload rejects invalid payload kind', () => {
    expect(parseVCardPayload(JSON.stringify({ kind: 'another-kind', partyId: 1 }))).toBeNull();
  });

  it('parseVCardPayload ignores invalid timestamp and party id', () => {
    const parsed = parseVCardPayload(
      JSON.stringify({
        kind: 'vcard-exchange',
        name: 'Nombre',
        partyId: '-12',
        ts: 'yesterday',
      }),
    );
    expect(parsed).toEqual({
      kind: 'vcard-exchange',
      name: 'Nombre',
      email: null,
      phone: null,
      partyId: null,
      ts: undefined,
    });
  });
});
