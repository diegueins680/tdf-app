import { readFileSync } from 'node:fs';

describe('BookingsPage party relationships', () => {
  const source = readFileSync(new URL('./BookingsPage.tsx', import.meta.url), 'utf8');

  it('uses the server-backed selector instead of downloading the Party directory', () => {
    expect(source).toContain('<PartySelector');
    expect(source).not.toMatch(/Parties\.list\s*\(/);
    expect(source).not.toMatch(/displayName\.toLowerCase\(\).*partyId/s);
  });

  it('persists only canonical Party IDs from selected options', () => {
    expect(source).toContain('setCustomerPartyId(party?.partyId ?? null)');
    expect(source).toContain('setEngineerPartyId(party?.partyId ?? null)');
    expect(source).toContain('El texto escrito no se guarda como cliente.');
  });
});
