import { buildSignupPayload, deriveEffectiveRoles, normalizeRolesInput, normalizeSignupRoles } from '../roles';
import { SELF_SIGNUP_ROLES } from '../../constants/roles';

describe('role normalization helpers', () => {
  it('normalizes roles with trimming, de-dup, and filtering', () => {
    const allowed = ['Fan', 'Admin'] as const;
    const result = normalizeRolesInput([' Fan ', 'fan', 'Admin', 'Unknown'], allowed);
    expect(result).toEqual(['Fan', 'Admin']);
  });

  it('normalizes signup roles using the allowlist', () => {
    const result = normalizeSignupRoles(['Fan', 'Admin', 'Promotor', 'Bogus'] as string[]);
    // Fan/Promotor are allowed; Admin is filtered for self-signup; Bogus is dropped; duplicates removed.
    expect(result).toEqual(['Fan', 'Promotor']);
    expect(result.every((r) => SELF_SIGNUP_ROLES.includes(r))).toBe(true);
  });
});

describe('signup payload builder', () => {
  const baseForm = {
    firstName: ' Ana ',
    lastName: '   ',
    email: 'ana@tdf.com ',
    phone: '  ',
    password: 'changeme123',
  };

  it('includes fanArtistIds only when Fan is selected', () => {
    const payloadWithFan = buildSignupPayload(baseForm, ['Fan', 'Artista'], [1, 2]);
    expect(payloadWithFan.roles).toEqual(['Fan', 'Artista']);
    expect(payloadWithFan.fanArtistIds).toEqual([1, 2]);

    const payloadWithoutFan = buildSignupPayload(baseForm, ['Artista'], [1, 2]);
    expect(payloadWithoutFan.roles).toEqual(['Artista']);
    expect(payloadWithoutFan.fanArtistIds).toBeUndefined();
  });

  it('omits roles and fanArtistIds when nothing valid is selected', () => {
    const payload = buildSignupPayload(baseForm, [], [99]);
    expect(payload.roles).toBeUndefined();
    expect(payload.fanArtistIds).toBeUndefined();
  });
});

describe('deriveEffectiveRoles', () => {
  it('prefers API roles when provided', () => {
    const roles = deriveEffectiveRoles(['Admin', 'Fan'], ['Fan']);
    expect(roles).toEqual(['admin', 'fan']);
  });

  it('falls back to selected roles when API returns none', () => {
    const roles = deriveEffectiveRoles([], ['Fan', 'Artista']);
    expect(roles).toEqual(['fan', 'artista']);
  });

  it('falls back to default when both API and selections are empty', () => {
    const roles = deriveEffectiveRoles(undefined, []);
    expect(roles).toEqual(['fan']);
  });
});
