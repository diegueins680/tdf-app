import { buildSignupPayload, deriveEffectiveRoles, normalizeRolesInput } from '../roles';

describe('role normalization helpers', () => {
  it('normalizes roles with trimming, case-insensitive de-dup, and filtering', () => {
    const allowed = ['Fan', 'Admin'] as const;
    const result = normalizeRolesInput([' Fan ', 'fan', 'ADMIN', 'Unknown'], allowed);
    expect(result).toEqual(['Fan', 'Admin']);
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

  it('never emits a caller-selected role field', () => {
    const payload = buildSignupPayload(baseForm, []);
    expect(payload).not.toHaveProperty('roles');
    expect(payload.fanArtistIds).toBeUndefined();
  });

  it('keeps claimable artist ids only when valid', () => {
    const claimed = buildSignupPayload(baseForm, [], 42);
    expect(claimed.claimArtistId).toBe(42);

    const ignored = buildSignupPayload(baseForm, [], 0);
    expect(ignored.claimArtistId).toBeUndefined();
  });

  it('normalizes fan artist ids to unique positive safe integers', () => {
    const payload = buildSignupPayload(baseForm, [7, 7, 0, -3, 9.5, Number.NaN, 11]);
    expect(payload.fanArtistIds).toEqual([7, 11]);
  });

  it('keeps claim artist id only when it is a positive safe integer', () => {
    const accepted = buildSignupPayload(baseForm, [], 42);
    expect(accepted.claimArtistId).toBe(42);

    const rejectedFraction = buildSignupPayload(baseForm, [], 42.5);
    expect(rejectedFraction.claimArtistId).toBeUndefined();

    const rejectedUnsafe = buildSignupPayload(baseForm, [], Number.MAX_SAFE_INTEGER + 1);
    expect(rejectedUnsafe.claimArtistId).toBeUndefined();
  });
});

describe('deriveEffectiveRoles', () => {
  it('prefers API roles when provided', () => {
    const roles = deriveEffectiveRoles(['Admin', 'Fan']);
    expect(roles).toEqual(['admin', 'fan']);
  });

  it('trims and de-duplicates API roles before returning them', () => {
    const roles = deriveEffectiveRoles(['  Fan ', 'fan', ' ADMIN  ', '']);
    expect(roles).toEqual(['fan', 'admin']);
  });

  it('does not invent a role when the authoritative response is empty', () => {
    expect(deriveEffectiveRoles(undefined)).toEqual([]);
  });
});
