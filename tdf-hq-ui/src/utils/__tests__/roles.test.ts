import { buildSignupPayload, deriveEffectiveRoles, normalizeRolesInput, normalizeSignupRoles } from '../roles';
import { SELF_SIGNUP_ROLES } from '../../constants/roles';

describe('role normalization helpers', () => {
  it('normalizes roles with trimming, case-insensitive de-dup, and filtering', () => {
    const allowed = ['Fan', 'Admin'] as const;
    const result = normalizeRolesInput([' Fan ', 'fan', 'ADMIN', 'Unknown'], allowed);
    expect(result).toEqual(['Fan', 'Admin']);
  });

  it('normalizes signup role query values regardless of casing', () => {
    const result = normalizeSignupRoles('fan,intern,ARTISTA,promotor,bogus');
    expect(result).toEqual(['Fan', 'Intern', 'Artista', 'Promotor']);
  });

  it('splits comma-separated role values even when query parsing returns an array', () => {
    const result = normalizeSignupRoles([' fan,intern ', 'ARTISTA', 'promotor,bogus']);
    expect(result).toEqual(['Fan', 'Intern', 'Artista', 'Promotor']);
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
    internshipStartAt: '',
    internshipEndAt: '',
    internshipRequiredHours: '',
    internshipSkills: '',
    internshipAreas: '',
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

  it('keeps claimable artist ids only when valid', () => {
    const claimed = buildSignupPayload(baseForm, ['Artista'], [], 42);
    expect(claimed.claimArtistId).toBe(42);

    const ignored = buildSignupPayload(baseForm, ['Artista'], [], 0);
    expect(ignored.claimArtistId).toBeUndefined();
  });

  it('includes internship required hours when Intern is selected', () => {
    const form = { ...baseForm, internshipRequiredHours: '120' };
    const payload = buildSignupPayload(form, ['Intern'], []);
    expect(payload.internshipRequiredHours).toBe(120);
  });

  it('normalizes fan artist ids to unique positive safe integers', () => {
    const payload = buildSignupPayload(baseForm, ['Fan'], [7, 7, 0, -3, 9.5, Number.NaN, 11]);
    expect(payload.fanArtistIds).toEqual([7, 11]);
  });

  it('keeps claim artist id only when it is a positive safe integer', () => {
    const accepted = buildSignupPayload(baseForm, ['Artista'], [], 42);
    expect(accepted.claimArtistId).toBe(42);

    const rejectedFraction = buildSignupPayload(baseForm, ['Artista'], [], 42.5);
    expect(rejectedFraction.claimArtistId).toBeUndefined();

    const rejectedUnsafe = buildSignupPayload(baseForm, ['Artista'], [], Number.MAX_SAFE_INTEGER + 1);
    expect(rejectedUnsafe.claimArtistId).toBeUndefined();
  });
});

describe('deriveEffectiveRoles', () => {
  it('prefers API roles when provided', () => {
    const roles = deriveEffectiveRoles(['Admin', 'Fan'], ['Fan']);
    expect(roles).toEqual(['admin', 'fan']);
  });

  it('trims and de-duplicates API roles before returning them', () => {
    const roles = deriveEffectiveRoles(['  Fan ', 'fan', ' ADMIN  ', ''], ['Artista']);
    expect(roles).toEqual(['fan', 'admin']);
  });

  it('falls back to selected roles when API returns none', () => {
    const roles = deriveEffectiveRoles([], ['Fan', 'Artista']);
    expect(roles).toEqual(['fan', 'artista']);
  });

  it('falls back to default when both API and selections are empty', () => {
    const roles = deriveEffectiveRoles(undefined, []);
    expect(roles).toEqual(['fan']);
  });

  it('uses fan fallback when default role is blank', () => {
    const roles = deriveEffectiveRoles(undefined, [], '   ');
    expect(roles).toEqual(['fan']);
  });
});
