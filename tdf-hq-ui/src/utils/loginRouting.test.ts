import {
  buildLoginRedirectPath,
  pickLandingPath,
  readSafeRedirectPath,
  sanitizeRedirectPath,
} from './loginRouting';

describe('pickLandingPath', () => {
  it('sends interns to the internships panel even without explicit modules', () => {
    expect(pickLandingPath(['Intern'])).toBe('/practicas');
  });

  it('keeps fan users with staff modules on the staff destination instead of the fan hub', () => {
    expect(pickLandingPath(['fan'], ['label'])).toBe('/label/artistas');
    expect(pickLandingPath(['customer'], ['internships'])).toBe('/practicas');
  });

  it('maps school-only access to the school landing page', () => {
    expect(pickLandingPath([], ['school'])).toBe('/escuela/clases');
  });

  it('honors legacy packages access as label/ops access', () => {
    expect(pickLandingPath([], ['packages'])).toBe('/label/artistas');
  });

  it('does not route public music-industry roles into staff panels without explicit modules', () => {
    expect(pickLandingPath(['Manager'])).toBe('/inicio');
    expect(pickLandingPath(['TourManager'])).toBe('/inicio');
    expect(pickLandingPath(['LabelRep'])).toBe('/inicio');
    expect(pickLandingPath(['Manager'], ['crm'])).toBe('/crm/contactos');
  });
});

describe('redirect helpers', () => {
  it('accepts local app paths with search and hash fragments', () => {
    expect(sanitizeRedirectPath('/fans?tab=artists#radio')).toBe('/fans?tab=artists#radio');
    expect(readSafeRedirectPath('?redirect=%2Freservar%3Froom%3Da%23slot-2')).toBe('/reservar?room=a#slot-2');
  });

  it('rejects malformed, external, and self-loop redirects', () => {
    expect(sanitizeRedirectPath(null)).toBeNull();
    expect(sanitizeRedirectPath('https://example.com')).toBeNull();
    expect(sanitizeRedirectPath('//example.com')).toBeNull();
    expect(sanitizeRedirectPath('/login?signup=1')).toBeNull();
    expect(readSafeRedirectPath('?redirect=%2Flogin%3Fsignup%3D1')).toBeNull();
  });

  it('builds login links that preserve a valid return path and drops invalid ones', () => {
    expect(buildLoginRedirectPath('/fans?tab=artists#radio')).toBe(
      '/login?redirect=%2Ffans%3Ftab%3Dartists%23radio',
    );
    expect(buildLoginRedirectPath('/login')).toBe('/login');
  });
});
