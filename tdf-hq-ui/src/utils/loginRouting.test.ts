import {
  buildLoginRedirectPath,
  pickLandingPath,
  readSafeRedirectPath,
  sanitizeRedirectPath,
} from './loginRouting';
import { canAccessPath } from './accessControl';

describe('pickLandingPath', () => {
  it('sends interns to the internships panel when the backend session publishes that access', () => {
    expect(pickLandingPath(['Intern'], ['internships'])).toBe('/practicas');
  });

  it('routes strict admins to account management without forcing broader admin-module roles there', () => {
    expect(pickLandingPath(['Admin'], ['admin'])).toBe('/configuracion/roles-permisos');
    expect(pickLandingPath(['Webmaster'], ['admin', 'crm'])).toBe('/crm/contactos');
    expect(pickLandingPath(['customer'], ['internships'])).toBe('/fans');
  });

  it('does not land synthetic school-only sessions on routes the backend would reject', () => {
    expect(pickLandingPath([], ['school'])).toBe('/inicio');
  });

  it('keeps package-only sessions on public destinations', () => {
    expect(pickLandingPath(['customer'], ['packages'])).toBe('/fans');
  });

  it('does not route public music-industry roles into staff panels without explicit modules', () => {
    expect(pickLandingPath(['TourManager'])).toBe('/inicio');
    expect(pickLandingPath(['LabelRep'])).toBe('/inicio');
    expect(pickLandingPath(['TourManager'], ['crm'])).toBe('/crm/contactos');
  });

  it('always returns a route the same session can actually access', () => {
    const rolesUniverse = [
      'admin',
      'manager',
      'studio manager',
      'reception',
      'teacher',
      'artist',
      'artista',
      'maintenance',
      'intern',
      'fan',
      'customer',
      'tourmanager',
      'labelrep',
      'stagemanager',
    ] as const;
    const moduleUniverse = [
      'admin',
      'crm',
      'scheduling',
      'school',
      'invoicing',
      'packages',
      'ops',
      'label',
      'internships',
    ] as const;

    const subsetsUpToTwo = <T,>(values: readonly T[]): T[][] => {
      const combos: T[][] = [[]];
      values.forEach((value, index) => {
        combos.push([value]);
        values.slice(index + 1).forEach((other) => {
          combos.push([value, other]);
        });
      });
      return combos;
    };

    const failures: { roles: string[]; modules: string[]; path: string }[] = [];

    subsetsUpToTwo(rolesUniverse).forEach((roles) => {
      subsetsUpToTwo(moduleUniverse).forEach((modules) => {
        const path = pickLandingPath(roles, modules);
        if (!canAccessPath(path, roles, modules)) {
          failures.push({ roles: [...roles], modules: [...modules], path });
        }
      });
    });

    expect(failures).toEqual([]);
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
