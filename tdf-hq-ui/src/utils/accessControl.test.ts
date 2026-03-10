import {
  buildAccessibleModuleSet,
  canAccessPath,
  hasOperationsAccess,
  isSchoolStaffRole,
  normalizeAccessRoles,
} from './accessControl';

describe('buildAccessibleModuleSet', () => {
  it('does not infer staff access from public music-industry roles', () => {
    const modules = buildAccessibleModuleSet(['TourManager', 'LabelRep', 'StageManager'], []);

    expect(modules.size).toBe(0);
    expect(modules.has('crm')).toBe(false);
    expect(modules.has('label')).toBe(false);
    expect(modules.has('school')).toBe(false);
  });

  it('does not treat packages access as implicit label or operations access', () => {
    const modules = buildAccessibleModuleSet(['Fan', 'Customer'], ['Packages']);

    expect(modules.has('packages')).toBe(true);
    expect(modules.has('ops')).toBe(false);
    expect(modules.has('label')).toBe(false);
  });

  it('normalizes backend role labels before deriving capabilities', () => {
    expect(normalizeAccessRoles([' Studio Manager ', 'studio-manager', 'STUDIO MANAGER'])).toEqual([
      'studiomanager',
    ]);
  });

  it('grants operations access only to real operations staff or admins', () => {
    expect(hasOperationsAccess(['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(hasOperationsAccess(['Manager'], ['CRM', 'Scheduling', 'Packages'])).toBe(true);
    expect(hasOperationsAccess(['Maintenance'], ['Packages'])).toBe(true);
  });
});

describe('isSchoolStaffRole', () => {
  it('only treats manager as school staff when the session carries staff modules', () => {
    expect(isSchoolStaffRole(['Manager'], [])).toBe(false);
    expect(isSchoolStaffRole(['Manager'], ['school'])).toBe(true);
  });
});

describe('canAccessPath', () => {
  it('blocks staff-only routes for public roles and allows legitimate staff access', () => {
    expect(canAccessPath('/configuracion/roles-permisos', ['Fan'], [])).toBe(false);
    expect(canAccessPath('/label/artistas', ['LabelRep'], [])).toBe(false);
    expect(canAccessPath('/escuela/clases', ['Manager'], [])).toBe(false);
    expect(canAccessPath('/escuela/clases', ['Reception'], ['school'])).toBe(true);
    expect(canAccessPath('/mi-profesor', ['Teacher'], [])).toBe(true);
  });

  it('keeps package-only customer sessions out of operations and label routes', () => {
    expect(canAccessPath('/operacion/inventario', ['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(canAccessPath('/label/artistas', ['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(canAccessPath('/marketplace', ['Fan', 'Customer'], ['Packages'])).toBe(true);
  });

  it('matches route-specific backend access rules for operations and label tracks', () => {
    expect(canAccessPath('/operacion/inventario', ['Manager'], ['CRM', 'Scheduling', 'Packages'])).toBe(true);
    expect(canAccessPath('/label/assets', ['Maintenance'], ['Packages'])).toBe(true);
    expect(canAccessPath('/label/tracks', ['Artist'], ['Scheduling', 'Packages'])).toBe(true);
    expect(canAccessPath('/label/tracks', ['Fan', 'Customer'], ['Packages'])).toBe(false);
  });
});
