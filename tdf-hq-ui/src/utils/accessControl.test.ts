import {
  buildAccessibleModuleSet,
  canAccessPath,
  isSchoolStaffRole,
} from './accessControl';

describe('buildAccessibleModuleSet', () => {
  it('does not infer staff access from public music-industry roles', () => {
    const modules = buildAccessibleModuleSet(['Manager', 'TourManager', 'LabelRep', 'StageManager'], []);

    expect(modules.size).toBe(0);
    expect(modules.has('crm')).toBe(false);
    expect(modules.has('label')).toBe(false);
    expect(modules.has('school')).toBe(false);
  });

  it('keeps legacy package aliases aligned across ops and label access', () => {
    const modules = buildAccessibleModuleSet([], ['packages']);

    expect(modules.has('packages')).toBe(true);
    expect(modules.has('ops')).toBe(true);
    expect(modules.has('label')).toBe(true);
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
    expect(canAccessPath('/escuela/clases', ['Manager'], ['school'])).toBe(true);
    expect(canAccessPath('/mi-profesor', ['Teacher'], [])).toBe(true);
  });
});
