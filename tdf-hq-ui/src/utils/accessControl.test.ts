import {
  buildAccessibleModuleSet,
  canAccessPath,
  hasAiToolingAccess,
  hasInternshipsAccess,
  hasStrictAdminAccess,
  hasSocialInboxAccess,
  hasSchoolPortalAccess,
  hasOperationsAccess,
  isSchoolStaffRole,
  normalizeAccessRoles,
} from './accessControl';

const BACKEND_ROLE_MODULES: Record<string, readonly string[]> = {
  Admin: ['crm', 'scheduling', 'packages', 'invoicing', 'admin', 'internships'],
  Manager: ['crm', 'scheduling', 'packages', 'invoicing', 'internships'],
  'Studio Manager': ['crm', 'scheduling', 'packages', 'invoicing', 'admin', 'internships'],
  Reception: ['crm', 'scheduling'],
  Accounting: ['invoicing'],
  Engineer: ['scheduling'],
  Teacher: ['scheduling'],
  'Live Sessions Producer': ['crm', 'scheduling'],
  Intern: ['internships'],
  Artist: ['scheduling', 'packages'],
  Artista: ['scheduling', 'packages'],
  Webmaster: ['admin', 'crm'],
  Promotor: [],
  Promoter: [],
  Producer: ['crm', 'scheduling'],
  Songwriter: [],
  DJ: [],
  Publicist: [],
  TourManager: [],
  LabelRep: [],
  StageManager: [],
  RoadCrew: [],
  Photographer: [],
  'A&R': ['crm', 'scheduling'],
  Student: ['scheduling'],
  Vendor: ['packages'],
  ReadOnly: ['crm'],
  Customer: ['packages'],
  Fan: [],
  Maintenance: ['packages', 'scheduling'],
};

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

const backendModulesForRoles = (roles: readonly string[]): string[] => {
  const modules = new Set<string>();
  roles.forEach((role) => {
    (BACKEND_ROLE_MODULES[role] ?? []).forEach((module) => {
      modules.add(module);
    });
  });
  return Array.from(modules);
};

const hasBackendSocialInboxAccess = (roles: readonly string[]) =>
  roles.some((role) =>
    ['Admin', 'Manager', 'Studio Manager', 'Reception', 'Live Sessions Producer', 'Producer', 'A&R', 'Webmaster'].includes(role),
  );

const hasBackendAiToolingAccess = (roles: readonly string[]) =>
  roles.some((role) => ['Admin', 'Manager', 'Studio Manager', 'Webmaster', 'Maintenance'].includes(role));

const hasBackendStrictAdminAccess = (roles: readonly string[]) =>
  roles.some((role) => ['Admin'].includes(role));

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

  it('matches the backend social inbox and AI-tooling predicates', () => {
    expect(hasSocialInboxAccess(['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(hasSocialInboxAccess(['ReadOnly'], ['CRM'])).toBe(false);
    expect(hasSocialInboxAccess(['Reception'], ['CRM', 'Scheduling'])).toBe(true);
    expect(hasAiToolingAccess(['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(hasAiToolingAccess(['Manager'], ['CRM', 'Scheduling', 'Packages'])).toBe(true);
    expect(hasAiToolingAccess(['Maintenance'], ['Packages'])).toBe(true);
    expect(hasStrictAdminAccess(['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(hasStrictAdminAccess(['Admin'], ['admin'])).toBe(true);
    expect(hasStrictAdminAccess(['Webmaster'], ['admin'])).toBe(false);
  });
});

describe('isSchoolStaffRole', () => {
  it('matches the backend school-staff role predicate instead of trusting a synthetic school module', () => {
    expect(isSchoolStaffRole(['Manager'], ['scheduling'])).toBe(true);
    expect(isSchoolStaffRole(['Reception'], ['scheduling'])).toBe(true);
    expect(isSchoolStaffRole([], ['school'])).toBe(false);
  });
});

describe('hasSchoolPortalAccess', () => {
  it('requires the same scheduling-plus-role predicate as the backend teacher portal', () => {
    expect(hasSchoolPortalAccess(['Teacher'], ['scheduling'])).toBe(true);
    expect(hasSchoolPortalAccess(['Manager'], ['scheduling'])).toBe(true);
    expect(hasSchoolPortalAccess([], ['school'])).toBe(false);
  });
});

describe('canAccessPath', () => {
  it('blocks staff-only routes for public roles and allows legitimate staff access', () => {
    expect(canAccessPath('/social', ['Fan', 'Customer'], ['Packages'])).toBe(true);
    expect(canAccessPath('/social/eventos', ['Fan', 'Customer'], ['Packages'])).toBe(true);
    expect(canAccessPath('/social/instagram', ['Fan', 'Customer'], ['Packages'])).toBe(true);
    expect(canAccessPath('/social/inbox', ['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(canAccessPath('/social/inbox', ['ReadOnly'], ['CRM'])).toBe(false);
    expect(canAccessPath('/social/inbox', ['Reception'], ['CRM', 'Scheduling'])).toBe(true);
    expect(canAccessPath('/herramientas/chatkit', ['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(canAccessPath('/herramientas/chatkit', ['Manager'], ['CRM', 'Scheduling', 'Packages'])).toBe(true);
    expect(canAccessPath('/herramientas/tidal-agent', ['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(canAccessPath('/configuracion/roles-permisos', ['Fan'], [])).toBe(false);
    expect(canAccessPath('/configuracion/roles-permisos', ['Webmaster'], ['admin'])).toBe(false);
    expect(canAccessPath('/configuracion/usuarios-admin', ['Studio Manager'], ['admin'])).toBe(false);
    expect(canAccessPath('/configuracion/usuarios-admin', ['Admin'], ['admin'])).toBe(true);
    expect(canAccessPath('/label/artistas', ['LabelRep'], [])).toBe(false);
    expect(canAccessPath('/configuracion/cms', ['Studio Manager'], ['admin'])).toBe(false);
    expect(canAccessPath('/configuracion/cms', ['Webmaster'], ['admin'])).toBe(true);
    expect(canAccessPath('/configuracion/integraciones/calendario', ['Webmaster'], ['admin'])).toBe(false);
    expect(canAccessPath('/configuracion/whatsapp-consentimiento', ['Admin'], ['admin'])).toBe(true);
    expect(canAccessPath('/perfil/7', ['Fan', 'Customer'], ['Packages'])).toBe(true);
    expect(canAccessPath('/escuela/clases', ['Manager'], ['scheduling'])).toBe(true);
    expect(canAccessPath('/escuela/clases', ['Reception'], ['scheduling'])).toBe(true);
    expect(canAccessPath('/mi-profesor', ['Teacher'], ['scheduling'])).toBe(true);
    expect(canAccessPath('/mi-profesor', [], ['school'])).toBe(false);
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
    expect(canAccessPath('/label/tracks', ['Admin'], ['admin'])).toBe(true);
    expect(canAccessPath('/label/tracks', ['Studio Manager'], ['admin'])).toBe(false);
    expect(canAccessPath('/label/tracks', ['Webmaster'], ['admin'])).toBe(false);
    expect(canAccessPath('/label/tracks', ['Fan', 'Customer'], ['Packages'])).toBe(false);
    expect(hasInternshipsAccess(['Studio Manager'], ['internships', 'admin'])).toBe(true);
    expect(canAccessPath('/practicas', ['Studio Manager'], ['internships', 'admin'])).toBe(true);
    expect(canAccessPath('/practicas', [], ['internships'])).toBe(false);
  });

  it('matches backend route predicates for key protected routes across realistic sessions', () => {
    const routes = [
      {
        path: '/social',
        expected: () => true,
      },
      {
        path: '/social/eventos',
        expected: () => true,
      },
      {
        path: '/social/instagram',
        expected: () => true,
      },
      {
        path: '/social/inbox',
        expected: (roles: readonly string[]) => hasBackendSocialInboxAccess(roles),
      },
      {
        path: '/herramientas/chatkit',
        expected: (roles: readonly string[]) => hasBackendAiToolingAccess(roles),
      },
      {
        path: '/herramientas/tidal-agent',
        expected: (roles: readonly string[]) => hasBackendAiToolingAccess(roles),
      },
      {
        path: '/mi-profesor',
        expected: (roles: readonly string[]) =>
          roles.some((role) =>
            ['Teacher', 'Admin', 'Manager', 'Studio Manager', 'Reception'].includes(role),
          ),
      },
      {
        path: '/escuela/clases',
        expected: (roles: readonly string[]) =>
          roles.some((role) => ['Admin', 'Manager', 'Studio Manager', 'Reception'].includes(role)),
      },
      {
        path: '/operacion/inventario',
        expected: (roles: readonly string[]) =>
          roles.some((role) =>
            ['Admin', 'Manager', 'Studio Manager', 'Webmaster', 'Maintenance'].includes(role),
          ),
      },
      {
        path: '/label/artistas',
        expected: (roles: readonly string[]) =>
          roles.some((role) => ['Admin', 'Studio Manager', 'Webmaster'].includes(role)),
      },
      {
        path: '/label/tracks',
        expected: (roles: readonly string[]) =>
          roles.some((role) => ['Admin', 'Artist', 'Artista'].includes(role)),
      },
      {
        path: '/configuracion/cms',
        expected: (roles: readonly string[]) =>
          roles.some((role) => ['Admin', 'Webmaster'].includes(role)),
      },
      {
        path: '/configuracion/integraciones/calendario',
        expected: (roles: readonly string[]) =>
          roles.some((role) => ['Admin'].includes(role)),
      },
      {
        path: '/configuracion/roles-permisos',
        expected: (roles: readonly string[]) => hasBackendStrictAdminAccess(roles),
      },
      {
        path: '/configuracion/usuarios-admin',
        expected: (roles: readonly string[]) => hasBackendStrictAdminAccess(roles),
      },
      {
        path: '/configuracion/whatsapp-consentimiento',
        expected: (roles: readonly string[]) =>
          roles.some((role) => ['Admin'].includes(role)),
      },
      {
        path: '/perfil/7',
        expected: () => true,
      },
      {
        path: '/practicas',
        expected: (roles: readonly string[]) =>
          roles.some((role) => ['Admin', 'Manager', 'Studio Manager', 'Intern'].includes(role)),
      },
    ] as const;

    const failures: {
      roles: string[];
      modules: string[];
      path: string;
      expected: boolean;
      actual: boolean;
    }[] = [];

    subsetsUpToTwo(Object.keys(BACKEND_ROLE_MODULES)).forEach((roles) => {
      const modules = backendModulesForRoles(roles);
      routes.forEach(({ path, expected }) => {
        const actual = canAccessPath(path, roles, modules);
        const expectedResult = expected(roles);
        if (actual !== expectedResult) {
          failures.push({
            roles: [...roles],
            modules: [...modules],
            path,
            expected: expectedResult,
            actual,
          });
        }
      });
    });

    expect(failures).toEqual([]);
  });
});
