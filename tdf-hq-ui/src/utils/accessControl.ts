const INTERNAL_MANAGER_MODULE_KEYS = [
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

const EXPLICIT_SCHOOL_STAFF_ROLE_KEYS = ['admin', 'reception', 'studiomanager'] as const;

export const normalizeTokens = (
  values: readonly string[] | undefined,
  { lowerCase = false }: { lowerCase?: boolean } = {},
): string[] => {
  if (!values || values.length === 0) return [];
  const seen = new Set<string>();
  const normalized: string[] = [];

  values.forEach((value) => {
    const trimmed = value.trim();
    if (trimmed === '') return;
    const normalizedValue = lowerCase ? trimmed.toLowerCase() : trimmed;
    const dedupeKey = normalizedValue.toLowerCase();
    if (seen.has(dedupeKey)) return;
    seen.add(dedupeKey);
    normalized.push(normalizedValue);
  });

  return normalized;
};

const hasAnyModule = (moduleSet: ReadonlySet<string>, needles: readonly string[]): boolean =>
  needles.some((needle) => moduleSet.has(needle));

export function normalizeAccessRoles(roles: readonly string[] | undefined): string[] {
  return normalizeTokens(roles, { lowerCase: true });
}

export function deriveModulesFromRoles(roles: readonly string[] | undefined): string[] {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = new Set<string>();

  normalizedRoles.forEach((role) => {
    switch (role) {
      case 'admin':
        moduleSet.add('admin');
        moduleSet.add('crm');
        moduleSet.add('scheduling');
        moduleSet.add('school');
        moduleSet.add('invoicing');
        moduleSet.add('packages');
        moduleSet.add('ops');
        moduleSet.add('label');
        moduleSet.add('internships');
        return;
      case 'reception':
        moduleSet.add('crm');
        moduleSet.add('scheduling');
        moduleSet.add('school');
        return;
      case 'accounting':
      case 'finance':
      case 'billing':
        moduleSet.add('invoicing');
        return;
      case 'engineer':
      case 'scheduling':
        moduleSet.add('scheduling');
        return;
      case 'teacher':
        moduleSet.add('scheduling');
        moduleSet.add('school');
        return;
      case 'intern':
        moduleSet.add('internships');
        return;
      case 'packages':
      case 'package':
        moduleSet.add('packages');
        return;
      case 'maintenance':
        moduleSet.add('packages');
        moduleSet.add('scheduling');
        moduleSet.add('ops');
        return;
      case 'label':
        moduleSet.add('label');
        return;
      case 'inventory':
      case 'operacion':
      case 'operation':
      case 'ops':
        moduleSet.add('ops');
        return;
      default:
        return;
    }
  });

  return Array.from(moduleSet);
}

export function buildAccessibleModuleSet(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): Set<string> {
  const normalizedModules = normalizeTokens(modules, { lowerCase: true });
  const moduleSet = new Set([...normalizedModules, ...deriveModulesFromRoles(roles)]);

  if (moduleSet.has('packages')) {
    moduleSet.add('ops');
    moduleSet.add('label');
  }
  if (moduleSet.has('ops')) {
    moduleSet.add('packages');
  }

  return moduleSet;
}

export function isSchoolStaffRole(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined = [],
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  if (EXPLICIT_SCHOOL_STAFF_ROLE_KEYS.some((needle) => normalizedRoles.includes(needle))) {
    return true;
  }
  if (!normalizedRoles.includes('manager')) {
    return false;
  }

  const moduleSet = buildAccessibleModuleSet(roles, modules);
  return hasAnyModule(moduleSet, INTERNAL_MANAGER_MODULE_KEYS);
}

export function pathRequiresSchoolStaff(path: string): boolean {
  if (path.startsWith('/escuela/profesores')) return true;
  if (path.startsWith('/escuela/clases')) return true;
  if (path.startsWith('/escuela/trial-lessons')) return true;
  if (path.startsWith('/escuela/trial-queue')) return true;
  return false;
}

export function pathRequiresModule(path: string): string | null {
  if (path.startsWith('/crm')) return 'crm';
  if (path.startsWith('/social')) return 'crm';
  if (path.startsWith('/estudio')) return 'scheduling';
  if (path.startsWith('/finanzas')) return 'invoicing';
  if (path.startsWith('/configuracion')) return 'admin';
  if (path.startsWith('/admin')) return 'admin';
  if (path.startsWith('/herramientas/token-admin')) return 'admin';
  if (path.startsWith('/operacion')) return 'ops';
  if (path.startsWith('/label')) return 'label';
  if (path.startsWith('/escuela')) return 'school';
  if (path.startsWith('/mi-profesor')) return 'school';
  if (path.startsWith('/practicas')) return 'internships';
  if (path.startsWith('/eventos')) return 'scheduling';
  return null;
}

export function canAccessPath(
  path: string,
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  if (pathRequiresSchoolStaff(path) && !isSchoolStaffRole(roles, modules)) {
    return false;
  }

  const requiredModule = pathRequiresModule(path);
  if (!requiredModule) {
    return true;
  }

  const moduleSet = buildAccessibleModuleSet(roles, modules);
  return moduleSet.has(requiredModule);
}
