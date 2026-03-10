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

const SCHOOL_STAFF_ROLE_KEYS = ['admin', 'manager', 'reception', 'studiomanager'] as const;
const OPERATIONS_ROLE_KEYS = ['manager', 'maintenance'] as const;
const ADMIN_ROLE_KEYS = ['admin'] as const;
const SOCIAL_INBOX_ROLE_KEYS = [
  'admin',
  'manager',
  'studiomanager',
  'reception',
  'livesessionsproducer',
  'producer',
  'a&r',
  'webmaster',
] as const;
const LABEL_TRACK_ROLE_KEYS = ['artist', 'artista'] as const;
const CMS_EDITOR_ROLE_KEYS = ['admin', 'webmaster'] as const;
const INTERNSHIPS_ADMIN_ROLE_KEYS = ['admin', 'manager', 'studiomanager'] as const;
const INTERNSHIPS_MEMBER_ROLE_KEYS = ['intern'] as const;
const ROLE_ALIASES: Record<string, string> = {
  'live sessions producer': 'livesessionsproducer',
  'live-sessions-producer': 'livesessionsproducer',
  'studio manager': 'studiomanager',
  'studio-manager': 'studiomanager',
};

const normalizeAccessRole = (value: string): string => {
  const normalized = value.trim().toLowerCase();
  if (normalized === '') return '';
  return ROLE_ALIASES[normalized] ?? normalized;
};

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

const hasAnyRole = (normalizedRoles: readonly string[], needles: readonly string[]): boolean =>
  needles.some((needle) => normalizedRoles.includes(needle));

export function normalizeAccessRoles(roles: readonly string[] | undefined): string[] {
  if (!roles || roles.length === 0) return [];
  const seen = new Set<string>();
  const normalized: string[] = [];

  roles.forEach((role) => {
    const normalizedRole = normalizeAccessRole(role);
    if (!normalizedRole || seen.has(normalizedRole)) return;
    seen.add(normalizedRole);
    normalized.push(normalizedRole);
  });

  return normalized;
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
        moduleSet.add('internships');
        return;
      case 'manager':
        moduleSet.add('crm');
        moduleSet.add('scheduling');
        moduleSet.add('invoicing');
        moduleSet.add('packages');
        moduleSet.add('ops');
        moduleSet.add('internships');
        return;
      case 'studiomanager':
        moduleSet.add('admin');
        moduleSet.add('crm');
        moduleSet.add('scheduling');
        moduleSet.add('school');
        moduleSet.add('invoicing');
        moduleSet.add('packages');
        moduleSet.add('ops');
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
      case 'livesessionsproducer':
      case 'producer':
      case 'a&r':
      case 'aandr':
      case 'ar':
        moduleSet.add('crm');
        moduleSet.add('scheduling');
        return;
      case 'webmaster':
        moduleSet.add('admin');
        moduleSet.add('crm');
        return;
      case 'readonly':
        moduleSet.add('crm');
        return;
      case 'teacher':
        moduleSet.add('scheduling');
        moduleSet.add('school');
        return;
      case 'intern':
        moduleSet.add('internships');
        return;
      case 'artist':
      case 'artista':
        moduleSet.add('scheduling');
        moduleSet.add('packages');
        return;
      case 'student':
        moduleSet.add('scheduling');
        return;
      case 'vendor':
      case 'customer':
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

  if (moduleSet.has('admin')) {
    moduleSet.add('label');
  }
  if (moduleSet.has('ops') || hasAnyRole(normalizeAccessRoles(roles), OPERATIONS_ROLE_KEYS)) {
    moduleSet.add('packages');
  }

  return moduleSet;
}

export function hasOperationsAccess(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = buildAccessibleModuleSet(roles, modules);
  return moduleSet.has('admin') || hasAnyRole(normalizedRoles, OPERATIONS_ROLE_KEYS);
}

export function hasStrictAdminAccess(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = buildAccessibleModuleSet(roles, modules);
  return moduleSet.has('admin') && hasAnyRole(normalizedRoles, ADMIN_ROLE_KEYS);
}

export function hasAiToolingAccess(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  return hasOperationsAccess(roles, modules);
}

export function hasSocialInboxAccess(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = buildAccessibleModuleSet(roles, modules);
  return moduleSet.has('crm') && hasAnyRole(normalizedRoles, SOCIAL_INBOX_ROLE_KEYS);
}

export function canAccessLabelCatalog(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  return buildAccessibleModuleSet(roles, modules).has('admin');
}

export function canAccessLabelTracks(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = buildAccessibleModuleSet(roles, modules);
  return moduleSet.has('admin')
    ? hasAnyRole(normalizedRoles, [...ADMIN_ROLE_KEYS, ...LABEL_TRACK_ROLE_KEYS])
    : hasAnyRole(normalizedRoles, LABEL_TRACK_ROLE_KEYS);
}

export function canAccessCmsAdmin(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = buildAccessibleModuleSet(roles, modules);
  return moduleSet.has('admin') && hasAnyRole(normalizedRoles, CMS_EDITOR_ROLE_KEYS);
}

export function canAccessAdminOnlyRoute(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  return hasStrictAdminAccess(roles, modules);
}

export function hasInternshipsAdminAccess(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = buildAccessibleModuleSet(roles, modules);
  return moduleSet.has('internships') && hasAnyRole(normalizedRoles, INTERNSHIPS_ADMIN_ROLE_KEYS);
}

export function hasInternshipsAccess(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined,
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  return hasInternshipsAdminAccess(roles, modules)
    || (buildAccessibleModuleSet(roles, modules).has('internships')
      && hasAnyRole(normalizedRoles, INTERNSHIPS_MEMBER_ROLE_KEYS));
}

export function isSchoolStaffRole(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined = [],
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  if (!hasAnyRole(normalizedRoles, SCHOOL_STAFF_ROLE_KEYS)) {
    return false;
  }

  const moduleSet = buildAccessibleModuleSet(roles, modules);
  return moduleSet.has('scheduling') || hasAnyModule(moduleSet, INTERNAL_MANAGER_MODULE_KEYS);
}

export function hasSchoolPortalAccess(
  roles: readonly string[] | undefined,
  modules: readonly string[] | undefined = [],
): boolean {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = buildAccessibleModuleSet(roles, modules);
  if (!moduleSet.has('scheduling')) {
    return false;
  }

  return hasAnyRole(normalizedRoles, ['teacher']) || isSchoolStaffRole(normalizedRoles, modules);
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
  if (path.startsWith('/social/inbox')) return 'crm';
  if (path.startsWith('/estudio')) return 'scheduling';
  if (path.startsWith('/finanzas')) return 'invoicing';
  if (path.startsWith('/configuracion')) return 'admin';
  if (path.startsWith('/admin')) return 'admin';
  if (path.startsWith('/herramientas/chatkit')) return 'ops';
  if (path.startsWith('/herramientas/tidal-agent')) return 'ops';
  if (path.startsWith('/herramientas/token-admin')) return 'admin';
  if (path.startsWith('/operacion')) return 'ops';
  if (path.startsWith('/label/assets')) return 'ops';
  if (path.startsWith('/label/tracks')) return 'artist';
  if (path.startsWith('/label')) return 'admin';
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
  if (path.startsWith('/configuracion/roles-permisos') || path.startsWith('/configuracion/usuarios-admin')) {
    return hasStrictAdminAccess(roles, modules);
  }
  if (path.startsWith('/admin/roles')) {
    return hasStrictAdminAccess(roles, modules);
  }
  if (path.startsWith('/social/inbox')) {
    return hasSocialInboxAccess(roles, modules);
  }
  if (path.startsWith('/social')) {
    return true;
  }
  if (path.startsWith('/herramientas/chatkit') || path.startsWith('/herramientas/tidal-agent')) {
    return hasAiToolingAccess(roles, modules);
  }
  if (path.startsWith('/configuracion/cms')) {
    return canAccessCmsAdmin(roles, modules);
  }
  if (path.startsWith('/configuracion/integraciones/calendario')) {
    return canAccessAdminOnlyRoute(roles, modules);
  }
  if (path.startsWith('/configuracion/whatsapp-consentimiento')) {
    return canAccessAdminOnlyRoute(roles, modules);
  }
  if (path.startsWith('/practicas')) {
    return hasInternshipsAccess(roles, modules);
  }
  if (path.startsWith('/perfil/')) {
    return true;
  }
  if (path.startsWith('/mi-profesor')) {
    return hasSchoolPortalAccess(roles, modules);
  }
  if (path === '/escuela' || path.startsWith('/escuela/')) {
    return isSchoolStaffRole(roles, modules);
  }
  if (path.startsWith('/label/tracks')) {
    return canAccessLabelTracks(roles, modules);
  }
  if (path.startsWith('/label/assets')) {
    return hasOperationsAccess(roles, modules);
  }
  if (path.startsWith('/label')) {
    return canAccessLabelCatalog(roles, modules);
  }
  if (path.startsWith('/operacion')) {
    return hasOperationsAccess(roles, modules);
  }
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
