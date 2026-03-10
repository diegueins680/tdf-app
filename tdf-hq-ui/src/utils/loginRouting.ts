import {
  buildAccessibleModuleSet,
  canAccessLabelCatalog,
  canAccessLabelTracks,
  canAccessPath,
  hasSchoolPortalAccess,
  hasOperationsAccess,
  isSchoolStaffRole,
  normalizeAccessRoles,
} from './accessControl';

const LOGIN_ROUTE = '/login';
const URL_BASE = 'https://tdf.local';

export function pickLandingPath(roles: readonly string[], modules?: readonly string[]): string {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = buildAccessibleModuleSet(normalizedRoles, modules);
  const hasRole = (...needles: string[]) =>
    needles.some((needle) => normalizedRoles.includes(needle));
  const candidates = [
    hasRole('admin') || moduleSet.has('admin') ? '/configuracion/roles-permisos' : null,
    hasRole('artist', 'artista') ? '/mi-artista' : null,
    hasRole('teacher') ? '/mi-profesor' : null,
    moduleSet.has('scheduling') ? '/estudio/calendario' : null,
    moduleSet.has('crm') ? '/crm/contactos' : null,
    hasSchoolPortalAccess(normalizedRoles, modules)
      ? (isSchoolStaffRole(normalizedRoles, modules) ? '/escuela/clases' : '/mi-profesor')
      : null,
    canAccessLabelCatalog(normalizedRoles, modules) ? '/label/artistas' : null,
    canAccessLabelTracks(normalizedRoles, modules) ? '/label/tracks' : null,
    hasOperationsAccess(normalizedRoles, modules) ? '/operacion/inventario' : null,
    moduleSet.has('internships') ? '/practicas' : null,
    moduleSet.has('invoicing') ? '/finanzas/pagos' : null,
    hasRole('fan', 'customer') ? '/fans' : null,
  ];

  const firstAccessible = candidates.find(
    (path): path is string => path !== null && canAccessPath(path, normalizedRoles, modules),
  );
  if (firstAccessible) {
    return firstAccessible;
  }
  return '/inicio';
}

export function sanitizeRedirectPath(value: string | null | undefined): string | null {
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  if (trimmed === '' || !trimmed.startsWith('/') || trimmed.startsWith('//')) return null;

  try {
    const parsed = new URL(trimmed, URL_BASE);
    if (parsed.origin !== URL_BASE) return null;
    if (parsed.pathname === LOGIN_ROUTE) return null;
    return `${parsed.pathname}${parsed.search}${parsed.hash}`;
  } catch {
    return null;
  }
}

export function readSafeRedirectPath(search: string): string | null {
  const params = new URLSearchParams(search);
  return sanitizeRedirectPath(params.get('redirect'));
}

export function buildLoginRedirectPath(targetPath: string | null | undefined): string {
  const safeTarget = sanitizeRedirectPath(targetPath);
  if (!safeTarget) return LOGIN_ROUTE;

  const params = new URLSearchParams({ redirect: safeTarget });
  return `${LOGIN_ROUTE}?${params.toString()}`;
}
