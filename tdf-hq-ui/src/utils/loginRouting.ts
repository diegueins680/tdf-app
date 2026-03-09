import { buildAccessibleModuleSet, normalizeTokens } from './accessControl';

const LOGIN_ROUTE = '/login';
const URL_BASE = 'https://tdf.local';

export function pickLandingPath(roles: readonly string[], modules?: readonly string[]): string {
  const normalizedRoles = normalizeTokens(roles, { lowerCase: true });
  const moduleSet = buildAccessibleModuleSet(normalizedRoles, modules);
  const hasRole = (...needles: string[]) =>
    needles.some((needle) => normalizedRoles.includes(needle));
  const hasModule = (needle: string) => moduleSet.has(needle);

  if (hasRole('admin') || hasModule('admin')) return '/configuracion/roles-permisos';
  if (hasRole('artist', 'artista')) return '/mi-artista';
  if (hasRole('teacher')) return '/mi-profesor';
  if (hasModule('scheduling')) return '/estudio/calendario';
  if (hasModule('crm')) return '/crm/contactos';
  if (hasModule('school')) return '/escuela/clases';
  if (hasModule('label')) return '/label/artistas';
  if (hasModule('ops')) return '/operacion/inventario';
  if (hasModule('internships')) return '/practicas';
  if (hasModule('invoicing')) return '/finanzas/pagos';
  if (hasRole('fan', 'customer')) return '/fans';
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
