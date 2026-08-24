import {
  buildAccessibleModuleSet,
  hasInternshipsAccess,
  canAccessLabelCatalog,
  canAccessLabelTracks,
  canAccessPath,
  hasStrictAdminAccess,
  hasSchoolPortalAccess,
  hasOperationsAccess,
  isSchoolStaffRole,
  normalizeAccessRoles,
} from './accessControl';

const LOGIN_ROUTE = '/login';
const URL_BASE = 'https://tdf.local';

export type OnboardingIntent =
  | 'events'
  | 'follow_artists'
  | 'artist_profile'
  | 'internships'
  | 'learning'
  | 'professional_tools';

const ONBOARDING_INTENTS = new Set<OnboardingIntent>([
  'events',
  'follow_artists',
  'artist_profile',
  'internships',
  'learning',
  'professional_tools',
]);

const LEGACY_ROLE_INTENTS: Record<string, OnboardingIntent> = {
  fan: 'follow_artists',
  artist: 'artist_profile',
  artista: 'artist_profile',
  intern: 'internships',
  practicante: 'internships',
  pasante: 'internships',
  teacher: 'learning',
  profesor: 'learning',
  student: 'learning',
  estudiante: 'learning',
  dj: 'professional_tools',
  producer: 'professional_tools',
  productor: 'professional_tools',
  promoter: 'professional_tools',
  promotor: 'professional_tools',
  publicist: 'professional_tools',
  photographer: 'professional_tools',
};

export function normalizeOnboardingIntent(value: string | null | undefined): OnboardingIntent | null {
  const normalized = value?.trim().toLowerCase() ?? '';
  if (ONBOARDING_INTENTS.has(normalized as OnboardingIntent)) return normalized as OnboardingIntent;
  return LEGACY_ROLE_INTENTS[normalized] ?? null;
}

export function readOnboardingIntent(search: string): OnboardingIntent | null {
  const params = new URLSearchParams(search);
  return normalizeOnboardingIntent(params.get('intent'))
    ?? normalizeOnboardingIntent(params.get('roles'));
}

const accessRequestPath = (feature: string, action: string) =>
  `/solicitudes-acceso/nueva?feature=${encodeURIComponent(feature)}&action=${encodeURIComponent(action)}`;

export function resolvePostAuthPath(
  intent: OnboardingIntent | null,
  roles: readonly string[],
  modules: readonly string[] = [],
  requestedRedirect?: string | null,
): string {
  const safeRedirect = sanitizeRedirectPath(requestedRedirect);
  const normalizedRoles = normalizeAccessRoles(roles);
  const hasArtistAccess = normalizedRoles.some((role) => ['artist', 'artista', 'admin'].includes(role));
  const redirectNeedsArtistAccess = intent === 'artist_profile'
    && Boolean(safeRedirect && (safeRedirect === '/mi-artista' || safeRedirect.startsWith('/artista/crear')));
  if (
    safeRedirect
    && canAccessPath(safeRedirect, roles, modules)
    && (!redirectNeedsArtistAccess || hasArtistAccess)
  ) return safeRedirect;

  switch (intent) {
    case 'artist_profile':
      return hasArtistAccess
        ? '/mi-artista'
        : accessRequestPath('artist.onboarding', 'create');
    case 'internships':
      return canAccessPath('/practicas', roles, modules)
        ? '/practicas'
        : accessRequestPath('internships', 'view');
    case 'follow_artists':
      return '/fans';
    case 'learning':
      return '/trials';
    case 'events':
      return '/inicio';
    case 'professional_tools':
      return pickLandingPath(roles, modules);
    default:
      return pickLandingPath(roles, modules);
  }
}

export function pickLandingPath(roles: readonly string[], modules?: readonly string[]): string {
  const normalizedRoles = normalizeAccessRoles(roles);
  const moduleSet = buildAccessibleModuleSet(normalizedRoles, modules);
  const hasRole = (...needles: string[]) =>
    needles.some((needle) => normalizedRoles.includes(needle));
  const candidates = [
    hasStrictAdminAccess(normalizedRoles, modules) ? '/configuracion/roles-permisos' : null,
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
    hasInternshipsAccess(normalizedRoles, modules) ? '/practicas' : null,
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
