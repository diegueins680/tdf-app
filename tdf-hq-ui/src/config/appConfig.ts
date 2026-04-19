const stripTrailingSlash = (value: string) => value.replace(/\/+$/, '');

const sanitizeBase = (raw?: string | null): string => {
  const fallback = 'https://tdf-app.pages.dev';
  if (!raw) return fallback;
  const trimmed = stripTrailingSlash(raw.trim());
  return trimmed.length ? trimmed : fallback;
};

const parseList = (raw?: string): string[] =>
  raw
    ? raw
        .split(',')
        .map((item) => item.trim())
        .filter(Boolean)
    : [];

const env: Record<string, unknown> =
  (import.meta as unknown as { env?: Record<string, unknown> }).env ?? {};
const envString = (key: string): string | undefined => {
  const value = env[key];
  return typeof value === 'string' ? value : undefined;
};
const envTrimmedOrUndefined = (raw?: string): string | undefined => {
  const trimmed = raw?.trim();
  if (!trimmed) return undefined;
  return trimmed;
};

const envPublicBase = envString('VITE_PUBLIC_BASE');
const inferredOrigin =
  typeof window !== 'undefined' && window.location.origin ? window.location.origin : undefined;

export const PUBLIC_BASE = sanitizeBase(envPublicBase ?? inferredOrigin);
export const COURSE_PATH_BASE = sanitizeBase(
  envString('VITE_PUBLIC_COURSE_BASE') ?? `${PUBLIC_BASE}/curso`,
);

export const INVENTORY_SCAN_BASE = sanitizeBase(
  envString('VITE_INVENTORY_SCAN_BASE') ?? `${PUBLIC_BASE}/inventario/scan`,
);
export const buildInventoryScanUrl = (token: string) => `${INVENTORY_SCAN_BASE}/${token}`;

export const CARDANO_ADDRESS =
  (envString('VITE_CARDANO_ADDRESS') ??
    'addr1qx2mdr6n8d0v2y5s99tmdluzvcq6lvpvez0mx55vvpfy6ee4fzjjxl454z8d2f5gd2yualhds75ycvsl3wuar908v0csqksrwy').trim();

const demoTokenEnv = envString('VITE_API_DEMO_TOKEN')?.trim() ?? '';
const demoTokenHostsEnv = parseList(envString('VITE_DEMO_TOKEN_HOSTS'));
const demoTokenHosts = demoTokenHostsEnv.length
  ? demoTokenHostsEnv
  : ['localhost', '127.0.0.1', '::1'];
const demoTokenValue =
  envTrimmedOrUndefined(envString('VITE_DEFAULT_DEMO_TOKEN')) ?? '';

const URL_SCHEME_PATTERN = /^[a-z][a-z\d+\-.]*:\/\//i;

const stripIpv6Brackets = (host: string): string => {
  if (host.startsWith('[') && host.endsWith(']')) return host.slice(1, -1);
  return host;
};

const extractHostname = (value: string): string | null => {
  const trimmed = value.trim();
  if (!trimmed) return null;
  const normalizedUrl = URL_SCHEME_PATTERN.test(trimmed) ? trimmed : `http://${trimmed}`;
  try {
    return new URL(normalizedUrl).hostname;
  } catch {
    return null;
  }
};

const normalizeHost = (host: string): string => {
  const trimmed = host.trim();
  if (!trimmed) return '';
  const parsedHost = extractHostname(trimmed);
  return stripIpv6Brackets(parsedHost ?? trimmed).toLowerCase();
};

export const inferDemoToken = (host?: string): string => {
  if (demoTokenEnv) return demoTokenEnv;
  if (!host || !demoTokenValue) return '';
  const normalized = normalizeHost(host);
  return demoTokenHosts.map(normalizeHost).includes(normalized) ? demoTokenValue : '';
};

const defaultCourseSlug =
  envTrimmedOrUndefined(envString('VITE_COURSE_SLUG')) ?? 'produccion-musical-abr-2026';
const defaultCourseCohorts = parseList(envString('VITE_COURSE_COHORTS'));
const defaultMapUrl =
  envTrimmedOrUndefined(envString('VITE_COURSE_MAP_URL')) ??
  'https://maps.app.goo.gl/6pVYZ2CsbvQfGhAz6';
const defaultWhatsappUrl =
  envTrimmedOrUndefined(envString('VITE_COURSE_WHATSAPP_URL')) ??
  'https://wa.me/?text=INSCRIBIRME%20Curso%20Produccion%20Musical';
const defaultInstructorAvatar =
  envTrimmedOrUndefined(envString('VITE_COURSE_INSTRUCTOR_AVATAR')) ??
  `${PUBLIC_BASE}/assets/tdf-ui/esteban-munoz.jpg`;

export const COURSE_DEFAULTS = {
  slug: defaultCourseSlug,
  mapUrl: defaultMapUrl,
  whatsappUrl: defaultWhatsappUrl,
  instructorAvatarUrl: defaultInstructorAvatar,
};
export const COURSE_COHORTS = defaultCourseCohorts.length ? defaultCourseCohorts : [defaultCourseSlug];

export const TRIALS_WHATSAPP_URL =
  envTrimmedOrUndefined(envString('VITE_TRIALS_WHATSAPP_URL')) ??
  'https://wa.me/593999001122?text=Hola%20quiero%20una%20clase%20de%20prueba%20en%20TDF%20Records';

export const STUDIO_MAP_URL =
  envTrimmedOrUndefined(envString('VITE_STUDIO_MAP_URL')) ?? COURSE_DEFAULTS.mapUrl;

export const STUDIO_WHATSAPP_URL =
  envTrimmedOrUndefined(envString('VITE_STUDIO_WHATSAPP_URL')) ??
  'https://wa.me/593999001122?text=Hola%20quiero%20reservar%20un%20servicio%20en%20TDF%20Records';
