const stripTrailingSlash = (value: string) => value.replace(/\/+$/, '');

const sanitizeBase = (raw?: string | null): string => {
  const fallback = 'http://localhost:5173';
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

const envPublicBase = (import.meta.env['VITE_PUBLIC_BASE'] as string | undefined) ?? undefined;
const inferredOrigin =
  typeof window !== 'undefined' && window.location.origin ? window.location.origin : undefined;

export const PUBLIC_BASE = sanitizeBase(envPublicBase ?? inferredOrigin);
export const COURSE_PATH_BASE = sanitizeBase(
  (import.meta.env['VITE_PUBLIC_COURSE_BASE'] as string | undefined) ?? `${PUBLIC_BASE}/curso`,
);

export const INVENTORY_SCAN_BASE = sanitizeBase(
  (import.meta.env['VITE_INVENTORY_SCAN_BASE'] as string | undefined) ??
    `${PUBLIC_BASE}/inventario/scan`,
);
export const buildInventoryScanUrl = (token: string) => `${INVENTORY_SCAN_BASE}/${token}`;

export const CARDANO_ADDRESS =
  ((import.meta.env['VITE_CARDANO_ADDRESS'] as string | undefined) ??
    'addr1qx2mdr6n8d0v2y5s99tmdluzvcq6lvpvez0mx55vvpfy6ee4fzjjxl454z8d2f5gd2yualhds75ycvsl3wuar908v0csqksrwy').trim();

const demoTokenEnv = (import.meta.env['VITE_API_DEMO_TOKEN'] as string | undefined)?.trim() ?? '';
const demoTokenHostsEnv = parseList(import.meta.env['VITE_DEMO_TOKEN_HOSTS'] as string | undefined);
const demoTokenHosts = demoTokenHostsEnv.length
  ? demoTokenHostsEnv
  : ['localhost', '127.0.0.1', 'tdf-app.pages.dev'];
const demoTokenValue =
  (import.meta.env['VITE_DEFAULT_DEMO_TOKEN'] as string | undefined)?.trim() ?? 'admin-token';

const normalizeHost = (host: string) => (host.split(':')[0] ?? host).toLowerCase();
export const inferDemoToken = (host?: string): string => {
  if (demoTokenEnv) return demoTokenEnv;
  if (!host) return '';
  const normalized = normalizeHost(host);
  return demoTokenHosts.map(normalizeHost).includes(normalized) ? demoTokenValue : '';
};

const defaultCourseSlug =
  (import.meta.env['VITE_COURSE_SLUG'] as string | undefined)?.trim() || 'produccion-musical-dic-2025';
const defaultMapUrl =
  (import.meta.env['VITE_COURSE_MAP_URL'] as string | undefined)?.trim() ||
  'https://maps.app.goo.gl/6pVYZ2CsbvQfGhAz6';
const defaultWhatsappUrl =
  (import.meta.env['VITE_COURSE_WHATSAPP_URL'] as string | undefined)?.trim() ||
  'https://wa.me/?text=INSCRIBIRME%20Curso%20Produccion%20Musical';
const defaultInstructorAvatar =
  (import.meta.env['VITE_COURSE_INSTRUCTOR_AVATAR'] as string | undefined)?.trim() ||
  `${PUBLIC_BASE}/assets/esteban-munoz.jpg`;

export const COURSE_DEFAULTS = {
  slug: defaultCourseSlug,
  mapUrl: defaultMapUrl,
  whatsappUrl: defaultWhatsappUrl,
  instructorAvatarUrl: defaultInstructorAvatar,
};

export const TRIALS_WHATSAPP_URL =
  (import.meta.env['VITE_TRIALS_WHATSAPP_URL'] as string | undefined)?.trim() ||
  'https://wa.me/593999001122?text=Hola%20quiero%20una%20clase%20de%20prueba%20en%20TDF%20Records';
