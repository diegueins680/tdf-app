import { env } from '../utils/env';

export const DEFAULT_DEPLOYED_API_BASE = 'https://tdf-hq.fly.dev';
const LEGACY_KOYEB_API_BASE = 'https://the-dream-factory.koyeb.app';

const stripTrailingSlash = (value: string): string => value.replace(/\/+$/, '');

const isTdfPagesHost = (hostname: string): boolean => {
  const normalized = hostname.trim().toLowerCase();
  return normalized === 'tdf-app.pages.dev' || normalized.endsWith('.tdf-app.pages.dev');
};

export const resolveApiBase = (
  options: { envValue?: string | null; hostname?: string | null } = {},
): string => {
  const configured = options.envValue ?? env.read('VITE_API_BASE');
  const hostname =
    options.hostname ??
    (typeof window !== 'undefined' ? window.location.hostname : '');
  if (typeof configured === 'string' && configured.trim() !== '') {
    const normalizedConfigured = stripTrailingSlash(configured.trim());
    if (!(hostname && isTdfPagesHost(hostname) && normalizedConfigured === LEGACY_KOYEB_API_BASE)) {
      return normalizedConfigured;
    }
  }

  return hostname && isTdfPagesHost(hostname) ? DEFAULT_DEPLOYED_API_BASE : '';
};
