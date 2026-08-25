export const INSTAGRAM_TRAFFIC_STORAGE_KEY = 'tdf-instagram-traffic';

const INSTAGRAM_PARAM_KEYS = [
  'utm_source',
  'utm_campaign',
  'utm_content',
  'source',
  'ref',
  'referrer',
  'from',
] as const;

export const isInstagramSourceValue = (value?: string | null): boolean => {
  const normalized = value?.trim().toLowerCase();
  if (!normalized) return false;
  if (normalized === 'ig' || normalized === 'insta' || normalized === 'instagram') return true;
  if (normalized.includes('instagram')) return true;
  return /(^|[._-])ig($|[._-])/.test(normalized);
};

export const searchHasInstagramSource = (search: string): boolean => {
  const params = new URLSearchParams(search.startsWith('?') ? search.slice(1) : search);
  return INSTAGRAM_PARAM_KEYS.some((key) => isInstagramSourceValue(params.get(key)));
};

export const referrerIsInstagram = (referrer?: string | null): boolean => {
  const trimmed = referrer?.trim();
  if (!trimmed) return false;
  try {
    const hostname = new URL(trimmed).hostname.toLowerCase();
    return hostname === 'instagram.com' || hostname.endsWith('.instagram.com');
  } catch {
    return isInstagramSourceValue(trimmed);
  }
};

export const hasInstagramTrafficSignal = ({
  search,
  referrer,
}: {
  search: string;
  referrer?: string | null;
}): boolean => searchHasInstagramSource(search) || referrerIsInstagram(referrer);

const getSessionStorage = (): Storage | null => {
  if (typeof window === 'undefined') return null;
  try {
    return window.sessionStorage;
  } catch {
    return null;
  }
};

export const readStoredInstagramTraffic = (storage = getSessionStorage()): boolean => {
  try {
    return storage?.getItem(INSTAGRAM_TRAFFIC_STORAGE_KEY) === '1';
  } catch {
    return false;
  }
};

export const rememberInstagramTraffic = (storage = getSessionStorage()): void => {
  try {
    storage?.setItem(INSTAGRAM_TRAFFIC_STORAGE_KEY, '1');
  } catch {
    // Ignore private browsing and storage permission failures.
  }
};
