import { buildAuthorizationHeader } from './authHeader';
import { extractErrorDetails } from './errorMessage';
import { env } from '../utils/env';

const API_BASE = env.read('VITE_API_BASE') ?? '';
export const API_BASE_URL = API_BASE;
const ABSOLUTE_URL_PATTERN = /^[a-zA-Z][a-zA-Z\d+\-.]*:\/\//;

const joinRequestUrl = (base: string, path: string): string => {
  const normalizedBase = base.trim();
  const normalizedPath = path.trim();
  if (!normalizedPath) return normalizedBase;
  if (!normalizedBase || ABSOLUTE_URL_PATTERN.test(normalizedPath)) return normalizedPath;
  const baseHasSlash = normalizedBase.endsWith('/');
  const pathHasSlash = normalizedPath.startsWith('/');
  const pathIsQueryOrHash = normalizedPath.startsWith('?') || normalizedPath.startsWith('#');
  if (!baseHasSlash && !pathHasSlash && !pathIsQueryOrHash) return `${normalizedBase}/${normalizedPath}`;
  if (baseHasSlash && pathHasSlash) return `${normalizedBase}${normalizedPath.slice(1)}`;
  return `${normalizedBase}${normalizedPath}`;
};

const isFormData = (value: unknown): value is FormData =>
  typeof FormData !== 'undefined' && value instanceof FormData;

const buildRequestHeaders = (initHeaders: HeadersInit | undefined, authHeader: string | undefined): Headers => {
  const headers = new Headers();
  if (authHeader) headers.set('Authorization', authHeader);
  if (initHeaders) {
    const extraHeaders = new Headers(initHeaders);
    extraHeaders.forEach((value, key) => headers.set(key, value));
  }
  return headers;
};

const shouldSetJsonContentType = (headers: Headers, body: BodyInit | null | undefined): boolean => {
  if (headers.has('Content-Type')) return false;
  if (body === undefined || body === null) return false;
  if (isFormData(body)) return false;
  return true;
};

const isJsonContentType = (contentType: string): boolean => /[/+]json(?:;|$)/i.test(contentType);

const looksLikeJsonPayload = (value: string): boolean => {
  const trimmed = value.trim();
  return trimmed.startsWith('{') || trimmed.startsWith('[');
};

const guessCrossOriginHint = () => {
  if (typeof window === 'undefined') return null;
  if (!API_BASE_URL) return null;
  try {
    const apiUrl = new URL(API_BASE_URL, window.location.origin);
    const appOrigin = window.location.origin;
    if (apiUrl.origin !== appOrigin) {
      return `Origen app: ${appOrigin} · API: ${apiUrl.origin}. Revisa CORS y VITE_API_BASE.`;
    }
  } catch {
    return null;
  }
  return null;
};

const normalizeNetworkError = (err: unknown) => {
  const hint = guessCrossOriginHint();
  if (hint) {
    console.warn('API network hint:', hint);
  }
  const message = 'No se pudo conectar con el servicio. Revisa tu conexión e inténtalo de nuevo.';
  const wrapped = new Error(message);
  (wrapped as Error & { cause?: unknown }).cause = err;
  return wrapped;
};

async function api<T>(path: string, init: RequestInit = {}): Promise<T> {
  const authHeader = buildAuthorizationHeader();
  const headers = buildRequestHeaders(init.headers, authHeader);
  if (shouldSetJsonContentType(headers, init.body)) {
    headers.set('Content-Type', 'application/json');
  }

  let res: Response;
  try {
    res = await fetch(joinRequestUrl(API_BASE, path), {
      ...init,
      credentials: 'include',
      headers,
    });
  } catch (err) {
    throw normalizeNetworkError(err);
  }

  if (!res.ok) {
    const contentType = res.headers.get('content-type') ?? '';
    const bodyText = await res.text().catch(() => '');
    const trimmed = extractErrorDetails(bodyText, contentType);
    const statusText = res.statusText.trim();
    const statusLabel = statusText !== '' ? statusText : `HTTP ${res.status}`;
    const details = trimmed !== '' ? trimmed : statusLabel;
    throw new Error(details);
  }

  if (res.status === 204 || res.status === 205) return undefined as T;
  const raw = await res.text().catch(() => '');
  const trimmedRaw = raw.trim();
  if (trimmedRaw === '') return undefined as T;
  const contentType = res.headers.get('content-type') ?? '';
  if (!isJsonContentType(contentType) && !looksLikeJsonPayload(trimmedRaw)) {
    return raw as unknown as T;
  }
  try {
    return JSON.parse(raw) as T;
  } catch {
    return raw as unknown as T;
  }
}

export const get = <T>(p: string) => api<T>(p, { method: 'GET' });
export const post = <T>(p: string, body: unknown) =>
  api<T>(p, { method: 'POST', body: JSON.stringify(body) });
export const postForm = <T>(p: string, form: FormData) =>
  api<T>(p, { method: 'POST', body: form });
export const put = <T>(p: string, body: unknown) =>
  api<T>(p, { method: 'PUT', body: JSON.stringify(body) });
export const patch = <T>(p: string, body: unknown) =>
  api<T>(p, { method: 'PATCH', body: JSON.stringify(body) });
export const del = <T>(p: string) =>
  api<T>(p, { method: 'DELETE' });
