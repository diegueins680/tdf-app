import { getStoredSessionToken } from '../session/SessionContext';

const API_BASE = import.meta.env.VITE_API_BASE ?? '';
export const API_BASE_URL = API_BASE;

function buildAuthHeader(): string | undefined {
  const token = getStoredSessionToken();
  if (!token) return undefined;
  return token.toLowerCase().startsWith('bearer ') ? token : `Bearer ${token}`;
}

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
  const message = [
    'No se pudo contactar la API. Verifica conexión, VITE_API_BASE y que el backend permita este origen.',
    hint ?? '',
  ]
    .filter(Boolean)
    .join(' ');
  const wrapped = new Error(message);
  (wrapped as Error & { cause?: unknown }).cause = err;
  return wrapped;
};

async function api<T>(path: string, init: RequestInit = {}): Promise<T> {
  const authHeader = buildAuthHeader();
  let res: Response;
  try {
    res = await fetch(`${API_BASE}${path}`, {
      headers: {
        'Content-Type': 'application/json',
        ...(authHeader ? { Authorization: authHeader } : {}),
        ...(init.headers ?? {}),
      },
      ...init,
    });
  } catch (err) {
    throw normalizeNetworkError(err);
  }

  if (!res.ok) {
    const contentType = res.headers.get('content-type') ?? '';
    let bodyText = '';
    if (contentType.includes('application/json')) {
      try {
        const json = (await res.json()) as Record<string, unknown>;
        const errorMsg =
          typeof json['message'] === 'string'
            ? json['message']
            : typeof json['error'] === 'string'
              ? json['error']
              : typeof json['detail'] === 'string'
                ? json['detail']
                : null;
        bodyText = errorMsg ?? JSON.stringify(json);
      } catch {
        bodyText = '';
      }
    } else {
      bodyText = await res.text().catch(() => '');
    }
    const trimmed = bodyText.trim();
    const statusText = res.statusText.trim();
    const statusLabel = statusText !== '' ? statusText : `HTTP ${res.status}`;
    const details = trimmed !== '' ? trimmed : statusLabel;
    throw new Error(details);
  }

  if (res.status === 204) return undefined as T;
  return res.json() as Promise<T>;
}

export const get = <T>(p: string) => api<T>(p, { method: 'GET' });
export const post = <T>(p: string, body: unknown) =>
  api<T>(p, { method: 'POST', body: JSON.stringify(body) });
export const put = <T>(p: string, body: unknown) =>
  api<T>(p, { method: 'PUT', body: JSON.stringify(body) });
export const patch = <T>(p: string, body: unknown) =>
  api<T>(p, { method: 'PATCH', body: JSON.stringify(body) });
export const del = <T>(p: string) =>
  api<T>(p, { method: 'DELETE' });
