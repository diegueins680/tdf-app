import { getStoredSessionToken } from '../session/SessionContext';

const API_BASE = import.meta.env.VITE_API_BASE ?? '';

function buildAuthHeader(): string | undefined {
  const token = getStoredSessionToken();
  if (!token) return undefined;
  return token.toLowerCase().startsWith('bearer ') ? token : `Bearer ${token}`;
}

async function api<T>(path: string, init: RequestInit = {}): Promise<T> {
  const authHeader = buildAuthHeader();
  const res = await fetch(`${API_BASE}${path}`, {
    headers: {
      'Content-Type': 'application/json',
      ...(authHeader ? { Authorization: authHeader } : {}),
      ...(init.headers ?? {}),
    },
    ...init,
  });
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    const trimmed = text.trim();
    const statusText = res.statusText.trim();
    const details = trimmed !== '' ? trimmed : statusText !== '' ? statusText : `HTTP ${res.status}`;
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
