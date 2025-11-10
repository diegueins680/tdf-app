const API_BASE = import.meta.env.VITE_API_BASE || '';

async function api<T>(path: string, init: RequestInit = {}): Promise<T> {
  const res = await fetch(`${API_BASE}${path}`, {
    headers: { 'Content-Type': 'application/json', ...(init.headers || {}) },
    ...init,
  });
  if (!res.ok) {
    const text = await res.text().catch(() => '');
    throw new Error(text || `HTTP ${res.status}`);
  }
  if (res.status === 204) return undefined as T;
  return res.json() as Promise<T>;
}

export const get  = <T>(p: string) => api<T>(p, { method: 'GET' });
export const post = <T>(p: string, body: unknown) =>
  api<T>(p, { method: 'POST', body: JSON.stringify(body) });
export const put  = <T>(p: string, body: unknown) =>
  api<T>(p, { method: 'PUT', body: JSON.stringify(body) });
