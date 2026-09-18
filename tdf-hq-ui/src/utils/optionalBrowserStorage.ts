// Best-effort browser caches. A missing value must follow the caller's explicit
// unavailable-data path; this helper never supplies credentials or authority.
export function readOptionalBrowserStorage(scope: 'local' | 'session', key: string): string | null {
  try {
    const storage = scope === 'local' ? window.localStorage : window.sessionStorage;
    return storage.getItem(key);
  } catch {
    return null;
  }
}

export function writeOptionalBrowserPreference(key: string, value: string): void {
  try { window.localStorage.setItem(key, value); }
  catch { /* The active form retains its in-memory preference. */ }
}
