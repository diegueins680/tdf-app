/**
 * Keeps short-lived personal-data drafts in the current browser tab only.
 * Any value left by the former persistent implementation is discarded rather
 * than restored, so a later user of a shared browser cannot inherit it.
 */
export const readSessionPersonalData = (key: string): string | null => {
  if (typeof window === 'undefined') return null;

  let sessionValue: string | null = null;
  try {
    sessionValue = window.sessionStorage.getItem(key);
  } catch {
    // Continue so a legacy persistent value can still be removed.
  }
  try {
    window.localStorage.removeItem(key);
  } catch {
    // Storage can be unavailable in hardened/private browser contexts.
  }
  return sessionValue;
};

export const writeSessionPersonalData = (key: string, value: string): boolean => {
  if (typeof window === 'undefined') return false;
  let saved = false;
  try {
    window.sessionStorage.setItem(key, value);
    saved = true;
  } catch {
    // The form remains usable without browser storage.
  }
  try {
    window.localStorage.removeItem(key);
  } catch {
    // Best-effort cleanup for restricted browser contexts.
  }
  return saved;
};

export const clearSessionPersonalData = (key: string): void => {
  if (typeof window === 'undefined') return;
  try {
    window.sessionStorage.removeItem(key);
  } catch {
    // Best-effort cleanup for restricted browser contexts.
  }
  try {
    window.localStorage.removeItem(key);
  } catch {
    // Best-effort cleanup for restricted browser contexts.
  }
};
