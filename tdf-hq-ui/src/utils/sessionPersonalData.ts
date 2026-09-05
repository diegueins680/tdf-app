/**
 * Keeps short-lived personal-data drafts in the current browser tab only.
 * Any value left by the former persistent implementation is migrated once and
 * removed from localStorage so shared-device sessions do not retain it.
 */
export const readSessionPersonalData = (key: string): string | null => {
  if (typeof window === 'undefined') return null;

  let sessionValue: string | null = null;
  let legacyValue: string | null = null;
  try {
    sessionValue = window.sessionStorage.getItem(key);
  } catch {
    // Continue so a legacy persistent value can still be removed.
  }
  try {
    legacyValue = window.localStorage.getItem(key);
    window.localStorage.removeItem(key);
  } catch {
    // Storage can be unavailable in hardened/private browser contexts.
  }

  if (sessionValue != null) return sessionValue;
  if (legacyValue == null) return null;
  try {
    window.sessionStorage.setItem(key, legacyValue);
  } catch {
    // The caller can still use the in-memory value for the current render.
  }
  return legacyValue;
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
