/**
 * Keeps short-lived personal-data drafts in the current browser tab only.
 * Any value left by the former persistent implementation is discarded rather
 * than restored, so a later user of a shared browser cannot inherit it.
 */
const SESSION_PERSONAL_DATA_REGISTRY_KEY = 'tdf-session-personal-data-keys-v1';

const readRegisteredKeys = (): string[] => {
  try {
    const raw = window.sessionStorage.getItem(SESSION_PERSONAL_DATA_REGISTRY_KEY);
    if (!raw) return [];
    const parsed = JSON.parse(raw) as unknown;
    return Array.isArray(parsed)
      ? parsed.filter((key): key is string => typeof key === 'string' && key.length > 0 && key.length <= 200)
      : [];
  } catch {
    return [];
  }
};

const writeRegisteredKeys = (keys: readonly string[]): void => {
  try {
    if (keys.length === 0) {
      window.sessionStorage.removeItem(SESSION_PERSONAL_DATA_REGISTRY_KEY);
      return;
    }
    window.sessionStorage.setItem(SESSION_PERSONAL_DATA_REGISTRY_KEY, JSON.stringify(Array.from(new Set(keys))));
  } catch {
    // The form remains usable without browser storage.
  }
};

const registerKey = (key: string): void => {
  writeRegisteredKeys([...readRegisteredKeys(), key]);
};

const unregisterKey = (key: string): void => {
  writeRegisteredKeys(readRegisteredKeys().filter((registeredKey) => registeredKey !== key));
};

const removePersonalDataKey = (key: string): void => {
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
  if (sessionValue !== null) registerKey(key);
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
  if (saved) registerKey(key);
  return saved;
};

export const clearSessionPersonalData = (key: string): void => {
  if (typeof window === 'undefined') return;
  removePersonalDataKey(key);
  unregisterKey(key);
};

export const clearAllSessionPersonalData = (): void => {
  if (typeof window === 'undefined') return;
  // Include keys written before the registry existed so an upgrade cannot
  // leave an older contact draft behind at an identity boundary.
  removePersonalDataKey('tdf-marketplace-buyer');
  removePersonalDataKey('tdf-public-booking-profile');
  readRegisteredKeys().forEach(removePersonalDataKey);
  try {
    window.sessionStorage.removeItem(SESSION_PERSONAL_DATA_REGISTRY_KEY);
  } catch {
    // Best-effort cleanup for restricted browser contexts.
  }
  try {
    window.localStorage.removeItem(SESSION_PERSONAL_DATA_REGISTRY_KEY);
  } catch {
    // Best-effort cleanup for restricted browser contexts.
  }
};

export const reconcileSessionPersonalData = (
  previousPartyId: number | null | undefined,
  nextPartyId: number | null | undefined,
): boolean => {
  if (previousPartyId == null || previousPartyId === nextPartyId) return false;
  clearAllSessionPersonalData();
  return true;
};
