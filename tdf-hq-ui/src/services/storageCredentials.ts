const legacyKey = 'tdf-google-drive-token';

export const isDriveConnectionHandle = (value: unknown): value is string =>
  typeof value === 'string' && /^tdf-vault-v1_[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/.test(value);

/** Retire old provider secrets for every account on a shared browser. */
export function retireLegacyDriveCredentials(): void {
  if (typeof window === 'undefined') return;
  try {
    const storage = window.localStorage;
    const keys = Array.from({ length: storage.length }, (_, index) => storage.key(index));
    for (const key of keys) {
      if (!key || (key !== legacyKey && !key.startsWith(`${legacyKey}:`))) continue;
      let safe = false;
      if (key !== legacyKey) {
        try {
          const value = JSON.parse(storage.getItem(key) ?? 'null') as unknown;
          if (value && typeof value === 'object') {
            const descriptor = value as Record<string, unknown>;
            safe = typeof descriptor['ownerPartyId'] === 'number' && Number.isSafeInteger(descriptor['ownerPartyId'])
              && Number(descriptor['ownerPartyId']) > 0
              && typeof descriptor['expiresAt'] === 'number' && Number.isFinite(descriptor['expiresAt'])
              && (descriptor['ownerEpoch'] === undefined || (typeof descriptor['ownerEpoch'] === 'number' && Number.isSafeInteger(descriptor['ownerEpoch'])))
              && isDriveConnectionHandle(descriptor['accessToken'])
              && (descriptor['refreshToken'] === undefined || descriptor['refreshToken'] === descriptor['accessToken'])
              && Object.keys(descriptor).every((field) => ['ownerPartyId', 'ownerEpoch', 'accessToken', 'refreshToken', 'expiresAt'].includes(field));
          }
        } catch { /* An invalid legacy entry must be retired too. */ }
      }
      if (!safe) storage.removeItem(key);
    }
  } catch { /* Unavailable storage is never read as a credential fallback. */ }
}
