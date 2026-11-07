import { env } from '../utils/env';

const legacyKey = 'tdf-google-drive-token';

// Activate only after the encrypted backend and its recovery procedure are ready.
export const isDriveVaultRequired = (): boolean => env.read('VITE_DRIVE_VAULT_REQUIRED') === 'true';

export const isDriveConnectionHandle = (value: unknown): value is string =>
  typeof value === 'string' && /^tdf-vault-v1_[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/.test(value);

const isLegacyProviderToken = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0 && value.length <= 16384
  && /^[\x21-\x7e]+$/.test(value) && !/^tdf-vault/i.test(value);

/** Preserve the old proxy contract during staging; malformed vault identities never downgrade. */
export function isAcceptedDriveCredentialPair(access: unknown, refresh: unknown): access is string {
  if (isDriveConnectionHandle(access)) return refresh === undefined || refresh === access;
  return !isDriveVaultRequired() && isLegacyProviderToken(access)
    && (refresh === undefined || isLegacyProviderToken(refresh));
}

/** Retire unbound/invalid entries; strict vault mode also retires all provider secrets. */
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
              && key === `${legacyKey}:${descriptor['ownerPartyId']}`
              && typeof descriptor['expiresAt'] === 'number' && Number.isFinite(descriptor['expiresAt'])
              && (descriptor['ownerEpoch'] === undefined || (typeof descriptor['ownerEpoch'] === 'number' && Number.isSafeInteger(descriptor['ownerEpoch'])))
              && isAcceptedDriveCredentialPair(descriptor['accessToken'], descriptor['refreshToken'])
              && Object.keys(descriptor).every((field) => ['ownerPartyId', 'ownerEpoch', 'accessToken', 'refreshToken', 'expiresAt'].includes(field));
          }
        } catch { /* An invalid legacy entry must be retired too. */ }
      }
      if (!safe) storage.removeItem(key);
    }
  } catch { /* Unavailable storage is never read as a credential fallback. */ }
}
