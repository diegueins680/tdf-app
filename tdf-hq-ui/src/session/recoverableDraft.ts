import { clearSessionPersonalData, readSessionPersonalData, writeSessionPersonalData } from '../utils/sessionPersonalData';

const retentionMs = 8 * 60 * 60 * 1000;
const maximumCharacters = 65_536;

function draftKey(ownerId: number, scope: string): string | null {
  if (!Number.isSafeInteger(ownerId) || ownerId <= 0 || scope.length === 0 || scope.length > 100) return null;
  const key = `tdf-recoverable-draft-v1:${ownerId}:${encodeURIComponent(scope)}`;
  return key.length <= 200 ? key : null;
}

// The stored value is form metadata, never a serialized File or a credential.
function allowedMetadata(value: unknown, depth = 0): boolean {
  if (depth > 6) return false;
  if (value === null || typeof value === 'boolean') return true;
  if (typeof value === 'string') return value.length <= maximumCharacters;
  if (typeof value === 'number') return Number.isFinite(value);
  if (Array.isArray(value)) return value.length <= 250 && value.every((entry: unknown) => allowedMetadata(entry, depth + 1));
  if (!value || typeof value !== 'object' || Object.getPrototypeOf(value) !== Object.prototype) return false;
  const entries = Object.entries(value);
  return entries.length <= 100 && entries.every(([key, entry]) =>
    !/^(?:__proto__|constructor|prototype|apiToken|accessToken|refreshToken|clientSecret|password|authorization|cookie)$/i.test(key)
    && (entry === undefined || allowedMetadata(entry, depth + 1)));
}

export function saveRecoverableDraft(ownerId: number, scope: string, value: unknown, now = Date.now()): boolean {
  const key = draftKey(ownerId, scope);
  if (!key || key.length > 200 || !allowedMetadata(value)) return false;
  const raw = JSON.stringify({ version: 1, ownerId, scope, expiresAt: now + retentionMs, value });
  if (raw.length > maximumCharacters) return false;
  return writeSessionPersonalData(key, raw);
}

export function readRecoverableDraft<T>(ownerId: number, scope: string, parse: (value: unknown) => T | undefined, now = Date.now()): T | undefined {
  const key = draftKey(ownerId, scope);
  if (!key) return undefined;
  try {
    const raw = readSessionPersonalData(key);
    if (!raw) return undefined;
    if (raw.length > maximumCharacters) throw new Error('Oversized draft');
    const data = JSON.parse(raw) as Record<string, unknown>;
    if (data?.['version'] !== 1 || data['ownerId'] !== ownerId || data['scope'] !== scope
      || typeof data['expiresAt'] !== 'number' || data['expiresAt'] <= now || data['expiresAt'] > now + retentionMs
      || !allowedMetadata(data['value'])) throw new Error('Invalid draft');
    const value = parse(data['value']);
    if (value === undefined) throw new Error('Invalid draft fields');
    return value;
  } catch {
    clearSessionPersonalData(key);
    return undefined;
  }
}

export function discardRecoverableDraft(ownerId: number, scope: string): void {
  const key = draftKey(ownerId, scope);
  if (key) clearSessionPersonalData(key);
}

/** Flat editor schema: text, booleans, numeric IDs and string collections. */
export function parseFormMetadata<T extends object>(value: unknown, defaults: T): T | undefined {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return undefined;
  const data = value as Record<string, unknown>;
  const fields = Object.entries(defaults);
  if (Object.keys(data).length !== fields.length) return undefined;
  for (const [key, expected] of fields) {
    if (!Object.prototype.hasOwnProperty.call(data, key)) return undefined;
    const actual = data[key];
    if (expected === null) {
      if (actual !== null && (typeof actual !== 'number' || !Number.isSafeInteger(actual) || actual <= 0)) return undefined;
    } else if (Array.isArray(expected)) {
      if (!Array.isArray(actual) || actual.some((entry: unknown) => typeof entry !== 'string')) return undefined;
    } else if (typeof actual !== typeof expected) return undefined;
  }
  return data as T;
}
