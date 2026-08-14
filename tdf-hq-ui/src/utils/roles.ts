import type { SignupPayload } from '../api/auth';

export function normalizeRolesInput<T extends string>(
  value: string | string[],
  allowedRoles: readonly T[],
): T[] {
  const entries = (Array.isArray(value) ? value : [value]).flatMap((entry) => entry.split(','));
  const allowedByLower = new Map<string, T>();
  allowedRoles.forEach((role) => {
    allowedByLower.set(role.toLowerCase(), role);
  });
  const unique: T[] = [];

  entries
    .map((entry) => entry.trim())
    .filter(Boolean)
    .forEach((role) => {
      const canonicalRole = allowedByLower.get(role.toLowerCase());
      if (!canonicalRole || unique.includes(canonicalRole)) return;
      unique.push(canonicalRole);
    });

  return unique;
}

export interface SignupFormState {
  firstName: string;
  lastName: string;
  email: string;
  phone: string;
  password: string;
}

const parsePositiveSafeInt = (value: unknown): number | undefined => {
  if (typeof value === 'number') {
    if (!Number.isSafeInteger(value) || value <= 0) return undefined;
    return value;
  }
  if (typeof value === 'string') {
    const trimmed = value.trim();
    if (!/^\d+$/.test(trimmed)) return undefined;
    const parsed = Number.parseInt(trimmed, 10);
    return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : undefined;
  }
  return undefined;
};

const normalizePositiveSafeIntList = (values: readonly number[]): number[] => {
  const seen = new Set<number>();
  const unique: number[] = [];
  values.forEach((value) => {
    const normalized = parsePositiveSafeInt(value);
    if (normalized === undefined || seen.has(normalized)) return;
    seen.add(normalized);
    unique.push(normalized);
  });
  return unique;
};

export function buildSignupPayload(
  form: SignupFormState,
  favoriteArtistIds: number[],
  claimArtistId?: number | null,
): SignupPayload {
  const normalizedClaimId = parsePositiveSafeInt(claimArtistId);
  const normalizedFavoriteArtistIds = normalizePositiveSafeIntList(favoriteArtistIds);

  return {
    firstName: form.firstName.trim(),
    lastName: form.lastName.trim(),
    email: form.email.trim(),
    phone: form.phone.trim() || undefined,
    password: form.password,
    fanArtistIds: normalizedFavoriteArtistIds.length ? normalizedFavoriteArtistIds : undefined,
    claimArtistId: normalizedClaimId,
  };
}

const normalizeRoleTokens = (roles: readonly string[]): string[] => {
  const seen = new Set<string>();
  const normalized: string[] = [];
  roles.forEach((role) => {
    const clean = role.trim().toLowerCase();
    if (!clean || seen.has(clean)) return;
    seen.add(clean);
    normalized.push(clean);
  });
  return normalized;
};

export function deriveEffectiveRoles(
  apiRoles: string[] | undefined,
): string[] {
  return normalizeRoleTokens(apiRoles ?? []);
}
