import type { SignupPayload } from '../api/auth';
import { SELF_SIGNUP_ROLES, type SignupRole } from '../constants/roles';

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

export function normalizeSignupRoles(value: string | string[]): SignupRole[] {
  return normalizeRolesInput(value, SELF_SIGNUP_ROLES);
}

export interface SignupFormState {
  firstName: string;
  lastName: string;
  email: string;
  phone: string;
  password: string;
  internshipStartAt: string;
  internshipEndAt: string;
  internshipRequiredHours: string;
  internshipSkills: string;
  internshipAreas: string;
}

const parseOptionalInt = (value: string): number | undefined => {
  const trimmed = value.trim();
  if (trimmed === '') return undefined;
  if (!/^\d+$/.test(trimmed)) return undefined;
  const parsed = Number.parseInt(trimmed, 10);
  if (parsed < 0) return undefined;
  return Number.isNaN(parsed) ? undefined : parsed;
};

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
  signupRoles: SignupRole[],
  favoriteArtistIds: number[],
  claimArtistId?: number | null,
): SignupPayload {
  const roles = normalizeSignupRoles(signupRoles);
  const wantsFanRole = roles.includes('Fan');
  const wantsInternRole = roles.includes('Intern');
  const normalizedClaimId = parsePositiveSafeInt(claimArtistId);
  const normalizedFavoriteArtistIds = wantsFanRole
    ? normalizePositiveSafeIntList(favoriteArtistIds)
    : [];
  const requiredHours = wantsInternRole ? parseOptionalInt(form.internshipRequiredHours) : undefined;

  return {
    firstName: form.firstName.trim(),
    lastName: form.lastName.trim(),
    email: form.email.trim(),
    phone: form.phone.trim() || undefined,
    password: form.password,
    internshipStartAt: wantsInternRole && form.internshipStartAt.trim() ? form.internshipStartAt.trim() : undefined,
    internshipEndAt: wantsInternRole && form.internshipEndAt.trim() ? form.internshipEndAt.trim() : undefined,
    internshipRequiredHours: requiredHours,
    internshipSkills: wantsInternRole && form.internshipSkills.trim() ? form.internshipSkills.trim() : undefined,
    internshipAreas: wantsInternRole && form.internshipAreas.trim() ? form.internshipAreas.trim() : undefined,
    roles: roles.length ? roles : undefined,
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
  selectedRoles: SignupRole[],
  defaultRole = 'fan',
): string[] {
  const apiNormalized = normalizeRoleTokens(apiRoles ?? []);
  if (apiNormalized.length) return apiNormalized;

  const selectedNormalized = normalizeRoleTokens(normalizeSignupRoles(selectedRoles));
  if (selectedNormalized.length) return selectedNormalized;

  const fallbackRole = defaultRole.trim().toLowerCase();
  return [fallbackRole || 'fan'];
}
