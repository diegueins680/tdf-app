import type { SignupPayload } from '../api/auth';
import { SELF_SIGNUP_ROLES, type SignupRole } from '../constants/roles';

export function normalizeRolesInput<T extends string>(
  value: string | string[],
  allowedRoles: readonly T[],
): T[] {
  const entries = Array.isArray(value) ? value : value.split(',');
  const allowed = new Set<T>(allowedRoles as T[]);
  const unique: T[] = [];

  entries
    .map((entry) => entry.trim())
    .filter(Boolean)
    .forEach((role) => {
      if (allowed.has(role as T) && !unique.includes(role as T)) {
        unique.push(role as T);
      }
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

export function buildSignupPayload(
  form: SignupFormState,
  signupRoles: SignupRole[],
  favoriteArtistIds: number[],
  claimArtistId?: number | null,
): SignupPayload {
  const roles = normalizeSignupRoles(signupRoles);
  const wantsFanRole = roles.includes('Fan');
  const wantsInternRole = roles.includes('Intern');
  const normalizedClaimId = claimArtistId && claimArtistId > 0 ? claimArtistId : undefined;
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
    fanArtistIds: wantsFanRole && favoriteArtistIds.length ? favoriteArtistIds : undefined,
    claimArtistId: normalizedClaimId,
  };
}

export function deriveEffectiveRoles(
  apiRoles: string[] | undefined,
  selectedRoles: SignupRole[],
  defaultRole = 'fan',
): string[] {
  const apiNormalized = (apiRoles ?? []).map((role) => role.toLowerCase()).filter(Boolean);
  if (apiNormalized.length) return apiNormalized;

  const selectedNormalized = normalizeSignupRoles(selectedRoles).map((role) => role.toLowerCase());
  if (selectedNormalized.length) return selectedNormalized;

  return [defaultRole];
}
