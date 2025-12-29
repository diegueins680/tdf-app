import type { Role } from '../api/generated/client';

export const EXTENDED_MUSIC_ROLES = [
  'Fan',
  'Artista',
  'Promotor',
  'Promoter',
  'Manager',
  'A&R',
  'Producer',
  'Songwriter',
  'DJ',
  'Publicist',
  'TourManager',
  'LabelRep',
  'StageManager',
  'RoadCrew',
  'Photographer',
] as const;

export type ExtendedMusicRole = (typeof EXTENDED_MUSIC_ROLES)[number];
export type SignupRole = Role | ExtendedMusicRole;

export const CORE_ROLES: Role[] = [
  'Admin',
  'Manager',
  'Intern',
  'Engineer',
  'Teacher',
  'Reception',
  'Accounting',
  'Webmaster',
  'Artist',
  'Student',
  'ReadOnly',
];

// Keep a stable, alphabetized list so todos los roles (incluido Webmaster) se muestren explícitamente en selects.
export const ALL_ROLES: SignupRole[] = Array.from(
  new Set<SignupRole>([...CORE_ROLES, ...EXTENDED_MUSIC_ROLES]),
).sort((a, b) => a.localeCompare(b));

const SENSITIVE_ROLES: SignupRole[] = ['Admin', 'Accounting'];

const SIGNUP_SAFE_CORE_ROLES: SignupRole[] = CORE_ROLES.filter(
  (role) => !SENSITIVE_ROLES.includes(role),
);

export const SELF_SIGNUP_ROLES: SignupRole[] = Array.from(
  new Set<SignupRole>([...SIGNUP_SAFE_CORE_ROLES, ...EXTENDED_MUSIC_ROLES]),
);
