import type { PartyRole } from '../api/generated/client';

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
export type SignupRole = PartyRole | ExtendedMusicRole;

export const CORE_ROLES: PartyRole[] = [
  'Admin',
  'Manager',
  'Engineer',
  'Teacher',
  'Reception',
  'Accounting',
  'Artist',
  'Student',
  'ReadOnly',
];

export const ALL_ROLES: SignupRole[] = Array.from(
  new Set<SignupRole>([...CORE_ROLES, ...EXTENDED_MUSIC_ROLES]),
);

const SENSITIVE_ROLES: SignupRole[] = ['Admin', 'Accounting'];

const SIGNUP_SAFE_CORE_ROLES: SignupRole[] = CORE_ROLES.filter(
  (role) => !SENSITIVE_ROLES.includes(role),
);

export const SELF_SIGNUP_ROLES: SignupRole[] = Array.from(
  new Set<SignupRole>([...SIGNUP_SAFE_CORE_ROLES, ...EXTENDED_MUSIC_ROLES]),
);
