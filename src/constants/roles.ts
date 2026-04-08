import type { RoleKey } from '../api/types';

export const ROLE_VALUES = [
  'Admin',
  'Manager',
  'Engineer',
  'Teacher',
  'Reception',
  'Accounting',
  'Artist',
  'Student',
  'Vendor',
  'ReadOnly',
  'Customer',
] as const satisfies readonly [RoleKey, ...RoleKey[]];

export const ROLE_OPTIONS: { value: RoleKey; label: string }[] = ROLE_VALUES.map((value) => ({
  value,
  label: value,
}));

export function formatRoleList(roles?: RoleKey[] | null): string {
  if (!roles || roles.length === 0) {
    return '—';
  }
  return roles.join(', ');
}

