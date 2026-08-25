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

const ROLE_ORDER = new Map<RoleKey, number>(ROLE_VALUES.map((value, index) => [value, index]));

export function normalizeRoleList(roles?: readonly RoleKey[] | null): RoleKey[] {
  if (!roles || roles.length === 0) {
    return [];
  }

  return [...new Set(roles)].sort((left, right) => {
    const leftOrder = ROLE_ORDER.get(left) ?? Number.MAX_SAFE_INTEGER;
    const rightOrder = ROLE_ORDER.get(right) ?? Number.MAX_SAFE_INTEGER;

    if (leftOrder !== rightOrder) {
      return leftOrder - rightOrder;
    }

    return left.localeCompare(right);
  });
}

export function formatRoleList(roles?: readonly RoleKey[] | null): string {
  const normalizedRoles = normalizeRoleList(roles);

  if (normalizedRoles.length === 0) {
    return '—';
  }

  return normalizedRoles.join(', ');
}
