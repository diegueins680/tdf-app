import { useDeferredValue, useEffect, useMemo, useState } from 'react';
import {
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  FormControlLabel,
  IconButton,
  InputAdornment,
  Link,
  Stack,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import ClearIcon from '@mui/icons-material/Clear';
import WhatsAppIcon from '@mui/icons-material/WhatsApp';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Link as RouterLink } from 'react-router-dom';
import { Admin, type AdminUser } from '../api/admin';
import AdminUserCommunicationDialog from '../components/AdminUserCommunicationDialog';

const CONTACT_PLACEHOLDER_VALUE_KEYS = new Set([
  '-',
  'n a',
  'na',
  'ninguna',
  'ninguno',
  'no tiene celular',
  'no tiene correo',
  'no tiene email',
  'no tiene telefono',
  'no tiene whatsapp',
  'no aplica',
  'no disponible',
  'none',
  'not available',
  'pendiente',
  'por actualizar',
  'pendiente por validar',
  'por validar',
  'por confirmar',
  'por definir',
  'desconocido',
  'desconocida',
  'sin definir',
  'sin celular',
  'sin correo',
  'sin email',
  'sin numero',
  'sin telefono',
  'sin whatsapp',
  'sin actualizar',
  'tbd',
]);

const normalizeContactPlaceholderKey = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/[.!?:;]+$/g, '')
    .replace(/[^a-z0-9-]+/gi, ' ')
    .replace(/\s+/g, ' ')
    .trim()
    .toLocaleLowerCase('es');

const isPlaceholderContactValue = (value: string) =>
  CONTACT_PLACEHOLDER_VALUE_KEYS.has(normalizeContactPlaceholderKey(value));

const normalizeContactValue = (value?: string | null) => {
  const trimmed = value?.trim();
  if (trimmed == null || trimmed === '') return null;
  if (isPlaceholderContactValue(trimmed)) return null;
  return trimmed;
};

const normalizeIdentityDisplayName = (value?: string | null) => normalizeContactValue(value);

const normalizeAccessKey = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/\s+/g, ' ')
    .trim()
    .toLocaleLowerCase('es');

const ACCESS_PLACEHOLDER_VALUE_KEYS = new Set([
  '-',
  'n/a',
  'na',
  'desconocido',
  'desconocida',
  'no aplica',
  'no asignado',
  'no asignada',
  'not assigned',
  'none',
  'pendiente',
  'pendiente por validar',
  'por actualizar',
  'por confirmar',
  'por definir',
  'por validar',
  'sin actualizar',
  'sin acceso',
  'sin acceso asignado',
  'sin asignar',
  'sin modulo',
  'sin modulo asignado',
  'sin modulos',
  'sin modulos asignados',
  'sin rol',
  'sin rol asignado',
  'sin roles',
  'sin roles asignados',
  'tbd',
  'unassigned',
]);

const isPlaceholderAccessValue = (value: string) => {
  const accessKey = normalizeAccessKey(value).replace(/[.!?:;]+$/g, '').trim();
  return ACCESS_PLACEHOLDER_VALUE_KEYS.has(accessKey);
};

const normalizeAccessValues = (values: readonly string[]) => {
  const valuesByKey = new Map<string, string>();

  values.forEach((value) => {
    const trimmedValue = value.trim();
    if (!trimmedValue) return;
    if (isPlaceholderAccessValue(trimmedValue)) return;

    const accessKey = normalizeAccessKey(trimmedValue);
    if (!valuesByKey.has(accessKey)) {
      valuesByKey.set(accessKey, trimmedValue);
    }
  });

  return [...valuesByKey.values()]
    .sort((left, right) => left.localeCompare(right));
};

const preferNonEmptyText = (primary?: string | null, fallback?: string | null) => {
  const normalizedPrimary = normalizeContactValue(primary);
  if (normalizedPrimary) return normalizedPrimary;
  return normalizeContactValue(fallback);
};

const preferLinkedProfileId = (primary?: number | null, fallback?: number | null) => {
  if (typeof primary === 'number' && Number.isInteger(primary) && primary > 0) return primary;
  if (typeof fallback === 'number' && Number.isInteger(fallback) && fallback > 0) return fallback;
  return primary ?? fallback ?? null;
};

const mergeAdminUserRecords = (primary: AdminUser, fallback: AdminUser): AdminUser => ({
  ...primary,
  partyId: preferLinkedProfileId(primary.partyId, fallback.partyId),
  partyName: preferNonEmptyText(primary.partyName, fallback.partyName) ?? '',
  username: preferNonEmptyText(primary.username, fallback.username) ?? '',
  primaryEmail: preferNonEmptyText(primary.primaryEmail, fallback.primaryEmail),
  primaryPhone: preferNonEmptyText(primary.primaryPhone, fallback.primaryPhone),
  whatsapp: preferNonEmptyText(primary.whatsapp, fallback.whatsapp),
  active: primary.active || fallback.active,
  roles: normalizeAccessValues([...primary.roles, ...fallback.roles]),
  modules: normalizeAccessValues([...primary.modules, ...fallback.modules]),
});

const getUserContactSummary = (user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail'>) => {
  const preferredPhone = getUserWhatsAppChannel(user);
  const email = normalizeContactValue(user.primaryEmail);

  if (preferredPhone && email) return `${preferredPhone} · ${email}`;
  return preferredPhone ?? email;
};

const normalizeContactComparisonValue = (value?: string | null) =>
  normalizeContactValue(value)?.toLocaleLowerCase('es') ?? null;

const normalizePhoneComparisonValue = (value?: string | null) => {
  const normalizedValue = normalizeContactValue(value);
  if (!normalizedValue || !/^\+?[\d\s().-]+$/.test(normalizedValue)) return null;

  const digits = normalizedValue.replace(/\D/g, '');
  return digits.length >= 7 ? digits : null;
};

const getPhoneComparisonCandidates = (value?: string | null) => {
  const digits = normalizePhoneComparisonValue(value);
  if (!digits) return [];

  const candidates = [digits];
  if (digits.startsWith('0') && digits.length > 7) {
    candidates.push(digits.slice(1));
    candidates.push(`593${digits.slice(1)}`);
  }
  if (digits.startsWith('593') && digits.length > 9) {
    candidates.push(`0${digits.slice(3)}`);
  }

  return candidates.filter((candidate, index) => (
    candidate.length >= 7 && candidates.indexOf(candidate) === index
  ));
};

const phoneComparisonValuesMatch = (left?: string | null, right?: string | null) => {
  const leftCandidates = getPhoneComparisonCandidates(left);
  const rightCandidates = getPhoneComparisonCandidates(right);

  if (leftCandidates.length === 0 || rightCandidates.length === 0) return false;

  return leftCandidates.some((leftDigits) => (
    rightCandidates.some((rightDigits) => (
      leftDigits === rightDigits
      || leftDigits.endsWith(rightDigits)
      || rightDigits.endsWith(leftDigits)
    ))
  ));
};

const contactComparisonValuesMatch = (left?: string | null, right?: string | null) => {
  const normalizedLeft = normalizeContactComparisonValue(left);
  const normalizedRight = normalizeContactComparisonValue(right);

  return Boolean(normalizedLeft && normalizedRight && normalizedLeft === normalizedRight)
    || phoneComparisonValuesMatch(left, right);
};

const matchesVisibleIdentityValue = (
  value: string | null,
  identityValues: readonly (string | null | undefined)[],
) => {
  if (!normalizeContactValue(value)) return false;

  return identityValues.some((identityValue) => (
    contactComparisonValuesMatch(identityValue, value)
  ));
};

const getVisibleUserContactSummary = (
  user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail' | 'partyName' | 'username'>,
) => {
  const identityValues = [normalizeIdentityDisplayName(user.partyName), user.username];
  const preferredPhone = getUserWhatsAppChannel(user);
  const email = normalizeContactValue(user.primaryEmail);
  const visibleEmail = matchesVisibleIdentityValue(email, identityValues) ? null : email;
  const visiblePhone = preferredPhone && visibleEmail ? null : (
    matchesVisibleIdentityValue(preferredPhone, identityValues) ? null : preferredPhone
  );

  if (visiblePhone && visibleEmail) return `${visiblePhone} · ${visibleEmail}`;
  return visiblePhone ?? visibleEmail;
};

const getUserContactSearchValues = (
  user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail'>,
) => (
  [user.whatsapp, user.primaryPhone, user.primaryEmail]
    .map(normalizeContactValue)
    .filter((value): value is string => value != null)
    .flatMap((value) => {
      const phoneDigits = normalizePhoneComparisonValue(value);
      return phoneDigits ? [value, ...getPhoneComparisonCandidates(value)] : [value];
    })
);

const normalizeWhatsAppChannel = (value?: string | null) => {
  const normalizedValue = normalizeContactValue(value);
  return normalizedValue && normalizePhoneComparisonValue(normalizedValue) ? normalizedValue : null;
};

const getUserWhatsAppChannel = (user: Pick<AdminUser, 'whatsapp' | 'primaryPhone'>) =>
  normalizeWhatsAppChannel(user.whatsapp) ?? normalizeWhatsAppChannel(user.primaryPhone);

const hasUserWhatsAppChannel = (user: Pick<AdminUser, 'whatsapp' | 'primaryPhone'>) =>
  Boolean(getUserWhatsAppChannel(user));

const getUserContactReadiness = (
  user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail'>,
) => {
  if (hasUserWhatsAppChannel(user)) return 'whatsapp-ready' as const;
  if (getUserContactSummary(user)) return 'contact-ready' as const;
  return 'missing-contact' as const;
};

const getUserContactReadinessSearchValues = (
  user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail'>,
) => {
  const readiness = getUserContactReadiness(user);

  if (readiness === 'whatsapp-ready') {
    return ['listo para WhatsApp', 'listos para WhatsApp', 'WhatsApp listo'];
  }

  if (readiness === 'contact-ready') {
    return [
      'pendiente de WhatsApp',
      'pendientes de WhatsApp',
      'pendiente WhatsApp',
      'pendientes WhatsApp',
      'WhatsApp pendiente',
      'sin WhatsApp',
    ];
  }

  return [
    'pendiente de contacto',
    'pendientes de contacto',
    'pendiente contacto',
    'pendientes contacto',
    'contacto pendiente',
    'sin contacto',
    'sin WhatsApp',
  ];
};

const joinSpanishSummaryParts = (parts: readonly string[]) => {
  if (parts.length <= 1) return parts[0] ?? '';
  if (parts.length === 2) return `${parts[0]} y ${parts[1]}`;
  return `${parts.slice(0, -1).join(', ')} y ${parts[parts.length - 1]}`;
};

const buildContactStateSummary = ({
  readyForWhatsAppCount,
  pendingWhatsAppCount,
  pendingContactCount,
}: {
  readyForWhatsAppCount: number;
  pendingWhatsAppCount: number;
  pendingContactCount: number;
}) => {
  const parts: string[] = [];

  if (readyForWhatsAppCount > 0) {
    parts.push(`${readyForWhatsAppCount} ${readyForWhatsAppCount === 1 ? 'listo' : 'listos'} para WhatsApp`);
  }

  if (pendingWhatsAppCount > 0) {
    parts.push(`${pendingWhatsAppCount} ${pendingWhatsAppCount === 1 ? 'pendiente' : 'pendientes'} de WhatsApp`);
  }

  if (pendingContactCount > 0) {
    parts.push(`${pendingContactCount} ${pendingContactCount === 1 ? 'pendiente' : 'pendientes'} de contacto`);
  }

  return joinSpanishSummaryParts(parts);
};

const buildPendingProfileSummary = (count: number) => (
  count === 1
    ? '1 usuario todavía sin perfil vinculado; su nombre no abre un perfil.'
    : `${count} usuarios todavía sin perfil vinculado; sus nombres no abren un perfil.`
);

const buildCollapsedInactiveUsersToggleLabel = (users: readonly AdminUser[]) => {
  return `Ver ${formatInactiveUserCountLabel(users.length)}`;
};

const buildExpandedInactiveUsersToggleLabel = (users: readonly AdminUser[]) => {
  return `Ocultar ${formatInactiveUserCountLabel(users.length)}`;
};

const getUserAccessSummary = (values: string[]) =>
  normalizeAccessValues(values)
    .join(', ');

const getSharedAccessSummary = (values: string[]) => {
  if (values.length < 2) return '';
  const normalizedValues = values.map((value) => value.trim());
  if (normalizedValues.some((value) => value === '')) return '';
  const [firstValue, ...rest] = normalizedValues;
  const firstValueKey = normalizeAccessKey(firstValue ?? '');
  return rest.every((value) => normalizeAccessKey(value) === firstValueKey) ? (firstValue ?? '') : '';
};

const isSameAccessSummary = (left: string, right: string) =>
  normalizeAccessKey(left) === normalizeAccessKey(right);

const getSharedAccessValues = (valuesByUser: readonly string[][]) => {
  if (valuesByUser.length < 2) return [];

  const normalizedValuesByUser = valuesByUser.map(normalizeAccessValues);
  if (normalizedValuesByUser.some((values) => values.length === 0)) return [];

  const [firstValues = [], ...restValues] = normalizedValuesByUser;
  const firstSummary = getUserAccessSummary(firstValues);

  return restValues.every((values) => isSameAccessSummary(getUserAccessSummary(values), firstSummary))
    ? firstValues
    : [];
};

const formatAccessSummaryParts = ({
  modulesSummary,
  rolesSummary,
}: {
  modulesSummary: string;
  rolesSummary: string;
}) => {
  if (rolesSummary && modulesSummary && isSameAccessSummary(rolesSummary, modulesSummary)) {
    return `Roles y módulos: ${rolesSummary}`;
  }

  return [
    rolesSummary ? `Roles: ${rolesSummary}` : '',
    modulesSummary ? `Módulos: ${modulesSummary}` : '',
  ].filter(Boolean).join(' · ');
};

const INLINE_ACCESS_VALUE_LIMIT = 3;

const formatCompactAccessValues = (
  values: readonly string[],
  hiddenSingularLabel: string,
  hiddenPluralLabel: string,
) => {
  const normalizedValues = normalizeAccessValues(values);

  if (normalizedValues.length <= INLINE_ACCESS_VALUE_LIMIT) {
    return normalizedValues.join(', ');
  }

  const visibleValues = normalizedValues.slice(0, INLINE_ACCESS_VALUE_LIMIT);
  const hiddenCount = normalizedValues.length - INLINE_ACCESS_VALUE_LIMIT;

  return `${visibleValues.join(', ')} +${hiddenCount} ${
    hiddenCount === 1 ? hiddenSingularLabel : hiddenPluralLabel
  }`;
};

const normalizeSearchValue = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .replace(/\s+/g, ' ')
    .trim()
    .toLocaleLowerCase('es');
const normalizeIdentityComparison = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLocaleLowerCase('es')
    .replace(/[^a-z0-9]+/g, '');
const getSearchValueVariants = (value?: string | null) => {
  const rawValue = value ?? '';
  const variants = [
    normalizeSearchValue(rawValue),
    normalizeIdentityComparison(rawValue),
  ];

  return variants.filter((variant, index) => variant !== '' && variants.indexOf(variant) === index);
};
const normalizeVisibleSearchInput = (value: string) => (value.trim().length === 0 ? '' : value);
const MAX_SEARCH_QUERY_SUMMARY_LENGTH = 64;
const normalizeSearchQuerySummary = (value: string) => value.trim().replace(/\s+/g, ' ');
const formatSearchQuerySummary = (value: string) => {
  const normalizedValue = normalizeSearchQuerySummary(value);

  if (normalizedValue.length <= MAX_SEARCH_QUERY_SUMMARY_LENGTH) {
    return normalizedValue;
  }

  const summaryPrefix = normalizedValue.slice(0, MAX_SEARCH_QUERY_SUMMARY_LENGTH + 1);
  const lastWordBoundary = summaryPrefix.lastIndexOf(' ');
  const compactPrefix = (
    lastWordBoundary > 0
      ? summaryPrefix.slice(0, lastWordBoundary)
      : normalizedValue.slice(0, MAX_SEARCH_QUERY_SUMMARY_LENGTH)
  ).trimEnd();

  return `${compactPrefix}...`;
};
const hasLinkedAdminUserProfile = (user: Pick<AdminUser, 'partyId'>) =>
  typeof user.partyId === 'number' && Number.isInteger(user.partyId) && user.partyId > 0;

const formatUserCountLabel = (count: number) => `${count} usuario${count === 1 ? '' : 's'}`;
const formatActiveUserCountLabel = (count: number) => `${formatUserCountLabel(count)} activo${count === 1 ? '' : 's'}`;
const formatInactiveUserCountLabel = (count: number) => `${formatUserCountLabel(count)} inactivo${count === 1 ? '' : 's'}`;
const MIN_USERS_FOR_SEARCH = 3;
const MIN_USERS_FOR_REFRESH = 4;
const SEARCH_INPUT_PLACEHOLDER = 'Nombre, usuario, contacto, rol o módulo';
const ACCOUNT_SEARCH_PLACEHOLDER = 'Cuenta';
const ADMIN_USERS_PAGE_TITLE = 'Usuarios admin';
const ADMIN_USERS_EMPTY_STATE =
  'No hay cuentas admin activas. Cuando exista la primera cuenta activa, esta vista mostrará perfil, contacto y WhatsApp si está disponible.';
const ADMIN_USERS_EMPTY_WITH_INACTIVE_STATE =
  'No hay cuentas admin activas ni inactivas. Cuando exista la primera, esta vista mostrará perfil, contacto y WhatsApp si está disponible.';
const ADMIN_USERS_REVIEW_INACTIVE_EMPTY_ACTION = 'Ver si hay cuentas inactivas';
const ADMIN_USERS_REVIEW_INACTIVE_SINGLE_USER_ACTION = ADMIN_USERS_REVIEW_INACTIVE_EMPTY_ACTION;
const ADMIN_USERS_SEARCH_EMPTY_INACTIVE_ACTION = 'Buscar también en cuentas inactivas';
const ADMIN_USERS_SEARCH_INACTIVE_STATUS_ACTION = 'Buscar cuentas inactivas';
const INCLUDE_INACTIVE_FILTER_LABEL = 'Incluir inactivos';
const INACTIVE_FILTER_ACTIVE_LABEL = 'Inactivos incluidos';
const INCLUDE_INACTIVE_SEARCH_FILTER_LABEL = 'Buscar también en inactivos';
const INACTIVE_SEARCH_FILTER_ACTIVE_LABEL = 'Buscando en inactivos';
const RETURN_TO_ACTIVE_USERS_ACTION = 'Volver a usuarios activos';
const DEFAULT_SHARED_ADMIN_ROLES_SUMMARY = 'Admin';
const DEFAULT_SHARED_ADMIN_MODULES_SUMMARY = 'admin';
const ADMIN_USERS_PAGE_INTRO =
  'Abre el perfil desde el nombre y usa WhatsApp cuando haya un número disponible.';
const ADMIN_USERS_PAGE_PROFILE_PENDING_INTRO =
  'Usa WhatsApp cuando haya un número disponible. El acceso al perfil aparecerá desde el nombre cuando el usuario ya tenga un perfil vinculado.';
const ADMIN_USERS_PAGE_NUMBER_SETUP_INTRO =
  'Abre el perfil desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible.';
const ADMIN_USERS_PAGE_PROFILE_PENDING_NUMBER_SETUP_INTRO =
  'Estos usuarios todavía no tienen un perfil vinculado. Cuando lo tengan, podrás abrirlos desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible.';
const ADMIN_USERS_PAGE_CONTACT_SETUP_INTRO =
  'Abre el perfil desde el nombre para completar el contacto pendiente. WhatsApp aparecerá cuando haya un número disponible.';
const ADMIN_USERS_PAGE_PROFILE_PENDING_CONTACT_SETUP_INTRO =
  'Estos usuarios todavía no tienen un perfil vinculado. Cuando lo tengan, podrás abrirlos desde el nombre para completar el contacto pendiente. WhatsApp aparecerá cuando haya un número disponible.';
const NO_ACCESS_ASSIGNED_SUMMARY = 'Sin acceso asignado';
const SINGLE_USER_GUIDANCE =
  'Solo hay un usuario por ahora. Abre su perfil desde el nombre y usa WhatsApp si ya tiene un número disponible.';
const SINGLE_USER_NUMBER_SETUP_GUIDANCE =
  'Solo hay un usuario por ahora. Abre su perfil desde el nombre para agregar o corregir un número. Cuando tenga un número disponible, WhatsApp aparecerá aquí.';
const SINGLE_USER_CONTACT_SETUP_GUIDANCE =
  'Solo hay un usuario por ahora. Abre su perfil desde el nombre para completar el contacto pendiente. Cuando tenga un número disponible, WhatsApp aparecerá aquí.';
const SINGLE_SEARCH_RESULT_GUIDANCE =
  'Resultado único. Abre el perfil desde el nombre y usa WhatsApp si ya está disponible.';
const SINGLE_SEARCH_RESULT_NUMBER_SETUP_GUIDANCE =
  'Resultado único. Abre el perfil desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible.';
const SINGLE_SEARCH_RESULT_CONTACT_SETUP_GUIDANCE =
  'Resultado único. Abre el perfil desde el nombre para completar el contacto pendiente. WhatsApp aparecerá cuando haya un número disponible.';
const makeSingleUserInactiveGuidance = (guidance: string) =>
  guidance
    .replace('Solo hay un usuario por ahora.', 'Solo hay un usuario inactivo por ahora.');
const makeSingleSearchResultInactiveGuidance = (guidance: string) =>
  guidance
    .replace('Resultado único.', 'Resultado único inactivo.');

const spanishOrConnector = (term: string) => (/^h?o/i.test(term.trim()) ? 'u' : 'o');

const formatSearchPlaceholderTerms = (
  terms: readonly string[],
  emptyFallback = SEARCH_INPUT_PLACEHOLDER,
) => {
  if (terms.length === 0) return emptyFallback;
  if (terms.length === 1) return terms[0] ?? emptyFallback;

  const lastTerm = terms[terms.length - 1] ?? '';
  const connector = spanishOrConnector(lastTerm);

  if (terms.length === 2) return `${terms[0]} ${connector} ${lastTerm}`;
  return `${terms.slice(0, -1).join(', ')} ${connector} ${lastTerm}`;
};

type UserContactReadiness = ReturnType<typeof getUserContactReadiness>;
const CONTACT_READINESS_SORT_ORDER: Record<UserContactReadiness, number> = {
  'whatsapp-ready': 0,
  'contact-ready': 1,
  'missing-contact': 2,
};

const buildPendingProfileGuidance = ({
  scope,
  readiness,
}: {
  scope: 'single-user' | 'single-result';
  readiness: UserContactReadiness;
}) => {
  const scopePrefix = scope === 'single-user' ? 'Solo hay un usuario por ahora.' : 'Resultado único.';
  const missingProfileMessage =
    ' Este usuario todavía no tiene un perfil vinculado, así que el nombre no abre un perfil.';

  if (readiness === 'whatsapp-ready') {
    return `${scopePrefix}${missingProfileMessage} Usa WhatsApp si ya tiene un número disponible.`;
  }

  if (readiness === 'contact-ready') {
    return `${scopePrefix}${missingProfileMessage} Cuando se vincule, podrás abrirlo desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible.`;
  }

  return `${scopePrefix}${missingProfileMessage} Cuando se vincule, podrás abrirlo desde el nombre para completar el contacto pendiente. WhatsApp aparecerá cuando haya un número disponible.`;
};

const buildUserAccessSummary = ({
  modules,
  roles,
  sharedModulesSummary = '',
  sharedRolesSummary = '',
}: {
  modules: string[];
  roles: string[];
  sharedModulesSummary?: string;
  sharedRolesSummary?: string;
}) => {
  const rolesSummary = getUserAccessSummary(roles);
  const modulesSummary = getUserAccessSummary(modules);

  if (!rolesSummary && !modulesSummary) {
    return NO_ACCESS_ASSIGNED_SUMMARY;
  }

  const showRolesSummary = Boolean(rolesSummary)
    && !isSameAccessSummary(rolesSummary, sharedRolesSummary)
    && !isSameAccessSummary(rolesSummary, DEFAULT_SHARED_ADMIN_ROLES_SUMMARY);
  const showModulesSummary = Boolean(modulesSummary)
    && !isSameAccessSummary(modulesSummary, sharedModulesSummary)
    && !isSameAccessSummary(modulesSummary, DEFAULT_SHARED_ADMIN_MODULES_SUMMARY);

  return formatAccessSummaryParts({
    rolesSummary: showRolesSummary ? rolesSummary : '',
    modulesSummary: showModulesSummary ? modulesSummary : '',
  });
};

const buildUserRowAccessSummary = ({
  modules,
  roles,
  sharedModulesSummary = '',
  sharedRolesSummary = '',
}: {
  modules: string[];
  roles: string[];
  sharedModulesSummary?: string;
  sharedRolesSummary?: string;
}) => {
  const rolesSummary = getUserAccessSummary(roles);
  const modulesSummary = getUserAccessSummary(modules);

  if (!rolesSummary && !modulesSummary) {
    return NO_ACCESS_ASSIGNED_SUMMARY;
  }

  if (isDefaultAdminAccessSummary({ rolesSummary, modulesSummary })) {
    return '';
  }

  const showRolesSummary = Boolean(rolesSummary)
    && !isSameAccessSummary(rolesSummary, sharedRolesSummary)
    && !isSameAccessSummary(rolesSummary, DEFAULT_SHARED_ADMIN_ROLES_SUMMARY);
  const showModulesSummary = Boolean(modulesSummary)
    && !isSameAccessSummary(modulesSummary, sharedModulesSummary)
    && !isSameAccessSummary(modulesSummary, DEFAULT_SHARED_ADMIN_MODULES_SUMMARY);
  const compactRolesSummary = formatCompactAccessValues(roles, 'rol', 'roles');
  const compactModulesSummary = formatCompactAccessValues(modules, 'módulo', 'módulos');

  if (showRolesSummary && showModulesSummary && isSameAccessSummary(rolesSummary, modulesSummary)) {
    return `Roles y módulos: ${compactRolesSummary}`;
  }

  return formatAccessSummaryParts({
    rolesSummary: showRolesSummary ? compactRolesSummary : '',
    modulesSummary: showModulesSummary ? compactModulesSummary : '',
  });
};

const hasNoAccessAssigned = (user: Pick<AdminUser, 'modules' | 'roles'>) =>
  getUserAccessSummary(user.roles) === '' && getUserAccessSummary(user.modules) === '';

const buildHiddenInactiveAccessSummary = (users: readonly AdminUser[]) => {
  if (users.length < 2) return '';

  const sharedRolesValues = getSharedAccessValues(users.map((user) => user.roles));
  const sharedModulesValues = getSharedAccessValues(users.map((user) => user.modules));
  if (sharedRolesValues.length === 0 && sharedModulesValues.length === 0) return '';

  const compactAccessSummary = buildUserRowAccessSummary({
    roles: sharedRolesValues,
    modules: sharedModulesValues,
  });

  return compactAccessSummary ? `acceso compartido (${compactAccessSummary})` : '';
};

const buildCollapsedInactiveUsersToggleTitle = (users: readonly AdminUser[]) => {
  const noAccessCount = users.filter(hasNoAccessAssigned).length;
  const pendingProfileCount = users.filter((user) => !hasLinkedAdminUserProfile(user)).length;
  const readyForWhatsAppCount = users.filter((user) => getUserContactReadiness(user) === 'whatsapp-ready').length;
  const pendingWhatsAppCount = users.filter((user) => getUserContactReadiness(user) === 'contact-ready').length;
  const pendingContactCount = users.filter((user) => getUserContactReadiness(user) === 'missing-contact').length;
  const hiddenAccessSummary = noAccessCount === 0 ? buildHiddenInactiveAccessSummary(users) : '';
  const contactStateSummary = buildContactStateSummary({
    readyForWhatsAppCount,
    pendingWhatsAppCount,
    pendingContactCount,
  });
  const parts = [
    noAccessCount > 0 ? `${noAccessCount} sin acceso asignado` : '',
    hiddenAccessSummary,
    pendingProfileCount > 0 ? `${pendingProfileCount} sin perfil vinculado` : '',
    contactStateSummary,
  ].filter(Boolean);

  return parts.length > 0
    ? `Usuarios inactivos ocultos: ${joinSpanishSummaryParts(parts)}.`
    : undefined;
};

const getUserAccessStateSearchValues = (user: Pick<AdminUser, 'modules' | 'roles'>) => (
  hasNoAccessAssigned(user)
    ? ['sin acceso', 'sin acceso asignado', 'sin permisos', 'acceso pendiente']
    : []
);

const getUserProfileStateSearchValues = (user: Pick<AdminUser, 'partyId'>) => (
  hasLinkedAdminUserProfile(user)
    ? []
    : ['perfil pendiente', 'sin perfil', 'sin perfil vinculado', 'perfil no vinculado']
);

const getLinkedProfileLabelSearchValues = (user: Pick<AdminUser, 'partyId'>) => (
  hasLinkedAdminUserProfile(user)
    ? [
        `perfil ${user.partyId}`,
        `perfil #${user.partyId}`,
      ]
    : []
);

const getLinkedProfileSearchValues = (user: Pick<AdminUser, 'partyId'>) => (
  hasLinkedAdminUserProfile(user)
    ? [
        String(user.partyId),
        `id ${user.partyId}`,
        ...getLinkedProfileLabelSearchValues(user),
      ]
    : []
);

const matchesLinkedProfileLabelSearchQuery = (user: Pick<AdminUser, 'partyId'>, rawQuery: string) => {
  const queryVariants = getSearchValueVariants(rawQuery);
  if (queryVariants.length === 0) return false;

  const profileSearchSpace = getLinkedProfileLabelSearchValues(user)
    .flatMap(getSearchValueVariants)
    .filter(Boolean);

  return queryVariants.some((query) => (
    profileSearchSpace.some((value) => value.includes(query))
  ));
};

const isDefaultAdminAccessSummary = ({
  modulesSummary,
  rolesSummary,
}: {
  modulesSummary: string;
  rolesSummary: string;
}) => (
  isSameAccessSummary(rolesSummary, DEFAULT_SHARED_ADMIN_ROLES_SUMMARY)
  && isSameAccessSummary(modulesSummary, DEFAULT_SHARED_ADMIN_MODULES_SUMMARY)
);

const buildNonDefaultUserAccessSummary = (user: Pick<AdminUser, 'modules' | 'roles'> | null) => {
  if (!user) return '';

  const rolesSummary = getUserAccessSummary(user.roles);
  const modulesSummary = getUserAccessSummary(user.modules);

  if (isDefaultAdminAccessSummary({ rolesSummary, modulesSummary })) {
    return '';
  }

  return buildUserAccessSummary({
    roles: user.roles,
    modules: user.modules,
  });
};

const buildNonDefaultUserAccessSummaryCopy = (user: Pick<AdminUser, 'modules' | 'roles'> | null) => {
  const fullSummary = buildNonDefaultUserAccessSummary(user);

  if (!user || !fullSummary) {
    return { text: '', title: undefined as string | undefined };
  }

  const compactSummary = buildUserRowAccessSummary({
    roles: user.roles,
    modules: user.modules,
  });

  return {
    text: compactSummary,
    title: compactSummary === fullSummary ? undefined : fullSummary,
  };
};

const buildAdminUsersSearchPlaceholder = (users: readonly AdminUser[]) => {
  let hasNameIdentity = false;
  let hasDistinctUsername = false;
  let hasContact = false;
  let hasActiveUsers = false;
  let hasInactiveUsers = false;
  let hasNoAccessAssignedUsers = false;
  const roleSummaries: string[] = [];
  const moduleSummaries: string[] = [];

  users.forEach((user) => {
    const partyName = normalizeIdentityDisplayName(user.partyName) ?? '';
    const username = user.username.trim();
    const rolesSummary = getUserAccessSummary(user.roles);
    const modulesSummary = getUserAccessSummary(user.modules);
    if (!rolesSummary && !modulesSummary) hasNoAccessAssignedUsers = true;

    if (partyName) hasNameIdentity = true;
    if (username && normalizeIdentityComparison(username) !== normalizeIdentityComparison(partyName)) {
      hasDistinctUsername = true;
    }
    if (getVisibleUserContactSummary(user)) hasContact = true;
    if (user.active) {
      hasActiveUsers = true;
    } else {
      hasInactiveUsers = true;
    }

    roleSummaries.push(rolesSummary);
    moduleSummaries.push(modulesSummary);
  });

  const hasNonDefaultRoles = users.length > 1
    && roleSummaries.some((summary) => summary && !isSameAccessSummary(summary, DEFAULT_SHARED_ADMIN_ROLES_SUMMARY))
    && getSharedAccessSummary(roleSummaries) === '';
  const hasNonDefaultModules = users.length > 1
    && moduleSummaries.some((summary) => summary && !isSameAccessSummary(summary, DEFAULT_SHARED_ADMIN_MODULES_SUMMARY))
    && getSharedAccessSummary(moduleSummaries) === '';
  const modulesAddDistinctSearchValue = !hasNonDefaultRoles || moduleSummaries.some((moduleSummary, index) => {
    if (!moduleSummary.trim()) return false;

    const roleSummary = roleSummaries[index]?.trim() ?? '';
    return roleSummary === '' || !isSameAccessSummary(moduleSummary, roleSummary);
  });

  const terms: string[] = [];
  if (hasNameIdentity) terms.push('Nombre');
  if (hasDistinctUsername) terms.push(hasNameIdentity ? 'usuario' : 'Usuario');
  if (hasContact) terms.push(terms.length === 0 ? 'Contacto' : 'contacto');
  const shouldUseAccessUmbrellaTerm =
    hasActiveUsers
    && hasInactiveUsers
    && hasNonDefaultRoles
    && hasNonDefaultModules
    && modulesAddDistinctSearchValue;

  if (shouldUseAccessUmbrellaTerm) {
    terms.push(terms.length === 0 ? 'Acceso' : 'acceso');
  } else {
    if (hasNonDefaultRoles) terms.push(terms.length === 0 ? 'Rol' : 'rol');
    if (hasNonDefaultModules && modulesAddDistinctSearchValue) terms.push(terms.length === 0 ? 'Módulo' : 'módulo');
  }
  if (
    hasNoAccessAssignedUsers
    && !hasNonDefaultRoles
    && !hasNonDefaultModules
    && terms.length > 0
  ) {
    terms.push('acceso');
  }
  if (hasActiveUsers && hasInactiveUsers) terms.push(terms.length === 0 ? 'Estado' : 'estado');

  return formatSearchPlaceholderTerms(
    terms,
    users.length > 0 ? ACCOUNT_SEARCH_PLACEHOLDER : SEARCH_INPUT_PLACEHOLDER,
  );
};

const summarizeUserIdentity = (user: Pick<AdminUser, 'partyName' | 'username' | 'userId'>) => {
  const displayName = normalizeIdentityDisplayName(user.partyName) ?? '';
  const username = user.username.trim();
  const primary = displayName || username || `Cuenta #${user.userId}`;
  const showUsername = displayName !== ''
    && username !== ''
    && normalizeIdentityComparison(displayName) !== normalizeIdentityComparison(username);
  const secondaryParts: string[] = [];

  if (showUsername) secondaryParts.push(`Usuario: ${username}`);

  return {
    primary,
    secondary: secondaryParts.join(' · '),
  };
};

const getUserVisibleIdentityKey = (user: Pick<AdminUser, 'partyName' | 'username' | 'userId'>) => {
  const identity = summarizeUserIdentity(user);
  return [identity.primary, identity.secondary]
    .map((value) => value.trim().toLocaleLowerCase('es'))
    .join('::');
};

const getUserIdsRequiringIdentityDisambiguator = (users: readonly AdminUser[]) => {
  const identityCounts = new Map<string, number>();

  users.forEach((user) => {
    const identityKey = getUserVisibleIdentityKey(user);
    identityCounts.set(identityKey, (identityCounts.get(identityKey) ?? 0) + 1);
  });

  return new Set(
    users
      .filter((user) => (identityCounts.get(getUserVisibleIdentityKey(user)) ?? 0) > 1)
      .map((user) => user.userId),
  );
};

const compareAdminUsers = (left: AdminUser, right: AdminUser) => {
  if (left.active !== right.active) return left.active ? -1 : 1;

  const leftReadinessOrder = CONTACT_READINESS_SORT_ORDER[getUserContactReadiness(left)];
  const rightReadinessOrder = CONTACT_READINESS_SORT_ORDER[getUserContactReadiness(right)];
  if (leftReadinessOrder !== rightReadinessOrder) {
    return leftReadinessOrder - rightReadinessOrder;
  }

  const leftIdentity = summarizeUserIdentity(left).primary.trim();
  const rightIdentity = summarizeUserIdentity(right).primary.trim();
  const identityComparison = leftIdentity.localeCompare(rightIdentity, 'es', { sensitivity: 'base' });
  if (identityComparison !== 0) return identityComparison;

  const usernameComparison = left.username.trim().localeCompare(right.username.trim(), 'es', { sensitivity: 'base' });
  if (usernameComparison !== 0) return usernameComparison;

  return left.userId - right.userId;
};

const dedupeAdminUsers = (users: readonly AdminUser[]) => {
  const usersById = new Map<number, AdminUser>();

  users.forEach((user) => {
    const existingUser = usersById.get(user.userId);
    if (!existingUser) {
      usersById.set(user.userId, user);
      return;
    }

    usersById.set(user.userId, mergeAdminUserRecords(existingUser, user));
  });

  return [...usersById.values()];
};

const ACTIVE_STATUS_SEARCH_VALUES = [
  'activo',
  'activa',
  'activos',
  'activas',
  'habilitado',
  'habilitada',
  'habilitados',
  'habilitadas',
].map(normalizeSearchValue);

const INACTIVE_STATUS_SEARCH_VALUES = [
  'inactivo',
  'inactiva',
  'inactivos',
  'inactivas',
  'desactivado',
  'desactivada',
  'desactivados',
  'desactivadas',
  'archivado',
  'archivada',
  'archivados',
  'archivadas',
  'suspendido',
  'suspendida',
  'suspendidos',
  'suspendidas',
].map(normalizeSearchValue);

const getUserStatusSearchValues = (active: boolean) => (
  active ? ACTIVE_STATUS_SEARCH_VALUES : INACTIVE_STATUS_SEARCH_VALUES
);

const getUserConcreteSearchValues = (user: AdminUser) => {
  const identity = summarizeUserIdentity(user);

  return [
    user.username,
    normalizeIdentityDisplayName(user.partyName) ?? '',
    identity.primary,
    String(user.userId),
    ...getLinkedProfileSearchValues(user),
    ...getUserContactSearchValues(user),
    getUserAccessSummary(user.roles),
    getUserAccessSummary(user.modules),
  ]
    .flatMap(getSearchValueVariants)
    .filter(Boolean);
};

const getUserStateSearchValues = (user: AdminUser) => [
  ...getUserContactReadinessSearchValues(user),
  ...getUserAccessStateSearchValues(user),
  ...getUserProfileStateSearchValues(user),
]
  .flatMap(getSearchValueVariants)
  .filter(Boolean);

const getSearchTokens = (value: string) =>
  normalizeSearchValue(value)
    .split(' ')
    .filter(Boolean);

const matchesSearchTokenAcrossValues = (
  token: string,
  concreteSearchSpace: readonly string[],
  statusSearchValues: readonly string[],
) => (
  concreteSearchSpace.some((value) => value.includes(token))
  || statusSearchValues.includes(token)
);

const isActiveStatusSearchQuery = (value: string) => {
  const queryVariants = getSearchValueVariants(value);

  return queryVariants.length > 0
    && queryVariants.every((query) => ACTIVE_STATUS_SEARCH_VALUES.includes(query));
};

const isInactiveStatusSearchQuery = (value: string) => {
  const queryVariants = getSearchValueVariants(value);

  return queryVariants.length > 0
    && queryVariants.every((query) => INACTIVE_STATUS_SEARCH_VALUES.includes(query));
};

const matchesUserQuery = (user: AdminUser, rawQuery: string) => {
  const queryVariants = getSearchValueVariants(rawQuery);
  if (queryVariants.length === 0) return true;
  const statusSearchValues = getUserStatusSearchValues(user.active);
  const concreteSearchSpace = getUserConcreteSearchValues(user);
  const stateSearchSpace = getUserStateSearchValues(user);
  const searchSpace = [...concreteSearchSpace, ...stateSearchSpace];
  const matchesExactQuery = queryVariants.some((query) => (
    searchSpace.some((value) => value.includes(query)) || statusSearchValues.includes(query)
  ));

  if (matchesExactQuery) return true;

  const queryTokens = getSearchTokens(rawQuery);
  if (queryTokens.length <= 1) return false;

  return queryTokens.every((token) => (
    matchesSearchTokenAcrossValues(token, concreteSearchSpace, statusSearchValues)
  ));
};

export default function AdminUsersPage() {
  const qc = useQueryClient();
  const [includeInactive, setIncludeInactive] = useState(false);
  const [selectedUser, setSelectedUser] = useState<AdminUser | null>(null);
  const [searchQuery, setSearchQuery] = useState('');
  const [showInactiveUsers, setShowInactiveUsers] = useState(false);
  const deferredSearchQuery = useDeferredValue(searchQuery);

  const usersQuery = useQuery({
    queryKey: ['admin', 'users', includeInactive],
    queryFn: () => Admin.listUsers(includeInactive),
  });
  const users = useMemo(
    () => dedupeAdminUsers(usersQuery.data ?? []),
    [usersQuery.data],
  );

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['admin', 'users'] });
  };

  const handleClearSearch = () => {
    setSearchQuery('');
  };

  const visibleUsers = useMemo(
    () => users
      .filter((user) => matchesUserQuery(user, deferredSearchQuery))
      .sort(compareAdminUsers),
    [deferredSearchQuery, users],
  );
  const visibleInactiveUsersCount = useMemo(
    () => visibleUsers.filter((user) => !user.active).length,
    [visibleUsers],
  );
  const totalUsersCount = users.length;
  const hasUsers = totalUsersCount > 0;
  const hasActiveSearch = normalizeSearchValue(searchQuery).length > 0;
  const fullSearchSummary = normalizeSearchQuerySummary(searchQuery);
  const activeSearchSummary = formatSearchQuerySummary(searchQuery);
  const hasMultipleUsers = totalUsersCount > 1;
  const showGeneralIntro = hasMultipleUsers && !hasActiveSearch;
  const showSingleUserGuidance = totalUsersCount === 1 && !hasActiveSearch;
  const singleVisibleUser = showSingleUserGuidance ? (visibleUsers[0] ?? null) : null;
  const singleVisibleUserReadiness = singleVisibleUser ? getUserContactReadiness(singleVisibleUser) : null;
  const isFiltered = hasActiveSearch && visibleUsers.length !== totalUsersCount;
  const showSingleSearchResultGuidance = hasActiveSearch && visibleUsers.length === 1;
  const singleSearchResult = showSingleSearchResultGuidance ? (visibleUsers[0] ?? null) : null;
  const singleSearchResultReadiness = singleSearchResult ? getUserContactReadiness(singleSearchResult) : null;
  const showOnlyInactiveUsers = !hasActiveSearch
    && includeInactive
    && visibleUsers.length > 0
    && visibleInactiveUsersCount === visibleUsers.length;
  const showOnlyInactiveSearchResults = hasActiveSearch
    && includeInactive
    && visibleUsers.length > 0
    && visibleInactiveUsersCount === visibleUsers.length;
  const showInactiveOnlyScopeSummary = showOnlyInactiveUsers || showOnlyInactiveSearchResults;
  const showInactiveUsersGroup =
    includeInactive
    && visibleInactiveUsersCount > 0
    && (visibleUsers.length > 1 || showOnlyInactiveUsers);
  const activeVisibleUsers = useMemo(
    () => (showInactiveUsersGroup ? visibleUsers.filter((user) => user.active) : visibleUsers),
    [showInactiveUsersGroup, visibleUsers],
  );
  const inactiveVisibleUsers = useMemo(
    () => (showInactiveUsersGroup ? visibleUsers.filter((user) => !user.active) : []),
    [showInactiveUsersGroup, visibleUsers],
  );
  const inactiveVisibleUsersSignature = useMemo(
    () => inactiveVisibleUsers.map((user) => `${user.userId}:${user.active}`).join('|'),
    [inactiveVisibleUsers],
  );
  const shouldCollapseInactiveUsers =
    showInactiveUsersGroup && !hasActiveSearch && activeVisibleUsers.length > 0;
  const showInactiveUsersList = showInactiveUsersGroup && (!shouldCollapseInactiveUsers || showInactiveUsers);
  const usersInCurrentSummary = shouldCollapseInactiveUsers && !showInactiveUsersList
    ? activeVisibleUsers
    : visibleUsers;
  const showSearchField = usersInCurrentSummary.length >= MIN_USERS_FOR_SEARCH || hasActiveSearch;
  const searchInputPlaceholder = useMemo(
    () => buildAdminUsersSearchPlaceholder(usersInCurrentSummary),
    [usersInCurrentSummary],
  );
  const currentSummaryPendingProfileCount = usersInCurrentSummary.filter(
    (user) => !hasLinkedAdminUserProfile(user),
  ).length;
  const hasCurrentSummaryLinkedProfile = usersInCurrentSummary.some((user) => hasLinkedAdminUserProfile(user));
  const sharedRolesSummary = useMemo(
    () => getSharedAccessSummary(usersInCurrentSummary.map((user) => getUserAccessSummary(user.roles))),
    [usersInCurrentSummary],
  );
  const sharedModulesSummary = useMemo(
    () => getSharedAccessSummary(usersInCurrentSummary.map((user) => getUserAccessSummary(user.modules))),
    [usersInCurrentSummary],
  );
  const sharedRolesValues = useMemo(
    () => getSharedAccessValues(usersInCurrentSummary.map((user) => user.roles)),
    [usersInCurrentSummary],
  );
  const sharedModulesValues = useMemo(
    () => getSharedAccessValues(usersInCurrentSummary.map((user) => user.modules)),
    [usersInCurrentSummary],
  );
  const currentSummaryMissingWhatsAppCount = usersInCurrentSummary.filter((user) => !hasUserWhatsAppChannel(user)).length;
  const currentSummaryMissingContactCount = usersInCurrentSummary.filter(
    (user) => !hasUserWhatsAppChannel(user) && !getUserContactSummary(user),
  ).length;
  const currentSummaryPendingWhatsAppCount = currentSummaryMissingWhatsAppCount - currentSummaryMissingContactCount;
  const currentSummaryWithWhatsAppCount = usersInCurrentSummary.length - currentSummaryMissingWhatsAppCount;
  const showExplicitWhatsAppAction = currentSummaryWithWhatsAppCount === 1;
  const currentSummaryHasMixedPendingContactStates =
    currentSummaryPendingWhatsAppCount > 0 && currentSummaryMissingContactCount > 0;
  const currentSummaryAllNeedContact =
    usersInCurrentSummary.length > 0 && currentSummaryMissingContactCount === usersInCurrentSummary.length;
  const currentSummaryAllNeedWhatsApp =
    usersInCurrentSummary.length > 1 && currentSummaryPendingWhatsAppCount === usersInCurrentSummary.length;
  const hasCurrentSummaryWhatsAppAction = currentSummaryWithWhatsAppCount > 0;
  const showMixedContactStateGuidance =
    (hasCurrentSummaryWhatsAppAction || currentSummaryHasMixedPendingContactStates)
    && (currentSummaryPendingWhatsAppCount > 0 || currentSummaryMissingContactCount > 0);
  const showSharedPendingProfileGuidance = usersInCurrentSummary.length > 1
    && currentSummaryPendingProfileCount > 0
    && (hasCurrentSummaryLinkedProfile || hasActiveSearch);
  const showSharedContactStateGuidance = usersInCurrentSummary.length > 1
    && (showMixedContactStateGuidance || currentSummaryAllNeedContact || currentSummaryAllNeedWhatsApp);
  const showCurrentSummaryContactState =
    usersInCurrentSummary.length > 1
    && (
      currentSummaryWithWhatsAppCount > 0
      || currentSummaryHasMixedPendingContactStates
      || currentSummaryAllNeedContact
      || currentSummaryAllNeedWhatsApp
    )
    && (currentSummaryPendingWhatsAppCount > 0 || currentSummaryMissingContactCount > 0);
  const hideRepeatedPendingStateChips = showSharedContactStateGuidance;
  const hideSingleRowPendingState =
    showSingleSearchResultGuidance
    || showSingleUserGuidance
    || hideRepeatedPendingStateChips;
  const hideRepeatedPendingProfileLabel =
    showSharedPendingProfileGuidance
    || (showGeneralIntro && !hasCurrentSummaryLinkedProfile)
    || Boolean(singleVisibleUser && showSingleUserGuidance && !hasLinkedAdminUserProfile(singleVisibleUser))
    || Boolean(
      singleSearchResult
      && showSingleSearchResultGuidance
      && !hasLinkedAdminUserProfile(singleSearchResult),
    );
  const hideRowAccessSummary = showSingleSearchResultGuidance || showSingleUserGuidance;
  const showSearchEmptyState = hasUsers && visibleUsers.length === 0;
  const hasActiveStatusSearch = hasActiveSearch && isActiveStatusSearchQuery(searchQuery);
  const hasInactiveStatusSearch = hasActiveSearch && isInactiveStatusSearchQuery(searchQuery);
  const hasStatusSearch = hasActiveStatusSearch || hasInactiveStatusSearch;
  const showReviewInactiveSearchEmptyAction =
    showSearchEmptyState && !includeInactive && !hasActiveStatusSearch;
  const reviewInactiveSearchEmptyActionLabel = hasInactiveStatusSearch
    ? ADMIN_USERS_SEARCH_INACTIVE_STATUS_ACTION
    : ADMIN_USERS_SEARCH_EMPTY_INACTIVE_ACTION;
  const hasConfirmedNoInactiveUsers =
    includeInactive
    && hasUsers
    && !hasActiveSearch
    && visibleUsers.length > 0
    && visibleInactiveUsersCount === 0;
  const hasConfirmedNoInactiveSearchMatches =
    includeInactive
    && hasActiveSearch
    && !hasActiveStatusSearch
    && hasUsers
    && visibleUsers.length > 0
    && visibleInactiveUsersCount === 0;
  const showInactiveFilterAction = !showSearchEmptyState
    && !hasStatusSearch
    && !hasConfirmedNoInactiveUsers
    && !hasConfirmedNoInactiveSearchMatches
    && !showOnlyInactiveUsers
    && (hasMultipleUsers || (includeInactive && hasUsers));
  const showReturnToActiveUsersAction = showOnlyInactiveUsers;
  const showReviewInactiveEmptyAction =
    !includeInactive && !usersQuery.isLoading && !usersQuery.error && users.length === 0;
  const singleUserPrimarySetupComplete = Boolean(
    singleVisibleUser
    && hasLinkedAdminUserProfile(singleVisibleUser)
    && getUserContactReadiness(singleVisibleUser) === 'whatsapp-ready'
    && !hasNoAccessAssigned(singleVisibleUser),
  );
  const showReviewInactiveSingleUserAction =
    showSingleUserGuidance
    && singleUserPrimarySetupComplete
    && !includeInactive
    && !usersQuery.isLoading
    && !usersQuery.error;
  const showInlineErrorRetryAction = Boolean(usersQuery.error);
  const showRefreshAction = !usersQuery.error
    && !hasActiveSearch
    && hasUsers
    && !showSearchEmptyState
    && !showOnlyInactiveUsers
    && usersInCurrentSummary.length >= MIN_USERS_FOR_REFRESH;
  const refreshActionTitle = includeInactive
    ? 'Refrescar usuarios activos e inactivos'
    : 'Refrescar usuarios activos';
  const showHeaderActions = !usersQuery.error
    && (showInactiveFilterAction || showRefreshAction || showReturnToActiveUsersAction);
  const showEmptySearchClearAction = showSearchEmptyState && hasActiveSearch;
  const showInlineClearSearchAction = showSearchField && hasActiveSearch && !showEmptySearchClearAction;
  const showActiveScopeSummary = hasMultipleUsers && !includeInactive && !hasActiveSearch;
  const inactiveUsersToggleTarget = formatInactiveUserCountLabel(visibleInactiveUsersCount);
  const collapsedInactiveUsersToggleLabel = useMemo(
    () => buildCollapsedInactiveUsersToggleLabel(inactiveVisibleUsers),
    [inactiveVisibleUsers],
  );
  const collapsedInactiveUsersToggleTitle = useMemo(
    () => buildCollapsedInactiveUsersToggleTitle(inactiveVisibleUsers),
    [inactiveVisibleUsers],
  );
  const expandedInactiveUsersToggleLabel = useMemo(
    () => buildExpandedInactiveUsersToggleLabel(inactiveVisibleUsers),
    [inactiveVisibleUsers],
  );
  const showInactiveUsersGroupLabel = showInactiveUsersList
    && !shouldCollapseInactiveUsers
    && !showInactiveOnlyScopeSummary;
  const showInactiveUsersGroupHeader = showInactiveUsersGroupLabel || shouldCollapseInactiveUsers;
  const usersVisibleForIdentityDisambiguation = useMemo(
    () => (showInactiveUsersList ? visibleUsers : activeVisibleUsers),
    [activeVisibleUsers, showInactiveUsersList, visibleUsers],
  );
  const userIdsRequiringIdentityDisambiguator = useMemo(
    () => getUserIdsRequiringIdentityDisambiguator(usersVisibleForIdentityDisambiguation),
    [usersVisibleForIdentityDisambiguation],
  );
  const userIdsMatchingProfileSearch = useMemo(
    () => (
      hasActiveSearch
        ? new Set(
            visibleUsers
              .filter((user) => matchesLinkedProfileLabelSearchQuery(user, searchQuery))
              .map((user) => user.userId),
          )
        : new Set<number>()
    ),
    [hasActiveSearch, searchQuery, visibleUsers],
  );
  const currentSummaryNoAccessCount = useMemo(
    () => usersInCurrentSummary.filter(hasNoAccessAssigned).length,
    [usersInCurrentSummary],
  );
  const showSharedNoAccessGuidance = usersInCurrentSummary.length > 1
    && currentSummaryNoAccessCount === usersInCurrentSummary.length;
  useEffect(() => {
    if (!showInactiveUsersGroup || hasActiveSearch) {
      setShowInactiveUsers(false);
    }
  }, [hasActiveSearch, showInactiveUsersGroup]);
  useEffect(() => {
    setShowInactiveUsers((current) => (current ? false : current));
  }, [inactiveVisibleUsersSignature]);
  const activeScopeSummary = showActiveScopeSummary
    ? 'Vista actual: solo usuarios activos.'
    : '';
  const showNoInactiveScopeSummary = hasConfirmedNoInactiveUsers;
  const usersErrorMessage = usersQuery.error instanceof Error ? usersQuery.error.message : '';
  const inactiveFilterLabel = hasActiveSearch
    ? includeInactive
      ? INACTIVE_SEARCH_FILTER_ACTIVE_LABEL
      : INCLUDE_INACTIVE_SEARCH_FILTER_LABEL
    : includeInactive
      ? INACTIVE_FILTER_ACTIVE_LABEL
      : INCLUDE_INACTIVE_FILTER_LABEL;
  const inactiveScopeSummary = showNoInactiveScopeSummary
    ? 'No hay usuarios inactivos.'
    : '';
  const inactiveOnlyScopeSummary = (
    showInactiveOnlyScopeSummary
    && !showSingleUserGuidance
    && !showSingleSearchResultGuidance
  )
    ? 'Vista actual: solo usuarios inactivos.'
    : '';
  const inactiveSearchScopeSummary = hasConfirmedNoInactiveSearchMatches
    ? 'Sin coincidencias inactivas para esta búsqueda.'
    : '';
  const searchEmptyStateMessage = showSearchEmptyState
    ? (
      !includeInactive
        ? `No hay coincidencias para "${activeSearchSummary}" entre los usuarios activos.`
        : `No hay coincidencias para "${activeSearchSummary}" entre usuarios activos e inactivos.`
    )
    : '';
  const searchEmptyStateTitle = showSearchEmptyState && activeSearchSummary !== fullSearchSummary
    ? (
      !includeInactive
        ? `No hay coincidencias para "${fullSearchSummary}" entre los usuarios activos.`
        : `No hay coincidencias para "${fullSearchSummary}" entre usuarios activos e inactivos.`
    )
    : undefined;
  const visibleUsersSummary = useMemo(() => {
    if (!hasUsers || showSingleUserGuidance || showSingleSearchResultGuidance || usersInCurrentSummary.length === 0) return '';

    const parts: string[] = [];
    const countLabel =
      (shouldCollapseInactiveUsers && !showInactiveUsersList) || showNoInactiveScopeSummary
        ? formatActiveUserCountLabel(usersInCurrentSummary.length)
        : formatUserCountLabel(usersInCurrentSummary.length);

    if (isFiltered) {
      parts.push(`Mostrando ${usersInCurrentSummary.length} de ${totalUsersCount} usuarios.`);
    } else if (hasActiveSearch && hasMultipleUsers) {
      parts.push(`La búsqueda coincide con los ${countLabel} de esta vista.`);
    } else if (hasMultipleUsers) {
      parts.push(`${countLabel} en esta vista.`);
    }

    if (showSharedPendingProfileGuidance) {
      parts.push(buildPendingProfileSummary(currentSummaryPendingProfileCount));
    }

    if (showCurrentSummaryContactState) {
      parts.push(`${buildContactStateSummary({
        readyForWhatsAppCount: currentSummaryWithWhatsAppCount,
        pendingWhatsAppCount: currentSummaryPendingWhatsAppCount,
        pendingContactCount: currentSummaryMissingContactCount,
      })}.`);
    }

    return parts.join(' ');
  }, [
    hasActiveSearch,
    hasMultipleUsers,
    hasUsers,
    isFiltered,
    currentSummaryMissingContactCount,
    currentSummaryPendingWhatsAppCount,
    currentSummaryWithWhatsAppCount,
    showCurrentSummaryContactState,
    showSharedPendingProfileGuidance,
    showSingleSearchResultGuidance,
    showSingleUserGuidance,
    showNoInactiveScopeSummary,
    shouldCollapseInactiveUsers,
    showInactiveUsersList,
    totalUsersCount,
    currentSummaryPendingProfileCount,
    usersInCurrentSummary.length,
  ]);
  const sharedAccessGuidanceCopy = useMemo(() => {
    const emptyGuidance = { text: '', title: undefined as string | undefined };

    if (showSingleUserGuidance) return emptyGuidance;
    if (showSharedNoAccessGuidance) {
      return {
        text: `Acceso compartido en esta vista: ${NO_ACCESS_ASSIGNED_SUMMARY}.`,
        title: undefined,
      };
    }

    const isDefaultSharedAdminAccess = isDefaultAdminAccessSummary({
      rolesSummary: sharedRolesSummary,
      modulesSummary: sharedModulesSummary,
    });
    if (isDefaultSharedAdminAccess) return emptyGuidance;

    const showSharedRolesSummary = Boolean(sharedRolesSummary)
      && !isSameAccessSummary(sharedRolesSummary, DEFAULT_SHARED_ADMIN_ROLES_SUMMARY);
    const showSharedModulesSummary = Boolean(sharedModulesSummary)
      && !isSameAccessSummary(sharedModulesSummary, DEFAULT_SHARED_ADMIN_MODULES_SUMMARY);
    const fullSharedAccessSummary = formatAccessSummaryParts({
      rolesSummary: showSharedRolesSummary ? sharedRolesSummary : '',
      modulesSummary: showSharedModulesSummary ? sharedModulesSummary : '',
    });

    if (!fullSharedAccessSummary) return emptyGuidance;

    const compactSharedAccessSummary = (
      showSharedRolesSummary
      && showSharedModulesSummary
      && isSameAccessSummary(sharedRolesSummary, sharedModulesSummary)
    )
      ? `Roles y módulos: ${
        sharedRolesValues.length > 0
          ? formatCompactAccessValues(sharedRolesValues, 'acceso', 'accesos')
          : sharedRolesSummary
      }`
      : formatAccessSummaryParts({
        rolesSummary: showSharedRolesSummary
          ? (
            sharedRolesValues.length > 0
              ? formatCompactAccessValues(sharedRolesValues, 'rol', 'roles')
              : sharedRolesSummary
          )
          : '',
        modulesSummary: showSharedModulesSummary
          ? (
            sharedModulesValues.length > 0
              ? formatCompactAccessValues(sharedModulesValues, 'módulo', 'módulos')
              : sharedModulesSummary
          )
          : '',
      });
    const text = `Acceso compartido en esta vista: ${compactSharedAccessSummary}.`;

    return {
      text,
      title: compactSharedAccessSummary === fullSharedAccessSummary
        ? undefined
        : `Acceso compartido en esta vista: ${fullSharedAccessSummary}.`,
    };
  }, [
    sharedModulesSummary,
    sharedModulesValues,
    sharedRolesSummary,
    sharedRolesValues,
    showSharedNoAccessGuidance,
    showSingleUserGuidance,
  ]);
  const sharedAccessGuidance = sharedAccessGuidanceCopy.text;
  const hideAccessSummaryForCurrentRows = hideRowAccessSummary || showSharedNoAccessGuidance;
  const viewGuidance = useMemo(
    () => [
      visibleUsersSummary,
      activeScopeSummary,
      inactiveScopeSummary,
      inactiveOnlyScopeSummary,
      inactiveSearchScopeSummary,
    ]
      .filter(Boolean)
      .join(' '),
    [
      activeScopeSummary,
      inactiveOnlyScopeSummary,
      inactiveScopeSummary,
      inactiveSearchScopeSummary,
      visibleUsersSummary,
    ],
  );
  const generalIntro = hasCurrentSummaryWhatsAppAction
    ? hasCurrentSummaryLinkedProfile
      ? ADMIN_USERS_PAGE_INTRO
      : ADMIN_USERS_PAGE_PROFILE_PENDING_INTRO
    : currentSummaryAllNeedContact
      ? hasCurrentSummaryLinkedProfile
        ? ADMIN_USERS_PAGE_CONTACT_SETUP_INTRO
        : ADMIN_USERS_PAGE_PROFILE_PENDING_CONTACT_SETUP_INTRO
      : hasCurrentSummaryLinkedProfile
        ? ADMIN_USERS_PAGE_NUMBER_SETUP_INTRO
        : ADMIN_USERS_PAGE_PROFILE_PENDING_NUMBER_SETUP_INTRO;
  const baseSingleUserGuidance = singleVisibleUser && !hasLinkedAdminUserProfile(singleVisibleUser)
    ? buildPendingProfileGuidance({
        scope: 'single-user',
        readiness: singleVisibleUserReadiness ?? 'missing-contact',
      })
    : singleVisibleUserReadiness === 'whatsapp-ready'
      ? SINGLE_USER_GUIDANCE
      : singleVisibleUserReadiness === 'contact-ready'
        ? SINGLE_USER_NUMBER_SETUP_GUIDANCE
        : SINGLE_USER_CONTACT_SETUP_GUIDANCE;
  const singleUserGuidance = showOnlyInactiveUsers
    ? makeSingleUserInactiveGuidance(baseSingleUserGuidance)
    : baseSingleUserGuidance;
  const baseSingleSearchResultGuidance = singleSearchResult && !hasLinkedAdminUserProfile(singleSearchResult)
    ? buildPendingProfileGuidance({
        scope: 'single-result',
        readiness: singleSearchResultReadiness ?? 'missing-contact',
      })
    : singleSearchResultReadiness === 'whatsapp-ready'
      ? SINGLE_SEARCH_RESULT_GUIDANCE
      : singleSearchResultReadiness === 'contact-ready'
        ? SINGLE_SEARCH_RESULT_NUMBER_SETUP_GUIDANCE
        : SINGLE_SEARCH_RESULT_CONTACT_SETUP_GUIDANCE;
  const singleSearchResultGuidance = showSingleSearchResultGuidance
    ? showOnlyInactiveSearchResults
      ? makeSingleSearchResultInactiveGuidance(baseSingleSearchResultGuidance)
      : baseSingleSearchResultGuidance
    : '';
  const singleSearchResultAccessSummary = useMemo(
    () => buildNonDefaultUserAccessSummaryCopy(singleSearchResult),
    [singleSearchResult],
  );
  const singleUserAccessSummary = useMemo(
    () => buildNonDefaultUserAccessSummaryCopy(singleVisibleUser),
    [singleVisibleUser],
  );
  const primaryGuidance = showSingleUserGuidance
    ? singleUserGuidance
    : showSingleSearchResultGuidance
      ? singleSearchResultGuidance
    : showGeneralIntro
      ? generalIntro
      : '';
  const pageGuidanceParts = [
    primaryGuidance,
    viewGuidance,
    singleUserAccessSummary.text ? `Acceso de este usuario: ${singleUserAccessSummary.text}.` : '',
    singleSearchResultAccessSummary.text ? `Acceso en este resultado: ${singleSearchResultAccessSummary.text}.` : '',
    sharedAccessGuidance,
  ].filter(Boolean);
  const pageGuidance = pageGuidanceParts.join(' ');
  const hasExpandedGuidanceTitle = Boolean(
    singleUserAccessSummary.title
    || singleSearchResultAccessSummary.title
    || sharedAccessGuidanceCopy.title,
  );
  const pageGuidanceTitle = hasExpandedGuidanceTitle
    ? [
        primaryGuidance,
        viewGuidance,
        singleUserAccessSummary.text
          ? `Acceso de este usuario: ${singleUserAccessSummary.title ?? singleUserAccessSummary.text}.`
          : '',
        singleSearchResultAccessSummary.text
          ? `Acceso en este resultado: ${singleSearchResultAccessSummary.title ?? singleSearchResultAccessSummary.text}.`
          : '',
        sharedAccessGuidanceCopy.title ?? sharedAccessGuidance,
      ].filter(Boolean).join(' ')
    : undefined;

  return (
    <>
      <Stack spacing={3}>
        <Stack spacing={1}>
          <Stack
            direction={{ xs: 'column', lg: 'row' }}
            justifyContent="space-between"
            alignItems={{ xs: 'stretch', lg: 'center' }}
            spacing={2}
          >
            <Stack spacing={1} sx={{ minWidth: 0, flex: '1 1 360px' }}>
              <Typography variant="h4" fontWeight={700}>{ADMIN_USERS_PAGE_TITLE}</Typography>
              {showSearchField && (
                <TextField
                  label="Buscar usuarios"
                  value={searchQuery}
                  onChange={(event) => setSearchQuery(normalizeVisibleSearchInput(event.target.value))}
                  size="small"
                  fullWidth
                  placeholder={searchInputPlaceholder}
                  InputProps={{
                    endAdornment: showInlineClearSearchAction ? (
                      <InputAdornment position="end">
                        <Tooltip title="Limpiar búsqueda">
                          <IconButton
                            edge="end"
                            size="small"
                            aria-label="Limpiar búsqueda"
                            onClick={handleClearSearch}
                          >
                            <ClearIcon fontSize="small" />
                          </IconButton>
                        </Tooltip>
                      </InputAdornment>
                    ) : null,
                  }}
                />
              )}
              {(pageGuidance || showReviewInactiveSingleUserAction) && (
                <Stack spacing={0.75} alignItems="flex-start">
                  {pageGuidance && (
                    <Typography
                      data-testid="admin-users-page-guidance"
                      variant="body2"
                      color="text.secondary"
                      title={pageGuidanceTitle}
                    >
                      {pageGuidance}
                    </Typography>
                  )}
                  {showReviewInactiveSingleUserAction && (
                    <Button size="small" variant="text" onClick={() => setIncludeInactive(true)}>
                      {ADMIN_USERS_REVIEW_INACTIVE_SINGLE_USER_ACTION}
                    </Button>
                  )}
                </Stack>
              )}
            </Stack>
            {showHeaderActions && (
              <Stack
                data-testid="admin-users-header-actions"
                direction="row"
                spacing={1}
                alignItems="center"
                flexWrap="wrap"
                useFlexGap
              >
                {showInactiveFilterAction && (
                  <FormControlLabel
                    control={(
                      <Checkbox
                        checked={includeInactive}
                        onChange={(event) => setIncludeInactive(event.target.checked)}
                      />
                    )}
                    label={inactiveFilterLabel}
                  />
                )}
                {showReturnToActiveUsersAction && (
                  <Button size="small" variant="outlined" onClick={() => setIncludeInactive(false)}>
                    {RETURN_TO_ACTIVE_USERS_ACTION}
                  </Button>
                )}
                {showRefreshAction && (
                  <Tooltip title={refreshActionTitle}>
                    <span>
                      <IconButton
                        aria-label="Refrescar lista de usuarios"
                        title={refreshActionTitle}
                        onClick={handleRefresh}
                        disabled={usersQuery.isFetching}
                      >
                        <RefreshIcon />
                      </IconButton>
                    </span>
                  </Tooltip>
                )}
              </Stack>
            )}
          </Stack>
        </Stack>

        <Card>
          <CardContent>
            {usersQuery.isLoading && <Typography>Cargando usuarios…</Typography>}
            {usersQuery.error && (
              <Stack spacing={1} alignItems="flex-start">
                <Typography color="error">
                  No se pudieron cargar los usuarios{usersErrorMessage ? `: ${usersErrorMessage}` : ''}.
                </Typography>
                {showInlineErrorRetryAction && (
                  <Button size="small" variant="outlined" onClick={handleRefresh} disabled={usersQuery.isFetching}>
                    Reintentar usuarios
                  </Button>
                )}
              </Stack>
            )}
            {!usersQuery.isLoading && !usersQuery.error && users.length === 0 && (
              <Stack spacing={1} alignItems="flex-start">
                <Typography color="text.secondary">
                  {includeInactive ? ADMIN_USERS_EMPTY_WITH_INACTIVE_STATE : ADMIN_USERS_EMPTY_STATE}
                </Typography>
                {showReviewInactiveEmptyAction && (
                  <Button size="small" variant="outlined" onClick={() => setIncludeInactive(true)}>
                    {ADMIN_USERS_REVIEW_INACTIVE_EMPTY_ACTION}
                  </Button>
                )}
              </Stack>
            )}
            {showSearchEmptyState ? (
              <Stack spacing={1} alignItems="flex-start">
                <Typography color="text.secondary" title={searchEmptyStateTitle}>
                  {searchEmptyStateMessage}
                </Typography>
                {(showEmptySearchClearAction || showReviewInactiveSearchEmptyAction) && (
                  <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                    {showEmptySearchClearAction && (
                      <Button size="small" variant="outlined" onClick={handleClearSearch}>
                        Limpiar búsqueda
                      </Button>
                    )}
                    {showReviewInactiveSearchEmptyAction && (
                      <Button
                        size="small"
                        variant="outlined"
                        onClick={() => setIncludeInactive(true)}
                      >
                        {reviewInactiveSearchEmptyActionLabel}
                      </Button>
                    )}
                  </Stack>
                )}
              </Stack>
            ) : null}
            {visibleUsers.length ? (
              <Stack spacing={1.5}>
                {activeVisibleUsers.map((user) => (
                  <UserRow
                    key={user.userId}
                    user={user}
                    showInactiveStatusChip={includeInactive && !user.active && !showInactiveOnlyScopeSummary}
                    onOpenCommunications={() => setSelectedUser(user)}
                    sharedModulesSummary={sharedModulesSummary}
                    sharedRolesSummary={sharedRolesSummary}
                    hideAccessSummary={hideAccessSummaryForCurrentRows}
                    hidePendingStateChip={hideSingleRowPendingState}
                    hidePendingProfileLabel={hideRepeatedPendingProfileLabel}
                    showIdentityDisambiguator={
                      userIdsRequiringIdentityDisambiguator.has(user.userId)
                      || userIdsMatchingProfileSearch.has(user.userId)
                    }
                    showExplicitWhatsAppAction={showExplicitWhatsAppAction}
                  />
                ))}
                {showInactiveUsersGroup ? (
                  <>
                    {showInactiveUsersGroupHeader && (
                      <Stack
                        data-testid="admin-users-inactive-group-header"
                        direction={{ xs: 'column', sm: 'row' }}
                        spacing={1}
                        justifyContent="space-between"
                        alignItems={{ xs: 'flex-start', sm: 'center' }}
                      >
                        {showInactiveUsersGroupLabel && (
                          <Typography
                            data-testid="admin-users-inactive-group-label"
                            variant="overline"
                            color="text.secondary"
                          >
                            {formatInactiveUserCountLabel(visibleInactiveUsersCount)}
                          </Typography>
                        )}
                        {shouldCollapseInactiveUsers && (
                          <Button
                            size="small"
                            variant="text"
                            onClick={() => setShowInactiveUsers((current) => !current)}
                            aria-controls="admin-users-inactive-list"
                            aria-expanded={showInactiveUsers}
                            aria-label={showInactiveUsers
                              ? `Ocultar ${inactiveUsersToggleTarget}`
                              : `Ver ${inactiveUsersToggleTarget}`}
                            title={showInactiveUsers ? undefined : collapsedInactiveUsersToggleTitle}
                          >
                            {showInactiveUsers
                              ? expandedInactiveUsersToggleLabel
                              : collapsedInactiveUsersToggleLabel}
                          </Button>
                        )}
                      </Stack>
                    )}
                    {showInactiveUsersList && (
                      <Stack id="admin-users-inactive-list" spacing={1.5}>
                        {inactiveVisibleUsers.map((user) => (
                          <UserRow
                            key={user.userId}
                            user={user}
                            showInactiveStatusChip={false}
                            onOpenCommunications={() => setSelectedUser(user)}
                            sharedModulesSummary={sharedModulesSummary}
                            sharedRolesSummary={sharedRolesSummary}
                            hideAccessSummary={hideAccessSummaryForCurrentRows}
                            hidePendingStateChip={hideSingleRowPendingState}
                            hidePendingProfileLabel={hideRepeatedPendingProfileLabel}
                            showIdentityDisambiguator={
                              userIdsRequiringIdentityDisambiguator.has(user.userId)
                              || userIdsMatchingProfileSearch.has(user.userId)
                            }
                            showExplicitWhatsAppAction={showExplicitWhatsAppAction}
                          />
                        ))}
                      </Stack>
                    )}
                  </>
                ) : null}
              </Stack>
            ) : null}
          </CardContent>
        </Card>
      </Stack>
      <AdminUserCommunicationDialog
        open={Boolean(selectedUser)}
        user={selectedUser}
        onClose={() => setSelectedUser(null)}
      />
    </>
  );
}

function UserRow({
  user,
  showInactiveStatusChip,
  onOpenCommunications,
  sharedRolesSummary,
  sharedModulesSummary,
  hideAccessSummary,
  hidePendingStateChip,
  hidePendingProfileLabel,
  showIdentityDisambiguator,
  showExplicitWhatsAppAction,
}: {
  user: AdminUser;
  showInactiveStatusChip: boolean;
  onOpenCommunications: () => void;
  sharedRolesSummary: string;
  sharedModulesSummary: string;
  hideAccessSummary: boolean;
  hidePendingStateChip: boolean;
  hidePendingProfileLabel: boolean;
  showIdentityDisambiguator: boolean;
  showExplicitWhatsAppAction: boolean;
}) {
  const contactSummary = getUserContactSummary(user);
  const visibleContactSummary = getVisibleUserContactSummary(user);
  const hasContactInfo = Boolean(contactSummary);
  const hasWhatsAppChannel = hasUserWhatsAppChannel(user);
  const fullAccessSummary = buildUserAccessSummary({
    roles: user.roles,
    modules: user.modules,
    sharedRolesSummary,
    sharedModulesSummary,
  });
  const accessSummary = buildUserRowAccessSummary({
    roles: user.roles,
    modules: user.modules,
    sharedRolesSummary,
    sharedModulesSummary,
  });
  const identity = summarizeUserIdentity(user);
  const hasLinkedProfile = hasLinkedAdminUserProfile(user);
  const identityDisambiguator = hasLinkedProfile ? `Perfil #${user.partyId}` : `Cuenta #${user.userId}`;
  const visibleIdentityDisambiguator = showIdentityDisambiguator ? identityDisambiguator : '';
  const identityTarget = identity.secondary ? `${identity.primary} (${identity.secondary})` : identity.primary;
  const communicationTarget = visibleIdentityDisambiguator
    ? `${identityTarget} · ${visibleIdentityDisambiguator}`
    : identityTarget;
  const whatsappActionLabel = `Abrir WhatsApp para ${communicationTarget}`;
  const whatsappChannel = getUserWhatsAppChannel(user);
  const whatsappActionTitle = whatsappChannel ? `${whatsappActionLabel} · ${whatsappChannel}` : whatsappActionLabel;
  const profilePath = hasLinkedProfile ? `/perfil/${user.partyId}` : null;
  const missingChannelLabel = hasContactInfo ? 'WhatsApp pendiente' : 'Contacto pendiente';
  const profileActionLabel = visibleIdentityDisambiguator
    ? `Abrir perfil de ${identity.primary} (${visibleIdentityDisambiguator})`
    : `Abrir perfil de ${identity.primary}`;
  const showCommunicationActions = hasWhatsAppChannel || !hidePendingStateChip;

  return (
    <Box
      data-testid={`admin-user-row-${user.userId}`}
      sx={{
        border: '1px solid rgba(148,163,184,0.3)',
        borderRadius: 2,
        p: 1.5,
        display: 'flex',
        flexWrap: 'wrap',
        gap: 1,
        alignItems: 'center',
      }}
    >
      <Box sx={{ minWidth: 180 }}>
        {profilePath ? (
          <Link
            component={RouterLink}
            to={profilePath}
            underline="hover"
            color="primary"
            variant="subtitle1"
            aria-label={profileActionLabel}
            sx={{ display: 'inline-flex', width: 'fit-content', fontWeight: 700 }}
          >
            {identity.primary}
          </Link>
        ) : (
          <Typography variant="subtitle1" fontWeight={700}>
            {identity.primary}
          </Typography>
        )}
        {identity.secondary && (
          <Typography variant="body2" color="text.secondary">
            {identity.secondary}
          </Typography>
        )}
        {showIdentityDisambiguator && (
          <Typography variant="caption" color="text.secondary">
            {identityDisambiguator}
          </Typography>
        )}
        {!hasLinkedProfile && !hidePendingProfileLabel && (
          <Typography variant="caption" color="text.secondary">
            Perfil pendiente
          </Typography>
        )}
        {visibleContactSummary && (
          <Typography variant="body2" color="text.secondary">
            {visibleContactSummary}
          </Typography>
        )}
      </Box>
      {showInactiveStatusChip && (
        <Chip label="Inactivo" color="default" size="small" />
      )}
      {!hideAccessSummary && accessSummary && (
        <Box sx={{ minWidth: 220, flex: '1 1 240px' }}>
          <Typography
            variant="body2"
            color="text.secondary"
            title={accessSummary === fullAccessSummary ? undefined : fullAccessSummary}
          >
            {accessSummary}
          </Typography>
        </Box>
      )}
      {showCommunicationActions && (
        <Stack
          data-testid={`admin-user-actions-${user.userId}`}
          direction="row"
          spacing={1}
          sx={{ ml: 'auto' }}
        >
          {hasWhatsAppChannel ? (
            showExplicitWhatsAppAction ? (
              <Button
                size="small"
                variant="outlined"
                color="primary"
                aria-label={whatsappActionLabel}
                title={whatsappActionTitle}
                startIcon={<WhatsAppIcon fontSize="small" />}
                onClick={onOpenCommunications}
              >
                WhatsApp
              </Button>
            ) : (
              <Tooltip title={whatsappActionTitle} describeChild>
                <IconButton
                  size="small"
                  color="primary"
                  aria-label={whatsappActionLabel}
                  onClick={onOpenCommunications}
                  sx={{
                    border: '1px solid',
                    borderColor: 'primary.main',
                  }}
                >
                  <WhatsAppIcon fontSize="small" />
                </IconButton>
              </Tooltip>
            )
          ) : (
            <Chip label={missingChannelLabel} color="warning" variant="outlined" size="small" />
          )}
        </Stack>
      )}
    </Box>
  );
}
