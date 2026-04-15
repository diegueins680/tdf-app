import { useDeferredValue, useMemo, useState } from 'react';
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
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Link as RouterLink } from 'react-router-dom';
import { Admin, type AdminUser } from '../api/admin';
import AdminUserCommunicationDialog from '../components/AdminUserCommunicationDialog';

const normalizeContactValue = (value?: string | null) => {
  const trimmed = value?.trim();
  if (trimmed == null || trimmed === '') return null;
  return trimmed;
};

const getUserContactSummary = (user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail'>) => {
  const preferredPhone = normalizeContactValue(user.whatsapp) ?? normalizeContactValue(user.primaryPhone);
  const email = normalizeContactValue(user.primaryEmail);

  if (preferredPhone && email) return `${preferredPhone} · ${email}`;
  return preferredPhone ?? email;
};

const getUserWhatsAppChannel = (user: Pick<AdminUser, 'whatsapp' | 'primaryPhone'>) =>
  normalizeContactValue(user.whatsapp) ?? normalizeContactValue(user.primaryPhone);

const hasUserWhatsAppChannel = (user: Pick<AdminUser, 'whatsapp' | 'primaryPhone'>) =>
  Boolean(getUserWhatsAppChannel(user));

const getUserContactReadiness = (
  user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail'>,
) => {
  if (hasUserWhatsAppChannel(user)) return 'whatsapp-ready' as const;
  if (getUserContactSummary(user)) return 'contact-ready' as const;
  return 'missing-contact' as const;
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

const getUserAccessSummary = (values: string[]) =>
  Array.from(new Set(values.map((value) => value.trim()).filter(Boolean)))
    .sort((left, right) => left.localeCompare(right))
    .join(', ');

const getSharedAccessSummary = (values: string[]) => {
  if (values.length < 2) return '';
  const normalizedValues = values.map((value) => value.trim());
  if (normalizedValues.some((value) => value === '')) return '';
  const [firstValue, ...rest] = normalizedValues;
  return rest.every((value) => value === firstValue) ? (firstValue ?? '') : '';
};

const normalizeSearchValue = (value: string) => value.trim().toLowerCase();
const hasLinkedAdminUserProfile = (user: Pick<AdminUser, 'partyId'>) =>
  typeof user.partyId === 'number' && Number.isInteger(user.partyId) && user.partyId > 0;

const formatUserCountLabel = (count: number) => `${count} usuario${count === 1 ? '' : 's'}`;
const formatInactiveUserCountLabel = (count: number) => `${formatUserCountLabel(count)} inactivo${count === 1 ? '' : 's'}`;
const MIN_USERS_FOR_SEARCH = 3;
const SEARCH_THRESHOLD_GUIDANCE = 'La búsqueda aparecerá desde el tercer usuario.';
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
const SINGLE_USER_GUIDANCE =
  'Solo hay un usuario por ahora. Abre su perfil desde el nombre y usa WhatsApp si ya tiene un número disponible. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.';
const SINGLE_USER_NUMBER_SETUP_GUIDANCE =
  'Solo hay un usuario por ahora. Abre su perfil desde el nombre para agregar o corregir un número. Cuando tenga un número disponible, WhatsApp aparecerá aquí. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.';
const SINGLE_USER_CONTACT_SETUP_GUIDANCE =
  'Solo hay un usuario por ahora. Abre su perfil desde el nombre para completar el contacto pendiente. Cuando tenga un número disponible, WhatsApp aparecerá aquí. Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.';
const SINGLE_SEARCH_RESULT_GUIDANCE =
  'Resultado único. Abre el perfil desde el nombre y usa WhatsApp si ya está disponible.';
const SINGLE_SEARCH_RESULT_NUMBER_SETUP_GUIDANCE =
  'Resultado único. Abre el perfil desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible.';
const SINGLE_SEARCH_RESULT_CONTACT_SETUP_GUIDANCE =
  'Resultado único. Abre el perfil desde el nombre para completar el contacto pendiente. WhatsApp aparecerá cuando haya un número disponible.';

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
  const scopeSuffix =
    scope === 'single-user'
      ? ' Cuando la lista crezca, aquí aparecerán búsqueda y resumen de resultados.'
      : '';
  const missingProfileMessage =
    ' Este usuario todavía no tiene un perfil vinculado, así que el nombre no abre un perfil.';

  if (readiness === 'whatsapp-ready') {
    return `${scopePrefix}${missingProfileMessage} Usa WhatsApp si ya tiene un número disponible.${scopeSuffix}`;
  }

  if (readiness === 'contact-ready') {
    return `${scopePrefix}${missingProfileMessage} Cuando se vincule, podrás abrirlo desde el nombre para agregar o corregir un número. WhatsApp aparecerá cuando haya un número disponible.${scopeSuffix}`;
  }

  return `${scopePrefix}${missingProfileMessage} Cuando se vincule, podrás abrirlo desde el nombre para completar el contacto pendiente. WhatsApp aparecerá cuando haya un número disponible.${scopeSuffix}`;
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
  const showRolesSummary = Boolean(rolesSummary) && rolesSummary !== sharedRolesSummary;
  const showModulesSummary = Boolean(modulesSummary) && modulesSummary !== sharedModulesSummary;

  return [
    showRolesSummary ? `Roles: ${rolesSummary}` : '',
    showModulesSummary ? `Módulos: ${modulesSummary}` : '',
  ].filter(Boolean).join(' · ');
};

const isDefaultAdminAccessSummary = ({
  modulesSummary,
  rolesSummary,
}: {
  modulesSummary: string;
  rolesSummary: string;
}) => (
  rolesSummary === DEFAULT_SHARED_ADMIN_ROLES_SUMMARY
  && modulesSummary === DEFAULT_SHARED_ADMIN_MODULES_SUMMARY
);

const summarizeUserIdentity = (user: Pick<AdminUser, 'partyName' | 'username'>) => {
  const displayName = user.partyName.trim();
  const username = user.username.trim();
  const primary = displayName || username;
  const showUsername = displayName !== '' && displayName.toLowerCase() !== username.toLowerCase();
  const secondaryParts: string[] = [];

  if (showUsername) secondaryParts.push(`Usuario: ${username}`);

  return {
    primary,
    secondary: secondaryParts.join(' · '),
  };
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
  const seenUserIds = new Set<number>();

  return users.filter((user) => {
    if (seenUserIds.has(user.userId)) return false;
    seenUserIds.add(user.userId);
    return true;
  });
};

const matchesUserQuery = (user: AdminUser, rawQuery: string) => {
  const query = normalizeSearchValue(rawQuery);
  if (!query) return true;
  const partyIdSearchSpace = hasLinkedAdminUserProfile(user)
    ? [String(user.partyId), `id ${user.partyId}`]
    : [];

  const searchSpace = [
    user.username,
    user.partyName,
    String(user.userId),
    ...partyIdSearchSpace,
    getUserContactSummary(user) ?? '',
    getUserAccessSummary(user.roles),
    getUserAccessSummary(user.modules),
    user.active ? 'activo' : 'inactivo',
  ]
    .map(normalizeSearchValue)
    .filter(Boolean);

  return searchSpace.some((value) => value.includes(query));
};

export default function AdminUsersPage() {
  const qc = useQueryClient();
  const [includeInactive, setIncludeInactive] = useState(false);
  const [selectedUser, setSelectedUser] = useState<AdminUser | null>(null);
  const [searchQuery, setSearchQuery] = useState('');
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
  const visibleUsersMissingWhatsAppCount = useMemo(
    () => visibleUsers.filter((user) => !hasUserWhatsAppChannel(user)).length,
    [visibleUsers],
  );
  const visibleUsersMissingContactCount = useMemo(
    () => visibleUsers.filter((user) => !hasUserWhatsAppChannel(user) && !getUserContactSummary(user)).length,
    [visibleUsers],
  );
  const visibleInactiveUsersCount = useMemo(
    () => visibleUsers.filter((user) => !user.active).length,
    [visibleUsers],
  );
  const visibleUsersPendingProfileCount = useMemo(
    () => visibleUsers.filter((user) => !hasLinkedAdminUserProfile(user)).length,
    [visibleUsers],
  );
  const visibleUsersPendingWhatsAppCount = visibleUsersMissingWhatsAppCount - visibleUsersMissingContactCount;
  const hasVisibleLinkedProfile = visibleUsers.some((user) => hasLinkedAdminUserProfile(user));
  const sharedRolesSummary = useMemo(
    () => getSharedAccessSummary(visibleUsers.map((user) => getUserAccessSummary(user.roles))),
    [visibleUsers],
  );
  const sharedModulesSummary = useMemo(
    () => getSharedAccessSummary(visibleUsers.map((user) => getUserAccessSummary(user.modules))),
    [visibleUsers],
  );
  const visibleUsersWithWhatsAppCount = visibleUsers.length - visibleUsersMissingWhatsAppCount;
  const totalUsersCount = users.length;
  const hasUsers = totalUsersCount > 0;
  const hasActiveSearch = normalizeSearchValue(searchQuery).length > 0;
  const activeSearchSummary = searchQuery.trim();
  const hasMultipleUsers = totalUsersCount > 1;
  const showGeneralIntro = hasMultipleUsers && !hasActiveSearch;
  const showSingleUserGuidance = totalUsersCount === 1 && !hasActiveSearch;
  const singleVisibleUser = showSingleUserGuidance ? (visibleUsers[0] ?? null) : null;
  const singleVisibleUserReadiness = singleVisibleUser ? getUserContactReadiness(singleVisibleUser) : null;
  const visibleUsersAllNeedContact = visibleUsers.length > 0 && visibleUsersMissingContactCount === visibleUsers.length;
  const visibleUsersAllNeedWhatsApp =
    visibleUsers.length > 1
    && visibleUsersPendingWhatsAppCount === visibleUsers.length;
  const hasVisibleWhatsAppAction = visibleUsersWithWhatsAppCount > 0;
  const isFiltered = hasActiveSearch && visibleUsers.length !== totalUsersCount;
  const showSearchField = totalUsersCount >= MIN_USERS_FOR_SEARCH || hasActiveSearch;
  const showSingleSearchResultGuidance = hasActiveSearch && visibleUsers.length === 1;
  const singleSearchResult = showSingleSearchResultGuidance ? (visibleUsers[0] ?? null) : null;
  const singleSearchResultReadiness = singleSearchResult ? getUserContactReadiness(singleSearchResult) : null;
  const showMixedContactStateGuidance = hasVisibleWhatsAppAction
    && (visibleUsersPendingWhatsAppCount > 0 || visibleUsersMissingContactCount > 0);
  const showSharedPendingProfileGuidance = visibleUsers.length > 1
    && visibleUsersPendingProfileCount > 1
    && (hasVisibleLinkedProfile || hasActiveSearch);
  const showSharedContactStateGuidance = visibleUsers.length > 1
    && (showMixedContactStateGuidance || visibleUsersAllNeedContact || visibleUsersAllNeedWhatsApp);
  const showInactiveUsersGroup = includeInactive && visibleInactiveUsersCount > 1;
  const hideRepeatedPendingStateChips = showSharedContactStateGuidance;
  const hideSingleRowPendingState =
    showSingleSearchResultGuidance
    || showSingleUserGuidance
    || hideRepeatedPendingStateChips;
  const hideRepeatedPendingProfileLabel =
    showSharedPendingProfileGuidance
    || (showGeneralIntro && !hasVisibleLinkedProfile)
    || Boolean(singleVisibleUser && showSingleUserGuidance && !hasLinkedAdminUserProfile(singleVisibleUser))
    || Boolean(
      singleSearchResult
      && showSingleSearchResultGuidance
      && !hasLinkedAdminUserProfile(singleSearchResult),
    );
  const hideRowAccessSummary = showSingleSearchResultGuidance || showSingleUserGuidance;
  const showSearchEmptyState = hasUsers && visibleUsers.length === 0;
  const showRefreshAction = Boolean(usersQuery.error) || (!hasActiveSearch && hasUsers && !showSearchEmptyState);
  const showInlineClearSearchAction = showSearchField && hasActiveSearch;
  const showActiveScopeSummary = hasMultipleUsers && !includeInactive && !hasActiveSearch;
  const showSearchThresholdGuidance = !showSearchField && totalUsersCount === MIN_USERS_FOR_SEARCH - 1;
  const activeVisibleUsers = useMemo(
    () => (showInactiveUsersGroup ? visibleUsers.filter((user) => user.active) : visibleUsers),
    [showInactiveUsersGroup, visibleUsers],
  );
  const inactiveVisibleUsers = useMemo(
    () => (showInactiveUsersGroup ? visibleUsers.filter((user) => !user.active) : []),
    [showInactiveUsersGroup, visibleUsers],
  );
  const activeScopeSummary = showActiveScopeSummary
    ? 'Vista actual: solo usuarios activos.'
    : '';
  const searchEmptyStateMessage = showSearchEmptyState
    ? (
      !includeInactive
        ? `No hay coincidencias para "${activeSearchSummary}" entre los usuarios activos. Activa Incluir inactivos si necesitas revisar cuentas deshabilitadas.`
        : `No hay coincidencias para "${activeSearchSummary}".`
    )
    : '';
  const visibleUsersSummary = useMemo(() => {
    if (!hasUsers || showSingleUserGuidance || showSingleSearchResultGuidance || visibleUsers.length === 0) return '';

    const parts: string[] = [];

    if (isFiltered) {
      parts.push(`Mostrando ${visibleUsers.length} de ${totalUsersCount} usuarios.`);
    } else if (hasMultipleUsers) {
      parts.push(`${formatUserCountLabel(visibleUsers.length)} en esta vista.`);
    }

    if (showSharedPendingProfileGuidance) {
      parts.push(buildPendingProfileSummary(visibleUsersPendingProfileCount));
    }

    if (showMixedContactStateGuidance) {
      parts.push(`${buildContactStateSummary({
        readyForWhatsAppCount: visibleUsersWithWhatsAppCount,
        pendingWhatsAppCount: visibleUsersPendingWhatsAppCount,
        pendingContactCount: visibleUsersMissingContactCount,
      })}.`);
    }

    return parts.join(' ');
  }, [
    hasMultipleUsers,
    hasUsers,
    isFiltered,
    showSharedPendingProfileGuidance,
    showMixedContactStateGuidance,
    showSingleSearchResultGuidance,
    showSingleUserGuidance,
    totalUsersCount,
    visibleUsers.length,
    visibleUsersMissingContactCount,
    visibleUsersPendingProfileCount,
    visibleUsersPendingWhatsAppCount,
    visibleUsersWithWhatsAppCount,
  ]);
  const sharedAccessGuidance = useMemo(() => {
    if (showSingleUserGuidance) return '';

    const isDefaultSharedAdminAccess = isDefaultAdminAccessSummary({
      rolesSummary: sharedRolesSummary,
      modulesSummary: sharedModulesSummary,
    });
    if (isDefaultSharedAdminAccess) return '';

    const sharedAccessSummaryParts: string[] = [];
    if (sharedRolesSummary) sharedAccessSummaryParts.push(`Roles: ${sharedRolesSummary}`);
    if (sharedModulesSummary) sharedAccessSummaryParts.push(`Módulos: ${sharedModulesSummary}`);
    return sharedAccessSummaryParts.length
      ? `Acceso compartido en esta vista: ${sharedAccessSummaryParts.join(' · ')}.`
      : '';
  }, [sharedModulesSummary, sharedRolesSummary, showSingleUserGuidance]);
  const viewGuidance = useMemo(
    () => [visibleUsersSummary, showSearchThresholdGuidance ? SEARCH_THRESHOLD_GUIDANCE : '', activeScopeSummary]
      .filter(Boolean)
      .join(' '),
    [activeScopeSummary, showSearchThresholdGuidance, visibleUsersSummary],
  );
  const generalIntro = hasVisibleWhatsAppAction
    ? hasVisibleLinkedProfile
      ? ADMIN_USERS_PAGE_INTRO
      : ADMIN_USERS_PAGE_PROFILE_PENDING_INTRO
    : visibleUsersAllNeedContact
      ? hasVisibleLinkedProfile
        ? ADMIN_USERS_PAGE_CONTACT_SETUP_INTRO
        : ADMIN_USERS_PAGE_PROFILE_PENDING_CONTACT_SETUP_INTRO
      : hasVisibleLinkedProfile
        ? ADMIN_USERS_PAGE_NUMBER_SETUP_INTRO
        : ADMIN_USERS_PAGE_PROFILE_PENDING_NUMBER_SETUP_INTRO;
  const singleUserGuidance = singleVisibleUser && !hasLinkedAdminUserProfile(singleVisibleUser)
    ? buildPendingProfileGuidance({
        scope: 'single-user',
        readiness: singleVisibleUserReadiness ?? 'missing-contact',
      })
    : singleVisibleUserReadiness === 'whatsapp-ready'
      ? SINGLE_USER_GUIDANCE
      : singleVisibleUserReadiness === 'contact-ready'
        ? SINGLE_USER_NUMBER_SETUP_GUIDANCE
        : SINGLE_USER_CONTACT_SETUP_GUIDANCE;
  const singleSearchResultGuidance = showSingleSearchResultGuidance
    ? (
      singleSearchResult && !hasLinkedAdminUserProfile(singleSearchResult)
        ? buildPendingProfileGuidance({
            scope: 'single-result',
            readiness: singleSearchResultReadiness ?? 'missing-contact',
          })
        : singleSearchResultReadiness === 'whatsapp-ready'
          ? SINGLE_SEARCH_RESULT_GUIDANCE
          : singleSearchResultReadiness === 'contact-ready'
            ? SINGLE_SEARCH_RESULT_NUMBER_SETUP_GUIDANCE
            : SINGLE_SEARCH_RESULT_CONTACT_SETUP_GUIDANCE
    )
    : '';
  const singleSearchResultAccessSummary = useMemo(
    () => {
      if (!singleSearchResult) return '';

      const rolesSummary = getUserAccessSummary(singleSearchResult.roles);
      const modulesSummary = getUserAccessSummary(singleSearchResult.modules);

      if (isDefaultAdminAccessSummary({ rolesSummary, modulesSummary })) {
        return '';
      }

      return buildUserAccessSummary({
        roles: singleSearchResult.roles,
        modules: singleSearchResult.modules,
      });
    },
    [singleSearchResult],
  );
  const primaryGuidance = showSingleUserGuidance
    ? singleUserGuidance
    : showSingleSearchResultGuidance
      ? singleSearchResultGuidance
    : showGeneralIntro
      ? generalIntro
      : '';
  const pageGuidance = [
    primaryGuidance,
    viewGuidance,
    singleSearchResultAccessSummary ? `Acceso en este resultado: ${singleSearchResultAccessSummary}.` : '',
    sharedAccessGuidance,
  ].filter(Boolean).join(' ');

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
              <Typography variant="h4" fontWeight={700}>Usuarios</Typography>
              {showSearchField && (
                <TextField
                  label="Buscar usuarios"
                  value={searchQuery}
                  onChange={(event) => setSearchQuery(event.target.value)}
                  size="small"
                  fullWidth
                  placeholder="Usuario, nombre, ID, contacto o acceso"
                  InputProps={{
                    endAdornment: showInlineClearSearchAction ? (
                      <InputAdornment position="end">
                        <Button
                          size="small"
                          aria-label="Limpiar búsqueda"
                          onClick={handleClearSearch}
                          sx={{ minWidth: 0, px: 0.5, textTransform: 'none' }}
                        >
                          Limpiar
                        </Button>
                      </InputAdornment>
                    ) : null,
                  }}
                />
              )}
              {pageGuidance && (
                <Typography data-testid="admin-users-page-guidance" variant="body2" color="text.secondary">
                  {pageGuidance}
                </Typography>
              )}
            </Stack>
            <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
              {hasUsers && (
                <>
                  <FormControlLabel
                    control={(
                      <Checkbox
                        checked={includeInactive}
                        onChange={(event) => setIncludeInactive(event.target.checked)}
                      />
                    )}
                    label="Incluir inactivos"
                  />
                </>
              )}
              {showRefreshAction && (
                <Tooltip title="Refrescar">
                  <span>
                    <IconButton
                      aria-label="Refrescar lista de usuarios"
                      onClick={handleRefresh}
                      disabled={usersQuery.isFetching}
                    >
                      <RefreshIcon />
                    </IconButton>
                  </span>
                </Tooltip>
              )}
            </Stack>
          </Stack>
        </Stack>

        <Card>
          <CardContent>
            {usersQuery.isLoading && <Typography>Cargando usuarios…</Typography>}
            {usersQuery.error && <Typography color="error">Error al cargar usuarios</Typography>}
            {!usersQuery.isLoading && users.length === 0 && (
              <Typography color="text.secondary">
                No hay usuarios todavía. Cuando exista el primero, aquí aparecerán búsqueda, filtros y señales de contacto para revisar la lista más rápido.
              </Typography>
            )}
            {showSearchEmptyState ? (
              <Stack spacing={1} alignItems="flex-start">
                <Typography color="text.secondary">
                  {searchEmptyStateMessage}
                </Typography>
              </Stack>
            ) : null}
            {visibleUsers.length ? (
              <Stack spacing={1.5}>
                {activeVisibleUsers.map((user) => (
                  <UserRow
                    key={user.userId}
                    user={user}
                    showInactiveStatusChip={includeInactive && !user.active}
                    onOpenCommunications={() => setSelectedUser(user)}
                    sharedModulesSummary={sharedModulesSummary}
                    sharedRolesSummary={sharedRolesSummary}
                    hideAccessSummary={hideRowAccessSummary}
                    hidePendingStateChip={hideSingleRowPendingState}
                    hidePendingProfileLabel={hideRepeatedPendingProfileLabel}
                  />
                ))}
                {showInactiveUsersGroup ? (
                  <>
                    <Typography
                      data-testid="admin-users-inactive-group-label"
                      variant="overline"
                      color="text.secondary"
                    >
                      {formatInactiveUserCountLabel(visibleInactiveUsersCount)}
                    </Typography>
                    {inactiveVisibleUsers.map((user) => (
                      <UserRow
                        key={user.userId}
                        user={user}
                        showInactiveStatusChip={false}
                        onOpenCommunications={() => setSelectedUser(user)}
                        sharedModulesSummary={sharedModulesSummary}
                        sharedRolesSummary={sharedRolesSummary}
                        hideAccessSummary={hideRowAccessSummary}
                        hidePendingStateChip={hideSingleRowPendingState}
                        hidePendingProfileLabel={hideRepeatedPendingProfileLabel}
                      />
                    ))}
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
}: {
  user: AdminUser;
  showInactiveStatusChip: boolean;
  onOpenCommunications: () => void;
  sharedRolesSummary: string;
  sharedModulesSummary: string;
  hideAccessSummary: boolean;
  hidePendingStateChip: boolean;
  hidePendingProfileLabel: boolean;
}) {
  const contactSummary = getUserContactSummary(user);
  const hasContactInfo = Boolean(contactSummary);
  const hasWhatsAppChannel = hasUserWhatsAppChannel(user);
  const accessSummary = buildUserAccessSummary({
    roles: user.roles,
    modules: user.modules,
    sharedRolesSummary,
    sharedModulesSummary,
  });
  const identity = summarizeUserIdentity(user);
  const hasLinkedProfile = hasLinkedAdminUserProfile(user);
  const profilePath = hasLinkedProfile ? `/perfil/${user.partyId}` : null;
  const missingChannelLabel = hasContactInfo ? 'WhatsApp pendiente' : 'Contacto pendiente';

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
            aria-label={`Abrir perfil de ${identity.primary}`}
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
        {!hasLinkedProfile && !hidePendingProfileLabel && (
          <Typography variant="caption" color="text.secondary">
            Perfil pendiente
          </Typography>
        )}
        {contactSummary && (
          <Typography variant="body2" color="text.secondary">
            {contactSummary}
          </Typography>
        )}
      </Box>
      {showInactiveStatusChip && (
        <Chip label="Inactivo" color="default" size="small" />
      )}
      {!hideAccessSummary && accessSummary && (
        <Box sx={{ minWidth: 220, flex: '1 1 240px' }}>
          <Typography variant="body2" color="text.secondary">
            {accessSummary}
          </Typography>
        </Box>
      )}
      <Stack direction="row" spacing={1} sx={{ ml: 'auto' }}>
        {hasWhatsAppChannel ? (
          <Button size="small" variant="contained" onClick={onOpenCommunications}>
            WhatsApp
          </Button>
        ) : !hidePendingStateChip ? (
          <Chip label={missingChannelLabel} color="warning" variant="outlined" size="small" />
        ) : null}
      </Stack>
    </Box>
  );
}
