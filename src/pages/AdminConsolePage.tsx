import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  CardHeader,
  Checkbox,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControl,
  FormHelperText,
  Grid,
  InputLabel,
  MenuItem,
  Paper,
  Select,
  ListItemText,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';
import type { SxProps, Theme } from '@mui/material/styles';
import RefreshIcon from '@mui/icons-material/Refresh';
import AutoFixHighIcon from '@mui/icons-material/AutoFixHigh';
import { useMutation, useQuery, useQueryClient, type QueryClient } from '@tanstack/react-query';
import { AdminApi } from '../api/admin';
import { Health } from '../utilities/health';
import type { AdminConsoleCard, AdminUserDTO, AdminUserStatus, AuditLogEntry, RoleKey } from '../api/types';
import { ROLE_OPTIONS, formatRoleList, normalizeRoleList } from '../constants/roles';
import { normalizeRoles } from '../config/menu';

const ADMIN_REFRESH_QUERY_KEYS = [
  ['admin', 'health'],
  ['admin', 'console'],
  ['admin', 'users'],
  ['admin', 'audit'],
] as const;
const BUILT_IN_ADMIN_CARD_IDS = [
  'access-control',
  'user-management',
  'users',
  'audit',
  'audit-log',
  'service-health',
  'health',
  'demo-seed',
  'seed',
] as const;
const BUILT_IN_ADMIN_CARD_TITLES = [
  'estado del servicio',
  'salud del servicio',
  'datos de demostracion',
  'usuarios y roles',
  'gestion de usuarios',
  'roles y permisos',
  'gestion de roles',
  'control de acceso',
  'auditoria reciente',
  'historial de auditoria',
  'registro de auditoria',
] as const;
const ADMIN_CONSOLE_PLACEHOLDER_BODY_FRAGMENTS = [
  'estamos trabajando en esta vista',
  'proximamente encontraras la funcionalidad completa aqui',
  'si necesitas priorizar esta seccion',
  'comparte los requisitos con el equipo de producto',
] as const;
const GETTING_STARTED_ADMIN_SECTIONS = [
  { label: '1. Estado del servicio', targetId: 'admin-service-health' },
  { label: '2. Usuarios y roles', targetId: 'admin-users-and-roles' },
  { label: '3. Auditoría reciente', targetId: 'admin-recent-audit' },
] as const;
const FIRST_RUN_USERS_EMPTY_STATE = 'Aún no hay usuarios administrables.';
const FIRST_RUN_AUDIT_EMPTY_STATE = 'La auditoría aparecerá cuando se registre el primer cambio.';
const ADMIN_USER_TABLE_BASE_COLUMN_COUNT = 2;
const AUDIT_TABLE_BASE_COLUMN_COUNT = 3;
const HEALTHY_HEALTH_INDICATORS = new Set(['ok', 'healthy', 'up', 'ready']);
const ADMIN_USERS_INLINE_EDIT_HINT_ID = 'admin-users-inline-edit-hint';
const SINGLE_ADMIN_USER_INLINE_EDIT_HINT =
  'Primer usuario administrable. Cuando exista una segunda cuenta, volverá la tabla comparativa.';

function invalidateAdminPanelQueries(queryClient: QueryClient) {
  ADMIN_REFRESH_QUERY_KEYS.forEach((queryKey) => {
    void queryClient.invalidateQueries({ queryKey: [...queryKey] });
  });
}

function normalizeAdminConsoleCardKey(value: string) {
  return value
    .replace(/\s+/g, ' ')
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .trim();
}

function normalizeAdminConsoleSectionKey(value: string) {
  return normalizeAdminConsoleCardKey(value)
    .replace(/[^a-z0-9]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

const BUILT_IN_ADMIN_CARD_ID_KEYS = new Set(
  BUILT_IN_ADMIN_CARD_IDS.map((value) => normalizeAdminConsoleSectionKey(value)),
);
const BUILT_IN_ADMIN_CARD_TITLE_KEYS = new Set(
  BUILT_IN_ADMIN_CARD_TITLES.map((value) => normalizeAdminConsoleSectionKey(value)),
);

function sanitizeAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  return cards.flatMap((card) => {
    const title = card.title.trim();
    const body = card.body
      .map((paragraph) => paragraph.trim())
      .filter((paragraph) => paragraph.length > 0)
      .filter((paragraph) => !isPlaceholderAdminConsoleParagraph(paragraph));

    if (title === '' || body.length === 0) {
      return [];
    }

    return [{ ...card, title, body }];
  });
}

function isPlaceholderAdminConsoleParagraph(paragraph: string) {
  const normalizedParagraph = normalizeAdminConsoleCardKey(paragraph);

  return ADMIN_CONSOLE_PLACEHOLDER_BODY_FRAGMENTS.some((fragment) => (
    normalizedParagraph.includes(fragment)
  ));
}

function isDedicatedAdminSectionCard(card: AdminConsoleCard) {
  const normalizedId = normalizeAdminConsoleSectionKey(card.cardId);
  const normalizedTitle = normalizeAdminConsoleSectionKey(card.title);

  return BUILT_IN_ADMIN_CARD_ID_KEYS.has(normalizedId) || BUILT_IN_ADMIN_CARD_TITLE_KEYS.has(normalizedTitle);
}

function dedupeAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  const cardsByTitle = new Map<string, AdminConsoleCard>();

  cards.forEach((card) => {
    const titleKey = normalizeAdminConsoleSectionKey(card.title);
    const existingCard = cardsByTitle.get(titleKey);

    if (!existingCard) {
      cardsByTitle.set(titleKey, { ...card, body: [...card.body] });
      return;
    }

    const seenParagraphs = new Set(
      existingCard.body.map((paragraph) => normalizeAdminConsoleCardKey(paragraph)),
    );
    const mergedBody = [...existingCard.body];

    card.body.forEach((paragraph) => {
      const paragraphKey = normalizeAdminConsoleCardKey(paragraph);

      if (seenParagraphs.has(paragraphKey)) {
        return;
      }

      seenParagraphs.add(paragraphKey);
      mergedBody.push(paragraph);
    });

    cardsByTitle.set(titleKey, { ...existingCard, body: mergedBody });
  });

  return [...cardsByTitle.values()];
}

function compareAdminConsoleCards(left: Pick<AdminConsoleCard, 'cardId' | 'title'>, right: Pick<AdminConsoleCard, 'cardId' | 'title'>) {
  const leftTitleKey = normalizeAdminConsoleSectionKey(left.title);
  const rightTitleKey = normalizeAdminConsoleSectionKey(right.title);

  if (leftTitleKey !== rightTitleKey) {
    return leftTitleKey < rightTitleKey ? -1 : 1;
  }

  const leftIdKey = normalizeAdminConsoleSectionKey(left.cardId);
  const rightIdKey = normalizeAdminConsoleSectionKey(right.cardId);

  if (leftIdKey !== rightIdKey) {
    return leftIdKey < rightIdKey ? -1 : 1;
  }

  return 0;
}

function sortAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  return [...cards].sort(compareAdminConsoleCards);
}

function dedupeAdminUsers(users: readonly AdminUserDTO[]) {
  const seenUserIds = new Set<number>();

  return users.filter((user) => {
    if (seenUserIds.has(user.userId)) {
      return false;
    }

    seenUserIds.add(user.userId);
    return true;
  });
}

function dedupeAuditEntries(entries: readonly AuditLogEntry[]) {
  const seenEntries = new Set<string>();

  return entries.filter((entry) => {
    const fingerprint = [
      entry.entity,
      entry.entityId,
      entry.action,
      entry.actorId ?? '',
      entry.diff ?? '',
      entry.createdAt,
    ].join('::');

    if (seenEntries.has(fingerprint)) {
      return false;
    }

    seenEntries.add(fingerprint);
    return true;
  });
}

function formatDate(value: string) {
  return new Date(value).toLocaleString();
}

function formatDateOrDash(value?: string | null) {
  if (!value) {
    return '—';
  }
  try {
    return formatDate(value);
  } catch {
    return value;
  }
}

function summarizeAdminUserIdentity(user: Pick<AdminUserDTO, 'displayName' | 'username'>) {
  const displayName = user.displayName?.trim() ?? '';
  const username = user.username.trim();
  const primary = displayName || username;
  const showUsername = displayName !== '' && displayName.toLowerCase() !== username.toLowerCase();

  return { primary, username, showUsername };
}

function getAdminUserVisibleIdentityKey(user: Pick<AdminUserDTO, 'displayName' | 'username'>) {
  const identity = summarizeAdminUserIdentity(user);

  return [
    identity.primary.trim().toLowerCase(),
    identity.showUsername ? identity.username.trim().toLowerCase() : '',
  ].join('::');
}

function getAdminUserIdsRequiringPartyId(
  users: readonly Pick<AdminUserDTO, 'userId' | 'displayName' | 'username' | 'partyId'>[],
) {
  const visibleIdentityCounts = new Map<string, number>();

  users.forEach((user) => {
    const identityKey = getAdminUserVisibleIdentityKey(user);
    visibleIdentityCounts.set(identityKey, (visibleIdentityCounts.get(identityKey) ?? 0) + 1);
  });

  return new Set(
    users
      .filter((user) => user.partyId != null)
      .filter((user) => (visibleIdentityCounts.get(getAdminUserVisibleIdentityKey(user)) ?? 0) > 1)
      .map((user) => user.userId),
  );
}

function formatAuditActor(
  actorId: number | null | undefined,
  usersById: ReadonlyMap<number, Pick<AdminUserDTO, 'displayName' | 'username'>>,
) {
  if (actorId == null) {
    return 'Sistema';
  }

  const actor = usersById.get(actorId);
  if (!actor) {
    return actorId;
  }

  const identity = summarizeAdminUserIdentity(actor);
  return identity.showUsername ? `${identity.primary} (${identity.username})` : identity.primary;
}

function hasAuditActor(actorId?: number | null) {
  return actorId != null;
}

function hasAuditDetail(diff?: string | null) {
  return (diff?.trim().length ?? 0) > 0;
}

function getAdminUserLastAccess(user: Pick<AdminUserDTO, 'lastSeenAt' | 'lastLoginAt'>) {
  return user.lastSeenAt ?? user.lastLoginAt;
}

function normalizeHealthIndicator(value?: string | null) {
  return value?.trim().toLowerCase() ?? '';
}

function isHealthyHealthIndicator(value?: string | null) {
  return HEALTHY_HEALTH_INDICATORS.has(normalizeHealthIndicator(value));
}

function normalizeRoleSelection(roles?: readonly RoleKey[] | null) {
  return normalizeRoleList(roles);
}

function hasRoleSelectionChanged(currentRoles?: readonly RoleKey[] | null, nextRoles?: readonly RoleKey[] | null) {
  const normalizedCurrentRoles = normalizeRoleSelection(currentRoles);
  const normalizedNextRoles = normalizeRoleSelection(nextRoles);

  if (normalizedCurrentRoles.length !== normalizedNextRoles.length) {
    return true;
  }

  return normalizedCurrentRoles.some((role, index) => role !== normalizedNextRoles[index]);
}

function formatRoleGroupLabel(roles: readonly RoleKey[]) {
  if (roles.length <= 1) {
    return roles[0] ?? '';
  }

  if (roles.length === 2) {
    return `${roles[0]} y ${roles[1]}`;
  }

  return `${roles.slice(0, -1).join(', ')} y ${roles[roles.length - 1]}`;
}

function buildPendingRoleChangesSummary(
  currentRoles?: readonly RoleKey[] | null,
  nextRoles?: readonly RoleKey[] | null,
) {
  const normalizedCurrentRoles = normalizeRoleSelection(currentRoles);
  const normalizedNextRoles = normalizeRoleSelection(nextRoles);
  const rolesToAdd = normalizedNextRoles.filter((role) => !normalizedCurrentRoles.includes(role));
  const rolesToRemove = normalizedCurrentRoles.filter((role) => !normalizedNextRoles.includes(role));
  const actions: string[] = [];

  if (rolesToAdd.length > 0) {
    actions.push(`agregar ${formatRoleGroupLabel(rolesToAdd)}`);
  }

  if (rolesToRemove.length > 0) {
    actions.push(`quitar ${formatRoleGroupLabel(rolesToRemove)}`);
  }

  if (actions.length === 0) {
    return null;
  }

  return `${actions.length === 1 ? 'Cambio pendiente' : 'Cambios pendientes'}: ${actions.join(' · ')}.`;
}

function getNavigationEquivalentRoleGroups(roles?: readonly RoleKey[] | null) {
  const groupedByNavigationProfile = new Map<string, RoleKey[]>();

  normalizeRoleSelection(roles).forEach((role) => {
    const navigationProfile = normalizeRoles([role])[0] ?? role.toLowerCase();
    const group = groupedByNavigationProfile.get(navigationProfile) ?? [];
    group.push(role);
    groupedByNavigationProfile.set(navigationProfile, group);
  });

  return [...groupedByNavigationProfile.values()]
    .map((group) => normalizeRoleSelection(group))
    .filter((group): group is RoleKey[] => group.length > 1);
}

function buildAdminUsersSectionDescription({
  showLastAccessColumn,
  showStatusColumn,
}: {
  showLastAccessColumn: boolean;
  showStatusColumn: boolean;
}) {
  const hiddenColumnSummaries: string[] = [];

  if (!showLastAccessColumn) {
    hiddenColumnSummaries.push(
      'la columna de último acceso reaparecerá cuando exista al menos un ingreso registrado',
    );
  }

  if (!showStatusColumn) {
    hiddenColumnSummaries.push(
      'la columna de estado reaparecerá cuando exista una cuenta invitada o suspendida',
    );
  }

  if (hiddenColumnSummaries.length > 0) {
    return `Vista actual: ${hiddenColumnSummaries.join(' y ')}.`;
  }

  return null;
}

function buildAuditSectionDescription({
  showActorColumn,
  showDetailColumn,
}: {
  showActorColumn: boolean;
  showDetailColumn: boolean;
}) {
  const hiddenColumnSummaries: string[] = [];

  if (!showActorColumn) {
    hiddenColumnSummaries.push(
      'la columna de actor reaparecerá cuando un cambio quede asociado a una cuenta específica',
    );
  }

  if (!showDetailColumn) {
    hiddenColumnSummaries.push(
      'la columna de detalle reaparecerá cuando exista información extra para revisar',
    );
  }

  if (hiddenColumnSummaries.length === 0) {
    return null;
  }

  return `Vista actual: ${hiddenColumnSummaries.join(' y ')}.`;
}

function formatFirstRunAdditionalModulesActionLabel(cards: readonly Pick<AdminConsoleCard, 'title'>[]) {
  const count = cards.length;

  if (count === 0) {
    return 'Opcional: ver módulos adicionales';
  }

  if (count === 1) {
    return `Opcional: ver ${cards[0]?.title ?? 'módulo adicional'}`;
  }

  return `Opcional: ver ${count} módulos adicionales`;
}

function formatStandaloneAdditionalModulesActionLabel(cards: readonly Pick<AdminConsoleCard, 'title'>[]) {
  const count = cards.length;

  if (count === 0) {
    return 'Ver módulos adicionales';
  }

  if (count === 1) {
    return `Ver ${cards[0]?.title ?? 'módulo adicional'}`;
  }

  return `Ver ${count} módulos adicionales`;
}

const STATUS_META: Record<AdminUserStatus, { label: string; color: 'default' | 'success' | 'warning' | 'error' | 'info' }> = {
  ACTIVE: { label: 'Activo', color: 'success' },
  INVITED: { label: 'Invitado', color: 'info' },
  DISABLED: { label: 'Suspendido', color: 'default' },
};

function renderAdditionalModuleCardsGrid({
  cards,
  isFetching,
  isPending,
  id,
  sx,
}: {
  cards: readonly AdminConsoleCard[];
  isFetching: boolean;
  isPending: boolean;
  id?: string;
  sx?: SxProps<Theme>;
}) {
  return (
    <Grid container spacing={2} sx={sx} id={id}>
      {cards.map((card) => (
        <Grid item xs={12} md={4} key={card.cardId}>
          <Card variant="outlined">
            <CardHeader title={card.title} />
            <CardContent>
              {card.body.map((paragraph, idx) => (
                <Typography
                  key={`${card.cardId}-line-${idx}`}
                  variant="body2"
                  color="text.secondary"
                  paragraph={idx < card.body.length - 1}
                >
                  {paragraph}
                </Typography>
              ))}
              {isFetching && !isPending && (
                <Typography variant="caption" color="text.secondary">
                  Actualizando…
                </Typography>
              )}
            </CardContent>
          </Card>
        </Grid>
      ))}
    </Grid>
  );
}

export default function AdminConsolePage() {
  const qc = useQueryClient();
  const [rotationWarning, setRotationWarning] = useState(false);
  const [editingUser, setEditingUser] = useState<AdminUserDTO | null>(null);
  const [selectedRoles, setSelectedRoles] = useState<RoleKey[]>([]);
  const [dialogError, setDialogError] = useState<string | null>(null);
  const [showFirstRunAdditionalModules, setShowFirstRunAdditionalModules] = useState(false);
  const [showStandaloneAdditionalModules, setShowStandaloneAdditionalModules] = useState(false);

  const healthQuery = useQuery({
    queryKey: ['admin', 'health'],
    queryFn: Health.fetch,
    staleTime: 30_000,
  });

  const auditQuery = useQuery({
    queryKey: ['admin', 'audit'],
    queryFn: AdminApi.auditLogs,
    staleTime: 10_000,
  });

  const consoleQuery = useQuery({
    queryKey: ['admin', 'console'],
    queryFn: AdminApi.consolePreview,
    staleTime: 60_000,
  });

  const usersQuery = useQuery({
    queryKey: ['admin', 'users'],
    queryFn: AdminApi.listUsers,
    staleTime: 60_000,
  });

  const seedMutation = useMutation({
    mutationFn: AdminApi.seed,
    onSuccess: () => {
      invalidateAdminPanelQueries(qc);
    },
  });

  const updateRolesMutation = useMutation({
    mutationFn: ({ userId, roles }: { userId: number; roles: RoleKey[] }) =>
      AdminApi.updateUserRoles(userId, roles),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['admin', 'users'] });
      setEditingUser(null);
      setDialogError(null);
    },
    onError: (error: unknown) => {
      const message = error instanceof Error ? error.message : 'No se pudieron actualizar los roles.';
      setDialogError(message);
    },
  });

  useEffect(() => {
    const handler = () => setRotationWarning(true);
    window.addEventListener('tdf-session-rotation-due', handler as EventListener);
    return () => {
      window.removeEventListener('tdf-session-rotation-due', handler as EventListener);
    };
  }, []);

  useEffect(() => {
    if (editingUser) {
      setSelectedRoles(normalizeRoleList(editingUser.roles));
      setDialogError(null);
    } else {
      setSelectedRoles([]);
      setDialogError(null);
    }
  }, [editingUser]);

  const audits = dedupeAuditEntries(auditQuery.data ?? []);
  const previewCards = sortAdminConsoleCards(dedupeAdminConsoleCards(
    sanitizeAdminConsoleCards(
      consoleQuery.data?.cards?.filter((card) => !isDedicatedAdminSectionCard(card)) ?? [],
    ),
  ));
  const consoleCards: AdminConsoleCard[] = previewCards;
  const consoleError = consoleQuery.isError ? (consoleQuery.error as Error).message : null;
  const users = dedupeAdminUsers(usersQuery.data ?? []);
  const usersById = useMemo(
    () => new Map(users.map((user) => [user.userId, user])),
    [users],
  );
  const userIdsRequiringPartyId = useMemo(
    () => getAdminUserIdsRequiringPartyId(users),
    [users],
  );
  const isUsersLoading = usersQuery.isLoading;
  const usersError = usersQuery.isError ? (usersQuery.error as Error).message : null;
  const singleAdminUser = !isUsersLoading && users.length === 1 ? (users[0] ?? null) : null;
  const singleAdminUserIdentity = singleAdminUser ? summarizeAdminUserIdentity(singleAdminUser) : null;
  const singleAdminUserLastAccess = singleAdminUser ? getAdminUserLastAccess(singleAdminUser) : null;
  const shouldShowSingleAdminUserStatus = singleAdminUser?.status != null && singleAdminUser.status !== 'ACTIVE';
  const singleAdminUserStatusLabel = shouldShowSingleAdminUserStatus && singleAdminUser?.status
    ? (STATUS_META[singleAdminUser.status]?.label ?? singleAdminUser.status)
    : null;
  const showUsersTable = isUsersLoading || users.length > 1;
  const showUsersInlineEditHint = singleAdminUser !== null;
  const showUsersLastAccessColumn = isUsersLoading || users.some((user) => getAdminUserLastAccess(user) != null);
  const showUsersStatusColumn = isUsersLoading || users.some((user) => user.status !== 'ACTIVE');
  const singleAuditEntry = !auditQuery.isLoading && audits.length === 1 ? (audits[0] ?? null) : null;
  const singleAuditHasActor = hasAuditActor(singleAuditEntry?.actorId);
  const singleAuditHasDetail = hasAuditDetail(singleAuditEntry?.diff);
  const showAuditTable = auditQuery.isLoading || audits.length > 1;
  const showAuditActorColumn = auditQuery.isLoading || audits.some((entry) => hasAuditActor(entry.actorId));
  const showAuditDetailColumn = auditQuery.isLoading || audits.some((entry) => hasAuditDetail(entry.diff));
  const hasUsersSectionData = showUsersTable || singleAdminUser !== null;
  const hasAuditSectionData = showAuditTable || singleAuditEntry !== null;
  const showGettingStartedGuidance =
    !consoleQuery.isPending
    && !usersQuery.isLoading
    && !auditQuery.isLoading
    && users.length === 0
    && audits.length === 0;
  const showHeaderRefreshAction =
    !showGettingStartedGuidance
    || healthQuery.isError
    || auditQuery.isError
    || consoleQuery.isError
    || usersQuery.isError;
  const showHeaderActions = showHeaderRefreshAction || !showGettingStartedGuidance;
  const usersSectionDescription = showGettingStartedGuidance
    ? null
    : (
      singleAdminUser
        ? SINGLE_ADMIN_USER_INLINE_EDIT_HINT
        : (
          showUsersTable
            ? buildAdminUsersSectionDescription({
              showLastAccessColumn: showUsersLastAccessColumn,
              showStatusColumn: showUsersStatusColumn,
            })
            : null
        )
    );
  const auditSectionDescription = showGettingStartedGuidance
    ? null
    : (
      showAuditTable
        ? (
          buildAuditSectionDescription({
            showActorColumn: showAuditActorColumn,
            showDetailColumn: showAuditDetailColumn,
          })
          ?? 'Confirma quién cambió qué y cuándo antes de repetir una acción o ajustar permisos.'
        )
        : null
    );
  const firstRunAdditionalModulesActionLabel = formatFirstRunAdditionalModulesActionLabel(consoleCards);
  const shouldShowAdditionalModuleCards = !showGettingStartedGuidance || showFirstRunAdditionalModules;
  const showFirstRunAdditionalModulesShowAction =
    showGettingStartedGuidance
    && consoleCards.length > 0
    && !shouldShowAdditionalModuleCards;
  const showFirstRunAdditionalModulesHideAction =
    showGettingStartedGuidance
    && shouldShowAdditionalModuleCards;
  const showStandaloneAdditionalModulesSection = consoleCards.length > 0 && !showGettingStartedGuidance;
  const standaloneAdditionalModulesActionLabel = formatStandaloneAdditionalModulesActionLabel(consoleCards);
  useEffect(() => {
    if (showGettingStartedGuidance) {
      setShowFirstRunAdditionalModules(false);
    }
  }, [showGettingStartedGuidance]);
  const editingTitle = useMemo(() => {
    if (!editingUser) return '';
    return editingUser.displayName?.trim() || editingUser.username;
  }, [editingUser]);
  const currentRoleSummary = useMemo(() => (
    editingUser ? formatRoleList(editingUser.roles) : '—'
  ), [editingUser]);
  const hasPendingRoleChanges = useMemo(() => (
    editingUser ? hasRoleSelectionChanged(editingUser.roles, selectedRoles) : false
  ), [editingUser, selectedRoles]);
  const pendingRoleChangesSummary = useMemo(() => (
    editingUser ? buildPendingRoleChangesSummary(editingUser.roles, selectedRoles) : null
  ), [editingUser, selectedRoles]);
  const equivalentRoleWarning = useMemo(() => {
    if (!hasPendingRoleChanges) {
      return null;
    }

    const groups = getNavigationEquivalentRoleGroups(selectedRoles);
    if (groups.length === 0) {
      return null;
    }

    return groups
      .map((group) => formatRoleGroupLabel(group))
      .join(' · ');
  }, [hasPendingRoleChanges, selectedRoles]);
  const isRefreshingPanel =
    healthQuery.isFetching
    || auditQuery.isFetching
    || consoleQuery.isFetching
    || usersQuery.isFetching;
  const shouldShowHealthLoadingState = healthQuery.isPending && healthQuery.data == null;
  const showCompactHealthyServiceSummary =
    healthQuery.data != null
    && isHealthyHealthIndicator(healthQuery.data.status)
    && isHealthyHealthIndicator(healthQuery.data.db);
  const firstRunDemoActionCopy = {
    description: 'Opcional: carga datos de ejemplo para revisar usuarios, roles y auditoría sin tocar producción.',
    buttonLabel: 'Cargar datos de ejemplo',
    pendingLabel: 'Cargando ejemplo…',
  } as const;
  const demoSeedActionCopy = {
    buttonLabel: 'Restablecer datos demo',
    pendingLabel: 'Restableciendo demo…',
    successMessage: 'Datos de demostración preparados correctamente.',
  } as const;

  const handleCloseDialog = () => {
    if (updateRolesMutation.isPending) return;
    setEditingUser(null);
  };

  const handleSaveRoles = () => {
    if (!editingUser || !hasPendingRoleChanges) return;
    setDialogError(null);
    updateRolesMutation.mutate({ userId: editingUser.userId, roles: selectedRoles });
  };

  const handleRefreshPanel = () => {
    invalidateAdminPanelQueries(qc);
  };

  const renderStatus = (status?: AdminUserStatus | null, options?: { hideActive?: boolean }) => {
    if (!status) return '—';
    if (options?.hideActive && status === 'ACTIVE') return '—';
    const meta = STATUS_META[status];
    if (!meta) {
      return status;
    }
    return <Chip label={meta.label} color={meta.color} size="small" />;
  };

  return (
    <Stack spacing={3}>
      <Stack
        direction={{ xs: 'column', sm: 'row' }}
        spacing={2}
        justifyContent="space-between"
        alignItems={{ xs: 'flex-start', sm: 'center' }}
      >
        <Box>
          <Typography variant="h5">Consola de administración</Typography>
          <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
            Revisa el estado del sistema, ajusta permisos y valida cambios recientes desde un solo lugar.
          </Typography>
        </Box>
        {showHeaderActions && (
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            {showHeaderRefreshAction && (
              <Button
                variant="outlined"
                startIcon={<RefreshIcon />}
                onClick={handleRefreshPanel}
                disabled={isRefreshingPanel}
              >
                {isRefreshingPanel ? 'Actualizando panel…' : 'Actualizar panel'}
              </Button>
            )}
            {!showGettingStartedGuidance && (
              <Button
                variant="text"
                startIcon={<AutoFixHighIcon />}
                onClick={() => seedMutation.mutate()}
                disabled={seedMutation.isPending}
              >
                {seedMutation.isPending ? demoSeedActionCopy.pendingLabel : demoSeedActionCopy.buttonLabel}
              </Button>
            )}
          </Stack>
        )}
      </Stack>

      {rotationWarning && (
        <Alert severity="warning" onClose={() => setRotationWarning(false)}>
          Tu sesión lleva 30 días activa. Genera un nuevo token o vuelve a autenticarte para mantener la seguridad.
        </Alert>
      )}

      {seedMutation.isSuccess && !showGettingStartedGuidance && (
        <Alert severity="success">
          {demoSeedActionCopy.successMessage}
        </Alert>
      )}

      {showGettingStartedGuidance && (
        <Alert severity="info" variant="outlined">
          <Stack spacing={1.5}>
            <Box>
              <Typography variant="subtitle2">Primeros pasos</Typography>
              <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                Sigue este recorrido para ubicar cada bloque sin repetir revisiones vacías.
              </Typography>
            </Box>
            <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
              {GETTING_STARTED_ADMIN_SECTIONS.map((section) => (
                <Chip
                  key={section.targetId}
                  component="a"
                  clickable
                  color="info"
                  variant="outlined"
                  label={section.label}
                  href={`#${section.targetId}`}
                />
              ))}
            </Stack>
            {showFirstRunAdditionalModulesShowAction && (
              <Button
                size="small"
                variant="text"
                onClick={() => setShowFirstRunAdditionalModules(true)}
                aria-controls="admin-additional-modules-list"
                aria-expanded="false"
                sx={{ alignSelf: 'flex-start' }}
              >
                {firstRunAdditionalModulesActionLabel}
              </Button>
            )}
            {showFirstRunAdditionalModulesHideAction && (
              <Stack
                spacing={1.5}
                sx={{
                  pt: 1.5,
                  borderTop: '1px solid',
                  borderColor: 'divider',
                }}
              >
                <Stack
                  direction={{ xs: 'column', sm: 'row' }}
                  spacing={1}
                  justifyContent="space-between"
                  alignItems={{ xs: 'flex-start', sm: 'center' }}
                >
                  <Box>
                    <Typography variant="subtitle2">Módulos opcionales</Typography>
                    <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                      Revísalos aquí solo si ya necesitas ese flujo extra, sin salir del recorrido inicial.
                    </Typography>
                  </Box>
                  <Button
                    size="small"
                    variant="text"
                    onClick={() => setShowFirstRunAdditionalModules(false)}
                    aria-controls="admin-additional-modules-list"
                    aria-expanded="true"
                  >
                    Ocultar módulos adicionales
                  </Button>
                </Stack>
                {renderAdditionalModuleCardsGrid({
                  cards: consoleCards,
                  isFetching: consoleQuery.isFetching,
                  isPending: consoleQuery.isPending,
                  id: 'admin-additional-modules-list',
                })}
              </Stack>
            )}
            <Stack spacing={1} alignItems="flex-start">
              <Typography variant="body2" color="text.secondary">
                {firstRunDemoActionCopy.description}
              </Typography>
              <Button
                variant="contained"
                size="small"
                startIcon={<AutoFixHighIcon />}
                onClick={() => seedMutation.mutate()}
                disabled={seedMutation.isPending}
              >
                {seedMutation.isPending ? firstRunDemoActionCopy.pendingLabel : firstRunDemoActionCopy.buttonLabel}
              </Button>
            </Stack>
          </Stack>
        </Alert>
      )}

      {consoleError && (
        <Alert severity="warning">
          No se pudo cargar el panel dinámico. Mostrando la consola base. Detalle: {consoleError}
        </Alert>
      )}

      <Box sx={{ maxWidth: 480 }}>
        <Card variant="outlined" id="admin-service-health">
          <CardHeader title="Estado del servicio" />
          <CardContent>
            {shouldShowHealthLoadingState ? (
              <Stack direction="row" alignItems="center" spacing={1}>
                <CircularProgress size={18} />
                <Typography variant="body2" color="text.secondary">
                  Comprobando API y base de datos…
                </Typography>
              </Stack>
            ) : showCompactHealthyServiceSummary ? (
              <Typography variant="body2">
                Todo listo: API y base de datos responden correctamente.
              </Typography>
            ) : healthQuery.data ? (
              <>
                <Typography variant="body2">API: {healthQuery.data?.status ?? '—'}</Typography>
                <Typography variant="body2">Base de datos: {healthQuery.data?.db ?? '—'}</Typography>
              </>
            ) : null}
            {healthQuery.isError && (
              <Alert severity="error" sx={{ mt: 2 }}>
                {(healthQuery.error as Error).message}
              </Alert>
            )}
          </CardContent>
        </Card>
      </Box>

      <Paper variant="outlined" id="admin-users-and-roles">
        <Box sx={{ px: 2, py: 1 }}>
          <Box>
            <Typography variant="h6">Usuarios y roles</Typography>
            {usersSectionDescription && (
              <Typography
                variant="body2"
                color="text.secondary"
              >
                {usersSectionDescription}
              </Typography>
            )}
          </Box>
        </Box>
        {usersError && (
          <Alert severity="error" sx={{ mx: 2 }}>
            {usersError}
          </Alert>
        )}
        {showUsersTable ? (
          <TableContainer>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Usuario</TableCell>
                  <TableCell>
                    <Stack spacing={0} component="span">
                      <Typography component="span" variant="body2" fontWeight={600}>
                        Roles
                      </Typography>
                      <Typography component="span" variant="caption" color="text.secondary">
                        Clic para editar
                      </Typography>
                    </Stack>
                  </TableCell>
                  {showUsersLastAccessColumn && <TableCell>Último acceso</TableCell>}
                  {showUsersStatusColumn && <TableCell>Estado</TableCell>}
                </TableRow>
              </TableHead>
              <TableBody>
                {isUsersLoading && (
                  <TableRow>
                    <TableCell
                      colSpan={
                        ADMIN_USER_TABLE_BASE_COLUMN_COUNT
                        + (showUsersLastAccessColumn ? 1 : 0)
                        + (showUsersStatusColumn ? 1 : 0)
                      }
                    >
                      <Stack direction="row" alignItems="center" justifyContent="center" spacing={1} sx={{ py: 2 }}>
                        <CircularProgress size={18} />
                        <Typography variant="body2" color="text.secondary">
                          Cargando usuarios…
                        </Typography>
                      </Stack>
                    </TableCell>
                  </TableRow>
                )}
                {users.map((user) => {
                  const identity = summarizeAdminUserIdentity(user);
                  const shouldShowPartyId = user.partyId != null && userIdsRequiringPartyId.has(user.userId);
                  return (
                    <TableRow key={user.userId} hover>
                      <TableCell>
                        <Stack spacing={0.25}>
                          <Typography variant="body2" fontWeight={600}>
                            {identity.primary}
                          </Typography>
                          {identity.showUsername && (
                            <Typography variant="caption" color="text.secondary">
                              Usuario: {identity.username}
                            </Typography>
                          )}
                          {shouldShowPartyId ? (
                            <Typography variant="caption" color="text.secondary">
                              Party #{user.partyId}
                            </Typography>
                          ) : null}
                        </Stack>
                      </TableCell>
                      <TableCell>
                        <Button
                          size="small"
                          onClick={() => setEditingUser(user)}
                          aria-label={`Editar roles de ${identity.primary}`}
                          aria-describedby={showUsersInlineEditHint ? ADMIN_USERS_INLINE_EDIT_HINT_ID : undefined}
                          sx={{
                            px: 0,
                            minWidth: 0,
                            justifyContent: 'flex-start',
                            textTransform: 'none',
                          }}
                        >
                          {formatRoleList(user.roles)}
                        </Button>
                      </TableCell>
                      {showUsersLastAccessColumn && <TableCell>{formatDateOrDash(getAdminUserLastAccess(user))}</TableCell>}
                      {showUsersStatusColumn && <TableCell>{renderStatus(user.status, { hideActive: true })}</TableCell>}
                    </TableRow>
                  );
                })}
              </TableBody>
            </Table>
          </TableContainer>
        ) : singleAdminUser && !usersError ? (
          <Box sx={{ px: 2, pb: 2 }}>
            <Stack
              spacing={1}
              direction={{ xs: 'column', md: 'row' }}
              justifyContent="space-between"
              alignItems={{ xs: 'flex-start', md: 'center' }}
              sx={{
                border: '1px solid',
                borderColor: 'divider',
                borderRadius: 2,
                px: 1.5,
                py: 1.25,
              }}
            >
              <Stack spacing={0.25}>
                <Typography variant="body2" fontWeight={600}>
                  {singleAdminUserIdentity?.primary}
                </Typography>
                {singleAdminUserIdentity?.showUsername && (
                  <Typography variant="caption" color="text.secondary">
                    Usuario: {singleAdminUserIdentity.username}
                  </Typography>
                )}
                {singleAdminUser.partyId != null && userIdsRequiringPartyId.has(singleAdminUser.userId) ? (
                  <Typography variant="caption" color="text.secondary">
                    Party #{singleAdminUser.partyId}
                  </Typography>
                ) : null}
              </Stack>
              <Stack spacing={0.5} alignItems={{ xs: 'flex-start', md: 'flex-end' }}>
                <Typography id={ADMIN_USERS_INLINE_EDIT_HINT_ID} variant="caption" color="text.secondary">
                  Roles · Clic para editar
                </Typography>
                <Button
                  size="small"
                  onClick={() => setEditingUser(singleAdminUser)}
                  aria-label={`Editar roles de ${singleAdminUserIdentity?.primary ?? singleAdminUser.username}`}
                  aria-describedby={ADMIN_USERS_INLINE_EDIT_HINT_ID}
                  sx={{
                    px: 0,
                    minWidth: 0,
                    textTransform: 'none',
                  }}
                >
                  {formatRoleList(singleAdminUser.roles)}
                </Button>
                {singleAdminUserLastAccess && (
                  <Typography variant="body2" color="text.secondary">
                    Último acceso: {formatDateOrDash(singleAdminUserLastAccess)}
                  </Typography>
                )}
                {singleAdminUserStatusLabel && (
                  <Typography variant="body2" color="text.secondary">
                    Estado: {singleAdminUserStatusLabel}
                  </Typography>
                )}
              </Stack>
            </Stack>
          </Box>
        ) : !usersError ? (
          <Box sx={{ px: 2, pb: 2 }}>
            <Typography variant="body2" color="text.secondary">
              {showGettingStartedGuidance
                ? FIRST_RUN_USERS_EMPTY_STATE
                : 'Todavía no hay usuarios administrables. Cuando exista el primero, aquí verás roles, último acceso y el atajo para editar roles.'}
            </Typography>
          </Box>
        ) : null}
      </Paper>

      <Paper variant="outlined" id="admin-recent-audit">
        <Box sx={{ px: 2, py: 1 }}>
          <Typography variant="h6">Auditoría reciente</Typography>
          {auditSectionDescription && (
            <Typography variant="body2" color="text.secondary">
              {auditSectionDescription}
            </Typography>
          )}
        </Box>
        {auditQuery.isError && (
          <Alert severity="error" sx={{ mx: 2 }}>
            {(auditQuery.error as Error).message}
          </Alert>
        )}
        {showAuditTable ? (
          <TableContainer>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Fecha</TableCell>
                  <TableCell>Entidad</TableCell>
                  <TableCell>Acción</TableCell>
                  {showAuditActorColumn && <TableCell>Actor</TableCell>}
                  {showAuditDetailColumn && <TableCell>Detalle</TableCell>}
                </TableRow>
              </TableHead>
              <TableBody>
                {audits.map((entry: AuditLogEntry, index: number) => (
                  <TableRow key={`${entry.entity}-${entry.entityId}-${index}`}>
                    <TableCell>{formatDate(entry.createdAt)}</TableCell>
                    <TableCell>{entry.entity} · {entry.entityId}</TableCell>
                    <TableCell>{entry.action}</TableCell>
                    {showAuditActorColumn && <TableCell>{formatAuditActor(entry.actorId, usersById)}</TableCell>}
                    {showAuditDetailColumn && (
                      <TableCell>
                        <Typography variant="body2" color="text.secondary" sx={{ maxWidth: 320, whiteSpace: 'pre-wrap' }}>
                          {entry.diff ?? '—'}
                        </Typography>
                      </TableCell>
                    )}
                  </TableRow>
                ))}
                {auditQuery.isLoading && (
                  <TableRow>
                    <TableCell
                      colSpan={
                        AUDIT_TABLE_BASE_COLUMN_COUNT
                        + (showAuditActorColumn ? 1 : 0)
                        + (showAuditDetailColumn ? 1 : 0)
                      }
                    >
                      <Typography variant="body2" align="center" color="text.secondary" sx={{ py: 2 }}>
                        Cargando auditoría…
                      </Typography>
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          </TableContainer>
        ) : singleAuditEntry && !auditQuery.isError ? (
          <Box sx={{ px: 2, pb: 2 }}>
            <Stack spacing={1.25}>
              <Typography variant="body2" color="text.secondary">
                Primer evento de auditoría. Revísalo aquí; cuando exista el segundo, volverá la tabla cronológica.
              </Typography>
              <Stack
                spacing={0.75}
                sx={{
                  border: '1px solid',
                  borderColor: 'divider',
                  borderRadius: 2,
                  px: 1.5,
                  py: 1.25,
                }}
              >
                <Typography variant="body2">
                  <Box component="span" sx={{ fontWeight: 600 }}>Acción:</Box> {singleAuditEntry.action}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Fecha:</Box> {formatDate(singleAuditEntry.createdAt)}
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Entidad:</Box> {singleAuditEntry.entity} · {singleAuditEntry.entityId}
                </Typography>
                {singleAuditHasActor && (
                  <Typography variant="body2" color="text.secondary">
                    <Box component="span" sx={{ fontWeight: 600 }}>Actor:</Box> {formatAuditActor(singleAuditEntry.actorId, usersById)}
                  </Typography>
                )}
                {singleAuditHasDetail && (
                  <Typography variant="body2" color="text.secondary" sx={{ whiteSpace: 'pre-wrap' }}>
                    <Box component="span" sx={{ fontWeight: 600 }}>Detalle:</Box> {singleAuditEntry.diff}
                  </Typography>
                )}
              </Stack>
            </Stack>
          </Box>
        ) : !auditQuery.isError ? (
          <Box sx={{ px: 2, pb: 2 }}>
            <Typography variant="body2" color="text.secondary">
              {showGettingStartedGuidance
                ? FIRST_RUN_AUDIT_EMPTY_STATE
                : 'Todavía no hay eventos de auditoría. Cuando alguien cambie permisos o datos del sistema, aquí verás quién hizo qué y cuándo.'}
            </Typography>
          </Box>
        ) : null}
      </Paper>

      {showStandaloneAdditionalModulesSection && (
        <Paper variant="outlined" id="admin-additional-modules">
          <Box sx={{ px: 2, py: 1 }}>
            <Stack
              direction={{ xs: 'column', sm: 'row' }}
              spacing={1}
              justifyContent="space-between"
              alignItems={{ xs: 'flex-start', sm: 'center' }}
            >
              <Box>
                <Typography variant="h6">Módulos adicionales</Typography>
                <Typography variant="body2" color="text.secondary">
                  Tarjetas auxiliares del panel. Ábrelas solo cuando ya confirmaste salud, usuarios y auditoría.
                </Typography>
              </Box>
              <Button
                size="small"
                variant="text"
                onClick={() => setShowStandaloneAdditionalModules((current) => !current)}
                aria-controls="admin-additional-modules-list"
                aria-expanded={showStandaloneAdditionalModules}
              >
                {showStandaloneAdditionalModules
                  ? 'Ocultar módulos adicionales'
                  : standaloneAdditionalModulesActionLabel}
              </Button>
            </Stack>
          </Box>
          {showStandaloneAdditionalModules && renderAdditionalModuleCardsGrid({
            cards: consoleCards,
            isFetching: consoleQuery.isFetching,
            isPending: consoleQuery.isPending,
            id: 'admin-additional-modules-list',
            sx: { px: 2, pb: 2 },
          })}
        </Paper>
      )}

      <Dialog open={!!editingUser} onClose={handleCloseDialog} fullWidth maxWidth="sm">
        <DialogTitle>Editar roles · {editingTitle}</DialogTitle>
        <DialogContent>
          <Typography variant="body2" color="text.secondary" paragraph>
            Roles actuales: {currentRoleSummary}. Ajusta la selección para abrir o retirar módulos en esta cuenta.
          </Typography>
          {dialogError && (
            <Alert severity="error" sx={{ mb: 2 }}>
              {dialogError}
            </Alert>
          )}
          <FormControl fullWidth>
            <InputLabel id="admin-user-roles-label">Roles asignados</InputLabel>
            <Select
              labelId="admin-user-roles-label"
              label="Roles asignados"
              multiple
              value={selectedRoles}
              onChange={(event) => setSelectedRoles(normalizeRoleList(event.target.value as RoleKey[]))}
              renderValue={(value) => formatRoleList(value as RoleKey[])}
            >
              {ROLE_OPTIONS.map((option) => (
                <MenuItem key={option.value} value={option.value}>
                  <Checkbox checked={selectedRoles.includes(option.value)} />
                  <ListItemText primary={option.label} />
                </MenuItem>
              ))}
            </Select>
            <FormHelperText>
              {hasPendingRoleChanges
                ? (pendingRoleChangesSummary ?? 'Hay cambios pendientes. Revisa la selección antes de guardar.')
                : 'Sin cambios pendientes. Modifica la selección para habilitar Guardar cambios.'}
            </FormHelperText>
          </FormControl>
          {equivalentRoleWarning && (
            <Alert severity="info" variant="outlined" sx={{ mt: 2 }}>
              Estos roles muestran la misma navegación principal en esta app: {equivalentRoleWarning}. Revisa si necesitas ambos antes de guardar.
            </Alert>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog} disabled={updateRolesMutation.isPending}>
            Cancelar
          </Button>
          <Button
            onClick={handleSaveRoles}
            variant="contained"
            disabled={updateRolesMutation.isPending || !hasPendingRoleChanges}
          >
            {updateRolesMutation.isPending ? 'Guardando…' : 'Guardar cambios'}
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}
