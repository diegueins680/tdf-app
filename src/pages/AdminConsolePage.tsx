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
import EditOutlinedIcon from '@mui/icons-material/EditOutlined';
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
  'system-health',
  'system-status',
  'health',
  'demo-seed',
  'demo-data',
  'sample-data',
  'example-data',
  'seed-data',
  'seed',
  'first-run',
  'getting-started',
  'quick-start',
  'start-here',
  'primeros-pasos',
  'inicio-rapido',
  'admin-console',
  'admin-panel',
  'administration',
] as const;
const BUILT_IN_ADMIN_CARD_TITLES = [
  'consola de administracion',
  'panel de administracion',
  'administracion',
  'admin console',
  'admin panel',
  'admin overview',
  'administration',
  'administrative overview',
  'resumen administrativo',
  'resumen de administracion',
  'primeros pasos',
  'inicio rapido',
  'guia inicial',
  'getting started',
  'quick start',
  'start here',
  'admin getting started',
  'admin quick start',
  'admin setup guide',
  'administration setup guide',
  'configuracion',
  'configuration',
  'settings',
  'preferencias',
  'preferences',
  'ajustes',
  'workspace settings',
  'system settings',
  'estado del servicio',
  'salud del servicio',
  'estado del sistema',
  'salud',
  'service health',
  'service status',
  'system health',
  'system status',
  'health',
  'datos de demostracion',
  'datos de ejemplo',
  'demo data',
  'demo seed',
  'demo workspace',
  'sample data',
  'example data',
  'seed data',
  'usuarios y roles',
  'usuarios y permisos',
  'permisos de usuarios',
  'gestion de usuarios',
  'administracion de usuarios',
  'usuarios',
  'users and roles',
  'users and permissions',
  'user permissions',
  'user management',
  'user administration',
  'users',
  'roles y permisos',
  'gestion de roles',
  'roles and permissions',
  'role management',
  'control de acceso',
  'gestion de accesos',
  'administracion de accesos',
  'access control',
  'access management',
  'gestion de permisos',
  'administracion de permisos',
  'permissions management',
  'auditoria reciente',
  'historial de auditoria',
  'registro de auditoria',
  'auditoria',
  'recent audit',
  'audit history',
  'audit log',
  'audit',
] as const;
const ADMIN_CONSOLE_PLACEHOLDER_BODY_FRAGMENTS = [
  'estamos trabajando en esta vista',
  'proximamente encontraras la funcionalidad completa aqui',
  'si necesitas priorizar esta seccion',
  'comparte los requisitos con el equipo de producto',
  'working on this view',
  'full functionality will be available here soon',
  'coming soon',
  'proximamente',
  'not implemented yet',
  'not implemented',
  'under construction',
  'work in progress',
  'to be determined',
  'placeholder',
  'tbd',
  'pendiente de implementacion',
  'en desarrollo',
  'en construccion',
  'if you need to prioritize this section',
  'share the requirements with the product team',
  'no data available',
  'no records yet',
  'no records found',
  'no results yet',
  'no results found',
  'nothing to show',
  'no information available',
  'no items yet',
  'no items found',
  'no elements available',
  'empty state',
  'sin informacion disponible',
  'no hay informacion disponible',
  'sin datos disponibles',
  'sin elementos disponibles',
  'sin registros',
  'sin resultados',
  'no hay registros',
  'no hay resultados',
  'no hay datos disponibles',
  'no hay elementos disponibles',
  'aun no hay datos',
  'todavia no hay datos',
] as const;
const BUILT_IN_ADMIN_CARD_BODY_COPY = [
  'valida el estado del servicio antes de cambiar permisos o repetir una accion',
  'check whether the api and database are ready before changing permissions',
  'la asignacion de roles se administra desde la pantalla de parties',
  'administra aqui la asignacion de roles y permisos del equipo',
  'administra accesos y permisos del equipo desde esta vista',
  'ajusta los accesos desde usuarios y roles para resolver el caso actual',
  'adjust team access from the users and roles workflow',
  'confirma el resultado en auditoria reciente antes de seguir con otro cambio',
  'confirm who changed what before repeating an admin action',
  'review service health users roles and audit activity from one admin landing page',
] as const;
const GETTING_STARTED_ADMIN_SECTIONS = [
  { label: '1. Estado del servicio', targetId: 'admin-service-health' },
  { label: '2. Usuarios y roles', targetId: 'admin-users-and-roles' },
  { label: '3. Auditoría reciente', targetId: 'admin-recent-audit' },
] as const;
const FIRST_RUN_USERS_EMPTY_STATE = 'Aún no hay usuarios administrables.';
const FIRST_RUN_AUDIT_EMPTY_STATE = 'La auditoría aparecerá cuando se registre el primer cambio.';
const HEALTHY_HEALTH_INDICATORS = new Set(['ok', 'healthy', 'up', 'ready']);
const SINGLE_ADMIN_USER_INLINE_EDIT_HINT =
  'Primer usuario administrable. Usa el botón del rol para ajustar accesos; cuando exista una segunda cuenta, volverá la tabla comparativa.';
const INLINE_ROLE_SUMMARY_LIMIT = 2;
const AUDIT_ACTION_LABELS: Record<string, string> = {
  'roles.updated': 'Roles actualizados',
};

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

function normalizeBuiltInAdminConsoleSectionKey(value: string) {
  return normalizeAdminConsoleSectionKey(value)
    .replace(/\b(?:and|y|draft|fallback|planned|placeholder|preview|stub)\b/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}

function normalizeAdminConsoleParagraphKey(value: string) {
  return normalizeAdminConsoleSectionKey(value);
}

function getAdminConsoleCardBodyKey(card: Pick<AdminConsoleCard, 'body'>) {
  return card.body
    .map((paragraph) => normalizeAdminConsoleParagraphKey(paragraph))
    .filter((paragraphKey) => paragraphKey.length > 0)
    .join('\n');
}

const BUILT_IN_ADMIN_CARD_ID_KEYS = new Set(
  BUILT_IN_ADMIN_CARD_IDS.map((value) => normalizeBuiltInAdminConsoleSectionKey(value)),
);
const BUILT_IN_ADMIN_CARD_TITLE_KEYS = new Set(
  BUILT_IN_ADMIN_CARD_TITLES.map((value) => normalizeBuiltInAdminConsoleSectionKey(value)),
);
const BUILT_IN_ADMIN_CARD_BODY_KEYS = new Set(
  BUILT_IN_ADMIN_CARD_BODY_COPY.map((value) => normalizeAdminConsoleParagraphKey(value)),
);

function sanitizeAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  return cards.flatMap((card) => {
    const title = card.title.trim();
    const titleParagraphKey = normalizeAdminConsoleParagraphKey(title);
    const seenParagraphs = new Set<string>();
    const body = card.body
      .map((paragraph) => paragraph.trim())
      .filter((paragraph) => paragraph.length > 0)
      .filter((paragraph) => !isPlaceholderAdminConsoleParagraph(paragraph))
      .filter((paragraph) => !isBuiltInAdminConsoleParagraph(paragraph))
      .filter((paragraph) => {
        const paragraphKey = normalizeAdminConsoleParagraphKey(paragraph);

        if (paragraphKey === '' || seenParagraphs.has(paragraphKey)) {
          return false;
        }

        if (titleParagraphKey !== '' && paragraphKey === titleParagraphKey) {
          return false;
        }

        seenParagraphs.add(paragraphKey);
        return true;
      });

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

function isBuiltInAdminConsoleParagraph(paragraph: string) {
  return BUILT_IN_ADMIN_CARD_BODY_KEYS.has(normalizeAdminConsoleParagraphKey(paragraph));
}

function isDedicatedAdminSectionCard(card: AdminConsoleCard) {
  const normalizedId = normalizeBuiltInAdminConsoleSectionKey(card.cardId);
  const normalizedTitle = normalizeBuiltInAdminConsoleSectionKey(card.title);

  return BUILT_IN_ADMIN_CARD_ID_KEYS.has(normalizedId) || BUILT_IN_ADMIN_CARD_TITLE_KEYS.has(normalizedTitle);
}

function dedupeAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  const cardsByTitle = new Map<string, AdminConsoleCard>();
  const titleKeyByBody = new Map<string, string>();
  const titleKeyByCardId = new Map<string, string>();

  cards.forEach((card) => {
    const titleKey = normalizeAdminConsoleSectionKey(card.title);
    const cardIdKey = normalizeAdminConsoleSectionKey(card.cardId);
    const bodyKey = getAdminConsoleCardBodyKey(card);
    const existingTitleKey = cardsByTitle.has(titleKey)
      ? titleKey
      : (titleKeyByBody.get(bodyKey) ?? titleKeyByCardId.get(cardIdKey));
    const existingCard = existingTitleKey ? cardsByTitle.get(existingTitleKey) : undefined;

    if (!existingTitleKey || !existingCard) {
      cardsByTitle.set(titleKey, { ...card, body: [...card.body] });
      if (bodyKey !== '') {
        titleKeyByBody.set(bodyKey, titleKey);
      }
      if (cardIdKey !== '') {
        titleKeyByCardId.set(cardIdKey, titleKey);
      }
      return;
    }

    const seenParagraphs = new Set(
      existingCard.body.map((paragraph) => normalizeAdminConsoleParagraphKey(paragraph)),
    );
    const mergedBody = [...existingCard.body];

    card.body.forEach((paragraph) => {
      const paragraphKey = normalizeAdminConsoleParagraphKey(paragraph);

      if (seenParagraphs.has(paragraphKey)) {
        return;
      }

      seenParagraphs.add(paragraphKey);
      mergedBody.push(paragraph);
    });

    cardsByTitle.set(existingTitleKey, { ...existingCard, body: mergedBody });
    if (bodyKey !== '') {
      titleKeyByBody.set(bodyKey, existingTitleKey);
    }
    if (cardIdKey !== '') {
      titleKeyByCardId.set(cardIdKey, existingTitleKey);
    }
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

function preferNonEmptyAdminUserText(primary?: string | null, fallback?: string | null) {
  return primary?.trim() ? primary : fallback;
}

function mergeAdminUserRecords(primary: AdminUserDTO, duplicate: AdminUserDTO): AdminUserDTO {
  return {
    ...primary,
    username: preferNonEmptyAdminUserText(primary.username, duplicate.username) ?? primary.username,
    displayName: preferNonEmptyAdminUserText(primary.displayName, duplicate.displayName),
    partyId: primary.partyId ?? duplicate.partyId,
    roles: normalizeRoleList([...primary.roles, ...duplicate.roles]),
    status: primary.status ?? duplicate.status,
    lastLoginAt: primary.lastLoginAt ?? duplicate.lastLoginAt,
    lastSeenAt: primary.lastSeenAt ?? duplicate.lastSeenAt,
  };
}

function dedupeAdminUsers(users: readonly AdminUserDTO[]) {
  const usersById = new Map<number, AdminUserDTO>();

  users.forEach((user) => {
    const existingUser = usersById.get(user.userId);

    usersById.set(
      user.userId,
      existingUser ? mergeAdminUserRecords(existingUser, user) : user,
    );
  });

  return [...usersById.values()];
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

function buildAdminUserRoleEditLabel(user: Pick<AdminUserDTO, 'displayName' | 'username'>) {
  return `Editar roles de ${summarizeAdminUserIdentity(user).primary}`;
}

function formatEditableRoleList(roles?: readonly RoleKey[] | null) {
  const formattedRoles = formatRoleList(roles);

  return formattedRoles === '—' ? 'Sin roles' : formattedRoles;
}

function formatInlineEditableRoleList(roles?: readonly RoleKey[] | null) {
  const normalizedRoles = normalizeRoleSelection(roles);

  if (normalizedRoles.length <= INLINE_ROLE_SUMMARY_LIMIT) {
    return formatEditableRoleList(normalizedRoles);
  }

  const visibleRoles = normalizedRoles.slice(0, INLINE_ROLE_SUMMARY_LIMIT).join(', ');
  const hiddenRoleCount = normalizedRoles.length - INLINE_ROLE_SUMMARY_LIMIT;

  return `${visibleRoles} +${hiddenRoleCount} ${hiddenRoleCount === 1 ? 'rol' : 'roles'}`;
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
    return `Usuario #${actorId}`;
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

function formatAuditAction(action: string) {
  const trimmedAction = action.trim();

  return AUDIT_ACTION_LABELS[trimmedAction] ?? formatFallbackAuditAction(trimmedAction);
}

function formatFallbackAuditAction(action: string) {
  const words = action
    .replace(/([a-z])([A-Z])/g, '$1 $2')
    .split(/[._\s-]+/)
    .filter((word) => word.length > 0);

  if (words.length === 0) {
    return action;
  }

  return words
    .map((word, index) => {
      const normalizedWord = word.toLowerCase();
      return index === 0
        ? `${normalizedWord.charAt(0).toUpperCase()}${normalizedWord.slice(1)}`
        : normalizedWord;
    })
    .join(' ');
}

function getAuditActionTitle(action: string) {
  const formattedAction = formatAuditAction(action);

  return formattedAction === action ? undefined : action;
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

function formatRoleGroupLabel(roles: readonly string[]) {
  if (roles.length <= 1) {
    return roles[0] ?? '';
  }

  if (roles.length === 2) {
    return `${roles[0]} y ${roles[1]}`;
  }

  return `${roles.slice(0, -1).join(', ')} y ${roles[roles.length - 1]}`;
}

function buildCompactHiddenColumnsDescription(hiddenColumnLabels: readonly string[]) {
  if (hiddenColumnLabels.length === 0) {
    return null;
  }

  const labels = formatRoleGroupLabel(hiddenColumnLabels);
  const verb = hiddenColumnLabels.length === 1 ? 'aparecerá' : 'aparecerán';
  const contextVerb = hiddenColumnLabels.length === 1 ? 'aporte' : 'aporten';

  return `Vista compacta: ${labels} ${verb} cuando ${contextVerb} contexto.`;
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
  const hiddenColumnLabels: string[] = [];

  if (!showLastAccessColumn) {
    hiddenColumnLabels.push('último acceso');
  }

  if (!showStatusColumn) {
    hiddenColumnLabels.push('estado');
  }

  return buildCompactHiddenColumnsDescription(hiddenColumnLabels);
}

function buildAuditSectionDescription({
  showActorColumn,
  showDetailColumn,
}: {
  showActorColumn: boolean;
  showDetailColumn: boolean;
}) {
  const hiddenColumnLabels: string[] = [];

  if (!showActorColumn) {
    hiddenColumnLabels.push('actor');
  }

  if (!showDetailColumn) {
    hiddenColumnLabels.push('detalle');
  }

  return buildCompactHiddenColumnsDescription(hiddenColumnLabels);
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

function renderSectionLoading(label: string) {
  return (
    <Box sx={{ px: 2, pb: 2 }}>
      <Stack direction="row" alignItems="center" justifyContent="center" spacing={1} sx={{ py: 2 }}>
        <CircularProgress size={18} />
        <Typography variant="body2" color="text.secondary">
          {label}
        </Typography>
      </Stack>
    </Box>
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
  const showUsersTable = users.length > 1;
  const showUsersLastAccessColumn = users.some((user) => getAdminUserLastAccess(user) != null);
  const showUsersStatusColumn = users.some((user) => user.status != null && user.status !== 'ACTIVE');
  const singleAuditEntry = !auditQuery.isLoading && audits.length === 1 ? (audits[0] ?? null) : null;
  const singleAuditHasActor = hasAuditActor(singleAuditEntry?.actorId);
  const singleAuditHasDetail = hasAuditDetail(singleAuditEntry?.diff);
  const showAuditTable = audits.length > 1;
  const showAuditActorColumn = audits.some((entry) => hasAuditActor(entry.actorId));
  const showAuditDetailColumn = audits.some((entry) => hasAuditDetail(entry.diff));
  const isAdminPanelBaselining = consoleQuery.isPending || usersQuery.isLoading || auditQuery.isLoading;
  const hasAdminPanelError =
    healthQuery.isError
    || auditQuery.isError
    || consoleQuery.isError
    || usersQuery.isError;
  const shouldShowHealthLoadingState = healthQuery.isPending && healthQuery.data == null;
  const showCompactHealthyServiceSummary =
    healthQuery.data != null
    && isHealthyHealthIndicator(healthQuery.data.status)
    && isHealthyHealthIndicator(healthQuery.data.db);
  const showGettingStartedGuidance =
    !consoleQuery.isPending
    && !usersQuery.isLoading
    && !auditQuery.isLoading
    && users.length === 0
    && audits.length === 0;
  const hasFirstRunDataError = usersQuery.isError || auditQuery.isError;
  const showFirstRunDemoAction =
    showGettingStartedGuidance
    && showCompactHealthyServiceSummary
    && !hasFirstRunDataError
    && !seedMutation.isSuccess;
  const showFirstRunServiceHealthGate =
    showGettingStartedGuidance && !showCompactHealthyServiceSummary;
  const firstRunServiceNeedsRefresh =
    showFirstRunServiceHealthGate && !shouldShowHealthLoadingState;
  const showHeaderRefreshAction =
    hasAdminPanelError || (!isAdminPanelBaselining && (!showGettingStartedGuidance || firstRunServiceNeedsRefresh));
  const showHeaderActions = showHeaderRefreshAction;
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
        ? buildAuditSectionDescription({
          showActorColumn: showAuditActorColumn,
          showDetailColumn: showAuditDetailColumn,
        })
        : null
    );
  const firstRunAdditionalModulesActionLabel = formatFirstRunAdditionalModulesActionLabel(consoleCards);
  const canShowFirstRunAdditionalModules =
    showGettingStartedGuidance && showCompactHealthyServiceSummary;
  const shouldShowAdditionalModuleCards =
    consoleCards.length > 0
    && (!showGettingStartedGuidance || (canShowFirstRunAdditionalModules && showFirstRunAdditionalModules));
  const showFirstRunAdditionalModulesShowAction =
    canShowFirstRunAdditionalModules
    && consoleCards.length > 0
    && !shouldShowAdditionalModuleCards;
  const showFirstRunAdditionalModulesHideAction =
    canShowFirstRunAdditionalModules
    && shouldShowAdditionalModuleCards;
  const showStandaloneAdditionalModulesSection = consoleCards.length > 0 && !showGettingStartedGuidance;
  const standaloneAdditionalModulesActionLabel = formatStandaloneAdditionalModulesActionLabel(consoleCards);
  const firstRunAdditionalModuleSignature = JSON.stringify(
    consoleCards.map((card) => [card.cardId, card.title, card.body]),
  );
  useEffect(() => {
    if (showGettingStartedGuidance) {
      setShowFirstRunAdditionalModules(false);
    }
  }, [firstRunAdditionalModuleSignature, showGettingStartedGuidance]);
  useEffect(() => {
    if (consoleCards.length === 0) {
      setShowFirstRunAdditionalModules(false);
      setShowStandaloneAdditionalModules(false);
    }
  }, [consoleCards.length]);
  const editingTitle = useMemo(() => {
    if (!editingUser) return '';
    return editingUser.displayName?.trim() || editingUser.username;
  }, [editingUser]);
  const currentRoleSummary = useMemo(() => (
    editingUser ? formatEditableRoleList(editingUser.roles) : 'Sin roles'
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
  const firstRunDemoActionCopy = {
    description: 'Opcional: carga datos de ejemplo para revisar usuarios, roles y auditoría sin tocar producción.',
    buttonLabel: 'Cargar datos de ejemplo',
    pendingLabel: 'Cargando ejemplo…',
  } as const;
  const firstRunServiceGateCopy = shouldShowHealthLoadingState
    ? 'Espera la comprobación de API y base de datos antes de cargar datos de ejemplo.'
    : 'Primero resuelve el estado del servicio; luego podrás cargar datos de ejemplo con la API y base de datos listas.';
  const firstRunDataGateCopy = 'Actualiza el panel para confirmar usuarios y auditoría antes de cargar datos de ejemplo.';
  const firstRunDemoStatusCopy = seedMutation.isSuccess
    ? null
    : hasFirstRunDataError
      ? firstRunDataGateCopy
      : showFirstRunDemoAction
        ? firstRunDemoActionCopy.description
        : firstRunServiceGateCopy;
  const demoSeedActionCopy = {
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
          </Stack>
        )}
      </Stack>

      {rotationWarning && (
        <Alert severity="warning" onClose={() => setRotationWarning(false)}>
          Tu sesión lleva 30 días activa. Genera un nuevo token o vuelve a autenticarte para mantener la seguridad.
        </Alert>
      )}

      {seedMutation.isSuccess && (
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
            {(firstRunDemoStatusCopy || showFirstRunDemoAction) && (
              <Stack spacing={1} alignItems="flex-start">
                {firstRunDemoStatusCopy && (
                  <Typography variant="body2" color="text.secondary">
                    {firstRunDemoStatusCopy}
                  </Typography>
                )}
                {showFirstRunDemoAction && (
                  <Button
                    variant="contained"
                    size="small"
                    startIcon={<AutoFixHighIcon />}
                    onClick={() => seedMutation.mutate()}
                    disabled={seedMutation.isPending}
                  >
                    {seedMutation.isPending ? firstRunDemoActionCopy.pendingLabel : firstRunDemoActionCopy.buttonLabel}
                  </Button>
                )}
              </Stack>
            )}
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
        {isUsersLoading ? (
          renderSectionLoading('Cargando usuarios…')
        ) : showUsersTable ? (
          <TableContainer>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Usuario</TableCell>
                  <TableCell>Roles</TableCell>
                  {showUsersLastAccessColumn && <TableCell>Último acceso</TableCell>}
                  {showUsersStatusColumn && <TableCell>Estado</TableCell>}
                </TableRow>
              </TableHead>
              <TableBody>
                {users.map((user) => {
                  const identity = summarizeAdminUserIdentity(user);
                  const editRoleLabel = buildAdminUserRoleEditLabel(user);
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
                          endIcon={<EditOutlinedIcon fontSize="small" />}
                          onClick={() => setEditingUser(user)}
                          aria-label={editRoleLabel}
                          title={editRoleLabel}
                          sx={{
                            px: 0,
                            minWidth: 0,
                            justifyContent: 'flex-start',
                            textTransform: 'none',
                          }}
                        >
                          {formatInlineEditableRoleList(user.roles)}
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
                <Button
                  size="small"
                  endIcon={<EditOutlinedIcon fontSize="small" />}
                  onClick={() => setEditingUser(singleAdminUser)}
                  aria-label={buildAdminUserRoleEditLabel(singleAdminUser)}
                  title={buildAdminUserRoleEditLabel(singleAdminUser)}
                  sx={{
                    px: 0,
                    minWidth: 0,
                    textTransform: 'none',
                  }}
                >
                  {formatInlineEditableRoleList(singleAdminUser.roles)}
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
        {auditQuery.isLoading ? (
          renderSectionLoading('Cargando auditoría…')
        ) : showAuditTable ? (
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
                    <TableCell>
                      <Typography variant="body2" title={getAuditActionTitle(entry.action)}>
                        {formatAuditAction(entry.action)}
                      </Typography>
                    </TableCell>
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
                <Typography variant="body2" title={getAuditActionTitle(singleAuditEntry.action)}>
                  <Box component="span" sx={{ fontWeight: 600 }}>Acción:</Box>{' '}
                  {formatAuditAction(singleAuditEntry.action)}
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
