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
  'configuracion del sistema',
  'configuracion del espacio de trabajo',
  'configuration',
  'system configuration',
  'settings',
  'preferencias',
  'preferences',
  'ajustes',
  'workspace configuration',
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
  'roles y accesos',
  'accesos y roles',
  'gestion de roles',
  'roles and permissions',
  'roles and access',
  'access and roles',
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
  'debe administrarse desde un flujo dedicado',
  'deben administrarse desde un flujo dedicado',
  'should be managed from a dedicated flow',
  'quedara separado',
  'will be separated',
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
  'access denied',
  'not authorized',
  'unauthorized',
  'forbidden',
  'you do not have permission',
  'you don\'t have permission',
  'insufficient permissions',
  'permission required',
  'requires permission',
  'requires additional permissions',
  'temporarily unavailable',
  'not available',
  'acceso denegado',
  'no autorizado',
  'no tienes permiso',
  'no tienes permisos',
  'sin permisos suficientes',
  'permiso requerido',
  'permisos requeridos',
  'temporalmente no disponible',
  'no disponible',
] as const;
const BUILT_IN_ADMIN_CARD_BODY_COPY = [
  'revisa el estado del sistema ajusta permisos y valida cambios recientes desde un solo lugar',
  'review system status adjust permissions and validate recent changes from one place',
  'sigue este recorrido para ubicar cada bloque sin repetir revisiones vacias',
  'follow this walkthrough to find each block without repeating empty reviews',
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
const MAX_SINGLE_ADDITIONAL_MODULE_ACTION_TITLE_LENGTH = 32;
const FIRST_RUN_USERS_EMPTY_STATE = 'Aún no hay usuarios administrables.';
const FIRST_RUN_AUDIT_EMPTY_STATE = 'La auditoría aparecerá cuando se registre el primer cambio.';
const HEALTHY_HEALTH_INDICATORS = new Set(['ok', 'healthy', 'up', 'ready']);
const WARNING_HEALTH_INDICATORS = new Set(['degraded', 'warning', 'warn', 'starting']);
const ERROR_HEALTH_INDICATORS = new Set(['down', 'offline', 'error', 'failed', 'fail', 'unhealthy']);
const ADMIN_USER_ROLES_COLUMN_HEADER = 'Roles';
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
    .replace(
      /\b(?:and|y|draft|fallback|planned|placeholder|preview|stub|module|modules|modulo|modulos|section|sections|seccion|secciones|page|pages|pagina|paginas|screen|screens|pantalla|pantallas|view|views|vista|vistas|card|cards|tarjeta|tarjetas)\b/g,
      ' ',
    )
    .replace(/\s+/g, ' ')
    .trim();
}

function normalizeAdminConsoleParagraphKey(value: string) {
  return normalizeAdminConsoleSectionKey(value);
}

function getAdminConsoleCardBodyKey(card: Pick<AdminConsoleCard, 'body'>) {
  return [...new Set(
    card.body
      .map((paragraph) => normalizeAdminConsoleParagraphKey(paragraph))
      .filter((paragraphKey) => paragraphKey.length > 0),
  )]
    .sort()
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

function compareAdminUsers(left: AdminUserDTO, right: AdminUserDTO) {
  const leftIdentity = summarizeAdminUserIdentity(left);
  const rightIdentity = summarizeAdminUserIdentity(right);
  const leftPrimary = leftIdentity.primary.trim().toLowerCase();
  const rightPrimary = rightIdentity.primary.trim().toLowerCase();

  if (leftPrimary !== rightPrimary) {
    return leftPrimary.localeCompare(rightPrimary);
  }

  const leftUsername = leftIdentity.username.trim().toLowerCase();
  const rightUsername = rightIdentity.username.trim().toLowerCase();

  if (leftUsername !== rightUsername) {
    return leftUsername.localeCompare(rightUsername);
  }

  return left.userId - right.userId;
}

function sortAdminUsers(users: readonly AdminUserDTO[]) {
  return [...users].sort(compareAdminUsers);
}

function preferRicherAuditDiff(primary?: string | null, fallback?: string | null) {
  const normalizedPrimary = primary?.trim() ?? '';
  const normalizedFallback = fallback?.trim() ?? '';

  if (normalizedPrimary === '') {
    return fallback ?? primary;
  }

  if (normalizedFallback === '') {
    return primary;
  }

  return normalizedFallback.length > normalizedPrimary.length ? fallback : primary;
}

function mergeAuditEntries(primary: AuditLogEntry, duplicate: AuditLogEntry): AuditLogEntry {
  return {
    ...primary,
    auditId: preferNonEmptyAdminUserText(primary.auditId, duplicate.auditId) ?? primary.auditId,
    actorId: primary.actorId ?? duplicate.actorId,
    entity: preferNonEmptyAdminUserText(primary.entity, duplicate.entity) ?? primary.entity,
    entityId: preferNonEmptyAdminUserText(primary.entityId, duplicate.entityId) ?? primary.entityId,
    action: preferNonEmptyAdminUserText(primary.action, duplicate.action) ?? primary.action,
    diff: preferRicherAuditDiff(primary.diff, duplicate.diff),
    createdAt: preferNonEmptyAdminUserText(primary.createdAt, duplicate.createdAt) ?? primary.createdAt,
  };
}

function getAuditEntryFingerprint(entry: AuditLogEntry) {
  return [
    entry.entity,
    entry.entityId,
    entry.action,
    entry.actorId ?? '',
    entry.diff ?? '',
    entry.createdAt,
  ].join('::');
}

function dedupeAuditEntries(entries: readonly AuditLogEntry[]) {
  const dedupedEntries: AuditLogEntry[] = [];
  const entryIndexByAuditId = new Map<string, number>();
  const entryIndexByFingerprint = new Map<string, number>();

  entries.forEach((entry) => {
    const auditIdKey = entry.auditId?.trim() ?? '';

    if (auditIdKey !== '') {
      const existingIndex = entryIndexByAuditId.get(auditIdKey);

      if (existingIndex != null) {
        const existingEntry = dedupedEntries[existingIndex];

        if (existingEntry) {
          const mergedEntry = mergeAuditEntries(existingEntry, entry);
          dedupedEntries[existingIndex] = mergedEntry;
          entryIndexByFingerprint.set(getAuditEntryFingerprint(mergedEntry), existingIndex);
        }

        return;
      }
    }

    const fingerprint = getAuditEntryFingerprint(entry);

    if (entryIndexByFingerprint.has(fingerprint)) {
      return;
    }

    const index = dedupedEntries.length;
    dedupedEntries.push(entry);
    entryIndexByFingerprint.set(fingerprint, index);

    if (auditIdKey !== '') {
      entryIndexByAuditId.set(auditIdKey, index);
    }
  });

  return dedupedEntries;
}

function getAuditEntryTimestamp(entry: Pick<AuditLogEntry, 'createdAt'>) {
  const timestamp = Date.parse(entry.createdAt);

  return Number.isNaN(timestamp) ? Number.NEGATIVE_INFINITY : timestamp;
}

function compareAuditEntries(left: AuditLogEntry, right: AuditLogEntry) {
  const timestampDifference = getAuditEntryTimestamp(right) - getAuditEntryTimestamp(left);

  if (timestampDifference !== 0) {
    return timestampDifference;
  }

  const leftEntity = `${left.entity}::${left.entityId}`.toLowerCase();
  const rightEntity = `${right.entity}::${right.entityId}`.toLowerCase();

  if (leftEntity !== rightEntity) {
    return leftEntity.localeCompare(rightEntity);
  }

  const leftAction = left.action.trim().toLowerCase();
  const rightAction = right.action.trim().toLowerCase();

  if (leftAction !== rightAction) {
    return leftAction.localeCompare(rightAction);
  }

  return (left.auditId ?? '').localeCompare(right.auditId ?? '');
}

function sortAuditEntries(entries: readonly AuditLogEntry[]) {
  return [...entries].sort(compareAuditEntries);
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

function buildAdminUserRoleButtonTitle(user: Pick<AdminUserDTO, 'displayName' | 'username' | 'roles'>) {
  const roleSummary = formatEditableRoleList(user.roles);

  return `${buildAdminUserRoleEditLabel(user)}. Roles actuales: ${roleSummary}`;
}

function formatEditableRoleList(roles?: readonly RoleKey[] | null) {
  const formattedRoles = formatRoleList(roles);

  return formattedRoles === '—' ? 'Sin roles' : formattedRoles;
}

function formatInlineEditableRoleList(roles?: readonly RoleKey[] | null) {
  const normalizedRoles = normalizeRoleSelection(roles);

  if (normalizedRoles.length === 0) {
    return 'Asignar roles';
  }

  if (normalizedRoles.length <= INLINE_ROLE_SUMMARY_LIMIT) {
    return formatEditableRoleList(normalizedRoles);
  }

  const visibleRoles = normalizedRoles.slice(0, INLINE_ROLE_SUMMARY_LIMIT).join(', ');
  const hiddenRoleCount = normalizedRoles.length - INLINE_ROLE_SUMMARY_LIMIT;

  return `${visibleRoles} +${hiddenRoleCount} ${hiddenRoleCount === 1 ? 'rol' : 'roles'}`;
}

function formatInlineAdminUserRoleSummary(roles?: readonly RoleKey[] | null) {
  const normalizedRoles = normalizeRoleSelection(roles);

  if (normalizedRoles.length === 0) {
    return 'Sin roles';
  }

  return formatInlineEditableRoleList(normalizedRoles);
}

function buildAdminUserRoleActionLabel(roles?: readonly RoleKey[] | null) {
  return normalizeRoleSelection(roles).length === 0 ? 'Asignar roles' : 'Editar roles';
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

function getHealthStatusChipColor(value?: string | null): 'default' | 'success' | 'warning' | 'error' {
  const normalizedValue = normalizeHealthIndicator(value);

  if (normalizedValue === '') {
    return 'default';
  }

  if (HEALTHY_HEALTH_INDICATORS.has(normalizedValue)) {
    return 'success';
  }

  if (ERROR_HEALTH_INDICATORS.has(normalizedValue)) {
    return 'error';
  }

  if (WARNING_HEALTH_INDICATORS.has(normalizedValue)) {
    return 'warning';
  }

  return 'warning';
}

function normalizeRoleSelection(roles?: readonly RoleKey[] | null) {
  return normalizeRoleList(roles);
}

function sortRoleOptionsForEditor(selectedRoles: readonly RoleKey[]) {
  const selectedRoleSet = new Set(normalizeRoleSelection(selectedRoles));

  return [...ROLE_OPTIONS].sort((left, right) => {
    const leftSelected = selectedRoleSet.has(left.value);
    const rightSelected = selectedRoleSet.has(right.value);

    if (leftSelected === rightSelected) {
      return 0;
    }

    return leftSelected ? -1 : 1;
  });
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

function buildServiceHealthWarningMessage({
  apiStatus,
  dbStatus,
}: {
  apiStatus?: string | null;
  dbStatus?: string | null;
}) {
  const dependenciesNeedingAttention = [
    !isHealthyHealthIndicator(apiStatus) ? 'API' : null,
    !isHealthyHealthIndicator(dbStatus) ? 'base de datos' : null,
  ].filter((dependency): dependency is string => dependency != null);

  if (dependenciesNeedingAttention.length === 0) {
    return null;
  }

  const dependenciesLabel = formatRoleGroupLabel(dependenciesNeedingAttention);
  const verb = dependenciesNeedingAttention.length === 1 ? 'requiere' : 'requieren';

  return `Atención: ${dependenciesLabel} ${verb} revisión antes de cambiar permisos o seguir con otras acciones administrativas.`;
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
  isSingleUserSummary,
  primaryRoleActionLabel = 'Editar roles',
}: {
  showLastAccessColumn: boolean;
  showStatusColumn: boolean;
  isSingleUserSummary?: boolean;
  primaryRoleActionLabel?: string;
}) {
  const editHint = `Revisa los roles actuales y usa ${primaryRoleActionLabel} para ajustar permisos desde esta misma vista.`;
  const hiddenColumnLabels: string[] = [];

  if (!showLastAccessColumn) {
    hiddenColumnLabels.push('último acceso');
  }

  if (!showStatusColumn) {
    hiddenColumnLabels.push('estado');
  }

  const compactViewHint = buildCompactHiddenColumnsDescription(hiddenColumnLabels);

  if (isSingleUserSummary) {
    return compactViewHint ? `${editHint} ${compactViewHint}` : editHint;
  }

  return compactViewHint ? `${editHint} ${compactViewHint}` : editHint;
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

function formatAdditionalModuleCountLabel(count: number) {
  return count === 1 ? '1 módulo adicional' : `${count} módulos adicionales`;
}

function shouldCollapseSingleAdditionalModuleActionTitle(title: string) {
  return title.trim().length > MAX_SINGLE_ADDITIONAL_MODULE_ACTION_TITLE_LENGTH;
}

function formatCompactAdditionalModuleActionTitle(title: string) {
  const trimmedTitle = title.trim();

  if (trimmedTitle.length <= MAX_SINGLE_ADDITIONAL_MODULE_ACTION_TITLE_LENGTH) {
    return trimmedTitle;
  }

  const visibleTitleLength = Math.max(MAX_SINGLE_ADDITIONAL_MODULE_ACTION_TITLE_LENGTH - 3, 1);
  return `${trimmedTitle.slice(0, visibleTitleLength).trimEnd()}...`;
}

function buildAdditionalModulesActionCopy({
  cards,
  optionalPrefix,
  avoidSingleTitleRepeat = false,
}: {
  cards: readonly Pick<AdminConsoleCard, 'title'>[];
  optionalPrefix: boolean;
  avoidSingleTitleRepeat?: boolean;
}) {
  const count = cards.length;
  const prefix = optionalPrefix ? 'Opcional: ver' : 'Ver';

  if (count === 0) {
    return { label: `${prefix} módulos adicionales` };
  }

  if (count === 1) {
    const title = cards[0]?.title?.trim() ?? '';

    if (avoidSingleTitleRepeat && title) {
      return {
        label: 'Ver detalles',
        title: `Ver detalles de ${title}`,
        ariaLabel: `Ver detalles de ${title}`,
      };
    }

    const shouldCollapseTitle = shouldCollapseSingleAdditionalModuleActionTitle(title);
    const summary = title
      ? (
        shouldCollapseTitle
          ? formatCompactAdditionalModuleActionTitle(title)
          : title
      )
      : 'módulo adicional';

    return {
      label: `${prefix} ${summary}`,
      title: shouldCollapseTitle && title ? `${prefix} ${title}` : undefined,
      ariaLabel: shouldCollapseTitle && title ? `${prefix} ${title}` : undefined,
    };
  }

  return { label: `${prefix} ${formatAdditionalModuleCountLabel(count)}` };
}

function formatFirstRunAdditionalModulesActionCopy(cards: readonly Pick<AdminConsoleCard, 'title'>[]) {
  if (cards.length === 1) {
    return {
      label: 'Opcional: ver 1 módulo adicional',
      title: undefined,
      ariaLabel: undefined,
    } as const;
  }

  return buildAdditionalModulesActionCopy({ cards, optionalPrefix: true });
}

function formatStandaloneAdditionalModulesActionCopy(cards: readonly Pick<AdminConsoleCard, 'title'>[]) {
  return buildAdditionalModulesActionCopy({
    cards,
    optionalPrefix: false,
    avoidSingleTitleRepeat: true,
  });
}

function getSingleAdditionalModule(cards: readonly AdminConsoleCard[]) {
  return cards.length === 1 ? (cards[0] ?? null) : null;
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
  hideSingleCardHeader,
}: {
  cards: readonly AdminConsoleCard[];
  isFetching: boolean;
  isPending: boolean;
  id?: string;
  sx?: SxProps<Theme>;
  hideSingleCardHeader?: boolean;
}) {
  const singleCard = hideSingleCardHeader ? getSingleAdditionalModule(cards) : null;

  return (
    <Box id={id} sx={sx}>
      {isFetching && !isPending && (
        <Typography
          variant="caption"
          color="text.secondary"
          sx={{ display: 'block', mb: 1.5 }}
        >
          Actualizando módulos…
        </Typography>
      )}
      <Grid container spacing={2}>
        {cards.map((card) => (
          <Grid item xs={12} md={4} key={card.cardId}>
            <Card variant="outlined">
              {singleCard == null && <CardHeader title={card.title} />}
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
              </CardContent>
            </Card>
          </Grid>
        ))}
      </Grid>
    </Box>
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

function renderFirstRunSectionStatus(label: string, testId: string) {
  return (
    <Box sx={{ px: 2, pb: 2 }}>
      <Chip
        data-testid={testId}
        label={label}
        size="small"
        variant="outlined"
      />
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

  const audits = sortAuditEntries(dedupeAuditEntries(auditQuery.data ?? []));
  const previewCards = sortAdminConsoleCards(dedupeAdminConsoleCards(
    sanitizeAdminConsoleCards(
      consoleQuery.data?.cards?.filter((card) => !isDedicatedAdminSectionCard(card)) ?? [],
    ),
  ));
  const consoleCards: AdminConsoleCard[] = previewCards;
  const consoleError = consoleQuery.isError ? (consoleQuery.error as Error).message : null;
  const users = sortAdminUsers(dedupeAdminUsers(usersQuery.data ?? []));
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
  const serviceHealthWarningMessage = buildServiceHealthWarningMessage({
    apiStatus: healthQuery.data?.status,
    dbStatus: healthQuery.data?.db,
  });
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
  const gettingStartedSections = showFirstRunServiceHealthGate
    ? GETTING_STARTED_ADMIN_SECTIONS.slice(0, 1)
    : GETTING_STARTED_ADMIN_SECTIONS;
  const firstRunServiceNeedsRefresh =
    showFirstRunServiceHealthGate && !shouldShowHealthLoadingState;
  const showHeaderRefreshAction =
    hasAdminPanelError || (!isAdminPanelBaselining && (!showGettingStartedGuidance || firstRunServiceNeedsRefresh));
  const showFirstRunRefreshAction = showGettingStartedGuidance && showHeaderRefreshAction;
  const showHeaderActions = showHeaderRefreshAction && !showGettingStartedGuidance;
  const usersSectionDescription = showGettingStartedGuidance || users.length === 0
    ? null
    : buildAdminUsersSectionDescription({
      showLastAccessColumn: showUsersLastAccessColumn,
      showStatusColumn: showUsersStatusColumn,
      isSingleUserSummary: singleAdminUser != null && !showUsersTable,
      primaryRoleActionLabel: singleAdminUser != null && !showUsersTable
        ? buildAdminUserRoleActionLabel(singleAdminUser.roles)
        : 'Editar roles',
    });
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
  const firstRunAdditionalModulesActionCopy = formatFirstRunAdditionalModulesActionCopy(consoleCards);
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
  const standaloneAdditionalModulesActionCopy = formatStandaloneAdditionalModulesActionCopy(consoleCards);
  const singleAdditionalModule = getSingleAdditionalModule(consoleCards);
  const firstRunAdditionalModulesTitle = singleAdditionalModule?.title ?? 'Módulos opcionales';
  const firstRunAdditionalModulesDescription = singleAdditionalModule
    ? 'Revísalo aquí solo si ya necesitas este flujo extra, sin salir del recorrido inicial.'
    : 'Revísalos aquí solo si ya necesitas ese flujo extra, sin salir del recorrido inicial.';
  const firstRunAdditionalModulesHideLabel = singleAdditionalModule
    ? 'Ocultar módulo opcional'
    : 'Ocultar módulos adicionales';
  const standaloneAdditionalModulesTitle = singleAdditionalModule?.title ?? 'Módulos adicionales';
  const standaloneAdditionalModulesDescription = singleAdditionalModule
    ? 'Tarjeta auxiliar del panel. Ábrela solo cuando ya confirmaste salud, usuarios y auditoría.'
    : 'Tarjetas auxiliares del panel. Ábrelas solo cuando ya confirmaste salud, usuarios y auditoría.';
  const standaloneAdditionalModulesHideLabel = singleAdditionalModule
    ? 'Ocultar módulo adicional'
    : 'Ocultar módulos adicionales';
  const showStandaloneAdditionalModulesOptionalChip = singleAdditionalModule != null;
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
  const selectedRoleFullSummary = formatEditableRoleList(selectedRoles);
  const selectedRoleControlSummary = hasPendingRoleChanges
    ? selectedRoleFullSummary
    : formatInlineEditableRoleList(selectedRoles);
  const selectedRoleControlTitle = selectedRoleControlSummary === selectedRoleFullSummary
    ? undefined
    : selectedRoleFullSummary;
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
  const roleOptionsForEditor = useMemo(
    () => sortRoleOptionsForEditor(selectedRoles),
    [selectedRoles],
  );
  const isRefreshingPanel =
    healthQuery.isFetching
    || auditQuery.isFetching
    || consoleQuery.isFetching
    || usersQuery.isFetching;
  const firstRunDemoActionCopy = {
    description: 'Carga datos de ejemplo para revisar usuarios, roles y auditoría sin tocar producción.',
    buttonLabel: 'Cargar datos de ejemplo (opcional)',
    pendingLabel: 'Cargando ejemplo…',
  } as const;
  const firstRunServiceGateCopy = shouldShowHealthLoadingState
    ? 'Espera la comprobación de API y base de datos para habilitar usuarios, auditoría y datos de ejemplo.'
    : 'Primero resuelve el estado del servicio; luego se habilitarán usuarios, auditoría y datos de ejemplo.';
  const firstRunDataGateCopy = 'Actualiza el panel para confirmar usuarios y auditoría antes de cargar datos de ejemplo.';
  const firstRunDemoStatusCopy = seedMutation.isSuccess
    ? null
    : hasFirstRunDataError
      ? firstRunDataGateCopy
      : showFirstRunDemoAction
        ? null
        : firstRunServiceGateCopy;
  const firstRunRefreshActionCopy = hasFirstRunDataError
    ? {
      label: 'Reintentar carga inicial',
      ariaLabel: 'Reintentar carga de usuarios y auditoría',
      title: 'Reintentar carga de usuarios y auditoría',
    }
    : {
      label: 'Revisar estado del servicio',
      ariaLabel: 'Volver a comprobar API y base de datos',
      title: 'Volver a comprobar API y base de datos',
    };
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
              {gettingStartedSections.map((section) => (
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
                aria-label={firstRunAdditionalModulesActionCopy.ariaLabel}
                title={firstRunAdditionalModulesActionCopy.title}
                sx={{ alignSelf: 'flex-start' }}
              >
                {firstRunAdditionalModulesActionCopy.label}
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
                    <Typography variant="subtitle2">{firstRunAdditionalModulesTitle}</Typography>
                    <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                      {firstRunAdditionalModulesDescription}
                    </Typography>
                  </Box>
                  <Button
                    size="small"
                    variant="text"
                    onClick={() => setShowFirstRunAdditionalModules(false)}
                    aria-controls="admin-additional-modules-list"
                    aria-expanded="true"
                  >
                    {firstRunAdditionalModulesHideLabel}
                  </Button>
                </Stack>
                {renderAdditionalModuleCardsGrid({
                  cards: consoleCards,
                  isFetching: consoleQuery.isFetching,
                  isPending: consoleQuery.isPending,
                  id: 'admin-additional-modules-list',
                  hideSingleCardHeader: true,
                })}
              </Stack>
            )}
            {(firstRunDemoStatusCopy || showFirstRunDemoAction || showFirstRunRefreshAction) && (
              <Stack spacing={1} alignItems="flex-start">
                {firstRunDemoStatusCopy && (
                  <Typography variant="body2" color="text.secondary">
                    {firstRunDemoStatusCopy}
                  </Typography>
                )}
                {showFirstRunRefreshAction && (
                  <Button
                    variant="outlined"
                    size="small"
                    startIcon={<RefreshIcon />}
                    onClick={handleRefreshPanel}
                    disabled={isRefreshingPanel}
                    aria-label={firstRunRefreshActionCopy.ariaLabel}
                    title={firstRunRefreshActionCopy.title}
                  >
                    {isRefreshingPanel ? 'Actualizando panel…' : firstRunRefreshActionCopy.label}
                  </Button>
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
              <Stack spacing={1.25}>
                {serviceHealthWarningMessage && (
                  <Typography variant="body2">
                    {serviceHealthWarningMessage}
                  </Typography>
                )}
                <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                  <Chip
                    data-testid="admin-service-health-chip"
                    label={`API: ${healthQuery.data?.status ?? '—'}`}
                    color={getHealthStatusChipColor(healthQuery.data?.status)}
                    size="small"
                    variant={isHealthyHealthIndicator(healthQuery.data?.status) ? 'outlined' : 'filled'}
                  />
                  <Chip
                    data-testid="admin-service-health-chip"
                    label={`Base de datos: ${healthQuery.data?.db ?? '—'}`}
                    color={getHealthStatusChipColor(healthQuery.data?.db)}
                    size="small"
                    variant={isHealthyHealthIndicator(healthQuery.data?.db) ? 'outlined' : 'filled'}
                  />
                </Stack>
              </Stack>
            ) : null}
            {healthQuery.isError && (
              <Alert severity="error" sx={{ mt: 2 }}>
                {(healthQuery.error as Error).message}
              </Alert>
            )}
          </CardContent>
        </Card>
      </Box>

      {!showFirstRunServiceHealthGate && (
        <>
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
                      <TableCell>{ADMIN_USER_ROLES_COLUMN_HEADER}</TableCell>
                      {showUsersLastAccessColumn && <TableCell>Último acceso</TableCell>}
                      {showUsersStatusColumn && <TableCell>Estado</TableCell>}
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {users.map((user) => {
                      const identity = summarizeAdminUserIdentity(user);
                      const editRoleLabel = buildAdminUserRoleEditLabel(user);
                      const editRoleTitle = buildAdminUserRoleButtonTitle(user);
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
                            <Stack spacing={0.5} alignItems="flex-start">
                              <Typography
                                variant="body2"
                                color="text.secondary"
                                title={formatEditableRoleList(user.roles)}
                              >
                                {formatInlineAdminUserRoleSummary(user.roles)}
                              </Typography>
                              <Button
                                size="small"
                                onClick={() => setEditingUser(user)}
                                aria-label={editRoleLabel}
                                title={editRoleTitle}
                                sx={{
                                  px: 0,
                                  minWidth: 0,
                                  justifyContent: 'flex-start',
                                  textTransform: 'none',
                                }}
                              >
                                {buildAdminUserRoleActionLabel(user.roles)}
                              </Button>
                            </Stack>
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
                    <Typography variant="body2" color="text.secondary">
                      Roles: {formatEditableRoleList(singleAdminUser.roles)}
                    </Typography>
                    <Button
                      size="small"
                      onClick={() => setEditingUser(singleAdminUser)}
                      aria-label={buildAdminUserRoleEditLabel(singleAdminUser)}
                      title={buildAdminUserRoleButtonTitle(singleAdminUser)}
                      sx={{
                        px: 0,
                        minWidth: 0,
                        textTransform: 'none',
                      }}
                    >
                      {buildAdminUserRoleActionLabel(singleAdminUser.roles)}
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
              showGettingStartedGuidance
                ? renderFirstRunSectionStatus(FIRST_RUN_USERS_EMPTY_STATE, 'admin-first-run-users-status')
                : (
                  <Box sx={{ px: 2, pb: 2 }}>
                    <Typography variant="body2" color="text.secondary">
                      Todavía no hay usuarios administrables. Cuando exista el primero, aquí verás roles, último acceso y el atajo para editar roles.
                    </Typography>
                  </Box>
                )
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
              showGettingStartedGuidance
                ? renderFirstRunSectionStatus(FIRST_RUN_AUDIT_EMPTY_STATE, 'admin-first-run-audit-status')
                : (
                  <Box sx={{ px: 2, pb: 2 }}>
                    <Typography variant="body2" color="text.secondary">
                      Todavía no hay eventos de auditoría. Cuando alguien cambie permisos o datos del sistema, aquí verás quién hizo qué y cuándo.
                    </Typography>
                  </Box>
                )
            ) : null}
          </Paper>
        </>
      )}

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
                <Stack direction="row" spacing={1} alignItems="center" useFlexGap flexWrap="wrap">
                  <Typography variant="h6">{standaloneAdditionalModulesTitle}</Typography>
                  {showStandaloneAdditionalModulesOptionalChip && (
                    <Chip label="Opcional" size="small" variant="outlined" />
                  )}
                </Stack>
                {!showStandaloneAdditionalModulesOptionalChip && (
                  <Typography variant="body2" color="text.secondary">
                    {standaloneAdditionalModulesDescription}
                  </Typography>
                )}
              </Box>
              <Button
                size="small"
                variant="text"
                onClick={() => setShowStandaloneAdditionalModules((current) => !current)}
                aria-controls="admin-additional-modules-list"
                aria-expanded={showStandaloneAdditionalModules}
                aria-label={standaloneAdditionalModulesActionCopy.ariaLabel}
                title={showStandaloneAdditionalModules ? undefined : standaloneAdditionalModulesActionCopy.title}
              >
                {showStandaloneAdditionalModules
                  ? standaloneAdditionalModulesHideLabel
                  : standaloneAdditionalModulesActionCopy.label}
              </Button>
            </Stack>
          </Box>
          {showStandaloneAdditionalModules && renderAdditionalModuleCardsGrid({
            cards: consoleCards,
            isFetching: consoleQuery.isFetching,
            isPending: consoleQuery.isPending,
            id: 'admin-additional-modules-list',
            sx: { px: 2, pb: 2 },
            hideSingleCardHeader: true,
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
            <InputLabel id="admin-user-roles-label" shrink>Roles asignados</InputLabel>
            <Select
              labelId="admin-user-roles-label"
              label="Roles asignados"
              multiple
              displayEmpty
              value={selectedRoles}
              onChange={(event) => setSelectedRoles(normalizeRoleList(event.target.value as RoleKey[]))}
              renderValue={() => selectedRoleControlSummary}
              SelectDisplayProps={{
                title: selectedRoleControlTitle,
              }}
            >
              {roleOptionsForEditor.map((option) => (
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
