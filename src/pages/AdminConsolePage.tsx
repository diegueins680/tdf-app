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
const BUILT_IN_ADMIN_CARD_IDS = new Set([
  'user-management',
  'users',
  'audit',
  'audit-log',
  'service-health',
  'health',
  'demo-seed',
  'seed',
]);
const BUILT_IN_ADMIN_CARD_TITLES = new Set([
  'estado del servicio',
  'datos de demostracion',
  'usuarios y roles',
  'gestion de usuarios',
  'auditoria reciente',
]);
const GETTING_STARTED_ADMIN_SECTIONS = [
  { label: '1. Estado del servicio', targetId: 'admin-service-health' },
  { label: '2. Usuarios y roles', targetId: 'admin-users-and-roles' },
  { label: '3. Auditoría reciente', targetId: 'admin-recent-audit' },
] as const;
const FIRST_RUN_USERS_EMPTY_STATE = 'Aún no hay usuarios administrables.';
const FIRST_RUN_AUDIT_EMPTY_STATE = 'La auditoría aparecerá cuando se registre el primer cambio.';
const ADMIN_USER_TABLE_COLUMN_COUNT = 4;
const AUDIT_TABLE_COLUMN_COUNT = 5;

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

function sanitizeAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  return cards.flatMap((card) => {
    const title = card.title.trim();
    const body = card.body
      .map((paragraph) => paragraph.trim())
      .filter((paragraph) => paragraph.length > 0);

    if (title === '' || body.length === 0) {
      return [];
    }

    return [{ ...card, title, body }];
  });
}

function isDedicatedAdminSectionCard(card: AdminConsoleCard) {
  const normalizedId = normalizeAdminConsoleCardKey(card.cardId);
  const normalizedTitle = normalizeAdminConsoleCardKey(card.title);

  return BUILT_IN_ADMIN_CARD_IDS.has(normalizedId) || BUILT_IN_ADMIN_CARD_TITLES.has(normalizedTitle);
}

function dedupeAdminConsoleCards(cards: readonly AdminConsoleCard[]) {
  const seenCards = new Set<string>();

  return cards.filter((card) => {
    const fingerprint = [
      normalizeAdminConsoleCardKey(card.title),
      card.body.map((paragraph) => normalizeAdminConsoleCardKey(paragraph)).join('|'),
    ].join('::');

    if (seenCards.has(fingerprint)) {
      return false;
    }

    seenCards.add(fingerprint);
    return true;
  });
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

function formatAuditActor(actorId?: number | null) {
  return actorId ?? 'Sistema';
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

const STATUS_META: Record<AdminUserStatus, { label: string; color: 'default' | 'success' | 'warning' | 'error' | 'info' }> = {
  ACTIVE: { label: 'Activo', color: 'success' },
  INVITED: { label: 'Invitado', color: 'info' },
  DISABLED: { label: 'Suspendido', color: 'default' },
};

export default function AdminConsolePage() {
  const qc = useQueryClient();
  const [rotationWarning, setRotationWarning] = useState(false);
  const [editingUser, setEditingUser] = useState<AdminUserDTO | null>(null);
  const [selectedRoles, setSelectedRoles] = useState<RoleKey[]>([]);
  const [dialogError, setDialogError] = useState<string | null>(null);

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

  const audits = auditQuery.data ?? [];
  const previewCards = dedupeAdminConsoleCards(
    sanitizeAdminConsoleCards(
      consoleQuery.data?.cards?.filter((card) => !isDedicatedAdminSectionCard(card)) ?? [],
    ),
  );
  const consoleCards: AdminConsoleCard[] = previewCards;
  const consoleError = consoleQuery.isError ? (consoleQuery.error as Error).message : null;
  const users = dedupeAdminUsers(usersQuery.data ?? []);
  const isUsersLoading = usersQuery.isLoading;
  const usersError = usersQuery.isError ? (usersQuery.error as Error).message : null;
  const singleAdminUser = !isUsersLoading && users.length === 1 ? (users[0] ?? null) : null;
  const singleAdminUserIdentity = singleAdminUser ? summarizeAdminUserIdentity(singleAdminUser) : null;
  const singleAdminUserStatusLabel = singleAdminUser?.status
    ? (STATUS_META[singleAdminUser.status]?.label ?? singleAdminUser.status)
    : '—';
  const showUsersTable = isUsersLoading || users.length > 1;
  const singleAuditEntry = !auditQuery.isLoading && audits.length === 1 ? (audits[0] ?? null) : null;
  const showAuditTable = auditQuery.isLoading || audits.length > 1;
  const hasUsersSectionData = showUsersTable || singleAdminUser !== null;
  const hasAuditSectionData = showAuditTable || singleAuditEntry !== null;
  const showGettingStartedGuidance =
    !consoleQuery.isPending
    && !usersQuery.isLoading
    && !auditQuery.isLoading
    && users.length === 0
    && audits.length === 0;
  const usersSectionDescription = showGettingStartedGuidance
    ? null
    : (
      hasUsersSectionData
        ? 'Haz clic sobre un rol para editarlo desde esta misma vista.'
        : null
    );
  const auditSectionDescription = showGettingStartedGuidance
    ? null
    : (
      hasAuditSectionData
        ? 'Confirma quién cambió qué y cuándo antes de repetir una acción o ajustar permisos.'
        : null
    );
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
  const equivalentRoleWarning = useMemo(() => {
    const groups = getNavigationEquivalentRoleGroups(selectedRoles);
    if (groups.length === 0) {
      return null;
    }

    return groups
      .map((group) => formatRoleGroupLabel(group))
      .join(' · ');
  }, [selectedRoles]);
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

  const renderStatus = (status?: AdminUserStatus | null) => {
    if (!status) return '—';
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
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
          <Button
            variant="outlined"
            startIcon={<RefreshIcon />}
            onClick={handleRefreshPanel}
            disabled={isRefreshingPanel}
          >
            {isRefreshingPanel ? 'Actualizando panel…' : 'Actualizar panel'}
          </Button>
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
              {consoleCards.length > 0 && (
                <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                  Los módulos adicionales aparecen aparte; primero ubica salud, usuarios y auditoría.
                </Typography>
              )}
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
            <Typography variant="body2">API: {healthQuery.data?.status ?? '—'}</Typography>
            <Typography variant="body2">Base de datos: {healthQuery.data?.db ?? '—'}</Typography>
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
              <Typography variant="body2" color="text.secondary">
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
                  <TableCell>Roles</TableCell>
                  <TableCell>Último acceso</TableCell>
                  <TableCell>Estado</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {isUsersLoading && (
                  <TableRow>
                    <TableCell colSpan={ADMIN_USER_TABLE_COLUMN_COUNT}>
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
                          {user.partyId ? (
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
                      <TableCell>{formatDateOrDash(user.lastSeenAt ?? user.lastLoginAt)}</TableCell>
                      <TableCell>{renderStatus(user.status)}</TableCell>
                    </TableRow>
                  );
                })}
              </TableBody>
            </Table>
          </TableContainer>
        ) : singleAdminUser && !usersError ? (
          <Box sx={{ px: 2, pb: 2 }}>
            <Stack spacing={1.25}>
              <Typography variant="body2" color="text.secondary">
                Primer usuario administrable. Revísalo aquí; cuando exista el segundo, volverá la tabla comparativa.
              </Typography>
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
                  {singleAdminUser.partyId ? (
                    <Typography variant="caption" color="text.secondary">
                      Party #{singleAdminUser.partyId}
                    </Typography>
                  ) : null}
                </Stack>
                <Stack spacing={0.5} alignItems={{ xs: 'flex-start', md: 'flex-end' }}>
                  <Button
                    size="small"
                    onClick={() => setEditingUser(singleAdminUser)}
                    aria-label={`Editar roles de ${singleAdminUserIdentity?.primary ?? singleAdminUser.username}`}
                    sx={{
                      px: 0,
                      minWidth: 0,
                      textTransform: 'none',
                    }}
                  >
                    Roles: {formatRoleList(singleAdminUser.roles)}
                  </Button>
                  <Typography variant="body2" color="text.secondary">
                    Último acceso: {formatDateOrDash(singleAdminUser.lastSeenAt ?? singleAdminUser.lastLoginAt)}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    Estado: {singleAdminUserStatusLabel}
                  </Typography>
                </Stack>
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
                  <TableCell>Actor</TableCell>
                  <TableCell>Detalle</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {audits.map((entry: AuditLogEntry, index: number) => (
                  <TableRow key={`${entry.entity}-${entry.entityId}-${index}`}>
                    <TableCell>{formatDate(entry.createdAt)}</TableCell>
                    <TableCell>{entry.entity} · {entry.entityId}</TableCell>
                    <TableCell>{entry.action}</TableCell>
                    <TableCell>{formatAuditActor(entry.actorId)}</TableCell>
                    <TableCell>
                      <Typography variant="body2" color="text.secondary" sx={{ maxWidth: 320, whiteSpace: 'pre-wrap' }}>
                        {entry.diff ?? '—'}
                      </Typography>
                    </TableCell>
                  </TableRow>
                ))}
                {auditQuery.isLoading && (
                  <TableRow>
                    <TableCell colSpan={AUDIT_TABLE_COLUMN_COUNT}>
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
                <Typography variant="body2" color="text.secondary">
                  <Box component="span" sx={{ fontWeight: 600 }}>Actor:</Box> {formatAuditActor(singleAuditEntry.actorId)}
                </Typography>
                <Typography variant="body2" color="text.secondary" sx={{ whiteSpace: 'pre-wrap' }}>
                  <Box component="span" sx={{ fontWeight: 600 }}>Detalle:</Box> {singleAuditEntry.diff ?? '—'}
                </Typography>
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

      {consoleCards.length > 0 && (
        <Paper variant="outlined" id="admin-additional-modules">
          <Box sx={{ px: 2, py: 1 }}>
            <Typography variant="h6">Módulos adicionales</Typography>
            <Typography variant="body2" color="text.secondary">
              {showGettingStartedGuidance
                ? 'Se muestran aparte para que el recorrido inicial siga centrado en salud, usuarios y auditoría.'
                : 'Tarjetas auxiliares del panel. Revísalas cuando ya confirmaste salud, usuarios y auditoría.'}
            </Typography>
          </Box>
          <Grid container spacing={2} sx={{ px: 2, pb: 2 }}>
            {consoleCards.map((card) => (
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
                    {consoleQuery.isFetching && !consoleQuery.isError && (
                      <Typography variant="caption" color="text.secondary">
                        {consoleQuery.isPending ? 'Cargando…' : 'Actualizando…'}
                      </Typography>
                    )}
                  </CardContent>
                </Card>
              </Grid>
            ))}
          </Grid>
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
                ? 'Puedes asignar múltiples roles para combinar permisos.'
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
