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
import RefreshIcon from '@mui/icons-material/Refresh';
import AutoFixHighIcon from '@mui/icons-material/AutoFixHigh';
import { useMutation, useQuery, useQueryClient, type QueryClient } from '@tanstack/react-query';
import { AdminApi } from '../api/admin';
import { Health } from '../utilities/health';
import type { AdminConsoleCard, AdminUserDTO, AdminUserStatus, AuditLogEntry, RoleKey } from '../api/types';
import { ROLE_OPTIONS, formatRoleList } from '../constants/roles';

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
const GETTING_STARTED_ADMIN_STEPS = [
  'Valida el estado del servicio antes de cambiar permisos o repetir una acción.',
  'Ajusta los accesos desde Usuarios y roles para resolver el caso actual.',
  'Confirma el resultado en Auditoría reciente antes de seguir con otro cambio.',
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
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .trim();
}

function isDedicatedAdminSectionCard(card: AdminConsoleCard) {
  const normalizedId = normalizeAdminConsoleCardKey(card.cardId);
  const normalizedTitle = normalizeAdminConsoleCardKey(card.title);

  return BUILT_IN_ADMIN_CARD_IDS.has(normalizedId) || BUILT_IN_ADMIN_CARD_TITLES.has(normalizedTitle);
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
  return [...new Set(roles ?? [])].sort();
}

function hasRoleSelectionChanged(currentRoles?: readonly RoleKey[] | null, nextRoles?: readonly RoleKey[] | null) {
  const normalizedCurrentRoles = normalizeRoleSelection(currentRoles);
  const normalizedNextRoles = normalizeRoleSelection(nextRoles);

  if (normalizedCurrentRoles.length !== normalizedNextRoles.length) {
    return true;
  }

  return normalizedCurrentRoles.some((role, index) => role !== normalizedNextRoles[index]);
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
      setSelectedRoles(editingUser.roles ?? []);
      setDialogError(null);
    } else {
      setSelectedRoles([]);
      setDialogError(null);
    }
  }, [editingUser]);

  const audits = auditQuery.data ?? [];
  const previewCards = consoleQuery.data?.cards?.filter((card) => !isDedicatedAdminSectionCard(card)) ?? [];
  const consoleCards: AdminConsoleCard[] = previewCards;
  const consoleError = consoleQuery.isError ? (consoleQuery.error as Error).message : null;
  const users = usersQuery.data ?? [];
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
  const showGettingStartedGuidance =
    !consoleQuery.isPending
    && !usersQuery.isLoading
    && !auditQuery.isLoading
    && consoleCards.length === 0
    && users.length === 0
    && audits.length === 0;
  const editingTitle = useMemo(() => {
    if (!editingUser) return '';
    return editingUser.displayName?.trim() || editingUser.username;
  }, [editingUser]);
  const hasPendingRoleChanges = useMemo(() => (
    editingUser ? hasRoleSelectionChanged(editingUser.roles, selectedRoles) : false
  ), [editingUser, selectedRoles]);
  const isRefreshingPanel =
    healthQuery.isFetching
    || auditQuery.isFetching
    || consoleQuery.isFetching
    || usersQuery.isFetching;
  const demoSeedCardCopy = showGettingStartedGuidance
    ? {
        title: 'Recorrido con demo',
        description: 'Si es tu primera vez aquí, carga datos de ejemplo para ver usuarios, roles y auditoría sin tocar producción.',
        buttonLabel: 'Cargar datos de ejemplo',
        pendingLabel: 'Cargando ejemplo…',
      }
    : {
        title: 'Datos de demostración',
        description: 'Restablece los datos de demo en ambientes de prueba cuando necesites repetir el flujo sin refrescos manuales extra.',
        buttonLabel: 'Restablecer demo',
        pendingLabel: 'Restableciendo demo…',
      };

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
        <Button
          variant="outlined"
          startIcon={<RefreshIcon />}
          onClick={handleRefreshPanel}
          disabled={isRefreshingPanel}
        >
          {isRefreshingPanel ? 'Actualizando panel…' : 'Actualizar panel'}
        </Button>
      </Stack>

      {rotationWarning && (
        <Alert severity="warning" onClose={() => setRotationWarning(false)}>
          Tu sesión lleva 30 días activa. Genera un nuevo token o vuelve a autenticarte para mantener la seguridad.
        </Alert>
      )}

      {showGettingStartedGuidance && (
        <Alert severity="info" variant="outlined">
          <Typography variant="subtitle2" sx={{ mb: 1 }}>
            Primeros pasos
          </Typography>
          <Box component="ol" sx={{ pl: 2.5, m: 0 }}>
            {GETTING_STARTED_ADMIN_STEPS.map((step) => (
              <Box component="li" key={step} sx={{ '& + &': { mt: 0.5 } }}>
                <Typography variant="body2">{step}</Typography>
              </Box>
            ))}
          </Box>
        </Alert>
      )}

      {consoleError && (
        <Alert severity="warning">
          No se pudo cargar el panel dinámico. Mostrando la consola base. Detalle: {consoleError}
        </Alert>
      )}

      <Grid container spacing={2}>
        <Grid item xs={12} md={4}>
          <Card variant="outlined">
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
        </Grid>

        <Grid item xs={12} md={4}>
          <Card variant="outlined">
            <CardHeader title={demoSeedCardCopy.title} />
            <CardContent>
              <Typography variant="body2">
                {demoSeedCardCopy.description}
              </Typography>
              <Button
                sx={{ mt: 2 }}
                variant="contained"
                startIcon={<AutoFixHighIcon />}
                onClick={() => seedMutation.mutate()}
                disabled={seedMutation.isPending}
              >
                {seedMutation.isPending ? demoSeedCardCopy.pendingLabel : demoSeedCardCopy.buttonLabel}
              </Button>
              {seedMutation.isSuccess && (
                <Alert severity="success" sx={{ mt: 2 }}>
                  Datos de demostración preparados correctamente.
                </Alert>
              )}
            </CardContent>
          </Card>
        </Grid>

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

      <Paper variant="outlined">
        <Box sx={{ px: 2, py: 1 }}>
          <Box>
            <Typography variant="h6">Usuarios y roles</Typography>
            <Typography variant="body2" color="text.secondary">
              Consulta y ajusta los permisos del equipo.
            </Typography>
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
                        <Stack spacing={0.5} alignItems="flex-start">
                          <Typography variant="body2">
                            {formatRoleList(user.roles)}
                          </Typography>
                          <Button
                            size="small"
                            onClick={() => setEditingUser(user)}
                            aria-label={`Editar roles de ${identity.primary}`}
                            sx={{ px: 0, minWidth: 0 }}
                          >
                            Editar roles
                          </Button>
                        </Stack>
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
                  <Typography variant="body2">
                    Roles: {formatRoleList(singleAdminUser.roles)}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    Último acceso: {formatDateOrDash(singleAdminUser.lastSeenAt ?? singleAdminUser.lastLoginAt)}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    Estado: {singleAdminUserStatusLabel}
                  </Typography>
                  <Button
                    size="small"
                    onClick={() => setEditingUser(singleAdminUser)}
                    aria-label={`Editar roles de ${singleAdminUserIdentity?.primary ?? singleAdminUser.username}`}
                    sx={{ px: 0, minWidth: 0 }}
                  >
                    Editar roles
                  </Button>
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

      <Paper variant="outlined">
        <Box sx={{ px: 2, py: 1 }}>
          <Typography variant="h6">Auditoría reciente</Typography>
          <Typography variant="body2" color="text.secondary">
            Confirma quién cambió qué y cuándo antes de repetir una acción o ajustar permisos.
          </Typography>
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

      <Dialog open={!!editingUser} onClose={handleCloseDialog} fullWidth maxWidth="sm">
        <DialogTitle>Editar roles · {editingTitle}</DialogTitle>
        <DialogContent>
          <Typography variant="body2" color="text.secondary" paragraph>
            Selecciona los roles que determinan los módulos visibles y acciones permitidas para esta cuenta.
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
              onChange={(event) => setSelectedRoles(event.target.value as RoleKey[])}
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
