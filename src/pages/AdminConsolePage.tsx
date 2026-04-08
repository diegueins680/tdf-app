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
import EditIcon from '@mui/icons-material/Edit';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { AdminApi } from '../api/admin';
import { Health } from '../utilities/health';
import type { AdminConsoleCard, AdminUserDTO, AdminUserStatus, AuditLogEntry, RoleKey } from '../api/types';
import { ROLE_OPTIONS, formatRoleList } from '../constants/roles';

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
      qc.invalidateQueries({ queryKey: ['admin', 'audit'] });
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
  const fallbackCard: AdminConsoleCard = {
    cardId: 'user-management',
    title: 'Gestión de usuarios',
    body: [
      'Administra el acceso de los usuarios asignando roles según sus responsabilidades.',
      'Próximamente aquí se podrá crear usuarios de servicio y tokens API.',
    ],
  };
  const consoleCards: AdminConsoleCard[] = consoleQuery.data?.cards?.length
    ? consoleQuery.data.cards
    : [fallbackCard];
  const consoleError = consoleQuery.isError ? (consoleQuery.error as Error).message : null;
  const users = usersQuery.data ?? [];
  const isUsersLoading = usersQuery.isLoading;
  const usersError = usersQuery.isError ? (usersQuery.error as Error).message : null;
  const editingTitle = useMemo(() => {
    if (!editingUser) return '';
    return editingUser.displayName?.trim() || editingUser.username;
  }, [editingUser]);

  const handleCloseDialog = () => {
    if (updateRolesMutation.isPending) return;
    setEditingUser(null);
  };

  const handleSaveRoles = () => {
    if (!editingUser) return;
    setDialogError(null);
    updateRolesMutation.mutate({ userId: editingUser.userId, roles: selectedRoles });
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
      <Typography variant="h5">Consola de administración</Typography>

      {rotationWarning && (
        <Alert severity="warning" onClose={() => setRotationWarning(false)}>
          Tu sesión lleva 30 días activa. Genera un nuevo token o vuelve a autenticarte para mantener la seguridad.
        </Alert>
      )}

      <Grid container spacing={2}>
        <Grid item xs={12} md={4}>
          <Card variant="outlined">
            <CardHeader
              title="Estado del servicio"
              action={(
                <Button size="small" variant="text" startIcon={<RefreshIcon />} onClick={() => qc.invalidateQueries({ queryKey: ['admin', 'health'] })}>
                  Refrescar
                </Button>
              )}
            />
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
            <CardHeader title="Datos de demostración" />
            <CardContent>
              <Typography variant="body2">
                Ejecuta la siembra para restablecer datos de demo en ambientes de prueba.
              </Typography>
              <Button
                sx={{ mt: 2 }}
                variant="contained"
                startIcon={<AutoFixHighIcon />}
                onClick={() => seedMutation.mutate()}
                disabled={seedMutation.isPending}
              >
                {seedMutation.isPending ? 'Sembrando…' : 'Seed demo data'}
              </Button>
              {seedMutation.isSuccess && (
                <Alert severity="success" sx={{ mt: 2 }}>
                  Datos de demo regenerados correctamente.
                </Alert>
              )}
            </CardContent>
          </Card>
        </Grid>

        {consoleCards.map((card, index) => (
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
                {index === 0 && consoleError && (
                  <Alert severity="warning" sx={{ mt: 2 }}>
                    No se pudo cargar el panel dinámico. Mostrando información predeterminada. Detalle: {consoleError}
                  </Alert>
                )}
              </CardContent>
            </Card>
          </Grid>
        ))}
      </Grid>

      <Paper variant="outlined">
        <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ px: 2, py: 1 }}>
          <Box>
            <Typography variant="h6">Usuarios y roles</Typography>
            <Typography variant="body2" color="text.secondary">
              Consulta y ajusta los permisos del equipo.
            </Typography>
          </Box>
          <Button
            startIcon={<RefreshIcon />}
            onClick={() => qc.invalidateQueries({ queryKey: ['admin', 'users'] })}
            disabled={isUsersLoading}
          >
            Actualizar
          </Button>
        </Stack>
        {usersError && (
          <Alert severity="error" sx={{ mx: 2 }}>
            {usersError}
          </Alert>
        )}
        <TableContainer>
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>Usuario</TableCell>
                <TableCell>Roles</TableCell>
                <TableCell>Último acceso</TableCell>
                <TableCell>Estado</TableCell>
                <TableCell align="right">Acciones</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {isUsersLoading && (
                <TableRow>
                  <TableCell colSpan={5}>
                    <Stack direction="row" alignItems="center" justifyContent="center" spacing={1} sx={{ py: 2 }}>
                      <CircularProgress size={18} />
                      <Typography variant="body2" color="text.secondary">
                        Cargando usuarios…
                      </Typography>
                    </Stack>
                  </TableCell>
                </TableRow>
              )}
              {!isUsersLoading && users.length === 0 && !usersError && (
                <TableRow>
                  <TableCell colSpan={5}>
                    <Typography variant="body2" align="center" color="text.secondary" sx={{ py: 2 }}>
                      Todavía no hay usuarios administrables.
                    </Typography>
                  </TableCell>
                </TableRow>
              )}
              {users.map((user) => (
                <TableRow key={user.userId} hover>
                  <TableCell>
                    <Stack spacing={0.25}>
                      <Typography variant="body2" fontWeight={600}>
                        {user.displayName?.trim() || user.username}
                      </Typography>
                      <Typography variant="caption" color="text.secondary">
                        {user.username}
                      </Typography>
                      {user.partyId ? (
                        <Typography variant="caption" color="text.secondary">
                          Party #{user.partyId}
                        </Typography>
                      ) : null}
                    </Stack>
                  </TableCell>
                  <TableCell>{formatRoleList(user.roles)}</TableCell>
                  <TableCell>{formatDateOrDash(user.lastSeenAt ?? user.lastLoginAt)}</TableCell>
                  <TableCell>{renderStatus(user.status)}</TableCell>
                  <TableCell align="right">
                    <Button size="small" startIcon={<EditIcon />} onClick={() => setEditingUser(user)}>
                      Editar roles
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      </Paper>

      <Paper variant="outlined">
        <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ px: 2, py: 1 }}>
          <Typography variant="h6">Auditoría reciente</Typography>
          <Button startIcon={<RefreshIcon />} onClick={() => qc.invalidateQueries({ queryKey: ['admin', 'audit'] })}>
            Actualizar
          </Button>
        </Stack>
        {auditQuery.isError && (
          <Alert severity="error" sx={{ mx: 2 }}>
            {(auditQuery.error as Error).message}
          </Alert>
        )}
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
                  <TableCell>{entry.actorId ?? 'Sistema'}</TableCell>
                  <TableCell>
                    <Typography variant="body2" color="text.secondary" sx={{ maxWidth: 320, whiteSpace: 'pre-wrap' }}>
                      {entry.diff ?? '—'}
                    </Typography>
                  </TableCell>
                </TableRow>
              ))}
              {audits.length === 0 && (
                <TableRow>
                  <TableCell colSpan={5}>
                    <Typography variant="body2" align="center" color="text.secondary" sx={{ py: 2 }}>
                      Sin eventos de auditoría todavía.
                    </Typography>
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </TableContainer>
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
              renderValue={(value) => (value as RoleKey[]).join(', ')}
            >
              {ROLE_OPTIONS.map((option) => (
                <MenuItem key={option.value} value={option.value}>
                  <Checkbox checked={selectedRoles.includes(option.value)} />
                  <ListItemText primary={option.label} />
                </MenuItem>
              ))}
            </Select>
            <FormHelperText>Puedes asignar múltiples roles para combinar permisos.</FormHelperText>
          </FormControl>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog} disabled={updateRolesMutation.isPending}>
            Cancelar
          </Button>
          <Button onClick={handleSaveRoles} variant="contained" disabled={updateRolesMutation.isPending}>
            {updateRolesMutation.isPending ? 'Guardando…' : 'Guardar cambios'}
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}
