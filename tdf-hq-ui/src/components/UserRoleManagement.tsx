import { useState, useEffect } from 'react';
import {
  Box,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Chip,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  Select,
  MenuItem,
  FormControl,
  FormHelperText,
  InputLabel,
  OutlinedInput,
  CircularProgress,
  Alert,
  Typography,
  Stack,
  ButtonBase,
} from '@mui/material';
import type { SelectChangeEvent } from '@mui/material/Select';
import type { Role } from '../api/generated/client';
import { apiClient } from '../api/generated/client';
import { ALL_ROLES } from '../constants/roles';
import { normalizeRolesInput } from '../utils/roles';

type RoleValue = Role | (string & Record<never, never>);

interface NormalizedUser {
  id: number;
  name: string;
  email: string | null | undefined;
  phone: string | null | undefined;
  status: 'Active' | 'Inactive';
  roles: RoleValue[];
}

const STATUS_COLORS: Record<'Active' | 'Inactive', 'success' | 'default'> = {
  Active: 'success',
  Inactive: 'default',
};
const STATUS_LABELS: Record<'Active' | 'Inactive', string> = {
  Active: 'Activo',
  Inactive: 'Inactivo',
};

const ROLE_COLORS: Partial<Record<RoleValue, 'primary' | 'secondary' | 'success' | 'error' | 'warning' | 'info' | 'default'>> = {
  Admin: 'error',
  Manager: 'primary',
  Engineer: 'info',
  Teacher: 'success',
  Reception: 'secondary',
  Accounting: 'warning',
  Artist: 'primary',
  Student: 'default',
  ReadOnly: 'default',
  Fan: 'info',
  Artista: 'primary',
  Promotor: 'secondary',
  Promoter: 'secondary',
  'A&R': 'warning',
  Producer: 'primary',
  Songwriter: 'default',
  DJ: 'info',
  Publicist: 'success',
  TourManager: 'warning',
  LabelRep: 'warning',
  StageManager: 'warning',
  RoadCrew: 'secondary',
  Photographer: 'info',
};

const getRoleColor = (role: RoleValue) => ROLE_COLORS[role] ?? 'default';
const EDITABLE_ROLES_LABEL = 'Roles editables';

const normalizeContactValue = (value?: string | null) => {
  const trimmed = value?.trim();
  return trimmed ? trimmed : null;
};

const getContactSummary = (user: Pick<NormalizedUser, 'email' | 'phone'>) => {
  const contactValues = [normalizeContactValue(user.email), normalizeContactValue(user.phone)].filter(
    (value): value is string => value != null,
  );

  if (contactValues.length === 0) return null;
  return contactValues.join(' · ');
};

const getUserIdentityKey = (user: Pick<NormalizedUser, 'name'>) =>
  user.name.trim().toLocaleLowerCase('es');

const getUserIdsRequiringIdentityDisambiguator = (users: readonly NormalizedUser[]) => {
  const identityCounts = new Map<string, number>();

  users.forEach((user) => {
    const identityKey = getUserIdentityKey(user);
    identityCounts.set(identityKey, (identityCounts.get(identityKey) ?? 0) + 1);
  });

  return new Set(
    users
      .filter((user) => (identityCounts.get(getUserIdentityKey(user)) ?? 0) > 1)
      .map((user) => user.id),
  );
};

const buildEditRolesLabel = (user: Pick<NormalizedUser, 'id' | 'name'>, showIdentityDisambiguator: boolean) =>
  showIdentityDisambiguator
    ? `Editar roles de ${user.name} (ID ${user.id})`
    : `Editar roles de ${user.name}`;

const normalizeRoleSelection = (roles?: readonly RoleValue[] | null) =>
  Array.from(new Set((roles ?? []).map((role) => role.trim()).filter(Boolean)))
    .sort((left, right) => left.localeCompare(right));

const hasRoleSelectionChanged = (
  currentRoles?: readonly RoleValue[] | null,
  nextRoles?: readonly RoleValue[] | null,
) => {
  const normalizedCurrentRoles = normalizeRoleSelection(currentRoles);
  const normalizedNextRoles = normalizeRoleSelection(nextRoles);

  if (normalizedCurrentRoles.length !== normalizedNextRoles.length) {
    return true;
  }

  return normalizedCurrentRoles.some((role, index) => role !== normalizedNextRoles[index]);
};

const formatRoleGroupLabel = (roles: readonly RoleValue[]) => {
  if (roles.length <= 1) return roles[0] ?? '';
  if (roles.length === 2) return `${roles[0]} y ${roles[1]}`;
  return `${roles.slice(0, -1).join(', ')} y ${roles[roles.length - 1]}`;
};

const buildPendingRoleChangesSummary = (
  currentRoles?: readonly RoleValue[] | null,
  nextRoles?: readonly RoleValue[] | null,
) => {
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

  if (actions.length === 0) return null;
  return `${actions.length === 1 ? 'Cambio pendiente' : 'Cambios pendientes'}: ${actions.join(' · ')}.`;
};

const buildRoleManagementSummary = ({
  showAllInactiveSummary,
  showContactColumn,
  showStatusColumn,
  showMixedStatusSummary,
}: {
  showAllInactiveSummary: boolean;
  showContactColumn: boolean;
  showStatusColumn: boolean;
  showMixedStatusSummary: boolean;
}) => {
  const hiddenColumnSummaries: string[] = [];

  if (!showContactColumn) {
    hiddenColumnSummaries.push(
      'la columna de contacto sigue oculta hasta que exista al menos un email o teléfono',
    );
  }

  if (!showStatusColumn) {
    hiddenColumnSummaries.push(
      showAllInactiveSummary
        ? 'todas las cuentas administrables están inactivas; la columna de estado volverá cuando haya cuentas activas e inactivas para comparar'
        : 'la columna de estado sigue oculta mientras todas las cuentas sigan activas',
    );
  } else if (showMixedStatusSummary) {
    hiddenColumnSummaries.push(
      'la columna Estado solo marca las cuentas inactivas; las activas quedan implícitas',
    );
  }

  if (hiddenColumnSummaries.length === 0) return '';
  return `Vista actual: ${hiddenColumnSummaries.join(' y ')}.`;
};

export default function UserRoleManagement() {
  const [users, setUsers] = useState<NormalizedUser[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [editDialogOpen, setEditDialogOpen] = useState(false);
  const [selectedUser, setSelectedUser] = useState<NormalizedUser | null>(null);
  const [selectedRoles, setSelectedRoles] = useState<RoleValue[]>([]);
  const [saving, setSaving] = useState(false);
  const showContactColumn = users.some((user) => getContactSummary(user) != null);
  const inactiveUsersCount = users.filter((user) => user.status === 'Inactive').length;
  const activeUsersCount = users.length - inactiveUsersCount;
  const showAllInactiveSummary = users.length > 1 && inactiveUsersCount === users.length;
  const showStatusColumn = inactiveUsersCount > 0 && activeUsersCount > 0;
  const showMixedStatusSummary = showStatusColumn && inactiveUsersCount < users.length;
  const roleManagementSummary = buildRoleManagementSummary({
    showAllInactiveSummary,
    showContactColumn,
    showStatusColumn,
    showMixedStatusSummary,
  });
  const singleUser = users.length === 1 ? users[0] : null;
  const singleUserContactSummary = singleUser ? getContactSummary(singleUser) : null;
  const showComparisonTable = users.length > 1;
  const userIdsRequiringIdentityDisambiguator = getUserIdsRequiringIdentityDisambiguator(users);
  const hasPendingRoleChanges = selectedUser
    ? hasRoleSelectionChanged(selectedUser.roles, selectedRoles)
    : false;
  const pendingRoleChangesSummary = selectedUser
    ? buildPendingRoleChangesSummary(selectedUser.roles, selectedRoles)
    : null;

  useEffect(() => {
    void loadUsers();
  }, []);

  const loadUsers = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await apiClient.getUsers();
      const normalized: NormalizedUser[] = data.map((u) => ({
        id: u.id ?? 0,
        name: u.name ?? 'Sin nombre',
        email: u.email,
        phone: u.phone,
        status: u.status ?? 'Inactive',
        roles: normalizeRoleSelection((u.roles ?? []) as RoleValue[]),
      }));
      setUsers(normalized);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No se pudieron cargar los usuarios');
    } finally {
      setLoading(false);
    }
  };

  const handleEditClick = (user: NormalizedUser) => {
    setSelectedUser(user);
    setSelectedRoles(normalizeRoleSelection(user.roles));
    setEditDialogOpen(true);
  };

  const handleCloseDialog = () => {
    setEditDialogOpen(false);
    setSelectedUser(null);
    setSelectedRoles([]);
  };

  const normalizeRoles = (value: string | string[]): RoleValue[] =>
    normalizeRolesInput(value, ALL_ROLES);

  const handleRoleChange = (event: SelectChangeEvent<RoleValue[]>) => {
    setSelectedRoles(normalizeRoles(event.target.value));
  };

  const handleSaveRoles = async () => {
    if (!selectedUser || !hasPendingRoleChanges) return;

    try {
      setSaving(true);
      const normalizedRoles = normalizeRoleSelection(selectedRoles);
      await apiClient.updateUserRoles(selectedUser.id, normalizedRoles);
      setUsers((prev) => prev.map((u) => (u.id === selectedUser.id ? { ...u, roles: normalizedRoles } : u)));
      handleCloseDialog();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No se pudieron actualizar los roles');
    } finally {
      setSaving(false);
    }
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Box p={2}>
        <Alert severity="error">{error}</Alert>
      </Box>
    );
  }

  return (
    <Box p={3}>
      {users.length === 0 ? (
        <Paper variant="outlined" sx={{ p: 3 }}>
          <Stack spacing={1}>
            <Typography variant="h6" fontWeight={700}>
              Roles y permisos
            </Typography>
            <Typography variant="body2" color="text.secondary">
              Todavía no hay usuarios administrables. Cuando exista el primero, aquí aparecerán contacto, estado y
              roles para editar permisos desde una sola tabla.
            </Typography>
          </Stack>
        </Paper>
      ) : (
        <Stack spacing={1.5}>
          <Stack spacing={0.5}>
            <Typography variant="h5" fontWeight={700}>
              Roles y permisos
            </Typography>
            {showComparisonTable && roleManagementSummary && (
              <Typography variant="body2" color="text.secondary">
                {roleManagementSummary}
              </Typography>
            )}
          </Stack>
          {singleUser ? (
            <Paper variant="outlined" sx={{ p: 2 }}>
              <Stack spacing={1.25}>
                <Typography variant="body2" color="text.secondary">
                  Primer usuario administrable. Revisa sus datos clave y edita roles aquí; cuando exista el segundo,
                  volverá la tabla comparativa.
                </Typography>
                <Stack
                  spacing={1}
                  direction={{ xs: 'column', md: 'row' }}
                  justifyContent="space-between"
                  alignItems={{ xs: 'flex-start', md: 'center' }}
                >
                  <Stack spacing={0.25}>
                    <Typography variant="body2" fontWeight={600}>
                      {singleUser.name}
                    </Typography>
                    {userIdsRequiringIdentityDisambiguator.has(singleUser.id) && (
                      <Typography variant="caption" color="text.secondary">
                        ID {singleUser.id}
                      </Typography>
                    )}
                    {singleUserContactSummary && (
                      <Typography variant="body2" color="text.secondary">
                        {singleUserContactSummary}
                      </Typography>
                    )}
                  </Stack>
                  <Stack spacing={0.75} alignItems={{ xs: 'flex-start', md: 'flex-end' }}>
                    {singleUser.status === 'Inactive' && (
                      <Chip label={STATUS_LABELS[singleUser.status]} color={STATUS_COLORS[singleUser.status]} size="small" />
                    )}
                    <Stack spacing={0.25} alignItems={{ xs: 'flex-start', md: 'flex-end' }}>
                      <Typography variant="caption" color="text.secondary">
                        {EDITABLE_ROLES_LABEL}
                      </Typography>
                      <ButtonBase
                        onClick={() => handleEditClick(singleUser)}
                        aria-label={buildEditRolesLabel(
                          singleUser,
                          userIdsRequiringIdentityDisambiguator.has(singleUser.id),
                        )}
                        sx={{
                          borderRadius: 1,
                          display: 'inline-flex',
                          justifyContent: 'flex-start',
                          maxWidth: '100%',
                          textAlign: 'left',
                        }}
                      >
                        <Box display="flex" gap={0.5} flexWrap="wrap">
                          {singleUser.roles.map((role) => (
                            <Chip key={role} label={role} color={getRoleColor(role)} size="small" />
                          ))}
                          {singleUser.roles.length === 0 && <Chip label="No roles" size="small" variant="outlined" />}
                        </Box>
                      </ButtonBase>
                    </Stack>
                  </Stack>
                </Stack>
              </Stack>
            </Paper>
          ) : (
            <TableContainer component={Paper}>
              <Table>
                <TableHead>
                  <TableRow>
                    <TableCell>Usuario</TableCell>
                    {showContactColumn && <TableCell>Contacto</TableCell>}
                    {showStatusColumn && <TableCell>Estado</TableCell>}
                    <TableCell>{EDITABLE_ROLES_LABEL}</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {users.map((user) => {
                    const contactSummary = getContactSummary(user);
                    const showIdentityDisambiguator = userIdsRequiringIdentityDisambiguator.has(user.id);

                    return (
                      <TableRow key={user.id}>
                        <TableCell>
                          <Stack spacing={0.25}>
                            <Typography variant="body2" fontWeight={600}>
                              {user.name}
                            </Typography>
                            {showIdentityDisambiguator && (
                              <Typography variant="caption" color="text.secondary">
                                ID {user.id}
                              </Typography>
                            )}
                          </Stack>
                        </TableCell>
                        {showContactColumn && (
                          <TableCell>
                            {contactSummary ? (
                              <Typography variant="body2">
                                {contactSummary}
                              </Typography>
                            ) : (
                              <Typography variant="body2" color="text.secondary">
                                Sin email ni teléfono
                              </Typography>
                            )}
                          </TableCell>
                        )}
                        {showStatusColumn && (
                          <TableCell>
                            {user.status === 'Inactive' ? (
                              <Chip label={STATUS_LABELS[user.status]} color={STATUS_COLORS[user.status]} size="small" />
                            ) : null}
                          </TableCell>
                        )}
                        <TableCell>
                          <ButtonBase
                            onClick={() => handleEditClick(user)}
                            aria-label={buildEditRolesLabel(user, showIdentityDisambiguator)}
                            sx={{
                              borderRadius: 1,
                              display: 'inline-flex',
                              justifyContent: 'flex-start',
                              maxWidth: '100%',
                              textAlign: 'left',
                            }}
                          >
                            <Box display="flex" gap={0.5} flexWrap="wrap">
                              {user.roles.map((role) => (
                                <Chip key={role} label={role} color={getRoleColor(role)} size="small" />
                              ))}
                              {user.roles.length === 0 && <Chip label="No roles" size="small" variant="outlined" />}
                            </Box>
                          </ButtonBase>
                        </TableCell>
                      </TableRow>
                    );
                  })}
                </TableBody>
              </Table>
            </TableContainer>
          )}
        </Stack>
      )}

      <Dialog open={editDialogOpen} onClose={handleCloseDialog} maxWidth="sm" fullWidth>
        <DialogTitle>Editar roles de {selectedUser?.name}</DialogTitle>
        <DialogContent>
          <FormControl fullWidth sx={{ mt: 2 }}>
            <InputLabel id="roles-label">Roles</InputLabel>
            <Select<RoleValue[]>
              labelId="roles-label"
              multiple
              value={selectedRoles}
              onChange={handleRoleChange}
              input={<OutlinedInput label="Roles" />}
              renderValue={(selected) => (
                <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                  {selected.map((role) => (
                    <Chip key={role} label={role} size="small" color={getRoleColor(role)} />
                  ))}
                </Box>
              )}
            >
              {ALL_ROLES.map((role) => (
                <MenuItem key={role} value={role}>
                  {role}
                </MenuItem>
              ))}
            </Select>
            <FormHelperText>
              {hasPendingRoleChanges
                ? (pendingRoleChangesSummary ?? 'Listo para guardar esta actualización de permisos.')
                : 'Sin cambios pendientes. Modifica la selección para habilitar Guardar cambios.'}
            </FormHelperText>
          </FormControl>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog} disabled={saving}>
            Cancelar
          </Button>
          <Button onClick={() => void handleSaveRoles()} variant="contained" disabled={saving || !hasPendingRoleChanges}>
            {saving ? 'Guardando...' : 'Guardar cambios'}
          </Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
}
