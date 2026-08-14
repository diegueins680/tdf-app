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
  ListSubheader,
  FormControl,
  FormHelperText,
  InputLabel,
  OutlinedInput,
  CircularProgress,
  Alert,
  Typography,
  Stack,
  ButtonBase,
  TextField,
} from '@mui/material';
import type { SelectChangeEvent } from '@mui/material/Select';
import type {
  SecurityPartyRoleAssignment,
  SecurityRole,
} from '../api/generated/client';
import { apiClient } from '../api/generated/client';
import LazyPaginatedList from './LazyPaginatedList';

type RoleValue = string;

interface NormalizedUser {
  id: number;
  partyId: number;
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

const getRoleColor = (role?: SecurityRole): 'error' | 'primary' | 'default' => {
  if (role?.emergencyAdministrator) return 'error';
  if (role?.systemRole) return 'primary';
  return 'default';
};
const EDITABLE_ROLES_LABEL = 'Roles editables';
const EMPTY_ROLES_LABEL = 'Sin roles';
const EMPTY_CONTACT_LABEL = 'Sin email ni teléfono';
const INLINE_ROLE_CHIP_LIMIT = 3;

const normalizeContactValue = (value?: string | null) => {
  const trimmed = value?.trim();
  if (!trimmed) return null;
  return trimmed;
};

const getContactSummary = (user: Pick<NormalizedUser, 'email' | 'phone'>) => {
  const contactValues = [normalizeContactValue(user.email), normalizeContactValue(user.phone)].filter(
    (value): value is string => value != null,
  );

  if (contactValues.length === 0) return null;
  return contactValues.join(' · ');
};

const preferContactValue = (primary?: string | null, fallback?: string | null) =>
  normalizeContactValue(primary) ?? normalizeContactValue(fallback);

const preferUserName = (primary: string, fallback: string) => {
  const normalizedPrimary = normalizeContactValue(primary);
  if (normalizedPrimary && normalizedPrimary !== 'Sin nombre') return normalizedPrimary;
  return normalizeContactValue(fallback) ?? primary;
};

const mergeNormalizedUserRecords = (primary: NormalizedUser, fallback: NormalizedUser): NormalizedUser => ({
  ...primary,
  name: preferUserName(primary.name, fallback.name),
  email: preferContactValue(primary.email, fallback.email),
  phone: preferContactValue(primary.phone, fallback.phone),
  status: primary.status === 'Active' || fallback.status === 'Active' ? 'Active' : 'Inactive',
  roles: normalizeRoleSelection([...primary.roles, ...fallback.roles]),
});

const dedupeNormalizedUsers = (users: readonly NormalizedUser[]) => {
  const dedupedUsers: NormalizedUser[] = [];
  const indexByUserId = new Map<number, number>();

  users.forEach((user) => {
    if (!Number.isInteger(user.id) || user.id <= 0) {
      dedupedUsers.push(user);
      return;
    }

    const existingIndex = indexByUserId.get(user.id);
    if (existingIndex == null) {
      indexByUserId.set(user.id, dedupedUsers.length);
      dedupedUsers.push(user);
      return;
    }

    dedupedUsers[existingIndex] = mergeNormalizedUserRecords(dedupedUsers[existingIndex]!, user);
  });

  return dedupedUsers;
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

const getRoleEditActionLabel = (roles?: readonly RoleValue[] | null) =>
  roles?.some((role) => role.trim()) ? 'Editar roles' : 'Asignar roles';

const buildEditRolesLabel = (
  user: Pick<NormalizedUser, 'id' | 'name'>,
  showIdentityDisambiguator: boolean,
  roles?: readonly RoleValue[] | null,
) => {
  const actionLabel = getRoleEditActionLabel(roles);

  return showIdentityDisambiguator
    ? `${actionLabel} de ${user.name} (ID ${user.id})`
    : `${actionLabel} de ${user.name}`;
};

const normalizeRoleSelection = (roles?: readonly RoleValue[] | null) => {
  const rolesByKey = new Map<string, RoleValue>();

  (roles ?? []).forEach((role) => {
    const trimmedRole = role.trim();
    if (!trimmedRole) return;

    const roleKey = trimmedRole.toLocaleLowerCase('es');
    if (rolesByKey.has(roleKey)) return;

    rolesByKey.set(roleKey, trimmedRole);
  });

  return [...rolesByKey.values()]
    .sort((left, right) => left.localeCompare(right));
};

const isRenderableNormalizedUser = (user: NormalizedUser) => {
  const normalizedName = normalizeContactValue(user.name);

  return (
    (Number.isInteger(user.id) && user.id > 0)
    || (normalizedName != null && normalizedName !== 'Sin nombre')
    || getContactSummary(user) != null
    || normalizeRoleSelection(user.roles).length > 0
  );
};

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

const sortRolesForEditor = (
  availableRoles: readonly RoleValue[],
  currentRoles?: readonly RoleValue[] | null,
) => {
  const currentRoleKeys = new Set(
    normalizeRoleSelection(currentRoles).map((role) => role.toLocaleLowerCase('es')),
  );

  return normalizeRoleSelection(availableRoles).sort((left, right) => {
    const leftPinned = currentRoleKeys.has(left.toLocaleLowerCase('es'));
    const rightPinned = currentRoleKeys.has(right.toLocaleLowerCase('es'));

    if (leftPinned !== rightPinned) return leftPinned ? -1 : 1;
    return left.localeCompare(right);
  });
};

const formatSpanishList = (items: readonly string[]) => {
  if (items.length <= 1) return items[0] ?? '';
  if (items.length === 2) return `${items[0]} y ${items[1]}`;
  return `${items.slice(0, -1).join(', ')} y ${items[items.length - 1]}`;
};

const formatRoleGroupLabel = (roles: readonly RoleValue[]) => formatSpanishList(roles);

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

const buildRoleButtonTitle = ({
  roles,
  roleByCode,
  user,
  showIdentityDisambiguator,
}: {
  roles: readonly RoleValue[];
  roleByCode: ReadonlyMap<string, SecurityRole>;
  user: Pick<NormalizedUser, 'id' | 'name'>;
  showIdentityDisambiguator: boolean;
}) => {
  const normalizedRoles = normalizeRoleSelection(roles);
  const rolesSummary = normalizedRoles.length === 0
    ? EMPTY_ROLES_LABEL
    : normalizedRoles.map((role) => roleByCode.get(role.toLocaleLowerCase('es'))?.nameEs ?? role).join(', ');
  return `${buildEditRolesLabel(user, showIdentityDisambiguator, roles)}. Roles actuales: ${rolesSummary}.`;
};

const renderInlineRoleChips = (
  roles: readonly RoleValue[],
  roleByCode: ReadonlyMap<string, SecurityRole>,
) => {
  const normalizedRoles = normalizeRoleSelection(roles);

  if (normalizedRoles.length === 0) {
    return <Chip label={EMPTY_ROLES_LABEL} size="small" variant="outlined" />;
  }

  const visibleRoles = normalizedRoles.slice(0, INLINE_ROLE_CHIP_LIMIT);
  const hiddenRoles = normalizedRoles.slice(INLINE_ROLE_CHIP_LIMIT);

  return (
    <>
      {visibleRoles.map((role) => (
        <Chip
          key={role}
          label={roleByCode.get(role.toLocaleLowerCase('es'))?.nameEs ?? role}
          color={getRoleColor(roleByCode.get(role.toLocaleLowerCase('es')))}
          size="small"
        />
      ))}
      {hiddenRoles.length > 0 && (
        <Chip
          label={`+${hiddenRoles.length} ${hiddenRoles.length === 1 ? 'rol' : 'roles'}`}
          size="small"
          variant="outlined"
        />
      )}
    </>
  );
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
  const summaryParts: string[] = [];
  const hiddenColumnLabels: string[] = [];

  if (!showContactColumn) {
    hiddenColumnLabels.push('contacto');
  }

  if (!showStatusColumn && !showAllInactiveSummary) {
    hiddenColumnLabels.push('estado');
  }

  if (hiddenColumnLabels.length > 0) {
    const hiddenColumnsLabel = formatSpanishList(hiddenColumnLabels);
    const verb = hiddenColumnLabels.length === 1 ? 'aparecerá' : 'aparecerán';
    const contextVerb = hiddenColumnLabels.length === 1 ? 'aporte' : 'aporten';
    summaryParts.push(`${hiddenColumnsLabel} ${verb} cuando ${contextVerb} contexto`);
  }

  if (showAllInactiveSummary) {
    summaryParts.push('todas las cuentas están inactivas; Estado volverá cuando exista una cuenta activa para comparar');
  } else if (showMixedStatusSummary) {
    summaryParts.push('Estado solo marca cuentas inactivas; las activas quedan implícitas');
  }

  if (summaryParts.length === 0) return '';
  return `Vista compacta: ${summaryParts.join('. ')}.`;
};

const renderRoleEditButtonContents = (
  roles: readonly RoleValue[],
  roleByCode: ReadonlyMap<string, SecurityRole>,
) => (
  <Box display="inline-flex" alignItems="center" gap={0.75} flexWrap="wrap">
    <Box display="flex" gap={0.5} flexWrap="wrap">
      {renderInlineRoleChips(roles, roleByCode)}
    </Box>
  </Box>
);

export default function UserRoleManagement() {
  const [users, setUsers] = useState<NormalizedUser[]>([]);
  const [securityRoles, setSecurityRoles] = useState<SecurityRole[]>([]);
  const [roleAssignments, setRoleAssignments] = useState<SecurityPartyRoleAssignment[]>([]);
  const [loading, setLoading] = useState(true);
  const [loadingAssignments, setLoadingAssignments] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [submissionSuccess, setSubmissionSuccess] = useState<string | null>(null);
  const [editDialogOpen, setEditDialogOpen] = useState(false);
  const [selectedUser, setSelectedUser] = useState<NormalizedUser | null>(null);
  const [selectedRoles, setSelectedRoles] = useState<RoleValue[]>([]);
  const [saving, setSaving] = useState(false);
  const [dialogError, setDialogError] = useState<string | null>(null);
  const [changeReason, setChangeReason] = useState('');
  const roleByCode = new Map(
    securityRoles.map((role) => [role.code.toLocaleLowerCase('es'), role]),
  );
  const availableRoleCodes = securityRoles.map((role) => role.code);
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
  const roleOptionsForEditor = sortRolesForEditor(availableRoleCodes, selectedUser?.roles);
  const currentRoleKeysForEditor = new Set(
    normalizeRoleSelection(selectedUser?.roles).map((role) => role.toLocaleLowerCase('es')),
  );
  const selectedUserNeedsIdentityDisambiguator = selectedUser
    ? userIdsRequiringIdentityDisambiguator.has(selectedUser.id)
    : false;
  const editDialogTitle = selectedUser
    ? buildEditRolesLabel(selectedUser, selectedUserNeedsIdentityDisambiguator, selectedUser.roles)
    : 'Editar roles';

  useEffect(() => {
    void loadUsers();
  }, []);

  const loadUsers = async () => {
    try {
      setLoading(true);
      setError(null);
      const [data, persistedRoles] = await Promise.all([
        apiClient.getUsers(),
        apiClient.getSecurityRoles(),
      ]);
      const normalized: NormalizedUser[] = data.map((u) => ({
        id: u.id ?? 0,
        partyId: u.partyId ?? 0,
        name: u.name ?? 'Sin nombre',
        email: u.email,
        phone: u.phone,
        status: u.status ?? 'Inactive',
        roles: normalizeRoleSelection((u.roles ?? []) as RoleValue[]),
      })).filter(isRenderableNormalizedUser);
      setUsers(dedupeNormalizedUsers(normalized));
      setSecurityRoles(persistedRoles.filter((role) => role.active));
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No se pudieron cargar los usuarios');
    } finally {
      setLoading(false);
    }
  };

  const handleEditClick = async (user: NormalizedUser) => {
    setSelectedUser(user);
    setSelectedRoles(normalizeRoleSelection(user.roles));
    setRoleAssignments([]);
    setChangeReason('');
    setDialogError(null);
    setEditDialogOpen(true);
    if (!Number.isSafeInteger(user.partyId) || user.partyId <= 0) {
      setDialogError('El usuario no tiene un identificador canónico de contacto válido.');
      return;
    }
    try {
      setLoadingAssignments(true);
      setRoleAssignments(await apiClient.getPartyRoleAssignments(user.partyId));
    } catch (err) {
      setDialogError(err instanceof Error ? err.message : 'No se pudieron cargar las asignaciones versionadas');
    } finally {
      setLoadingAssignments(false);
    }
  };

  const handleCloseDialog = () => {
    setEditDialogOpen(false);
    setSelectedUser(null);
    setSelectedRoles([]);
    setRoleAssignments([]);
    setChangeReason('');
    setDialogError(null);
  };

  const handleRoleChange = (event: SelectChangeEvent<RoleValue[]>) => {
    setDialogError(null);
    const value = event.target.value;
    setSelectedRoles(normalizeRoleSelection(typeof value === 'string' ? value.split(',') : value));
  };

  const handleSaveRoles = async () => {
    if (!selectedUser || !hasPendingRoleChanges) return;
    const reason = changeReason.trim();
    if (!reason) {
      setDialogError('Explica el motivo del cambio antes de enviarlo a revisión.');
      return;
    }

    try {
      setSaving(true);
      setDialogError(null);
      const normalizedRoles = normalizeRoleSelection(selectedRoles);
      const selectedKeys = new Set(normalizedRoles.map((role) => role.toLocaleLowerCase('es')));
      const currentKeys = new Set(
        normalizeRoleSelection(selectedUser.roles).map((role) => role.toLocaleLowerCase('es')),
      );
      const changedRoles = securityRoles.filter((role) => (
        selectedKeys.has(role.code.toLocaleLowerCase('es'))
        !== currentKeys.has(role.code.toLocaleLowerCase('es'))
      ));
      const assignmentByRoleId = new Map(roleAssignments.map((assignment) => [assignment.roleId, assignment]));
      let submittedCount = 0;

      for (const [index, role] of changedRoles.entries()) {
        const desiredActive = selectedKeys.has(role.code.toLocaleLowerCase('es'));
        const assignment = assignmentByRoleId.get(role.id);
        const correlationId = `web-security-${selectedUser.partyId}-${Date.now()}-${index}-${role.id}`;
        const revision = await apiClient.createPartyRoleRevision({
          partyId: selectedUser.partyId,
          roleId: role.id,
          desiredActive,
          expectedVersion: assignment?.version ?? 0,
          reason,
          sourcePlatform: 'web',
          correlationId,
        });
        await apiClient.submitSecurityRevision(revision.id);
        submittedCount += 1;
      }

      setSubmissionSuccess(
        `${submittedCount === 1 ? 'Se envió 1 cambio' : `Se enviaron ${submittedCount} cambios`} a revisión. `
        + 'Los roles actuales no cambiarán hasta que otra persona autorizada los apruebe.',
      );
      handleCloseDialog();
    } catch (err) {
      setDialogError(err instanceof Error ? err.message : 'No se pudieron enviar los cambios de roles a revisión');
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
      {submissionSuccess && (
        <Alert severity="success" onClose={() => setSubmissionSuccess(null)} sx={{ mb: 2 }}>
          {submissionSuccess}
        </Alert>
      )}
      {users.length === 0 ? (
        <Paper variant="outlined" sx={{ p: 3 }}>
          <Stack spacing={1}>
            <Typography variant="h6" fontWeight={700}>
              Roles y permisos
            </Typography>
            <Typography variant="body2" color="text.secondary">
              Todavía no hay usuarios administrables. Cuando exista el primero, verás sus datos clave y roles
              editables aquí.
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
                    {!singleUserContactSummary && (
                      <Typography variant="body2" color="text.secondary">
                        {EMPTY_CONTACT_LABEL}
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
                        onClick={() => void handleEditClick(singleUser)}
                        aria-label={buildEditRolesLabel(
                          singleUser,
                          userIdsRequiringIdentityDisambiguator.has(singleUser.id),
                          singleUser.roles,
                        )}
                        title={buildRoleButtonTitle({
                          roles: singleUser.roles,
                          roleByCode,
                          user: singleUser,
                          showIdentityDisambiguator: userIdsRequiringIdentityDisambiguator.has(singleUser.id),
                        })}
                        sx={{
                          borderRadius: 1,
                          display: 'inline-flex',
                          justifyContent: 'flex-start',
                          maxWidth: '100%',
                          textAlign: 'left',
                        }}
                      >
                        {renderRoleEditButtonContents(singleUser.roles, roleByCode)}
                      </ButtonBase>
                    </Stack>
                  </Stack>
                </Stack>
              </Stack>
            </Paper>
          ) : (
            <LazyPaginatedList
              items={users}
              pagination={{ itemLabel: 'usuarios', initialRowsPerPage: 25 }}
              renderItems={(visibleUsers) => (
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
                      {visibleUsers.map((user) => {
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
                                    {EMPTY_CONTACT_LABEL}
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
                                onClick={() => void handleEditClick(user)}
                                aria-label={buildEditRolesLabel(user, showIdentityDisambiguator, user.roles)}
                                title={buildRoleButtonTitle({
                                  roles: user.roles,
                                  roleByCode,
                                  user,
                                  showIdentityDisambiguator,
                                })}
                                sx={{
                                  borderRadius: 1,
                                  display: 'inline-flex',
                                  justifyContent: 'flex-start',
                                  maxWidth: '100%',
                                  textAlign: 'left',
                                }}
                              >
                                {renderRoleEditButtonContents(user.roles, roleByCode)}
                              </ButtonBase>
                            </TableCell>
                          </TableRow>
                        );
                      })}
                    </TableBody>
                  </Table>
                </TableContainer>
              )}
            />
          )}
        </Stack>
      )}

      <Dialog open={editDialogOpen} onClose={handleCloseDialog} maxWidth="sm" fullWidth>
        <DialogTitle>{editDialogTitle}</DialogTitle>
        <DialogContent>
          {dialogError && (
            <Alert severity="error" sx={{ mt: 2 }}>
              {dialogError}
            </Alert>
          )}
          {loadingAssignments && (
            <Box display="flex" alignItems="center" gap={1} sx={{ mt: 2 }} role="status">
              <CircularProgress size={20} />
              <Typography variant="body2">Cargando asignaciones y versiones vigentes…</Typography>
            </Box>
          )}
          <FormControl fullWidth sx={{ mt: 2 }}>
            <InputLabel id="roles-label" shrink>Roles</InputLabel>
            <Select<RoleValue[]>
              labelId="roles-label"
              multiple
              value={selectedRoles}
              onChange={handleRoleChange}
              disabled={loadingAssignments || Boolean(dialogError)}
              input={<OutlinedInput label="Roles" />}
              displayEmpty
              renderValue={(selected) => (
                <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                  {selected.length === 0 ? (
                    <Chip label={EMPTY_ROLES_LABEL} size="small" variant="outlined" />
                  ) : (
                    selected.map((role) => (
                      <Chip
                        key={role}
                        label={roleByCode.get(role.toLocaleLowerCase('es'))?.nameEs ?? role}
                        size="small"
                        color={getRoleColor(roleByCode.get(role.toLocaleLowerCase('es')))}
                      />
                    ))
                  )}
                </Box>
              )}
            >
              {currentRoleKeysForEditor.size > 0 && (
                <ListSubheader disableSticky data-testid="role-editor-current-roles-header">
                  Roles actuales
                </ListSubheader>
              )}
              {roleOptionsForEditor.map((role, index) => {
                const isCurrentRole = currentRoleKeysForEditor.has(role.toLocaleLowerCase('es'));
                const previousRole = roleOptionsForEditor[index - 1];
                const previousWasCurrentRole = previousRole
                  ? currentRoleKeysForEditor.has(previousRole.toLocaleLowerCase('es'))
                  : false;
                const startsAvailableRoles = !isCurrentRole && (index === 0 || previousWasCurrentRole);
                const persistedRole = roleByCode.get(role.toLocaleLowerCase('es'));
                const optionItem = (
                  <MenuItem key={role} value={role}>
                    <Stack spacing={0}>
                      <Typography variant="body2">{persistedRole?.nameEs ?? role}</Typography>
                      {persistedRole && persistedRole.nameEs !== persistedRole.code && (
                        <Typography variant="caption" color="text.secondary">{persistedRole.code}</Typography>
                      )}
                    </Stack>
                  </MenuItem>
                );

                if (!startsAvailableRoles) {
                  return optionItem;
                }

                return [
                  <ListSubheader
                    key={`${role}-available-roles`}
                    disableSticky
                    data-testid="role-editor-available-roles-header"
                  >
                    Roles disponibles
                  </ListSubheader>,
                  optionItem,
                ];
              })}
            </Select>
            <FormHelperText>
              {hasPendingRoleChanges
                ? (pendingRoleChangesSummary ?? 'Listo para enviar esta actualización a revisión.')
                : 'Sin cambios pendientes. Modifica la selección para preparar una revisión.'}
            </FormHelperText>
          </FormControl>
          {hasPendingRoleChanges && (
            <TextField
              label="Motivo del cambio"
              value={changeReason}
              onChange={(event) => {
                setChangeReason(event.target.value);
                setDialogError(null);
              }}
              helperText="Quedará registrado en el historial inmutable y será visible para quien revise."
              multiline
              minRows={3}
              required
              fullWidth
              inputProps={{ maxLength: 2000 }}
              sx={{ mt: 2 }}
            />
          )}
          <Alert severity="info" sx={{ mt: 2 }}>
            Enviar no modifica permisos de inmediato. Otra persona con autorización debe aprobar cada cambio.
          </Alert>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog} disabled={saving}>
            {hasPendingRoleChanges ? 'Descartar cambios' : 'Cerrar'}
          </Button>
          {hasPendingRoleChanges && (
            <Button
              onClick={() => void handleSaveRoles()}
              variant="contained"
              disabled={saving || loadingAssignments || !changeReason.trim()}
            >
              {saving ? 'Enviando…' : 'Enviar a revisión'}
            </Button>
          )}
        </DialogActions>
      </Dialog>
    </Box>
  );
}
