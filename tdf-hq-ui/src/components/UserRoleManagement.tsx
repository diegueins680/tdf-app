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
  IconButton,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  Select,
  MenuItem,
  FormControl,
  InputLabel,
  OutlinedInput,
  CircularProgress,
  Alert,
  Tooltip,
  Typography,
} from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';
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

const normalizeContactValue = (value?: string | null) => {
  const trimmed = value?.trim();
  return trimmed ? trimmed : null;
};

const getContactLines = (user: Pick<NormalizedUser, 'email' | 'phone'>) =>
  [normalizeContactValue(user.email), normalizeContactValue(user.phone)].filter(
    (value): value is string => value != null,
  );

export default function UserRoleManagement() {
  const [users, setUsers] = useState<NormalizedUser[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [editDialogOpen, setEditDialogOpen] = useState(false);
  const [selectedUser, setSelectedUser] = useState<NormalizedUser | null>(null);
  const [selectedRoles, setSelectedRoles] = useState<RoleValue[]>([]);
  const [saving, setSaving] = useState(false);

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
        roles: (u.roles ?? []) as RoleValue[],
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
    setSelectedRoles(user.roles);
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
    if (!selectedUser) return;

    try {
      setSaving(true);
      await apiClient.updateUserRoles(selectedUser.id, selectedRoles);
      setUsers((prev) => prev.map((u) => (u.id === selectedUser.id ? { ...u, roles: selectedRoles } : u)));
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
      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>ID</TableCell>
              <TableCell>Usuario</TableCell>
              <TableCell>Contacto</TableCell>
              <TableCell>Estado</TableCell>
              <TableCell>Roles</TableCell>
              <TableCell>Acciones</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {users.map((user) => {
              const contactLines = getContactLines(user);

              return (
                <TableRow key={user.id}>
                <TableCell>{user.id}</TableCell>
                <TableCell>{user.name}</TableCell>
                <TableCell>
                  {contactLines.length > 0 ? (
                    <Box display="flex" flexDirection="column" gap={0.25}>
                      {contactLines.map((line) => (
                        <Typography key={`${user.id}-${line}`} variant="body2">
                          {line}
                        </Typography>
                      ))}
                    </Box>
                  ) : (
                    <Typography variant="body2" color="text.secondary">
                      Sin email ni teléfono
                    </Typography>
                  )}
                </TableCell>
                <TableCell>
                  <Chip label={user.status} color={STATUS_COLORS[user.status]} size="small" />
                </TableCell>
                <TableCell>
                  <Box display="flex" gap={0.5} flexWrap="wrap">
                    {user.roles.map((role) => (
                      <Chip key={role} label={role} color={getRoleColor(role)} size="small" />
                    ))}
                    {user.roles.length === 0 && <Chip label="No roles" size="small" variant="outlined" />}
                  </Box>
                </TableCell>
                <TableCell>
                  <Tooltip title="Editar roles">
                    <IconButton
                      size="small"
                      color="primary"
                      onClick={() => handleEditClick(user)}
                      aria-label={`Editar roles de ${user.name}`}
                    >
                      <EditIcon />
                    </IconButton>
                  </Tooltip>
                </TableCell>
                </TableRow>
              );
            })}
          </TableBody>
        </Table>
      </TableContainer>

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
          </FormControl>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleCloseDialog} disabled={saving}>
            Cancelar
          </Button>
          <Button onClick={() => void handleSaveRoles()} variant="contained" disabled={saving}>
            {saving ? 'Guardando...' : 'Guardar'}
          </Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
}
