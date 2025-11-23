import { useState } from 'react';
import {
  Box,
  Card,
  CardContent,
  Checkbox,
  FormControlLabel,
  Stack,
  Typography,
  Chip,
  Tooltip,
  IconButton,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Admin, type AdminUser } from '../api/admin';

export default function AdminUsersPage() {
  const qc = useQueryClient();
  const [includeInactive, setIncludeInactive] = useState(false);

  const usersQuery = useQuery({
    queryKey: ['admin', 'users', includeInactive],
    queryFn: () => Admin.listUsers(includeInactive),
  });

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['admin', 'users'] });
  };

  return (
    <Stack spacing={3}>
      <Stack direction="row" justifyContent="space-between" alignItems="center">
        <Typography variant="h4" fontWeight={700}>Usuarios (admin API)</Typography>
        <Stack direction="row" spacing={1} alignItems="center">
          <FormControlLabel
            control={
              <Checkbox
                checked={includeInactive}
                onChange={(e) => setIncludeInactive(e.target.checked)}
              />
            }
            label="Incluir inactivos"
          />
          <Tooltip title="Refrescar">
            <IconButton onClick={handleRefresh} disabled={usersQuery.isFetching}>
              <RefreshIcon />
            </IconButton>
          </Tooltip>
        </Stack>
      </Stack>

      <Card>
        <CardContent>
          {usersQuery.isLoading && <Typography>Cargando usuarios…</Typography>}
          {usersQuery.error && (
            <Typography color="error">Error al cargar usuarios</Typography>
          )}
          {usersQuery.data && usersQuery.data.length === 0 && (
            <Typography color="text.secondary">No hay usuarios.</Typography>
          )}
          {usersQuery.data && usersQuery.data.length > 0 && (
            <Stack spacing={1.5}>
              {usersQuery.data.map((u) => (
                <UserRow key={u.userId} user={u} />
              ))}
            </Stack>
          )}
        </CardContent>
      </Card>
    </Stack>
  );
}

function UserRow({ user }: { user: AdminUser }) {
  return (
    <Box
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
        <Typography variant="subtitle1" fontWeight={700}>{user.username}</Typography>
        <Typography variant="body2" color="text.secondary">
          {user.partyName} · ID {user.partyId}
        </Typography>
      </Box>
      <Chip label={user.active ? 'Activo' : 'Inactivo'} color={user.active ? 'success' : 'default'} size="small" />
      <Stack direction="row" spacing={0.5} flexWrap="wrap">
        {user.roles.map((r) => (
          <Chip key={r} label={r} size="small" variant="outlined" />
        ))}
      </Stack>
      <Stack direction="row" spacing={0.5} flexWrap="wrap">
        {user.modules.map((m) => (
          <Chip key={m} label={m} size="small" color="info" variant="outlined" />
        ))}
      </Stack>
    </Box>
  );
}
