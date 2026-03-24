import { useState } from 'react';
import {
  Box,
  Button,
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
import { Link as RouterLink } from 'react-router-dom';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Admin, type AdminUser } from '../api/admin';
import AdminUserCommunicationDialog from '../components/AdminUserCommunicationDialog';

const summarizeUserContacts = (user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail'>) => {
  const preferredPhone = user.whatsapp?.trim() || user.primaryPhone?.trim() || '';
  const email = user.primaryEmail?.trim() || '';
  const parts = [preferredPhone, email].filter((value) => value !== '');
  return parts.length > 0 ? parts.join(' · ') : null;
};

export default function AdminUsersPage() {
  const qc = useQueryClient();
  const [includeInactive, setIncludeInactive] = useState(false);
  const [selectedUser, setSelectedUser] = useState<AdminUser | null>(null);

  const usersQuery = useQuery({
    queryKey: ['admin', 'users', includeInactive],
    queryFn: () => Admin.listUsers(includeInactive),
  });

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['admin', 'users'] });
  };

  return (
    <>
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
            <span>
              <IconButton
                aria-label="Refrescar lista de usuarios"
                onClick={handleRefresh}
                disabled={usersQuery.isFetching}
              >
                <RefreshIcon />
              </IconButton>
            </span>
          </Tooltip>
        </Stack>
      </Stack>

      <Card>
        <CardContent>
          {usersQuery.isLoading && <Typography>Cargando usuarios…</Typography>}
          {usersQuery.error && (
            <Typography color="error">Error al cargar usuarios</Typography>
          )}
          {usersQuery.data?.length === 0 && (
            <Typography color="text.secondary">No hay usuarios.</Typography>
          )}
          {usersQuery.data?.length ? (
            <Stack spacing={1.5}>
              {usersQuery.data.map((u) => (
                <UserRow key={u.userId} user={u} onOpenCommunications={() => setSelectedUser(u)} />
              ))}
            </Stack>
          ) : null}
        </CardContent>
      </Card>
      </Stack>
      <AdminUserCommunicationDialog
        open={Boolean(selectedUser)}
        user={selectedUser}
        onClose={() => setSelectedUser(null)}
      />
    </>
  );
}

function UserRow({ user, onOpenCommunications }: { user: AdminUser; onOpenCommunications: () => void }) {
  const contactSummary = summarizeUserContacts(user);

  return (
    <Box
      data-testid={`admin-user-row-${user.userId}`}
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
        {contactSummary && (
          <Typography variant="body2" color="text.secondary">
            {contactSummary}
          </Typography>
        )}
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
      <Stack direction="row" spacing={1} sx={{ ml: 'auto' }}>
        <Button component={RouterLink} to={`/perfil/${user.partyId}`} size="small" variant="outlined">
          Perfil
        </Button>
        <Button size="small" variant="contained" onClick={onOpenCommunications}>
          Comunicación
        </Button>
      </Stack>
    </Box>
  );
}
