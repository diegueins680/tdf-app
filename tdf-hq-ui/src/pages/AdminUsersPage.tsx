import { useMemo, useState } from 'react';
import {
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  FormControlLabel,
  IconButton,
  Stack,
  Tooltip,
  Typography,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Link as RouterLink } from 'react-router-dom';
import { Admin, type AdminUser } from '../api/admin';
import AdminUserCommunicationDialog from '../components/AdminUserCommunicationDialog';

const normalizeContactValue = (value?: string | null) => {
  const trimmed = value?.trim();
  if (trimmed == null || trimmed === '') return null;
  return trimmed;
};

const getUserContactSummary = (user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail'>) => {
  const preferredPhone = normalizeContactValue(user.whatsapp) ?? normalizeContactValue(user.primaryPhone);
  const email = normalizeContactValue(user.primaryEmail);

  if (preferredPhone && email) return `${preferredPhone} · ${email}`;
  return preferredPhone ?? email;
};

const hasUserContactChannel = (user: Pick<AdminUser, 'whatsapp' | 'primaryPhone' | 'primaryEmail'>) =>
  Boolean(getUserContactSummary(user));

const getUserAccessSummary = (values: string[]) =>
  Array.from(new Set(values.map((value) => value.trim()).filter(Boolean))).join(', ');

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

  const usersMissingContactCount = useMemo(
    () => (usersQuery.data ?? []).filter((user) => !hasUserContactChannel(user)).length,
    [usersQuery.data],
  );
  const usersMissingContactVerb = usersMissingContactCount === 1 ? 'sigue' : 'siguen';

  return (
    <>
      <Stack spacing={3}>
        <Stack spacing={1}>
          <Stack direction="row" justifyContent="space-between" alignItems="center">
            <Typography variant="h4" fontWeight={700}>Usuarios (admin API)</Typography>
            <Stack direction="row" spacing={1} alignItems="center">
              <FormControlLabel
                control={
                  <Checkbox
                    checked={includeInactive}
                    onChange={(event) => setIncludeInactive(event.target.checked)}
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
          {usersMissingContactCount > 0 && (
            <Typography variant="body2" color="text.secondary">
              Comunicación solo aparece cuando el usuario ya tiene WhatsApp, teléfono o correo.
              {` ${usersMissingContactCount} usuario${usersMissingContactCount === 1 ? '' : 's'} `}
              {usersMissingContactVerb} sin canal de contacto; complétalo desde Perfil.
            </Typography>
          )}
        </Stack>

        <Card>
          <CardContent>
            {usersQuery.isLoading && <Typography>Cargando usuarios…</Typography>}
            {usersQuery.error && <Typography color="error">Error al cargar usuarios</Typography>}
            {usersQuery.data?.length === 0 && (
              <Typography color="text.secondary">No hay usuarios.</Typography>
            )}
            {usersQuery.data?.length ? (
              <Stack spacing={1.5}>
                {usersQuery.data.map((user) => (
                  <UserRow
                    key={user.userId}
                    user={user}
                    onOpenCommunications={() => setSelectedUser(user)}
                  />
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
  const contactSummary = getUserContactSummary(user);
  const hasContactInfo = Boolean(contactSummary);
  const rolesSummary = getUserAccessSummary(user.roles);
  const modulesSummary = getUserAccessSummary(user.modules);

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
      {!hasContactInfo && <Chip label="Falta contacto" color="warning" size="small" variant="outlined" />}
      {(rolesSummary || modulesSummary) && (
        <Box sx={{ minWidth: 220, flex: '1 1 240px' }}>
          {rolesSummary && (
            <Typography variant="body2" color="text.secondary">
              Roles: {rolesSummary}
            </Typography>
          )}
          {modulesSummary && (
            <Typography variant="body2" color="text.secondary">
              Módulos: {modulesSummary}
            </Typography>
          )}
        </Box>
      )}
      <Stack direction="row" spacing={1} sx={{ ml: 'auto' }}>
        <Button component={RouterLink} to={`/perfil/${user.partyId}`} size="small" variant="outlined">
          Perfil
        </Button>
        {hasContactInfo && (
          <Button size="small" variant="contained" onClick={onOpenCommunications}>
            Comunicación
          </Button>
        )}
      </Stack>
    </Box>
  );
}
