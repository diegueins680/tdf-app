import { useDeferredValue, useMemo, useState } from 'react';
import {
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  FormControlLabel,
  IconButton,
  InputAdornment,
  Link,
  Stack,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import ClearIcon from '@mui/icons-material/Clear';
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

const normalizeSearchValue = (value: string) => value.trim().toLowerCase();

const formatUserCountLabel = (count: number) => `${count} usuario${count === 1 ? '' : 's'}`;

const summarizeUserIdentity = (user: Pick<AdminUser, 'partyId' | 'partyName' | 'username'>) => {
  const displayName = user.partyName.trim();
  const username = user.username.trim();
  const primary = displayName || username;
  const showUsername = displayName !== '' && displayName.toLowerCase() !== username.toLowerCase();
  const secondaryParts: string[] = [];

  if (showUsername) secondaryParts.push(`Usuario: ${username}`);
  secondaryParts.push(`ID ${user.partyId}`);

  return {
    primary,
    secondary: secondaryParts.join(' · '),
  };
};

const matchesUserQuery = (user: AdminUser, rawQuery: string) => {
  const query = normalizeSearchValue(rawQuery);
  if (!query) return true;

  const searchSpace = [
    user.username,
    user.partyName,
    String(user.userId),
    String(user.partyId),
    `id ${user.partyId}`,
    getUserContactSummary(user) ?? '',
    getUserAccessSummary(user.roles),
    getUserAccessSummary(user.modules),
    user.active ? 'activo' : 'inactivo',
  ]
    .map(normalizeSearchValue)
    .filter(Boolean);

  return searchSpace.some((value) => value.includes(query));
};

export default function AdminUsersPage() {
  const qc = useQueryClient();
  const [includeInactive, setIncludeInactive] = useState(false);
  const [selectedUser, setSelectedUser] = useState<AdminUser | null>(null);
  const [searchQuery, setSearchQuery] = useState('');
  const deferredSearchQuery = useDeferredValue(searchQuery);

  const usersQuery = useQuery({
    queryKey: ['admin', 'users', includeInactive],
    queryFn: () => Admin.listUsers(includeInactive),
  });

  const handleRefresh = () => {
    void qc.invalidateQueries({ queryKey: ['admin', 'users'] });
  };

  const handleClearSearch = () => {
    setSearchQuery('');
  };

  const visibleUsers = useMemo(
    () => (usersQuery.data ?? []).filter((user) => matchesUserQuery(user, deferredSearchQuery)),
    [deferredSearchQuery, usersQuery.data],
  );
  const visibleUsersMissingContactCount = useMemo(
    () => visibleUsers.filter((user) => !hasUserContactChannel(user)).length,
    [visibleUsers],
  );
  const visibleUsersWithContactCount = visibleUsers.length - visibleUsersMissingContactCount;
  const totalUsersCount = usersQuery.data?.length ?? 0;
  const hasUsers = totalUsersCount > 0;
  const hasActiveSearch = normalizeSearchValue(searchQuery).length > 0;
  const activeSearchSummary = searchQuery.trim();
  const hasMultipleUsers = totalUsersCount > 1;
  const isFiltered = hasActiveSearch && visibleUsers.length !== totalUsersCount;
  const showSearchField = hasMultipleUsers || hasActiveSearch;
  const showMixedContactStateGuidance = visibleUsersMissingContactCount > 0 && visibleUsersWithContactCount > 0;
  const showSingleUserGuidance = totalUsersCount === 1 && !hasActiveSearch;
  const showClearSearchAction = showSearchField && hasActiveSearch;
  const showProfileLinkGuidance = visibleUsers.length > 0;
  const visibleUsersSummary = useMemo(() => {
    if (!hasUsers || showSingleUserGuidance || visibleUsers.length === 0) return '';

    const parts: string[] = [];

    if (isFiltered) {
      parts.push(`Mostrando ${visibleUsers.length} de ${totalUsersCount} usuarios.`);
    } else if (hasMultipleUsers) {
      parts.push(`${formatUserCountLabel(visibleUsers.length)} en esta vista.`);
    }

    if (showSearchField && !hasActiveSearch) {
      parts.push('Busca por nombre, ID, contacto o acceso.');
    }

    if (showMixedContactStateGuidance) {
      parts.push(
        `${visibleUsersWithContactCount} ${visibleUsersWithContactCount === 1 ? 'listo' : 'listos'} para comunicación y `
        + `${visibleUsersMissingContactCount} ${visibleUsersMissingContactCount === 1 ? 'pendiente' : 'pendientes'} de contacto.`,
      );
    }

    return parts.join(' ');
  }, [
    hasActiveSearch,
    hasMultipleUsers,
    hasUsers,
    isFiltered,
    showMixedContactStateGuidance,
    showSearchField,
    showSingleUserGuidance,
    totalUsersCount,
    visibleUsers.length,
    visibleUsersMissingContactCount,
    visibleUsersWithContactCount,
  ]);

  return (
    <>
      <Stack spacing={3}>
        <Stack spacing={1}>
          <Stack
            direction={{ xs: 'column', lg: 'row' }}
            justifyContent="space-between"
            alignItems={{ xs: 'stretch', lg: 'center' }}
            spacing={2}
          >
            <Stack spacing={1} sx={{ minWidth: 0, flex: '1 1 360px' }}>
              <Typography variant="h4" fontWeight={700}>Usuarios (admin API)</Typography>
              {showSearchField && (
                <TextField
                  label="Buscar usuarios"
                  value={searchQuery}
                  onChange={(event) => setSearchQuery(event.target.value)}
                  size="small"
                  fullWidth
                  placeholder="Usuario, nombre, ID, contacto o acceso"
                  InputProps={{
                    endAdornment: showClearSearchAction ? (
                      <InputAdornment position="end">
                        <IconButton size="small" aria-label="Limpiar búsqueda" onClick={handleClearSearch}>
                          <ClearIcon fontSize="small" />
                        </IconButton>
                      </InputAdornment>
                    ) : null,
                  }}
                />
              )}
              {showSingleUserGuidance && (
                <Typography variant="body2" color="text.secondary">
                  Solo hay un usuario por ahora. Cuando exista el segundo, aquí aparecerán búsqueda y resumen de resultados.
                </Typography>
              )}
              {visibleUsersSummary && (
                <Typography variant="body2" color="text.secondary">
                  {visibleUsersSummary}
                </Typography>
              )}
              {showProfileLinkGuidance && (
                <Typography variant="body2" color="text.secondary">
                  Haz clic en el nombre para abrir el perfil.
                </Typography>
              )}
            </Stack>
            <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
              {hasUsers && (
                <>
                  <FormControlLabel
                    control={(
                      <Checkbox
                        checked={includeInactive}
                        onChange={(event) => setIncludeInactive(event.target.checked)}
                      />
                    )}
                    label="Incluir inactivos"
                  />
                </>
              )}
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
        </Stack>

        <Card>
          <CardContent>
            {usersQuery.isLoading && <Typography>Cargando usuarios…</Typography>}
            {usersQuery.error && <Typography color="error">Error al cargar usuarios</Typography>}
            {usersQuery.data?.length === 0 && (
              <Typography color="text.secondary">
                No hay usuarios todavía. Cuando exista el primero, aquí aparecerán búsqueda, filtros y señales de contacto para revisar la lista más rápido.
              </Typography>
            )}
            {usersQuery.data?.length && visibleUsers.length === 0 ? (
              <Typography color="text.secondary">
                {activeSearchSummary
                  ? `No hay coincidencias para "${activeSearchSummary}".`
                  : 'No hay coincidencias para este filtro.'}
              </Typography>
            ) : null}
            {visibleUsers.length ? (
              <Stack spacing={1.5}>
                {visibleUsers.map((user) => (
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
  const identity = summarizeUserIdentity(user);
  const profilePath = `/perfil/${user.partyId}`;

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
        <Link
          component={RouterLink}
          to={profilePath}
          underline="hover"
          color="primary"
          variant="subtitle1"
          aria-label={`Abrir perfil de ${identity.primary}`}
          sx={{ display: 'inline-flex', width: 'fit-content', fontWeight: 700 }}
        >
          {identity.primary}
        </Link>
        <Typography variant="body2" color="text.secondary">
          {identity.secondary}
        </Typography>
        {contactSummary && (
          <Typography variant="body2" color="text.secondary">
            {contactSummary}
          </Typography>
        )}
      </Box>
      <Chip label={user.active ? 'Activo' : 'Inactivo'} color={user.active ? 'success' : 'default'} size="small" />
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
        {hasContactInfo ? (
          <Button size="small" variant="contained" onClick={onOpenCommunications}>
            Comunicación
          </Button>
        ) : (
          <Button component={RouterLink} to={profilePath} size="small" variant="outlined">
            Completar contacto
          </Button>
        )}
      </Stack>
    </Box>
  );
}
