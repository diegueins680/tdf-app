import { useEffect, useMemo, useState } from 'react';
import type { ChangeEvent } from 'react';
import { useQuery, useMutation, useQueryClient, type UseQueryResult } from '@tanstack/react-query';
import { Parties } from '../api/parties';
import type { PartyDTO, PartyCreate, PartyUpdate } from '../api/types';
import {
  Typography,
  Stack,
  Paper,
  TextField,
  Button,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  IconButton,
  Chip,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  Tooltip,
  Alert,
  InputAdornment,
  Box,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  OutlinedInput,
} from '@mui/material';
import type { SelectChangeEvent } from '@mui/material/Select';
import EditIcon from '@mui/icons-material/Edit';
import SchoolIcon from '@mui/icons-material/School';
import AddIcon from '@mui/icons-material/Add';
import SearchIcon from '@mui/icons-material/Search';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import { useNavigate } from 'react-router-dom';
import type { Role } from '../api/generated/client';
import { Admin } from '../api/admin';
import { ALL_ROLES } from '../constants/roles';
import { useSession } from '../session/SessionContext';
import { canAccessPath } from '../utils/accessControl';
import { normalizeRolesInput } from '../utils/roles';
import PartyRelatedPopover from '../components/PartyRelatedPopover';

type RoleValue = Role | (string & Record<never, never>);

interface CreatePartyDialogProps {
  open: boolean;
  onClose: () => void;
}

function CreatePartyDialog({ open, onClose }: CreatePartyDialogProps) {
  const qc = useQueryClient();
  const [name, setName] = useState('');
  const [isOrg, setIsOrg] = useState(false);

  const mutation = useMutation<PartyDTO, Error, PartyCreate>({
    mutationFn: (body) => Parties.create(body),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['parties'] });
      setName('');
      onClose();
    },
  });

  const handleSubmit = () => {
    if (!name) return;
    mutation.mutate({ cDisplayName: name, cIsOrg: isOrg });
  };

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Nuevo contacto</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField label="Nombre / Display" value={name} onChange={(e) => setName(e.target.value)} required />
          <Typography variant="body2" color="text.secondary">
            Usa Persona para individuos y Empresa para bandas, sellos o negocios.
          </Typography>
          <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
            <Chip
              label={isOrg ? 'Empresa' : 'Persona'}
              color={isOrg ? 'primary' : 'default'}
              variant={isOrg ? 'filled' : 'outlined'}
              size="small"
            />
            <Button
              variant="outlined"
              onClick={() => setIsOrg((prev) => !prev)}
              sx={{ alignSelf: 'flex-start' }}
            >
              {isOrg ? 'Cambiar a persona' : 'Cambiar a empresa'}
            </Button>
          </Stack>
          {mutation.isError && mutation.error && (
            <Alert severity="error">{mutation.error.message}</Alert>
          )}
        </Stack>
      </DialogContent>
      <DialogActions>
          <Button onClick={onClose}>Cancelar</Button>
          <Button onClick={handleSubmit} variant="contained" disabled={!name || mutation.isPending}>
            Crear
          </Button>
        </DialogActions>
    </Dialog>
  );
}

interface EditPartyDialogProps {
  party: PartyDTO | null;
  open: boolean;
  onClose: () => void;
}

function EditPartyDialog({ party, open, onClose }: EditPartyDialogProps) {
  const qc = useQueryClient();
  const [instagram, setInstagram] = useState(party?.instagram ?? '');
  const [phone, setPhone] = useState(party?.primaryPhone ?? '');
  const [displayName, setDisplayName] = useState(party?.displayName ?? '');
  const [email, setEmail] = useState(party?.primaryEmail ?? '');

  useEffect(() => {
    setInstagram(party?.instagram ?? '');
    setPhone(party?.primaryPhone ?? '');
    setDisplayName(party?.displayName ?? '');
    setEmail(party?.primaryEmail ?? '');
  }, [party, open]);

  const mutation = useMutation<PartyDTO, Error, PartyUpdate>({
    mutationFn: (body) => {
      if (!party) {
        return Promise.reject(new Error('Contacto no disponible'));
      }
      return Parties.update(party.partyId, body);
    },
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['parties'] });
      onClose();
    },
  });

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Editar {party?.displayName}</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField label="Display" value={displayName} onChange={(e) => setDisplayName(e.target.value)} />
          <TextField
            label="Email"
            type="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            helperText="Se usa como contacto principal y para crear accesos de usuario."
          />
          <TextField label="Instagram" value={instagram} onChange={(e) => setInstagram(e.target.value)} />
          <TextField label="Teléfono" value={phone} onChange={(e) => setPhone(e.target.value)} />
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button
          onClick={() =>
            mutation.mutate({
              uDisplayName: displayName,
              uInstagram: instagram,
              uPrimaryPhone: phone,
              uPrimaryEmail: email.trim() || null,
            })
          }
          variant="contained"
          disabled={mutation.isPending}
        >
          Guardar
        </Button>
      </DialogActions>
    </Dialog>
  );
}

interface CreateUserFromPartyDialogProps {
  party: PartyDTO | null;
  open: boolean;
  onClose: () => void;
}

function CreateUserFromPartyDialog({ party, open, onClose }: CreateUserFromPartyDialogProps) {
  const qc = useQueryClient();
  const [username, setUsername] = useState('');
  const [roles, setRoles] = useState<RoleValue[]>([]);
  const [email, setEmail] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    setUsername(party?.primaryEmail ?? '');
    setEmail(party?.primaryEmail ?? '');
    setRoles([]);
    setError(null);
    setSuccess(null);
  }, [party, open]);

  const selectRoles = (value: string | string[]) => normalizeRolesInput(value, ALL_ROLES);

  const handleRoleChange = (event: SelectChangeEvent<RoleValue[]>) => {
    setRoles(selectRoles(event.target.value));
  };

  const handleCreateUser = async () => {
    if (!party) return;
    const trimmedEmail = email.trim();
    if (!trimmedEmail) {
      setError('Necesitas ingresar un correo principal para crear el usuario.');
      return;
    }
    try {
      setLoading(true);
      setError(null);
      if (trimmedEmail !== (party.primaryEmail ?? '')) {
        await Parties.update(party.partyId, { uPrimaryEmail: trimmedEmail });
      }
      await Admin.createUser({
        partyId: party.partyId,
        username: username.trim() || undefined,
        roles: roles.length ? roles : undefined,
      });
      setSuccess('Se creó la cuenta y se envió la contraseña temporal por correo.');
      await qc.invalidateQueries({ queryKey: ['parties'] });
    } catch (err) {
      setError(err instanceof Error ? err.message : 'No se pudo crear el usuario');
    } finally {
      setLoading(false);
    }
  };

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Crear usuario para {party?.displayName}</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField
            label="Correo del contacto"
            type="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            helperText="Se actualiza en el contacto antes de crear la cuenta."
            required
            fullWidth
          />
          <TextField
            label="Usuario (opcional)"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            helperText="Si lo dejas vacío se usará el correo principal."
            fullWidth
          />
          <FormControl fullWidth>
            <InputLabel id="create-user-roles-label">Roles iniciales</InputLabel>
            <Select<RoleValue[]>
              labelId="create-user-roles-label"
              multiple
              value={roles}
              onChange={handleRoleChange}
              input={<OutlinedInput label="Roles iniciales" />}
              renderValue={(selected) => (
                <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
                  {selected.map((role) => (
                    <Chip key={role} label={role} size="small" />
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
          {error && <Alert severity="error">{error}</Alert>}
          {success && <Alert severity="success">{success}</Alert>}
          <Alert severity="info">
            El usuario recibirá un correo con una contraseña temporal y podrá iniciar sesión usando su correo o usuario.
          </Alert>
        </Stack>
      </DialogContent>
      <DialogActions>
        {success ? (
          <Button variant="contained" onClick={onClose}>
            Cerrar
          </Button>
        ) : (
          <>
            <Button onClick={onClose} disabled={loading}>
              Cancelar
            </Button>
            <Button variant="contained" onClick={() => void handleCreateUser()} disabled={loading}>
              {loading ? 'Creando...' : 'Crear usuario'}
            </Button>
          </>
        )}
      </DialogActions>
    </Dialog>
  );
}

export default function PartiesPage() {
  const navigate = useNavigate();
  const { session } = useSession();
  const partiesQuery: UseQueryResult<PartyDTO[], Error> = useQuery<PartyDTO[], Error>({
    queryKey: ['parties'],
    queryFn: Parties.list,
  });
  const parties = useMemo<PartyDTO[]>(() => partiesQuery.data ?? [], [partiesQuery.data]);
  const [editing, setEditing] = useState<PartyDTO | null>(null);
  const [createOpen, setCreateOpen] = useState(false);
  const [search, setSearch] = useState('');
  const [userDialogParty, setUserDialogParty] = useState<PartyDTO | null>(null);
  const [relatedParty, setRelatedParty] = useState<PartyDTO | null>(null);
  const [relatedAnchor, setRelatedAnchor] = useState<HTMLElement | null>(null);
  const canManageRoles = canAccessPath('/configuracion/roles-permisos', session?.roles, session?.modules);
  const hasContacts = parties.length > 0;
  const showDirectoryChrome = partiesQuery.isLoading || hasContacts;

  const filtered = useMemo(() => {
    const term = search.toLowerCase();
    return parties.filter((p) => p.displayName.toLowerCase().includes(term));
  }, [parties, search]);

  return (
    <Stack gap={3}>
      <Stack spacing={1}>
        <Typography variant="h4">Personas / CRM</Typography>
        <Typography variant="body2" color="text.secondary">
          Gestiona personas, empresas y roles en un solo lugar. El tipo se elige dentro del formulario de alta.
        </Typography>
      </Stack>

      <Paper sx={{ p: 3 }}>
        <Stack
          direction={{ xs: 'column', md: 'row' }}
          justifyContent="space-between"
          alignItems={{ xs: 'stretch', md: 'center' }}
          spacing={2}
          sx={{ mb: 2 }}
        >
          {showDirectoryChrome && (
            <TextField
              aria-label="Buscar contactos"
              placeholder="Buscar..."
              value={search}
              onChange={(e: ChangeEvent<HTMLInputElement>) => setSearch(e.target.value)}
              inputProps={{ 'aria-label': 'Buscar contactos' }}
              InputProps={{
                startAdornment: (
                  <InputAdornment position="start">
                    <SearchIcon sx={{ color: 'text.secondary' }} />
                  </InputAdornment>
                ),
              }}
              fullWidth
            />
          )}
          <Stack direction="row" spacing={1}>
            <Button
              variant="contained"
              startIcon={<AddIcon />}
              onClick={() => setCreateOpen(true)}
            >
              Nuevo contacto
            </Button>
          </Stack>
        </Stack>

        {partiesQuery.error && <Alert severity="error">{partiesQuery.error.message}</Alert>}

        {!partiesQuery.isLoading && !partiesQuery.error && !hasContacts ? (
          <Alert severity="info" variant="outlined">
            Todavía no hay contactos. Crea el primero desde Nuevo contacto. El buscador y la tabla aparecerán cuando exista al menos un contacto.
          </Alert>
        ) : (
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>Nombre</TableCell>
                <TableCell>Email</TableCell>
                <TableCell>Instagram</TableCell>
                <TableCell align="right">Acciones</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {partiesQuery.isLoading && (
                <TableRow>
                  <TableCell colSpan={4}>Cargando...</TableCell>
                </TableRow>
              )}
              {!partiesQuery.isLoading && filtered.map((party) => (
                <TableRow key={party.partyId} hover>
                  <TableCell>
                    <Stack direction="row" spacing={1} alignItems="center">
                      <Button
                        variant="text"
                        onClick={(event) => {
                          setRelatedParty(party);
                          setRelatedAnchor(event.currentTarget);
                        }}
                        sx={{ p: 0, minWidth: 0, textTransform: 'none', justifyContent: 'flex-start' }}
                      >
                        <Typography fontWeight={600} sx={{ textDecoration: 'underline', textUnderlineOffset: 3 }}>
                          {party.displayName}
                        </Typography>
                      </Button>
                      {party.isOrg && <Chip label="Empresa" size="small" />}
                    </Stack>
                  </TableCell>
                  <TableCell>{party.primaryEmail ?? '—'}</TableCell>
                  <TableCell>{party.instagram ?? '—'}</TableCell>
                  <TableCell align="right">
                    <Tooltip title="Editar contacto">
                      <IconButton
                        onClick={() => setEditing(party)}
                        aria-label={`Editar contacto ${party.displayName}`}
                      >
                        <EditIcon fontSize="small" />
                      </IconButton>
                    </Tooltip>
                    <Tooltip
                      title={
                        party.hasUserAccount
                          ? 'Este contacto ya tiene usuario'
                          : party.primaryEmail
                            ? 'Crear usuario y enviar contraseña'
                            : 'Agrega o corrige el correo antes de crear la cuenta'
                      }
                    >
                      <span>
                        <IconButton
                          onClick={() => setUserDialogParty(party)}
                          disabled={Boolean(party.hasUserAccount)}
                          aria-label={`Crear usuario para ${party.displayName}`}
                        >
                          <PersonAddAltIcon fontSize="small" />
                        </IconButton>
                      </span>
                    </Tooltip>
                    {canManageRoles && (
                      <Tooltip title="Roles y accesos">
                        <IconButton
                          onClick={() => navigate('/configuracion/roles-permisos')}
                          aria-label={`Abrir roles y accesos para ${party.displayName}`}
                        >
                          <SchoolIcon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                    )}
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        )}
      </Paper>

      <CreatePartyDialog open={createOpen} onClose={() => setCreateOpen(false)} />
      <EditPartyDialog party={editing} open={!!editing} onClose={() => setEditing(null)} />
      <CreateUserFromPartyDialog
        party={userDialogParty}
        open={Boolean(userDialogParty)}
        onClose={() => setUserDialogParty(null)}
      />
      <PartyRelatedPopover
        party={relatedParty}
        anchorEl={relatedAnchor}
        onClose={() => {
          setRelatedParty(null);
          setRelatedAnchor(null);
        }}
      />
    </Stack>
  );
}
