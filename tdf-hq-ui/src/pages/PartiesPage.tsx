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
import GroupAddIcon from '@mui/icons-material/GroupAdd';
import SearchIcon from '@mui/icons-material/Search';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import { useNavigate } from 'react-router-dom';
import type { PartyRole } from '../api/generated/client';
import { Admin } from '../api/admin';
import { ALL_ROLES } from '../constants/roles';

interface CreatePartyDialogProps {
  open: boolean;
  onClose: () => void;
  defaultIsOrg: boolean;
}

function CreatePartyDialog({ open, onClose, defaultIsOrg }: CreatePartyDialogProps) {
  const qc = useQueryClient();
  const [name, setName] = useState('');
  const [isOrg, setIsOrg] = useState(defaultIsOrg);

  useEffect(() => {
    setIsOrg(defaultIsOrg);
  }, [defaultIsOrg, open]);

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
          <Button
            variant={isOrg ? 'contained' : 'outlined'}
            onClick={() => setIsOrg((prev) => !prev)}
            sx={{ alignSelf: 'flex-start' }}
          >
            {isOrg ? 'Empresa' : 'Persona'}
          </Button>
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
  const [roles, setRoles] = useState<PartyRole[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    setUsername(party?.primaryEmail ?? '');
    setRoles([]);
    setError(null);
    setSuccess(null);
  }, [party, open]);

  const selectRoles = (value: string | string[]) => {
    const entries = Array.isArray(value) ? value : value.split(',');
    return entries.filter((role): role is PartyRole => ALL_ROLES.includes(role as PartyRole));
  };

  const handleRoleChange = (event: SelectChangeEvent<PartyRole[]>) => {
    setRoles(selectRoles(event.target.value));
  };

  const handleCreateUser = async () => {
    if (!party) return;
    if (!party.primaryEmail) {
      setError('Este contacto necesita un email principal antes de crear el usuario.');
      return;
    }
    try {
      setLoading(true);
      setError(null);
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
            label="Usuario (opcional)"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            helperText="Si lo dejas vacío se usará el correo principal."
            fullWidth
          />
          <FormControl fullWidth>
            <InputLabel id="create-user-roles-label">Roles iniciales</InputLabel>
            <Select<PartyRole[]>
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
  const partiesQuery: UseQueryResult<PartyDTO[], Error> = useQuery<PartyDTO[], Error>({
    queryKey: ['parties'],
    queryFn: Parties.list,
  });
  const parties = useMemo<PartyDTO[]>(() => partiesQuery.data ?? [], [partiesQuery.data]);
  const [editing, setEditing] = useState<PartyDTO | null>(null);
  const [createOpen, setCreateOpen] = useState(false);
  const [createDialogIsOrg, setCreateDialogIsOrg] = useState(false);
  const [search, setSearch] = useState('');
  const [userDialogParty, setUserDialogParty] = useState<PartyDTO | null>(null);

  const filtered = useMemo(() => {
    const term = search.toLowerCase();
    return parties.filter((p) => p.displayName.toLowerCase().includes(term));
  }, [parties, search]);

  return (
    <Stack gap={3}>
      <Stack spacing={1}>
        <Typography variant="h4">Personas / CRM</Typography>
        <Typography variant="body2" color="text.secondary">
          Gestiona leads, empresas y roles en un solo lugar.
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
          <TextField
            placeholder="Buscar..."
            value={search}
            onChange={(e: ChangeEvent<HTMLInputElement>) => setSearch(e.target.value)}
            InputProps={{
              startAdornment: (
                <InputAdornment position="start">
                  <SearchIcon sx={{ color: 'text.secondary' }} />
                </InputAdornment>
              ),
            }}
            fullWidth
          />
          <Stack direction="row" spacing={1}>
            <Button
              variant="outlined"
              startIcon={<GroupAddIcon />}
              onClick={() => {
                setCreateDialogIsOrg(true);
                setCreateOpen(true);
              }}
            >
              Nueva banda
            </Button>
            <Button
              variant="contained"
              startIcon={<AddIcon />}
              onClick={() => {
                setCreateDialogIsOrg(false);
                setCreateOpen(true);
              }}
            >
              Nueva persona
            </Button>
          </Stack>
        </Stack>

        {partiesQuery.error && <Alert severity="error">{partiesQuery.error.message}</Alert>}

        <Table size="small">
          <TableHead>
            <TableRow>
              <TableCell>Nombre</TableCell>
              <TableCell>Org</TableCell>
              <TableCell>Email</TableCell>
              <TableCell>Instagram</TableCell>
              <TableCell align="right">Acciones</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {partiesQuery.isLoading && (
              <TableRow>
                <TableCell colSpan={5}>Cargando...</TableCell>
              </TableRow>
            )}
            {!partiesQuery.isLoading && filtered.map((party) => (
              <TableRow key={party.partyId} hover>
                <TableCell>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <Typography fontWeight={600}>{party.displayName}</Typography>
                    {party.isOrg && <Chip label="ORG" size="small" />}
                  </Stack>
                </TableCell>
                <TableCell>{party.isOrg ? 'Sí' : 'No'}</TableCell>
                <TableCell>{party.primaryEmail ?? '—'}</TableCell>
                <TableCell>{party.instagram ?? '—'}</TableCell>
                <TableCell align="right">
                  <Tooltip title="Editar contacto">
                    <IconButton onClick={() => setEditing(party)}>
                      <EditIcon fontSize="small" />
                    </IconButton>
                  </Tooltip>
                  <Tooltip
                    title={
                      party.hasUserAccount
                        ? 'Este contacto ya tiene usuario'
                        : party.primaryEmail
                          ? 'Crear usuario y enviar contraseña'
                          : 'Agrega un correo para crear acceso'
                    }
                  >
                    <span>
                      <IconButton
                        onClick={() => setUserDialogParty(party)}
                        disabled={Boolean(party.hasUserAccount) || !party.primaryEmail}
                      >
                        <PersonAddAltIcon fontSize="small" />
                      </IconButton>
                    </span>
                  </Tooltip>
                  <Tooltip title="Roles y accesos">
                    <IconButton onClick={() => navigate('/configuracion/roles-permisos')}>
                      <SchoolIcon fontSize="small" />
                    </IconButton>
                  </Tooltip>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </Paper>

      <CreatePartyDialog open={createOpen} onClose={() => setCreateOpen(false)} defaultIsOrg={createDialogIsOrg} />
      <EditPartyDialog party={editing} open={!!editing} onClose={() => setEditing(null)} />
      <CreateUserFromPartyDialog
        party={userDialogParty}
        open={Boolean(userDialogParty)}
        onClose={() => setUserDialogParty(null)}
      />
    </Stack>
  );
}
