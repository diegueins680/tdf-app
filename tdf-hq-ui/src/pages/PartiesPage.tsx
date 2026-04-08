import { useEffect, useMemo, useState, type ChangeEvent, type MouseEvent } from 'react';
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
  Menu,
  MenuItem,
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
  OutlinedInput,
} from '@mui/material';
import type { SelectChangeEvent } from '@mui/material/Select';
import EditIcon from '@mui/icons-material/Edit';
import SchoolIcon from '@mui/icons-material/School';
import AddIcon from '@mui/icons-material/Add';
import MoreHorizIcon from '@mui/icons-material/MoreHoriz';
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

const normalizePartyContactValue = (value?: string | null) => {
  const trimmed = value?.trim();
  return trimmed ? trimmed : null;
};

const getPartyContactSummary = (party: Pick<PartyDTO, 'primaryEmail' | 'instagram'>) => {
  const contactParts = [
    normalizePartyContactValue(party.primaryEmail),
    normalizePartyContactValue(party.instagram),
  ].filter((value): value is string => Boolean(value));

  return contactParts.length > 0 ? contactParts.join(' · ') : 'Falta correo e Instagram';
};

const formatPartyCountLabel = (count: number) => `${count} contacto${count === 1 ? '' : 's'}`;
const hasPartyPrimaryEmail = (party?: Pick<PartyDTO, 'primaryEmail'> | null) =>
  normalizePartyContactValue(party?.primaryEmail) != null;

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
            {`Tipo actual: ${isOrg ? 'Empresa' : 'Persona'}. Usa Persona para individuos y Empresa para bandas, sellos o negocios.`}
          </Typography>
          <Button
            variant="outlined"
            onClick={() => setIsOrg((prev) => !prev)}
            sx={{ alignSelf: 'flex-start' }}
          >
            {isOrg ? 'Cambiar a persona' : 'Cambiar a empresa'}
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

  const needsPrimaryEmail = !hasPartyPrimaryEmail(party);

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
      <DialogTitle>
        {needsPrimaryEmail ? 'Completar correo y crear usuario' : 'Crear usuario'}
        {party?.displayName ? ` para ${party.displayName}` : ''}
      </DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField
            label="Correo del contacto"
            type="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            helperText={needsPrimaryEmail
              ? 'Este correo se guardará en el contacto y se usará para enviar la contraseña temporal.'
              : 'Se actualiza en el contacto antes de crear la cuenta.'}
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
              {loading ? 'Creando...' : needsPrimaryEmail ? 'Guardar correo y crear usuario' : 'Crear usuario'}
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
  const [actionsMenuTarget, setActionsMenuTarget] = useState<{
    anchorEl: HTMLElement;
    party: PartyDTO;
  } | null>(null);
  const canManageRoles = canAccessPath('/configuracion/roles-permisos', session?.roles, session?.modules);
  const hasContacts = parties.length > 0;
  const trimmedSearch = search.trim();
  const showInitialLoadingState = partiesQuery.isLoading && partiesQuery.data == null;

  const filtered = useMemo(() => {
    const term = trimmedSearch.toLowerCase();
    if (!term) return parties;
    return parties.filter((party) => {
      const haystack = [party.displayName, party.primaryEmail ?? '', party.instagram ?? '']
        .join(' ')
        .toLowerCase();
      return haystack.includes(term);
    });
  }, [parties, trimmedSearch]);
  const showSearchField = !showInitialLoadingState && (parties.length > 1 || trimmedSearch !== '');
  const showSearchEmptyState = !partiesQuery.isLoading && hasContacts && filtered.length === 0 && trimmedSearch !== '';
  const showSingleContactGuidance = !partiesQuery.isLoading && parties.length === 1 && trimmedSearch === '';
  const showTableGuidance = !partiesQuery.isLoading && filtered.length > 0 && !showSingleContactGuidance;
  const showSearchContextSummary = !partiesQuery.isLoading && hasContacts && filtered.length > 0 && trimmedSearch !== '';
  const searchContextSummary = showSearchContextSummary
    ? `Mostrando ${filtered.length} de ${formatPartyCountLabel(parties.length)} para "${trimmedSearch}".`
    : '';
  const singleContactGuidanceText = 'Solo hay un contacto por ahora. Usa su nombre para ver relaciones y abre Acciones para editarlo o gestionar su acceso. El buscador aparecerá cuando exista el segundo contacto.';
  const tableGuidanceText = canManageRoles
    ? 'Haz clic en el nombre para ver relaciones. Contacto reúne correo e Instagram en una sola columna. Abre Acciones para editar el contacto o crear la cuenta cuando haga falta. Roles y accesos aparece solo cuando ese contacto ya tiene usuario.'
    : 'Haz clic en el nombre para ver relaciones. Contacto reúne correo e Instagram en una sola columna. Abre Acciones para editar el contacto o crear la cuenta cuando haga falta.';

  const openActionsMenu = (event: MouseEvent<HTMLButtonElement>, party: PartyDTO) => {
    setActionsMenuTarget({ anchorEl: event.currentTarget, party });
  };

  const closeActionsMenu = () => {
    setActionsMenuTarget(null);
  };

  const runPartyMenuAction = (action: (party: PartyDTO) => void) => {
    const party = actionsMenuTarget?.party;
    closeActionsMenu();
    if (party) {
      action(party);
    }
  };

  const createUserActionLabel = hasPartyPrimaryEmail(actionsMenuTarget?.party)
    ? 'Crear usuario y enviar contraseña'
    : 'Completar correo y crear usuario';

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
          {showSearchField && (
            <TextField
              aria-label="Buscar contactos"
              placeholder="Buscar por nombre, email o Instagram"
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

        {showInitialLoadingState ? (
          <Alert severity="info" variant="outlined">
            Cargando contactos… El buscador y la tabla aparecerán cuando esta primera carga termine.
          </Alert>
        ) : !partiesQuery.isLoading && !partiesQuery.error && !hasContacts ? (
          <Alert severity="info" variant="outlined">
            Todavía no hay contactos. Crea el primero desde Nuevo contacto. El buscador y la tabla aparecerán cuando exista al menos un contacto.
          </Alert>
        ) : showSearchEmptyState ? (
          <Alert
            severity="info"
            variant="outlined"
            action={(
              <Button color="inherit" size="small" onClick={() => setSearch('')}>
                Limpiar búsqueda
              </Button>
            )}
          >
            {`No hay contactos que coincidan con "${trimmedSearch}". Limpia la búsqueda para volver a ver toda la lista.`}
          </Alert>
        ) : (
          <>
            {showSingleContactGuidance && (
              <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mb: 1.5 }}>
                {singleContactGuidanceText}
              </Typography>
            )}
            {showTableGuidance && (
              showSearchContextSummary ? (
                <Stack
                  direction="row"
                  spacing={1}
                  alignItems="center"
                  flexWrap="wrap"
                  useFlexGap
                  sx={{ mb: 1.5 }}
                >
                  <Typography variant="caption" color="text.secondary">
                    {searchContextSummary}
                  </Typography>
                  <Button size="small" onClick={() => setSearch('')}>
                    Limpiar búsqueda
                  </Button>
                </Stack>
              ) : (
                <Typography variant="caption" color="text.secondary" sx={{ display: 'block', mb: 1.5 }}>
                  {tableGuidanceText}
                </Typography>
              )
            )}
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Nombre</TableCell>
                  <TableCell>Contacto</TableCell>
                  <TableCell align="right">Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {partiesQuery.isLoading && (
                  <TableRow>
                    <TableCell colSpan={3}>Cargando...</TableCell>
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
                        {party.hasUserAccount && (
                          <Chip label="Usuario creado" size="small" color="success" variant="outlined" />
                        )}
                      </Stack>
                    </TableCell>
                    <TableCell>{getPartyContactSummary(party)}</TableCell>
                    <TableCell align="right">
                      <Tooltip title="Acciones">
                        <IconButton
                          onClick={(event) => openActionsMenu(event, party)}
                          aria-label={`Abrir acciones para ${party.displayName}`}
                          aria-haspopup="menu"
                          aria-expanded={actionsMenuTarget?.party.partyId === party.partyId ? 'true' : undefined}
                        >
                          <MoreHorizIcon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </>
        )}
      </Paper>

      <Menu
        open={Boolean(actionsMenuTarget)}
        anchorEl={actionsMenuTarget?.anchorEl ?? null}
        onClose={closeActionsMenu}
      >
        <MenuItem onClick={() => runPartyMenuAction((party) => setEditing(party))}>
          <EditIcon fontSize="small" sx={{ mr: 1 }} />
          Editar contacto
        </MenuItem>
        {!actionsMenuTarget?.party.hasUserAccount && (
          <MenuItem onClick={() => runPartyMenuAction((party) => setUserDialogParty(party))}>
            <PersonAddAltIcon fontSize="small" sx={{ mr: 1 }} />
            {createUserActionLabel}
          </MenuItem>
        )}
        {canManageRoles && actionsMenuTarget?.party.hasUserAccount && (
          <MenuItem onClick={() => runPartyMenuAction(() => navigate('/configuracion/roles-permisos'))}>
            <SchoolIcon fontSize="small" sx={{ mr: 1 }} />
            Roles y accesos
          </MenuItem>
        )}
      </Menu>

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
