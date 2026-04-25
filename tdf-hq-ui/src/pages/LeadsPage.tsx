import { useEffect, useMemo, useState } from 'react';
import {
  Typography,
  Stack,
  Paper,
  TextField,
  Button,
  Box,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Alert,
  IconButton,
  InputAdornment,
  Chip,
  MenuItem,
  Select,
  FormControl,
  InputLabel,
  Tooltip,
} from '@mui/material';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import EditIcon from '@mui/icons-material/Edit';
import SearchIcon from '@mui/icons-material/Search';
import ClearIcon from '@mui/icons-material/Clear';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import type { PartyDTO, PartyCreate, PartyUpdate } from '../api/types';
import { Parties } from '../api/parties';
import PartyRelatedPopover from '../components/PartyRelatedPopover';

const STATUS_OPTIONS = ['Nuevo', 'Contactado', 'En progreso', 'Ganado', 'Perdido'] as const;
type LeadStatus = (typeof STATUS_OPTIONS)[number];
const isLeadStatus = (value: string): value is LeadStatus =>
  STATUS_OPTIONS.some((status) => status === value);

const normalizeLeadFieldValue = (value?: string | null) => {
  const trimmed = value?.trim();
  return trimmed ? trimmed : null;
};

const getLeadContactSummary = (lead: Pick<PartyDTO, 'primaryEmail' | 'primaryPhone'>) => {
  const contactParts = [
    normalizeLeadFieldValue(lead.primaryEmail),
    normalizeLeadFieldValue(lead.primaryPhone),
  ].filter((value): value is string => value != null);

  return contactParts.length > 0 ? contactParts.join(' · ') : 'Falta correo y teléfono';
};

interface LeadCreateDialogProps {
  open: boolean;
  onClose: () => void;
}

function LeadCreateDialog({ open, onClose }: LeadCreateDialogProps) {
  const qc = useQueryClient();
  const [name, setName] = useState('');
  const [email, setEmail] = useState('');
  const [phone, setPhone] = useState('');
  const [source, setSource] = useState('');
  const [status, setStatus] = useState<LeadStatus>('Nuevo');
  const [notes, setNotes] = useState('');
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!open) {
      setName('');
      setEmail('');
      setPhone('');
      setSource('');
      setStatus('Nuevo');
      setNotes('');
      setError(null);
    }
  }, [open]);

  const mutation = useMutation<PartyDTO, Error, PartyCreate>({
    mutationFn: (body) => Parties.create(body),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['parties'] });
      onClose();
    },
    onError: (err) => setError(err.message),
  });

  const handleSubmit = () => {
    if (!name.trim()) {
      setError('Nombre requerido');
      return;
    }
    setError(null);
    mutation.mutate({
      cDisplayName: name.trim(),
      cIsOrg: false,
      cPrimaryEmail: email.trim() || null,
      cPrimaryPhone: phone.trim() || null,
      cNotes: [status, source, notes].filter(Boolean).join(' · ') || null,
    });
  };

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Nuevo lead</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField label="Nombre" value={name} onChange={(e) => setName(e.target.value)} required />
          <TextField label="Correo" type="email" value={email} onChange={(e) => setEmail(e.target.value)} />
          <TextField label="Teléfono" value={phone} onChange={(e) => setPhone(e.target.value)} />
          <TextField label="Fuente (campaña, referidor)" value={source} onChange={(e) => setSource(e.target.value)} />
          <FormControl>
            <InputLabel id="status-label">Estado</InputLabel>
            <Select
              labelId="status-label"
              label="Estado"
              value={status}
              onChange={(e) => {
                const nextStatus = e.target.value.trim();
                setStatus(isLeadStatus(nextStatus) ? nextStatus : 'Nuevo');
              }}
            >
              {STATUS_OPTIONS.map((opt) => (
                <MenuItem key={opt} value={opt}>
                  {opt}
                </MenuItem>
              ))}
            </Select>
          </FormControl>
          <TextField
            label="Notas"
            value={notes}
            onChange={(e) => setNotes(e.target.value)}
            multiline
            minRows={3}
          />
          {error && <Alert severity="error">{error}</Alert>}
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button onClick={handleSubmit} variant="contained" disabled={mutation.isPending}>
          Crear
        </Button>
      </DialogActions>
    </Dialog>
  );
}

interface LeadEditDialogProps {
  lead: PartyDTO | null;
  open: boolean;
  onClose: () => void;
}

function LeadEditDialog({ lead, open, onClose }: LeadEditDialogProps) {
  const qc = useQueryClient();
  const [name, setName] = useState(lead?.displayName ?? '');
  const [email, setEmail] = useState(lead?.primaryEmail ?? '');
  const [phone, setPhone] = useState(lead?.primaryPhone ?? '');
  const [notes, setNotes] = useState(lead?.notes ?? '');
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setName(lead?.displayName ?? '');
    setEmail(lead?.primaryEmail ?? '');
    setPhone(lead?.primaryPhone ?? '');
    setNotes(lead?.notes ?? '');
    setError(null);
  }, [lead, open]);

  const mutation = useMutation<PartyDTO, Error, PartyUpdate>({
    mutationFn: (body) => {
      if (!lead) return Promise.reject(new Error('Lead no disponible'));
      return Parties.update(lead.partyId, body);
    },
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['parties'] });
      onClose();
    },
    onError: (err) => setError(err.message),
  });

  const handleSave = () => {
    if (!lead) return;
    mutation.mutate({
      uDisplayName: name.trim() || lead.displayName,
      uPrimaryEmail: email.trim() || null,
      uPrimaryPhone: phone.trim() || null,
      uNotes: notes.trim() || null,
    });
  };

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Editar lead</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField label="Nombre" value={name} onChange={(e) => setName(e.target.value)} />
          <TextField label="Correo" type="email" value={email} onChange={(e) => setEmail(e.target.value)} />
          <TextField label="Teléfono" value={phone} onChange={(e) => setPhone(e.target.value)} />
          <TextField
            label="Notas / Estado"
            value={notes}
            onChange={(e) => setNotes(e.target.value)}
            multiline
            minRows={3}
          />
          {error && <Alert severity="error">{error}</Alert>}
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button onClick={handleSave} variant="contained" disabled={mutation.isPending}>
          Guardar
        </Button>
      </DialogActions>
    </Dialog>
  );
}

export default function LeadsPage() {
  const [search, setSearch] = useState('');
  const [createOpen, setCreateOpen] = useState(false);
  const [editOpen, setEditOpen] = useState(false);
  const [selected, setSelected] = useState<PartyDTO | null>(null);
  const [relatedParty, setRelatedParty] = useState<PartyDTO | null>(null);
  const [relatedAnchor, setRelatedAnchor] = useState<HTMLElement | null>(null);

  const { data, isLoading, isError, error } = useQuery<PartyDTO[], Error>({
    queryKey: ['parties'],
    queryFn: () => Parties.list(),
  });

  const trimmedSearch = search.trim();
  const allLeads = useMemo(() => (data ?? []).filter((party) => !party.isOrg), [data]);
  const leads = useMemo(() => {
    const term = trimmedSearch.toLowerCase();
    return allLeads
      .filter((p) => {
        if (!term) return true;
        return (
          p.displayName.toLowerCase().includes(term) ||
          (p.primaryEmail?.toLowerCase().includes(term) ?? false) ||
          (p.notes?.toLowerCase().includes(term) ?? false) ||
          (p.primaryPhone?.toLowerCase().includes(term) ?? false)
        );
      });
  }, [allLeads, trimmedSearch]);

  const hasLeads = allLeads.length > 0;
  const showInitialLoadingState = isLoading && data == null;
  const showSearchField = !showInitialLoadingState && (allLeads.length > 1 || trimmedSearch !== '');
  const showClearSearchAction = showSearchField && trimmedSearch !== '';
  const showSearchEmptyState = !isLoading && hasLeads && leads.length === 0 && trimmedSearch !== '';
  const showSingleLeadSummary = !isLoading && leads.length === 1;
  const singleLead = showSingleLeadSummary ? (leads[0] ?? null) : null;
  const singleLeadSummaryTitle = trimmedSearch === ''
    ? 'Primer lead registrado'
    : `1 coincidencia para "${trimmedSearch}"`;
  const singleLeadSummaryDescription = trimmedSearch === ''
    ? 'Revísalo aquí sin tabla ni buscador. Cuando llegue el segundo, volverá la vista comparativa para revisar contacto y seguimiento lado a lado.'
    : 'La búsqueda dejó un solo lead visible. Revísalo aquí; limpia o ajusta el buscador para volver a comparar leads en la tabla.';
  const tableGuidance = trimmedSearch === ''
    ? 'Haz clic en el nombre para revisar relaciones. Notas / Estado concentra estado, fuente y siguiente paso en una sola columna. Usa Editar solo cuando necesites actualizarlo.'
    : `Mostrando ${leads.length} de ${allLeads.length} leads para "${trimmedSearch}".`;

  return (
    <Stack gap={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems={{ xs: 'flex-start', md: 'center' }} gap={2}>
        <Stack spacing={0.5}>
          <Typography variant="h4" fontWeight={800}>CRM / Leads</Typography>
          <Typography variant="body1" color="text.secondary">
            Captura y gestiona leads rápido. Usa notas para estado, fuente o siguiente paso.
          </Typography>
        </Stack>
        <Button variant="contained" startIcon={<PersonAddAltIcon />} onClick={() => setCreateOpen(true)}>
          Nuevo lead
        </Button>
      </Stack>

      <Paper sx={{ p: 2.5, borderRadius: 3, boxShadow: '0 10px 30px rgba(15,23,42,0.12)' }}>
        <Stack spacing={2}>
          {showSearchField && (
            <TextField
              label="Buscar leads"
              aria-label="Buscar leads"
              placeholder="Buscar por nombre, correo, teléfono o nota"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              fullWidth
              inputProps={{ 'aria-label': 'Buscar leads' }}
              InputProps={{
                startAdornment: (
                  <InputAdornment position="start">
                    <SearchIcon />
                  </InputAdornment>
                ),
                endAdornment: showClearSearchAction ? (
                  <InputAdornment position="end">
                    <Tooltip title="Limpiar búsqueda">
                      <IconButton
                        size="small"
                        aria-label="Limpiar búsqueda"
                        onClick={() => setSearch('')}
                      >
                        <ClearIcon fontSize="small" />
                      </IconButton>
                    </Tooltip>
                  </InputAdornment>
                ) : null,
              }}
            />
          )}

          {isError && <Alert severity="error">{error?.message ?? 'No se pudieron cargar los leads'}</Alert>}
          {showInitialLoadingState ? (
            <Alert severity="info" variant="outlined">
              Cargando leads… El buscador y la tabla aparecerán cuando termine esta primera carga.
            </Alert>
          ) : !isLoading && !isError && !hasLeads ? (
            <Alert severity="info" variant="outlined">
              Todavía no hay leads. Crea el primero desde Nuevo lead. El primer lead aparecerá aquí como resumen y la tabla volverá cuando exista un segundo para comparar.
            </Alert>
          ) : showSearchEmptyState ? (
            <Alert severity="info" variant="outlined">
              {`No hay leads que coincidan con "${trimmedSearch}". Limpia o ajusta la búsqueda desde el campo de arriba para volver a la lista completa.`}
            </Alert>
          ) : showSingleLeadSummary && singleLead ? (
            <Box
              sx={{
                border: '1px solid',
                borderColor: 'divider',
                borderRadius: 2,
                p: 2,
                bgcolor: 'background.default',
              }}
            >
              <Stack spacing={2}>
                <Stack spacing={0.75}>
                  <Typography variant="subtitle1" fontWeight={700}>
                    {singleLeadSummaryTitle}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    {singleLeadSummaryDescription}
                  </Typography>
                </Stack>

                <Stack spacing={1}>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }}>
                    <Button
                      variant="text"
                      onClick={(event) => {
                        setRelatedParty(singleLead);
                        setRelatedAnchor(event.currentTarget);
                      }}
                      sx={{ p: 0, minWidth: 0, textTransform: 'none', justifyContent: 'flex-start' }}
                    >
                      <Typography fontWeight={700} sx={{ textDecoration: 'underline', textUnderlineOffset: 3 }}>
                        {singleLead.displayName}
                      </Typography>
                    </Button>
                    {singleLead.hasUserAccount && <Chip label="Cuenta de usuario" size="small" color="primary" />}
                  </Stack>

                  <Typography variant="body2">
                    Contacto: {getLeadContactSummary(singleLead)}
                  </Typography>
                  <Typography variant="body2" color="text.secondary">
                    {normalizeLeadFieldValue(singleLead.notes)
                      ? `Contexto: ${singleLead.notes?.trim()}`
                      : 'Todavía no hay contexto registrado. Usa Editar lead para agregar estado, fuente o siguiente paso.'}
                  </Typography>
                </Stack>

                <Button
                  variant="outlined"
                  startIcon={<EditIcon />}
                  onClick={() => {
                    setSelected(singleLead);
                    setEditOpen(true);
                  }}
                  sx={{ alignSelf: 'flex-start' }}
                >
                  Editar lead
                </Button>
              </Stack>
            </Box>
          ) : (
            <>
              {!isLoading && leads.length > 1 && (
                <Typography variant="caption" color="text.secondary" sx={{ display: 'block' }}>
                  {tableGuidance}
                </Typography>
              )}
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>Lead</TableCell>
                    <TableCell>Correo</TableCell>
                    <TableCell>Teléfono</TableCell>
                    <TableCell>Notas / Estado</TableCell>
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {leads.map((l) => (
                    <TableRow key={l.partyId} hover>
                      <TableCell>
                        <Stack spacing={0.5}>
                          <Button
                            variant="text"
                            onClick={(event) => {
                              setRelatedParty(l);
                              setRelatedAnchor(event.currentTarget);
                            }}
                            sx={{ p: 0, minWidth: 0, textTransform: 'none', justifyContent: 'flex-start', alignSelf: 'flex-start' }}
                          >
                            <Typography fontWeight={700} sx={{ textDecoration: 'underline', textUnderlineOffset: 3 }}>
                              {l.displayName}
                            </Typography>
                          </Button>
                          {l.hasUserAccount && <Chip label="Cuenta de usuario" size="small" color="primary" />}
                        </Stack>
                      </TableCell>
                      <TableCell>{l.primaryEmail ?? '—'}</TableCell>
                      <TableCell>{l.primaryPhone ?? '—'}</TableCell>
                      <TableCell>{l.notes ?? '—'}</TableCell>
                      <TableCell align="right">
                        <Button
                          size="small"
                          startIcon={<EditIcon />}
                          onClick={() => {
                            setSelected(l);
                            setEditOpen(true);
                          }}
                        >
                          Editar
                        </Button>
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </>
          )}
        </Stack>
      </Paper>

      <LeadCreateDialog open={createOpen} onClose={() => setCreateOpen(false)} />
      <LeadEditDialog lead={selected} open={editOpen} onClose={() => setEditOpen(false)} />
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
