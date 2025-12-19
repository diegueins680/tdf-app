import { useEffect, useMemo, useState } from 'react';
import {
  Typography,
  Stack,
  Paper,
  TextField,
  Button,
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
  InputAdornment,
  Chip,
  MenuItem,
  Select,
  FormControl,
  InputLabel,
} from '@mui/material';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import EditIcon from '@mui/icons-material/Edit';
import SearchIcon from '@mui/icons-material/Search';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import type { PartyDTO, PartyCreate, PartyUpdate } from '../api/types';
import { Parties } from '../api/parties';
import PartyRelatedPopover from '../components/PartyRelatedPopover';

const STATUS_OPTIONS = ['Nuevo', 'Contactado', 'En progreso', 'Ganado', 'Perdido'] as const;
type LeadStatus = (typeof STATUS_OPTIONS)[number];

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
              onChange={(e) => setStatus(e.target.value as LeadStatus)}
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

  const leads = useMemo(() => {
    const term = search.trim().toLowerCase();
    return (data ?? [])
      .filter((p) => !p.isOrg)
      .filter((p) => {
        if (!term) return true;
        return (
          p.displayName.toLowerCase().includes(term) ||
          (p.primaryEmail?.toLowerCase().includes(term) ?? false) ||
          (p.notes?.toLowerCase().includes(term) ?? false) ||
          (p.primaryPhone?.toLowerCase().includes(term) ?? false)
        );
      });
  }, [data, search]);

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
        <TextField
          placeholder="Buscar por nombre, correo, teléfono o nota"
          value={search}
          onChange={(e) => setSearch(e.target.value)}
          fullWidth
          InputProps={{
            startAdornment: (
              <InputAdornment position="start">
                <SearchIcon />
              </InputAdornment>
            ),
          }}
        />
      </Paper>

      <Paper sx={{ p: 2, borderRadius: 3, boxShadow: '0 8px 24px rgba(15,23,42,0.08)' }}>
        {isError && <Alert severity="error">{error?.message ?? 'No se pudieron cargar los leads'}</Alert>}
        {isLoading ? (
          <Typography>Cargando...</Typography>
        ) : leads.length === 0 ? (
          <Typography color="text.secondary">No hay leads aún.</Typography>
        ) : (
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
        )}
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
