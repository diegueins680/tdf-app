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
} from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';
import AddBusinessIcon from '@mui/icons-material/AddBusiness';
import SearchIcon from '@mui/icons-material/Search';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import type { PartyDTO, PartyCreate, PartyUpdate } from '../api/types';
import { Parties } from '../api/parties';

interface CreateCompanyDialogProps {
  open: boolean;
  onClose: () => void;
}

function CreateCompanyDialog({ open, onClose }: CreateCompanyDialogProps) {
  const qc = useQueryClient();
  const [displayName, setDisplayName] = useState('');
  const [legalName, setLegalName] = useState('');
  const [email, setEmail] = useState('');
  const [taxId, setTaxId] = useState('');
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!open) {
      setDisplayName('');
      setLegalName('');
      setEmail('');
      setTaxId('');
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
    if (!displayName.trim()) {
      setError('Nombre de empresa requerido');
      return;
    }
    setError(null);
    mutation.mutate({
      cDisplayName: displayName.trim(),
      cLegalName: legalName.trim() || null,
      cPrimaryEmail: email.trim() || null,
      cTaxId: taxId.trim() || null,
      cIsOrg: true,
    });
  };

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Nueva empresa</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField
            label="Nombre comercial"
            value={displayName}
            onChange={(e) => setDisplayName(e.target.value)}
            required
          />
          <TextField
            label="Razón social"
            value={legalName}
            onChange={(e) => setLegalName(e.target.value)}
          />
          <TextField
            label="Correo principal"
            type="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
          />
          <TextField
            label="RUC / Tax ID"
            value={taxId}
            onChange={(e) => setTaxId(e.target.value)}
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

interface EditCompanyDialogProps {
  company: PartyDTO | null;
  open: boolean;
  onClose: () => void;
}

function EditCompanyDialog({ company, open, onClose }: EditCompanyDialogProps) {
  const qc = useQueryClient();
  const [displayName, setDisplayName] = useState(company?.displayName ?? '');
  const [legalName, setLegalName] = useState(company?.legalName ?? '');
  const [email, setEmail] = useState(company?.primaryEmail ?? '');
  const [phone, setPhone] = useState(company?.primaryPhone ?? '');
  const [taxId, setTaxId] = useState(company?.taxId ?? '');
  const [notes, setNotes] = useState(company?.notes ?? '');
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setDisplayName(company?.displayName ?? '');
    setLegalName(company?.legalName ?? '');
    setEmail(company?.primaryEmail ?? '');
    setPhone(company?.primaryPhone ?? '');
    setTaxId(company?.taxId ?? '');
    setNotes(company?.notes ?? '');
    setError(null);
  }, [company, open]);

  const mutation = useMutation<PartyDTO, Error, PartyUpdate>({
    mutationFn: (body) => {
      if (!company) return Promise.reject(new Error('Empresa no disponible'));
      return Parties.update(company.partyId, body);
    },
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['parties'] });
      onClose();
    },
    onError: (err) => setError(err.message),
  });

  const handleSave = () => {
    if (!company) return;
    mutation.mutate({
      uDisplayName: displayName.trim() || company.displayName,
      uLegalName: legalName.trim() || null,
      uPrimaryEmail: email.trim() || null,
      uPrimaryPhone: phone.trim() || null,
      uTaxId: taxId.trim() || null,
      uNotes: notes.trim() || null,
    });
  };

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Editar empresa</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField label="Nombre comercial" value={displayName} onChange={(e) => setDisplayName(e.target.value)} />
          <TextField label="Razón social" value={legalName} onChange={(e) => setLegalName(e.target.value)} />
          <TextField label="Correo" type="email" value={email} onChange={(e) => setEmail(e.target.value)} />
          <TextField label="Teléfono" value={phone} onChange={(e) => setPhone(e.target.value)} />
          <TextField label="RUC / Tax ID" value={taxId} onChange={(e) => setTaxId(e.target.value)} />
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
        <Button onClick={handleSave} variant="contained" disabled={mutation.isPending}>
          Guardar
        </Button>
      </DialogActions>
    </Dialog>
  );
}

export default function CompaniesPage() {
  const [search, setSearch] = useState('');
  const [createOpen, setCreateOpen] = useState(false);
  const [editOpen, setEditOpen] = useState(false);
  const [selected, setSelected] = useState<PartyDTO | null>(null);

  const { data, isLoading, isError, error } = useQuery<PartyDTO[], Error>({
    queryKey: ['parties'],
    queryFn: () => Parties.list(),
  });

  const companies = useMemo(() => {
    const term = search.trim().toLowerCase();
    return (data ?? [])
      .filter((p) => p.isOrg)
      .filter((p) => {
        if (!term) return true;
        return (
          p.displayName.toLowerCase().includes(term) ||
          (p.legalName?.toLowerCase().includes(term) ?? false) ||
          (p.primaryEmail?.toLowerCase().includes(term) ?? false) ||
          (p.taxId?.toLowerCase().includes(term) ?? false)
        );
      });
  }, [data, search]);

  return (
    <Stack gap={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems={{ xs: 'flex-start', md: 'center' }} gap={2}>
        <Stack spacing={0.5}>
          <Typography variant="h4" fontWeight={800}>CRM / Empresas</Typography>
          <Typography variant="body1" color="text.secondary">
            Gestiona empresas, datos fiscales y contactos principales. Usa la búsqueda para filtrar por nombre, correo o RUC.
          </Typography>
        </Stack>
        <Button variant="contained" startIcon={<AddBusinessIcon />} onClick={() => setCreateOpen(true)}>
          Nueva empresa
        </Button>
      </Stack>

      <Paper sx={{ p: 2.5, borderRadius: 3, boxShadow: '0 10px 30px rgba(15,23,42,0.12)' }}>
        <Stack direction={{ xs: 'column', md: 'row' }} gap={2} alignItems={{ xs: 'stretch', md: 'center' }}>
          <TextField
            placeholder="Buscar por nombre, correo o RUC"
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
        </Stack>
      </Paper>

      <Paper sx={{ p: 2, borderRadius: 3, boxShadow: '0 8px 24px rgba(15,23,42,0.08)' }}>
        {isError && <Alert severity="error">{error?.message ?? 'No se pudieron cargar las empresas'}</Alert>}
        {isLoading ? (
          <Typography>Cargando...</Typography>
        ) : companies.length === 0 ? (
          <Typography color="text.secondary">No hay empresas aún.</Typography>
        ) : (
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>Empresa</TableCell>
                <TableCell>Correo</TableCell>
                <TableCell>Teléfono</TableCell>
                <TableCell>RUC / Tax ID</TableCell>
                <TableCell>Notas</TableCell>
                <TableCell align="right">Acciones</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {companies.map((c) => (
                <TableRow key={c.partyId} hover>
                  <TableCell>
                    <Stack spacing={0.5}>
                      <Typography fontWeight={700}>{c.displayName}</Typography>
                      {c.legalName && <Typography variant="body2" color="text.secondary">{c.legalName}</Typography>}
                      {c.hasUserAccount && <Chip label="Cuenta de usuario" size="small" color="primary" />}
                    </Stack>
                  </TableCell>
                  <TableCell>{c.primaryEmail ?? '—'}</TableCell>
                  <TableCell>{c.primaryPhone ?? '—'}</TableCell>
                  <TableCell>{c.taxId ?? '—'}</TableCell>
                  <TableCell>{c.notes ?? '—'}</TableCell>
                  <TableCell align="right">
                    <Button
                      size="small"
                      startIcon={<EditIcon />}
                      onClick={() => {
                        setSelected(c);
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

      <CreateCompanyDialog open={createOpen} onClose={() => setCreateOpen(false)} />
      <EditCompanyDialog company={selected} open={editOpen} onClose={() => setEditOpen(false)} />
    </Stack>
  );
}
