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
import BusinessIcon from '@mui/icons-material/Business';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import type { PartyDTO, PartyCreate, PartyUpdate } from '../api/types';
import { Parties } from '../api/parties';
import LazyPaginatedList from '../components/LazyPaginatedList';
import PartyRelatedPopover from '../components/PartyRelatedPopover';
import PageShell, { EmptyState } from '../components/PageShell';

const COMPANIES_INITIAL_ROWS_PER_PAGE: number = 5 * 5;

interface CompaniesTableProps {
  companies: readonly PartyDTO[];
  onOpenRelated: (anchor: HTMLElement, company: PartyDTO) => void;
  onEdit: (company: PartyDTO) => void;
}

function CompanyRow({ company, onOpenRelated, onEdit }: { company: PartyDTO; onOpenRelated: CompaniesTableProps['onOpenRelated']; onEdit: CompaniesTableProps['onEdit'] }) {
  return (
    <TableRow hover>
      <TableCell>
        <Stack spacing={0.5}>
          <Button
            variant="text"
            tabIndex={0}
            onClick={(event) => {
              event.currentTarget.focus();
              onOpenRelated(event.currentTarget, company);
            }}
            sx={{ p: 0, minWidth: 0, textTransform: 'none', justifyContent: 'flex-start', alignSelf: 'flex-start' }}
          >
            <Typography fontWeight={700} sx={{ textDecoration: 'underline', textUnderlineOffset: 3 }}>
              {company.displayName}
            </Typography>
          </Button>
          {company.legalName && <Typography variant="body2" color="text.secondary">{company.legalName}</Typography>}
          {company.hasUserAccount && <Chip label="Cuenta de usuario" size="small" color="primary" />}
        </Stack>
      </TableCell>
      <TableCell>{company.primaryEmail ?? '—'}</TableCell>
      <TableCell>{company.primaryPhone ?? '—'}</TableCell>
      <TableCell>{company.taxId ?? '—'}</TableCell>
      <TableCell>{company.notes ?? '—'}</TableCell>
      <TableCell align="right">
        <Button
          size="small"
          startIcon={<EditIcon />}
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            onEdit(company);
          }}
        >
          Editar
        </Button>
      </TableCell>
    </TableRow>
  );
}

function CompaniesTable({ companies, onOpenRelated, onEdit }: CompaniesTableProps) {
  return (
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
        {companies.map((company) => (
          <CompanyRow
            key={company.partyId}
            company={company}
            onOpenRelated={onOpenRelated}
            onEdit={onEdit}
          />
        ))}
      </TableBody>
    </Table>
  );
}

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
  const [error, setError] = useState(null as string | null);

  useEffect(() => {
    if (!open) {
      setDisplayName('');
      setLegalName('');
      setEmail('');
      setTaxId('');
      setError(null);
    }
  }, [open]);

  const mutation = useMutation({
    mutationFn: (body: PartyCreate) => Parties.create(body),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['parties'] });
      onClose();
    },
    onError: (err) => setError(err.message),
  });

  const submitCompany = () => {
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
        <Button
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            onClose();
          }}
        >
          Cancelar
        </Button>
        <Button
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            submitCompany();
          }}
          variant="contained"
          disabled={mutation.isPending}
        >
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
  const [error, setError] = useState(null as string | null);

  useEffect(() => {
    setDisplayName(company?.displayName ?? '');
    setLegalName(company?.legalName ?? '');
    setEmail(company?.primaryEmail ?? '');
    setPhone(company?.primaryPhone ?? '');
    setTaxId(company?.taxId ?? '');
    setNotes(company?.notes ?? '');
    setError(null);
  }, [company, open]);

  const mutation = useMutation({
    mutationFn: (body: PartyUpdate) => {
      if (!company) return Promise.reject(new Error('Empresa no disponible'));
      return Parties.update(company.partyId, body);
    },
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['parties'] });
      onClose();
    },
    onError: (err) => setError(err.message),
  });

  const saveCompany = () => {
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
        <Button
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            onClose();
          }}
        >
          Cancelar
        </Button>
        <Button
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            saveCompany();
          }}
          variant="contained"
          disabled={mutation.isPending}
        >
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
  const [selected, setSelected] = useState(null as PartyDTO | null);
  const [relatedParty, setRelatedParty] = useState(null as PartyDTO | null);
  const [relatedAnchor, setRelatedAnchor] = useState(null as HTMLElement | null);

  const { data, isLoading, isError, error } = useQuery({
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
    <>
    <PageShell
      title="Empresas"
      subtitle="Gestiona empresas, datos fiscales y contactos principales."
      actions={(
        <Button
          variant="contained"
          startIcon={<AddBusinessIcon />}
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            setCreateOpen(true);
          }}
        >
          Nueva empresa
        </Button>
      )}
    >
    <Stack gap={3}>
      <TextField
        label="Buscar empresas"
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
          endAdornment: search ? (
            <InputAdornment position="end">
              <Button
                size="small"
                tabIndex={0}
                onClick={(event) => {
                  event.currentTarget.focus();
                  setSearch('');
                }}
              >
                Limpiar
              </Button>
            </InputAdornment>
          ) : null,
        }}
      />

      <Paper sx={{ p: 2 }}>
        {isError && <Alert severity="error">{error?.message ?? 'No se pudieron cargar las empresas'}</Alert>}
        {isLoading ? (
          <Typography>Cargando empresas…</Typography>
        ) : companies.length === 0 ? (
          <EmptyState
            icon={<BusinessIcon />}
            title="Sin empresas"
            description="Aún no hay empresas registradas. Crea la primera para empezar."
            actionLabel="Nueva empresa"
            actionOnClick={() => setCreateOpen(true)}
          />
        ) : (
          <LazyPaginatedList
            items={companies}
            pagination={{ itemLabel: 'empresas', initialRowsPerPage: COMPANIES_INITIAL_ROWS_PER_PAGE, resetKey: search.trim() }}
            renderItems={(visibleCompanies) => (
              <CompaniesTable
                companies={visibleCompanies}
                onOpenRelated={(anchor, company) => {
                  setRelatedParty(company);
                  setRelatedAnchor(anchor);
                }}
                onEdit={(company) => {
                  setSelected(company);
                  setEditOpen(true);
                }}
              />
            )}
          />
        )}
      </Paper>
    </Stack>
    </PageShell>

      <CreateCompanyDialog open={createOpen} onClose={() => setCreateOpen(false)} />
      <EditCompanyDialog company={selected} open={editOpen} onClose={() => setEditOpen(false)} />
      <PartyRelatedPopover
        party={relatedParty}
        anchorEl={relatedAnchor}
        onClose={() => {
          setRelatedParty(null);
          setRelatedAnchor(null);
        }}
      />
    </>
  );
}
