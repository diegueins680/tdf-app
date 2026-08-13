import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControlLabel,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { Catalogs, type CatalogDraft, type CatalogRevision } from '../api/catalogs';
import { RadioAPI } from '../api/radio';

const CATALOG_CODE = 'radio-auto-stop-options';
const POLICY_QUERY_KEY = ['radio-auto-stop-options', 'admin'] as const;
const ITEMS_QUERY_KEY = ['catalog', CATALOG_CODE, 'admin-items'] as const;
const REVISIONS_QUERY_KEY = ['catalog', CATALOG_CODE, 'revisions'] as const;

export interface RadioAutoStopForm {
  entityId?: string;
  baseVersion?: number;
  code: string;
  nameEs: string;
  nameEn: string;
  descriptionEs: string;
  descriptionEn: string;
  durationMinutes: string;
  defaultForBroadcast: boolean;
  sortOrder: number;
  reason: string;
}

const emptyForm = (): RadioAutoStopForm => ({
  code: '',
  nameEs: '',
  nameEn: '',
  descriptionEs: '',
  descriptionEn: '',
  durationMinutes: '',
  defaultForBroadcast: false,
  sortOrder: 0,
  reason: '',
});

const optionalText = (value: string): string | undefined => value.trim() || undefined;

const correlationId = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `radio-auto-stop:${crypto.randomUUID()}`;
  }
  return `radio-auto-stop:${Date.now()}`;
};

export const buildRadioAutoStopDraft = (form: RadioAutoStopForm): CatalogDraft => ({
  entityId: form.entityId,
  baseVersion: form.baseVersion,
  code: form.code.trim(),
  nameEs: form.nameEs.trim(),
  nameEn: form.nameEn.trim(),
  descriptionEs: optionalText(form.descriptionEs),
  descriptionEn: optionalText(form.descriptionEn),
  searchAliasesEs: [],
  searchAliasesEn: [],
  sortOrder: form.sortOrder,
  radioAutoStop: {
    durationMinutes: Number(form.durationMinutes),
    defaultForBroadcast: form.defaultForBroadcast,
  },
  reason: form.reason.trim(),
  sourcePlatform: 'web-admin',
  correlationId: correlationId(),
});

const revisionColor = (state: string): 'default' | 'info' | 'success' | 'error' | 'warning' => {
  switch (state) {
    case 'review': return 'info';
    case 'published':
    case 'approved': return 'success';
    case 'rejected': return 'error';
    default: return 'warning';
  }
};

export default function RadioAutoStopCatalogPage() {
  const queryClient = useQueryClient();
  const [dialogOpen, setDialogOpen] = useState(false);
  const [form, setForm] = useState<RadioAutoStopForm>(emptyForm);
  const [reviewNotes, setReviewNotes] = useState<Record<string, string>>({});

  const policyQuery = useQuery({
    queryKey: POLICY_QUERY_KEY,
    queryFn: () => RadioAPI.listAutoStopOptions('es'),
  });
  const itemsQuery = useQuery({
    queryKey: ITEMS_QUERY_KEY,
    queryFn: () => Catalogs.listItems(CATALOG_CODE, { locale: 'es', includeInactive: true, pageSize: 500 }),
  });
  const revisionsQuery = useQuery<CatalogRevision[]>({
    queryKey: REVISIONS_QUERY_KEY,
    queryFn: () => Catalogs.listRevisions(CATALOG_CODE, 1, 100),
  });

  const itemById = useMemo(
    () => new Map((itemsQuery.data?.items ?? []).map((item) => [item.id, item])),
    [itemsQuery.data?.items],
  );

  const refresh = async () => {
    await Promise.all([
      queryClient.invalidateQueries({ queryKey: POLICY_QUERY_KEY }),
      queryClient.invalidateQueries({ queryKey: ITEMS_QUERY_KEY }),
      queryClient.invalidateQueries({ queryKey: REVISIONS_QUERY_KEY }),
    ]);
  };

  const createRevision = useMutation({
    mutationFn: (draft: CatalogDraft) => Catalogs.createRevision(CATALOG_CODE, draft),
    onSuccess: async () => {
      setDialogOpen(false);
      setForm(emptyForm());
      await refresh();
    },
  });
  const submitRevision = useMutation({ mutationFn: Catalogs.submitRevision, onSuccess: refresh });
  const approveRevision = useMutation({
    mutationFn: ({ id, notes }: { id: string; notes: string }) => Catalogs.approveRevision(id, {
      notes,
      emergencyOverride: false,
    }),
    onSuccess: refresh,
  });
  const rejectRevision = useMutation({
    mutationFn: ({ id, notes }: { id: string; notes: string }) => Catalogs.rejectRevision(id, {
      notes,
      emergencyOverride: false,
    }),
    onSuccess: refresh,
  });

  const openCreate = () => {
    const next = emptyForm();
    next.sortOrder = (itemsQuery.data?.items.length ?? 0) * 10;
    setForm(next);
    setDialogOpen(true);
  };

  const openEdit = (optionId: string) => {
    const option = policyQuery.data?.options.find((candidate) => candidate.id === optionId);
    const item = itemById.get(optionId);
    if (!option || !item) return;
    setForm({
      entityId: option.id,
      baseVersion: item.version,
      code: item.code,
      nameEs: item.nameEs,
      nameEn: item.nameEn,
      descriptionEs: item.descriptionEs ?? '',
      descriptionEn: item.descriptionEn ?? '',
      durationMinutes: String(option.durationMinutes),
      defaultForBroadcast: option.defaultForBroadcast,
      sortOrder: item.sortOrder,
      reason: '',
    });
    setDialogOpen(true);
  };

  const parsedDuration = Number(form.durationMinutes);
  const editingPublishedDefault = Boolean(
    form.entityId
      && policyQuery.data?.options.some((option) => option.id === form.entityId && option.defaultForBroadcast),
  );
  const formValid = Boolean(
    form.code.trim()
      && form.nameEs.trim()
      && form.nameEn.trim()
      && Number.isSafeInteger(parsedDuration)
      && parsedDuration >= 0
      && parsedDuration <= 1440
      && Number.isSafeInteger(form.sortOrder)
      && form.sortOrder >= 0
      && form.reason.trim(),
  );

  const queryFailed = policyQuery.isError || itemsQuery.isError || revisionsQuery.isError;

  return (
    <Box>
      <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1.5} mb={2}>
        <Stack spacing={0.75}>
          <Typography variant="h5" fontWeight={800}>Catálogo de auto-stop de Radio</Typography>
          <Typography variant="body2" color="text.secondary">
            Duraciones bilingües, orden y valor predeterminado para transmisiones desde el navegador.
          </Typography>
        </Stack>
        <Stack direction="row" spacing={1}>
          <Button component={RouterLink} to="/configuracion/catalogos">Todos los catálogos</Button>
          <Button variant="contained" onClick={openCreate} disabled={queryFailed}>Crear borrador</Button>
        </Stack>
      </Stack>

      {queryFailed && (
        <Alert severity="error" sx={{ mb: 2 }}>
          No se pudo cargar la política canónica. La administración queda bloqueada para evitar cambios parciales.
        </Alert>
      )}

      <Card sx={{ mb: 2 }}>
        <CardContent>
          <Typography variant="h6" mb={1}>Opciones publicadas</Typography>
          {policyQuery.isLoading || itemsQuery.isLoading ? (
            <Stack direction="row" spacing={1} alignItems="center">
              <CircularProgress size={18} />
              <Typography variant="body2">Cargando política…</Typography>
            </Stack>
          ) : (policyQuery.data?.options.length ?? 0) === 0 ? (
            <Alert severity="warning">No hay opciones publicadas.</Alert>
          ) : (
            <TableContainer sx={{ overflowX: 'auto' }}>
              <Table size="small" aria-label="Opciones de auto-stop publicadas">
                <TableHead>
                  <TableRow>
                    <TableCell>Opción</TableCell>
                    <TableCell>Duración</TableCell>
                    <TableCell>Orden</TableCell>
                    <TableCell>Política</TableCell>
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {policyQuery.data?.options.map((option) => {
                    const item = itemById.get(option.id);
                    return (
                      <TableRow key={option.id} hover>
                        <TableCell>
                          <Typography variant="body2">{option.label}</Typography>
                          <Typography variant="caption" color="text.secondary">{option.code} · {option.id}</Typography>
                        </TableCell>
                        <TableCell>{option.durationMinutes === 0 ? 'Sin límite' : `${option.durationMinutes} min`}</TableCell>
                        <TableCell>{item?.sortOrder ?? '—'}</TableCell>
                        <TableCell>
                          <Chip
                            size="small"
                            label={option.defaultForBroadcast ? 'Predeterminada' : 'Disponible'}
                            color={option.defaultForBroadcast ? 'success' : 'default'}
                          />
                        </TableCell>
                        <TableCell align="right">
                          <Button size="small" onClick={() => openEdit(option.id)} disabled={!item}>
                            Crear revisión
                          </Button>
                        </TableCell>
                      </TableRow>
                    );
                  })}
                </TableBody>
              </Table>
            </TableContainer>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardContent>
          <Typography variant="h6" mb={1}>Revisiones</Typography>
          {revisionsQuery.isLoading ? <CircularProgress size={18} /> : (revisionsQuery.data?.length ?? 0) === 0 ? (
            <Typography variant="body2" color="text.secondary">No hay revisiones pendientes ni recientes.</Typography>
          ) : (
            <TableContainer sx={{ overflowX: 'auto' }}>
              <Table size="small" aria-label="Revisiones del catálogo de auto-stop">
                <TableHead>
                  <TableRow>
                    <TableCell>Opción</TableCell>
                    <TableCell>Estado</TableCell>
                    <TableCell>Nota</TableCell>
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {revisionsQuery.data?.map((revision) => {
                    const notes = reviewNotes[revision.id] ?? '';
                    return (
                      <TableRow key={revision.id}>
                        <TableCell>
                          <Typography variant="body2">{revision.draft.nameEs}</Typography>
                          <Typography variant="caption" color="text.secondary">
                            {revision.draft.radioAutoStop?.durationMinutes ?? '—'} min · {revision.id}
                          </Typography>
                        </TableCell>
                        <TableCell><Chip size="small" label={revision.workflowState} color={revisionColor(revision.workflowState)} /></TableCell>
                        <TableCell>
                          <TextField
                            size="small"
                            label="Nota o motivo"
                            value={notes}
                            onChange={(event) => setReviewNotes((current) => ({ ...current, [revision.id]: event.target.value }))}
                            inputProps={{ 'aria-label': `Nota para ${revision.draft.nameEs}` }}
                          />
                        </TableCell>
                        <TableCell align="right">
                          <Stack direction="row" spacing={0.5} justifyContent="flex-end">
                            {(revision.workflowState === 'draft' || revision.workflowState === 'rejected') && (
                              <Button size="small" onClick={() => submitRevision.mutate(revision.id)}>Enviar</Button>
                            )}
                            {revision.workflowState === 'review' && (
                              <Button size="small" color="success" disabled={!notes.trim()} onClick={() => approveRevision.mutate({ id: revision.id, notes })}>Aprobar</Button>
                            )}
                            {revision.workflowState === 'review' && (
                              <Button size="small" color="error" disabled={!notes.trim()} onClick={() => rejectRevision.mutate({ id: revision.id, notes })}>Rechazar</Button>
                            )}
                          </Stack>
                        </TableCell>
                      </TableRow>
                    );
                  })}
                </TableBody>
              </Table>
            </TableContainer>
          )}
        </CardContent>
      </Card>

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} fullWidth maxWidth="md">
        <DialogTitle>{form.entityId ? 'Crear revisión de opción' : 'Crear opción en borrador'}</DialogTitle>
        <DialogContent>
          <Stack spacing={2} mt={1}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <TextField required fullWidth label="Código interno" value={form.code} onChange={(event) => setForm({ ...form, code: event.target.value })} />
              <TextField required fullWidth type="number" label="Duración (minutos)" value={form.durationMinutes} onChange={(event) => setForm({ ...form, durationMinutes: event.target.value })} inputProps={{ min: 0, max: 1440, step: 1 }} />
              <TextField required fullWidth type="number" label="Orden manual" value={form.sortOrder} onChange={(event) => setForm({ ...form, sortOrder: Number(event.target.value) })} inputProps={{ min: 0, step: 1 }} />
            </Stack>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <TextField required fullWidth label="Nombre en español" value={form.nameEs} onChange={(event) => setForm({ ...form, nameEs: event.target.value })} />
              <TextField required fullWidth label="Nombre en inglés" value={form.nameEn} onChange={(event) => setForm({ ...form, nameEn: event.target.value })} />
            </Stack>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
              <TextField fullWidth multiline minRows={2} label="Descripción en español" value={form.descriptionEs} onChange={(event) => setForm({ ...form, descriptionEs: event.target.value })} />
              <TextField fullWidth multiline minRows={2} label="Descripción en inglés" value={form.descriptionEn} onChange={(event) => setForm({ ...form, descriptionEn: event.target.value })} />
            </Stack>
            <FormControlLabel
              control={<Checkbox checked={form.defaultForBroadcast} disabled={editingPublishedDefault} onChange={(event) => setForm({ ...form, defaultForBroadcast: event.target.checked })} />}
              label="Usar como valor predeterminado global para nuevas transmisiones"
            />
            {editingPublishedDefault && (
              <Typography variant="caption" color="text.secondary">
                Para reemplazar el predeterminado, crea una revisión de otra opción y márcala como predeterminada.
              </Typography>
            )}
            <TextField required multiline minRows={2} label="Motivo del cambio" value={form.reason} onChange={(event) => setForm({ ...form, reason: event.target.value })} />
            {form.entityId && itemById.get(form.entityId)?.active === false && (
              <Alert severity="warning">Esta opción está inactiva. La revisión no la ofrecerá hasta que se restaure explícitamente.</Alert>
            )}
            {createRevision.isError && (
              <Alert severity="error">No se pudo crear el borrador. Revisa permisos, versión y unicidad de duración.</Alert>
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDialogOpen(false)}>Cancelar</Button>
          <Button variant="contained" disabled={!formValid || createRevision.isPending} onClick={() => createRevision.mutate(buildRadioAutoStopDraft(form))}>
            Guardar borrador
          </Button>
        </DialogActions>
      </Dialog>
    </Box>
  );
}
