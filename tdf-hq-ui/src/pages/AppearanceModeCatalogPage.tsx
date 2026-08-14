import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
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
import PageShell from '../components/PageShell';

const CATALOG_CODE = 'appearance-modes';
const PUBLIC_QUERY_KEY = ['catalogs', CATALOG_CODE, 'published'] as const;
const ITEMS_QUERY_KEY = ['catalog', CATALOG_CODE, 'admin-items'] as const;
const REVISIONS_QUERY_KEY = ['catalog', CATALOG_CODE, 'revisions'] as const;

export interface AppearanceModeForm {
  entityId: string;
  baseVersion: number;
  code: string;
  nameEs: string;
  nameEn: string;
  descriptionEs: string;
  descriptionEn: string;
  sortOrder: number;
  defaultForApplication: boolean;
  reason: string;
}

const optionalText = (value: string): string | undefined => value.trim() || undefined;

const correlationId = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `appearance-mode:${crypto.randomUUID()}`;
  }
  return `appearance-mode:${Date.now()}`;
};

export const buildAppearanceModeDraft = (form: AppearanceModeForm): CatalogDraft => ({
  entityId: form.entityId,
  baseVersion: form.baseVersion,
  code: form.code,
  nameEs: form.nameEs.trim(),
  nameEn: form.nameEn.trim(),
  descriptionEs: optionalText(form.descriptionEs),
  descriptionEn: optionalText(form.descriptionEn),
  searchAliasesEs: [],
  searchAliasesEn: [],
  sortOrder: form.sortOrder,
  appearanceMode: { defaultForApplication: form.defaultForApplication },
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

export default function AppearanceModeCatalogPage() {
  const queryClient = useQueryClient();
  const [form, setForm] = useState<AppearanceModeForm | null>(null);
  const [reviewNotes, setReviewNotes] = useState<Record<string, string>>({});

  const publicQuery = useQuery({
    queryKey: PUBLIC_QUERY_KEY,
    queryFn: () => Catalogs.listPublicBatch([CATALOG_CODE], { locale: 'es', pageSize: 50 }),
  });
  const itemsQuery = useQuery({
    queryKey: ITEMS_QUERY_KEY,
    queryFn: () => Catalogs.listItems(CATALOG_CODE, { locale: 'es', includeInactive: true, pageSize: 50 }),
  });
  const revisionsQuery = useQuery<CatalogRevision[]>({
    queryKey: REVISIONS_QUERY_KEY,
    queryFn: () => Catalogs.listRevisions(CATALOG_CODE, 1, 100),
  });
  const page = publicQuery.data?.catalogs.find((candidate) => candidate.catalog.code === CATALOG_CODE);
  const defaultId = page?.defaults.find(
    (entry) => entry.scopeKind === 'appearance-mode' && entry.scopeId === 'global' && !entry.localeId,
  )?.entityId;
  const itemById = useMemo(
    () => new Map((itemsQuery.data?.items ?? []).map((item) => [item.id, item])),
    [itemsQuery.data?.items],
  );

  const refresh = async () => {
    await Promise.all([
      queryClient.invalidateQueries({ queryKey: PUBLIC_QUERY_KEY }),
      queryClient.invalidateQueries({ queryKey: ITEMS_QUERY_KEY }),
      queryClient.invalidateQueries({ queryKey: REVISIONS_QUERY_KEY }),
      queryClient.invalidateQueries({ queryKey: ['catalogs', CATALOG_CODE] }),
    ]);
  };

  const createRevision = useMutation({
    mutationFn: (draft: CatalogDraft) => Catalogs.createRevision(CATALOG_CODE, draft),
    onSuccess: async () => {
      setForm(null);
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

  const openRevision = (id: string) => {
    const item = itemById.get(id);
    if (!item) return;
    setForm({
      entityId: item.id,
      baseVersion: item.version,
      code: item.code,
      nameEs: item.nameEs,
      nameEn: item.nameEn,
      descriptionEs: item.descriptionEs ?? '',
      descriptionEn: item.descriptionEn ?? '',
      sortOrder: item.sortOrder,
      defaultForApplication: item.id === defaultId,
      reason: '',
    });
  };

  const editingDefault = Boolean(form?.entityId && form.entityId === defaultId);
  const formValid = Boolean(
    form?.nameEs.trim()
      && form.nameEn.trim()
      && Number.isSafeInteger(form.sortOrder)
      && form.sortOrder >= 0
      && form.reason.trim(),
  );
  const queryFailed = publicQuery.isError || itemsQuery.isError || revisionsQuery.isError;

  return (
    <PageShell
      title="Catálogo de apariencia"
      subtitle="Registro cerrado de capacidades de renderizado; la base de datos gobierna etiquetas, orden, disponibilidad y default."
    >
      <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1.5} mb={2}>
        <Button component={RouterLink} to="/configuracion/catalogos">Volver a Catálogos</Button>
        <Chip label="Códigos ejecutables: system · light · dark" variant="outlined" />
      </Stack>

      {queryFailed && (
        <Alert severity="error" sx={{ mb: 2 }}>
          No se pudo validar el catálogo canónico. La edición queda bloqueada para evitar cambios parciales.
        </Alert>
      )}

      <Card sx={{ mb: 2 }}>
        <CardContent>
          <Typography variant="h6" mb={1}>Opciones publicadas</Typography>
          {publicQuery.isLoading || itemsQuery.isLoading ? (
            <Stack direction="row" spacing={1} alignItems="center">
              <CircularProgress size={18} />
              <Typography variant="body2">Cargando catálogo…</Typography>
            </Stack>
          ) : !page?.items.length ? (
            <Alert severity="warning">No hay opciones publicadas.</Alert>
          ) : (
            <TableContainer sx={{ overflowX: 'auto' }}>
              <Table size="small" aria-label="Opciones de apariencia publicadas">
                <TableHead>
                  <TableRow>
                    <TableCell>Opción</TableCell>
                    <TableCell>Orden</TableCell>
                    <TableCell>Estado</TableCell>
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {page.items.map((item) => (
                    <TableRow key={item.id} hover>
                      <TableCell>
                        <Typography variant="body2">{item.name}</Typography>
                        <Typography variant="caption" color="text.secondary">{item.code} · {item.id}</Typography>
                      </TableCell>
                      <TableCell>{item.sortOrder}</TableCell>
                      <TableCell>
                        <Chip
                          size="small"
                          label={item.id === defaultId ? 'Predeterminada' : 'Disponible'}
                          color={item.id === defaultId ? 'success' : 'default'}
                        />
                      </TableCell>
                      <TableCell align="right">
                        <Button size="small" disabled={queryFailed || !itemById.has(item.id)} onClick={() => openRevision(item.id)}>
                          Crear revisión
                        </Button>
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </TableContainer>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardContent>
          <Typography variant="h6" mb={1}>Revisiones</Typography>
          {revisionsQuery.isLoading ? <CircularProgress size={18} /> : !revisionsQuery.data?.length ? (
            <Typography variant="body2" color="text.secondary">No hay revisiones pendientes ni recientes.</Typography>
          ) : (
            <TableContainer sx={{ overflowX: 'auto' }}>
              <Table size="small" aria-label="Revisiones del catálogo de apariencia">
                <TableHead>
                  <TableRow>
                    <TableCell>Opción</TableCell>
                    <TableCell>Estado</TableCell>
                    <TableCell>Nota</TableCell>
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {revisionsQuery.data.map((revision) => {
                    const notes = reviewNotes[revision.id] ?? '';
                    return (
                      <TableRow key={revision.id}>
                        <TableCell>
                          <Typography variant="body2">{revision.draft.nameEs}</Typography>
                          <Typography variant="caption" color="text.secondary">{revision.draft.code} · {revision.id}</Typography>
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

      <Dialog open={Boolean(form)} onClose={() => setForm(null)} fullWidth maxWidth="md">
        <DialogTitle>Crear revisión de apariencia</DialogTitle>
        <DialogContent>
          {form && (
            <Stack spacing={2} mt={1}>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <TextField fullWidth disabled label="Código ejecutable" value={form.code} />
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
                control={<Checkbox checked={form.defaultForApplication} disabled={editingDefault} onChange={(event) => setForm({ ...form, defaultForApplication: event.target.checked })} />}
                label="Usar como apariencia predeterminada global"
              />
              {editingDefault && (
                <Typography variant="caption" color="text.secondary">
                  Publica otra opción como predeterminada antes de sustituir ésta.
                </Typography>
              )}
              <TextField required multiline minRows={2} label="Motivo del cambio" value={form.reason} onChange={(event) => setForm({ ...form, reason: event.target.value })} />
              {createRevision.isError && (
                <Alert severity="error">No se pudo crear el borrador. Revisa permisos, versión y código ejecutable.</Alert>
              )}
            </Stack>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setForm(null)}>Cancelar</Button>
          <Button
            variant="contained"
            disabled={!form || !formValid || createRevision.isPending}
            onClick={() => form && createRevision.mutate(buildAppearanceModeDraft(form))}
          >
            Guardar borrador
          </Button>
        </DialogActions>
      </Dialog>
    </PageShell>
  );
}
