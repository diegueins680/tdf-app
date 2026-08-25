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

type FeedbackCatalogCode = 'feedback-categories' | 'feedback-severities';

interface FeedbackCatalogConfig {
  code: FeedbackCatalogCode;
  defaultScopeKind: 'feedback-category' | 'feedback-severity';
}

const CATALOGS: readonly FeedbackCatalogConfig[] = [
  {
    code: 'feedback-categories',
    defaultScopeKind: 'feedback-category',
  },
  {
    code: 'feedback-severities',
    defaultScopeKind: 'feedback-severity',
  },
] as const;

export interface FeedbackCatalogForm {
  entityId?: string;
  baseVersion?: number;
  code: string;
  nameEs: string;
  nameEn: string;
  descriptionEs: string;
  descriptionEn: string;
  sortOrder: number;
  globalDefault: boolean;
  reason: string;
}

const emptyForm = (sortOrder = 0): FeedbackCatalogForm => ({
  code: '',
  nameEs: '',
  nameEn: '',
  descriptionEs: '',
  descriptionEn: '',
  sortOrder,
  globalDefault: false,
  reason: '',
});

const optionalText = (value: string): string | undefined => value.trim() || undefined;

const correlationId = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `feedback-catalog:${crypto.randomUUID()}`;
  }
  return `feedback-catalog:${Date.now()}`;
};

export const buildFeedbackCatalogDraft = (form: FeedbackCatalogForm): CatalogDraft => ({
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
  globalDefault: form.globalDefault,
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

export default function FeedbackCatalogPage() {
  const queryClient = useQueryClient();
  const [catalogCode, setCatalogCode] = useState<FeedbackCatalogCode>('feedback-categories');
  const [search, setSearch] = useState('');
  const [form, setForm] = useState<FeedbackCatalogForm | null>(null);
  const [reviewNotes, setReviewNotes] = useState<Record<string, string>>({});
  const config: FeedbackCatalogConfig = CATALOGS.find(
    (candidate) => candidate.code === catalogCode,
  ) ?? CATALOGS[0]!;

  const definitionsQuery = useQuery({
    queryKey: ['catalog', 'definitions', 'es'],
    queryFn: () => Catalogs.listDefinitions('es'),
  });
  const definitionByCode = useMemo(
    () => new Map((definitionsQuery.data ?? []).map((definition) => [definition.code, definition])),
    [definitionsQuery.data],
  );
  const selectedDefinition = definitionByCode.get(catalogCode);
  const itemsQuery = useQuery({
    queryKey: ['catalog', catalogCode, 'admin-items', search],
    queryFn: () => Catalogs.listItems(catalogCode, {
      locale: 'es',
      q: search.trim() || undefined,
      includeInactive: true,
      page: 1,
      pageSize: 100,
    }),
  });
  const revisionsQuery = useQuery<CatalogRevision[]>({
    queryKey: ['catalog', catalogCode, 'revisions'],
    queryFn: () => Catalogs.listRevisions(catalogCode, 1, 100),
  });
  const items = useMemo(() => itemsQuery.data?.items ?? [], [itemsQuery.data?.items]);
  const itemById = useMemo(() => new Map(items.map((item) => [item.id, item])), [items]);
  const defaultId = itemsQuery.data?.defaults.find(
    (entry) => entry.scopeKind === config.defaultScopeKind
      && entry.scopeId === 'global'
      && !entry.localeId,
  )?.entityId;

  const refresh = async () => {
    await Promise.all([
      queryClient.invalidateQueries({ queryKey: ['catalog', catalogCode] }),
      queryClient.invalidateQueries({ queryKey: ['catalogs', catalogCode] }),
    ]);
  };
  const createRevision = useMutation({
    mutationFn: (draft: CatalogDraft) => Catalogs.createRevision(catalogCode, draft),
    onSuccess: async () => {
      setForm(null);
      await refresh();
    },
  });
  const submitRevision = useMutation({ mutationFn: Catalogs.submitRevision, onSuccess: refresh });
  const approveRevision = useMutation({
    mutationFn: ({ id, notes }: { id: string; notes: string }) => Catalogs.approveRevision(id, {
      notes: notes.trim(),
      emergencyOverride: false,
    }),
    onSuccess: refresh,
  });
  const rejectRevision = useMutation({
    mutationFn: ({ id, notes }: { id: string; notes: string }) => Catalogs.rejectRevision(id, {
      notes: notes.trim(),
      emergencyOverride: false,
    }),
    onSuccess: refresh,
  });

  const selectCatalog = (next: FeedbackCatalogCode) => {
    setCatalogCode(next);
    setSearch('');
    setForm(null);
  };
  const openCreate = () => setForm(emptyForm(itemsQuery.data?.total ?? 0));
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
      globalDefault: item.id === defaultId,
      reason: '',
    });
  };
  const editingDefault = Boolean(form?.entityId && form.entityId === defaultId);
  const formValid = Boolean(
    form?.code.trim()
      && form.nameEs.trim()
      && form.nameEn.trim()
      && form.reason.trim()
      && Number.isSafeInteger(form.sortOrder)
      && form.sortOrder >= 0,
  );
  const queryFailed = definitionsQuery.isError || itemsQuery.isError || revisionsQuery.isError;

  return (
    <PageShell
      title="Catálogos de feedback"
      subtitle="Opciones bilingües y defaults publicados mediante revisión; los formularios consumen UUIDs canónicos."
    >
      <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1.5} mb={2}>
        <Button component={RouterLink} to="/configuracion/catalogos">Volver a Catálogos</Button>
        <Stack direction="row" spacing={1} role="group" aria-label="Tipo de catálogo de feedback">
          {CATALOGS.map((candidate) => (
            <Button
              key={candidate.code}
              variant={catalogCode === candidate.code ? 'contained' : 'outlined'}
              onClick={() => selectCatalog(candidate.code)}
              aria-pressed={catalogCode === candidate.code}
            >
              {definitionByCode.get(candidate.code)?.name ?? 'Catálogo'}
            </Button>
          ))}
        </Stack>
      </Stack>

      {queryFailed && (
        <Alert severity="error" sx={{ mb: 2 }}>
          No se pudo validar el catálogo canónico. La edición queda bloqueada para evitar cambios parciales.
        </Alert>
      )}

      <Card sx={{ mb: 2 }}>
        <CardContent>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5} justifyContent="space-between" mb={2}>
            <TextField
              size="small"
              label="Buscar elementos"
              value={search}
              onChange={(event) => setSearch(event.target.value)}
              inputProps={{ 'aria-label': `Buscar en ${selectedDefinition?.name ?? 'catálogo de feedback'}` }}
            />
            <Button variant="contained" disabled={queryFailed} onClick={openCreate}>
              Crear elemento
            </Button>
          </Stack>
          {itemsQuery.isLoading ? (
            <Stack direction="row" spacing={1} alignItems="center">
              <CircularProgress size={18} />
              <Typography variant="body2">Cargando catálogo…</Typography>
            </Stack>
          ) : !items.length ? (
            <Alert severity="info">No hay elementos que coincidan con la búsqueda.</Alert>
          ) : (
            <TableContainer sx={{ overflowX: 'auto' }}>
              <Table size="small" aria-label={selectedDefinition?.name ?? 'Elementos de feedback'}>
                <TableHead>
                  <TableRow>
                    <TableCell>Elemento</TableCell>
                    <TableCell>Orden</TableCell>
                    <TableCell>Estado</TableCell>
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {items.map((item) => (
                    <TableRow key={item.id} hover>
                      <TableCell>
                        <Typography variant="body2">{item.nameEs} / {item.nameEn}</Typography>
                        <Typography variant="caption" color="text.secondary">{item.code} · {item.id}</Typography>
                      </TableCell>
                      <TableCell>{item.sortOrder}</TableCell>
                      <TableCell>
                        <Stack direction="row" spacing={0.5} flexWrap="wrap">
                          <Chip size="small" label={item.active ? 'Activa' : 'Inactiva'} color={item.active ? 'success' : 'default'} />
                          {item.id === defaultId && <Chip size="small" label="Predeterminada" color="info" />}
                        </Stack>
                      </TableCell>
                      <TableCell align="right">
                        <Button size="small" disabled={queryFailed} onClick={() => openRevision(item.id)}>
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
              <Table size="small" aria-label={`Revisiones de ${selectedDefinition?.name ?? 'feedback'}`}>
                <TableHead>
                  <TableRow>
                    <TableCell>Elemento</TableCell>
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
        <DialogTitle>{form?.entityId ? 'Crear revisión del elemento' : 'Crear elemento'}</DialogTitle>
        <DialogContent>
          {form && (
            <Stack spacing={2} mt={1}>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <TextField required fullWidth label="Código interno" value={form.code} onChange={(event) => setForm({ ...form, code: event.target.value })} />
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
                control={<Checkbox checked={form.globalDefault} disabled={editingDefault} onChange={(event) => setForm({ ...form, globalDefault: event.target.checked })} />}
                label="Usar como valor predeterminado global"
              />
              {editingDefault && (
                <Typography variant="caption" color="text.secondary">
                  Publica otro elemento como predeterminado antes de sustituir éste.
                </Typography>
              )}
              <TextField required multiline minRows={2} label="Motivo del cambio" value={form.reason} onChange={(event) => setForm({ ...form, reason: event.target.value })} />
              {createRevision.isError && (
                <Alert severity="error">No se pudo crear el borrador. Revisa permisos, versión y datos bilingües.</Alert>
              )}
            </Stack>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setForm(null)}>Cancelar</Button>
          <Button
            variant="contained"
            disabled={!form || !formValid || createRevision.isPending}
            onClick={() => form && createRevision.mutate(buildFeedbackCatalogDraft(form))}
          >
            Guardar borrador
          </Button>
        </DialogActions>
      </Dialog>
    </PageShell>
  );
}
