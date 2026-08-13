import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
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
import { Catalogs, type CatalogDraft, type CatalogItem, type CatalogRevision } from '../api/catalogs';
import PageShell from '../components/PageShell';

const catalogConfiguration = (catalogCode: string | undefined) => {
  if (catalogCode === 'content-reaction-types') return {
    code: 'content-reaction-types',
    title: 'Reacciones de contenido',
    subtitle: 'Opciones persistidas para publicaciones y recuerdos del club de fans.',
    itemLabel: 'tipo de reacción',
    requiresSymbol: true,
  };
  if (catalogCode === 'creator-badge-types') return {
    code: 'creator-badge-types',
    title: 'Insignias de creadores',
    subtitle: 'Insignias bilingües persistidas que se asignan dentro de clubes de fans.',
    itemLabel: 'tipo de insignia',
    requiresSymbol: false,
  };
  return {
    code: 'reaction-types',
    title: 'Reacciones de momentos',
    subtitle: 'Opciones persistidas para momentos de eventos.',
    itemLabel: 'tipo de reacción',
    requiresSymbol: true,
  };
};

export interface ReactionTypeForm {
  entityId?: string;
  baseVersion?: number;
  code: string;
  displaySymbol: string;
  nameEs: string;
  nameEn: string;
  descriptionEs: string;
  descriptionEn: string;
  sortOrder: number;
  reason: string;
}

const emptyForm = (): ReactionTypeForm => ({
  code: '',
  displaySymbol: '',
  nameEs: '',
  nameEn: '',
  descriptionEs: '',
  descriptionEn: '',
  sortOrder: 0,
  reason: '',
});

const optionalText = (value: string): string | undefined => value.trim() || undefined;

const correlationId = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `reaction-type:${crypto.randomUUID()}`;
  }
  return `reaction-type:${Date.now()}`;
};

export const buildReactionTypeDraft = (form: ReactionTypeForm): CatalogDraft => ({
  ...(form.entityId ? { entityId: form.entityId, baseVersion: form.baseVersion } : {}),
  code: form.code.trim().toLowerCase(),
  nameEs: form.nameEs.trim(),
  nameEn: form.nameEn.trim(),
  descriptionEs: optionalText(form.descriptionEs),
  descriptionEn: optionalText(form.descriptionEn),
  searchAliasesEs: [],
  searchAliasesEn: [],
  sortOrder: form.sortOrder,
  ...(form.displaySymbol.trim() ? { displaySymbol: form.displaySymbol.trim() } : {}),
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

const itemForm = (item: CatalogItem): ReactionTypeForm => ({
  entityId: item.id,
  baseVersion: item.version,
  code: item.code,
  displaySymbol: item.displaySymbol ?? '',
  nameEs: item.nameEs,
  nameEn: item.nameEn,
  descriptionEs: item.descriptionEs ?? '',
  descriptionEn: item.descriptionEn ?? '',
  sortOrder: item.sortOrder,
  reason: '',
});

export default function ReactionTypesCatalogPage({ catalogCode }: { catalogCode?: string }) {
  const catalog = catalogConfiguration(catalogCode);
  const itemsQueryKey = useMemo(() => ['catalog', catalog.code, 'admin-items'] as const, [catalog.code]);
  const revisionsQueryKey = useMemo(() => ['catalog', catalog.code, 'revisions'] as const, [catalog.code]);
  const queryClient = useQueryClient();
  const [search, setSearch] = useState('');
  const [form, setForm] = useState<ReactionTypeForm | null>(null);
  const [reviewNotes, setReviewNotes] = useState<Record<string, string>>({});

  const itemsQuery = useQuery({
    queryKey: [...itemsQueryKey, search],
    queryFn: () => Catalogs.listItems(catalog.code, {
      locale: 'es', q: search || undefined, includeInactive: true, pageSize: 100,
    }),
  });
  const revisionsQuery = useQuery<CatalogRevision[]>({
    queryKey: revisionsQueryKey,
    queryFn: () => Catalogs.listRevisions(catalog.code, 1, 100),
  });
  const items = useMemo(() => itemsQuery.data?.items ?? [], [itemsQuery.data?.items]);
  const nextSortOrder = useMemo(
    () => items.reduce((current, item) => Math.max(current, item.sortOrder + 1), 0),
    [items],
  );

  const refresh = async () => {
    await Promise.all([
      queryClient.invalidateQueries({ queryKey: itemsQueryKey }),
      queryClient.invalidateQueries({ queryKey: revisionsQueryKey }),
      queryClient.invalidateQueries({ queryKey: ['catalogs', catalog.code] }),
    ]);
  };
  const createRevision = useMutation({
    mutationFn: (draft: CatalogDraft) => Catalogs.createRevision(catalog.code, draft),
    onSuccess: async () => { setForm(null); await refresh(); },
  });
  const submitRevision = useMutation({ mutationFn: Catalogs.submitRevision, onSuccess: refresh });
  const approveRevision = useMutation({
    mutationFn: ({ id, notes }: { id: string; notes: string }) => Catalogs.approveRevision(id, {
      notes, emergencyOverride: false,
    }),
    onSuccess: refresh,
  });
  const rejectRevision = useMutation({
    mutationFn: ({ id, notes }: { id: string; notes: string }) => Catalogs.rejectRevision(id, {
      notes, emergencyOverride: false,
    }),
    onSuccess: refresh,
  });

  const formValid = Boolean(
    form?.code.trim()
      && (!catalog.requiresSymbol || (form.displaySymbol.trim() && form.displaySymbol.trim().length <= 16))
      && form.nameEs.trim()
      && form.nameEn.trim()
      && Number.isSafeInteger(form.sortOrder)
      && form.sortOrder >= 0
      && form.reason.trim(),
  );
  const queryFailed = itemsQuery.isError || revisionsQuery.isError;

  return (
    <PageShell
      title={catalog.title}
      subtitle={catalog.subtitle}
    >
      <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1.5} mb={2}>
        <Button component={RouterLink} to="/configuracion/catalogos" sx={{ minHeight: 44 }}>
          Volver a Catálogos
        </Button>
        <Button
          variant="contained"
          sx={{ minHeight: 44 }}
          disabled={queryFailed}
          onClick={() => setForm({ ...emptyForm(), sortOrder: nextSortOrder })}
        >
          Crear {catalog.itemLabel}
        </Button>
      </Stack>

      {queryFailed && (
        <Alert severity="error" sx={{ mb: 2 }}>
          No se pudo validar el catálogo canónico. La edición queda bloqueada para evitar cambios parciales.
        </Alert>
      )}

      <Card sx={{ mb: 2 }}>
        <CardContent>
          <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" spacing={1.5} mb={1.5}>
            <Typography variant="h6">Elementos publicados e inactivos</Typography>
            <TextField
              size="small"
              label="Buscar / Search"
              value={search}
              onChange={(event) => setSearch(event.target.value)}
              inputProps={{ 'aria-label': 'Buscar tipos de reacción' }}
            />
          </Stack>
          {itemsQuery.isLoading ? (
            <Stack direction="row" spacing={1} alignItems="center">
              <CircularProgress size={18} />
              <Typography variant="body2">Cargando catálogo…</Typography>
            </Stack>
          ) : items.length === 0 ? (
            <Alert severity="info">No hay tipos de reacción para este filtro.</Alert>
          ) : (
            <TableContainer sx={{ overflowX: 'auto' }}>
              <Table size="small" aria-label={`${catalog.title}: elementos administrables`}>
                <TableHead>
                  <TableRow>
                    {catalog.requiresSymbol && <TableCell>Símbolo</TableCell>}
                    <TableCell>Nombre / Name</TableCell>
                    <TableCell>Orden</TableCell>
                    <TableCell>Estado</TableCell>
                    <TableCell align="right">Acciones</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {items.map((item) => (
                    <TableRow key={item.id} hover>
                      {catalog.requiresSymbol && <TableCell><Typography fontSize="1.5rem">{item.displaySymbol}</Typography></TableCell>}
                      <TableCell>
                        <Typography variant="body2">{item.nameEs} / {item.nameEn}</Typography>
                        <Typography variant="caption" color="text.secondary">{item.code} · {item.id}</Typography>
                      </TableCell>
                      <TableCell>{item.sortOrder}</TableCell>
                      <TableCell>
                        <Chip size="small" label={item.active ? item.workflowState : 'inactivo'} color={item.active ? 'success' : 'default'} />
                      </TableCell>
                      <TableCell align="right">
                        <Button size="small" sx={{ minHeight: 44 }} disabled={queryFailed} onClick={() => setForm(itemForm(item))}>
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
              <Table size="small" aria-label="Revisiones de tipos de reacción">
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
                          <Typography variant="body2">{revision.draft.displaySymbol} {revision.draft.nameEs}</Typography>
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
                              <Button size="small" sx={{ minHeight: 44 }} onClick={() => submitRevision.mutate(revision.id)}>Enviar</Button>
                            )}
                            {revision.workflowState === 'review' && (
                              <Button size="small" sx={{ minHeight: 44 }} color="success" disabled={!notes.trim()} onClick={() => approveRevision.mutate({ id: revision.id, notes })}>Aprobar</Button>
                            )}
                            {revision.workflowState === 'review' && (
                              <Button size="small" sx={{ minHeight: 44 }} color="error" disabled={!notes.trim()} onClick={() => rejectRevision.mutate({ id: revision.id, notes })}>Rechazar</Button>
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
        <DialogTitle>{form?.entityId ? `Crear revisión de ${catalog.itemLabel}` : `Crear ${catalog.itemLabel}`}</DialogTitle>
        <DialogContent>
          {form && (
            <Stack spacing={2} mt={1}>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <TextField required fullWidth disabled={Boolean(form.entityId)} label="Código estable" value={form.code} onChange={(event) => setForm({ ...form, code: event.target.value })} />
                {catalog.requiresSymbol && <TextField required fullWidth label="Símbolo o emoji" value={form.displaySymbol} onChange={(event) => setForm({ ...form, displaySymbol: event.target.value })} inputProps={{ maxLength: 16 }} />}
                <TextField required fullWidth type="number" label="Orden manual" value={form.sortOrder} onChange={(event) => setForm({ ...form, sortOrder: Number(event.target.value) })} inputProps={{ min: 0, step: 1 }} />
              </Stack>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <TextField required fullWidth label="Nombre en español" value={form.nameEs} onChange={(event) => setForm({ ...form, nameEs: event.target.value })} />
                <TextField required fullWidth label="Name in English" value={form.nameEn} onChange={(event) => setForm({ ...form, nameEn: event.target.value })} />
              </Stack>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                <TextField fullWidth multiline minRows={2} label="Descripción en español" value={form.descriptionEs} onChange={(event) => setForm({ ...form, descriptionEs: event.target.value })} />
                <TextField fullWidth multiline minRows={2} label="Description in English" value={form.descriptionEn} onChange={(event) => setForm({ ...form, descriptionEn: event.target.value })} />
              </Stack>
              <TextField required multiline minRows={2} label="Motivo del cambio / Change reason" value={form.reason} onChange={(event) => setForm({ ...form, reason: event.target.value })} />
              {createRevision.isError && (
                <Alert severity="error">No se pudo crear el borrador. Revisa permisos, versión y datos bilingües.</Alert>
              )}
            </Stack>
          )}
        </DialogContent>
        <DialogActions>
          <Button sx={{ minHeight: 44 }} onClick={() => setForm(null)}>Cancelar</Button>
          <Button
            variant="contained"
            sx={{ minHeight: 44 }}
            disabled={!form || !formValid || createRevision.isPending}
            onClick={() => form && createRevision.mutate(buildReactionTypeDraft(form))}
          >
            Guardar borrador
          </Button>
        </DialogActions>
      </Dialog>
    </PageShell>
  );
}
