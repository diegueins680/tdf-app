import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardActions,
  CardContent,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControlLabel,
  Paper,
  Stack,
  Switch,
  TextField,
  Typography,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import AddIcon from '@mui/icons-material/Add';
import EditIcon from '@mui/icons-material/Edit';
import { Brain, RagAdmin, type BrainEntryCreate, type BrainEntryDTO, type BrainEntryUpdate } from '../api/brain';
import ApiErrorNotice, { ApiLoadingNotice } from '../components/ApiErrorNotice';
import LazyPaginatedList from '../components/LazyPaginatedList';
import { SessionGate } from '../components/SessionGate';
import { useSession } from '../session/SessionContext';

interface BrainFormState {
  title: string;
  body: string;
  category: string;
  tags: string;
  active: boolean;
}

const emptyForm: BrainFormState = {
  title: '',
  body: '',
  category: '',
  tags: '',
  active: true,
};

type BrainAdminPageDisplayContract = Readonly<{
  singleEntryBodyPreviewChars: number;
  entryListBodyPreviewChars: number;
}>;

// Invariant: Brain entry previews are bounded by view density; the single-entry
// view can be more verbose than cards in a multi-entry list.
const BRAIN_ADMIN_PAGE_DISPLAY_CONTRACTS = {
  singleEntryBodyPreviewChars: 2 * 100 + 2 * 10,
  entryListBodyPreviewChars: 2 * 100 - 2 * 10,
} as const satisfies BrainAdminPageDisplayContract;

const formatTimestamp = (value?: string | null) => {
  if (!value) return '-';
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return value;
  return parsed.toLocaleString();
};

const parseTags = (raw: string) =>
  raw
    .split(/,|\n/)
    .map((tag) => tag.trim())
    .filter(Boolean);

const summarizeEntryBody = (
  value: string,
  maxLength = BRAIN_ADMIN_PAGE_DISPLAY_CONTRACTS.singleEntryBodyPreviewChars,
) => {
  const trimmed = value.trim();
  if (maxLength >= trimmed.length) return trimmed;
  return `${trimmed.slice(0, maxLength)}...`;
};

const formatEntryTagsSummary = (tags?: string[] | null) =>
  (tags ?? [])
    .map((tag) => tag.trim())
    .filter(Boolean)
    .join(', ');

interface SingleBrainEntryCardProps {
  entry: BrainEntryDTO;
  tagsSummary: string;
  bodyPreview: string;
  onEdit: (entry: BrainEntryDTO) => void;
}

function SingleBrainEntryCard(props: SingleBrainEntryCardProps) {
  const { entry, tagsSummary, bodyPreview, onEdit } = props;

  return (
    <Card variant="outlined">
      <CardContent>
        <Stack spacing={1.5}>
          <Stack spacing={0.5}>
            <Typography variant="subtitle1" fontWeight={700}>
              Primera entrada del Brain
            </Typography>
            <Typography variant="body2" color="text.secondary">
              Revisa esta entrada desde un resumen simple. Cuando exista la segunda, volvera la lista
              completa para comparar titulo, categoria y actualizacion.
            </Typography>
          </Stack>
          <Stack spacing={0.75}>
            <Typography variant="h6" fontWeight={700}>
              {entry.bedTitle}
            </Typography>
            {entry.bedCategory && (
              <Typography variant="body2" color="text.secondary">
                Categoria: {entry.bedCategory}
              </Typography>
            )}
            <Typography variant="body2" color="text.secondary">
              {bodyPreview}
            </Typography>
            {tagsSummary && (
              <Typography variant="body2" color="text.secondary">
                Tags: {tagsSummary}
              </Typography>
            )}
            <Typography variant="caption" color="text.secondary">
              {`Actualizado: ${formatTimestamp(entry.bedUpdatedAt)}`}
            </Typography>
          </Stack>
          <Button
            startIcon={<EditIcon />}
            size="small"
            variant="outlined"
            tabIndex={0}
            onClick={(event) => {
              event.currentTarget.focus();
              onEdit(entry);
            }}
            sx={{ alignSelf: 'flex-start' }}
          >
            Editar entrada
          </Button>
        </Stack>
      </CardContent>
    </Card>
  );
}

interface BrainEntryCardProps {
  entry: BrainEntryDTO;
  includeInactive: boolean;
  onEdit: (entry: BrainEntryDTO) => void;
}

function BrainEntryCard(props: BrainEntryCardProps) {
  const { entry, includeInactive, onEdit } = props;
  const bodyPreviewMaxLength = BRAIN_ADMIN_PAGE_DISPLAY_CONTRACTS.entryListBodyPreviewChars;
  const bodyPreview = entry.bedBody.length > bodyPreviewMaxLength
    ? `${entry.bedBody.slice(0, bodyPreviewMaxLength)}...`
    : entry.bedBody;

  return (
    <Card variant="outlined">
      <CardContent>
        <Stack spacing={1}>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
            <Typography variant="h6" fontWeight={700}>
              {entry.bedTitle}
            </Typography>
            {(includeInactive || !entry.bedActive) && (
              <Chip label={entry.bedActive ? 'Activa' : 'Inactiva'} color={entry.bedActive ? 'success' : 'default'} size="small" />
            )}
            {entry.bedCategory && <Chip label={entry.bedCategory} variant="outlined" size="small" />}
          </Stack>
          <Typography variant="body2" color="text.secondary">
            {bodyPreview}
          </Typography>
          <Stack direction="row" spacing={1} flexWrap="wrap">
            {(entry.bedTags ?? []).map((tag) => (
              <Chip key={tag} label={tag} size="small" variant="outlined" />
            ))}
            <Chip label={`Actualizado: ${formatTimestamp(entry.bedUpdatedAt)}`} size="small" />
          </Stack>
        </Stack>
      </CardContent>
      <CardActions>
        <Button
          startIcon={<EditIcon />}
          size="small"
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            onEdit(entry);
          }}
        >
          Editar
        </Button>
      </CardActions>
    </Card>
  );
}

interface BrainEntriesListProps {
  entries: readonly BrainEntryDTO[];
  loading: boolean;
  includeInactive: boolean;
  onEdit: (entry: BrainEntryDTO) => void;
}

function BrainEntriesList(props: BrainEntriesListProps) {
  const { entries, loading, includeInactive, onEdit } = props;

  return (
    <LazyPaginatedList
      items={entries}
      loading={loading}
      pagination={{ itemLabel: 'entradas', initialRowsPerPage: 10, resetKey: includeInactive }}
      renderItems={(visibleEntries) => (
        <Stack spacing={2}>
          {visibleEntries.map((entry) => (
            <BrainEntryCard key={entry.bedId} entry={entry} includeInactive={includeInactive} onEdit={onEdit} />
          ))}
        </Stack>
      )}
    />
  );
}

export default function BrainAdminPage() {
  const { session } = useSession();
  const qc = useQueryClient();
  const hasToken = Boolean(session);
  const [includeInactive, setIncludeInactive] = useState(false);
  const [dialogOpen, setDialogOpen] = useState(false);
  const [editingId, setEditingId] = useState(null as number | null);
  const [form, setForm] = useState(emptyForm as BrainFormState);
  const [formError, setFormError] = useState(null as string | null);
  const [refreshNotice, setRefreshNotice] = useState(null as string | null);

  const ragStatusQuery = useQuery({
    queryKey: ['admin', 'rag', 'status'],
    queryFn: RagAdmin.status,
    enabled: hasToken,
  });

  const entriesQuery = useQuery({
    queryKey: ['admin', 'brain', 'entries', includeInactive],
    queryFn: () => Brain.listEntries(includeInactive),
    enabled: hasToken,
  });

  const createMutation = useMutation({
    mutationFn: (payload: BrainEntryCreate) => Brain.createEntry(payload),
    onSuccess: () => {
      setDialogOpen(false);
      setForm(emptyForm);
      setEditingId(null);
      setFormError(null);
      void qc.invalidateQueries({ queryKey: ['admin', 'brain', 'entries'] });
    },
  });

  const updateMutation = useMutation({
    mutationFn: ({ entryId, payload }: { entryId: number; payload: BrainEntryUpdate }) =>
      Brain.updateEntry(entryId, payload),
    onSuccess: () => {
      setDialogOpen(false);
      setEditingId(null);
      setFormError(null);
      void qc.invalidateQueries({ queryKey: ['admin', 'brain', 'entries'] });
    },
  });

  const refreshMutation = useMutation({
    mutationFn: RagAdmin.refresh,
    onSuccess: (data) => {
      setRefreshNotice(`Indice actualizado. Chunks: ${data.rrrChunks}`);
      void qc.invalidateQueries({ queryKey: ['admin', 'rag', 'status'] });
    },
  });

  const entries: BrainEntryDTO[] = useMemo(() => entriesQuery.data ?? [], [entriesQuery.data]);
  const showEmptyEntriesState = !entriesQuery.isLoading && !entriesQuery.isError && entries.length === 0;
  const showInactiveToggle = includeInactive || entries.length > 0;
  const emptyEntriesMessage = includeInactive
    ? 'No hay entradas cargadas, incluyendo inactivas.'
    : 'No hay entradas activas. Crea la primera entrada del Brain o revisa inactivas si esperabas contenido archivado.';
  const showRagFirstEntryGuidance = showEmptyEntriesState;
  const hasBuiltRagIndex = Boolean(
    ragStatusQuery.data
      && (ragStatusQuery.data.risCount > 0 || ragStatusQuery.data.risUpdatedAt),
  );
  const showRagIndexPendingState = Boolean(
    ragStatusQuery.data && !showRagFirstEntryGuidance && !hasBuiltRagIndex,
  );
  const singleActiveEntry = !entriesQuery.isLoading
    && !entriesQuery.isError
    && !includeInactive
    && entries.length === 1
    && (entries[0]?.bedActive ?? false)
      ? (entries[0] ?? null)
      : null;
  const singleEntryTagsSummary = singleActiveEntry ? formatEntryTagsSummary(singleActiveEntry.bedTags) : '';
  const singleEntryBodyPreview = singleActiveEntry ? summarizeEntryBody(singleActiveEntry.bedBody) : '';

  const openCreate = () => {
    setEditingId(null);
    setForm(emptyForm);
    setFormError(null);
    setDialogOpen(true);
  };

  const openEdit = (entry: BrainEntryDTO) => {
    setEditingId(entry.bedId);
    setForm({
      title: entry.bedTitle ?? '',
      body: entry.bedBody ?? '',
      category: entry.bedCategory ?? '',
      tags: (entry.bedTags ?? []).join(', '),
      active: entry.bedActive,
    });
    setFormError(null);
    setDialogOpen(true);
  };

  const saveEntry = async () => {
    const title = form.title.trim();
    const body = form.body.trim();
    if (!title) {
      setFormError('Agrega un titulo.');
      return;
    }
    if (!body) {
      setFormError('Agrega contenido.');
      return;
    }
    const category = form.category.trim();
    const tags = parseTags(form.tags);
    setFormError(null);
    const payloadBase = {
      title,
      body,
      category: category || null,
      tags,
      active: form.active,
    };
    try {
      if (editingId != null) {
        await updateMutation.mutateAsync({
          entryId: editingId,
          payload: {
            beuTitle: payloadBase.title,
            beuBody: payloadBase.body,
            beuCategory: payloadBase.category,
            beuTags: payloadBase.tags,
            beuActive: payloadBase.active,
          },
        });
      } else {
        await createMutation.mutateAsync({
          becTitle: payloadBase.title,
          becBody: payloadBase.body,
          becCategory: payloadBase.category,
          becTags: payloadBase.tags,
          becActive: payloadBase.active,
        });
      }
    } catch (err) {
      setFormError(err instanceof Error ? err.message : 'No se pudo guardar la entrada.');
    }
  };

  return (
    <SessionGate message="Inicia sesion para administrar el Brain del estudio.">
      <Stack spacing={3}>
        <Stack spacing={0.5}>
          <Typography variant="overline" color="text.secondary">
            Configuracion - Brain
          </Typography>
          <Typography variant="h4" fontWeight={800}>
            Brain del estudio
          </Typography>
          <Typography color="text.secondary">
            Manten el conocimiento base que alimenta el Brain y el indice RAG.
          </Typography>
        </Stack>

        <Paper sx={{ p: 2.5 }}>
          <Stack spacing={2}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ sm: 'center' }}>
              <Box>
                <Typography variant="subtitle1" fontWeight={700}>
                  Estado del indice RAG
                </Typography>
                <Typography variant="body2" color="text.secondary">
                  {showRagFirstEntryGuidance
                    ? 'Crea la primera entrada del Brain para activar este resumen. El refresco del indice aparecera cuando exista contenido para reindexar.'
                    : 'Ultima actualizacion y total de chunks indexados.'}
                </Typography>
              </Box>
              <Box flex={1} />
              {!showRagFirstEntryGuidance && (
                <Button
                  variant="outlined"
                  startIcon={<RefreshIcon />}
                  tabIndex={0}
                  onClick={(event) => {
                    event.currentTarget.focus();
                    setRefreshNotice(null);
                    refreshMutation.mutate();
                  }}
                  disabled={refreshMutation.isPending}
                >
                  {refreshMutation.isPending ? 'Actualizando...' : 'Refrescar indice'}
                </Button>
              )}
            </Stack>
            {ragStatusQuery.isError && (
              <ApiErrorNotice error={ragStatusQuery.error} title="Error cargando RAG" />
            )}
            {ragStatusQuery.isLoading && (
              <ApiLoadingNotice
                title="Cargando estado RAG"
                message="Consultando el indice antes de mostrar acciones."
              />
            )}
            {refreshMutation.isError && (
              <Alert severity="error">
                {refreshMutation.error instanceof Error ? refreshMutation.error.message : 'Error al refrescar RAG.'}
              </Alert>
            )}
            {refreshNotice && <Alert severity="success">{refreshNotice}</Alert>}
            {showRagIndexPendingState && (
              <Alert severity="info" variant="outlined" data-testid="brain-admin-rag-index-pending">
                El indice todavia no tiene chunks. Usa Refrescar indice cuando termines de revisar entradas.
              </Alert>
            )}
            {ragStatusQuery.data && !showRagFirstEntryGuidance && hasBuiltRagIndex && (
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
                <Chip label={`Chunks: ${ragStatusQuery.data.risCount}`} color="primary" />
                <Chip
                  label={`Actualizado: ${formatTimestamp(ragStatusQuery.data.risUpdatedAt)}`}
                  variant="outlined"
                />
                <Chip
                  label={ragStatusQuery.data.risStale ? 'Stale' : 'OK'}
                  color={ragStatusQuery.data.risStale ? 'warning' : 'success'}
                />
              </Stack>
            )}
          </Stack>
        </Paper>

        <Paper sx={{ p: 2.5 }}>
          <Stack spacing={2}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ sm: 'center' }}>
              <Typography variant="h6" fontWeight={700}>
                Entradas del Brain
              </Typography>
              <Box flex={1} />
              {showInactiveToggle && (
                <FormControlLabel
                  control={
                    <Switch
                      checked={includeInactive}
                      onChange={(e) => setIncludeInactive(e.target.checked)}
                    />
                  }
                  label="Incluir inactivas"
                />
              )}
              <Button
                variant="contained"
                startIcon={<AddIcon />}
                tabIndex={0}
                onClick={(event) => {
                  event.currentTarget.focus();
                  openCreate();
                }}
              >
                Nueva entrada
              </Button>
            </Stack>

            {entriesQuery.isError && (
              <ApiErrorNotice error={entriesQuery.error} title="Error cargando entradas" />
            )}
            {entriesQuery.isLoading && (
              <ApiLoadingNotice
                title="Cargando entradas"
                message="Estamos preparando las entradas del Brain."
              />
            )}

            {showEmptyEntriesState && (
              <Alert
                severity="info"
                action={
                  includeInactive ? undefined : (
                    <Button
                      color="inherit"
                      size="small"
                      tabIndex={0}
                      onClick={(event) => {
                        event.currentTarget.focus();
                        setIncludeInactive(true);
                      }}
                    >
                      Revisar inactivas
                    </Button>
                  )
                }
              >
                {emptyEntriesMessage}
              </Alert>
            )}

            {singleActiveEntry ? (
              <SingleBrainEntryCard
                entry={singleActiveEntry}
                tagsSummary={singleEntryTagsSummary}
                bodyPreview={singleEntryBodyPreview}
                onEdit={openEdit}
              />
            ) : (
              <BrainEntriesList
                entries={entries}
                // Suppress the list's own progress bar during the very first
                // load: the dedicated "Cargando entradas" notice already covers
                // it. The bar still shows for refetches over existing entries.
                loading={entriesQuery.isFetching && !entriesQuery.isLoading}
                includeInactive={includeInactive}
                onEdit={openEdit}
              />
            )}
          </Stack>
        </Paper>
      </Stack>

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} maxWidth="md" fullWidth>
        <DialogTitle>{editingId ? 'Editar entrada' : 'Nueva entrada'}</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            {formError && <Alert severity="error">{formError}</Alert>}
            <TextField
              label="Titulo"
              value={form.title}
              onChange={(e) => setForm((prev) => ({ ...prev, title: e.target.value }))}
              required
              fullWidth
            />
            <TextField
              label="Categoria"
              value={form.category}
              onChange={(e) => setForm((prev) => ({ ...prev, category: e.target.value }))}
              placeholder="ej. servicios, pricing, estudio"
              fullWidth
            />
            <TextField
              label="Tags"
              value={form.tags}
              onChange={(e) => setForm((prev) => ({ ...prev, tags: e.target.value }))}
              placeholder="Separados por coma"
              fullWidth
            />
            <TextField
              label="Contenido"
              value={form.body}
              onChange={(e) => setForm((prev) => ({ ...prev, body: e.target.value }))}
              multiline
              minRows={6}
              required
              fullWidth
            />
            <FormControlLabel
              control={
                <Switch
                  checked={form.active}
                  onChange={(e) => setForm((prev) => ({ ...prev, active: e.target.checked }))}
                />
              }
              label="Activa"
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button
            tabIndex={0}
            onClick={(event) => {
              event.currentTarget.focus();
              setDialogOpen(false);
            }}
            color="inherit"
          >
            Cancelar
          </Button>
          <Button
            variant="contained"
            tabIndex={0}
            onClick={(event) => {
              event.currentTarget.focus();
              void saveEntry();
            }}
            disabled={createMutation.isPending || updateMutation.isPending}
          >
            {createMutation.isPending || updateMutation.isPending ? 'Guardando...' : 'Guardar'}
          </Button>
        </DialogActions>
      </Dialog>
    </SessionGate>
  );
}
