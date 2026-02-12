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
import { Brain, RagAdmin, type BrainEntryDTO } from '../api/brain';
import ApiErrorNotice from '../components/ApiErrorNotice';
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

export default function BrainAdminPage() {
  const { session } = useSession();
  const qc = useQueryClient();
  const hasToken = Boolean(session?.apiToken);
  const [includeInactive, setIncludeInactive] = useState(false);
  const [dialogOpen, setDialogOpen] = useState(false);
  const [editingId, setEditingId] = useState<number | null>(null);
  const [form, setForm] = useState<BrainFormState>(emptyForm);
  const [formError, setFormError] = useState<string | null>(null);
  const [refreshNotice, setRefreshNotice] = useState<string | null>(null);

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
    mutationFn: (payload: Parameters<typeof Brain.createEntry>[0]) => Brain.createEntry(payload),
    onSuccess: () => {
      setDialogOpen(false);
      setForm(emptyForm);
      setEditingId(null);
      setFormError(null);
      void qc.invalidateQueries({ queryKey: ['admin', 'brain', 'entries'] });
    },
  });

  const updateMutation = useMutation({
    mutationFn: ({ entryId, payload }: { entryId: number; payload: Parameters<typeof Brain.updateEntry>[1] }) =>
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

  const entries = useMemo<BrainEntryDTO[]>(() => entriesQuery.data ?? [], [entriesQuery.data]);

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

  const handleSave = async () => {
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
                  Ultima actualizacion y total de chunks indexados.
                </Typography>
              </Box>
              <Box flex={1} />
              <Button
                variant="outlined"
                startIcon={<RefreshIcon />}
                onClick={() => {
                  setRefreshNotice(null);
                  refreshMutation.mutate();
                }}
                disabled={refreshMutation.isPending}
              >
                {refreshMutation.isPending ? 'Actualizando...' : 'Refrescar indice'}
              </Button>
            </Stack>
            {ragStatusQuery.isError && (
              <ApiErrorNotice error={ragStatusQuery.error} title="Error cargando RAG" />
            )}
            {refreshMutation.isError && (
              <Alert severity="error">
                {refreshMutation.error instanceof Error ? refreshMutation.error.message : 'Error al refrescar RAG.'}
              </Alert>
            )}
            {refreshNotice && <Alert severity="success">{refreshNotice}</Alert>}
            {ragStatusQuery.data && (
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
              <FormControlLabel
                control={
                  <Switch
                    checked={includeInactive}
                    onChange={(e) => setIncludeInactive(e.target.checked)}
                  />
                }
                label="Incluir inactivas"
              />
              <Button
                variant="contained"
                startIcon={<AddIcon />}
                onClick={openCreate}
              >
                Nueva entrada
              </Button>
            </Stack>

            {entriesQuery.isError && (
              <ApiErrorNotice error={entriesQuery.error} title="Error cargando entradas" />
            )}

            {!entriesQuery.isLoading && entries.length === 0 && (
              <Alert severity="info">No hay entradas cargadas.</Alert>
            )}

            <Stack spacing={2}>
              {entries.map((entry) => (
                <Card key={entry.bedId} variant="outlined">
                  <CardContent>
                    <Stack spacing={1}>
                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
                        <Typography variant="h6" fontWeight={700}>
                          {entry.bedTitle}
                        </Typography>
                        <Chip
                          label={entry.bedActive ? 'Activa' : 'Inactiva'}
                          color={entry.bedActive ? 'success' : 'default'}
                          size="small"
                        />
                        {entry.bedCategory && (
                          <Chip label={entry.bedCategory} variant="outlined" size="small" />
                        )}
                      </Stack>
                      <Typography variant="body2" color="text.secondary">
                        {entry.bedBody.length > 180 ? `${entry.bedBody.slice(0, 180)}...` : entry.bedBody}
                      </Typography>
                      <Stack direction="row" spacing={1} flexWrap="wrap">
                        {(entry.bedTags ?? []).map((tag) => (
                          <Chip key={tag} label={tag} size="small" variant="outlined" />
                        ))}
                        <Chip
                          label={`Actualizado: ${formatTimestamp(entry.bedUpdatedAt)}`}
                          size="small"
                        />
                      </Stack>
                    </Stack>
                  </CardContent>
                  <CardActions>
                    <Button
                      startIcon={<EditIcon />}
                      size="small"
                      onClick={() => {
                        openEdit(entry);
                      }}
                    >
                      Editar
                    </Button>
                  </CardActions>
                </Card>
              ))}
            </Stack>
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
          <Button onClick={() => setDialogOpen(false)} color="inherit">
            Cancelar
          </Button>
          <Button
            variant="contained"
            onClick={() => {
              void handleSave();
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
