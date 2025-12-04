import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  IconButton,
  Snackbar,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import RadioButtonUncheckedIcon from '@mui/icons-material/RadioButtonUnchecked';
import KeyboardIcon from '@mui/icons-material/Keyboard';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Label } from '../api/label';
import type { LabelTrackDTO } from '../api/types';

export default function LabelTracksPage() {
  const qc = useQueryClient();
  const [input, setInput] = useState('');
  const [note, setNote] = useState('');
  const [statusFilter, setStatusFilter] = useState<'all' | 'open' | 'done'>('all');
  const [query, setQuery] = useState('');
  const [timeFilter, setTimeFilter] = useState<'any' | 'today' | 'week'>('any');
  const [quickEditId, setQuickEditId] = useState<string | null>(null);
  const [quickEditValue, setQuickEditValue] = useState<string>('');
  const [editing, setEditing] = useState<LabelTrackDTO | null>(null);
  const [editTitle, setEditTitle] = useState('');
  const [editNote, setEditNote] = useState('');
  const [toast, setToast] = useState<string | null>(null);
  const [showShortcuts, setShowShortcuts] = useState(false);

  const tracksQuery = useQuery({
    queryKey: ['label-tracks'],
    queryFn: Label.listTracks,
  });

  const createMutation = useMutation({
    mutationFn: () => Label.createTrack({ ltcTitle: input.trim(), ltcNote: note.trim() || undefined }),
    onSuccess: () => {
      setInput('');
      setNote('');
      void qc.invalidateQueries({ queryKey: ['label-tracks'] });
    },
  });

  const updateMutation = useMutation({
    mutationFn: ({ id, status, title, note: noteVal }: { id: string; status?: string; title?: string; note?: string }) =>
      Label.updateTrack(id, {
        ...(status ? { ltuStatus: status } : {}),
        ...(title ? { ltuTitle: title } : {}),
        ...(typeof noteVal !== 'undefined' ? { ltuNote: noteVal } : {}),
      }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['label-tracks'] });
      setToast('Guardado');
    },
  });

  const deleteMutation = useMutation({
    mutationFn: (id: string) => Label.deleteTrack(id),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['label-tracks'] });
      setToast('Eliminado');
    },
  });

  useEffect(() => {
    const handler = (event: KeyboardEvent) => {
      const activeTag = (event.target as HTMLElement | null)?.tagName?.toLowerCase();
      if (activeTag === 'input' || activeTag === 'textarea' || (event.target as HTMLElement | null)?.isContentEditable) {
        return;
      }
      if (event.key.toLowerCase() === 'n') {
        event.preventDefault();
        const titleInput = document.getElementById('track-title-input');
        if (titleInput instanceof HTMLElement) titleInput.focus();
      }
      if (event.key.toLowerCase() === 'f') {
        event.preventDefault();
        const searchInput = document.getElementById('track-search-input');
        if (searchInput instanceof HTMLElement) searchInput.focus();
      }
    };
    window.addEventListener('keydown', handler);
    return () => window.removeEventListener('keydown', handler);
  }, []);

  const tracks = useMemo(() => tracksQuery.data ?? [], [tracksQuery.data]);
  const filteredTracks = useMemo(() => {
    const byStatus = statusFilter === 'all' ? tracks : tracks.filter((t) => t.ltStatus === statusFilter);
    const q = query.trim().toLowerCase();
    const byTime = byStatus.filter((t) => {
      if (timeFilter === 'any') return true;
      const created = new Date(t.ltCreatedAt);
      const now = new Date();
      if (timeFilter === 'today') {
        const isToday =
          created.getFullYear() === now.getFullYear() &&
          created.getMonth() === now.getMonth() &&
          created.getDate() === now.getDate();
        return isToday;
      }
      const startOfWeek = new Date(now);
      startOfWeek.setDate(now.getDate() - now.getDay());
      startOfWeek.setHours(0, 0, 0, 0);
      return created >= startOfWeek;
    });
    if (!q) return byTime;
    return byTime.filter(
      (t) =>
        t.ltTitle.toLowerCase().includes(q) ||
        (t.ltNote ?? '').toLowerCase().includes(q),
    );
  }, [tracks, statusFilter, query, timeFilter]);

  const handleAdd = () => {
    if (!input.trim()) return;
    createMutation.mutate();
    setToast('Agregado');
  };

  const handleToggle = (track: LabelTrackDTO) => {
    const next = track.ltStatus === 'done' ? 'open' : 'done';
    updateMutation.mutate({ id: track.ltId, status: next });
  };

  const openEdit = (track: LabelTrackDTO) => {
    setEditing(track);
    setEditTitle(track.ltTitle);
    setEditNote(track.ltNote ?? '');
  };

  const submitEdit = () => {
    if (!editing) return;
    updateMutation.mutate({
      id: editing.ltId,
      title: editTitle.trim(),
      note: editNote.trim(),
    });
    setEditing(null);
  };

  return (
    <Stack spacing={3}>
      <Stack spacing={0.5}>
        <Typography variant="h4" fontWeight={700}>
          Label / Tracks
        </Typography>
        <Typography variant="body1" color="text.secondary">
          Captura notas y pendientes rápidos para esta sección.
        </Typography>
      </Stack>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
              <TextField
                id="track-title-input"
                label="Título"
                value={input}
                onChange={(e) => setInput(e.target.value)}
                fullWidth
                size="small"
              />
              <TextField
                label="Nota (opcional)"
                value={note}
                onChange={(e) => setNote(e.target.value)}
                fullWidth
                size="small"
              />
              <Button
                variant="contained"
                onClick={handleAdd}
                sx={{ minWidth: 140 }}
                disabled={createMutation.isPending}
              >
                {createMutation.isPending ? 'Guardando…' : 'Agregar'}
              </Button>
            </Stack>
            {createMutation.isError && (
              <Alert severity="error">No pudimos guardar la nota. Intenta de nuevo.</Alert>
            )}
          </Stack>
        </CardContent>
      </Card>

      {tracksQuery.isLoading && (
        <Box display="flex" justifyContent="center" py={3}>
          <CircularProgress />
        </Box>
      )}

      {!tracksQuery.isLoading && tracks.length === 0 && (
        <Typography color="text.secondary">No hay notas aún.</Typography>
      )}

      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }}>
        <Stack direction="row" spacing={1} alignItems="center">
          <Typography variant="body2" color="text.secondary">
            Filtrar:
          </Typography>
          {(['all', 'open', 'done'] as const).map((key) => (
            <Chip
              key={key}
              label={key === 'all' ? 'Todos' : key === 'open' ? 'Abiertos' : 'Cerrados'}
              variant={statusFilter === key ? 'filled' : 'outlined'}
              color={key === 'done' ? 'success' : 'default'}
              size="small"
              onClick={() => setStatusFilter(key)}
            />
          ))}
        </Stack>
        <Stack direction="row" spacing={1} alignItems="center">
          <Typography variant="body2" color="text.secondary">
            Creación:
          </Typography>
          {(['any', 'today', 'week'] as const).map((key) => (
            <Chip
              key={key}
              label={key === 'any' ? 'Todas' : key === 'today' ? 'Hoy' : 'Últimos 7 días'}
              variant={timeFilter === key ? 'filled' : 'outlined'}
              size="small"
              onClick={() => setTimeFilter(key)}
            />
          ))}
        </Stack>
        <TextField
          id="track-search-input"
          size="small"
          label="Buscar por título o nota"
          value={query}
          onChange={(e) => setQuery(e.target.value)}
          sx={{ minWidth: { xs: '100%', sm: 260 } }}
        />
        <Chip
          icon={<KeyboardIcon />}
          label="Atajos: N (nuevo), F (buscar)"
          variant={showShortcuts ? 'filled' : 'outlined'}
          onClick={() => setShowShortcuts((prev) => !prev)}
          size="small"
        />
      </Stack>

      <Stack spacing={1.5}>
        {filteredTracks.map((track) => {
          const isDone = track.ltStatus === 'done';
          const isQuickEditing = quickEditId === track.ltId;
          return (
            <Card key={track.ltId} variant="outlined">
              <CardContent>
                <Stack direction="row" spacing={1} alignItems="flex-start">
                  <IconButton size="small" onClick={() => handleToggle(track)}>
                    {isDone ? (
                      <CheckCircleIcon color="success" />
                    ) : (
                      <RadioButtonUncheckedIcon color="disabled" />
                    )}
                  </IconButton>
                  <Box sx={{ flexGrow: 1 }}>
                    <Typography fontWeight={700} sx={{ textDecoration: isDone ? 'line-through' : 'none' }}>
                      {track.ltTitle}
                    </Typography>
                    {track.ltNote && (
                      <Typography variant="body2" color="text.secondary">
                        {track.ltNote}
                      </Typography>
                    )}
                    {isQuickEditing && (
                      <Stack spacing={1} mt={1}>
                        <TextField
                          size="small"
                          label="Editar nota rápido"
                          value={quickEditValue}
                          onChange={(e) => setQuickEditValue(e.target.value)}
                          multiline
                          minRows={2}
                        />
                        <Stack direction="row" spacing={1}>
                          <Button
                            size="small"
                            variant="contained"
                            onClick={() => {
                              updateMutation.mutate({
                                id: track.ltId,
                                note: quickEditValue.trim(),
                              });
                              setQuickEditId(null);
                            }}
                          >
                            Guardar nota
                          </Button>
                          <Button size="small" onClick={() => setQuickEditId(null)}>
                            Cancelar
                          </Button>
                        </Stack>
                      </Stack>
                    )}
                    <Stack direction="row" spacing={1} mt={1}>
                      <Chip
                        size="small"
                        label={isDone ? 'Cerrado' : 'Abierto'}
                        color={isDone ? 'success' : 'default'}
                      />
                      <Typography variant="caption" color="text.secondary">
                        Creado {new Date(track.ltCreatedAt).toLocaleDateString()}
                      </Typography>
                    </Stack>
                  </Box>
                  <Stack direction="row" spacing={1}>
                    <Button variant="text" size="small" onClick={() => openEdit(track)}>
                      Editar
                    </Button>
                    <Button
                      variant="outlined"
                      size="small"
                      onClick={() => {
                        setQuickEditId(track.ltId);
                        setQuickEditValue(track.ltNote ?? '');
                      }}
                    >
                      Editar nota rápido
                    </Button>
                  </Stack>
                  <IconButton size="small" onClick={() => deleteMutation.mutate(track.ltId)}>
                    <DeleteIcon fontSize="small" />
                  </IconButton>
                </Stack>
              </CardContent>
            </Card>
          );
        })}
      </Stack>

      <Dialog open={Boolean(editing)} onClose={() => setEditing(null)} maxWidth="sm" fullWidth>
        <DialogTitle>Editar track</DialogTitle>
        <DialogContent sx={{ pt: 1 }}>
          <Stack spacing={2}>
            <TextField
              label="Título"
              value={editTitle}
              onChange={(e) => setEditTitle(e.target.value)}
              fullWidth
              size="small"
            />
            <TextField
              label="Nota"
              value={editNote}
              onChange={(e) => setEditNote(e.target.value)}
              fullWidth
              size="small"
              multiline
              minRows={2}
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setEditing(null)} color="inherit">
            Cancelar
          </Button>
          <Button onClick={submitEdit} variant="contained" disabled={updateMutation.isPending}>
            Guardar
          </Button>
        </DialogActions>
      </Dialog>

      <Snackbar
        open={Boolean(toast)}
        autoHideDuration={2500}
        onClose={() => setToast(null)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
      >
        <Alert severity="success" onClose={() => setToast(null)} sx={{ width: '100%' }}>
          {toast}
        </Alert>
      </Snackbar>
    </Stack>
  );
}
