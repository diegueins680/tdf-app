import { useEffect, useMemo, useState } from 'react';
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
import { Link as RouterLink } from 'react-router-dom';
import { Label } from '../api/label';
import type { LabelTrackDTO } from '../api/types';
import { SessionGate } from '../components/SessionGate';
import { useSession } from '../session/SessionContext';

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

  const { session } = useSession();
  const roles = useMemo(() => (session?.roles ?? []).map((r) => r.toLowerCase()), [session?.roles]);
  const isAdmin = roles.some((r) => r.includes('admin'));
  const isArtist = roles.some((r) => r.includes('artist'));
  const ownerIdForApi = isAdmin ? undefined : session?.partyId && session.partyId > 0 ? session.partyId : undefined;
  const ownerKey = isAdmin ? 'tdf' : ownerIdForApi ?? 'missing-owner';
  const canUseTracks = isAdmin || isArtist;
  const hasOwnerScope = isAdmin || Boolean(ownerIdForApi);
  const scopeLabel = isAdmin ? 'TDF Records' : 'Mi artista';
  const inputsDisabled = !hasOwnerScope;

  const tracksQuery = useQuery({
    queryKey: ['label-tracks', ownerKey],
    queryFn: () => Label.listTracks(ownerIdForApi ?? undefined),
    enabled: canUseTracks && hasOwnerScope,
  });

  const createMutation = useMutation({
    mutationFn: () =>
      Label.createTrack(
        { ltcTitle: input.trim(), ltcNote: note.trim() || undefined },
        ownerIdForApi ?? undefined,
      ),
    onSuccess: () => {
      setInput('');
      setNote('');
      void qc.invalidateQueries({ queryKey: ['label-tracks', ownerKey] });
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
      void qc.invalidateQueries({ queryKey: ['label-tracks', ownerKey] });
      setToast('Guardado');
    },
  });

  const deleteMutation = useMutation({
    mutationFn: (id: string) => Label.deleteTrack(id),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['label-tracks', ownerKey] });
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

  const tracks = useMemo(() => (hasOwnerScope ? tracksQuery.data ?? [] : []), [hasOwnerScope, tracksQuery.data]);
  const missingOwner = !isAdmin && !ownerIdForApi;
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
    if (!hasOwnerScope) {
      setToast('Asocia tu perfil de artista para crear operaciones.');
      return;
    }
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

  if (!canUseTracks) {
    return (
      <SessionGate message="Inicia sesión para gestionar operaciones.">
        <Alert severity="warning">Necesitas rol de artista o admin para gestionar operaciones.</Alert>
      </SessionGate>
    );
  }

  return (
    <SessionGate message="Inicia sesión para gestionar tus operaciones.">
      <Stack spacing={3}>
        <Stack spacing={0.5}>
          <Typography variant="h4" fontWeight={700}>
            {isAdmin ? 'Label / Tracks' : 'Operaciones de artista'}
          </Typography>
          <Typography variant="body1" color="text.secondary">
            Captura notas y pendientes rápidos con las mismas herramientas que usa el admin para {scopeLabel}.
          </Typography>
          <Stack direction="row" spacing={1} alignItems="center">
            <Chip label={`Scope: ${scopeLabel}`} color="primary" size="small" />
            {!isAdmin && session?.displayName && (
              <Chip label={session.displayName} variant="outlined" size="small" />
            )}
          </Stack>
        </Stack>

        {missingOwner && (
          <Alert
            severity="warning"
            action={
              <Button
                size="small"
                variant="outlined"
                color="inherit"
                component={RouterLink}
                to="/mi-artista"
              >
                Vincular perfil
              </Button>
            }
          >
            No encontramos un artista asociado a tu cuenta. Vincula tu perfil para usar estas operaciones.
          </Alert>
        )}

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
                  disabled={inputsDisabled}
                />
                <TextField
                  label="Nota (opcional)"
                  value={note}
                  onChange={(e) => setNote(e.target.value)}
                  fullWidth
                  size="small"
                  disabled={inputsDisabled}
                />
                <Button
                  variant="contained"
                  onClick={handleAdd}
                  sx={{ minWidth: 140 }}
                  disabled={createMutation.isPending || !input.trim() || !hasOwnerScope}
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
        <Card variant="outlined" sx={{ borderColor: 'rgba(255,255,255,0.1)' }}>
          <CardContent>
            <Stack spacing={1}>
              <Typography variant="h6">Aún no hay pendientes</Typography>
              <Typography variant="body2" color="text.secondary">
                Crea tus primeras operaciones internas. Si no tienes perfil de artista, créalo o reclámalo para habilitar tus herramientas.
              </Typography>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                <Button variant="contained" size="small" onClick={() => setShowShortcuts(false)} href="/mi-artista">
                  Crear/editar mi perfil
                </Button>
                <Button
                  variant="outlined"
                  size="small"
                  href="/label/artistas"
                  target="_blank"
                  rel="noreferrer"
                >
                  Ver artistas del label
                </Button>
              </Stack>
            </Stack>
          </CardContent>
        </Card>
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
              disabled={inputsDisabled}
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
              disabled={inputsDisabled}
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
          disabled={inputsDisabled}
        />
        <Chip
          icon={<KeyboardIcon />}
          label="Atajos: N (nuevo), F (buscar)"
          variant={showShortcuts ? 'filled' : 'outlined'}
          onClick={() => setShowShortcuts((prev) => !prev)}
          size="small"
          disabled={inputsDisabled}
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
                    {!isDone && (
                      <Button
                        variant="outlined"
                        size="small"
                        onClick={() => updateMutation.mutate({ id: track.ltId, status: 'done' })}
                      >
                        Cerrar hoy
                      </Button>
                    )}
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
    </SessionGate>
  );
}
