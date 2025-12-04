import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  IconButton,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import RadioButtonUncheckedIcon from '@mui/icons-material/RadioButtonUnchecked';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Label } from '../api/label';
import type { LabelTrackDTO } from '../api/types';

export default function LabelTracksPage() {
  const qc = useQueryClient();
  const [input, setInput] = useState('');
  const [note, setNote] = useState('');

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
    mutationFn: ({ id, status }: { id: string; status: string }) =>
      Label.updateTrack(id, { ltuStatus: status }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['label-tracks'] });
    },
  });

  const deleteMutation = useMutation({
    mutationFn: (id: string) => Label.deleteTrack(id),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['label-tracks'] });
    },
  });

  const tracks = useMemo(() => tracksQuery.data ?? [], [tracksQuery.data]);

  const handleAdd = () => {
    if (!input.trim()) return;
    createMutation.mutate();
  };

  const handleToggle = (track: LabelTrackDTO) => {
    const next = track.ltStatus === 'done' ? 'open' : 'done';
    updateMutation.mutate({ id: track.ltId, status: next });
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

      <Stack spacing={1.5}>
        {tracks.map((track) => {
          const isDone = track.ltStatus === 'done';
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
                  <IconButton size="small" onClick={() => deleteMutation.mutate(track.ltId)}>
                    <DeleteIcon fontSize="small" />
                  </IconButton>
                </Stack>
              </CardContent>
            </Card>
          );
        })}
      </Stack>
    </Stack>
  );
}
