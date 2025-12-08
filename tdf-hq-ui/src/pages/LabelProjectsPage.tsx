import { useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  IconButton,
  LinearProgress,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import SyncIcon from '@mui/icons-material/Sync';
import { Cms, type CmsContentDTO } from '../api/cms';

interface ProjectNote {
  id: string;
  text: string;
  done: boolean;
  createdAt: string;
}

const slug = 'label-projects';
const locale = 'es';

function parsePayload(content?: CmsContentDTO): ProjectNote[] {
  if (!content) return [];
  try {
    const payload = content.ccdPayload;
    if (payload && typeof payload === 'object' && Array.isArray((payload as { items?: unknown }).items)) {
      const items = (payload as { items?: unknown }).items as ProjectNote[];
      return items;
    }
    return [];
  } catch {
    return [];
  }
}

export default function LabelProjectsPage() {
  const qc = useQueryClient();
  const [input, setInput] = useState('');
  const [notes, setNotes] = useState<ProjectNote[]>([]);

  const liveQuery = useQuery({
    queryKey: ['cms-public', slug, locale],
    queryFn: () => Cms.getPublic(slug, locale),
    retry: 1,
  });

  const listQuery = useQuery({
    queryKey: ['cms-content', slug, locale],
    queryFn: () => Cms.list({ slug, locale }),
  });

  useEffect(() => {
    const fromCms = parsePayload(listQuery.data?.[0]);
    if (fromCms.length) {
      setNotes(fromCms);
    } else {
      const fromLive = parsePayload(liveQuery.data);
      setNotes(fromLive);
    }
  }, [listQuery.data, liveQuery.data]);

  const saveMutation = useMutation({
    mutationFn: (items: ProjectNote[]) =>
      Cms.create({
        cciSlug: slug,
        cciLocale: locale,
        cciTitle: 'Notas de proyectos del label',
        cciStatus: 'published',
        cciPayload: { items },
      }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['cms-content'] });
      void qc.invalidateQueries({ queryKey: ['cms-public'] });
    },
  });

  const handleAdd = () => {
    const txt = input.trim();
    if (!txt) return;
    const now = new Date().toISOString();
    setNotes((prev) => [{ id: crypto.randomUUID(), text: txt, done: false, createdAt: now }, ...prev]);
    setInput('');
  };

  const handleToggle = (id: string) => {
    setNotes((prev) => prev.map((n) => (n.id === id ? { ...n, done: !n.done } : n)));
  };

  const handleDelete = (id: string) => {
    setNotes((prev) => prev.filter((n) => n.id !== id));
  };

  const handleSave = () => {
    saveMutation.mutate(notes);
  };

  const handleReloadLive = () => {
    const live = parsePayload(liveQuery.data);
    setNotes(live);
  };

  const pending = useMemo(() => notes.filter((n) => !n.done), [notes]);
  const completed = useMemo(() => notes.filter((n) => n.done), [notes]);

  return (
    <Stack spacing={3}>
      {(listQuery.isLoading || liveQuery.isLoading) && <LinearProgress />}
      <Stack spacing={0.5}>
        <Typography variant="overline" color="text.secondary">
          Label / Proyectos
        </Typography>
        <Typography variant="h4" fontWeight={800}>
          Notas y pendientes rápidos
        </Typography>
        <Typography color="text.secondary">
          Guarda ideas, estado y pendientes de proyectos del label. Las notas se almacenan en el CMS (slug {slug}) para que el equipo las comparta.
        </Typography>
      </Stack>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              <TextField
                fullWidth
                placeholder="Agregar nota o pendiente"
                value={input}
                onChange={(e) => setInput(e.target.value)}
              />
              <Button variant="contained" onClick={handleAdd} disabled={!input.trim()}>
                Agregar
              </Button>
              <Button
                variant="outlined"
                startIcon={<SyncIcon />}
                onClick={handleReloadLive}
                disabled={liveQuery.isLoading}
              >
                Cargar desde vivo
              </Button>
            </Stack>
            <Stack direction="row" spacing={1} alignItems="center">
              <Chip label={`${notes.length} notas`} />
              <Chip label={`${pending.length} pendientes`} color="warning" variant="outlined" />
              <Chip label={`${completed.length} completadas`} color="success" variant="outlined" />
            </Stack>
            {saveMutation.isError && (
              <Alert severity="error">No se pudieron guardar las notas.</Alert>
            )}
            {saveMutation.isSuccess && <Alert severity="success">Notas guardadas en el CMS.</Alert>}
            <Stack spacing={1}>
              {notes.length === 0 && (
                <Typography color="text.secondary">No hay notas aún.</Typography>
              )}
              {notes.map((n) => (
                <Box
                  key={n.id}
                  sx={{
                    borderRadius: 2,
                    border: '1px solid',
                    borderColor: n.done ? 'success.light' : 'divider',
                    p: 1.25,
                    display: 'flex',
                    alignItems: 'center',
                    gap: 1,
                    bgcolor: n.done ? 'rgba(16,185,129,0.08)' : 'background.paper',
                  }}
                >
                  <Button
                    size="small"
                    variant={n.done ? 'contained' : 'outlined'}
                    color={n.done ? 'success' : 'primary'}
                    startIcon={<CheckCircleIcon />}
                    onClick={() => handleToggle(n.id)}
                  >
                    {n.done ? 'Hecho' : 'Marcar'}
                  </Button>
                  <Typography
                    variant="body2"
                    sx={{ flexGrow: 1, textDecoration: n.done ? 'line-through' : 'none' }}
                  >
                    {n.text}
                  </Typography>
                  <IconButton size="small" onClick={() => handleDelete(n.id)}>
                    <DeleteIcon fontSize="small" />
                  </IconButton>
                </Box>
              ))}
            </Stack>
            <Stack direction="row" spacing={1}>
              <Button variant="contained" onClick={handleSave} disabled={saveMutation.isPending}>
                Guardar en CMS
              </Button>
            </Stack>
          </Stack>
        </CardContent>
      </Card>
    </Stack>
  );
}
