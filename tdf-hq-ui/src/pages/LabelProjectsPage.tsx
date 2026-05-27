import { useEffect, useMemo, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  IconButton,
  LinearProgress,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import DeleteIcon from '@mui/icons-material/Delete';
import SaveIcon from '@mui/icons-material/Save';
import SyncIcon from '@mui/icons-material/Sync';
import { Cms, type CmsContentDTO } from '../api/cms';
import LazyPaginatedList from '../components/LazyPaginatedList';
import PageShell from '../components/PageShell';

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

const fingerprintNotes = (items: readonly ProjectNote[]) => JSON.stringify(items);

const formatCount = (count: number, singular: string, plural: string) =>
  `${count} ${count === 1 ? singular : plural}`;

interface LiveReloadActionProps {
  loading: boolean;
  onReload: () => void;
}

function LiveReloadAction({ loading, onReload }: LiveReloadActionProps) {
  return (
    <Button
      disabled={loading}
      onClick={(event) => {
        onReload();
        event.currentTarget.focus();
      }}
      variant="outlined"
      size="small"
      startIcon={<SyncIcon />}
    >
      Cargar desde vivo
    </Button>
  );
}

interface NoteComposerProps {
  value: string;
  onChange: (value: string) => void;
  onAdd: () => void;
}

function NoteComposer({ value, onChange, onAdd }: NoteComposerProps) {
  return (
    <Box
      component="form"
      onSubmit={(event) => {
        event.preventDefault();
        onAdd();
        event.currentTarget.querySelector('input')?.focus();
      }}
    >
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
        <TextField
          fullWidth
          size="small"
          label="Nota o pendiente"
          placeholder="Idea, estado o pendiente"
          value={value}
          onChange={(event) => onChange(event.target.value)}
        />
        <Button
          type="submit"
          variant="contained"
          disabled={!value.trim()}
          startIcon={<AddIcon />}
          sx={{ alignSelf: { xs: 'stretch', sm: 'auto' } }}
        >
          Agregar
        </Button>
      </Stack>
    </Box>
  );
}

interface NoteStatsProps {
  total: number;
  pendingCount: number;
  completedCount: number;
}

function NoteStats({ total, pendingCount, completedCount }: NoteStatsProps) {
  return (
    <Typography variant="body2" color="text.secondary" sx={{ fontVariantNumeric: 'tabular-nums' }}>
      {formatCount(total, 'nota', 'notas')} · {formatCount(pendingCount, 'pendiente', 'pendientes')} ·{' '}
      {formatCount(completedCount, 'completada', 'completadas')}
    </Typography>
  );
}

interface SaveStatusProps {
  isError: boolean;
  isSuccess: boolean;
}

function SaveStatus({ isError, isSuccess }: SaveStatusProps) {
  if (isError) {
    return <Alert severity="error">No se pudieron guardar las notas.</Alert>;
  }

  if (isSuccess) {
    return <Alert severity="success">Notas guardadas en el CMS.</Alert>;
  }

  return null;
}

interface ProjectNoteRowProps {
  note: ProjectNote;
  onToggle: (id: string) => void;
  onDelete: (id: string) => void;
}

function ProjectNoteRow({ note, onToggle, onDelete }: ProjectNoteRowProps) {
  return (
    <Box
      sx={{
        borderRadius: 1,
        border: '1px solid',
        borderColor: note.done ? 'success.light' : 'divider',
        p: 1,
        display: 'flex',
        alignItems: 'center',
        gap: 1,
        bgcolor: note.done ? 'rgba(16,185,129,0.08)' : 'background.paper',
      }}
    >
      <Checkbox
        checked={note.done}
        color="success"
        onChange={() => onToggle(note.id)}
        inputProps={{ 'aria-label': `Marcar pendiente: ${note.text}` }}
      />
      <Typography
        variant="body2"
        sx={{
          flexGrow: 1,
          minWidth: 0,
          overflowWrap: 'anywhere',
          textDecoration: note.done ? 'line-through' : 'none',
        }}
      >
        {note.text}
      </Typography>
      <IconButton
        onClick={() => onDelete(note.id)}
        size="small"
        aria-label={`Eliminar nota: ${note.text}`}
      >
        <DeleteIcon fontSize="small" />
      </IconButton>
    </Box>
  );
}

interface ProjectNotesListProps {
  notes: readonly ProjectNote[];
  onToggle: (id: string) => void;
  onDelete: (id: string) => void;
}

function ProjectNotesList({ notes, onToggle, onDelete }: ProjectNotesListProps) {
  if (notes.length === 0) {
    return <Typography color="text.secondary">No hay notas aún.</Typography>;
  }

  return (
    <LazyPaginatedList
      items={notes}
      pagination={{ itemLabel: 'notas', initialRowsPerPage: 10 }}
      renderItems={(visibleNotes) => (
        <Stack spacing={1}>
          {visibleNotes.map((note) => (
            <ProjectNoteRow key={note.id} note={note} onToggle={onToggle} onDelete={onDelete} />
          ))}
        </Stack>
      )}
    />
  );
}

interface SaveActionsProps {
  saving: boolean;
  onSave: () => void;
}

function SaveActions({ saving, onSave }: SaveActionsProps) {
  return (
    <Stack direction="row" justifyContent="flex-end">
      <Button
        disabled={saving}
        onClick={(event) => {
          onSave();
          event.currentTarget.focus();
        }}
        variant="contained"
        startIcon={<SaveIcon />}
      >
        Guardar en CMS
      </Button>
    </Stack>
  );
}

export default function LabelProjectsPage() {
  const qc = useQueryClient();
  const [input, setInput] = useState('');
  const [notes, setNotes] = useState<ProjectNote[]>([]);
  const hydratedNotesFingerprintRef = useRef<string | null>(null);

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
    const nextNotes = fromCms.length ? fromCms : parsePayload(liveQuery.data);
    const nextFingerprint = fingerprintNotes(nextNotes);

    setNotes((currentNotes) => {
      const currentFingerprint = fingerprintNotes(currentNotes);
      const lastHydratedFingerprint = hydratedNotesFingerprintRef.current;

      if (currentFingerprint === nextFingerprint) {
        hydratedNotesFingerprintRef.current = nextFingerprint;
        return currentNotes;
      }
      if (lastHydratedFingerprint === null || currentFingerprint === lastHydratedFingerprint) {
        hydratedNotesFingerprintRef.current = nextFingerprint;
        return nextNotes;
      }
      return currentNotes;
    });
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
    hydratedNotesFingerprintRef.current = fingerprintNotes(live);
    setNotes(live);
  };

  const pending = useMemo(() => notes.filter((n) => !n.done), [notes]);
  const completed = useMemo(() => notes.filter((n) => n.done), [notes]);

  return (
    <PageShell
      title="Proyectos del label"
      subtitle="Guarda ideas, estado y pendientes de proyectos del label. Las notas se almacenan en el CMS para que el equipo las comparta."
      actions={<LiveReloadAction loading={liveQuery.isLoading} onReload={handleReloadLive} />}
    >
      <Stack spacing={3}>
        {(listQuery.isLoading || liveQuery.isLoading) && <LinearProgress />}

        <Card>
          <CardContent>
            <Stack spacing={2}>
              <NoteComposer value={input} onChange={setInput} onAdd={handleAdd} />
              <NoteStats total={notes.length} pendingCount={pending.length} completedCount={completed.length} />
              <SaveStatus isError={saveMutation.isError} isSuccess={saveMutation.isSuccess} />
              <ProjectNotesList notes={notes} onToggle={handleToggle} onDelete={handleDelete} />
              <SaveActions saving={saveMutation.isPending} onSave={handleSave} />
            </Stack>
          </CardContent>
        </Card>
      </Stack>
    </PageShell>
  );
}
