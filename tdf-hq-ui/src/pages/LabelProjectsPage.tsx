import { useMemo, useState } from 'react';
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
import { alpha } from '@mui/material/styles';
import AddIcon from '@mui/icons-material/Add';
import DeleteIcon from '@mui/icons-material/Delete';
import SyncIcon from '@mui/icons-material/Sync';
import { Label } from '../api/label';
import type { LabelProjectNoteDTO } from '../api/types';
import LazyPaginatedList from '../components/LazyPaginatedList';
import PageShell from '../components/PageShell';
import { useDocumentTitle } from '../hooks/useDocumentTitle';

interface ProjectNote {
  id: string;
  text: string;
  done: boolean;
  createdAt: string;
  version: number;
}

const COMPLETED_PROJECT_NOTE_BACKGROUND_ALPHA: number = 8 / 100;

function focusNextProjectNoteDeleteAction(currentTarget: HTMLElement) {
  const notesList = currentTarget.closest('[data-project-notes-list]');
  window.requestAnimationFrame(() => {
    const nextAction = notesList?.querySelector('[data-project-note-delete-action]');
    if (nextAction instanceof HTMLElement) {
      nextAction.focus();
      return;
    }
    if (notesList instanceof HTMLElement) {
      notesList.focus();
    }
  });
}

const toProjectNote = (note: LabelProjectNoteDTO): ProjectNote => ({
  id: note.lpnId,
  text: note.lpnText,
  done: note.lpnCompleted,
  createdAt: note.lpnCreatedAt,
  version: note.lpnVersion,
});

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
      tabIndex={0}
      onClick={(event) => {
        onReload();
        event.currentTarget.focus();
      }}
      variant="outlined"
      size="small"
      startIcon={<SyncIcon />}
    >
      Actualizar
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

function SaveStatus({ isError, isSuccess }: { isError: boolean; isSuccess: boolean }) {
  if (isError) {
    return <Alert severity="error">No se pudo guardar el cambio. Actualiza e inténtalo de nuevo.</Alert>;
  }

  if (isSuccess) {
    return <Alert severity="success">Cambio guardado.</Alert>;
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
      sx={(theme) => ({
        borderRadius: 1,
        border: '1px solid',
        borderColor: note.done ? 'success.light' : 'divider',
        p: 1,
        display: 'flex',
        alignItems: 'center',
        gap: 1,
        bgcolor: note.done ? alpha(theme.palette.success.main, COMPLETED_PROJECT_NOTE_BACKGROUND_ALPHA) : 'background.paper',
      })}
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
        data-project-note-delete-action
        tabIndex={0}
        onClick={(event) => {
          event.currentTarget.focus();
          onDelete(note.id);
          focusNextProjectNoteDeleteAction(event.currentTarget);
        }}
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
        <Stack spacing={1} data-project-notes-list tabIndex={-1}>
          {visibleNotes.map((note) => (
            <ProjectNoteRow key={note.id} note={note} onToggle={onToggle} onDelete={onDelete} />
          ))}
        </Stack>
      )}
    />
  );
}

export default function LabelProjectsPage() {
  useDocumentTitle('Label / Proyectos');
  const qc = useQueryClient();
  const [input, setInput] = useState('');

  const listQuery = useQuery({
    queryKey: ['label-project-notes'],
    queryFn: Label.listProjectNotes,
  });

  const refresh = () => qc.invalidateQueries({ queryKey: ['label-project-notes'] });
  const createMutation = useMutation({
    mutationFn: Label.createProjectNote,
    onSuccess: () => {
      setInput('');
      void refresh();
    },
  });
  const updateMutation = useMutation({
    mutationFn: (note: ProjectNote) =>
      Label.updateProjectNote(note.id, {
        lpnuCompleted: !note.done,
        lpnuExpectedVersion: note.version,
      }),
    onSuccess: () => void refresh(),
  });
  const deleteMutation = useMutation({
    mutationFn: Label.deactivateProjectNote,
    onSuccess: () => void refresh(),
  });

  const notes = useMemo(() => (listQuery.data ?? []).map(toProjectNote), [listQuery.data]);

  const handleAdd = () => {
    const txt = input.trim();
    if (!txt) return;
    createMutation.mutate(txt);
  };

  const handleToggle = (id: string) => {
    const note = notes.find((candidate) => candidate.id === id);
    if (note) updateMutation.mutate(note);
  };

  const handleDelete = (id: string) => {
    deleteMutation.mutate(id);
  };

  const pending = useMemo(() => notes.filter((n) => !n.done), [notes]);
  const completed = useMemo(() => notes.filter((n) => n.done), [notes]);

  return (
    <PageShell
      title="Proyectos del label"
      subtitle="Guarda ideas, estado y pendientes como registros operativos compartidos y recuperables."
      actions={<LiveReloadAction loading={listQuery.isFetching} onReload={() => void listQuery.refetch()} />}
    >
      <Stack spacing={3}>
        {(listQuery.isLoading || createMutation.isPending || updateMutation.isPending || deleteMutation.isPending) && <LinearProgress />}
        {listQuery.isError && <Alert severity="error">No se pudieron cargar las notas.</Alert>}

        <Card>
          <CardContent>
            <Stack spacing={2}>
              <NoteComposer value={input} onChange={setInput} onAdd={handleAdd} />
              <NoteStats total={notes.length} pendingCount={pending.length} completedCount={completed.length} />
              <SaveStatus
                isError={createMutation.isError || updateMutation.isError || deleteMutation.isError}
                isSuccess={createMutation.isSuccess || updateMutation.isSuccess || deleteMutation.isSuccess}
              />
              <ProjectNotesList notes={notes} onToggle={handleToggle} onDelete={handleDelete} />
            </Stack>
          </CardContent>
        </Card>
      </Stack>
    </PageShell>
  );
}
