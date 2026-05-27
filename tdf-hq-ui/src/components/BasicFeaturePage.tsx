import { useEffect, useMemo, useState } from 'react';
import {
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  IconButton,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import DeleteIcon from '@mui/icons-material/Delete';
import LazyPaginatedList from './LazyPaginatedList';

interface Item {
  id: string;
  text: string;
  done: boolean;
}

interface BasicFeaturePageProps {
  title: string;
  description?: string;
  storageKey: string;
}

interface PageIntroProps {
  title: string;
  description?: string;
}

function PageIntro({ title, description }: PageIntroProps) {
  return (
    <Stack spacing={0.5}>
      <Typography variant="h4" fontWeight={700}>
        {title}
      </Typography>
      <Typography variant="body1" color="text.secondary">
        {description ?? 'Captura notas y pendientes rápidos para esta sección.'}
      </Typography>
    </Stack>
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
          label="Agregar nota o pendiente"
          value={value}
          onChange={(event) => onChange(event.target.value)}
          fullWidth
          size="small"
        />
        <Button
          type="submit"
          variant="contained"
          disabled={!value.trim()}
          startIcon={<AddIcon />}
          sx={{ alignSelf: { xs: 'stretch', sm: 'auto' }, minWidth: { sm: 140 } }}
        >
          Agregar
        </Button>
      </Stack>
    </Box>
  );
}

interface NoteRowProps {
  item: Item;
  onToggle: (id: string) => void;
  onDelete: (id: string) => void;
}

function NoteRow({ item, onToggle, onDelete }: NoteRowProps) {
  return (
    <Box
      sx={{
        display: 'flex',
        alignItems: 'center',
        gap: 1,
        border: '1px solid rgba(148,163,184,0.35)',
        borderRadius: 1,
        p: 1,
      }}
    >
      <Checkbox
        checked={item.done}
        onChange={() => onToggle(item.id)}
        inputProps={{ 'aria-label': `Marcar pendiente: ${item.text}` }}
      />
      <Typography
        variant="body2"
        sx={{
          flexGrow: 1,
          minWidth: 0,
          overflowWrap: 'anywhere',
          textDecoration: item.done ? 'line-through' : 'none',
        }}
      >
        {item.text}
      </Typography>
      <IconButton
        onClick={() => onDelete(item.id)}
        size="small"
        aria-label={`Eliminar nota: ${item.text}`}
      >
        <DeleteIcon fontSize="small" />
      </IconButton>
    </Box>
  );
}

interface NotesListProps {
  items: readonly Item[];
  resetKey: string;
  onToggle: (id: string) => void;
  onDelete: (id: string) => void;
}

function NotesList({ items, resetKey, onToggle, onDelete }: NotesListProps) {
  if (items.length === 0) {
    return <Typography color="text.secondary">No hay notas aún.</Typography>;
  }

  return (
    <LazyPaginatedList
      items={items}
      pagination={{ itemLabel: 'notas', initialRowsPerPage: 10, resetKey }}
      renderItems={(visibleItems) => (
        <Stack spacing={1}>
          {visibleItems.map((item) => (
            <NoteRow key={item.id} item={item} onToggle={onToggle} onDelete={onDelete} />
          ))}
        </Stack>
      )}
    />
  );
}

interface NotesCardProps {
  input: string;
  items: readonly Item[];
  resetKey: string;
  onInputChange: (value: string) => void;
  onAdd: () => void;
  onToggle: (id: string) => void;
  onDelete: (id: string) => void;
}

function NotesCard({ input, items, resetKey, onInputChange, onAdd, onToggle, onDelete }: NotesCardProps) {
  return (
    <Card>
      <CardContent>
        <Stack spacing={2}>
          <NoteComposer value={input} onChange={onInputChange} onAdd={onAdd} />
          <NotesList items={items} resetKey={resetKey} onToggle={onToggle} onDelete={onDelete} />
        </Stack>
      </CardContent>
    </Card>
  );
}

export default function BasicFeaturePage({ title, description, storageKey }: BasicFeaturePageProps) {
  const storageKeySafe = useMemo(() => `tdf-notes:${storageKey}`, [storageKey]);
  const [items, setItems] = useState<Item[]>([]);
  const [input, setInput] = useState('');

  useEffect(() => {
    try {
      const raw = localStorage.getItem(storageKeySafe);
      if (raw) {
        const parsed = JSON.parse(raw);
        if (Array.isArray(parsed)) {
          setItems(parsed);
        }
      }
    } catch {
      // ignore
    }
  }, [storageKeySafe]);

  useEffect(() => {
    localStorage.setItem(storageKeySafe, JSON.stringify(items));
  }, [items, storageKeySafe]);

  const addItem = () => {
    const txt = input.trim();
    if (!txt) return;
    const newItem: Item = { id: crypto.randomUUID(), text: txt, done: false };
    setItems((prev) => [newItem, ...prev]);
    setInput('');
  };

  const toggleItem = (id: string) => {
    setItems((prev) => prev.map((it) => (it.id === id ? { ...it, done: !it.done } : it)));
  };

  const deleteItem = (id: string) => {
    setItems((prev) => prev.filter((it) => it.id !== id));
  };

  return (
    <Stack spacing={3}>
      <PageIntro title={title} description={description} />
      <NotesCard
        input={input}
        items={items}
        resetKey={storageKeySafe}
        onInputChange={setInput}
        onAdd={addItem}
        onToggle={toggleItem}
        onDelete={deleteItem}
      />
    </Stack>
  );
}
