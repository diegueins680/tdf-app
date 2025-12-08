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
import DeleteIcon from '@mui/icons-material/Delete';

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
      <Stack spacing={0.5}>
        <Typography variant="h4" fontWeight={700}>{title}</Typography>
        <Typography variant="body1" color="text.secondary">
          {description ?? 'Captura notas y pendientes rápidos para esta sección.'}
        </Typography>
      </Stack>

      <Card>
        <CardContent>
          <Stack spacing={2}>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              <TextField
                label="Agregar nota o pendiente"
                value={input}
                onChange={(e) => setInput(e.target.value)}
                fullWidth
                size="small"
              />
              <Button variant="contained" onClick={addItem} sx={{ minWidth: 140 }}>
                Agregar
              </Button>
            </Stack>
            <Stack spacing={1}>
              {items.length === 0 && (
                <Typography color="text.secondary">No hay notas aún.</Typography>
              )}
              {items.map((item) => (
                <Box
                  key={item.id}
                  sx={{
                    display: 'flex',
                    alignItems: 'center',
                    gap: 1,
                    border: '1px solid rgba(148,163,184,0.35)',
                    borderRadius: 2,
                    p: 1,
                  }}
                >
                  <Checkbox checked={item.done} onChange={() => toggleItem(item.id)} />
                  <Typography
                    variant="body2"
                    sx={{ flexGrow: 1, textDecoration: item.done ? 'line-through' : 'none' }}
                  >
                    {item.text}
                  </Typography>
                  <IconButton size="small" onClick={() => deleteItem(item.id)}>
                    <DeleteIcon fontSize="small" />
                  </IconButton>
                </Box>
              ))}
            </Stack>
          </Stack>
        </CardContent>
      </Card>
    </Stack>
  );
}
