import { useEffect, useMemo, useState } from 'react';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { Alert, Box, Button, CircularProgress, IconButton, Stack, Typography } from '@mui/material';
import { Reputation, type ReputationCategory } from '../../api/reputation';
import { useQuery } from '@tanstack/react-query';

type Props = {
  locale?: 'es' | 'en';
  onChange?: (items: Array<{ categoryId: string; weight: number }>) => void;
};

// Rank-order centroid: the mean cardinal allocation compatible with an
// ordinal order. The final remainder makes the displayed total exactly 100.
export const rankOrderCentroid = (count: number): number[] => {
  if (count <= 0) return [];
  const values = Array.from({ length: Math.max(0, count - 1) }, (_, index) => {
    const rank = index + 1;
    return 100 * Array.from({ length: count - rank + 1 }, (_, offset) => 1 / (rank + offset)).reduce((sum, value) => sum + value, 0) / count;
  });
  return [...values, 100 - values.reduce((sum, value) => sum + value, 0)];
};

export default function CategoryPriorityPrototype({ locale = 'es', onChange }: Props) {
  const categories = useQuery({ queryKey: ['reputation-categories', locale], queryFn: () => Reputation.categories(locale) });
  const [order, setOrder] = useState<ReputationCategory[]>([]);
  const [previous, setPrevious] = useState<ReputationCategory[] | null>(null);
  const [status, setStatus] = useState('');

  useEffect(() => {
    if (categories.data && order.length === 0) setOrder(categories.data);
  }, [categories.data, order.length]);
  const weights = useMemo(() => rankOrderCentroid(order.length), [order.length]);
  useEffect(() => onChange?.(order.map((category, index) => ({ categoryId: category.id, weight: weights[index] }))), [onChange, order, weights]);

  const move = (index: number, delta: number) => {
    const nextIndex = index + delta;
    if (nextIndex < 0 || nextIndex >= order.length) return;
    setPrevious(order);
    const next = [...order];
    [next[index], next[nextIndex]] = [next[nextIndex], next[index]];
    setOrder(next);
    setStatus(`${next[nextIndex].name} ahora tiene prioridad ${nextIndex + 1}.`);
  };

  if (categories.isLoading) return <CircularProgress size={24} aria-label="Cargando categorías" />;
  if (categories.isError) return <Alert severity="error">No se pudieron cargar las categorías. Inténtalo nuevamente.</Alert>;

  return (
    <Box component="section" aria-labelledby="category-priority-heading">
      <Typography id="category-priority-heading" variant="h5" fontWeight={800}>¿Qué es más importante para ti?</Typography>
      <Typography color="text.secondary" sx={{ mt: 0.5 }}>Tu orden crea una preferencia de compatibilidad personal. No cambia la reputación pública de nadie.</Typography>
      <Alert severity="info" sx={{ mt: 2 }}>Los porcentajes se calculan automáticamente con un método transparente y suman exactamente 100 %.</Alert>
      <Box role="status" aria-live="polite" sx={{ position: 'absolute', width: 1, height: 1, overflow: 'hidden', clip: 'rect(0 0 0 0)' }}>{status}</Box>
      <Stack component="ol" spacing={1} sx={{ listStyle: 'none', p: 0, mt: 2 }}>
        {order.map((category, index) => (
          <Box component="li" key={category.id} sx={{ border: 1, borderColor: 'divider', borderRadius: 2, p: 1.25 }}>
            <Stack direction="row" alignItems="center" spacing={1}>
              <Typography fontWeight={800} sx={{ minWidth: 24 }}>{index + 1}</Typography>
              <DragIndicatorIcon aria-hidden="true" color="action" />
              <Box sx={{ flex: 1 }}><Typography fontWeight={700}>{category.name}</Typography><Typography variant="body2" color="text.secondary">{category.description}</Typography></Box>
              <Typography fontWeight={800}>{weights[index].toFixed(1)}%</Typography>
              <IconButton aria-label={`Subir ${category.name}`} disabled={index === 0} onClick={() => move(index, -1)}>↑</IconButton>
              <IconButton aria-label={`Bajar ${category.name}`} disabled={index === order.length - 1} onClick={() => move(index, 1)}>↓</IconButton>
            </Stack>
          </Box>
        ))}
      </Stack>
      <Button disabled={!previous} onClick={() => { if (previous) { setOrder(previous); setPrevious(null); setStatus('Orden anterior restaurado.'); } }}>Deshacer</Button>
    </Box>
  );
}
