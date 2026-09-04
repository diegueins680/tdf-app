import { useEffect, useMemo, useRef, useState } from 'react';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { Alert, Box, Button, CircularProgress, IconButton, Stack, Typography } from '@mui/material';
import { Reputation, type ReputationCategory, type ReputationPreference } from '../../api/reputation';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useSession } from '../../session/SessionContext';

type Props = {
  locale?: 'es' | 'en';
  contextKind?: string;
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

export const orderCategoriesByPreference = (
  categories: ReputationCategory[],
  preference?: ReputationPreference,
): ReputationCategory[] => {
  const saved = new Map(
    (preference?.categories ?? []).map((item) => [item.categoryId, item.position]),
  );
  return [...categories].sort((left, right) => {
    const leftPosition = saved.get(left.id);
    const rightPosition = saved.get(right.id);
    if (leftPosition !== undefined && rightPosition !== undefined) return leftPosition - rightPosition;
    if (leftPosition !== undefined) return -1;
    if (rightPosition !== undefined) return 1;
    return left.defaultPosition - right.defaultPosition || left.slug.localeCompare(right.slug);
  });
};

export default function CategoryPriorityPrototype({ locale = 'es', contextKind = 'general', onChange }: Props) {
  const { session } = useSession();
  const queryClient = useQueryClient();
  const contextualReputationEnabled = session?.featureFlags?.includes('CONTEXTUAL_REPUTATION_ENABLED') ?? false;
  const categories = useQuery({ queryKey: ['reputation-categories', locale], queryFn: () => Reputation.categories(locale) });
  const preference = useQuery({
    queryKey: ['my-reputation-preference', contextKind],
    queryFn: () => Reputation.getMyPreferences(contextKind),
    enabled: contextualReputationEnabled,
    retry: false,
  });
  const [order, setOrder] = useState<ReputationCategory[]>([]);
  const [previous, setPrevious] = useState<ReputationCategory[] | null>(null);
  const [status, setStatus] = useState('');
  const [savedRevision, setSavedRevision] = useState<number | null>(null);
  const pendingIdempotencyKey = useRef<string | null>(null);

  useEffect(() => {
    if (!categories.data || order.length > 0 || (contextualReputationEnabled && preference.isLoading)) return;
    setOrder(orderCategoriesByPreference(categories.data, preference.data));
  }, [categories.data, contextualReputationEnabled, order.length, preference.data, preference.isLoading]);
  const weights = useMemo(() => rankOrderCentroid(order.length), [order.length]);
  useEffect(() => onChange?.(order.map((category, index) => ({ categoryId: category.id, weight: weights[index] }))), [onChange, order, weights]);
  useEffect(() => {
    if (preference.data && savedRevision === null) setSavedRevision(preference.data.revision);
  }, [preference.data, savedRevision]);

  const saveDraft = useMutation({
    mutationFn: async () => {
      const idempotencyKey = pendingIdempotencyKey.current ?? crypto.randomUUID();
      pendingIdempotencyKey.current = idempotencyKey;
      return Reputation.saveMyPreferences({
        contextKind,
        expectedRevision: savedRevision ?? preference.data?.revision ?? 0,
        activate: false,
        categories: order.map((category, index) => ({
          categoryId: category.id,
          position: index + 1,
          weight: weights[index] ?? 0,
          notApplicable: false,
        })),
      }, idempotencyKey);
    },
    onMutate: () => {
      setStatus('Guardando borrador de preferencias…');
    },
    onSuccess: (saved) => {
      pendingIdempotencyKey.current = null;
      setSavedRevision(saved.revision);
      queryClient.setQueryData(['my-reputation-preference', contextKind], saved);
      setStatus('Borrador de preferencias guardado.');
    },
    onError: () => {
      setStatus('No se pudo guardar el borrador. Puedes reintentar sin perder el orden.');
    },
  });

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
      {saveDraft.isError && <Alert severity="error" sx={{ mt: 2 }}>No se pudo guardar el borrador. Reinténtalo; tu orden permanece en esta pantalla.</Alert>}
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
      <Stack direction="row" spacing={1} sx={{ mt: 1 }}>
        <Button disabled={!previous || saveDraft.isPending} onClick={() => { if (previous) { setOrder(previous); setPrevious(null); setStatus('Orden anterior restaurado.'); } }}>Deshacer</Button>
        <Button
          variant="contained"
          disabled={!contextualReputationEnabled || order.length === 0 || saveDraft.isPending}
          onClick={() => saveDraft.mutate()}
        >
          {saveDraft.isPending ? 'Guardando borrador…' : 'Guardar borrador'}
        </Button>
      </Stack>
    </Box>
  );
}
