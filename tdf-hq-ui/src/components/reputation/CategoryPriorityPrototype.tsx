import { useEffect, useMemo, useRef, useState } from 'react';
import { DragDropContext, Draggable, Droppable, type DropResult } from '@hello-pangea/dnd';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import { Alert, Box, Button, CircularProgress, IconButton, Stack, Typography } from '@mui/material';
import { Reputation, type ReputationCategory, type ReputationPreference } from '../../api/reputation';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useSession } from '../../session/SessionContext';

interface Props {
  locale?: 'es' | 'en';
  contextKind?: string;
  onChange?: (items: { categoryId: string; weight: number }[]) => void;
}

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

export const reorderCategories = (
  categories: ReputationCategory[],
  sourceIndex: number,
  destinationIndex: number,
): ReputationCategory[] => {
  if (sourceIndex === destinationIndex || sourceIndex < 0 || destinationIndex < 0
    || sourceIndex >= categories.length || destinationIndex >= categories.length) return categories;
  const next = [...categories];
  const [moved] = next.splice(sourceIndex, 1);
  if (!moved) return categories;
  next.splice(destinationIndex, 0, moved);
  return next;
};

export default function CategoryPriorityPrototype({ locale = 'es', contextKind = 'general', onChange }: Props) {
  const { session } = useSession();
  const queryClient = useQueryClient();
  const contextualReputationEnabled = session?.featureFlags?.includes('CONTEXTUAL_REPUTATION_ENABLED') ?? false;
  const categories = useQuery({
    queryKey: ['reputation-categories', locale],
    queryFn: () => Reputation.categories(locale),
    enabled: contextualReputationEnabled,
  });
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
  const copy = locale === 'en'
    ? {
      loading: 'Loading categories',
      loadError: 'Categories could not be loaded. Please try again.',
      heading: 'What matters most to you?',
      explanation: 'Your order creates a personal compatibility preference. It does not change anyone’s public reputation.',
      method: 'Percentages are calculated automatically using a transparent method and add up to exactly 100%.',
      saveError: 'The draft could not be saved. Try again; your order remains on this screen.',
      saving: 'Saving preference draft…',
      saved: 'Preference draft saved.',
      retry: 'The draft could not be saved. You can retry without losing the order.',
      restored: 'Previous order restored.',
      undo: 'Undo',
      save: 'Save draft',
      moveUp: 'Move up',
      moveDown: 'Move down',
      unavailable: 'Contextual reputation is not enabled for this account yet.',
      reorderHelp: 'Drag a category or use the move buttons to set its priority.',
      priority: (name: string, position: number) => `${name} is now priority ${position}.`,
    }
    : {
      loading: 'Cargando categorías',
      loadError: 'No se pudieron cargar las categorías. Inténtalo nuevamente.',
      heading: '¿Qué es más importante para ti?',
      explanation: 'Tu orden crea una preferencia de compatibilidad personal. No cambia la reputación pública de nadie.',
      method: 'Los porcentajes se calculan automáticamente con un método transparente y suman exactamente 100 %.',
      saveError: 'No se pudo guardar el borrador. Reinténtalo; tu orden permanece en esta pantalla.',
      saving: 'Guardando borrador de preferencias…',
      saved: 'Borrador de preferencias guardado.',
      retry: 'No se pudo guardar el borrador. Puedes reintentar sin perder el orden.',
      restored: 'Orden anterior restaurado.',
      undo: 'Deshacer',
      save: 'Guardar borrador',
      moveUp: 'Subir',
      moveDown: 'Bajar',
      unavailable: 'La reputación contextual todavía no está habilitada para esta cuenta.',
      reorderHelp: 'Arrastra una categoría o usa los botones para definir su prioridad.',
      priority: (name: string, position: number) => `${name} ahora tiene prioridad ${position}.`,
    };

  useEffect(() => {
    if (!categories.data || order.length > 0 || (contextualReputationEnabled && preference.isLoading)) return;
    setOrder(orderCategoriesByPreference(categories.data, preference.data));
  }, [categories.data, contextualReputationEnabled, order.length, preference.data, preference.isLoading]);
  const weights = useMemo(() => rankOrderCentroid(order.length), [order.length]);
  useEffect(() => onChange?.(order.map((category, index) => ({ categoryId: category.id, weight: weights[index] ?? 0 }))), [onChange, order, weights]);
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
      setStatus(copy.saving);
    },
    onSuccess: (saved) => {
      pendingIdempotencyKey.current = null;
      setSavedRevision(saved.revision);
      queryClient.setQueryData(['my-reputation-preference', contextKind], saved);
      setStatus(copy.saved);
    },
    onError: () => {
      setStatus(copy.retry);
    },
  });

  const move = (index: number, delta: number) => {
    const nextIndex = index + delta;
    if (nextIndex < 0 || nextIndex >= order.length) return;
    setPrevious(order);
    const next = reorderCategories(order, index, nextIndex);
    setOrder(next);
    const moved = next[nextIndex];
    if (moved) setStatus(copy.priority(moved.name, nextIndex + 1));
  };

  const onDragEnd = ({ source, destination }: DropResult) => {
    if (!destination || source.index === destination.index) return;
    const next = reorderCategories(order, source.index, destination.index);
    setPrevious(order);
    setOrder(next);
    const moved = next[destination.index];
    if (moved) setStatus(copy.priority(moved.name, destination.index + 1));
  };

  if (!contextualReputationEnabled) return <Alert severity="info">{copy.unavailable}</Alert>;
  if (categories.isLoading) return <CircularProgress size={24} aria-label={copy.loading} />;
  if (categories.isError) return <Alert severity="error">{copy.loadError}</Alert>;

  return (
    <Box component="section" aria-labelledby="category-priority-heading">
      <Typography id="category-priority-heading" variant="h5" fontWeight={800}>{copy.heading}</Typography>
      <Typography color="text.secondary" sx={{ mt: 0.5 }}>{copy.explanation}</Typography>
      <Alert severity="info" sx={{ mt: 2 }}>{copy.method}</Alert>
      {saveDraft.isError && <Alert severity="error" sx={{ mt: 2 }}>{copy.saveError}</Alert>}
      <Box role="status" aria-live="polite" sx={{ position: 'absolute', width: 1, height: 1, overflow: 'hidden', clip: 'rect(0 0 0 0)' }}>{status}</Box>
      <Typography variant="body2" color="text.secondary" sx={{ mt: 2 }}>{copy.reorderHelp}</Typography>
      <DragDropContext onDragEnd={onDragEnd}>
        <Droppable droppableId="category-priority">
          {(provided) => (
            <Stack
              ref={provided.innerRef}
              {...provided.droppableProps}
              component="ol"
              spacing={1}
              sx={{ listStyle: 'none', p: 0, mt: 1 }}
            >
              {order.map((category, index) => (
                <Draggable draggableId={category.id} index={index} key={category.id}>
                  {(dragProvided, snapshot) => (
                    <Box
                      ref={dragProvided.innerRef}
                      {...dragProvided.draggableProps}
                      component="li"
                      sx={{
                        border: 1,
                        borderColor: snapshot.isDragging ? 'primary.main' : 'divider',
                        borderRadius: 2,
                        p: 1.25,
                        bgcolor: 'background.paper',
                        boxShadow: snapshot.isDragging ? 3 : 0,
                      }}
                    >
                      <Stack direction="row" alignItems="center" spacing={1}>
                        <Typography fontWeight={800} sx={{ minWidth: 24 }}>{index + 1}</Typography>
                        <Box {...dragProvided.dragHandleProps} aria-label={`${copy.reorderHelp} ${category.name}`} sx={{ display: 'inline-flex', cursor: 'grab' }}>
                          <DragIndicatorIcon aria-hidden="true" color="action" />
                        </Box>
                        <Box sx={{ flex: 1 }}><Typography fontWeight={700}>{category.name}</Typography><Typography variant="body2" color="text.secondary">{category.description}</Typography></Box>
                        <Typography fontWeight={800}>{(weights[index] ?? 0).toFixed(1)}%</Typography>
                        <IconButton aria-label={`${copy.moveUp} ${category.name}`} disabled={index === 0} onClick={() => move(index, -1)}>↑</IconButton>
                        <IconButton aria-label={`${copy.moveDown} ${category.name}`} disabled={index === order.length - 1} onClick={() => move(index, 1)}>↓</IconButton>
                      </Stack>
                    </Box>
                  )}
                </Draggable>
              ))}
              {provided.placeholder}
            </Stack>
          )}
        </Droppable>
      </DragDropContext>
      <Stack direction="row" spacing={1} sx={{ mt: 1 }}>
        <Button disabled={!previous || saveDraft.isPending} onClick={() => { if (previous) { setOrder(previous); setPrevious(null); setStatus(copy.restored); } }}>{copy.undo}</Button>
        <Button
          variant="contained"
          disabled={!contextualReputationEnabled || order.length === 0 || saveDraft.isPending}
          onClick={() => saveDraft.mutate()}
        >
          {saveDraft.isPending ? copy.saving : copy.save}
        </Button>
      </Stack>
    </Box>
  );
}
