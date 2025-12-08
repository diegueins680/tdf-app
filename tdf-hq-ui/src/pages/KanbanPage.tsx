import { useEffect, useMemo, useState, type SyntheticEvent } from 'react';
import { DragDropContext, Droppable, Draggable, type DropResult } from '@hello-pangea/dnd';
import {
  Alert,
  Box,
  CircularProgress,
  Paper,
  Stack,
  Tab,
  Tabs,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Pipelines } from '../api/pipelines';
import type { PipelineCardDTO } from '../api/types';

const PIPELINE_TYPES = [
  { slug: 'mixing', label: 'Mixing' },
  { slug: 'mastering', label: 'Mastering' },
  { slug: 'recording', label: 'Recording' },
  { slug: 'classes', label: 'Academy' },
];

type ColumnsState = Record<string, string[]>;
const DEFAULT_PIPELINE_TYPE = PIPELINE_TYPES[0]?.slug ?? 'mixing';

export default function KanbanPage() {
  const [activeType, setActiveType] = useState<string>(DEFAULT_PIPELINE_TYPE);
  const [columns, setColumns] = useState<ColumnsState>({});
  const [cards, setCards] = useState<Record<string, PipelineCardDTO>>({});
  const qc = useQueryClient();

  const stagesQuery = useQuery<string[], Error>({
    queryKey: ['pipelines', activeType, 'stages'],
    queryFn: () => Pipelines.stages(activeType),
  });

  const cardsQuery = useQuery<PipelineCardDTO[], Error>({
    queryKey: ['pipelines', activeType, 'cards'],
    queryFn: () => Pipelines.list(activeType),
    enabled: !!stagesQuery.data?.length,
  });

  useEffect(() => {
    if (!stagesQuery.data) return;
    const initialColumns: ColumnsState = {};
    stagesQuery.data.forEach((stage) => {
      initialColumns[stage] = [];
    });
    if (cardsQuery.data) {
      const map: Record<string, PipelineCardDTO> = {};
      const fallbackStage = stagesQuery.data[0];
      cardsQuery.data.forEach((card) => {
        const normalizedId = (card.pcId ?? '').toString().trim();
        if (!normalizedId) {
          return;
        }
        map[normalizedId] = card;
        const stageKey = initialColumns[card.pcStage] ? card.pcStage : fallbackStage;
        if (!stageKey) {
          return;
        }
        initialColumns[stageKey] ??= [];
        const columnEntries = initialColumns[stageKey];
        if (!columnEntries) {
          return;
        }
        if (!columnEntries.includes(normalizedId)) {
          columnEntries.push(normalizedId);
        }
      });
      Object.keys(initialColumns).forEach((stage) => {
        const stageEntries = initialColumns[stage];
        if (!stageEntries) {
          return;
        }
        initialColumns[stage] = stageEntries.sort((a, b) => {
          const cardA = map[a];
          const cardB = map[b];
          return (cardA?.pcSortOrder ?? 0) - (cardB?.pcSortOrder ?? 0);
        });
      });
      setCards(map);
    } else {
      setCards({});
    }
    setColumns(initialColumns);
  }, [stagesQuery.data, cardsQuery.data]);

  const updateMutation = useMutation<void, Error, { cardId: string; stage: string; sortOrder: number }>({
    mutationFn: ({ cardId, stage, sortOrder }: { cardId: string; stage: string; sortOrder: number }) =>
      Pipelines.update(activeType, cardId, { pcuStage: stage, pcuSortOrder: sortOrder }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['pipelines', activeType, 'cards'] });
    },
  });

  const onDragEnd = (result: DropResult) => {
    const { destination, source, draggableId } = result;
    if (!destination) return;
    if (
      destination.droppableId === source.droppableId &&
      destination.index === source.index
    ) {
      return;
    }

    const startStage = source.droppableId;
    const finishStage = destination.droppableId;
    const startColumn = columns[startStage];
    const finishColumn = columns[finishStage];
    if (!startColumn || !finishColumn) return;

    const startIds = Array.from(startColumn);
    startIds.splice(source.index, 1);
    const finishIds = Array.from(finishColumn);
    finishIds.splice(destination.index, 0, draggableId);

    setColumns((prev) => ({
      ...prev,
      [startStage]: startIds,
      [finishStage]: finishIds,
    }));

    setCards((prev) => {
      const next = { ...prev };
      const assignSortOrder = (ids: string[]) => {
        ids.forEach((id, idx) => {
          if (next[id]) {
            next[id] = { ...next[id], pcSortOrder: idx };
          }
        });
      };
      assignSortOrder(startIds);
      assignSortOrder(finishIds);
      if (next[draggableId]) {
        next[draggableId] = { ...next[draggableId], pcStage: finishStage, pcSortOrder: destination.index };
      }
      return next;
    });

    updateMutation.mutate({ cardId: draggableId, stage: finishStage, sortOrder: destination.index });
  };

  const columnOrder = useMemo(() => stagesQuery.data ?? [], [stagesQuery.data]);
  const loadError = stagesQuery.error ?? cardsQuery.error;
  const isLoading = stagesQuery.isLoading || cardsQuery.isLoading;
  const activeLabel = PIPELINE_TYPES.find((type) => type.slug === activeType)?.label ?? 'Pipeline';

  return (
    <Stack gap={2}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems={{ md: 'center' }} spacing={2}>
        <Typography variant="h5" fontWeight={600}>
          Pipelines — {activeLabel}
        </Typography>
        <Tabs
          value={activeType}
          onChange={(_: SyntheticEvent, value: string) => setActiveType(value)}
          variant="scrollable"
          allowScrollButtonsMobile
        >
          {PIPELINE_TYPES.map((type) => (
            <Tab key={type.slug} value={type.slug} label={type.label} />
          ))}
        </Tabs>
      </Stack>

      {loadError && <Alert severity="error">{loadError.message}</Alert>}
      {updateMutation.isError && updateMutation.error && (
        <Alert severity="warning">
          {updateMutation.error.message}
        </Alert>
      )}

      {isLoading ? (
        <Box display="flex" justifyContent="center" py={6}>
          <CircularProgress />
        </Box>
      ) : columnOrder.length === 0 ? (
        <Alert severity="info">No hay etapas definidas para este pipeline.</Alert>
      ) : (
        <DragDropContext onDragEnd={onDragEnd}>
          <Stack direction="row" gap={2} sx={{ overflowX: 'auto', pb: 2 }}>
            {columnOrder.map((stage) => {
              const cardIds = (columns[stage] ?? []).filter((id) => typeof id === 'string' && id.trim().length > 0);
              return (
                <Droppable droppableId={stage} key={stage}>
                  {(provided, snapshot) => (
                    <Paper
                      ref={provided.innerRef}
                      {...provided.droppableProps}
                      sx={{
                        p: 2,
                        width: 280,
                        flex: '0 0 auto',
                        bgcolor: snapshot.isDraggingOver ? 'rgba(148,163,184,0.15)' : 'background.paper',
                      }}
                    >
                      <Typography variant="subtitle1" sx={{ mb: 1, fontWeight: 700 }}>
                        {stage}
                      </Typography>
                      <Stack gap={1}>
                        {cardIds.map((cardId, idx) => {
                          const safeId = cardId?.toString().trim();
                          if (!safeId) return null;
                          const card = cards[safeId];
                          if (!card) return null;
                          return (
                            <Draggable draggableId={safeId} index={idx} key={`${stage}-${safeId}`}>
                              {(prov, snap) => (
                                <Box
                                  ref={prov.innerRef}
                                  {...prov.draggableProps}
                                  {...prov.dragHandleProps}
                                  sx={{
                                    p: 1.5,
                                    borderRadius: 2,
                                    border: '1px solid',
                                    borderColor: snap.isDragging ? 'primary.main' : 'divider',
                                    bgcolor: 'background.paper',
                                    boxShadow: snap.isDragging ? 3 : 0,
                                  }}
                                >
                                  <Typography fontWeight={600}>{card.pcTitle}</Typography>
                                  {card.pcArtist && (
                                    <Typography variant="body2" color="text.secondary">
                                      {card.pcArtist}
                                    </Typography>
                                  )}
                                </Box>
                              )}
                            </Draggable>
                          );
                        })}
                        {provided.placeholder}
                      </Stack>
                    </Paper>
                  )}
                </Droppable>
              );
            })}
          </Stack>
        </DragDropContext>
      )}
    </Stack>
  );
}
