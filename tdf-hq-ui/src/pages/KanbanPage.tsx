import { useEffect, useMemo, useState, type SyntheticEvent } from 'react';
import { DragDropContext, Droppable, Draggable, type DropResult } from '@hello-pangea/dnd';
import {
  Alert,
  Box,
  Paper,
  Stack,
  Tab,
  Tabs,
  Typography,
} from '@mui/material';
import PageShell, { SkeletonCards, EmptyState } from '../components/PageShell';
import ViewColumnIcon from '@mui/icons-material/ViewColumn';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useTranslation } from 'react-i18next';
import { Pipelines } from '../api/pipelines';
import type { PipelineCardDTO } from '../api/types';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';

type ColumnsState = Record<string, string[]>;

export default function KanbanPage() {
  const { t } = useTranslation();
  const { locale } = useLocalePreferences();
  const [activeWorkflowId, setActiveWorkflowId] = useState<string>('');
  const [columns, setColumns] = useState<ColumnsState>({});
  const [cards, setCards] = useState<Record<string, PipelineCardDTO>>({});
  const qc = useQueryClient();

  const definitionsQuery = useQuery({
    queryKey: ['pipelines', 'definitions'],
    queryFn: Pipelines.definitions,
  });

  useEffect(() => {
    const definitions = definitionsQuery.data ?? [];
    if (definitions.length === 0) return;
    if (!definitions.some((definition) => definition.workflowId === activeWorkflowId)) {
      setActiveWorkflowId(definitions[0]?.workflowId ?? '');
    }
  }, [activeWorkflowId, definitionsQuery.data]);

  const stagesQuery = useQuery({
    queryKey: ['pipelines', activeWorkflowId, 'stages'],
    queryFn: () => Pipelines.stages(activeWorkflowId),
    enabled: activeWorkflowId.length > 0,
  });

  const cardsQuery = useQuery<PipelineCardDTO[], Error>({
    queryKey: ['pipelines', activeWorkflowId, 'cards'],
    queryFn: () => Pipelines.list(activeWorkflowId),
    enabled: !!stagesQuery.data?.length,
  });

  useEffect(() => {
    if (!stagesQuery.data) return;
    const initialColumns: ColumnsState = {};
    stagesQuery.data.forEach((stage) => {
      initialColumns[stage.id] = [];
    });
    if (cardsQuery.data) {
      const map: Record<string, PipelineCardDTO> = {};
      const fallbackStage = stagesQuery.data[0];
      cardsQuery.data.forEach((card) => {
        const normalizedId = (card.id ?? '').toString().trim();
        if (!normalizedId) {
          return;
        }
        map[normalizedId] = card;
        const stageKey = initialColumns[card.workflowStateId] ? card.workflowStateId : fallbackStage?.id;
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
          return (cardA?.sortOrder ?? 0) - (cardB?.sortOrder ?? 0);
        });
      });
      setCards(map);
    } else {
      setCards({});
    }
    setColumns(initialColumns);
  }, [stagesQuery.data, cardsQuery.data]);

  const updateMutation = useMutation<PipelineCardDTO, Error, { cardId: string; workflowStateId: string; sortOrder: number }>({
    mutationFn: ({ cardId, workflowStateId, sortOrder }) =>
      Pipelines.update(activeWorkflowId, cardId, { workflowStateId, sortOrder }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['pipelines', activeWorkflowId, 'cards'] });
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
            next[id] = { ...next[id], sortOrder: idx };
          }
        });
      };
      assignSortOrder(startIds);
      assignSortOrder(finishIds);
      if (next[draggableId]) {
        next[draggableId] = { ...next[draggableId], workflowStateId: finishStage, sortOrder: destination.index };
      }
      return next;
    });

    updateMutation.mutate({ cardId: draggableId, workflowStateId: finishStage, sortOrder: destination.index });
  };

  const columnOrder = useMemo(() => stagesQuery.data ?? [], [stagesQuery.data]);
  const loadError = definitionsQuery.error ?? stagesQuery.error ?? cardsQuery.error;
  const isLoading = definitionsQuery.isLoading || stagesQuery.isLoading || cardsQuery.isLoading;
  const activeDefinition = definitionsQuery.data?.find((definition) => definition.workflowId === activeWorkflowId);
  const activeLabel = (locale.toLowerCase().startsWith('en')
    ? activeDefinition?.nameEn
    : activeDefinition?.nameEs) ?? 'pipeline';

  return (
    <PageShell
      title={t('pipelines.title')}
      subtitle={t('pipelines.subtitle', { workflow: activeLabel.toLowerCase() })}
    >
    <Stack gap={3}>
      <Tabs
          value={activeWorkflowId}
          onChange={(_: SyntheticEvent, value: string) => setActiveWorkflowId(value)}
          variant="scrollable"
          allowScrollButtonsMobile
          aria-label={t('pipelines.workflowTabs')}
        >
          {(definitionsQuery.data ?? []).map((definition) => (
            <Tab
              key={definition.workflowId}
              value={definition.workflowId}
              label={locale.toLowerCase().startsWith('en') ? definition.nameEn : definition.nameEs}
            />
          ))}
        </Tabs>

      {loadError && <Alert severity="error">{loadError.message}</Alert>}
      {updateMutation.isError && updateMutation.error && (
        <Alert severity="warning">
          {updateMutation.error.message}
        </Alert>
      )}

      {isLoading ? (
        <SkeletonCards count={4} />
      ) : columnOrder.length === 0 ? (
        <Alert severity="info">{t('pipelines.empty')}</Alert>
      ) : (
        <DragDropContext onDragEnd={onDragEnd}>
          <Typography variant="body2" color="text.secondary">
            {t('pipelines.keyboardHelp')}
          </Typography>
          <Stack direction="row" gap={2} sx={{ overflowX: 'auto', pb: 2 }}>
            {columnOrder.map((stage) => {
              const cardIds = (columns[stage.id] ?? []).filter((id) => typeof id === 'string' && id.trim().length > 0);
              return (
                <Droppable droppableId={stage.id} key={stage.id}>
                  {(provided, snapshot) => (
                    <Paper
                      ref={provided.innerRef}
                      {...provided.droppableProps}
                      component="section"
                      aria-label={t('pipelines.stageLabel', {
                        stage: locale.toLowerCase().startsWith('en') ? stage.nameEn : stage.nameEs,
                      })}
                      sx={{
                        p: 2,
                        width: 280,
                        flex: '0 0 auto',
                        bgcolor: snapshot.isDraggingOver ? 'rgba(148,163,184,0.15)' : 'background.paper',
                      }}
                    >
                      <Typography variant="subtitle1" sx={{ mb: 1, fontWeight: 700 }}>
                        {locale.toLowerCase().startsWith('en') ? stage.nameEn : stage.nameEs}
                      </Typography>
                      <Stack gap={1}>
                        {cardIds.map((cardId, idx) => {
                          const safeId = cardId?.toString().trim();
                          if (!safeId) return null;
                          const card = cards[safeId];
                          if (!card) return null;
                          return (
                            <Draggable draggableId={safeId} index={idx} key={`${stage.id}-${safeId}`}>
                              {(prov, snap) => (
                                <Box
                                  ref={prov.innerRef}
                                  {...prov.draggableProps}
                                  {...prov.dragHandleProps}
                                  aria-label={t('pipelines.cardMoveLabel', {
                                    title: card.title,
                                    stage: locale.toLowerCase().startsWith('en') ? stage.nameEn : stage.nameEs,
                                  })}
                                  sx={{
                                    p: 1.5,
                                    minHeight: 44,
                                    borderRadius: 2,
                                    border: '1px solid',
                                    borderColor: snap.isDragging ? 'primary.main' : 'divider',
                                    bgcolor: 'background.paper',
                                    boxShadow: snap.isDragging ? 3 : 0,
                                  }}
                                >
                                  <Typography fontWeight={600}>{card.title}</Typography>
                                  {card.artist && (
                                    <Typography variant="body2" color="text.secondary">
                                      {card.artist}
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
    </PageShell>
  );
}
