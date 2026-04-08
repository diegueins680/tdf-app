
import { useMemo } from 'react';
import { DragDropContext, Droppable, Draggable, DropResult } from '@hello-pangea/dnd';
import { Paper, Stack, Typography, Box, Chip, Divider } from '@mui/material';
import { updateStage } from '../api/pipelines';
import {
  MIXING_STAGES,
  MASTERING_STAGES,
  findPipelineCard,
  updatePipelineCardStage,
  usePipelineCards,
  type PipelineBoardCard,
} from '../features/pipelines/pipelineStore';

function Column({ id, title, jobs }: { id: string; title: string; jobs: PipelineBoardCard[] }) {
  return (
    <Paper
      sx={{ p: 1, minWidth: 280, height: '70vh', display: 'flex', flexDirection: 'column' }}
      variant="outlined"
    >
      <Typography variant="subtitle1" sx={{ mb: 1, fontWeight: 700 }}>
        {title} <Chip size="small" label={jobs.length} sx={{ ml: 1 }} />
      </Typography>
      <Droppable droppableId={id}>
        {(provided) => (
          <Stack
            ref={provided.innerRef}
            {...provided.droppableProps}
            gap={1}
            sx={{ overflowY: 'auto', pr: 0.5, flexGrow: 1 }}
          >
            {jobs.map((j, idx) => (
              <Draggable draggableId={j.id} index={idx} key={j.id}>
                {(prov) => (
                  <Paper ref={prov.innerRef} {...prov.draggableProps} {...prov.dragHandleProps} sx={{ p: 1.2 }}>
                    <strong>{j.title}</strong>
                    <div style={{ color: '#555', fontSize: 13 }}>{j.artist || '—'} • {j.type}</div>
                  </Paper>
                )}
              </Draggable>
            ))}
            {provided.placeholder}
          </Stack>
        )}
      </Droppable>
    </Paper>
  );
}

export default function PipelinesPage() {
  const jobs = usePipelineCards();

  const columns = useMemo(() => {
    const groups: Record<string, PipelineBoardCard[]> = {};
    [...MIXING_STAGES, ...MASTERING_STAGES].forEach(s => { groups[s] = []; });
    jobs.forEach(j => { (groups[j.stage] = groups[j.stage] || []).push(j); });
    return groups;
  }, [jobs]);

  const onDragEnd = async (res: DropResult) => {
    const { source, destination, draggableId } = res;
    if (!destination) return;
    const from = source.droppableId, to = destination.droppableId;
    if (from === to) return;
    const card = findPipelineCard(draggableId);
    if (!card) return;
    updatePipelineCardStage(draggableId, to);
    // Hook to backend (safe no-op if endpoint not present yet)
    updateStage(card, to).catch(() => {});
  };

  return (
    <>
      <Typography variant="h5" gutterBottom>Pipelines — Mixing / Mastering</Typography>
      <Divider sx={{ mb: 2 }} />
      <Box sx={{ display: 'flex', gap: 2, overflowX: 'auto' }}>
        <DragDropContext onDragEnd={onDragEnd}>
          {MIXING_STAGES.map(s => <Column key={s} id={s} title={`Mixing: ${s}`} jobs={columns[s] || []} />)}
          {MASTERING_STAGES.map(s => <Column key={s} id={s} title={`Mastering: ${s}`} jobs={columns[s] || []} />)}
        </DragDropContext>
      </Box>
    </>
  );
}
