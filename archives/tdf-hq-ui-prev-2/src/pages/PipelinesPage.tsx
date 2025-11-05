
import { useState, useMemo } from 'react';
import { DragDropContext, Droppable, Draggable, DropResult } from '@hello-pangea/dnd';
import { Paper, Stack, Typography, Box, Chip } from '@mui/material';

type Job = { id: string; title: string; artist?: string; type: 'Mixing' | 'Mastering'; stage: string };

const MIXING = ['Brief','Prep','v1 Sent','Revisions','Approved','Delivered'] as const;
const MASTERING = ['Brief','v1','Revisions','Approved','DDP Delivered'] as const;

function Column({ id, title, jobs }: { id: string; title: string; jobs: Job[] }) {
  return (
    <Paper sx={{ p: 1, minWidth: 260, maxHeight: '70vh', overflowY: 'auto' }} variant="outlined">
      <Typography variant="subtitle1" sx={{ mb: 1 }}>{title} <Chip size="small" label={jobs.length} sx={{ ml: 1 }} /></Typography>
      <Droppable droppableId={id}>
        {(provided) => (
          <Stack ref={provided.innerRef} {...provided.droppableProps} gap={1}>
            {jobs.map((j, idx) => (
              <Draggable draggableId={j.id} index={idx} key={j.id}>
                {(prov) => (
                  <Paper ref={prov.innerRef} {...prov.draggableProps} {...prov.dragHandleProps} sx={{ p: 1 }}>
                    <strong>{j.title}</strong>
                    <div style={{ color: '#555' }}>{j.artist || '—'} • {j.type}</div>
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
  const [jobs, setJobs] = useState<Job[]>([
    { id: 'mx-1', title: 'Arkabuz - Single A', artist: 'Arkabuz', type: 'Mixing', stage: 'Brief' },
    { id: 'mx-2', title: 'Quimika - EP', artist: 'Quimika Soul', type: 'Mixing', stage: 'Prep' },
    { id: 'ma-1', title: 'Skanka Fe - LP', artist: 'Skanka Fe', type: 'Mastering', stage: 'v1' },
    { id: 'ma-2', title: 'El Bloque - Single', artist: 'El Bloque', type: 'Mastering', stage: 'Approved' },
  ]);

  const columns = useMemo(() => {
    const groups: Record<string, Job[]> = {};
    [...MIXING, ...MASTERING].forEach(s => { groups[s] = []; });
    jobs.forEach(j => { groups[j.stage] = groups[j.stage] || []; groups[j.stage].push(j); });
    return groups;
  }, [jobs]);

  const onDragEnd = (res: DropResult) => {
    const { source, destination, draggableId } = res;
    if (!destination) return;
    const from = source.droppableId;
    const to = destination.droppableId;
    if (from === to) return;
    setJobs(prev => prev.map(j => j.id === draggableId ? { ...j, stage: to } : j));
    // TODO: POST to API once endpoints exist (e.g., /mixing/:id -> stage)
  };

  return (
    <>
      <Typography variant="h5" gutterBottom>Pipelines — Mixing / Mastering</Typography>
      <Box sx={{ display: 'flex', gap: 2, overflowX: 'auto' }}>
        <DragDropContext onDragEnd={onDragEnd}>
          {MIXING.map(s => <Column key={s} id={s} title={`Mixing: ${s}`} jobs={columns[s] || []} />)}
          {MASTERING.map(s => <Column key={s} id={s} title={`Mastering: ${s}`} jobs={columns[s] || []} />)}
        </DragDropContext>
      </Box>
    </>
  );
}
