import { useState } from 'react';
import { DragDropContext, Droppable, Draggable, DropResult } from '@hello-pangea/dnd';
import { Paper, Stack, Typography, Box } from '@mui/material';

/** Simple local Kanban for Mixing/Mastering.
 *  Backend endpoints are not defined yet; this page demonstrates UX and state handling.
 */

type Card = { id: string; title: string };
type Column = { id: string; title: string; cardIds: string[] };

const initialCards: Record<string, Card> = {
  'm1': { id: 'm1', title: 'Mix – Arkabuz – “Tema 1”' },
  'm2': { id: 'm2', title: 'Mix – Quimika Soul – “Voces”' },
  'a1': { id: 'a1', title: 'Master – Skanka Fe – “Single”' },
};
const initialColumns: Record<string, Column> = {
  'mixing-brief':   { id: 'mixing-brief',   title: 'Mixing / Brief',     cardIds: ['m1'] },
  'mixing-v1':      { id: 'mixing-v1',      title: 'Mixing / v1 Sent',   cardIds: [] },
  'mixing-rev':     { id: 'mixing-rev',     title: 'Mixing / Revisions', cardIds: ['m2'] },
  'mixing-done':    { id: 'mixing-done',    title: 'Mixing / Delivered', cardIds: [] },
  'mastering-brief':{ id: 'mastering-brief',title: 'Master / Brief',     cardIds: ['a1'] },
  'mastering-done': { id: 'mastering-done', title: 'Master / Delivered', cardIds: [] },
};
const columnOrder = ['mixing-brief','mixing-v1','mixing-rev','mixing-done','mastering-brief','mastering-done'];

export default function KanbanPage() {
  const [cards, setCards] = useState<Record<string, Card>>(initialCards);
  const [columns, setColumns] = useState<Record<string, Column>>(initialColumns);

  const onDragEnd = (result: DropResult) => {
    const { destination, source, draggableId } = result;
    if (!destination) return;
    if (destination.droppableId === source.droppableId && destination.index === source.index) return;

    const start = columns[source.droppableId];
    const finish = columns[destination.droppableId];

    if (start === finish) {
      const newCardIds = Array.from(start.cardIds);
      newCardIds.splice(source.index, 1);
      newCardIds.splice(destination.index, 0, draggableId);
      const newCol = { ...start, cardIds: newCardIds };
      setColumns({ ...columns, [newCol.id]: newCol });
      return;
    }

    // Move across columns
    const startIds = Array.from(start.cardIds);
    startIds.splice(source.index, 1);
    const finishIds = Array.from(finish.cardIds);
    finishIds.splice(destination.index, 0, draggableId);

    setColumns({
      ...columns,
      [start.id]: { ...start, cardIds: startIds },
      [finish.id]: { ...finish, cardIds: finishIds },
    });

    // TODO: POST to backend when pipeline endpoints exist.
  };

  return (
    <>
      <Typography variant="h5" gutterBottom>Pipelines — Mixing / Mastering</Typography>
      <DragDropContext onDragEnd={onDragEnd}>
        <Stack direction="row" gap={2} sx={{ overflowX: 'auto' }}>
          {columnOrder.map(colId => {
            const col = columns[colId];
            return (
              <Droppable droppableId={col.id} key={col.id}>
                {(provided, snapshot) => (
                  <Paper
                    ref={provided.innerRef}
                    {...provided.droppableProps}
                    sx={{ p: 2, width: 280, flex: '0 0 auto', bgcolor: snapshot.isDraggingOver ? 'grey.50' : 'background.paper' }}
                  >
                    <Typography variant="subtitle1" sx={{ mb: 1, fontWeight: 700 }}>{col.title}</Typography>
                    <Stack gap={1}>
                      {col.cardIds.map((cid, idx) => {
                        const card = cards[cid];
                        return (
                          <Draggable draggableId={cid} index={idx} key={cid}>
                            {(prov, snap) => (
                              <Box
                                ref={prov.innerRef}
                                {...prov.draggableProps}
                                {...prov.dragHandleProps}
                                sx={{
                                  p: 1.2,
                                  borderRadius: 1,
                                  border: '1px solid',
                                  borderColor: snap.isDragging ? 'primary.main' : 'divider',
                                  bgcolor: 'background.paper',
                                  boxShadow: snap.isDragging ? 2 : 0,
                                }}
                              >
                                {card.title}
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
    </>
  );
}
