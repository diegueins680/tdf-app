import { useState } from 'react';
import {
  Box,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  IconButton,
  Paper,
  Stack,
  Switch,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';
import { useForm } from 'react-hook-form';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Rooms } from '../api/rooms';
import type { RoomCreate, RoomDTO, RoomUpdate } from '../api/types';

const createSchema = z.object({
  rcName: z.string().min(2, 'Nombre muy corto'),
});

type RoomForm = z.infer<typeof createSchema>;

type DraftRoom = RoomDTO & { draftName: string };

function CreateRoomDialog({ open, onClose }: { open: boolean; onClose: () => void }) {
  const qc = useQueryClient();
  const { register, handleSubmit, reset, formState: { errors } } = useForm<RoomForm>({
    resolver: zodResolver(createSchema),
    defaultValues: { rcName: '' },
  });

  const mutation = useMutation({
    mutationFn: (body: RoomCreate) => Rooms.create(body),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['rooms'] });
      reset();
      onClose();
    },
  });

  const submit = (values: RoomForm) => {
    mutation.mutate({ rcName: values.rcName.trim() });
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="xs" fullWidth>
      <DialogTitle>Registrar sala</DialogTitle>
      <DialogContent>
        <TextField
          label="Nombre"
          fullWidth
          sx={{ mt: 1 }}
          {...register('rcName')}
          error={!!errors.rcName}
          helperText={errors.rcName?.message}
        />
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button variant="contained" onClick={handleSubmit(submit)} disabled={mutation.isPending}>
          {mutation.isPending ? 'Guardando…' : 'Guardar'}
        </Button>
      </DialogActions>
    </Dialog>
  );
}

export default function RoomsPage() {
  const qc = useQueryClient();
  const [createOpen, setCreateOpen] = useState(false);
  const [drafts, setDrafts] = useState<Record<string, DraftRoom>>({});
  const roomsQuery = useQuery({ queryKey: ['rooms'], queryFn: Rooms.list });

  const updateMutation = useMutation({
    mutationFn: ({ id, body }: { id: string; body: RoomUpdate }) => Rooms.update(id, body),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['rooms'] });
    },
  });

  const rooms = roomsQuery.data ?? [];

  const handleNameChange = (room: RoomDTO, value: string) => {
    setDrafts(prev => ({
      ...prev,
      [room.roomId]: { ...room, draftName: value },
    }));
  };

  const handleNameCommit = (room: RoomDTO) => {
    const draft = drafts[room.roomId];
    const trimmed = draft?.draftName?.trim();
    if (!trimmed || trimmed === room.rName) {
      return;
    }
    updateMutation.mutate({ id: room.roomId, body: { ruName: trimmed } });
  };

  const handleToggleBookable = (room: RoomDTO, next: boolean) => {
    updateMutation.mutate({ id: room.roomId, body: { ruIsBookable: next } });
  };

  return (
    <Stack spacing={2}>
      <Stack direction="row" alignItems="center" justifyContent="space-between">
        <Typography variant="h5">Salas</Typography>
        <Button variant="contained" onClick={() => setCreateOpen(true)}>Registrar sala</Button>
      </Stack>
      <Paper variant="outlined">
        <TableContainer>
          <Table size="small">
            <TableHead>
              <TableRow>
                <TableCell>Nombre</TableCell>
                <TableCell>Reservable</TableCell>
                <TableCell width={80}>Acciones</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {rooms.map(room => {
                const draft = drafts[room.roomId];
                const value = draft?.draftName ?? room.rName;
                return (
                  <TableRow key={room.roomId} hover>
                    <TableCell>
                      <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                        <TextField
                          value={value}
                          size="small"
                          onChange={(event) => handleNameChange(room, event.target.value)}
                          onBlur={() => handleNameCommit(room)}
                        />
                      </Box>
                    </TableCell>
                    <TableCell>
                      <Switch
                        checked={room.rBookable}
                        onChange={(event) => handleToggleBookable(room, event.target.checked)}
                        color="primary"
                      />
                    </TableCell>
                    <TableCell>
                      <Tooltip title="Editar nombre">
                        <IconButton size="small" onClick={() => handleNameCommit(room)}>
                          <EditIcon fontSize="small" />
                        </IconButton>
                      </Tooltip>
                    </TableCell>
                  </TableRow>
                );
              })}
              {rooms.length === 0 && (
                <TableRow>
                  <TableCell colSpan={3}>
                    <Typography variant="body2" color="text.secondary" align="center" sx={{ py: 2 }}>
                      Aún no hay salas registradas.
                    </Typography>
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </TableContainer>
      </Paper>

      <CreateRoomDialog open={createOpen} onClose={() => setCreateOpen(false)} />
    </Stack>
  );
}
