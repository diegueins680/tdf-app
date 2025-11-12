import { useMemo, useState } from 'react';
import {
  Alert,
  Button,
  Chip,
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
  Typography,
} from '@mui/material';
import RefreshIcon from '@mui/icons-material/Refresh';
import AddIcon from '@mui/icons-material/Add';
import EditIcon from '@mui/icons-material/Edit';
import { useMutation, useQuery, useQueryClient, type UseQueryResult } from '@tanstack/react-query';
import { Rooms } from '../api/rooms';
import type { RoomDTO } from '../api/types';

export default function RoomsPage() {
  const qc = useQueryClient();
  const [name, setName] = useState('');
  const [renameId, setRenameId] = useState<string | null>(null);
  const [renameValue, setRenameValue] = useState('');

  const roomsQuery: UseQueryResult<RoomDTO[], Error> = useQuery<RoomDTO[], Error>({
    queryKey: ['rooms'],
    queryFn: Rooms.list,
  });
  const rooms = useMemo<RoomDTO[]>(() => roomsQuery.data ?? [], [roomsQuery.data]);

  const createMutation = useMutation<RoomDTO, Error, { rcName: string }>({
    mutationFn: (body) => Rooms.create(body),
    onSuccess: () => {
      setName('');
      void qc.invalidateQueries({ queryKey: ['rooms'] });
    },
  });

  const updateMutation = useMutation<RoomDTO, Error, { roomId: string; payload: Record<string, unknown> }>({
    mutationFn: ({ roomId, payload }: { roomId: string; payload: Record<string, unknown> }) =>
      Rooms.update(roomId, payload),
    onSuccess: () => {
      setRenameId(null);
      setRenameValue('');
      void qc.invalidateQueries({ queryKey: ['rooms'] });
    },
  });

  const handleToggleBookable = (room: RoomDTO) => {
    updateMutation.mutate({ roomId: room.roomId, payload: { ruIsBookable: !room.rBookable } });
  };

  const handleRenameSave = () => {
    if (!renameId || !renameValue.trim()) return;
    updateMutation.mutate({ roomId: renameId, payload: { ruName: renameValue.trim() } });
  };

  return (
    <Stack gap={3}>
      <Stack spacing={1}>
        <Typography variant="h4">Salas y recursos</Typography>
        <Typography variant="body2" color="text.secondary">
          Administra las salas disponibles para el calendario y su disponibilidad operativa.
        </Typography>
      </Stack>

      <Paper sx={{ p: 3 }}>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ sm: 'center' }}>
          <TextField
            label="Nombre de la sala"
            value={name}
            onChange={(e) => setName(e.target.value)}
            fullWidth
          />
          <Button
            variant="contained"
            startIcon={<AddIcon />}
            disabled={!name.trim() || createMutation.isPending}
            onClick={() => createMutation.mutate({ rcName: name.trim() })}
          >
            Agregar
          </Button>
          <IconButton
            onClick={() => {
              void roomsQuery.refetch();
            }}
          >
            <RefreshIcon />
          </IconButton>
        </Stack>
        {createMutation.isError && (
          <Alert severity="error" sx={{ mt: 2 }}>
            {createMutation.error?.message}
          </Alert>
        )}
      </Paper>

      {roomsQuery.error && (
        <Alert severity="error">{roomsQuery.error.message}</Alert>
      )}

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>Nombre</TableCell>
              <TableCell>Estado</TableCell>
              <TableCell>Acciones</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {roomsQuery.isLoading && (
              <TableRow>
                <TableCell colSpan={3}>Cargando salas...</TableCell>
              </TableRow>
            )}
            {!roomsQuery.isLoading && rooms.length === 0 && (
              <TableRow>
                <TableCell colSpan={3}>No hay salas registradas.</TableCell>
              </TableRow>
            )}
            {rooms.map((room) => (
              <TableRow key={room.roomId}>
                <TableCell>
                  {renameId === room.roomId ? (
                    <Stack direction="row" spacing={1} alignItems="center">
                      <TextField
                        size="small"
                        value={renameValue}
                        onChange={(e) => setRenameValue(e.target.value)}
                      />
                      <Button size="small" onClick={handleRenameSave} disabled={updateMutation.isPending}>
                        Guardar
                      </Button>
                      <Button size="small" onClick={() => setRenameId(null)} color="inherit">
                        Cancelar
                      </Button>
                    </Stack>
                  ) : (
                    <Stack direction="row" spacing={1} alignItems="center">
                      <Typography fontWeight={600}>{room.rName}</Typography>
                      <IconButton size="small" onClick={() => { setRenameId(room.roomId); setRenameValue(room.rName); }}>
                        <EditIcon fontSize="small" />
                      </IconButton>
                    </Stack>
                  )}
                </TableCell>
                <TableCell>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <Chip
                      label={room.rBookable ? 'Reservable' : 'Bloqueado'}
                      color={room.rBookable ? 'success' : 'default'}
                      size="small"
                    />
                    <Switch
                      checked={room.rBookable}
                      onChange={() => handleToggleBookable(room)}
                      inputProps={{ 'aria-label': 'toggle bookable' }}
                    />
                  </Stack>
                </TableCell>
                <TableCell>
                  <Typography variant="body2" color="text.secondary">
                    ID: {room.roomId}
                  </Typography>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
    </Stack>
  );
}
