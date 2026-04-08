import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Grid,
  LinearProgress,
  MenuItem,
  Paper,
  Select,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Typography,
} from '@mui/material';
import AssignmentIndIcon from '@mui/icons-material/AssignmentInd';
import ScheduleIcon from '@mui/icons-material/Schedule';
import EventSeatIcon from '@mui/icons-material/EventSeat';
import { Controller, useForm } from 'react-hook-form';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Trials } from '../api/trials';
import { Rooms } from '../api/rooms';
import type { RoomDTO, TrialRequestDTO } from '../api/types';

const assignSchema = z.object({
  teacherId: z.coerce.number().int().positive('Ingresa un ID válido'),
});

type AssignForm = z.infer<typeof assignSchema>;

const scheduleSchema = z.object({
  teacherId: z.coerce.number().int().positive('Ingresa un ID válido'),
  startAt: z.string().min(1, 'Selecciona fecha y hora de inicio'),
  endAt: z.string().min(1, 'Selecciona fecha y hora de fin'),
  roomId: z.coerce.number().int().positive('Selecciona una sala'),
});

type ScheduleForm = z.infer<typeof scheduleSchema>;

const STATUS_OPTIONS = [
  { value: 'all', label: 'Todos' },
  { value: 'Requested', label: 'Pendiente' },
  { value: 'Assigned', label: 'Asignada' },
  { value: 'Scheduled', label: 'Agendada' },
  { value: 'Completed', label: 'Completada' },
];

function isoLocalValue(date: Date) {
  const offset = date.getTimezoneOffset();
  const adjusted = new Date(date.getTime() - offset * 60 * 1000);
  return adjusted.toISOString().slice(0, 16);
}

function formatSlot(slot?: { startAt: string; endAt: string; teacherName?: string | null }) {
  if (!slot) return '—';
  const start = new Date(slot.startAt);
  const end = new Date(slot.endAt);
  if (Number.isNaN(start.getTime()) || Number.isNaN(end.getTime())) {
    return `${slot.startAt} → ${slot.endAt}`;
  }
  const range = `${start.toLocaleString()} → ${end.toLocaleTimeString()}`;
  return slot.teacherName ? `${range} · ${slot.teacherName}` : range;
}

export default function TrialLessonsPage() {
  const qc = useQueryClient();
  const [statusFilter, setStatusFilter] = useState('all');
  const [subjectFilter, setSubjectFilter] = useState<number | 'all'>('all');
  const [selected, setSelected] = useState<TrialRequestDTO | null>(null);
  const [assignTarget, setAssignTarget] = useState<TrialRequestDTO | null>(null);
  const [scheduleTarget, setScheduleTarget] = useState<TrialRequestDTO | null>(null);

  const subjectsQuery = useQuery({ queryKey: ['trials', 'subjects'], queryFn: Trials.listSubjects });
  const roomsQuery = useQuery({ queryKey: ['rooms'], queryFn: Rooms.list });

  const trialRequestsQuery = useQuery({
    queryKey: ['trials', 'requests', { statusFilter, subjectFilter }],
    queryFn: () => Trials.listTrialRequests({
      status: statusFilter === 'all' ? undefined : statusFilter,
      subjectId: subjectFilter === 'all' ? undefined : subjectFilter,
    }),
  });

  const trialRequests = trialRequestsQuery.data ?? [];
  const subjects = subjectsQuery.data ?? [];
  const rooms = roomsQuery.data ?? [];

  const filtered = useMemo(() => trialRequests, [trialRequests]);

  const stats = useMemo(() => {
    const byStatus = trialRequests.reduce<Record<string, number>>((acc, item) => {
      acc[item.status] = (acc[item.status] ?? 0) + 1;
      return acc;
    }, {});
    return byStatus;
  }, [trialRequests]);

  const assignForm = useForm<AssignForm>({
    resolver: zodResolver(assignSchema),
    defaultValues: { teacherId: 0 },
  });

  const initialStart = new Date();
  const initialEnd = new Date(initialStart.getTime() + 60 * 60 * 1000);
  const scheduleForm = useForm<ScheduleForm>({
    resolver: zodResolver(scheduleSchema),
    defaultValues: {
      teacherId: 0,
      startAt: isoLocalValue(initialStart),
      endAt: isoLocalValue(initialEnd),
      roomId: 0,
    },
  });

  useEffect(() => {
    if (rooms.length) {
      const current = scheduleForm.getValues('roomId');
      if (!current || current <= 0) {
        scheduleForm.setValue('roomId', Number(rooms[0].roomId));
      }
    }
  }, [rooms, scheduleForm]);

  const assignMutation = useMutation({
    mutationFn: ({ requestId, teacherId }: { requestId: number; teacherId: number }) =>
      Trials.assignTrial(requestId, { teacherId }),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['trials', 'requests'] });
      setAssignTarget(null);
    },
  });

  const scheduleMutation = useMutation({
    mutationFn: (payload: { requestId: number } & ScheduleForm) =>
      Trials.scheduleTrial({
        requestId: payload.requestId,
        teacherId: payload.teacherId,
        startAt: new Date(payload.startAt).toISOString(),
        endAt: new Date(payload.endAt).toISOString(),
        roomId: payload.roomId,
      }),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['trials', 'requests'] });
      setScheduleTarget(null);
    },
  });

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ xs: 'flex-start', md: 'center' }}>
        <Box>
          <Typography variant="h5">Trial lessons</Typography>
          <Typography variant="body2" color="text.secondary">
            Gestiona las solicitudes de prueba, asigna docentes y agenda la primera clase.
          </Typography>
        </Box>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'stretch', sm: 'center' }}>
          <Select
            size="small"
            value={subjectFilter === 'all' ? 'all' : String(subjectFilter)}
            onChange={(event) => {
              const value = event.target.value;
              setSubjectFilter(value === 'all' ? 'all' : Number(value));
            }}
            sx={{ minWidth: 200 }}
          >
            <MenuItem value="all">Todas las materias</MenuItem>
            {subjects.map((subject) => (
              <MenuItem key={subject.subjectId} value={String(subject.subjectId)}>
                {subject.name}
              </MenuItem>
            ))}
          </Select>
          <Select
            size="small"
            value={statusFilter}
            onChange={(event) => setStatusFilter(event.target.value)}
            sx={{ minWidth: 180 }}
          >
            {STATUS_OPTIONS.map((option) => (
              <MenuItem key={option.value} value={option.value}>{option.label}</MenuItem>
            ))}
          </Select>
        </Stack>
      </Stack>

      <Paper variant="outlined" sx={{ p: 2 }}>
        <Grid container spacing={2}>
          {STATUS_OPTIONS.filter(opt => opt.value !== 'all').map((option) => (
            <Grid item key={option.value} xs={12} sm={6} md={3}>
              <Box sx={{ p: 2, borderRadius: 2, border: '1px solid', borderColor: 'divider' }}>
                <Typography variant="subtitle2" color="text.secondary">{option.label}</Typography>
                <Typography variant="h6">{stats[option.value] ?? 0}</Typography>
              </Box>
            </Grid>
          ))}
        </Grid>
      </Paper>

      <Stack direction={{ xs: 'column', lg: 'row' }} spacing={2} alignItems="stretch">
        <Paper variant="outlined" sx={{ flex: 1 }}>
          {trialRequestsQuery.isFetching && <LinearProgress />}
          <TableContainer>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>ID</TableCell>
                  <TableCell>Materia</TableCell>
                  <TableCell>Estatus</TableCell>
                  <TableCell>Notas</TableCell>
                  <TableCell align="right">Acciones</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {filtered.map((request) => (
                  <TableRow
                    key={request.requestId}
                    hover
                    selected={selected?.requestId === request.requestId}
                    onClick={() => setSelected(request)}
                    sx={{ cursor: 'pointer' }}
                  >
                    <TableCell width={90}>{request.requestId}</TableCell>
                    <TableCell>{request.subjectName ?? request.subjectId ?? '—'}</TableCell>
                    <TableCell>{request.status}</TableCell>
                    <TableCell>{request.notes ?? '—'}</TableCell>
                    <TableCell align="right">
                      <Stack direction="row" spacing={1} justifyContent="flex-end">
                        <Button
                          size="small"
                          variant="outlined"
                          startIcon={<AssignmentIndIcon fontSize="small" />}
                          onClick={(event) => {
                            event.stopPropagation();
                            setAssignTarget(request);
                            assignForm.reset({ teacherId: 0 });
                          }}
                        >
                          Asignar
                        </Button>
                        <Button
                          size="small"
                          variant="contained"
                          startIcon={<ScheduleIcon fontSize="small" />}
                          onClick={(event) => {
                            event.stopPropagation();
                            setScheduleTarget(request);
                            const now = new Date();
                            const end = new Date(now.getTime() + 60 * 60 * 1000);
                            scheduleForm.reset({
                              teacherId: 0,
                              startAt: isoLocalValue(now),
                              endAt: isoLocalValue(end),
                              roomId: rooms[0] ? Number(rooms[0].roomId) : 0,
                            });
                          }}
                        >
                          Agendar
                        </Button>
                      </Stack>
                    </TableCell>
                  </TableRow>
                ))}
                {filtered.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={5} align="center" sx={{ py: 4 }}>
                      No hay solicitudes con los filtros actuales.
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          </TableContainer>
        </Paper>

        <Paper variant="outlined" sx={{ flexBasis: 320, flexShrink: 0, p: 2 }}>
          {selected ? (
            <Stack spacing={1.5}>
              <Typography variant="subtitle1">Detalle de solicitud</Typography>
              <Box>
                <Typography variant="overline" display="block">Estado</Typography>
                <Typography>{selected.status}</Typography>
              </Box>
              <Box>
                <Typography variant="overline" display="block">Materia</Typography>
                <Typography>{selected.subjectName ?? selected.subjectId ?? '—'}</Typography>
              </Box>
              <Box>
                <Typography variant="overline" display="block">Notas</Typography>
                <Typography>{selected.notes ?? 'Sin notas'}</Typography>
              </Box>
              <Box>
                <Typography variant="overline" display="block">Slots preferidos</Typography>
                {selected.preferred?.length ? (
                  <Stack spacing={0.5}>
                    {selected.preferred.map((slot, index) => (
                      <Typography key={index} variant="body2">{formatSlot(slot)}</Typography>
                    ))}
                  </Stack>
                ) : (
                  <Typography>Sin preferencia cargada</Typography>
                )}
              </Box>
            </Stack>
          ) : (
            <Stack spacing={2} alignItems="center" justifyContent="center" sx={{ minHeight: 240 }}>
              <EventSeatIcon color="disabled" fontSize="large" />
              <Typography variant="body2" color="text.secondary" align="center">
                Selecciona una solicitud para revisar detalles y tomar acción.
              </Typography>
            </Stack>
          )}
        </Paper>
      </Stack>

      <Dialog open={!!assignTarget} onClose={() => setAssignTarget(null)} maxWidth="xs" fullWidth>
        <DialogTitle>Asignar profesor</DialogTitle>
        <DialogContent sx={{ display: 'flex', flexDirection: 'column', gap: 2, pt: 2 }}>
          <Alert severity="info">
            Ingresa el ID interno del profesor al que se le asignará la prueba. Puedes localizarlo en la sección de profesores.
          </Alert>
          <TextField
            label="Teacher ID"
            type="number"
            {...assignForm.register('teacherId')}
            error={!!assignForm.formState.errors.teacherId}
            helperText={assignForm.formState.errors.teacherId?.message}
          />
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setAssignTarget(null)}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={assignForm.handleSubmit((values) => {
              if (!assignTarget) return;
              assignMutation.mutate({ requestId: assignTarget.requestId, teacherId: values.teacherId });
            })}
            disabled={assignMutation.isPending}
          >
            {assignMutation.isPending ? 'Asignando…' : 'Asignar'}
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog open={!!scheduleTarget} onClose={() => setScheduleTarget(null)} maxWidth="sm" fullWidth>
        <DialogTitle>Agendar trial lesson</DialogTitle>
        <DialogContent sx={{ display: 'flex', flexDirection: 'column', gap: 2, pt: 2 }}>
          <Alert severity="info">
            Define fecha, hora y sala para confirmar la clase de prueba.
          </Alert>
          <TextField
            label="Teacher ID"
            type="number"
            {...scheduleForm.register('teacherId')}
            error={!!scheduleForm.formState.errors.teacherId}
            helperText={scheduleForm.formState.errors.teacherId?.message}
          />
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
            <TextField
              label="Inicio"
              type="datetime-local"
              fullWidth
              {...scheduleForm.register('startAt')}
              error={!!scheduleForm.formState.errors.startAt}
              helperText={scheduleForm.formState.errors.startAt?.message}
            />
            <TextField
              label="Fin"
              type="datetime-local"
              fullWidth
              {...scheduleForm.register('endAt')}
              error={!!scheduleForm.formState.errors.endAt}
              helperText={scheduleForm.formState.errors.endAt?.message}
            />
          </Stack>
          <Controller
            name="roomId"
            control={scheduleForm.control}
            render={({ field, fieldState }) => (
              <Box>
                <Select
                  fullWidth
                  displayEmpty
                  value={field.value ? String(field.value) : ''}
                  onChange={(event) => field.onChange(Number(event.target.value))}
                  error={!!fieldState.error}
                >
                  <MenuItem value="" disabled>Selecciona una sala</MenuItem>
                  {rooms.map((room: RoomDTO) => (
                    <MenuItem key={room.roomId} value={room.roomId}>{room.rName}</MenuItem>
                  ))}
                </Select>
                {fieldState.error && (
                  <Typography color="error" variant="caption">{fieldState.error.message}</Typography>
                )}
              </Box>
            )}
          />
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setScheduleTarget(null)}>Cancelar</Button>
          <Button
            variant="contained"
            startIcon={<ScheduleIcon />}
            onClick={scheduleForm.handleSubmit((values) => {
              if (!scheduleTarget) return;
              scheduleMutation.mutate({ requestId: scheduleTarget.requestId, ...values });
            })}
            disabled={scheduleMutation.isPending}
          >
            {scheduleMutation.isPending ? 'Agendando…' : 'Agendar'}
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}
