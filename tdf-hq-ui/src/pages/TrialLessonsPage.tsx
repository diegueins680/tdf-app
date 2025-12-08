import { useMemo, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Grid,
  LinearProgress,
  CircularProgress,
  MenuItem,
  Paper,
  Snackbar,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';
import RefreshIcon from '@mui/icons-material/Refresh';
import FilterAltIcon from '@mui/icons-material/FilterAlt';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import CancelIcon from '@mui/icons-material/Cancel';
import AutoAwesomeIcon from '@mui/icons-material/AutoAwesome';
import { Bookings } from '../api/bookings';
import type {
  ClassSessionDTO,
  ClassSessionUpdate,
  TrialSubject,
  ClassSessionCreate,
} from '../api/trials';
import { Trials } from '../api/trials';
import { Rooms } from '../api/rooms';

type StatusKey = 'programada' | 'por-confirmar' | 'cancelada' | 'realizada' | 'reprogramada';

const statusMeta: Record<StatusKey, { label: string; color: string; bg: string; border: string; icon: JSX.Element }> = {
  programada: {
    label: 'Programada',
    color: '#0ea5e9',
    bg: 'rgba(14,165,233,0.12)',
    border: 'rgba(14,165,233,0.35)',
    icon: <EventAvailableIcon fontSize="small" />,
  },
  'por-confirmar': {
    label: 'Por confirmar',
    color: '#f59e0b',
    bg: 'rgba(245,158,11,0.12)',
    border: 'rgba(245,158,11,0.4)',
    icon: <AutoAwesomeIcon fontSize="small" />,
  },
  cancelada: {
    label: 'Cancelada',
    color: '#ef4444',
    bg: 'rgba(239,68,68,0.12)',
    border: 'rgba(239,68,68,0.4)',
    icon: <CancelIcon fontSize="small" />,
  },
  realizada: {
    label: 'Realizada',
    color: '#10b981',
    bg: 'rgba(16,185,129,0.12)',
    border: 'rgba(16,185,129,0.35)',
    icon: <CheckCircleIcon fontSize="small" />,
  },
  reprogramada: {
    label: 'Reprogramada',
    color: '#8b5cf6',
    bg: 'rgba(139,92,246,0.12)',
    border: 'rgba(139,92,246,0.35)',
    icon: <EventAvailableIcon fontSize="small" />,
  },
};
const statusOptions: StatusKey[] = ['programada', 'por-confirmar', 'cancelada', 'realizada', 'reprogramada'];
const bookingStatusMap: Record<StatusKey, string> = {
  programada: 'Confirmed',
  'por-confirmar': 'Tentative',
  cancelada: 'Cancelled',
  realizada: 'Completed',
  reprogramada: 'Confirmed',
};

const toLocalInput = (iso?: string | null) => {
  if (!iso) return '';
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return '';
  const pad = (v: number) => String(v).padStart(2, '0');
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}T${pad(d.getHours())}:${pad(d.getMinutes())}`;
};

const addMinutes = (iso: string, minutes: number) => {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return '';
  const end = new Date(d.getTime() + minutes * 60000);
  return end.toISOString();
};

const formatDateTime = (iso: string) => {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return '';
  return d.toLocaleString('es-EC', { weekday: 'short', month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' });
};

export default function TrialLessonsPage() {
  const qc = useQueryClient();
  const navigate = useNavigate();
  const subjectsQuery = useQuery({
    queryKey: ['trial-subjects'],
    queryFn: Trials.listSubjects,
  });
  const teachersQuery = useQuery({
    queryKey: ['trial-teachers'],
    queryFn: Trials.listTeachers,
  });
  const roomsQuery = useQuery({
    queryKey: ['rooms'],
    queryFn: Rooms.list,
  });
  const studentsQuery = useQuery({
    queryKey: ['trial-students'],
    queryFn: Trials.listStudents,
  });

  const [subjectFilter, setSubjectFilter] = useState<number | 'all'>('all');
  const [teacherFilter, setTeacherFilter] = useState<number | 'all'>('all');
  const [statusFilter, setStatusFilter] = useState<StatusKey | 'all'>('all');
  const [fromInput, setFromInput] = useState(() => {
    const now = new Date();
    const start = new Date(now);
    start.setDate(now.getDate() - 7);
    start.setHours(0, 0, 0, 0);
    return toLocalInput(start.toISOString());
  });
  const [toInput, setToInput] = useState(() => {
    const end = new Date();
    end.setDate(end.getDate() + 30);
    end.setHours(23, 59, 0, 0);
    return toLocalInput(end.toISOString());
  });

  const toIsoOrUndefined = (val: string) => {
    if (!val) return undefined;
    const d = new Date(val);
    return Number.isNaN(d.getTime()) ? undefined : d.toISOString();
  };

  const filterFrom = useMemo(() => toIsoOrUndefined(fromInput), [fromInput]);
  const filterTo = useMemo(() => toIsoOrUndefined(toInput), [toInput]);

  const classesQuery = useQuery({
    queryKey: ['trial-class-sessions', subjectFilter, teacherFilter, statusFilter, filterFrom, filterTo],
    queryFn: () =>
      Trials.listClassSessions({
        subjectId: subjectFilter === 'all' ? undefined : subjectFilter,
        teacherId: teacherFilter === 'all' ? undefined : teacherFilter,
        from: filterFrom,
        to: filterTo,
        status: statusFilter === 'all' ? undefined : statusFilter,
      }),
  });

  const createMutation = useMutation({
    mutationFn: (payload: ClassSessionCreate) => Trials.createClassSession(payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['trial-class-sessions'] });
    },
  });

  const updateMutation = useMutation({
    mutationFn: (params: { id: number; patch: ClassSessionUpdate }) =>
      Trials.updateClassSession(params.id, params.patch),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['trial-class-sessions'] });
    },
  });

  const subjects = useMemo<TrialSubject[]>(() => (subjectsQuery.data ?? []).filter((s) => s.active), [subjectsQuery.data]);
  const teachers = teachersQuery.data ?? [];
  const rooms = roomsQuery.data ?? [];
  const students = studentsQuery.data ?? [];

  const [formError, setFormError] = useState<string | null>(null);
  const [listError, setListError] = useState<string | null>(null);
  const [statusPendingId, setStatusPendingId] = useState<number | null>(null);
  const [statusToast, setStatusToast] = useState<string | null>(null);
  const [dialogOpen, setDialogOpen] = useState(false);
  const [studentDialogOpen, setStudentDialogOpen] = useState(false);
  const [editing, setEditing] = useState<ClassSessionDTO | null>(null);
  const [form, setForm] = useState<{
    studentId: number | null;
    teacherId: number | null;
    subjectId: number | null;
    roomId: string | null;
    startAt: string;
    endAt: string;
    notes: string;
    status: StatusKey;
  }>({
    studentId: null,
    teacherId: null,
    subjectId: null,
    roomId: null,
    startAt: '',
    endAt: '',
    notes: '',
    status: 'programada',
  });

  const applyRangePreset = (pastDays: number, futureDays: number) => {
    const now = new Date();
    const from = new Date(now);
    from.setDate(now.getDate() - pastDays);
    from.setHours(0, 0, 0, 0);
    const to = new Date(now);
    to.setDate(now.getDate() + futureDays);
    to.setHours(23, 59, 0, 0);
    setFromInput(toLocalInput(from.toISOString()));
    setToInput(toLocalInput(to.toISOString()));
  };

  const openCreateDialog = () => {
    setFormError(null);
    setEditing(null);
    const now = new Date();
    const startLocal = toLocalInput(now.toISOString());
    setForm({
      studentId: null,
      teacherId: null,
      subjectId: null,
      roomId: null,
      startAt: startLocal,
      endAt: toLocalInput(addMinutes(now.toISOString(), 45)),
      notes: '',
      status: 'programada',
    });
    setDialogOpen(true);
  };

  const openEditDialog = (cls: ClassSessionDTO) => {
    setFormError(null);
    setEditing(cls);
    setForm({
      studentId: cls.studentId ?? null,
      teacherId: cls.teacherId ?? null,
      subjectId: cls.subjectId ?? null,
      roomId: cls.roomId ?? null,
      startAt: toLocalInput(cls.startAt),
      endAt: toLocalInput(cls.endAt),
      notes: cls.notes ?? '',
      status: normalizedStatus(cls.status),
    });
    setDialogOpen(true);
  };

  const closeDialog = () => {
    setFormError(null);
    setDialogOpen(false);
  };

  const [studentForm, setStudentForm] = useState<{ fullName: string; email: string; phone: string; notes: string }>({
    fullName: '',
    email: '',
    phone: '',
    notes: '',
  });
  const studentMutation = useMutation({
    mutationFn: Trials.createStudent,
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['trial-students'] });
    },
  });

  const openStudentDialog = () => {
    setStudentForm({ fullName: '', email: '', phone: '', notes: '' });
    setStudentDialogOpen(true);
  };
  const closeStudentDialog = () => setStudentDialogOpen(false);

  const handleDateChange = (value: string, minutesFallback: number) => {
    const iso = value ? new Date(value).toISOString() : '';
    const endIso = iso ? addMinutes(iso, minutesFallback) : '';
    setForm((prev) => ({
      ...prev,
      startAt: value,
      endAt: prev.endAt ? prev.endAt : toLocalInput(endIso),
    }));
  };

  const handleSubmit = async () => {
    setFormError(null);
    setListError(null);
    const { studentId, teacherId, subjectId, roomId, startAt, endAt, notes } = form;
    if (!studentId || !teacherId || !subjectId || !roomId || !startAt || !endAt) {
      setFormError('Completa estudiante, profesor, materia, sala y horario.');
      return;
    }
    const basePayload: ClassSessionCreate = {
      studentId: Number(studentId),
      teacherId: Number(teacherId),
      subjectId: Number(subjectId),
      startAt: new Date(startAt).toISOString(),
      endAt: new Date(endAt).toISOString(),
      roomId: Number(roomId),
      status: form.status,
    };

    try {
      if (editing) {
        const patch: ClassSessionUpdate = {
          ...basePayload,
          notes: notes.trim() || undefined,
        };
        await updateMutation.mutateAsync({ id: editing.classSessionId, patch });
        if (normalizedStatus(editing.status) !== form.status) {
          await syncStatus(editing, form.status, form.notes);
          setStatusToast(`Estado actualizado: ${statusMeta[form.status].label}`);
          void qc.invalidateQueries({ queryKey: ['trial-class-sessions'] });
        }
      } else {
        await createMutation.mutateAsync(basePayload);
      }
      setDialogOpen(false);
    } catch (error) {
      const message = error instanceof Error ? error.message : 'No se pudo guardar la clase.';
      setFormError(message);
    }
  };

  const syncStatus = async (cls: ClassSessionDTO, nextStatus: StatusKey, notesOverride?: string) => {
    const bookingId = cls.bookingId;
    const trimmedNotes = (notesOverride ?? cls.notes ?? '').trim() || undefined;
    const patch: ClassSessionUpdate = { status: nextStatus, notes: trimmedNotes };

    // Always try to persist class status directly so standalone trials are supported.
    if (nextStatus === 'realizada' && (!cls.startAt || !cls.endAt)) {
      throw new Error('Completa fecha y hora antes de marcar la clase como realizada.');
    }
    await Trials.updateClassSession(cls.classSessionId, patch);

    if (nextStatus === 'realizada') {
      await Trials.attendClassSession(cls.classSessionId, {
        attended: true,
        notes: trimmedNotes,
      });
    }

    if (!bookingId) return;
    const ubStatus = bookingStatusMap[nextStatus];
    if (ubStatus) {
      await Bookings.update(bookingId, { ubStatus });
    }
  };

  const statusMutation = useMutation({
    mutationFn: async (params: { cls: ClassSessionDTO; nextStatus: StatusKey }) => {
      setStatusPendingId(params.cls.classSessionId);
      await syncStatus(params.cls, params.nextStatus);
    },
    onSuccess: (_, variables) => {
      setListError(null);
      setStatusToast(`Estado actualizado: ${statusMeta[variables.nextStatus].label}`);
      void qc.invalidateQueries({ queryKey: ['trial-class-sessions'] });
    },
    onError: (error) => {
      const msg = error instanceof Error ? error.message : 'No pudimos actualizar el estado.';
      setListError(msg);
    },
    onSettled: () => {
      setStatusPendingId(null);
    },
  });

  const data = classesQuery.data ?? [];
  const loading = classesQuery.isLoading || subjectsQuery.isLoading || teachersQuery.isLoading || roomsQuery.isLoading || studentsQuery.isLoading;

  const normalizedStatus = (status: string): StatusKey =>
    (['programada', 'por-confirmar', 'cancelada', 'realizada', 'reprogramada'].includes(status) ? status : 'programada') as StatusKey;

  const handleQuickStatus = (cls: ClassSessionDTO, nextStatus: StatusKey) => {
    statusMutation.mutate({ cls, nextStatus });
  };

  const chipFilters = (
    <Stack direction="row" spacing={1} flexWrap="wrap">
      <Chip
        icon={<FilterAltIcon fontSize="small" />}
        label="Todas las materias"
        clickable
        color={subjectFilter === 'all' ? 'primary' : 'default'}
        variant={subjectFilter === 'all' ? 'filled' : 'outlined'}
        onClick={() => setSubjectFilter('all')}
      />
      {subjects.map((s) => (
        <Chip
          key={s.subjectId}
          label={s.name}
          clickable
          onClick={() => setSubjectFilter(s.subjectId)}
          color={subjectFilter === s.subjectId ? 'primary' : 'default'}
          variant={subjectFilter === s.subjectId ? 'filled' : 'outlined'}
          sx={{ mb: 0.5 }}
        />
      ))}
      <Chip
        icon={<FilterAltIcon fontSize="small" />}
        label="Todos los profesores"
        clickable
        color={teacherFilter === 'all' ? 'primary' : 'default'}
        variant={teacherFilter === 'all' ? 'filled' : 'outlined'}
        onClick={() => setTeacherFilter('all')}
      />
      {teachers.map((t) => (
        <Chip
          key={t.teacherId}
          label={t.teacherName}
          clickable
          onClick={() => setTeacherFilter(t.teacherId)}
          color={teacherFilter === t.teacherId ? 'primary' : 'default'}
          variant={teacherFilter === t.teacherId ? 'filled' : 'outlined'}
          sx={{ mb: 0.5 }}
        />
      ))}
      <Chip
        icon={<FilterAltIcon fontSize="small" />}
        label="Todos los estados"
        clickable
        color={statusFilter === 'all' ? 'primary' : 'default'}
        variant={statusFilter === 'all' ? 'filled' : 'outlined'}
        onClick={() => setStatusFilter('all')}
      />
      {(Object.keys(statusMeta) as StatusKey[]).map((status) => (
        <Chip
          key={status}
          label={statusMeta[status].label}
          clickable
          onClick={() => setStatusFilter(status)}
          color={statusFilter === status ? 'primary' : 'default'}
          variant={statusFilter === status ? 'filled' : 'outlined'}
          sx={{ mb: 0.5 }}
        />
      ))}
    </Stack>
  );

  const pushToCalendarWithPrefill = (cls: ClassSessionDTO, subjectName?: string, studentName?: string) => {
    try {
      const prefill = {
        title: `Trial - ${subjectName ?? 'Clase'}`,
        startAt: cls.startAt,
        endAt: cls.endAt,
        customerName: studentName ?? cls.studentName ?? '',
        notes: cls.notes ?? '',
        hint: 'Prefill creado desde Trial lessons',
      };
      if (typeof window !== 'undefined') {
        window.sessionStorage.setItem('booking-prefill', JSON.stringify(prefill));
      }
    } catch {
      // ignore prefill errors; just navigate
    }
    navigate('/estudio/calendario');
  };

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems="flex-start" spacing={2}>
        <Box>
          <Typography variant="overline" color="text.secondary">
            Escuela
          </Typography>
          <Typography variant="h4" fontWeight={800}>
            Trial lessons
          </Typography>
          <Typography color="text.secondary">
            Administra clases de prueba: horario, sala y profesor asignados.
          </Typography>
        </Box>
        <Stack direction="row" spacing={1}>
          <Button variant="outlined" onClick={openStudentDialog}>
            Nuevo alumno
          </Button>
          <Button
            variant="outlined"
            startIcon={<RefreshIcon />}
            onClick={() => void qc.invalidateQueries({ queryKey: ['trial-class-sessions'] })}
          >
            Refrescar
          </Button>
          <Button variant="contained" startIcon={<AddIcon />} onClick={openCreateDialog}>
            Nueva clase
          </Button>
        </Stack>
      </Stack>

      <Paper sx={{ p: 2.5 }} variant="outlined">
        <Stack spacing={2}>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={1.5} alignItems={{ md: 'center' }}>
            <TextField
              label="Desde"
              type="datetime-local"
              value={fromInput}
              onChange={(e) => setFromInput(e.target.value)}
              InputLabelProps={{ shrink: true }}
              fullWidth
            />
            <TextField
              label="Hasta"
              type="datetime-local"
              value={toInput}
              onChange={(e) => setToInput(e.target.value)}
              InputLabelProps={{ shrink: true }}
              fullWidth
            />
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              <Button variant="outlined" size="small" onClick={() => applyRangePreset(7, 30)}>
                Últimos 7d / próximos 30d
              </Button>
              <Button variant="outlined" size="small" onClick={() => applyRangePreset(0, 90)}>
                Próximas 12 semanas
              </Button>
            </Stack>
          </Stack>
          {chipFilters}
          {loading && <LinearProgress />}
          {classesQuery.error && (
            <Alert severity="error">
              {classesQuery.error instanceof Error ? classesQuery.error.message : 'Error al cargar clases.'}
            </Alert>
          )}
          {listError && (
            <Alert severity="warning">
              {listError}
            </Alert>
          )}
          {data.length === 0 && !loading && (
            <Typography color="text.secondary">No hay clases de prueba para este filtro.</Typography>
          )}
          <Stack spacing={1.25}>
            {data.map((cls) => {
              const meta = statusMeta[normalizedStatus(cls.status)];
              const teacher = teachers.find((t) => t.teacherId === cls.teacherId);
              const subject = subjects.find((s) => s.subjectId === cls.subjectId);
              const room = rooms.find((r) => r.roomId === cls.roomId);
              const student = students.find((p) => p.studentId === cls.studentId);
              const rowPending = statusPendingId === cls.classSessionId;
              const hasBooking = Boolean(cls.bookingId);
              const updatedDisplay = cls.updatedAt
                ? formatDateTime(cls.updatedAt)
                : formatDateTime(cls.endAt ?? cls.startAt);
              return (
                <Paper
                  key={cls.classSessionId}
                  variant="outlined"
                  sx={{ p: 1.5, borderRadius: 2, borderColor: meta?.border ?? 'divider' }}
                >
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }}>
                    <Stack direction="row" spacing={1} alignItems="center" sx={{ minWidth: 190 }}>
                      <Chip
                        size="small"
                        icon={meta?.icon}
                        label={meta?.label ?? cls.status}
                        sx={{
                          bgcolor: meta?.bg,
                          color: meta?.color,
                          borderColor: meta?.border,
                          borderWidth: 1,
                          borderStyle: 'solid',
                          fontWeight: 700,
                        }}
                      />
                      <Typography variant="body2" color="text.secondary">
                        {formatDateTime(cls.startAt)}
                      </Typography>
                    </Stack>
                    <Box sx={{ flexGrow: 1 }}>
                      <Typography fontWeight={700}>{subject?.name ?? 'Materia'}</Typography>
                      <Typography variant="body2" color="text.secondary">
                        {teacher?.teacherName ?? cls.teacherName ?? 'Profesor'} · Sala {room?.rName ?? cls.roomName ?? cls.roomId ?? ''}
                      </Typography>
                      <Typography variant="body2" color="text.secondary">
                        Alumno: {student?.displayName ?? cls.studentName ?? cls.studentId}
                      </Typography>
                      <Stack direction="row" spacing={1} alignItems="center" sx={{ mt: 0.5 }}>
                        <Chip
                          size="small"
                          color={hasBooking ? 'success' : 'default'}
                          label={hasBooking ? `Reserva #${cls.bookingId}` : 'Sin reserva vinculada'}
                        />
                        <Button
                          size="small"
                          variant="text"
                          sx={{ textTransform: 'none', minWidth: 0 }}
                          onClick={() => pushToCalendarWithPrefill(cls, subject?.name, student?.displayName)}
                        >
                          {hasBooking ? 'Abrir calendario' : 'Crear en calendario'}
                        </Button>
                      </Stack>
                      {cls.notes && (
                        <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                          {cls.notes}
                        </Typography>
                      )}
                      <Typography variant="caption" color="text.secondary">
                        Última actualización: {updatedDisplay || 'No disponible'}
                      </Typography>
                    </Box>
                    <Stack direction="row" spacing={1} alignItems="center">
                      <Button variant="outlined" size="small" onClick={() => openEditDialog(cls)}>
                        Editar
                      </Button>
                      <Button
                        variant="text"
                        size="small"
                        startIcon={
                          rowPending
                            ? <CircularProgress size={14} />
                            : <CheckCircleIcon fontSize="small" />
                        }
                        onClick={() => handleQuickStatus(cls, 'realizada')}
                        disabled={rowPending || normalizedStatus(cls.status) === 'realizada'}
                      >
                        Realizada
                      </Button>
                      <Button
                        variant="text"
                        size="small"
                        color="inherit"
                        startIcon={
                          rowPending
                            ? <CircularProgress size={14} />
                            : <CancelIcon fontSize="small" />
                        }
                        onClick={() => handleQuickStatus(cls, 'cancelada')}
                        disabled={rowPending || normalizedStatus(cls.status) === 'cancelada'}
                      >
                        Cancelar
                      </Button>
                    </Stack>
                  </Stack>
                </Paper>
              );
            })}
          </Stack>
        </Stack>
      </Paper>

      <Snackbar
        open={Boolean(statusToast)}
        autoHideDuration={3000}
        onClose={() => setStatusToast(null)}
        message={statusToast ?? ''}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
      />

      <Dialog open={dialogOpen} onClose={closeDialog} fullWidth maxWidth="sm">
        <DialogTitle>{editing ? 'Editar clase de prueba' : 'Nueva clase de prueba'}</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ pt: 1 }}>
            <TextField
              select
              label="Alumno"
              value={form.studentId ?? ''}
              onChange={(e) => setForm((prev) => ({ ...prev, studentId: Number(e.target.value) }))}
              fullWidth
            >
              {students.map((p) => (
                <MenuItem key={p.studentId} value={p.studentId}>
                  {p.displayName} {p.email ? `· ${p.email}` : ''}
                </MenuItem>
              ))}
            </TextField>
            <TextField
              select
              label="Materia"
              value={form.subjectId ?? ''}
              onChange={(e) => setForm((prev) => ({ ...prev, subjectId: Number(e.target.value) }))}
              fullWidth
            >
              {subjects.map((s) => (
                <MenuItem key={s.subjectId} value={s.subjectId}>
                  {s.name}
                </MenuItem>
              ))}
            </TextField>
            <TextField
              select
              label="Profesor"
              value={form.teacherId ?? ''}
              onChange={(e) => setForm((prev) => ({ ...prev, teacherId: Number(e.target.value) }))}
              fullWidth
            >
              {teachers.map((t) => (
                <MenuItem key={t.teacherId} value={t.teacherId}>
                  {t.teacherName}
                </MenuItem>
              ))}
            </TextField>
            <TextField
              select
              label="Sala"
              value={form.roomId ?? ''}
              onChange={(e) => setForm((prev) => ({ ...prev, roomId: e.target.value }))}
              fullWidth
            >
              {rooms.map((r) => (
                <MenuItem key={r.roomId} value={r.roomId}>
                  {r.rName}
                </MenuItem>
              ))}
            </TextField>
            <TextField
              select
              label="Estado"
              value={form.status}
              onChange={(e) => setForm((prev) => ({ ...prev, status: e.target.value as StatusKey }))}
              helperText="Se sincroniza con la reserva asociada cuando exista."
              fullWidth
            >
              {statusOptions.map((st) => (
                <MenuItem key={st} value={st}>
                  {statusMeta[st].label}
                </MenuItem>
              ))}
            </TextField>
            <Grid container spacing={2}>
              <Grid item xs={12} sm={6}>
                <TextField
                  label="Inicio"
                  type="datetime-local"
                  value={form.startAt}
                  onChange={(e) => handleDateChange(e.target.value, 45)}
                  fullWidth
                  InputLabelProps={{ shrink: true }}
                />
              </Grid>
              <Grid item xs={12} sm={6}>
                <TextField
                  label="Fin"
                  type="datetime-local"
                  value={form.endAt}
                  onChange={(e) => setForm((prev) => ({ ...prev, endAt: e.target.value }))}
                  fullWidth
                  InputLabelProps={{ shrink: true }}
                />
              </Grid>
            </Grid>
            <TextField
              label="Notas"
              value={form.notes}
              onChange={(e) => setForm((prev) => ({ ...prev, notes: e.target.value }))}
              fullWidth
              multiline
              minRows={2}
            />
            {(createMutation.isError || updateMutation.isError) && (
              <Alert severity="error">
                {createMutation.error instanceof Error
                  ? createMutation.error.message
                  : updateMutation.error instanceof Error
                    ? updateMutation.error.message
                    : 'No se pudo guardar.'}
              </Alert>
            )}
            {formError && <Alert severity="error">{formError}</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={closeDialog}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={() => { void handleSubmit(); }}
            disabled={createMutation.isPending || updateMutation.isPending}
          >
            {editing ? 'Guardar cambios' : 'Crear clase'}
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog open={studentDialogOpen} onClose={closeStudentDialog} fullWidth maxWidth="sm">
        <DialogTitle>Nuevo alumno</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ pt: 1 }}>
            <TextField
              label="Nombre completo"
              value={studentForm.fullName}
              onChange={(e) => setStudentForm((prev) => ({ ...prev, fullName: e.target.value }))}
              fullWidth
            />
            <TextField
              label="Correo"
              type="email"
              value={studentForm.email}
              onChange={(e) => setStudentForm((prev) => ({ ...prev, email: e.target.value }))}
              fullWidth
            />
            <TextField
              label="Teléfono"
              value={studentForm.phone}
              onChange={(e) => setStudentForm((prev) => ({ ...prev, phone: e.target.value }))}
              fullWidth
            />
            <TextField
              label="Notas"
              value={studentForm.notes}
              onChange={(e) => setStudentForm((prev) => ({ ...prev, notes: e.target.value }))}
              fullWidth
              multiline
              minRows={2}
            />
            {studentMutation.isError && (
              <Alert severity="error">
                {studentMutation.error instanceof Error ? studentMutation.error.message : 'No se pudo crear el alumno.'}
              </Alert>
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={closeStudentDialog}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={() => {
              void (async () => {
                const payload = {
                  fullName: studentForm.fullName.trim(),
                  email: studentForm.email.trim(),
                  phone: studentForm.phone.trim() || undefined,
                  notes: studentForm.notes.trim() || undefined,
                };
                await studentMutation.mutateAsync(payload);
                closeStudentDialog();
              })();
            }}
            disabled={studentMutation.isPending}
          >
            Crear alumno
          </Button>
        </DialogActions>
      </Dialog>
    </Stack>
  );
}
