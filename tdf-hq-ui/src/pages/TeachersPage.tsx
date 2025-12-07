import { useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Autocomplete,
  Alert,
  Avatar,
  Box,
  Button,
  Card,
  CardActionArea,
  CardContent,
  Chip,
  Divider,
  Grid,
  LinearProgress,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import SearchIcon from '@mui/icons-material/Search';
import FilterAltIcon from '@mui/icons-material/FilterAlt';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import CancelIcon from '@mui/icons-material/Cancel';
import AutoAwesomeIcon from '@mui/icons-material/AutoAwesome';
import type { TrialSlot, TrialSubject, TeacherDTO, ClassSessionDTO } from '../api/trials';
import { Trials } from '../api/trials';
import { Parties } from '../api/parties';

type ClassStatus = 'programada' | 'por-confirmar' | 'cancelada' | 'realizada' | 'reprogramada';

interface TeacherProfile {
  id: number;
  name: string;
  subjects: number[];
  headline?: string;
  focus?: string;
  color?: string;
}

interface ClassRow {
  id: string;
  teacherId: number;
  subjectId: number;
  title: string;
  student: string;
  startAt: string;
  endAt: string;
  status: ClassStatus;
  location?: string;
  notes?: string;
}

const statusMeta: Record<ClassStatus, { label: string; color: string; bg: string; border: string; icon: JSX.Element }> = {
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

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const buildDemoClasses = (): ClassRow[] => {
  const base = new Date();
  const dateAt = (daysFromNow: number, hour: number, minute: number, durationMinutes: number) => {
    const start = new Date(base);
    start.setDate(base.getDate() + daysFromNow);
    start.setHours(hour, minute, 0, 0);
    const end = new Date(start.getTime() + durationMinutes * 60 * 1000);
    return { startAt: start.toISOString(), endAt: end.toISOString() };
  };

  return [
    {
      id: 'demo-1',
      teacherId: 101,
      subjectId: 1,
      title: 'Beatmaking intro',
      student: 'Ana Torres',
      ...dateAt(0, 15, 0, 75),
      status: 'programada',
      location: 'Sala A',
      notes: 'Traer proyecto de Ableton.',
    },
    {
      id: 'demo-2',
      teacherId: 101,
      subjectId: 5,
      title: 'Live set review',
      student: 'Josué Pérez',
      ...dateAt(2, 18, 30, 60),
      status: 'por-confirmar',
      location: 'Cabina Domo',
    },
    {
      id: 'demo-3',
      teacherId: 102,
      subjectId: 3,
      title: 'Afinación y respiración',
      student: 'Valeria Ruiz',
      ...dateAt(-1, 11, 0, 60),
      status: 'realizada',
      location: 'Sala B',
    },
    {
      id: 'demo-4',
      teacherId: 102,
      subjectId: 4,
      title: 'Armonía aplicada',
      student: 'Camila Ortega',
      ...dateAt(3, 9, 30, 60),
      status: 'programada',
      location: 'Aula 2',
    },
    {
      id: 'demo-5',
      teacherId: 103,
      subjectId: 2,
      title: 'Timing y groove',
      student: 'Diego Jara',
      ...dateAt(1, 14, 0, 60),
      status: 'reprogramada',
      location: 'Sala C',
      notes: 'Mover a viernes si se libera espacio.',
    },
    {
      id: 'demo-6',
      teacherId: 103,
      subjectId: 5,
      title: 'Preparación showcase',
      student: 'Banda Ventana',
      ...dateAt(-3, 17, 0, 90),
      status: 'cancelada',
      location: 'Escenario',
      notes: 'Alumno canceló por viaje.',
    },
    {
      id: 'demo-7',
      teacherId: 104,
      subjectId: 4,
      title: 'Lectura a primera vista',
      student: 'María Molina',
      ...dateAt(4, 16, 0, 60),
      status: 'programada',
      location: 'Piano room',
    },
    {
      id: 'demo-8',
      teacherId: 104,
      subjectId: 4,
      title: 'Interpretación pieza pop',
      student: 'Grupo coral',
      ...dateAt(-5, 10, 30, 60),
      status: 'realizada',
      location: 'Aula 1',
    },
  ];
};

const formatDate = (iso: string) => {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return '';
  return d.toLocaleDateString('es-EC', { weekday: 'short', month: 'short', day: 'numeric' });
};

const formatRange = (startIso: string, endIso: string) => {
  const start = new Date(startIso);
  const end = new Date(endIso);
  if (Number.isNaN(start.getTime()) || Number.isNaN(end.getTime())) return '';
  const startLabel = start.toLocaleTimeString('es-EC', { hour: '2-digit', minute: '2-digit' });
  const endLabel = end.toLocaleTimeString('es-EC', { hour: '2-digit', minute: '2-digit' });
  return `${startLabel} - ${endLabel}`;
};

const buildTeachersFromSlots = (slots: TrialSlot[]): TeacherProfile[] => {
  if (!slots.length) return [];
  const map = new Map<number, { name: string; subjects: Set<number> }>();
  slots.forEach((slot) => {
    const entry = map.get(slot.teacherId) ?? { name: slot.teacherName, subjects: new Set<number>() };
    entry.subjects.add(slot.subjectId);
    map.set(slot.teacherId, entry);
  });
  return Array.from(map.entries()).map(([id, data]) => ({
    id,
    name: data.name,
    subjects: Array.from(data.subjects),
    headline: 'Disponibilidad publicada para trial',
    focus: 'Configurar materias',
    color: '#0ea5e9',
  }));
};

const buildTeachersFromDTO = (teachers: TeacherDTO[]): TeacherProfile[] => {
  if (!teachers.length) return [];
  const palette = ['#6366f1', '#ec4899', '#22c55e', '#06b6d4', '#f97316', '#14b8a6'];
  return teachers.map((teacher, idx) => ({
    id: teacher.teacherId,
    name: teacher.teacherName,
    subjects: teacher.subjects.map((s) => s.subjectId),
    headline: 'Profesor',
    focus: teacher.subjects.map((s) => s.name).slice(0, 3).join(' · ') || undefined,
    color: palette[idx % palette.length],
  }));
};

const buildTeachersFromParties = (parties: { partyId: number; displayName: string; roles?: string[] }[]): TeacherProfile[] => {
  if (!parties.length) return [];
  const palette = ['#2563eb', '#0ea5e9', '#22c55e', '#f59e0b', '#8b5cf6', '#14b8a6'];
  const isTeacher = (roles?: string[]) =>
    (roles ?? []).some((r) => r.toLowerCase() === 'teacher');

  return parties
    .filter((p) => isTeacher(p.roles))
    .map((p, idx) => ({
      id: p.partyId,
      name: p.displayName,
      subjects: [],
      headline: 'Profesor',
      focus: 'Rol Teacher asignado',
      color: palette[idx % palette.length],
    }));
};

const buildClassesFromSlots = (slots: TrialSlot[], subjectMap: Map<number, string>): ClassRow[] => {
  if (!slots.length) return [];
  const statuses: ClassStatus[] = ['por-confirmar', 'programada', 'programada', 'realizada', 'cancelada'];
  let idx = 0;
  const rows: ClassRow[] = [];
  slots.forEach((slot) => {
    slot.slots.forEach((s) => {
      const status: ClassStatus = statuses[idx % statuses.length] ?? 'programada';
      rows.push({
        id: `slot-${slot.teacherId}-${slot.subjectId}-${idx}`,
        teacherId: slot.teacherId,
        subjectId: slot.subjectId,
        title: subjectMap.get(slot.subjectId) ? `Trial: ${subjectMap.get(slot.subjectId)}` : 'Clase asignada',
        student: status === 'por-confirmar' ? 'Alumno por asignar' : 'En confirmación',
        startAt: s.startAt,
        endAt: s.endAt,
        status,
        location: 'Por definir',
      });
      idx += 1;
    });
  });
  return rows;
};

const normalizeStatus = (status: string): ClassStatus => {
  const allowed: ClassStatus[] = ['programada', 'por-confirmar', 'cancelada', 'realizada', 'reprogramada'];
  return allowed.includes(status as ClassStatus) ? (status as ClassStatus) : 'programada';
};

const buildClassesFromDTO = (classes: ClassSessionDTO[]): ClassRow[] =>
  classes.map((cls) => ({
    id: `class-${cls.classSessionId}`,
    teacherId: cls.teacherId,
    subjectId: cls.subjectId,
    title: cls.subjectName ? `Clase: ${cls.subjectName}` : 'Clase programada',
    student: cls.studentName ?? 'Alumno asignado',
    startAt: cls.startAt,
    endAt: cls.endAt,
    status: normalizeStatus(cls.status),
    location: cls.roomName ?? undefined,
    notes: cls.notes ?? undefined,
  }));

export default function TeachersPage() {
  const qc = useQueryClient();
  const subjectsQuery = useQuery({
    queryKey: ['trial-subjects-for-teachers'],
    queryFn: Trials.listSubjects,
  });
  const teachersQuery = useQuery({
    queryKey: ['trial-teachers'],
    queryFn: Trials.listTeachers,
  });
  const teacherPartiesQuery = useQuery({
    queryKey: ['party-teachers'],
    queryFn: Parties.list,
    enabled: !teachersQuery.isLoading && ((teachersQuery.data ?? []).length === 0),
  });
  const slotsQuery = useQuery({
    queryKey: ['trial-slots-teachers'],
    queryFn: () => Trials.listSlots(),
  });

  const subjects = useMemo<TrialSubject[]>(() => {
    const activeSubjects = (subjectsQuery.data ?? []).filter((s) => s.active);
    return activeSubjects;
  }, [subjectsQuery.data]);
  const subjectMap = useMemo(() => new Map(subjects.map((s) => [s.subjectId, s.name])), [subjects]);

  const teachersFromDTO = useMemo(() => buildTeachersFromDTO(teachersQuery.data ?? []), [teachersQuery.data]);
  const teachersFromParties = useMemo(
    () => buildTeachersFromParties(teacherPartiesQuery.data ?? []),
    [teacherPartiesQuery.data],
  );
  const teachersFromSlots = useMemo(() => buildTeachersFromSlots(slotsQuery.data ?? []), [slotsQuery.data]);

  const teachers: TeacherProfile[] = useMemo(() => {
    if (teachersFromDTO.length) return teachersFromDTO;
    if (teachersFromParties.length) return teachersFromParties;
    return teachersFromSlots;
  }, [teachersFromDTO, teachersFromParties, teachersFromSlots]);
  const usingDemoTeachers = teachers.length === 0;

  const classesFromSlots = useMemo(() => buildClassesFromSlots(slotsQuery.data ?? [], subjectMap), [slotsQuery.data, subjectMap]);
  const dateWindow = useMemo(() => {
    const now = new Date();
    const from = new Date(now);
    from.setDate(now.getDate() - 14);
    const to = new Date(now);
    to.setDate(now.getDate() + 30);
    return { from: from.toISOString(), to: to.toISOString() };
  }, []);

  const [subjectFilter, setSubjectFilter] = useState<number | 'all'>('all');
  const [search, setSearch] = useState('');
  const [isEditingSubjects, setIsEditingSubjects] = useState(false);
  const [selectedSubjectIds, setSelectedSubjectIds] = useState<number[]>([]);

  const filteredTeachers = useMemo(() => {
    const bySubject = (teacher: TeacherProfile) =>
      subjectFilter === 'all' || teacher.subjects.includes(subjectFilter);
    const byName = (teacher: TeacherProfile) =>
      teacher.name.toLowerCase().includes(search.trim().toLowerCase());
    return teachers.filter((t) => bySubject(t) && byName(t));
  }, [teachers, subjectFilter, search]);

  const [selectedTeacherId, setSelectedTeacherId] = useState<number | null>(filteredTeachers[0]?.id ?? null);
  useEffect(() => {
    if (filteredTeachers.length === 0) {
      setSelectedTeacherId(null);
      return;
    }
    if (!selectedTeacherId || !filteredTeachers.some((t) => t.id === selectedTeacherId)) {
      setSelectedTeacherId(filteredTeachers[0]?.id ?? null);
    }
  }, [filteredTeachers, selectedTeacherId]);

  const teacherClassesQuery = useQuery({
    queryKey: ['teacher-classes', selectedTeacherId, subjectFilter, dateWindow.from, dateWindow.to],
    queryFn: () =>
      Trials.listTeacherClasses(selectedTeacherId!, {
        subjectId: subjectFilter === 'all' ? undefined : subjectFilter,
        from: dateWindow.from,
        to: dateWindow.to,
      }),
    enabled: Boolean(selectedTeacherId),
  });

  const classesFromApi = useMemo(
    () => buildClassesFromDTO(teacherClassesQuery.data ?? []),
    [teacherClassesQuery.data],
  );
  const classes = classesFromApi.length ? classesFromApi : classesFromSlots;
  const usingDemoClasses = classes.length === 0;

  const selectedTeacher = filteredTeachers.find((t) => t.id === selectedTeacherId) ?? null;
  useEffect(() => {
    if (selectedTeacher) {
      setSelectedSubjectIds(selectedTeacher.subjects);
      setIsEditingSubjects(false);
    } else {
      setSelectedSubjectIds([]);
    }
  }, [selectedTeacher]);

  const teacherClasses = useMemo(() => {
    if (!selectedTeacherId) return [];
    return classes
      .filter((c) => c.teacherId === selectedTeacherId)
      .filter((c) => subjectFilter === 'all' || c.subjectId === subjectFilter)
      .sort((a, b) => new Date(a.startAt).getTime() - new Date(b.startAt).getTime());
  }, [classes, selectedTeacherId, subjectFilter]);

  const upcoming = teacherClasses.filter((c) =>
    c.status === 'programada' || c.status === 'por-confirmar' || c.status === 'reprogramada'
  );
  const history = teacherClasses.filter((c) => c.status === 'realizada' || c.status === 'cancelada');

  const loading =
    subjectsQuery.isLoading ||
    slotsQuery.isLoading ||
    teachersQuery.isLoading ||
    teacherPartiesQuery.isFetching ||
    teacherClassesQuery.isFetching;
  const canEditSubjects = Boolean(selectedTeacherId) && !usingDemoTeachers;

  const updateSubjectsMutation = useMutation({
    mutationFn: (subjectIds: number[]) =>
      Trials.updateTeacherSubjects(selectedTeacherId!, { subjectIds }),
    onSuccess: (data) => {
      setIsEditingSubjects(false);
      setSelectedSubjectIds(data.subjects.map((s) => s.subjectId));
      void qc.invalidateQueries({ queryKey: ['trial-teachers'] });
      void qc.invalidateQueries({ queryKey: ['trial-slots-teachers'] });
      void qc.invalidateQueries({ queryKey: ['teacher-classes'] });
    },
  });

  return (
    <Stack spacing={3}>
      <Box
        sx={{
          p: { xs: 2.5, md: 3 },
          borderRadius: 3,
          background: 'linear-gradient(135deg, #0f172a, #1e293b)',
          color: '#e2e8f0',
          boxShadow: '0 16px 60px rgba(15,23,42,0.4)',
        }}
      >
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} alignItems="center" justifyContent="space-between">
          <Box>
            <Typography variant="overline" sx={{ color: 'rgba(226,232,240,0.7)', letterSpacing: 1 }}>
              Escuela
            </Typography>
            <Typography variant="h4" fontWeight={800}>
              Profesores y clases
            </Typography>
            <Typography variant="body1" sx={{ color: 'rgba(226,232,240,0.82)' }}>
              Filtra por materia, elige un profesor y revisa rápidamente sus clases y estado.
            </Typography>
          </Box>
          <Stack direction="row" spacing={1} alignItems="center">
            <Chip
              label={`${teachers.length} profesores`}
              sx={{ bgcolor: 'rgba(226,232,240,0.12)', color: '#e2e8f0', borderColor: 'rgba(226,232,240,0.25)' }}
              variant="outlined"
            />
            <Chip
              label={`${subjects.length} materias`}
              sx={{ bgcolor: 'rgba(226,232,240,0.12)', color: '#e2e8f0', borderColor: 'rgba(226,232,240,0.25)' }}
              variant="outlined"
            />
          </Stack>
        </Stack>
      </Box>

      {(usingDemoTeachers || usingDemoClasses) && (
        <Alert severity="info" sx={{ borderRadius: 2 }}>
          No hay datos todavía para profesores o clases. Agrega profesores (rol Teacher) y clases en el módulo de escuela para verlos aquí.
        </Alert>
      )}

      <Paper sx={{ p: 2.5, borderRadius: 2.5 }} variant="outlined">
        <Grid container spacing={2} alignItems="center">
          <Grid item xs={12} md={4}>
            <TextField
              fullWidth
              size="small"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Buscar profesor"
              InputProps={{
                startAdornment: <SearchIcon fontSize="small" sx={{ mr: 1, color: 'text.secondary' }} />,
              }}
            />
          </Grid>
          <Grid item xs={12} md={8}>
            <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
              <Chip
                icon={<FilterAltIcon fontSize="small" />}
                label="Todas las materias"
                clickable
                color={subjectFilter === 'all' ? 'primary' : 'default'}
                variant={subjectFilter === 'all' ? 'filled' : 'outlined'}
                onClick={() => setSubjectFilter('all')}
              />
              {subjects.map((subject) => (
                <Chip
                  key={subject.subjectId}
                  label={subject.name}
                  clickable
                  onClick={() => setSubjectFilter(subject.subjectId)}
                  color={subjectFilter === subject.subjectId ? 'primary' : 'default'}
                  variant={subjectFilter === subject.subjectId ? 'filled' : 'outlined'}
                  sx={{ mb: 0.5 }}
                />
              ))}
            </Stack>
          </Grid>
        </Grid>
      </Paper>

      {loading && <LinearProgress />}

      <Grid container spacing={3}>
        <Grid item xs={12} md={4}>
          <Card variant="outlined" sx={{ borderRadius: 2.5, height: '100%' }}>
            <CardContent>
              <Stack spacing={2}>
                <Typography variant="h6" fontWeight={700}>
                  Profesores
                </Typography>
                <Stack spacing={1.5}>
                  {filteredTeachers.length === 0 && (
                    <Typography color="text.secondary">No hay profesores para este filtro.</Typography>
                  )}
                  {filteredTeachers.map((teacher) => (
                    <Card
                      key={teacher.id}
                      variant={teacher.id === selectedTeacherId ? 'outlined' : undefined}
                      sx={{
                        borderRadius: 2,
                        borderColor: teacher.id === selectedTeacherId ? 'primary.main' : 'divider',
                        bgcolor: teacher.id === selectedTeacherId ? 'rgba(59,130,246,0.04)' : 'background.paper',
                      }}
                    >
                      <CardActionArea onClick={() => setSelectedTeacherId(teacher.id)} sx={{ p: 1.5 }}>
                        <Stack direction="row" spacing={1.5} alignItems="center">
                          <Avatar
                            sx={{
                              bgcolor: teacher.color ?? '#2563eb',
                              color: '#fff',
                              width: 44,
                              height: 44,
                              fontWeight: 700,
                            }}
                          >
                            {teacher.name.charAt(0)}
                          </Avatar>
                          <Box sx={{ flexGrow: 1 }}>
                            <Typography fontWeight={700}>{teacher.name}</Typography>
                            <Typography variant="body2" color="text.secondary">
                              {teacher.headline ?? 'Profesor'}
                            </Typography>
                            {teacher.subjects.length > 0 ? (
                              <Stack direction="row" spacing={0.5} mt={0.5} flexWrap="wrap">
                                {teacher.subjects.map((sid) => (
                                  <Chip key={sid} label={subjectMap.get(sid) ?? `Materia ${sid}`} size="small" variant="outlined" />
                                ))}
                              </Stack>
                            ) : (
                              <Typography variant="caption" color="text.secondary">
                                Sin materias asignadas
                              </Typography>
                            )}
                          </Box>
                        </Stack>
                      </CardActionArea>
                    </Card>
                  ))}
                </Stack>
              </Stack>
            </CardContent>
          </Card>
        </Grid>

        <Grid item xs={12} md={8}>
          <Card variant="outlined" sx={{ borderRadius: 2.5 }}>
            <CardContent>
              {selectedTeacher ? (
                <Stack spacing={2}>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} justifyContent="space-between">
                    <Box>
                      <Typography variant="h6" fontWeight={800}>
                        {selectedTeacher.name}
                      </Typography>
                      {selectedTeacher.headline && (
                        <Typography variant="body2" color="text.secondary">
                          {selectedTeacher.headline}
                        </Typography>
                      )}
                      {selectedTeacher.focus && (
                      <Typography variant="body2" color="text.secondary">
                        {selectedTeacher.focus}
                      </Typography>
                    )}
                    <Stack direction="row" spacing={0.5} mt={1} flexWrap="wrap">
                      {selectedTeacher.subjects.length > 0 ? (
                        selectedTeacher.subjects.map((sid) => (
                          <Chip key={sid} label={subjectMap.get(sid) ?? `Materia ${sid}`} size="small" />
                        ))
                      ) : (
                        <Typography variant="body2" color="text.secondary">
                          Sin materias asignadas
                        </Typography>
                      )}
                    </Stack>
                  </Box>
                    <Stack spacing={1} alignItems={{ xs: 'flex-start', sm: 'flex-end' }}>
                      <Chip
                        label={`${upcoming.length} próximas`}
                        sx={{ bgcolor: 'rgba(14,165,233,0.12)', color: '#0369a1', borderColor: 'rgba(14,165,233,0.35)' }}
                        variant="outlined"
                      />
                      <Chip
                        label={`${history.length} históricas`}
                        sx={{ bgcolor: 'rgba(107,114,128,0.12)', color: '#1f2937', borderColor: 'rgba(107,114,128,0.35)' }}
                        variant="outlined"
                      />
                    </Stack>
                  </Stack>

                  <Stack spacing={1}>
                    <Typography variant="subtitle1" fontWeight={700}>
                      Materias
                    </Typography>
                    {isEditingSubjects ? (
                      <Stack spacing={1}>
                        <Autocomplete
                          multiple
                          disableCloseOnSelect
                          options={subjects}
                          getOptionLabel={(option) => option.name}
                          value={subjects.filter((s) => selectedSubjectIds.includes(s.subjectId))}
                          onChange={(_e, value) => setSelectedSubjectIds(value.map((v) => v.subjectId))}
                          renderInput={(params) => <TextField {...params} label="Materias asignadas" placeholder="Selecciona materias" />}
                        />
                        <Stack direction="row" spacing={1}>
                          <Button
                            variant="contained"
                            onClick={() => updateSubjectsMutation.mutate(selectedSubjectIds)}
                            disabled={updateSubjectsMutation.isPending}
                          >
                            Guardar materias
                          </Button>
                          <Button
                            variant="text"
                            onClick={() => {
                              setSelectedSubjectIds(selectedTeacher.subjects);
                              setIsEditingSubjects(false);
                            }}
                          >
                            Cancelar
                          </Button>
                        </Stack>
                        {updateSubjectsMutation.isError && (
                          <Alert severity="error">
                            {updateSubjectsMutation.error instanceof Error
                              ? updateSubjectsMutation.error.message
                              : 'No se pudo actualizar las materias.'}
                          </Alert>
                        )}
                        {updateSubjectsMutation.isSuccess && <Alert severity="success">Materias actualizadas.</Alert>}
                      </Stack>
                    ) : (
                      <Stack direction="row" spacing={0.5} flexWrap="wrap" alignItems="center">
                        {selectedTeacher.subjects.map((sid) => (
                          <Chip key={sid} label={subjectMap.get(sid) ?? `Materia ${sid}`} size="small" />
                        ))}
                        <Button
                          size="small"
                          variant="outlined"
                          onClick={() => setIsEditingSubjects(true)}
                          disabled={!canEditSubjects}
                        >
                          Editar materias
                        </Button>
                      </Stack>
                    )}
                  </Stack>

                  <Divider />

                  <Stack spacing={1}>
                    <Typography variant="subtitle1" fontWeight={700}>
                      Clases y estado
                    </Typography>
                    {teacherClasses.length === 0 && (
                      <Typography color="text.secondary">
                        No hay clases para este profesor con el filtro actual. Ajusta las fechas o agenda una clase nueva.
                      </Typography>
                    )}
                    {teacherClasses.map((row) => {
                      const meta = statusMeta[row.status];
                      return (
                        <Paper
                          key={row.id}
                          variant="outlined"
                          sx={{
                            p: 1.5,
                            borderRadius: 2,
                            borderColor: meta?.border ?? 'divider',
                          }}
                        >
                          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'flex-start', sm: 'center' }}>
                            <Stack direction="row" spacing={1} alignItems="center" sx={{ minWidth: 180 }}>
                              <Chip
                                size="small"
                                icon={meta?.icon}
                                label={meta?.label ?? row.status}
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
                                {formatDate(row.startAt)}
                              </Typography>
                            </Stack>
                            <Box sx={{ flexGrow: 1 }}>
                              <Typography fontWeight={700}>{row.title}</Typography>
                              <Typography variant="body2" color="text.secondary">
                                {subjectMap.get(row.subjectId) ?? 'Materia'} · {row.student}
                              </Typography>
                              <Typography variant="body2" color="text.secondary">
                                {formatRange(row.startAt, row.endAt)} {row.location ? `· ${row.location}` : ''}
                              </Typography>
                              {row.notes && (
                                <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                                  {row.notes}
                                </Typography>
                              )}
                            </Box>
                          </Stack>
                        </Paper>
                      );
                    })}
                  </Stack>
                </Stack>
              ) : (
                <Typography color="text.secondary">Selecciona un profesor para ver sus clases.</Typography>
              )}
            </CardContent>
          </Card>
        </Grid>
      </Grid>
    </Stack>
  );
}
