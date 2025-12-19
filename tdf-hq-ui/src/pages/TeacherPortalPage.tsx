import { useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  Grid,
  LinearProgress,
  MenuItem,
  Paper,
  Snackbar,
  Stack,
  Tab,
  Tabs,
  TextField,
  Typography,
} from '@mui/material';
import SchoolIcon from '@mui/icons-material/School';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import PeopleIcon from '@mui/icons-material/People';
import CalendarMonthIcon from '@mui/icons-material/CalendarMonth';
import { Link as RouterLink } from 'react-router-dom';
import type { RoomDTO } from '../api/types';
import { Rooms } from '../api/rooms';
import {
  Trials,
  type ClassSessionDTO,
  type ClassSessionUpdate,
  type StudentDTO,
  type StudentUpdate,
  type TrialAvailabilitySlotDTO,
  type TrialAvailabilityUpsert,
  type TrialSubject,
} from '../api/trials';
import { useSession } from '../session/SessionContext';

type PortalTab = 'agenda' | 'students' | 'subjects' | 'availability';

const toLocalInput = (iso?: string | null) => {
  if (!iso) return '';
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return '';
  const pad = (v: number) => String(v).padStart(2, '0');
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}T${pad(d.getHours())}:${pad(d.getMinutes())}`;
};

const toIsoOrNull = (localValue: string) => {
  if (!localValue.trim()) return null;
  const d = new Date(localValue);
  if (Number.isNaN(d.getTime())) return null;
  return d.toISOString();
};

const parseLocalDateTime = (value: string): Date | null => {
  const trimmed = value.trim();
  if (!trimmed) return null;
  const d = new Date(trimmed);
  if (Number.isNaN(d.getTime())) return null;
  return d;
};

const computeDurationMs = (startValue: string, endValue: string, fallbackMinutes = 60): number => {
  const start = parseLocalDateTime(startValue);
  const end = parseLocalDateTime(endValue);
  if (start && end && end > start) return end.getTime() - start.getTime();
  return fallbackMinutes * 60_000;
};

const addDurationToLocalStart = (startValue: string, durationMs: number): string | null => {
  const start = parseLocalDateTime(startValue);
  if (!start) return null;
  const end = new Date(start.getTime() + durationMs);
  return toLocalInput(end.toISOString());
};

const validateLocalRange = (startValue: string, endValue: string): string | null => {
  const start = parseLocalDateTime(startValue);
  if (!start) return 'Inicio inválido.';
  const end = parseLocalDateTime(endValue);
  if (!end) return 'Fin inválido.';
  if (end <= start) return 'El fin debe ser posterior al inicio.';
  return null;
};

const formatDateTime = (iso: string) => {
  const d = new Date(iso);
  if (Number.isNaN(d.getTime())) return '';
  return d.toLocaleString('es-EC', {
    weekday: 'short',
    month: 'short',
    day: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  });
};

const TabPill = ({ active, label }: { active: boolean; label: string }) => (
  <Chip
    label={label}
    size="small"
    sx={{
      bgcolor: active ? 'rgba(59,130,246,0.18)' : 'rgba(148,163,184,0.12)',
      color: active ? '#e2e8f0' : 'rgba(226,232,240,0.75)',
      borderColor: active ? 'rgba(59,130,246,0.4)' : 'rgba(226,232,240,0.12)',
    }}
    variant="outlined"
  />
);

export default function TeacherPortalPage() {
  const qc = useQueryClient();
  const { session } = useSession();
  const teacherId = session?.partyId ?? null;

  const getErrorMessage = (error: unknown, fallback: string) =>
    error instanceof Error ? error.message : fallback;

  const [toast, setToast] = useState<{
    message: string;
    severity: 'success' | 'info' | 'warning' | 'error';
  } | null>(null);
  const closeToast = (_?: unknown, reason?: string) => {
    if (reason === 'clickaway') return;
    setToast(null);
  };
  const showToast = (message: string, severity: 'success' | 'info' | 'warning' | 'error' = 'success') => {
    setToast({ message, severity });
  };

  const [tab, setTab] = useState<PortalTab>('agenda');
  type ScheduleView = 'today' | 'next24h' | 'week' | 'all';
  const [agendaView, setAgendaView] = useState<ScheduleView>('week');
  const [availabilityView, setAvailabilityView] = useState<ScheduleView>('week');

  const subjectsQuery = useQuery({
    queryKey: ['teacher-portal', 'subjects'],
    queryFn: Trials.listSubjects,
  });
  const roomsQuery = useQuery({
    queryKey: ['rooms'],
    queryFn: Rooms.list,
  });

  const teachersQuery = useQuery({
    queryKey: ['teacher-portal', 'teachers'],
    queryFn: Trials.listTeachers,
    enabled: Boolean(teacherId),
  });

  const activeSubjects = useMemo<TrialSubject[]>(
    () => (subjectsQuery.data ?? []).filter((s) => s.active),
    [subjectsQuery.data],
  );
  const subjectById = useMemo(() => new Map(activeSubjects.map((s) => [s.subjectId, s])), [activeSubjects]);
  const rooms = useMemo(() => roomsQuery.data ?? [], [roomsQuery.data]);
  const roomNameById = useMemo(() => new Map(rooms.map((r) => [r.roomId, r.rName])), [rooms]);

  const me = useMemo(
    () => (teachersQuery.data ?? []).find((t) => t.teacherId === teacherId) ?? null,
    [teacherId, teachersQuery.data],
  );

  const [selectedSubjectIds, setSelectedSubjectIds] = useState<number[]>([]);
  useEffect(() => {
    if (!me) return;
    setSelectedSubjectIds(me.subjects.map((s) => s.subjectId));
  }, [me]);

  const updateSubjectsMutation = useMutation({
    mutationFn: (subjectIds: number[]) => Trials.updateTeacherSubjects(teacherId!, { subjectIds }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['teacher-portal', 'teachers'] });
      void qc.invalidateQueries({ queryKey: ['teacher-availability'] });
      void qc.invalidateQueries({ queryKey: ['teacher-classes'] });
      showToast('Materias guardadas.', 'success');
    },
    onError: (error) => {
      showToast(getErrorMessage(error, 'No pudimos guardar las materias.'), 'error');
    },
  });

  const allowedRoomsForSubject = useMemo(() => {
    const allowed = new Set<string>();
    selectedSubjectIds.forEach((sid) => {
      const subject = subjectById.get(sid);
      subject?.roomIds.forEach((rid) => allowed.add(rid));
    });
    return allowed;
  }, [selectedSubjectIds, subjectById]);

  const [studentDialogOpen, setStudentDialogOpen] = useState(false);
  const [studentEditDialogOpen, setStudentEditDialogOpen] = useState(false);
  const [editingStudent, setEditingStudent] = useState<StudentDTO | null>(null);
  const [studentForm, setStudentForm] = useState({ fullName: '', email: '', phone: '' });
  const [studentEditForm, setStudentEditForm] = useState({ displayName: '', email: '', phone: '' });
  const [studentDialogError, setStudentDialogError] = useState<string | null>(null);
  const [studentEditDialogError, setStudentEditDialogError] = useState<string | null>(null);

  const studentsQuery = useQuery({
    queryKey: ['teacher-students', teacherId],
    queryFn: () => Trials.listTeacherStudents(teacherId!),
    enabled: Boolean(teacherId),
  });

  const createStudentMutation = useMutation({
    mutationFn: (payload: { fullName: string; email: string; phone?: string }) => Trials.createStudent(payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['teacher-students'] });
    },
  });

  const updateStudentMutation = useMutation({
    mutationFn: (params: { studentId: number; patch: StudentUpdate }) => Trials.updateStudent(params.studentId, params.patch),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['teacher-students'] });
    },
  });

  const [availabilityDialogOpen, setAvailabilityDialogOpen] = useState(false);
  const [editingAvailability, setEditingAvailability] = useState<TrialAvailabilitySlotDTO | null>(null);
  const [availabilityDialogError, setAvailabilityDialogError] = useState<string | null>(null);
  const [availabilityForm, setAvailabilityForm] = useState<{
    subjectId: number | '';
    roomId: string;
    startAt: string;
    endAt: string;
    notes: string;
  }>({ subjectId: '', roomId: '', startAt: '', endAt: '', notes: '' });
  const availabilityTimeError = useMemo(
    () => (availabilityDialogOpen ? validateLocalRange(availabilityForm.startAt, availabilityForm.endAt) : null),
    [availabilityDialogOpen, availabilityForm.endAt, availabilityForm.startAt],
  );

  const availabilityRange = useMemo(() => {
    const now = new Date();
    const from = new Date(now);
    from.setDate(now.getDate() - 7);
    from.setHours(0, 0, 0, 0);
    const to = new Date(now);
    to.setDate(now.getDate() + 60);
    to.setHours(23, 59, 0, 0);
    return { from: from.toISOString(), to: to.toISOString() };
  }, []);

  const availabilityQuery = useQuery({
    queryKey: ['teacher-availability', availabilityRange.from, availabilityRange.to],
    queryFn: () => Trials.listAvailabilitySlots({ from: availabilityRange.from, to: availabilityRange.to }),
    enabled: Boolean(teacherId),
  });

  const upsertAvailabilityMutation = useMutation({
    mutationFn: (payload: TrialAvailabilityUpsert) => Trials.upsertAvailabilitySlot(payload),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['teacher-availability'] });
      void qc.invalidateQueries({ queryKey: ['teacher-portal', 'subjects'] });
    },
  });

  const deleteAvailabilityMutation = useMutation({
    mutationFn: (availabilityId: number) => Trials.deleteAvailabilitySlot(availabilityId),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['teacher-availability'] });
      showToast('Disponibilidad eliminada.', 'success');
    },
    onError: (error) => {
      showToast(getErrorMessage(error, 'No pudimos eliminar la disponibilidad.'), 'error');
    },
  });

  const [classDialogOpen, setClassDialogOpen] = useState(false);
  const [editingClass, setEditingClass] = useState<ClassSessionDTO | null>(null);
  const [classDialogError, setClassDialogError] = useState<string | null>(null);
  const [classForm, setClassForm] = useState<{
    subjectId: number | '';
    studentId: number | '';
    roomId: string;
    startAt: string;
    endAt: string;
    notes: string;
  }>({ subjectId: '', studentId: '', roomId: '', startAt: '', endAt: '', notes: '' });
  const classTimeError = useMemo(
    () => (classDialogOpen ? validateLocalRange(classForm.startAt, classForm.endAt) : null),
    [classDialogOpen, classForm.endAt, classForm.startAt],
  );

  const classRange = useMemo(() => {
    const now = new Date();
    const from = new Date(now);
    from.setDate(now.getDate() - 14);
    from.setHours(0, 0, 0, 0);
    const to = new Date(now);
    to.setDate(now.getDate() + 60);
    to.setHours(23, 59, 0, 0);
    return { from: from.toISOString(), to: to.toISOString() };
  }, []);

  const classesQuery = useQuery({
    queryKey: ['teacher-classes', teacherId, classRange.from, classRange.to],
    queryFn: () => Trials.listTeacherClasses(teacherId!, { from: classRange.from, to: classRange.to }),
    enabled: Boolean(teacherId),
  });

  const createClassMutation = useMutation({
    mutationFn: async (payload: { subjectId: number; studentId: number; roomId: string; startIso: string; endIso: string }) => {
      const roomIdNum = Number.parseInt(payload.roomId, 10);
      if (!Number.isFinite(roomIdNum)) {
        throw new Error('Sala inválida.');
      }
      return Trials.createClassSession({
        subjectId: payload.subjectId,
        studentId: payload.studentId,
        teacherId: teacherId!,
        roomId: roomIdNum,
        startAt: payload.startIso,
        endAt: payload.endIso,
      });
    },
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['teacher-classes'] });
    },
  });

  const updateClassMutation = useMutation({
    mutationFn: async (params: { classId: number; patch: ClassSessionUpdate }) => {
      return Trials.updateClassSession(params.classId, params.patch);
    },
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['teacher-classes'] });
    },
  });

  const attendClassMutation = useMutation({
    mutationFn: (classId: number) => Trials.attendClassSession(classId, { attended: true }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['teacher-classes'] });
      showToast('Clase marcada como realizada.', 'success');
    },
    onError: (error) => {
      showToast(getErrorMessage(error, 'No pudimos marcar la clase.'), 'error');
    },
  });

  const loading =
    subjectsQuery.isLoading ||
    roomsQuery.isLoading ||
    teachersQuery.isLoading ||
    studentsQuery.isLoading ||
    availabilityQuery.isLoading ||
    classesQuery.isLoading;

  const myStudents = studentsQuery.data ?? [];
  const myAvailability = (availabilityQuery.data ?? []).slice().sort((a, b) => new Date(a.startAt).getTime() - new Date(b.startAt).getTime());
  const myClasses = (classesQuery.data ?? []).slice().sort((a, b) => new Date(a.startAt).getTime() - new Date(b.startAt).getTime());
  const agendaError = classesQuery.error ?? createClassMutation.error ?? updateClassMutation.error ?? attendClassMutation.error;
  const studentsError = studentsQuery.error ?? createStudentMutation.error ?? updateStudentMutation.error;
  const subjectsError = subjectsQuery.error ?? teachersQuery.error ?? updateSubjectsMutation.error;
  const availabilityError = availabilityQuery.error ?? upsertAvailabilityMutation.error ?? deleteAvailabilityMutation.error;
  const missingStudents = myStudents.length === 0;
  const missingSubjects = selectedSubjectIds.length === 0;
  const canScheduleClass = !missingStudents && !missingSubjects;

  const agendaClasses = useMemo(() => {
    const nowMs = Date.now();
    const startOfToday = new Date(nowMs);
    startOfToday.setHours(0, 0, 0, 0);
    const startOfTodayMs = startOfToday.getTime();
    const endOfTodayMs = startOfTodayMs + 24 * 60 * 60 * 1000;
    const next24hEndMs = nowMs + 24 * 60 * 60 * 1000;
    const weekEnd = new Date(nowMs);
    weekEnd.setDate(weekEnd.getDate() + 7);
    weekEnd.setHours(23, 59, 59, 999);
    const weekEndMs = weekEnd.getTime();

    return myClasses.filter((cls) => {
      const startMs = new Date(cls.startAt).getTime();
      if (Number.isNaN(startMs)) return false;
      if (agendaView === 'all') return true;
      if (agendaView === 'today') return startMs >= startOfTodayMs && startMs < endOfTodayMs;
      if (agendaView === 'next24h') return startMs >= nowMs && startMs < next24hEndMs;
      return startMs >= nowMs && startMs <= weekEndMs;
    });
  }, [agendaView, myClasses]);

  const availabilitySlots = useMemo(() => {
    const nowMs = Date.now();
    const startOfToday = new Date(nowMs);
    startOfToday.setHours(0, 0, 0, 0);
    const startOfTodayMs = startOfToday.getTime();
    const endOfTodayMs = startOfTodayMs + 24 * 60 * 60 * 1000;
    const next24hEndMs = nowMs + 24 * 60 * 60 * 1000;
    const weekEnd = new Date(nowMs);
    weekEnd.setDate(weekEnd.getDate() + 7);
    weekEnd.setHours(23, 59, 59, 999);
    const weekEndMs = weekEnd.getTime();

    return myAvailability.filter((slot) => {
      const startMs = new Date(slot.startAt).getTime();
      if (Number.isNaN(startMs)) return false;
      if (availabilityView === 'all') return true;
      if (availabilityView === 'today') return startMs >= startOfTodayMs && startMs < endOfTodayMs;
      if (availabilityView === 'next24h') return startMs >= nowMs && startMs < next24hEndMs;
      return startMs >= nowMs && startMs <= weekEndMs;
    });
  }, [availabilityView, myAvailability]);

  const agendaStats = useMemo(() => {
    const nowMs = Date.now();
    const startOfToday = new Date(nowMs);
    startOfToday.setHours(0, 0, 0, 0);
    const startOfTodayMs = startOfToday.getTime();
    const endOfTodayMs = startOfTodayMs + 24 * 60 * 60 * 1000;
    const weekEnd = new Date(nowMs);
    weekEnd.setDate(weekEnd.getDate() + 7);
    weekEnd.setHours(23, 59, 59, 999);
    const weekEndMs = weekEnd.getTime();

    let todayCount = 0;
    let weekCount = 0;
    let weekMinutes = 0;
    let overdueCount = 0;
    let nextClass: ClassSessionDTO | null = null;

    myClasses.forEach((cls) => {
      const startMs = new Date(cls.startAt).getTime();
      const endMs = new Date(cls.endAt).getTime();
      if (Number.isNaN(startMs) || Number.isNaN(endMs)) return;
      if (startMs >= startOfTodayMs && startMs < endOfTodayMs) todayCount += 1;
      if (startMs >= nowMs && startMs <= weekEndMs) {
        weekCount += 1;
        weekMinutes += Math.max(0, Math.round((endMs - startMs) / 60000));
      }
      if (endMs < nowMs && cls.status !== 'realizada') overdueCount += 1;
      if (!nextClass && startMs >= nowMs && cls.status !== 'realizada') nextClass = cls;
    });

    let nextMinutesAway: number | null = null;
    if (nextClass) {
      const startMs = new Date(String(nextClass.startAt)).getTime();
      if (!Number.isNaN(startMs)) {
        nextMinutesAway = Math.round((startMs - nowMs) / 60000);
      }
    }

    return {
      todayCount,
      weekCount,
      weekHours: weekMinutes / 60,
      overdueCount,
      nextClass,
      nextMinutesAway,
    };
  }, [myClasses]);

  if (!teacherId) {
    return (
      <Alert severity="warning" sx={{ borderRadius: 2 }}>
        No pudimos identificar tu sesión para el portal del profesor. Cierra sesión e ingresa nuevamente. Si el problema persiste, contacta soporte.
      </Alert>
    );
  }

  const openCreateStudent = () => {
    setStudentDialogError(null);
    setStudentForm({ fullName: '', email: '', phone: '' });
    setStudentDialogOpen(true);
  };

  const openEditStudent = (student: StudentDTO) => {
    setStudentEditDialogError(null);
    setEditingStudent(student);
    setStudentEditForm({
      displayName: student.displayName,
      email: student.email ?? '',
      phone: student.phone ?? '',
    });
    setStudentEditDialogOpen(true);
  };

  const openCreateAvailability = () => {
    setAvailabilityDialogError(null);
    const now = new Date();
    const startIso = now.toISOString();
    const endIso = new Date(now.getTime() + 60 * 60000).toISOString();
    setEditingAvailability(null);
    setAvailabilityForm({
      subjectId: '',
      roomId: '',
      startAt: toLocalInput(startIso),
      endAt: toLocalInput(endIso),
      notes: '',
    });
    setAvailabilityDialogOpen(true);
  };

  const openEditAvailability = (slot: TrialAvailabilitySlotDTO) => {
    setAvailabilityDialogError(null);
    setEditingAvailability(slot);
    setAvailabilityForm({
      subjectId: slot.subjectId,
      roomId: slot.roomId,
      startAt: toLocalInput(slot.startAt),
      endAt: toLocalInput(slot.endAt),
      notes: slot.notes ?? '',
    });
    setAvailabilityDialogOpen(true);
  };

  const openCreateClass = () => {
    setClassDialogError(null);
    const now = new Date();
    const startIso = now.toISOString();
    const endIso = new Date(now.getTime() + 60 * 60000).toISOString();
    setEditingClass(null);
    setClassForm({
      subjectId: '',
      studentId: '',
      roomId: '',
      startAt: toLocalInput(startIso),
      endAt: toLocalInput(endIso),
      notes: '',
    });
    setClassDialogOpen(true);
  };

  const openEditClass = (cls: ClassSessionDTO) => {
    setClassDialogError(null);
    setEditingClass(cls);
    setClassForm({
      subjectId: cls.subjectId,
      studentId: cls.studentId,
      roomId: cls.roomId ?? '',
      startAt: toLocalInput(cls.startAt),
      endAt: toLocalInput(cls.endAt),
      notes: cls.notes ?? '',
    });
    setClassDialogOpen(true);
  };

  const submitStudent = async () => {
    setStudentDialogError(null);
    const fullName = studentForm.fullName.trim();
    const email = studentForm.email.trim();
    const phone = studentForm.phone.trim();
    if (!fullName || !email) {
      setStudentDialogError('Completa nombre y correo.');
      return;
    }
    try {
      await createStudentMutation.mutateAsync({
        fullName,
        email,
        phone: phone ? phone : undefined,
      });
      setStudentDialogOpen(false);
      showToast('Alumno creado.', 'success');
    } catch (error) {
      const msg = getErrorMessage(error, 'No pudimos crear el alumno.');
      setStudentDialogError(msg);
      showToast(msg, 'error');
    }
  };

  const submitStudentEdit = async () => {
    setStudentEditDialogError(null);
    if (!editingStudent) return;
    const displayName = studentEditForm.displayName.trim();
    if (!displayName) {
      setStudentEditDialogError('El nombre es obligatorio.');
      return;
    }
    const patch: StudentUpdate = {
      displayName,
      ...(studentEditForm.email.trim() ? { email: studentEditForm.email.trim() } : {}),
      ...(studentEditForm.phone.trim() ? { phone: studentEditForm.phone.trim() } : {}),
    };
    try {
      await updateStudentMutation.mutateAsync({ studentId: editingStudent.studentId, patch });
      setStudentEditDialogOpen(false);
      setEditingStudent(null);
      showToast('Alumno actualizado.', 'success');
    } catch (error) {
      const msg = getErrorMessage(error, 'No pudimos actualizar el alumno.');
      setStudentEditDialogError(msg);
      showToast(msg, 'error');
    }
  };

  const submitAvailability = async () => {
    setAvailabilityDialogError(null);
    if (!availabilityForm.subjectId || !availabilityForm.roomId) {
      setAvailabilityDialogError('Completa materia y sala.');
      return;
    }
    if (availabilityTimeError) {
      setAvailabilityDialogError(availabilityTimeError);
      return;
    }
    const startIso = toIsoOrNull(availabilityForm.startAt);
    const endIso = toIsoOrNull(availabilityForm.endAt);
    if (!startIso || !endIso) {
      setAvailabilityDialogError('Completa el horario.');
      return;
    }
    const payload: TrialAvailabilityUpsert = {
      availabilityId: editingAvailability?.availabilityId ?? undefined,
      subjectId: Number(availabilityForm.subjectId),
      roomId: availabilityForm.roomId,
      startAt: startIso,
      endAt: endIso,
      notes: availabilityForm.notes.trim() ? availabilityForm.notes.trim() : undefined,
    };
    try {
      await upsertAvailabilityMutation.mutateAsync(payload);
      setAvailabilityDialogOpen(false);
      setEditingAvailability(null);
      showToast(editingAvailability ? 'Disponibilidad actualizada.' : 'Disponibilidad publicada.', 'success');
    } catch (error) {
      const msg = getErrorMessage(error, 'No pudimos guardar la disponibilidad.');
      setAvailabilityDialogError(msg);
      showToast(msg, 'error');
    }
  };

  const submitClass = async () => {
    setClassDialogError(null);
    if (!classForm.subjectId || !classForm.studentId || !classForm.roomId) {
      setClassDialogError('Completa materia, alumno, sala y horario.');
      return;
    }
    if (classTimeError) {
      setClassDialogError(classTimeError);
      return;
    }
    const startIso = toIsoOrNull(classForm.startAt);
    const endIso = toIsoOrNull(classForm.endAt);
    if (!startIso || !endIso) {
      setClassDialogError('Completa el horario.');
      return;
    }

    if (!editingClass) {
      try {
        await createClassMutation.mutateAsync({
          subjectId: Number(classForm.subjectId),
          studentId: Number(classForm.studentId),
          roomId: classForm.roomId,
          startIso,
          endIso,
        });
        setClassDialogOpen(false);
        showToast('Clase programada.', 'success');
      } catch (error) {
        const msg = getErrorMessage(error, 'No pudimos programar la clase.');
        setClassDialogError(msg);
        showToast(msg, 'error');
      }
      return;
    }

    const roomIdNum = Number.parseInt(classForm.roomId, 10);
    if (!Number.isFinite(roomIdNum)) {
      setClassDialogError('Sala inválida.');
      return;
    }
    const patch: ClassSessionUpdate = {
      subjectId: Number(classForm.subjectId),
      studentId: Number(classForm.studentId),
      roomId: roomIdNum,
      startAt: startIso,
      endAt: endIso,
      notes: classForm.notes.trim() ? classForm.notes.trim() : undefined,
    };
    try {
      await updateClassMutation.mutateAsync({ classId: editingClass.classSessionId, patch });
      setClassDialogOpen(false);
      setEditingClass(null);
      showToast('Clase actualizada.', 'success');
    } catch (error) {
      const msg = getErrorMessage(error, 'No pudimos actualizar la clase.');
      setClassDialogError(msg);
      showToast(msg, 'error');
    }
  };

  const availableRoomsForSubjectId = (subjectId: number | ''): RoomDTO[] => {
    if (!subjectId) return rooms;
    const subject = subjectById.get(Number(subjectId));
    const allowed = subject?.roomIds ?? [];
    if (allowed.length === 0) return rooms;
    const allowedSet = new Set(allowed);
    return rooms.filter((r) => allowedSet.has(r.roomId));
  };

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
              Portal del profesor
            </Typography>
            <Typography variant="body1" sx={{ color: 'rgba(226,232,240,0.82)' }}>
              Administra tus clases, alumnos, materias y disponibilidad.
            </Typography>
          </Box>
          <Stack direction="row" spacing={1} alignItems="center">
            <TabPill active={tab === 'agenda'} label={`${myClasses.length} clases`} />
            <TabPill active={tab === 'students'} label={`${myStudents.length} alumnos`} />
            <TabPill active={tab === 'subjects'} label={`${selectedSubjectIds.length} materias`} />
            <TabPill active={tab === 'availability'} label={`${myAvailability.length} slots`} />
          </Stack>
        </Stack>
      </Box>

      {loading && <LinearProgress />}

      <Paper sx={{ borderRadius: 2.5 }} variant="outlined">
        <Tabs
          value={tab}
          onChange={(_, value: PortalTab) => setTab(value)}
          variant="scrollable"
          scrollButtons="auto"
        >
          <Tab value="agenda" label="Agenda" icon={<EventAvailableIcon />} iconPosition="start" />
          <Tab value="students" label="Alumnos" icon={<PeopleIcon />} iconPosition="start" />
          <Tab value="subjects" label="Materias" icon={<SchoolIcon />} iconPosition="start" />
          <Tab value="availability" label="Disponibilidad" icon={<CalendarMonthIcon />} iconPosition="start" />
        </Tabs>
        <Divider />

        {tab === 'agenda' && (
          <Box sx={{ p: { xs: 2, md: 3 } }}>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ xs: 'stretch', md: 'center' }}>
              <Box>
                <Typography variant="h6" fontWeight={800}>Mis clases</Typography>
                <Typography variant="body2" color="text.secondary">
                  {formatDateTime(classRange.from)} → {formatDateTime(classRange.to)}
                </Typography>
              </Box>
              <Button variant="contained" onClick={openCreateClass} disabled={!canScheduleClass}>
                Programar clase
              </Button>
            </Stack>

            <Stack direction="row" spacing={1} sx={{ mt: 2 }} flexWrap="wrap" useFlexGap>
              {([
                { key: 'today', label: 'Hoy' },
                { key: 'next24h', label: 'Próximas 24h' },
                { key: 'week', label: 'Próx 7 días' },
                { key: 'all', label: 'Todo' },
              ] as const).map((opt) => (
                <Chip
                  key={opt.key}
                  label={opt.label}
                  clickable
                  onClick={() => setAgendaView(opt.key)}
                  color={agendaView === opt.key ? 'primary' : 'default'}
                  variant={agendaView === opt.key ? 'filled' : 'outlined'}
                  size="small"
                />
              ))}
            </Stack>

            <Stack direction="row" spacing={1} sx={{ mt: 1 }} flexWrap="wrap" useFlexGap>
              <Chip size="small" variant="outlined" label={`Hoy: ${agendaStats.todayCount}`} />
              <Chip size="small" variant="outlined" label={`Próx 7 días: ${agendaStats.weekCount}`} />
              <Chip size="small" variant="outlined" label={`Horas 7 días: ${agendaStats.weekHours.toFixed(1)}`} />
              {agendaStats.overdueCount > 0 && (
                <Chip size="small" color="warning" variant="outlined" label={`Pendientes: ${agendaStats.overdueCount}`} />
              )}
            </Stack>

            {agendaStats.nextClass && agendaStats.nextMinutesAway !== null && agendaStats.nextMinutesAway >= 0 && agendaStats.nextMinutesAway <= 120 && (
              <Alert
                severity={agendaStats.nextMinutesAway <= 30 ? 'warning' : 'info'}
                sx={{ mt: 2 }}
                action={(
                  <Stack direction="row" spacing={1}>
                    <Button color="inherit" size="small" onClick={() => openEditClass(agendaStats.nextClass!)}>
                      Editar
                    </Button>
                    <Button color="inherit" size="small" component={RouterLink} to="/estudio/calendario">
                      Calendario
                    </Button>
                  </Stack>
                )}
              >
                Próxima clase en {agendaStats.nextMinutesAway} min:{' '}
                {agendaStats.nextClass.studentName ?? `Alumno #${agendaStats.nextClass.studentId}`}{' '}
                · {formatDateTime(String(agendaStats.nextClass.startAt))}
              </Alert>
            )}

            {agendaError && (
              <Alert severity="warning" sx={{ mt: 2 }}>
                {getErrorMessage(agendaError, 'No pudimos cargar o guardar las clases.')}
              </Alert>
            )}

            {!canScheduleClass && (
              <Alert
                severity="info"
                sx={{ mt: 2 }}
                action={(
                  <Stack direction="row" spacing={1} flexWrap="wrap" justifyContent="flex-end">
                    {missingSubjects && (
                      <Button color="inherit" size="small" onClick={() => setTab('subjects')}>
                        Elegir materias
                      </Button>
                    )}
                    {missingStudents && (
                      <Button
                        color="inherit"
                        size="small"
                        onClick={() => {
                          setTab('students');
                          openCreateStudent();
                        }}
                      >
                        Agregar alumno
                      </Button>
                    )}
                  </Stack>
                )}
              >
                Para programar una clase necesitas {missingStudents ? 'alumnos' : null}
                {missingStudents && missingSubjects ? ' y ' : null}
                {missingSubjects ? 'materias' : null}.
              </Alert>
            )}

            <Stack spacing={1.5} sx={{ mt: 2 }}>
              {agendaClasses.length === 0 && (
                <Typography variant="body2" color="text.secondary">
                  No tienes clases en este filtro.
                </Typography>
              )}
              {agendaClasses.map((cls) => (
                <Paper key={cls.classSessionId} variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
                  <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ xs: 'flex-start', md: 'center' }}>
                    <Box>
                      <Typography fontWeight={800}>
                        {cls.subjectName ?? `Materia #${cls.subjectId}`} · {cls.studentName ?? `Alumno #${cls.studentId}`}
                      </Typography>
                      <Typography variant="body2" color="text.secondary">
                        {formatDateTime(cls.startAt)} → {formatDateTime(cls.endAt)} · {cls.roomName ?? cls.roomId ?? 'Sala'}
                      </Typography>
                      <Stack direction="row" spacing={1} sx={{ mt: 1 }} flexWrap="wrap">
                        <Chip size="small" label={cls.status} variant="outlined" />
                        {cls.notes && <Chip size="small" label="Notas" variant="outlined" />}
                      </Stack>
                    </Box>
                    <Stack direction="row" spacing={1}>
                      <Button size="small" variant="outlined" onClick={() => openEditClass(cls)}>
                        Editar
                      </Button>
                      <Button
                        size="small"
                        variant="contained"
                        onClick={() => attendClassMutation.mutate(cls.classSessionId)}
                        disabled={cls.status === 'realizada' || attendClassMutation.isPending}
                      >
                        Marcar realizada
                      </Button>
                    </Stack>
                  </Stack>
                </Paper>
              ))}
            </Stack>
          </Box>
        )}

        {tab === 'students' && (
          <Box sx={{ p: { xs: 2, md: 3 } }}>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ xs: 'stretch', md: 'center' }}>
              <Box>
                <Typography variant="h6" fontWeight={800}>Mis alumnos</Typography>
                <Typography variant="body2" color="text.secondary">
                  Crea y actualiza tus alumnos para poder programar clases.
                </Typography>
              </Box>
              <Button variant="contained" onClick={openCreateStudent}>
                Nuevo alumno
              </Button>
            </Stack>

            {studentsError && (
              <Alert severity="warning" sx={{ mt: 2 }}>
                {getErrorMessage(studentsError, 'No pudimos cargar o guardar alumnos.')}
              </Alert>
            )}

            <Stack spacing={1.5} sx={{ mt: 2 }}>
              {myStudents.length === 0 && (
                <Alert severity="info" action={<Button color="inherit" size="small" onClick={openCreateStudent}>Crear alumno</Button>}>
                  Todavía no tienes alumnos.
                </Alert>
              )}
              {myStudents.map((student) => (
                <Paper key={student.studentId} variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
                  <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ xs: 'flex-start', md: 'center' }}>
                    <Box>
                      <Typography fontWeight={800}>{student.displayName}</Typography>
                      <Typography variant="body2" color="text.secondary">
                        {[student.email, student.phone].filter(Boolean).join(' · ') || 'Sin datos de contacto'}
                      </Typography>
                    </Box>
                    <Button size="small" variant="outlined" onClick={() => openEditStudent(student)}>
                      Editar
                    </Button>
                  </Stack>
                </Paper>
              ))}
            </Stack>
          </Box>
        )}

        {tab === 'subjects' && (
          <Box sx={{ p: { xs: 2, md: 3 } }}>
            <Typography variant="h6" fontWeight={800}>Mis materias</Typography>
            <Typography variant="body2" color="text.secondary" sx={{ mb: 2 }}>
              Selecciona las materias que impartes. Esto controla qué disponibilidades y clases puedes crear.
            </Typography>
            {selectedSubjectIds.length === 0 && (
              <Alert severity="info" sx={{ mb: 2 }}>
                Selecciona al menos una materia para poder crear disponibilidad y programar clases.
              </Alert>
            )}

            {subjectsError && (
              <Alert severity="warning" sx={{ mb: 2 }}>
                {getErrorMessage(subjectsError, 'No pudimos cargar o guardar materias.')}
              </Alert>
            )}

            <Grid container spacing={2}>
              <Grid item xs={12} md={7}>
                <Autocomplete
                  multiple
                  options={activeSubjects}
                  getOptionLabel={(s) => s.name}
                  value={activeSubjects.filter((s) => selectedSubjectIds.includes(s.subjectId))}
                  onChange={(_, value) => setSelectedSubjectIds(value.map((s) => s.subjectId))}
                  renderInput={(params) => (
                    <TextField {...params} label="Materias" placeholder="Selecciona tus materias" />
                  )}
                />
                <Stack direction="row" spacing={1} sx={{ mt: 2 }}>
                  <Button
                    variant="contained"
                    onClick={() => updateSubjectsMutation.mutate(selectedSubjectIds)}
                    disabled={updateSubjectsMutation.isPending}
                  >
                    Guardar
                  </Button>
                  <Button
                    variant="outlined"
                    onClick={() => setSelectedSubjectIds(me?.subjects.map((s) => s.subjectId) ?? [])}
                    disabled={updateSubjectsMutation.isPending}
                  >
                    Descartar
                  </Button>
                </Stack>
              </Grid>

              <Grid item xs={12} md={5}>
                <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
                  <Typography fontWeight={800} sx={{ mb: 1 }}>
                    Salas configuradas
                  </Typography>
                  <Typography variant="body2" color="text.secondary" sx={{ mb: 1.5 }}>
                    Si una materia tiene salas configuradas, solo podrás elegir esas salas al crear disponibilidad o clases.
                  </Typography>
                    <Stack direction="row" spacing={1} flexWrap="wrap">
                      {(Array.from(allowedRoomsForSubject) || []).length === 0 && (
                        <Chip size="small" label="Sin restricciones" variant="outlined" />
                      )}
                      {Array.from(allowedRoomsForSubject).map((rid) => (
                        <Chip key={rid} size="small" label={roomNameById.get(rid) ?? rid} variant="outlined" />
                      ))}
                    </Stack>
                  </Paper>
                </Grid>
              </Grid>
          </Box>
        )}

        {tab === 'availability' && (
          <Box sx={{ p: { xs: 2, md: 3 } }}>
            <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ xs: 'stretch', md: 'center' }}>
              <Box>
                <Typography variant="h6" fontWeight={800}>Mi disponibilidad</Typography>
                <Typography variant="body2" color="text.secondary">
                  Publica tus horarios disponibles para clases o trials.
                </Typography>
              </Box>
              <Button variant="contained" onClick={openCreateAvailability} disabled={selectedSubjectIds.length === 0}>
                Agregar disponibilidad
              </Button>
            </Stack>

            <Stack direction="row" spacing={1} sx={{ mt: 2 }} flexWrap="wrap" useFlexGap>
              {([
                { key: 'today', label: 'Hoy' },
                { key: 'next24h', label: 'Próximas 24h' },
                { key: 'week', label: 'Próx 7 días' },
                { key: 'all', label: 'Todo' },
              ] as const).map((opt) => (
                <Chip
                  key={opt.key}
                  label={opt.label}
                  clickable
                  onClick={() => setAvailabilityView(opt.key)}
                  color={availabilityView === opt.key ? 'primary' : 'default'}
                  variant={availabilityView === opt.key ? 'filled' : 'outlined'}
                  size="small"
                />
              ))}
            </Stack>

            <Stack direction="row" spacing={1} sx={{ mt: 1 }} flexWrap="wrap" useFlexGap>
              <Chip size="small" variant="outlined" label={`Mostrando: ${availabilitySlots.length}`} />
              <Chip size="small" variant="outlined" label={`Total: ${myAvailability.length}`} />
            </Stack>

            {availabilityError && (
              <Alert severity="warning" sx={{ mt: 2 }}>
                {getErrorMessage(availabilityError, 'No pudimos cargar o guardar la disponibilidad.')}
              </Alert>
            )}

            {selectedSubjectIds.length === 0 && (
              <Alert
                severity="info"
                sx={{ mt: 2 }}
                action={<Button color="inherit" size="small" onClick={() => setTab('subjects')}>Ir a materias</Button>}
              >
                Primero selecciona tus materias en la pestaña <b>Materias</b>.
              </Alert>
            )}

            <Stack spacing={1.5} sx={{ mt: 2 }}>
              {myAvailability.length === 0 && (
                <Alert
                  severity="info"
                  action={<Button color="inherit" size="small" onClick={openCreateAvailability} disabled={selectedSubjectIds.length === 0}>Agregar disponibilidad</Button>}
                >
                  No has publicado disponibilidad todavía.
                </Alert>
              )}
              {myAvailability.length > 0 && availabilitySlots.length === 0 && (
                <Alert severity="info">
                  No hay disponibilidad en este filtro.
                </Alert>
              )}
              {availabilitySlots.map((slot) => (
                <Paper key={slot.availabilityId} variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
                  <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ xs: 'flex-start', md: 'center' }}>
                    <Box>
                      <Typography fontWeight={800}>
                        {slot.subjectName ?? `Materia #${slot.subjectId}`} · {slot.roomName ?? slot.roomId}
                      </Typography>
                      <Typography variant="body2" color="text.secondary">
                        {formatDateTime(slot.startAt)} → {formatDateTime(slot.endAt)}
                      </Typography>
                      {slot.notes && (
                        <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                          {slot.notes}
                        </Typography>
                      )}
                    </Box>
                    <Stack direction="row" spacing={1}>
                      <Button size="small" variant="outlined" onClick={() => openEditAvailability(slot)}>
                        Editar
                      </Button>
                      <Button
                        size="small"
                        variant="outlined"
                        color="error"
                        onClick={() => deleteAvailabilityMutation.mutate(slot.availabilityId)}
                        disabled={deleteAvailabilityMutation.isPending}
                      >
                        Eliminar
                      </Button>
                    </Stack>
                  </Stack>
                </Paper>
              ))}
            </Stack>
          </Box>
        )}
      </Paper>

      <Dialog
        open={studentDialogOpen}
        onClose={() => {
          setStudentDialogOpen(false);
          setStudentDialogError(null);
        }}
        fullWidth
        maxWidth="sm"
      >
        <DialogTitle>Nuevo alumno</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            {studentDialogError && <Alert severity="warning">{studentDialogError}</Alert>}
            <TextField
              label="Nombre completo"
              value={studentForm.fullName}
              onChange={(e) => setStudentForm((prev) => ({ ...prev, fullName: e.target.value }))}
              required
            />
            <TextField
              label="Email"
              value={studentForm.email}
              onChange={(e) => setStudentForm((prev) => ({ ...prev, email: e.target.value }))}
              required
            />
            <TextField
              label="Teléfono"
              value={studentForm.phone}
              onChange={(e) => setStudentForm((prev) => ({ ...prev, phone: e.target.value }))}
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setStudentDialogOpen(false)}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={() => void submitStudent()}
            disabled={createStudentMutation.isPending || studentForm.fullName.trim() === '' || studentForm.email.trim() === ''}
          >
            Crear
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={studentEditDialogOpen}
        onClose={() => {
          setStudentEditDialogOpen(false);
          setEditingStudent(null);
          setStudentEditDialogError(null);
        }}
        fullWidth
        maxWidth="sm"
      >
        <DialogTitle>Editar alumno</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            {studentEditDialogError && <Alert severity="warning">{studentEditDialogError}</Alert>}
            <TextField
              label="Nombre"
              value={studentEditForm.displayName}
              onChange={(e) => setStudentEditForm((prev) => ({ ...prev, displayName: e.target.value }))}
              required
            />
            <TextField
              label="Email"
              value={studentEditForm.email}
              onChange={(e) => setStudentEditForm((prev) => ({ ...prev, email: e.target.value }))}
            />
            <TextField
              label="Teléfono"
              value={studentEditForm.phone}
              onChange={(e) => setStudentEditForm((prev) => ({ ...prev, phone: e.target.value }))}
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setStudentEditDialogOpen(false)}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={() => void submitStudentEdit()}
            disabled={updateStudentMutation.isPending || studentEditForm.displayName.trim() === ''}
          >
            Guardar
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={availabilityDialogOpen}
        onClose={() => {
          setAvailabilityDialogOpen(false);
          setEditingAvailability(null);
          setAvailabilityDialogError(null);
        }}
        fullWidth
        maxWidth="sm"
      >
        <DialogTitle>{editingAvailability ? 'Editar disponibilidad' : 'Agregar disponibilidad'}</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            {availabilityDialogError && <Alert severity="warning">{availabilityDialogError}</Alert>}
            <TextField
              select
              label="Materia"
              value={availabilityForm.subjectId}
              onChange={(e) => setAvailabilityForm((prev) => ({ ...prev, subjectId: e.target.value === '' ? '' : Number(e.target.value), roomId: '' }))}
              required
            >
              <MenuItem value="">Selecciona</MenuItem>
              {activeSubjects
                .filter((s) => selectedSubjectIds.includes(s.subjectId))
                .map((s) => (
                  <MenuItem key={s.subjectId} value={s.subjectId}>
                    {s.name}
                  </MenuItem>
                ))}
            </TextField>

            <TextField
              select
              label="Sala"
              value={availabilityForm.roomId}
              onChange={(e) => setAvailabilityForm((prev) => ({ ...prev, roomId: e.target.value }))}
              required
              disabled={!availabilityForm.subjectId}
            >
              <MenuItem value="">Selecciona</MenuItem>
              {availableRoomsForSubjectId(availabilityForm.subjectId).map((room) => (
                <MenuItem key={room.roomId} value={room.roomId}>
                  {room.rName}
                </MenuItem>
              ))}
            </TextField>

            <TextField
              type="datetime-local"
              label="Inicio"
              value={availabilityForm.startAt}
              onChange={(e) => {
                const nextStart = e.target.value;
                setAvailabilityForm((prev) => {
                  const durationMs = computeDurationMs(prev.startAt, prev.endAt, 60);
                  const nextEnd = addDurationToLocalStart(nextStart, durationMs);
                  return {
                    ...prev,
                    startAt: nextStart,
                    endAt: nextEnd ?? prev.endAt,
                  };
                });
              }}
              InputLabelProps={{ shrink: true }}
              required
            />
            <TextField
              type="datetime-local"
              label="Fin"
              value={availabilityForm.endAt}
              onChange={(e) => setAvailabilityForm((prev) => ({ ...prev, endAt: e.target.value }))}
              InputLabelProps={{ shrink: true }}
              error={Boolean(availabilityTimeError)}
              helperText={availabilityTimeError ?? ' '}
              required
            />
            <TextField
              label="Notas (opcional)"
              value={availabilityForm.notes}
              onChange={(e) => setAvailabilityForm((prev) => ({ ...prev, notes: e.target.value }))}
              multiline
              minRows={2}
            />
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setAvailabilityDialogOpen(false)}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={() => void submitAvailability()}
            disabled={
              upsertAvailabilityMutation.isPending ||
              availabilityForm.subjectId === '' ||
              availabilityForm.roomId === '' ||
              Boolean(availabilityTimeError)
            }
          >
            Guardar
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={classDialogOpen}
        onClose={() => {
          setClassDialogOpen(false);
          setEditingClass(null);
          setClassDialogError(null);
        }}
        fullWidth
        maxWidth="sm"
      >
        <DialogTitle>{editingClass ? 'Editar clase' : 'Programar clase'}</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ mt: 1 }}>
            {classDialogError && <Alert severity="warning">{classDialogError}</Alert>}
            <TextField
              select
              label="Materia"
              value={classForm.subjectId}
              onChange={(e) => setClassForm((prev) => ({ ...prev, subjectId: e.target.value === '' ? '' : Number(e.target.value), roomId: '' }))}
              required
            >
              <MenuItem value="">Selecciona</MenuItem>
              {activeSubjects
                .filter((s) => selectedSubjectIds.includes(s.subjectId))
                .map((s) => (
                  <MenuItem key={s.subjectId} value={s.subjectId}>
                    {s.name}
                  </MenuItem>
                ))}
            </TextField>

            <TextField
              select
              label="Alumno"
              value={classForm.studentId}
              onChange={(e) => setClassForm((prev) => ({ ...prev, studentId: e.target.value === '' ? '' : Number(e.target.value) }))}
              required
            >
              <MenuItem value="">Selecciona</MenuItem>
              {myStudents.map((s) => (
                <MenuItem key={s.studentId} value={s.studentId}>
                  {s.displayName}
                </MenuItem>
              ))}
            </TextField>

            <TextField
              select
              label="Sala"
              value={classForm.roomId}
              onChange={(e) => setClassForm((prev) => ({ ...prev, roomId: e.target.value }))}
              required
              disabled={!classForm.subjectId}
            >
              <MenuItem value="">Selecciona</MenuItem>
              {availableRoomsForSubjectId(classForm.subjectId).map((room) => (
                <MenuItem key={room.roomId} value={room.roomId}>
                  {room.rName}
                </MenuItem>
              ))}
            </TextField>

            <TextField
              type="datetime-local"
              label="Inicio"
              value={classForm.startAt}
              onChange={(e) => {
                const nextStart = e.target.value;
                setClassForm((prev) => {
                  const durationMs = computeDurationMs(prev.startAt, prev.endAt, 60);
                  const nextEnd = addDurationToLocalStart(nextStart, durationMs);
                  return {
                    ...prev,
                    startAt: nextStart,
                    endAt: nextEnd ?? prev.endAt,
                  };
                });
              }}
              InputLabelProps={{ shrink: true }}
              required
            />
            <TextField
              type="datetime-local"
              label="Fin"
              value={classForm.endAt}
              onChange={(e) => setClassForm((prev) => ({ ...prev, endAt: e.target.value }))}
              InputLabelProps={{ shrink: true }}
              error={Boolean(classTimeError)}
              helperText={classTimeError ?? ' '}
              required
            />

            {editingClass && (
              <TextField
                label="Notas (opcional)"
                value={classForm.notes}
                onChange={(e) => setClassForm((prev) => ({ ...prev, notes: e.target.value }))}
                multiline
                minRows={2}
              />
            )}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setClassDialogOpen(false)}>Cancelar</Button>
          <Button
            variant="contained"
            onClick={() => void submitClass()}
            disabled={
              createClassMutation.isPending ||
              updateClassMutation.isPending ||
              classForm.subjectId === '' ||
              classForm.studentId === '' ||
              classForm.roomId === '' ||
              Boolean(classTimeError)
            }
          >
            Guardar
          </Button>
        </DialogActions>
      </Dialog>

      <Snackbar
        open={Boolean(toast)}
        autoHideDuration={4500}
        onClose={closeToast}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
      >
        {toast ? (
          <Alert onClose={() => setToast(null)} severity={toast.severity} sx={{ width: '100%' }}>
            {toast.message}
          </Alert>
        ) : (
          <span />
        )}
      </Snackbar>
    </Stack>
  );
}
