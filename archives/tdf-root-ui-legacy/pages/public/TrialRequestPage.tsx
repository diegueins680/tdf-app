import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  CardHeader,
  CircularProgress,
  Container,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import dayjs from 'dayjs';
import 'dayjs/locale/es';
import { Trials } from '../../api/trials';
import type { PreferredSlotDTO, SubjectDTO, TrialSlotDTO } from '../../api/types';

dayjs.locale('es');

type SelectedSlot = PreferredSlotDTO & { teacherId: number; teacherName: string };

function formatSlotLabel(slot: PreferredSlotDTO) {
  const start = dayjs(slot.startAt);
  const end = dayjs(slot.endAt);
  if (!start.isValid() || !end.isValid()) {
    return `${slot.startAt} → ${slot.endAt}`;
  }

  const durationMinutes = Math.max(end.diff(start, 'minute'), 0);
  const dateLabel = start.format('ddd D [de] MMMM');
  const timeLabel = `${start.format('HH:mm')} – ${end.format('HH:mm')}`;
  const durationLabel = durationMinutes ? ` · ${durationMinutes} min` : '';
  return `${dateLabel}\n${timeLabel}${durationLabel}`;
}

export default function TrialRequestPage() {
  const [notes, setNotes] = useState('');
  const [selectedSubject, setSelectedSubject] = useState<number | ''>('');
  const [selectedSlot, setSelectedSlot] = useState<SelectedSlot | null>(null);
  const [done, setDone] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [isSubmitting, setIsSubmitting] = useState(false);

  const subjectsQuery = useQuery({ queryKey: ['trials', 'subjects'], queryFn: Trials.listSubjects });

  const subjectOptions = useMemo(() => {
    const list = subjectsQuery.data ?? [];
    return list.slice().sort((a, b) => a.name.localeCompare(b.name, 'es'));
  }, [subjectsQuery.data]);

  useEffect(() => {
    if (!subjectOptions.length) return;
    setSelectedSubject((prev) => (prev ? prev : subjectOptions[0]?.subjectId ?? ''));
  }, [subjectOptions]);

  const slotsQuery = useQuery({
    queryKey: ['trials', 'slots', selectedSubject],
    queryFn: () => Trials.listTrialSlots(Number(selectedSubject)),
    enabled: typeof selectedSubject === 'number',
  });

  useEffect(() => {
    setSelectedSlot(null);
    setDone(false);
  }, [selectedSubject]);

  const teachersWithSlots: TrialSlotDTO[] = useMemo(() => slotsQuery.data ?? [], [slotsQuery.data]);

  const handleSelectSlot = (teacher: TrialSlotDTO, slot: PreferredSlotDTO) => {
    setSelectedSlot({ ...slot, teacherId: teacher.teacherId, teacherName: teacher.teacherName });
    setDone(false);
    setError(null);
  };

  const submit = async () => {
    if (!selectedSubject) {
      setError('Selecciona una materia para continuar.');
      return;
    }
    if (!selectedSlot) {
      setError('Selecciona un horario disponible.');
      return;
    }

    try {
      setIsSubmitting(true);
      setError(null);
      await Trials.createTrialRequest({
        subjectId: Number(selectedSubject),
        preferred: [
          {
            startAt: dayjs(selectedSlot.startAt).toISOString(),
            endAt: dayjs(selectedSlot.endAt).toISOString(),
            teacherId: selectedSlot.teacherId,
            teacherName: selectedSlot.teacherName,
          },
        ],
        notes: notes.trim() || undefined,
      });
      setDone(true);
      setSelectedSlot(null);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'No pudimos registrar tu solicitud.';
      setError(message);
    } finally {
      setIsSubmitting(false);
    }
  };

  const selectedSubjectData: SubjectDTO | undefined =
    typeof selectedSubject === 'number'
      ? subjectOptions.find((item) => item.subjectId === selectedSubject)
      : undefined;

  return (
    <Container maxWidth="md" sx={{ py: 6 }}>
      <Typography variant="h4" gutterBottom>
        Solicitar clase de prueba
      </Typography>
      {done ? (
        <Alert severity="success">¡Solicitud enviada! Te contactaremos pronto.</Alert>
      ) : (
        <Stack gap={3}>
          {error && <Alert severity="error">{error}</Alert>}
          {subjectsQuery.isError && (
            <Alert severity="error">
              No pudimos cargar las materias disponibles. Intenta de nuevo en unos minutos.
            </Alert>
          )}
          <TextField
            select
            label="Materia"
            value={selectedSubject}
            onChange={(event) => setSelectedSubject(event.target.value ? Number(event.target.value) : '')}
            sx={{ maxWidth: 360 }}
            disabled={subjectsQuery.isLoading || subjectOptions.length === 0}
            helperText={subjectsQuery.isLoading ? 'Cargando materias…' : undefined}
          >
            {subjectOptions.map((subject) => (
              <MenuItem key={subject.subjectId} value={subject.subjectId}>
                {subject.name}
              </MenuItem>
            ))}
          </TextField>

          <Stack gap={2}>
            <Typography variant="h6">Selecciona un horario de 45 minutos</Typography>
            {typeof selectedSubject !== 'number' ? (
              <Alert severity="info">Elige una materia para ver los horarios disponibles.</Alert>
            ) : slotsQuery.isLoading ? (
              <Box display="flex" justifyContent="center" py={4}>
                <CircularProgress />
              </Box>
            ) : slotsQuery.isError ? (
              <Alert severity="error">No pudimos cargar los horarios disponibles.</Alert>
            ) : teachersWithSlots.length === 0 ? (
              <Alert severity="warning">
                Por ahora no hay horarios disponibles para esta materia. Vuelve a intentarlo más tarde.
              </Alert>
            ) : (
              <Stack gap={2}>
                {teachersWithSlots.map((teacher) => (
                  <Card key={teacher.teacherId} variant="outlined">
                    <CardHeader
                      title={teacher.teacherName}
                      subheader={
                        selectedSubjectData?.name
                          ? `Profesor de ${selectedSubjectData.name}`
                          : 'Profesor disponible'
                      }
                    />
                    <CardContent>
                      {teacher.slots.length === 0 ? (
                        <Typography variant="body2" color="text.secondary">
                          No hay horarios disponibles con este profesor por el momento.
                        </Typography>
                      ) : (
                        <Stack direction="row" flexWrap="wrap" gap={1.5}>
                          {teacher.slots.map((slot) => {
                            const key = `${teacher.teacherId}-${slot.startAt}-${slot.endAt}`;
                            const isSelected =
                              !!selectedSlot &&
                              selectedSlot.teacherId === teacher.teacherId &&
                              selectedSlot.startAt === slot.startAt &&
                              selectedSlot.endAt === slot.endAt;

                            return (
                              <Button
                                key={key}
                                variant={isSelected ? 'contained' : 'outlined'}
                                onClick={() => handleSelectSlot(teacher, slot)}
                                sx={{
                                  textTransform: 'none',
                                  justifyContent: 'flex-start',
                                  minWidth: 220,
                                  whiteSpace: 'pre-line',
                                }}
                              >
                                {formatSlotLabel(slot)}
                              </Button>
                            );
                          })}
                        </Stack>
                      )}
                    </CardContent>
                  </Card>
                ))}
              </Stack>
            )}
          </Stack>

          <TextField
            label="Notas"
            multiline
            minRows={3}
            value={notes}
            onChange={(event) => setNotes(event.target.value)}
            placeholder="Comparte contexto adicional (experiencia, objetivo, etc.)"
          />
          <Button variant="contained" onClick={submit} size="large" disabled={isSubmitting || !selectedSlot}>
            {isSubmitting ? 'Enviando…' : 'Enviar solicitud'}
          </Button>
        </Stack>
      )}
    </Container>
  );
}
