import { useMemo, useState } from 'react';
import { useMutation, useQuery } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Container,
  Divider,
  Grid,
  Link,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import AccessTimeIcon from '@mui/icons-material/AccessTime';
import WhatsAppIcon from '@mui/icons-material/WhatsApp';
import type { TrialSubject } from '../api/trials';
import { Trials } from '../api/trials';
import PublicBrandBar from '../components/PublicBrandBar';
import { TRIALS_WHATSAPP_URL } from '../config/appConfig';

interface SlotInput {
  start: string;
}

const MAX_SLOT_COUNT = 3;

export const createEmptySlotInputs = (): SlotInput[] =>
  Array.from({ length: MAX_SLOT_COUNT }, () => ({ start: '' }));

export const parsePositiveSubjectId = (raw: unknown): number | '' => {
  if (typeof raw === 'number') {
    return Number.isSafeInteger(raw) && raw > 0 ? raw : '';
  }
  if (typeof raw !== 'string') return '';
  const trimmed = raw.trim();
  if (!/^\d+$/.test(trimmed)) return '';
  const parsed = Number(trimmed);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : '';
};

export const applySuggestedSlot = (slots: readonly SlotInput[], start: string): SlotInput[] => {
  const trimmed = start.trim();
  if (!trimmed) return [...slots];
  if (slots.some((slot) => slot.start === trimmed)) return [...slots];

  const nextSlots = slots.map((slot) => ({ ...slot }));
  const emptyIndex = nextSlots.findIndex((slot) => slot.start === '');
  const targetIndex = emptyIndex >= 0 ? emptyIndex : nextSlots.length - 1;
  nextSlots[targetIndex] = { start: trimmed };
  return nextSlots;
};

export const toFriendlyTrialError = (error: unknown): string | null => {
  if (!(error instanceof Error)) return null;
  const message = error.message.trim();
  if (!message) return 'No pudimos enviar la solicitud. Inténtalo de nuevo en un momento.';

  const normalized = message.toLowerCase();
  if (normalized.includes('correo requerido')) {
    return 'Déjanos un correo válido para poder confirmar tu clase de prueba.';
  }
  if (normalized.includes('need at least one preferred slot')) {
    return 'Elige al menos un horario preferido para continuar.';
  }
  if (normalized.includes('no hay profesores disponibles para esta materia')) {
    return 'Esta materia no tiene cupos publicados ahora mismo. Escríbenos por WhatsApp y te ayudamos a encontrar una opción.';
  }
  if (normalized.includes('no hay profesores disponibles en el horario solicitado')) {
    return 'Ese horario ya no está disponible. Prueba uno de los sugeridos o agrega otra hora.';
  }
  return message;
};

const getVisibleSlotCount = (slots: readonly SlotInput[]): number => {
  const lastFilledIndex = slots.reduce((latestIndex, slot, index) => (slot.start ? index : latestIndex), -1);
  return Math.min(MAX_SLOT_COUNT, Math.max(1, lastFilledIndex + 1));
};

const toDateTimeLocalValue = (iso: string): string => {
  const date = new Date(iso);
  if (Number.isNaN(date.getTime())) return '';
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, '0');
  const day = String(date.getDate()).padStart(2, '0');
  const hours = String(date.getHours()).padStart(2, '0');
  const minutes = String(date.getMinutes()).padStart(2, '0');
  return `${year}-${month}-${day}T${hours}:${minutes}`;
};

const fieldSx = {
  '& .MuiOutlinedInput-root': {
    color: '#f8fafc',
    bgcolor: 'rgba(255,255,255,0.03)',
    '& fieldset': { borderColor: 'rgba(255,255,255,0.12)' },
    '&:hover fieldset': { borderColor: 'rgba(255,255,255,0.25)' },
    '&.Mui-focused fieldset': {
      borderColor: '#7dd3fc',
      boxShadow: '0 0 0 1px rgba(125,211,252,0.35)',
    },
  },
  '& .MuiInputBase-input': {
    color: '#f8fafc',
    caretColor: '#cbd5f5',
    '&::placeholder': { color: 'rgba(226,232,240,0.7)' },
    '&::-webkit-calendar-picker-indicator': {
      filter: 'invert(0.8)',
    },
  },
  '& .MuiInputLabel-root': {
    color: 'rgba(226,232,240,0.8)',
  },
  '& .MuiInputLabel-root.Mui-focused': {
    color: '#cbd5f5',
  },
};

export default function TrialsPage() {
  const [subjectId, setSubjectId] = useState<number | ''>('');
  const [fullName, setFullName] = useState('');
  const [email, setEmail] = useState('');
  const [phone, setPhone] = useState('');
  const [notes, setNotes] = useState('');
  const [slotInputs, setSlotInputs] = useState<SlotInput[]>(() => createEmptySlotInputs());
  const [visibleSlotCount, setVisibleSlotCount] = useState(1);
  const [formError, setFormError] = useState<string | null>(null);

  const subjectsQuery = useQuery({
    queryKey: ['trial-subjects'],
    queryFn: Trials.listSubjects,
  });

  const slotsQuery = useQuery({
    queryKey: ['trial-slots', subjectId],
    queryFn: () => Trials.listSlots(typeof subjectId === 'number' ? subjectId : undefined),
    enabled: typeof subjectId === 'number',
  });

  const requestMutation = useMutation({
    mutationFn: Trials.createRequest,
    onSuccess: () => {
      setSlotInputs(createEmptySlotInputs());
      setVisibleSlotCount(1);
    },
  });

  const minimumDateTimeValue = useMemo(() => toDateTimeLocalValue(new Date().toISOString()), []);

  const subjects: TrialSubject[] = useMemo(
    () => (subjectsQuery.data ?? []).filter((subject) => subject.active),
    [subjectsQuery.data],
  );

  const suggestedSlotOptions = useMemo(
    () =>
      (slotsQuery.data ?? []).flatMap((teacherAvailability) =>
        teacherAvailability.slots.map((slot, slotIndex) => {
          const inputValue = toDateTimeLocalValue(slot.startAt);
          return {
            key: `${teacherAvailability.teacherId}-${slot.startAt}-${slotIndex}`,
            teacherName: teacherAvailability.teacherName,
            label: formatSlotLabel(slot.startAt, slot.endAt),
            inputValue,
            selected: slotInputs.some((candidate) => candidate.start === inputValue),
          };
        }),
      ),
    [slotInputs, slotsQuery.data],
  );

  const filledSlotCount = useMemo(
    () => slotInputs.filter((slot) => slot.start.trim() !== '').length,
    [slotInputs],
  );

  const submitting = requestMutation.isPending;
  const submitted = requestMutation.isSuccess;
  const submitError = toFriendlyTrialError(requestMutation.error);

  const handleSlotChange = (index: number, value: string) => {
    setFormError(null);
    setSlotInputs((prev) => prev.map((slot, currentIndex) => (currentIndex === index ? { ...slot, start: value } : slot)));
  };

  const handleSubjectChange = (raw: unknown) => {
    const nextSubjectId = parsePositiveSubjectId(raw);
    setSubjectId(nextSubjectId);
    setFormError(null);
    requestMutation.reset();
    setSlotInputs(createEmptySlotInputs());
    setVisibleSlotCount(1);
  };

  const handleSuggestedSlotClick = (inputValue: string) => {
    setFormError(null);
    const nextSlots = applySuggestedSlot(slotInputs, inputValue);
    setSlotInputs(nextSlots);
    setVisibleSlotCount(getVisibleSlotCount(nextSlots));
  };

  const handleAddAlternativeSlot = () => {
    setVisibleSlotCount((prev) => Math.min(MAX_SLOT_COUNT, prev + 1));
  };

  const handleClearSlots = () => {
    setSlotInputs(createEmptySlotInputs());
    setVisibleSlotCount(1);
    setFormError(null);
  };

  const parseIsoOrNull = (value: string) => {
    if (!value) return null;
    const parsedDate = new Date(value);
    return Number.isNaN(parsedDate.getTime()) ? null : parsedDate.toISOString();
  };

  const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setFormError(null);

    if (!subjectId) {
      setFormError('Elige la materia para la clase de prueba.');
      return;
    }

    const filledSlots = slotInputs.filter((slot) => slot.start.trim() !== '');
    if (filledSlots.length === 0) {
      setFormError('Elige al menos un horario preferido.');
      return;
    }

    const preferred = filledSlots
      .map((slot) => {
        const startAt = parseIsoOrNull(slot.start);
        if (!startAt) return null;
        const startDate = new Date(startAt);
        const endAt = new Date(startDate.getTime() + 45 * 60 * 1000).toISOString();
        return { startAt, endAt };
      })
      .filter((slot): slot is { startAt: string; endAt: string } => Boolean(slot));

    if (!preferred.length) {
      setFormError('Los horarios ingresados no son válidos.');
      return;
    }

    requestMutation.mutate({
      subjectId: Number(subjectId),
      preferred,
      fullName: fullName.trim() || undefined,
      email: email.trim() || undefined,
      phone: phone.trim() || undefined,
      notes: notes.trim() || undefined,
    });
  };

  return (
    <Box
      sx={{
        minHeight: '100vh',
        background: 'linear-gradient(135deg, #0b1224, #0f172a)',
        color: '#e2e8f0',
        py: { xs: 4, md: 6 },
      }}
    >
      <Container maxWidth="md">
        <Stack spacing={3}>
          <Box sx={{ display: 'flex', justifyContent: 'center' }}>
            <PublicBrandBar tagline="Clases de prueba · Escuela TDF" compact />
          </Box>
          <Box textAlign="center">
            <Chip
              label="Trial lesson"
              sx={{
                bgcolor: 'rgba(255,255,255,0.08)',
                color: '#cbd5f5',
                border: '1px solid rgba(255,255,255,0.16)',
                mb: 1,
              }}
            />
            <Typography variant="h3" fontWeight={800}>
              Solicita tu clase de prueba
            </Typography>
            <Typography variant="body1" color="rgba(226,232,240,0.8)">
              Elige la materia, toca un horario sugerido o propone el tuyo y te contactamos para agendar.
            </Typography>
          </Box>

          <Grid container spacing={3}>
            <Grid item xs={12} md={7}>
              <Card
                sx={{
                  background: 'rgba(15,23,42,0.65)',
                  border: '1px solid rgba(255,255,255,0.08)',
                  backdropFilter: 'blur(12px)',
                  color: '#e2e8f0',
                }}
              >
                <CardContent>
                  <Stack spacing={2} component="form" onSubmit={handleSubmit}>
                    <Typography variant="h6" fontWeight={700}>
                      Datos de contacto
                    </Typography>
                    <Typography variant="body2" color="rgba(226,232,240,0.8)">
                      Te toma menos de un minuto. Con esto te podemos confirmar la clase sin ir y venir.
                    </Typography>
                    <TextField
                      label="Nombre completo"
                      value={fullName}
                      onChange={(e) => setFullName(e.target.value)}
                      required
                      fullWidth
                      sx={fieldSx}
                    />
                    <TextField
                      label="Correo"
                      type="email"
                      value={email}
                      onChange={(e) => setEmail(e.target.value)}
                      required
                      fullWidth
                      sx={fieldSx}
                    />
                    <TextField
                      label="WhatsApp o teléfono"
                      value={phone}
                      onChange={(e) => setPhone(e.target.value)}
                      placeholder="+593..."
                      fullWidth
                      sx={fieldSx}
                    />

                    <Divider sx={{ borderColor: 'rgba(255,255,255,0.12)' }} />

                    <Typography variant="h6" fontWeight={700}>
                      Materia y horarios
                    </Typography>
                    <Typography variant="body2" color="rgba(226,232,240,0.8)">
                      La clase dura 45 minutos. Te basta con un horario; si tienes flexibilidad, agrega hasta tres opciones.
                    </Typography>

                    {subjectsQuery.error && (
                      <Alert severity="warning">
                        No pudimos cargar las materias ahora mismo. Si quieres, avanza por WhatsApp mientras lo resolvemos.
                      </Alert>
                    )}

                    <TextField
                      label="Materia"
                      select
                      value={subjectId}
                      onChange={(e) => handleSubjectChange(e.target.value)}
                      disabled={subjectsQuery.isLoading}
                      required
                      fullWidth
                      sx={fieldSx}
                    >
                      {subjects.map((subject) => (
                        <MenuItem key={subject.subjectId} value={subject.subjectId}>
                          {subject.name}
                        </MenuItem>
                      ))}
                    </TextField>

                    {typeof subjectId === 'number' && (
                      <Box
                        sx={{
                          p: 1.5,
                          borderRadius: 2,
                          border: '1px solid rgba(255,255,255,0.08)',
                          bgcolor: 'rgba(255,255,255,0.03)',
                        }}
                      >
                        <Stack spacing={1.25}>
                          <Typography variant="subtitle2" fontWeight={700}>
                            Horarios sugeridos
                          </Typography>
                          <Typography variant="body2" color="rgba(226,232,240,0.8)">
                            Toca uno y lo autocompletamos abajo. Luego puedes sumar horarios alternativos si quieres.
                          </Typography>

                          {slotsQuery.isLoading && (
                            <Stack direction="row" spacing={1} alignItems="center">
                              <CircularProgress size={18} />
                              <Typography variant="body2">Buscando horarios sugeridos…</Typography>
                            </Stack>
                          )}

                          {!slotsQuery.isLoading && suggestedSlotOptions.length > 0 && (
                            <Stack direction="row" useFlexGap flexWrap="wrap" gap={1}>
                              {suggestedSlotOptions.map((slot) => (
                                <Chip
                                  key={slot.key}
                                  clickable
                                  color={slot.selected ? 'primary' : 'default'}
                                  onClick={() => handleSuggestedSlotClick(slot.inputValue)}
                                  label={`${slot.label} · ${slot.teacherName}`}
                                  sx={{
                                    bgcolor: slot.selected ? 'rgba(125,211,252,0.18)' : 'rgba(255,255,255,0.08)',
                                    color: '#e2e8f0',
                                    border: slot.selected
                                      ? '1px solid rgba(125,211,252,0.55)'
                                      : '1px solid rgba(255,255,255,0.08)',
                                  }}
                                />
                              ))}
                            </Stack>
                          )}

                          {!slotsQuery.isLoading && !slotsQuery.error && suggestedSlotOptions.length === 0 && (
                            <Alert severity="info">
                              Aún no hay horarios sugeridos para esta materia. Igual puedes proponer uno manualmente.
                            </Alert>
                          )}

                          {slotsQuery.error && (
                            <Alert severity="warning">
                              No pudimos cargar horarios sugeridos. Igual puedes proponer un horario manual.
                            </Alert>
                          )}
                        </Stack>
                      </Box>
                    )}

                    <Stack spacing={1.5}>
                      {slotInputs.slice(0, visibleSlotCount).map((slot, index) => (
                        <TextField
                          key={index}
                          label={index === 0 ? 'Horario preferido' : `Horario alternativo ${index}`}
                          type="datetime-local"
                          value={slot.start}
                          onChange={(e) => handleSlotChange(index, e.target.value)}
                          fullWidth
                          InputLabelProps={{ shrink: true }}
                          inputProps={{ min: minimumDateTimeValue }}
                          sx={fieldSx}
                        />
                      ))}
                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                        {visibleSlotCount < MAX_SLOT_COUNT && (
                          <Button type="button" variant="outlined" onClick={handleAddAlternativeSlot}>
                            Agregar otro horario
                          </Button>
                        )}
                        {filledSlotCount > 0 && (
                          <Button type="button" variant="text" onClick={handleClearSlots}>
                            Limpiar horarios
                          </Button>
                        )}
                      </Stack>
                    </Stack>

                    <TextField
                      label="Notas (opcional)"
                      value={notes}
                      onChange={(e) => setNotes(e.target.value)}
                      placeholder="Ej: Tengo experiencia básica en Ableton, prefiero tardes."
                      fullWidth
                      multiline
                      minRows={2}
                      sx={fieldSx}
                    />

                    {formError && <Alert severity="warning">{formError}</Alert>}
                    {submitError && <Alert severity="error">{submitError}</Alert>}
                    {submitted && (
                      <Alert icon={<CheckCircleIcon fontSize="inherit" />} severity="success">
                        Recibimos tu solicitud. Te contactaremos para confirmar la clase de prueba.
                      </Alert>
                    )}

                    <Button
                      type="submit"
                      variant="contained"
                      size="large"
                      disabled={submitting || subjectsQuery.isLoading}
                    >
                      {submitting ? 'Enviando…' : 'Enviar solicitud'}
                    </Button>
                  </Stack>
                </CardContent>
              </Card>
            </Grid>

            <Grid item xs={12} md={5}>
              <Stack spacing={2}>
                <Card
                  sx={{
                    background: 'rgba(255,255,255,0.04)',
                    border: '1px solid rgba(255,255,255,0.08)',
                    color: '#e2e8f0',
                  }}
                >
                  <CardContent>
                    <Stack spacing={1}>
                      <Typography variant="h6" fontWeight={700}>
                        ¿Qué sigue?
                      </Typography>
                      <Typography variant="body2" color="rgba(226,232,240,0.8)">
                        1) Revisamos disponibilidad. 2) Te confirmamos profesor y horario. 3) Tomas tu clase de prueba en TDF Records.
                      </Typography>
                      <Divider sx={{ borderColor: 'rgba(255,255,255,0.08)', my: 1 }} />
                      <Stack direction="row" spacing={1} alignItems="center">
                        <AccessTimeIcon fontSize="small" />
                        <Typography variant="body2">Respuesta habitual: 1 día hábil.</Typography>
                      </Stack>
                    </Stack>
                  </CardContent>
                </Card>

                <Card
                  sx={{
                    background: 'rgba(255,255,255,0.04)',
                    border: '1px solid rgba(255,255,255,0.08)',
                    color: '#e2e8f0',
                  }}
                >
                  <CardContent>
                    <Stack spacing={1}>
                      <Stack direction="row" spacing={1} alignItems="center">
                        <WhatsAppIcon fontSize="small" />
                        <Typography variant="h6" fontWeight={700}>
                          Prefieres WhatsApp
                        </Typography>
                      </Stack>
                      <Typography variant="body2" color="rgba(226,232,240,0.8)">
                        Si quieres resolverlo más rápido, escríbenos y comparte tu materia + horario ideal.
                      </Typography>
                      <Link
                        href={TRIALS_WHATSAPP_URL}
                        target="_blank"
                        rel="noreferrer"
                        underline="hover"
                        sx={{ fontWeight: 700 }}
                      >
                        Abrir WhatsApp
                      </Link>
                    </Stack>
                  </CardContent>
                </Card>
              </Stack>
            </Grid>
          </Grid>
        </Stack>
      </Container>
    </Box>
  );
}

function formatSlotLabel(startIso: string, endIso: string) {
  const start = new Date(startIso);
  const end = new Date(endIso);
  const sameDay =
    start.getFullYear() === end.getFullYear() &&
    start.getMonth() === end.getMonth() &&
    start.getDate() === end.getDate();

  const dateFormatter = new Intl.DateTimeFormat('es-EC', {
    weekday: 'short',
    day: 'numeric',
    month: 'short',
  });
  const timeFormatter = new Intl.DateTimeFormat('es-EC', {
    hour: 'numeric',
    minute: '2-digit',
  });

  const datePart = dateFormatter.format(start);
  const timePart = sameDay
    ? `${timeFormatter.format(start)} - ${timeFormatter.format(end)}`
    : `${timeFormatter.format(start)} -> ${dateFormatter.format(end)} ${timeFormatter.format(end)}`;

  return `${datePart}, ${timePart}`;
}
