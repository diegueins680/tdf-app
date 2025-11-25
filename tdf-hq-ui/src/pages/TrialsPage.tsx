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

type SlotInput = { start: string; end: string };

const emptySlots: SlotInput[] = [
  { start: '', end: '' },
  { start: '', end: '' },
  { start: '', end: '' },
];

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
  const [slotInputs, setSlotInputs] = useState<SlotInput[]>(emptySlots);
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
      setSlotInputs(emptySlots);
    },
  });

  const handleSlotChange = (index: number, field: keyof SlotInput, value: string) => {
    setSlotInputs((prev) => prev.map((slot, idx) => (idx === index ? { ...slot, [field]: value } : slot)));
  };

  const parseIsoOrNull = (value: string) => {
    if (!value) return null;
    const d = new Date(value);
    return Number.isNaN(d.getTime()) ? null : d.toISOString();
  };

  const handleSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setFormError(null);

    if (!subjectId) {
      setFormError('Elige la materia para la clase de prueba.');
      return;
    }

    const filledSlots = slotInputs.filter((slot) => slot.start && slot.end);
    if (filledSlots.length === 0) {
      setFormError('Agrega al menos un horario preferido.');
      return;
    }

    const preferred = filledSlots
      .map((slot) => ({
        startAt: parseIsoOrNull(slot.start),
        endAt: parseIsoOrNull(slot.end),
      }))
      .filter((slot) => slot.startAt && slot.endAt) as { startAt: string; endAt: string }[];

    const invalidRange = preferred.find((slot) => new Date(slot.endAt) <= new Date(slot.startAt));
    if (invalidRange) {
      setFormError('Revisa tus horarios: la hora de fin debe ser mayor a la de inicio.');
      return;
    }

    if (!preferred.length) {
      setFormError('Los horarios ingresados no son válidos.');
      return;
    }

    const contactNote = [
      fullName && `Nombre: ${fullName}`,
      email && `Email: ${email}`,
      phone && `Tel: ${phone}`,
      notes && `Notas: ${notes}`,
    ]
      .filter(Boolean)
      .join(' | ');

    requestMutation.mutate({
      subjectId: Number(subjectId),
      preferred,
      notes: contactNote || undefined,
    });
  };

  const subjects: TrialSubject[] = useMemo(
    () => (subjectsQuery.data ?? []).filter((s) => s.active),
    [subjectsQuery.data],
  );

  const suggestedSlots = slotsQuery.data ?? [];
  const submitting = requestMutation.isPending;
  const submitted = requestMutation.isSuccess;
  const submitError =
    requestMutation.error instanceof Error ? requestMutation.error.message : null;

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
              Elige la materia, dinos cuándo puedes y te contactamos para agendar.
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
                    <TextField
                      label="Materia"
                      select
                      value={subjectId}
                      onChange={(e) => setSubjectId(Number(e.target.value))}
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
                    <Stack spacing={2}>
                      {slotInputs.map((slot, idx) => (
                        <Grid container spacing={1.5} key={idx}>
                          <Grid item xs={12} sm={6}>
                            <TextField
                              label={`Preferencia ${idx + 1} - inicio`}
                              type="datetime-local"
                              value={slot.start}
                              onChange={(e) => handleSlotChange(idx, 'start', e.target.value)}
                              fullWidth
                              InputLabelProps={{ shrink: true }}
                              sx={fieldSx}
                            />
                          </Grid>
                          <Grid item xs={12} sm={6}>
                            <TextField
                              label={`Preferencia ${idx + 1} - fin`}
                              type="datetime-local"
                              value={slot.end}
                              onChange={(e) => handleSlotChange(idx, 'end', e.target.value)}
                              fullWidth
                              InputLabelProps={{ shrink: true }}
                              sx={fieldSx}
                            />
                          </Grid>
                        </Grid>
                      ))}
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
                      disabled={submitting}
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
                        1) Revisamos disponibilidad. 2) Te confirmamos profesor y horario. 3) Toma tu
                        clase de prueba en TDF Records.
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
                        Escríbenos y comparte tus horarios:
                      </Typography>
                      <Link
                        href="https://wa.me/593999001122?text=Hola%20quiero%20una%20clase%20de%20prueba%20en%20TDF%20Records"
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

                {slotsQuery.isLoading && (
                  <Card
                    sx={{
                      background: 'rgba(255,255,255,0.04)',
                      border: '1px solid rgba(255,255,255,0.08)',
                      color: '#e2e8f0',
                      textAlign: 'center',
                    }}
                  >
                    <CardContent>
                      <CircularProgress size={24} />
                      <Typography variant="body2" sx={{ mt: 1 }}>
                        Buscando horarios sugeridos…
                      </Typography>
                    </CardContent>
                  </Card>
                )}

                {suggestedSlots.length > 0 && (
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
                          Horarios sugeridos
                        </Typography>
                        <Typography variant="body2" color="rgba(226,232,240,0.8)">
                          Elige uno parecido en tu solicitud. Mostramos disponibilidad aproximada por
                          profesor.
                        </Typography>
                        <Stack spacing={1} sx={{ mt: 1 }}>
                          {suggestedSlots.map((slot) => (
                            <Box
                              key={`${slot.teacherId}-${slot.subjectId}`}
                              sx={{
                                p: 1.2,
                                border: '1px solid rgba(255,255,255,0.1)',
                                borderRadius: 1,
                                bgcolor: 'rgba(15,23,42,0.5)',
                              }}
                            >
                              <Typography variant="subtitle2" fontWeight={700}>
                                {slot.teacherName}
                              </Typography>
                              <Stack direction="row" spacing={1} flexWrap="wrap" mt={0.5}>
                                {slot.slots.map((s, idx) => (
                                  <Chip
                                    key={idx}
                                    size="small"
                                    label={formatSlotLabel(s.startAt, s.endAt)}
                                    sx={{ bgcolor: 'rgba(255,255,255,0.08)', color: '#e2e8f0' }}
                                  />
                                ))}
                              </Stack>
                            </Box>
                          ))}
                        </Stack>
                      </Stack>
                    </CardContent>
                  </Card>
                )}

                {slotsQuery.error && (
                  <Alert severity="warning">
                    No pudimos cargar horarios sugeridos. Igual puedes enviar tu solicitud.
                  </Alert>
                )}
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
