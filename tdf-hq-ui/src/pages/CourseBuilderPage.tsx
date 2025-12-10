import { useEffect, useMemo, useState, useCallback } from 'react';
import { useMutation } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  Grid,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { Courses, type CourseUpsert } from '../api/courses';

interface SessionInput { label: string; date: string }
interface SyllabusInput { title: string; topics: string }

const DEFAULT_SESSIONS: SessionInput[] = [
  { label: 'Sábado 1 · Introducción', date: '2025-12-13' },
  { label: 'Sábado 2 · Grabación', date: '2025-12-20' },
  { label: 'Sábado 3 · Mezcla', date: '2025-12-27' },
  { label: 'Sábado 4 · Masterización', date: '2026-01-03' },
];

const DEFAULT_SYLLABUS: SyllabusInput[] = [
  { title: 'Introducción a la producción musical', topics: 'Conceptos básicos; Herramientas esenciales' },
  { title: 'Grabación y captura de audio', topics: 'Técnicas de grabación; Configuración de micrófonos' },
  { title: 'Mezcla y edición', topics: 'Ecualización y compresión; Balance y panoramización' },
  { title: 'Masterización y publicación', topics: 'Mastering; Distribución digital' },
];

const MONTH_SLUGS = ['ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'];

const slugifyValue = (value: string) =>
  value
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/-{2,}/g, '-')
    .replace(/(^-|-$)/g, '');

const stripCoursePrefix = (title: string) => title.replace(/^curso\s+(de\s+)?/i, '').trim();

const findEarliestSessionDate = (sessions: SessionInput[]) => {
  const validDates = sessions
    .map((s) => s.date.trim())
    .filter((d) => /^\d{4}-\d{2}-\d{2}$/.test(d));

  if (!validDates.length) return null;
  return validDates.sort()[0];
};

const generateSlug = (title: string, startDate: string | null) => {
  const cleanedTitle = stripCoursePrefix(title) || title;
  const titleSlug = slugifyValue(cleanedTitle) || 'curso';

  if (!startDate) return titleSlug;
  const match = /^(\d{4})-(\d{2})/.exec(startDate);
  if (!match) return titleSlug;

  const [, year, month] = match;
  const monthIdx = Number(month) - 1;
  const monthSlug = MONTH_SLUGS[monthIdx] ?? month;

  return [titleSlug, monthSlug, year].filter(Boolean).join('-');
};

const PUBLIC_BASE =
  (import.meta.env['VITE_PUBLIC_BASE'] as string | undefined)?.replace(/\/+$/, '') ??
  (typeof window !== 'undefined' && window.location.origin
    ? window.location.origin.replace(/\/+$/, '')
    : 'https://tdf-app.pages.dev');
const COURSE_PATH_BASE =
  (import.meta.env['VITE_PUBLIC_COURSE_BASE'] as string | undefined)?.replace(/\/+$/, '') ??
  `${PUBLIC_BASE}/curso`;

const DEFAULT_TITLE = 'Curso de Producción Musical';
const DEFAULT_SLUG = generateSlug(DEFAULT_TITLE, DEFAULT_SESSIONS[0]?.date ?? null);

export default function CourseBuilderPage() {
  const [title, setTitle] = useState(DEFAULT_TITLE);
  const [subtitle, setSubtitle] = useState('Presencial · 4 sábados · 16 horas');
  const [format, setFormat] = useState('Presencial');
  const [duration, setDuration] = useState('4 sábados · 16 horas');
  const [price, setPrice] = useState('349');
  const [currency, setCurrency] = useState('USD');
  const [capacity, setCapacity] = useState('16');
  const [sessionStartHour, setSessionStartHour] = useState('15');
  const [sessionDurationHours, setSessionDurationHours] = useState('4');
  const [locationLabel, setLocationLabel] = useState('TDF Records – Quito');
  const [locationMapUrl, setLocationMapUrl] = useState('https://maps.app.goo.gl/6pVYZ2CsbvQfGhAz6');
  const [whatsappCtaUrl, setWhatsappCtaUrl] = useState('https://wa.me/593995413168?text=Quiero%20inscribirme%20al%20curso');
  const landingFor = useCallback((s: string) => `${COURSE_PATH_BASE}/${s}`, []);
  const [landingUrl, setLandingUrl] = useState(landingFor(DEFAULT_SLUG));
  const [landingUrlTouched, setLandingUrlTouched] = useState(false);
  const [includes, setIncludes] = useState('Acceso a grabaciones\nCertificado de participación\nMentorías\nGrupo de WhatsApp\nAcceso a la plataforma de TDF Records');
  const [daws, setDaws] = useState('Logic\nLuna');
  const [sessions, setSessions] = useState<SessionInput[]>(DEFAULT_SESSIONS);
  const [syllabus, setSyllabus] = useState<SyllabusInput[]>(DEFAULT_SYLLABUS);

  const startDate = useMemo<string | null>(() => findEarliestSessionDate(sessions) ?? null, [sessions]);
  const slug = useMemo(() => generateSlug(title, startDate), [title, startDate]);
  const sessionDateErrors = useMemo(() => {
    const errors: string[] = [];
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    let lastDate: Date | null = null;
    sessions.forEach((s, idx) => {
      const raw = s.date.trim();
      if (!raw) {
        errors[idx] = 'Requerido';
        return;
      }
      const parsed = new Date(`${raw}T00:00:00`);
      if (Number.isNaN(parsed.getTime())) {
        errors[idx] = 'Fecha inválida';
        return;
      }
      if (parsed < today) {
        errors[idx] = 'No puede ser en el pasado';
      } else if (lastDate && parsed < lastDate) {
        errors[idx] = 'Mantén el orden cronológico';
      }
      lastDate = parsed;
    });
    return errors;
  }, [sessions]);

  useEffect(() => {
    if (!landingUrlTouched) {
      setLandingUrl(landingFor(slug));
    }
  }, [slug, landingUrlTouched, landingFor]);

  const handleLandingUrlChange = (value: string) => {
    setLandingUrl(value);
    setLandingUrlTouched(true);
  };
  const handleResetLanding = () => {
    setLandingUrl(landingFor(slug));
    setLandingUrlTouched(false);
  };

  const createMutation = useMutation({
    mutationFn: async () => {
      const payload = buildPayload();
      await Courses.upsert(payload);
    },
  });

  const buildPayload = (): CourseUpsert => {
    const toOptionalNumber = (value: string) => {
      const trimmed = value.trim();
      if (!trimmed) return null;
      const num = Number(trimmed);
      return Number.isFinite(num) ? num : null;
    };
    const cleanCapacity = Number.isFinite(Number(capacity)) ? Number(capacity) : 0;
    const cleanPrice = Math.round((Number(price) || 0) * 100);
    const landing = landingUrl.trim();
    const sessionStart = toOptionalNumber(sessionStartHour);
    const sessionDuration = toOptionalNumber(sessionDurationHours);
    return {
      slug: slug.trim(),
      title: title.trim(),
      subtitle: subtitle.trim() || null,
      format: format.trim() || null,
      duration: duration.trim() || null,
      priceCents: cleanPrice,
      currency: currency.trim() || 'USD',
      capacity: Math.max(0, Math.round(cleanCapacity)),
      sessionStartHour: sessionStart ?? null,
      sessionDurationHours: sessionDuration ?? null,
      locationLabel: locationLabel.trim() || null,
      locationMapUrl: locationMapUrl.trim() || null,
      daws: splitLines(daws),
      includes: splitLines(includes),
      sessions: sessions
        .filter((s) => s.label.trim() && s.date.trim())
        .map((s) => ({ label: s.label.trim(), date: s.date.trim() })),
      syllabus: syllabus
        .filter((s) => s.title.trim())
        .map((s) => ({
          title: s.title.trim(),
          topics: splitTopics(s.topics),
        })),
      whatsappCtaUrl: whatsappCtaUrl.trim() || null,
      landingUrl: landing || null,
    };
  };

  const splitLines = (input: string) =>
    input
      .split('\n')
      .map((line) => line.trim())
      .filter(Boolean);

  const splitTopics = (input: string) =>
    input
      .split(/;|,/)
      .map((t) => t.trim())
      .filter(Boolean);

  const handleSessionChange = (idx: number, field: keyof SessionInput, value: string) => {
    setSessions((prev) => prev.map((s, i) => (i === idx ? { ...s, [field]: value } : s)));
  };

  const handleSyllabusChange = (idx: number, field: keyof SyllabusInput, value: string) => {
    setSyllabus((prev) => prev.map((s, i) => (i === idx ? { ...s, [field]: value } : s)));
  };

  const handleDuplicateSession = (idx: number) => {
    setSessions((prev) => {
      const copy = prev[idx];
      if (!copy) return prev;
      return [...prev.slice(0, idx + 1), { ...copy }, ...prev.slice(idx + 1)];
    });
  };

  const payloadPreview = JSON.stringify(buildPayload(), null, 2);
  const canResetLanding = landingUrlTouched && landingUrl !== landingFor(slug);

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems="flex-start" spacing={2}>
        <Box>
          <Typography variant="overline" color="text.secondary">Cursos</Typography>
          <Typography variant="h4" fontWeight={800}>Crear curso</Typography>
          <Typography color="text.secondary">
            Publica un nuevo curso. Requiere permisos de administrador/webmaster (guarda en tablas de Escuela).
          </Typography>
        </Box>
        <Stack direction="row" spacing={1}>
          <Chip label={`Slug: ${slug}`} color="primary" />
          <Chip label={`Estado: ${createMutation.isSuccess ? 'Publicado' : 'Borrador'}`} />
        </Stack>
      </Stack>

      {createMutation.isSuccess && (
        <Alert severity="success">
          Curso publicado. Revisa la landing en <strong>/curso/{slug}</strong>.
        </Alert>
      )}
      {createMutation.isError && (
        <Alert severity="error">No pudimos guardar el curso. Revisa los campos obligatorios.</Alert>
      )}

      <Card variant="outlined">
        <CardContent>
          <Grid container spacing={2}>
            <Grid item xs={12} md={6}>
              <TextField
                label="Slug (auto)"
                fullWidth
                value={slug}
                InputProps={{ readOnly: true }}
                helperText="Se genera con el título y la fecha de inicio."
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                label="Landing URL"
                fullWidth
                value={landingUrl}
                onChange={(e) => handleLandingUrlChange(e.target.value)}
                helperText={canResetLanding ? 'Editaste la URL; puedes restablecer la sugerida.' : undefined}
              />
              {canResetLanding && (
                <Box sx={{ mt: 0.5 }}>
                  <Button size="small" onClick={handleResetLanding}>
                    Restablecer URL sugerida
                  </Button>
                </Box>
              )}
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField label="Título" fullWidth value={title} onChange={(e) => setTitle(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField label="Subtítulo" fullWidth value={subtitle} onChange={(e) => setSubtitle(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField label="Formato" fullWidth value={format} onChange={(e) => setFormat(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField label="Duración" fullWidth value={duration} onChange={(e) => setDuration(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField label="Precio" fullWidth value={price} onChange={(e) => setPrice(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField label="Moneda" fullWidth value={currency} onChange={(e) => setCurrency(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField label="Cupos" fullWidth value={capacity} onChange={(e) => setCapacity(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Hora de inicio (24h)"
                fullWidth
                value={sessionStartHour}
                onChange={(e) => setSessionStartHour(e.target.value)}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField
                label="Duración por sesión (h)"
                fullWidth
                value={sessionDurationHours}
                onChange={(e) => setSessionDurationHours(e.target.value)}
              />
            </Grid>
            <Grid item xs={12} md={3}>
              <TextField label="DAWs" fullWidth multiline minRows={3} value={daws} onChange={(e) => setDaws(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField label="Incluye" fullWidth multiline minRows={3} value={includes} onChange={(e) => setIncludes(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField label="WhatsApp CTA" fullWidth value={whatsappCtaUrl} onChange={(e) => setWhatsappCtaUrl(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField label="Lugar" fullWidth value={locationLabel} onChange={(e) => setLocationLabel(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField label="Mapa" fullWidth value={locationMapUrl} onChange={(e) => setLocationMapUrl(e.target.value)} />
            </Grid>
          </Grid>
        </CardContent>
      </Card>

      <Card variant="outlined">
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={800}>Sesiones</Typography>
            {sessions.map((s, idx) => (
              <Grid container spacing={2} key={idx} alignItems="center">
                <Grid item xs={12} md={6}>
                  <TextField
                    label="Título"
                    fullWidth
                    value={s.label}
                    onChange={(e) => handleSessionChange(idx, 'label', e.target.value)}
                  />
                </Grid>
                <Grid item xs={12} md={4}>
                  <TextField
                    label="Fecha (YYYY-MM-DD)"
                    fullWidth
                    value={s.date}
                    onChange={(e) => handleSessionChange(idx, 'date', e.target.value)}
                    error={Boolean(sessionDateErrors[idx])}
                    helperText={sessionDateErrors[idx] ?? undefined}
                  />
                </Grid>
                <Grid item xs={12} md={2}>
                  <Stack direction="row" spacing={1}>
                    <Button variant="text" size="small" onClick={() => handleDuplicateSession(idx)}>
                      Duplicar
                    </Button>
                    <Button
                      variant="text"
                      size="small"
                      color="error"
                      onClick={() => setSessions((prev) => prev.filter((_, i) => i !== idx))}
                      disabled={sessions.length <= 1}
                    >
                      Borrar
                    </Button>
                  </Stack>
                </Grid>
              </Grid>
            ))}
            <Button variant="outlined" onClick={() => setSessions((prev) => [...prev, { label: '', date: '' }])}>
              Añadir sesión
            </Button>
          </Stack>
        </CardContent>
      </Card>

      <Card variant="outlined">
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={800}>Temario</Typography>
            {syllabus.map((s, idx) => (
              <Grid container spacing={2} key={idx}>
                <Grid item xs={12} md={6}>
                  <TextField
                    label="Título de la sección"
                    fullWidth
                    value={s.title}
                    onChange={(e) => handleSyllabusChange(idx, 'title', e.target.value)}
                  />
                </Grid>
                <Grid item xs={12} md={6}>
                  <TextField
                    label="Temas (separa con ; o ,)"
                    fullWidth
                    value={s.topics}
                    onChange={(e) => handleSyllabusChange(idx, 'topics', e.target.value)}
                  />
                </Grid>
              </Grid>
            ))}
            <Button variant="outlined" onClick={() => setSyllabus((prev) => [...prev, { title: '', topics: '' }])}>
              Añadir sección
            </Button>
          </Stack>
        </CardContent>
      </Card>

      <Card variant="outlined">
        <CardContent>
          <Stack spacing={2}>
            <Typography variant="h6" fontWeight={800}>Revisar y publicar</Typography>
            <TextField
              label="Payload"
              multiline
              minRows={8}
              value={payloadPreview}
              InputProps={{ readOnly: true, sx: { fontFamily: 'monospace' } }}
            />
            <Button
              variant="contained"
              onClick={() => createMutation.mutate()}
              disabled={createMutation.isPending}
            >
              {createMutation.isPending ? 'Guardando...' : 'Publicar curso'}
            </Button>
          </Stack>
        </CardContent>
      </Card>
    </Stack>
  );
}
