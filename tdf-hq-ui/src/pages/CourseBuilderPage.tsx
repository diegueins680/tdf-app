import { useState } from 'react';
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
import { Cms } from '../api/cms';

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

export default function CourseBuilderPage() {
  const [slug, setSlug] = useState('produccion-musical-dic-2025');
  const [title, setTitle] = useState('Curso de Producción Musical');
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
  const [landingUrl, setLandingUrl] = useState('https://tdf-app.pages.dev/curso/produccion-musical-dic-2025');
  const [includes, setIncludes] = useState('Acceso a grabaciones\nCertificado de participación\nMentorías\nGrupo de WhatsApp\nAcceso a la plataforma de TDF Records');
  const [daws, setDaws] = useState('Logic\nLuna');
  const [sessions, setSessions] = useState<SessionInput[]>(DEFAULT_SESSIONS);
  const [syllabus, setSyllabus] = useState<SyllabusInput[]>(DEFAULT_SYLLABUS);

  const createMutation = useMutation({
    mutationFn: async () => {
      const payload = buildPayload();
      const landing = landingUrl.trim() || `https://tdf-app.pages.dev/curso/${slug}`;
      await Cms.create({
        cciSlug: `course:${slug}`,
        cciLocale: 'es',
        cciTitle: title.trim(),
        cciStatus: 'published',
        cciPayload: {
          ...payload,
          landingUrl: landing,
          remaining: payload.capacity,
          whatsappCtaUrl: whatsappCtaUrl.trim() || payload.whatsappCtaUrl,
        },
      });
    },
  });

  const buildPayload = () => {
    const cleanCapacity = Number(capacity) || 0;
    const cleanPrice = Number(price) || 0;
    const landing = landingUrl.trim() || `https://tdf-app.pages.dev/curso/${slug}`;
    const sessionStart = Number(sessionStartHour) || 0;
    const sessionDuration = Number(sessionDurationHours) || 0;
    return {
      slug: slug.trim(),
      title: title.trim(),
      subtitle: subtitle.trim(),
      format: format.trim(),
      duration: duration.trim(),
      price: cleanPrice,
      currency: currency.trim() || 'USD',
      capacity: cleanCapacity,
      remaining: cleanCapacity,
      sessionStartHour: sessionStart,
      sessionDurationHours: sessionDuration,
      locationLabel: locationLabel.trim(),
      locationMapUrl: locationMapUrl.trim(),
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
      whatsappCtaUrl: whatsappCtaUrl.trim() || `https://wa.me/?text=Curso%20${encodeURIComponent(title.trim())}`,
      landingUrl: landing,
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

  const payloadPreview = JSON.stringify(buildPayload(), null, 2);

  return (
    <Stack spacing={3}>
      <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" alignItems="flex-start" spacing={2}>
        <Box>
          <Typography variant="overline" color="text.secondary">Cursos</Typography>
          <Typography variant="h4" fontWeight={800}>Crear curso</Typography>
          <Typography color="text.secondary">
            Publica un nuevo curso. Requiere permisos de administrador/webmaster (usa CMS debajo).
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
              <TextField label="Slug" fullWidth value={slug} onChange={(e) => setSlug(e.target.value)} />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField label="Landing URL" fullWidth value={landingUrl} onChange={(e) => setLandingUrl(e.target.value)} />
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
              <Grid container spacing={2} key={idx}>
                <Grid item xs={12} md={8}>
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
                  />
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
