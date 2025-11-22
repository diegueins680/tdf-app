import { useMemo, useRef, useState, type ReactElement, type RefObject } from 'react';
import { useMutation, useQuery } from '@tanstack/react-query';
import {
  Alert,
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  CardMedia,
  Chip,
  CircularProgress,
  Container,
  Divider,
  Grid,
  Link,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import CelebrationIcon from '@mui/icons-material/Celebration';
import MusicNoteIcon from '@mui/icons-material/MusicNote';
import PlaceIcon from '@mui/icons-material/Place';
import VerifiedIcon from '@mui/icons-material/Verified';
import WhatsAppIcon from '@mui/icons-material/WhatsApp';
import CalendarTodayIcon from '@mui/icons-material/CalendarToday';
import HeadsetIcon from '@mui/icons-material/Headset';
import CheckCircleIcon from '@mui/icons-material/CheckCircle';
import type { CourseMetadata, CourseRegistrationRequest } from '../api/courses';
import { Courses } from '../api/courses';
import instructorImage from '../assets/tdf-ui/esteban-munoz.jpg';

const COURSE_SLUG = 'produccion-musical-dic-2025';
const INSTRUCTOR_IMAGE_URL = instructorImage;

const badgeStyle = {
  bgcolor: 'rgba(255,255,255,0.1)',
  color: '#f8fafc',
  borderRadius: 999,
  px: 1.5,
  py: 0.5,
  border: '1px solid rgba(255,255,255,0.18)',
  fontWeight: 600,
  letterSpacing: 0.4,
};

export default function CourseProductionLandingPage() {
  const formRef = useRef<HTMLDivElement | null>(null);
  const [fullName, setFullName] = useState('');
  const [email, setEmail] = useState('');
  const [phone, setPhone] = useState('');
  const [howHeard, setHowHeard] = useState('');

  const metaQuery = useQuery({
    queryKey: ['course-meta', COURSE_SLUG],
    queryFn: () => Courses.getMetadata(COURSE_SLUG),
  });

  const utmParams = useMemo(() => {
    if (typeof window === 'undefined') return undefined;
    const params = new URLSearchParams(window.location.search);
    const source = params.get('utm_source') ?? undefined;
    const medium = params.get('utm_medium') ?? undefined;
    const campaign = params.get('utm_campaign') ?? undefined;
    const content = params.get('utm_content') ?? undefined;
    const hasUtm = [source, medium, campaign, content].some(
      (value) => value !== undefined && value !== null && value !== '',
    );
    if (hasUtm) {
      return { source, medium, campaign, content };
    }
    return undefined;
  }, []);

  const registrationMutation = useMutation({
    mutationFn: (payload: CourseRegistrationRequest) => Courses.register(COURSE_SLUG, payload),
  });

  const handleSubmit = (evt: React.FormEvent<HTMLFormElement>) => {
    evt.preventDefault();
    const payload: CourseRegistrationRequest = {
      fullName,
      email,
      phoneE164: phone.trim() ? phone.trim() : undefined,
      source: 'landing',
      howHeard: howHeard.trim() ? howHeard.trim() : undefined,
      utm: utmParams,
    };
    registrationMutation.mutate(payload);
  };

  const scrollToForm = () => {
    if (formRef.current) {
      formRef.current.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  };

  const meta: CourseMetadata | undefined = metaQuery.data;
  const whatsappHref = meta?.whatsappCtaUrl ?? 'https://wa.me/?text=INSCRIBIRME%20Curso%20Produccion%20Musical';
  const patchedSessions = useMemo(() => {
    const targetDates = ['2025-12-13', '2025-12-20', '2025-12-27', '2026-01-03'];
    if (!meta?.sessions?.length) return undefined;
    return meta.sessions.map((s, idx) => ({
      ...s,
      date: targetDates[idx] ?? s.date,
    }));
  }, [meta?.sessions]);

  const submitted = registrationMutation.isSuccess;
  const submitting = registrationMutation.isPending;
  const submitError = registrationMutation.error instanceof Error ? registrationMutation.error.message : null;

  return (
    <Box
      sx={{
        minHeight: '100vh',
        bgcolor: '#0c1020',
        color: '#e2e8f0',
        background: 'radial-gradient(circle at 10% 20%, rgba(79,70,229,0.12), transparent 35%), radial-gradient(circle at 80% 0%, rgba(14,165,233,0.12), transparent 35%), linear-gradient(180deg, #0b0f1b, #0e1224)',
      }}
    >
      <Container maxWidth="lg" sx={{ py: { xs: 4, md: 6 } }}>
        <Stack spacing={4}>
          {metaQuery.error && (
            <Alert severity="error">
              No pudimos cargar la información del curso. Intenta de nuevo o escríbenos por WhatsApp.
            </Alert>
          )}
          <Hero meta={meta} onPrimaryClick={scrollToForm} whatsappHref={whatsappHref} loading={metaQuery.isLoading} />
          <Grid container spacing={3}>
            <Grid item xs={12} md={7}>
              <Info meta={meta} loading={metaQuery.isLoading} sessionsOverride={patchedSessions} />
            </Grid>
            <Grid item xs={12} md={5}>
              <FormCard
                formRef={formRef}
                onSubmit={handleSubmit}
                fullName={fullName}
                email={email}
                phone={phone}
                howHeard={howHeard}
                onFullNameChange={setFullName}
                onEmailChange={setEmail}
                onPhoneChange={setPhone}
                onHowHeardChange={setHowHeard}
                submitting={submitting}
                submitted={submitted}
                submitError={submitError}
              />
              <InstructorCard />
              {meta?.locationLabel && meta?.locationMapUrl && (
                <LocationCard label={meta.locationLabel} mapUrl={meta.locationMapUrl} />
              )}
            </Grid>
          </Grid>
        </Stack>
      </Container>
    </Box>
  );
}

function InstructorCard() {
  return (
    <Card
      sx={{
        mt: 3,
        background: 'rgba(255,255,255,0.03)',
        border: '1px solid rgba(255,255,255,0.08)',
        color: '#e2e8f0',
      }}
    >
      <CardMedia
        component="img"
        image={INSTRUCTOR_IMAGE_URL}
        alt="Esteban Muñoz en el control room"
        sx={{ height: 220, objectFit: 'cover' }}
      />
      <CardContent sx={{ pb: 3 }}>
        <Stack direction="row" spacing={2} alignItems="center" mb={1}>
          <Avatar alt="Esteban Muñoz" src={INSTRUCTOR_IMAGE_URL} />
          <Box>
            <Typography variant="subtitle1" sx={{ color: '#f8fafc', fontWeight: 700 }}>
              Esteban Muñoz
            </Typography>
            <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.7)' }}>
              Ingeniero de mezcla · Mentor del curso
            </Typography>
          </Box>
        </Stack>
        <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.75)' }}>
          Productor e ingeniero residente en TDF Records, con experiencia en grabación, mezcla y masterización
          en Logic y Luna. Te acompañará en sesiones prácticas dentro del control room del estudio.
        </Typography>
      </CardContent>
    </Card>
  );
}

function Hero({
  meta,
  loading,
  onPrimaryClick,
  whatsappHref,
}: {
  meta?: CourseMetadata;
  loading: boolean;
  onPrimaryClick: () => void;
  whatsappHref: string;
}) {
  return (
    <Box
      sx={{
        borderRadius: 4,
        p: { xs: 3, md: 5 },
        background: 'linear-gradient(120deg, rgba(124,58,237,0.25), rgba(14,165,233,0.2))',
        border: '1px solid rgba(255,255,255,0.08)',
        boxShadow: '0 20px 60px rgba(0,0,0,0.25)',
      }}
    >
      <Stack spacing={2}>
        <Stack direction="row" spacing={1} alignItems="center">
          <Chip icon={<VerifiedIcon />} label="Plazas limitadas" color="default" sx={{ bgcolor: 'rgba(255,255,255,0.12)', color: '#e2e8f0' }} />
          <Chip icon={<HeadsetIcon />} label="Mentorías incluidas" sx={{ bgcolor: 'rgba(255,255,255,0.12)', color: '#e2e8f0' }} />
          <Chip icon={<CalendarTodayIcon />} label="Dic 2025 / Ene 2026" sx={{ bgcolor: 'rgba(255,255,255,0.12)', color: '#e2e8f0' }} />
        </Stack>
        <Typography variant="h3" fontWeight={700} sx={{ color: '#f8fafc' }}>
          {loading ? 'Cargando curso…' : meta?.title ?? 'Curso de Producción Musical'}
        </Typography>
        <Typography variant="h6" sx={{ color: 'rgba(226,232,240,0.85)', maxWidth: 820 }}>
          {loading ? 'Preparando detalles...' : meta?.subtitle ?? 'Presencial · 4 sábados · 16 horas'}
        </Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ xs: 'stretch', sm: 'center' }}>
          <Typography variant="h4" fontWeight={800} sx={{ color: '#cbd5f5' }}>
            {loading ? '—' : `$${meta?.price?.toFixed(0) ?? '150'} ${meta?.currency ?? 'USD'}`}
          </Typography>
          <Typography variant="body1" sx={{ color: 'rgba(226,232,240,0.75)' }}>
            {loading ? '—' : `Cupo: ${meta?.capacity ?? 10} personas · ${meta?.format ?? 'Presencial'}`}
          </Typography>
        </Stack>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
          <Button
            variant="contained"
            size="large"
            onClick={onPrimaryClick}
            sx={{
              bgcolor: '#7c3aed',
              color: '#f8fafc',
              px: 3,
              boxShadow: '0 14px 30px rgba(124,58,237,0.35)',
            }}
          >
            Inscribirme
          </Button>
          <Button
            variant="outlined"
            size="large"
            startIcon={<WhatsAppIcon />}
            href={whatsappHref}
            target="_blank"
            rel="noreferrer"
            sx={{
              borderColor: 'rgba(255,255,255,0.3)',
              color: '#e2e8f0',
            }}
          >
            Inscribirme por WhatsApp
          </Button>
        </Stack>
      </Stack>
    </Box>
  );
}

function Info({ meta, loading, sessionsOverride }: { meta?: CourseMetadata; loading: boolean; sessionsOverride?: CourseMetadata['sessions'] }) {
  const formatDate = (value?: string | null) => {
    if (!value) return '—';
    const parsed = new Date(value);
    if (Number.isNaN(parsed.getTime())) return value;
    return parsed.toLocaleDateString('es-EC', { day: '2-digit', month: 'short', year: 'numeric' });
  };

  return (
    <Stack spacing={3}>
      <Card
        sx={{
          background: 'rgba(255,255,255,0.02)',
          border: '1px solid rgba(255,255,255,0.08)',
          color: '#e2e8f0',
        }}
      >
        <CardContent>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} flexWrap="wrap" useFlexGap>
            <Badge icon={<CelebrationIcon />} label="Presencial" />
            <Badge icon={<HeadsetIcon />} label="4 sábados · 16h" />
            <Badge icon={<MusicNoteIcon />} label="DAWs: Logic y Luna" />
            <Badge icon={<CheckCircleIcon />} label="Incluye grabaciones y certificado" />
          </Stack>
          <Divider sx={{ my: 2, borderColor: 'rgba(255,255,255,0.1)' }} />
          <Typography variant="subtitle1" gutterBottom sx={{ color: '#cbd5f5', fontWeight: 700 }}>
            Fechas
          </Typography>
          {loading && <Typography>Cargando fechas...</Typography>}
          {!loading && (
            <Stack spacing={1.2}>
              {(sessionsOverride ?? meta?.sessions ?? []).map((session) => (
                <Stack
                  key={`${session.date}-${session.label}`}
                  direction="row"
                  spacing={1}
                  alignItems="center"
                  sx={{ bgcolor: 'rgba(255,255,255,0.02)', borderRadius: 2, px: 1.5, py: 1 }}
                >
                  <Chip
                    icon={<CalendarTodayIcon />}
                    label={session.label}
                    size="small"
                    sx={badgeStyle}
                  />
                  <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.8)' }}>
                    {formatDate(session.date)}
                  </Typography>
                </Stack>
              ))}
            </Stack>
          )}
          <Divider sx={{ my: 2, borderColor: 'rgba(255,255,255,0.1)' }} />
          <Typography variant="subtitle1" gutterBottom sx={{ color: '#cbd5f5', fontWeight: 700 }}>
            Syllabus
          </Typography>
          {loading && <Typography>Cargando syllabus...</Typography>}
          {!loading && (
            <Stack spacing={1.5}>
              {meta?.syllabus?.map((item) => {
                const topics = item.topics ?? [];
                return (
                <Box key={item.title} sx={{ p: 1.5, borderRadius: 2, bgcolor: 'rgba(255,255,255,0.02)' }}>
                  <Typography variant="subtitle2" sx={{ color: '#e2e8f0', fontWeight: 700 }}>
                    {item.title}
                  </Typography>
                  <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.8)', mt: 0.5 }}>
                    {topics.join(' · ')}
                  </Typography>
                </Box>
              );
              })}
            </Stack>
          )}
          <Divider sx={{ my: 2, borderColor: 'rgba(255,255,255,0.1)' }} />
          <Typography variant="subtitle1" gutterBottom sx={{ color: '#cbd5f5', fontWeight: 700 }}>
            Incluye
          </Typography>
          <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
            {(meta?.includes ?? ['Acceso a grabaciones', 'Certificado de participación', 'Mentorías', 'Grupo de WhatsApp', 'Acceso a la plataforma de TDF Records']).map((item) => (
              <Chip key={item} icon={<CheckCircleIcon />} label={item} sx={badgeStyle} />
            ))}
          </Stack>
        </CardContent>
      </Card>
    </Stack>
  );
}

function FormCard({
  formRef,
  onSubmit,
  fullName,
  email,
  phone,
  howHeard,
  onFullNameChange,
  onEmailChange,
  onPhoneChange,
  onHowHeardChange,
  submitting,
  submitted,
  submitError,
}: {
  formRef: RefObject<HTMLDivElement>;
  onSubmit: (evt: React.FormEvent<HTMLFormElement>) => void;
  fullName: string;
  email: string;
  phone: string;
  howHeard: string;
  onFullNameChange: (val: string) => void;
  onEmailChange: (val: string) => void;
  onPhoneChange: (val: string) => void;
  onHowHeardChange: (val: string) => void;
  submitting: boolean;
  submitted: boolean;
  submitError: string | null;
}) {
  return (
    <Card
      ref={formRef}
      sx={{
        background: 'rgba(255,255,255,0.03)',
        border: '1px solid rgba(255,255,255,0.08)',
        color: '#e2e8f0',
      }}
    >
      <CardContent>
        <Stack spacing={2}>
          <Typography variant="h6" sx={{ color: '#f8fafc', fontWeight: 700 }}>
            Reserva tu cupo
          </Typography>
          <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.75)' }}>
            Déjanos tus datos y te enviaremos los pasos para completar el pago. Cupos limitados a 10 personas.
          </Typography>
          <Box component="form" onSubmit={onSubmit}>
            <Stack spacing={1.5}>
              <TextField
                label="Nombre completo"
                required
                value={fullName}
                onChange={(e) => onFullNameChange(e.target.value)}
                disabled={submitted}
                fullWidth
                InputProps={{
                  sx: {
                    color: '#f8fafc',
                    '& .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.28)' },
                    '&:hover .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.45)' },
                    '&.Mui-focused .MuiOutlinedInput-notchedOutline': { borderColor: '#93c5fd' },
                    input: {
                      color: '#f8fafc',
                      '::placeholder': { color: 'rgba(226,232,240,0.6)' },
                      caretColor: '#f8fafc',
                    },
                  },
                }}
                InputLabelProps={{ sx: { color: 'rgba(226,232,240,0.75)' } }}
              />
              <TextField
                label="Correo"
                type="email"
                required
                value={email}
                onChange={(e) => onEmailChange(e.target.value)}
                disabled={submitted}
                fullWidth
                InputProps={{
                  sx: {
                    color: '#f8fafc',
                    '& .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.28)' },
                    '&:hover .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.45)' },
                    '&.Mui-focused .MuiOutlinedInput-notchedOutline': { borderColor: '#93c5fd' },
                    input: {
                      color: '#f8fafc',
                      '::placeholder': { color: 'rgba(226,232,240,0.6)' },
                      caretColor: '#f8fafc',
                    },
                  },
                }}
                InputLabelProps={{ sx: { color: 'rgba(226,232,240,0.75)' } }}
              />
              <TextField
                label="WhatsApp (opcional)"
                value={phone}
                onChange={(e) => onPhoneChange(e.target.value)}
                disabled={submitted}
                fullWidth
                InputProps={{
                  sx: {
                    color: '#f8fafc',
                    '& .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.22)' },
                    '&:hover .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.4)' },
                    '&.Mui-focused .MuiOutlinedInput-notchedOutline': { borderColor: '#93c5fd' },
                    input: {
                      color: '#f8fafc',
                      '::placeholder': { color: 'rgba(226,232,240,0.6)' },
                      caretColor: '#f8fafc',
                    },
                  },
                }}
                InputLabelProps={{ sx: { color: 'rgba(226,232,240,0.68)' } }}
              />
              <TextField
                label="¿Cómo te enteraste del curso? (opcional)"
                value={howHeard}
                onChange={(e) => onHowHeardChange(e.target.value)}
                disabled={submitted}
                fullWidth
                multiline
                minRows={2}
                InputProps={{
                  sx: {
                    color: '#f8fafc',
                    '& .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.22)' },
                    '&:hover .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.4)' },
                    '&.Mui-focused .MuiOutlinedInput-notchedOutline': { borderColor: '#93c5fd' },
                    textarea: {
                      color: '#f8fafc',
                      '::placeholder': { color: 'rgba(226,232,240,0.6)' },
                      caretColor: '#f8fafc',
                    },
                  },
                }}
                InputLabelProps={{ sx: { color: 'rgba(226,232,240,0.68)' } }}
              />
              <Button
                type="submit"
                variant="contained"
                disabled={submitted || submitting}
                startIcon={submitting ? <CircularProgress size={18} color="inherit" /> : <CelebrationIcon />}
                sx={{ mt: 1 }}
              >
                {submitted ? 'Inscripción recibida' : 'Enviar inscripción'}
              </Button>
            </Stack>
          </Box>
          {submitted && (
            <Alert severity="success">
              ¡Gracias! Hemos recibido tu inscripción. Te contactaremos por correo/WhatsApp con los pasos para completar el pago.
            </Alert>
          )}
          {submitError && (
            <Alert severity="error">
              No pudimos registrar tu inscripción. Intenta de nuevo o escríbenos por WhatsApp.
            </Alert>
          )}
        </Stack>
      </CardContent>
    </Card>
  );
}

function LocationCard({ label, mapUrl }: { label: string; mapUrl: string }) {
  return (
    <Card
      sx={{
        mt: 3,
        background: 'rgba(255,255,255,0.03)',
        border: '1px solid rgba(255,255,255,0.08)',
        color: '#e2e8f0',
      }}
    >
      <CardContent>
        <Stack spacing={1}>
          <Typography variant="subtitle1" sx={{ color: '#f8fafc', fontWeight: 700 }}>
            Ubicación
          </Typography>
          <Stack direction="row" spacing={1} alignItems="center">
            <PlaceIcon fontSize="small" />
            <Typography variant="body2">{label}</Typography>
          </Stack>
          <Link href={mapUrl} target="_blank" rel="noreferrer" sx={{ color: '#93c5fd' }}>
            Ver mapa
          </Link>
        </Stack>
      </CardContent>
    </Card>
  );
}

function Badge({ icon, label }: { icon: ReactElement; label: string }) {
  return (
    <Chip
      icon={icon}
      label={label}
      sx={{
        bgcolor: 'rgba(255,255,255,0.08)',
        color: '#f8fafc',
        borderRadius: 999,
        px: 0.5,
      }}
    />
  );
}
