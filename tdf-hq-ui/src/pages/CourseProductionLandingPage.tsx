import { useEffect, useMemo, useRef, useState, type ReactElement, type RefObject, type SyntheticEvent } from 'react';
import { useMutation, useQueries, useQuery } from '@tanstack/react-query';
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
  MenuItem,
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
import EnrollmentSuccessDialog from '../components/EnrollmentSuccessDialog';
import PublicBrandBar from '../components/PublicBrandBar';
import { useCmsContent } from '../hooks/useCmsContent';
import { COURSE_COHORTS, COURSE_DEFAULTS, PUBLIC_BASE } from '../config/appConfig';
import { useLocation, useNavigate, useParams } from 'react-router-dom';

const isAbsoluteUrl = (url: string) => /^https?:\/\//i.test(url) || /^data:image\//i.test(url);
const normalizeCourseSlugs = (slugs: string[]) =>
  Array.from(new Set(slugs.map((slug) => slug.trim()).filter(Boolean)));

const formatCourseDate = (value?: string | null) => {
  if (!value) return '—';
  const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(value);
  if (match) {
    const [, y, m, d] = match;
    const dt = new Date(Date.UTC(Number(y), Number(m) - 1, Number(d), 12));
    return dt.toLocaleDateString('es-EC', {
      day: '2-digit',
      month: 'short',
      year: 'numeric',
      timeZone: 'UTC',
    });
  }
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return value;
  return parsed.toLocaleDateString('es-EC', { day: '2-digit', month: 'short', year: 'numeric' });
};

const getSessionDates = (sessions?: CourseMetadata['sessions']) => {
  if (!sessions?.length) return [];
  return sessions
    .map((s) => s.date)
    .filter((date): date is string => Boolean(date))
    .sort((a, b) => a.localeCompare(b));
};

const buildStartDateLabel = (sessions?: CourseMetadata['sessions']) => {
  const dates = getSessionDates(sessions);
  if (!dates.length) return null;
  const label = formatCourseDate(dates[0]);
  return label === '—' ? null : label;
};

const buildDateRangeLabel = (sessions?: CourseMetadata['sessions']) => {
  const dates = getSessionDates(sessions);
  if (!dates.length) return 'Fechas por confirmar';
  const startLabel = formatCourseDate(dates[0]);
  if (startLabel === '—') return 'Fechas por confirmar';
  const endLabel = formatCourseDate(dates[dates.length - 1]);
  if (endLabel === '—' || endLabel === startLabel) return `Inicio ${startLabel}`;
  return `${startLabel} / ${endLabel}`;
};

const MONTH_LABELS: Record<string, string> = {
  ene: 'Ene',
  feb: 'Feb',
  mar: 'Mar',
  abr: 'Abr',
  may: 'May',
  jun: 'Jun',
  jul: 'Jul',
  ago: 'Ago',
  sep: 'Sep',
  oct: 'Oct',
  nov: 'Nov',
  dic: 'Dic',
};

const buildFallbackCohortLabel = (slug: string) => {
  const match = /-([a-z]{3})-(\d{4})$/.exec(slug);
  if (!match) return slug.replace(/-/g, ' ');
  const [, month, year] = match;
  if (!month || !year) return slug.replace(/-/g, ' ');
  const monthLabel = MONTH_LABELS[month] ?? month;
  return `Inicio ${monthLabel} ${year}`;
};

const buildCohortLabel = (meta: CourseMetadata | undefined, slug: string) => {
  const startLabel = buildStartDateLabel(meta?.sessions);
  if (startLabel) return `Inicio ${startLabel}`;
  return buildFallbackCohortLabel(slug);
};

const PUBLIC_ESTEBAN_IMAGE_URL = `${PUBLIC_BASE}/assets/tdf-ui/esteban-munoz.jpg`;
const DEFAULT_INSTRUCTOR_IMAGE_URL = (() => {
  const envUrl = COURSE_DEFAULTS.instructorAvatarUrl;
  if (envUrl && isAbsoluteUrl(envUrl)) return envUrl;
  if (envUrl?.trim()) return `${PUBLIC_BASE}/${envUrl.trim().replace(/^\/+/, '')}`;
  return PUBLIC_ESTEBAN_IMAGE_URL;
})();
const INSTRUCTOR_IMAGE_FALLBACK = PUBLIC_ESTEBAN_IMAGE_URL;

const resolvePublicImageUrl = (
  url: string | null | undefined,
  fallback = DEFAULT_INSTRUCTOR_IMAGE_URL,
): string => {
  const trimmed = url?.trim();
  if (!trimmed) return fallback;
  if (isAbsoluteUrl(trimmed)) return trimmed;
  return `${PUBLIC_BASE}/${trimmed.replace(/^\/+/, '')}`;
};

const isProductionCourseSlug = (slug?: string) =>
  !slug || slug === 'produccion-musical' || slug.startsWith('produccion-musical-');

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

interface CourseCmsPayload {
  hero?: {
    title?: string;
    subtitle?: string;
    cta?: string;
    whatsappCta?: string;
    badge1?: string;
    badge2?: string;
    badge3?: string;
  };
}

export default function CourseProductionLandingPage() {
  const formRef = useRef<HTMLDivElement | null>(null);
  const location = useLocation();
  const navigate = useNavigate();
  const { slug: routeSlug } = useParams<{ slug: string }>();
  const [fullName, setFullName] = useState('');
  const [email, setEmail] = useState('');
  const [phone, setPhone] = useState('');
  const [howHeard, setHowHeard] = useState('');
  const [showSuccessDialog, setShowSuccessDialog] = useState(false);
  const productionSlugs = useMemo(() => {
    const cleaned = normalizeCourseSlugs(COURSE_COHORTS);
    return cleaned.length ? cleaned : [COURSE_DEFAULTS.slug];
  }, []);
  const pathSlug = useMemo(() => {
    const trimmed = routeSlug?.trim();
    return trimmed || undefined;
  }, [routeSlug]);
  const availableSlugs = useMemo(() => {
    if (!pathSlug || pathSlug === 'produccion-musical') return productionSlugs;
    if (isProductionCourseSlug(pathSlug) || productionSlugs.includes(pathSlug)) {
      return normalizeCourseSlugs([pathSlug, ...productionSlugs]);
    }
    return [pathSlug];
  }, [pathSlug, productionSlugs]);
  const defaultSelectedSlug = useMemo(() => {
    if (pathSlug && pathSlug !== 'produccion-musical') return pathSlug;
    return productionSlugs[0] ?? COURSE_DEFAULTS.slug;
  }, [pathSlug, productionSlugs]);
  const [selectedSlug, setSelectedSlug] = useState(defaultSelectedSlug);
  useEffect(() => {
    setSelectedSlug(defaultSelectedSlug);
  }, [defaultSelectedSlug]);
  const handleSelectedSlugChange = (nextSlug: string) => {
    setSelectedSlug(nextSlug);
    const nextPath = `/curso/${encodeURIComponent(nextSlug)}`;
    if (location.pathname !== nextPath) {
      navigate(`${nextPath}${location.search}`, { replace: false });
    }
  };

  const metaQuery = useQuery({
    queryKey: ['course-meta', selectedSlug],
    queryFn: () => Courses.getMetadata(selectedSlug),
    enabled: Boolean(selectedSlug),
  });
  const cohortQueries = useQueries({
    queries:
      availableSlugs.length > 1
        ? availableSlugs.map((slug) => ({
            queryKey: ['course-meta', slug],
            queryFn: () => Courses.getMetadata(slug),
            enabled: Boolean(slug),
          }))
        : [],
  });
  const cmsSlug = useMemo(
    () => (isProductionCourseSlug(pathSlug) ? 'course-production' : `course-${selectedSlug}`),
    [pathSlug, selectedSlug],
  );
  const cmsQuery = useCmsContent(cmsSlug, 'es');
  const cmsPayload = useMemo<CourseCmsPayload | null>(() => {
    const payload = cmsQuery.data?.ccdPayload;
    if (payload && typeof payload === 'object') {
      const hero = (payload as { hero?: unknown }).hero;
      if (hero && typeof hero === 'object') {
        return { hero: hero as CourseCmsPayload['hero'] };
      }
    }
    return null;
  }, [cmsQuery.data]);

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
    mutationFn: (payload: CourseRegistrationRequest) => Courses.register(selectedSlug, payload),
  });
  useEffect(() => {
    registrationMutation.reset();
  }, [registrationMutation, selectedSlug]);

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
  const remaining = meta?.remaining ?? undefined;
  const isFull = remaining !== undefined && remaining <= 0;
  const whatsappHref = meta?.whatsappCtaUrl ?? COURSE_DEFAULTS.whatsappUrl;
  const seatsLabel = isFull ? 'Cupos agotados' : 'Cupos limitados';
  const cohortOptions = availableSlugs.map((slug, idx) => {
    const cohortMeta = availableSlugs.length > 1 ? cohortQueries[idx]?.data : undefined;
    return {
      slug,
      label: buildCohortLabel(cohortMeta, slug),
    };
  });
  const startDateLabel = buildStartDateLabel(meta?.sessions);
  const dateRangeLabel = buildDateRangeLabel(meta?.sessions);
  const brandLabel = meta?.title ?? 'Cursos TDF';
  const brandTagline = startDateLabel ? `${brandLabel} · ${startDateLabel}` : brandLabel;
  const heroImageUrl = resolvePublicImageUrl(meta?.instructorAvatarUrl);

  const submitted = registrationMutation.isSuccess;
  const submitting = registrationMutation.isPending;
  const submitError = registrationMutation.error instanceof Error ? registrationMutation.error.message : null;
  useEffect(() => {
    if (submitted) setShowSuccessDialog(true);
  }, [submitted]);

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
        <EnrollmentSuccessDialog open={showSuccessDialog} onClose={() => setShowSuccessDialog(false)} />
        <Stack spacing={4}>
          {metaQuery.error && (
            <Alert severity="error">
              No pudimos cargar la información del curso. Intenta de nuevo o escríbenos por WhatsApp.
            </Alert>
          )}
          <Box sx={{ display: 'flex', justifyContent: 'center' }}>
            <PublicBrandBar tagline={brandTagline} />
          </Box>
          <Hero
            meta={meta}
            onPrimaryClick={scrollToForm}
            whatsappHref={whatsappHref}
            imageUrl={heroImageUrl}
            loading={metaQuery.isLoading}
            heroOverride={cmsPayload?.hero}
            seatsLabel={seatsLabel}
            isFull={isFull}
            dateRangeLabel={dateRangeLabel}
          />
          <Grid container spacing={3}>
            <Grid item xs={12} md={7}>
              <Info meta={meta} loading={metaQuery.isLoading} />
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
                isFull={isFull}
                whatsappHref={whatsappHref}
                cohortOptions={cohortOptions}
                selectedSlug={selectedSlug}
                onSlugChange={handleSelectedSlugChange}
              />
              <InstructorCard meta={meta} />
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

function InstructorCard({ meta }: { meta?: CourseMetadata }) {
  const handleImageError = (e: SyntheticEvent<HTMLImageElement>) => {
    const target = e.currentTarget;
    if (target.src !== INSTRUCTOR_IMAGE_FALLBACK) {
      target.src = INSTRUCTOR_IMAGE_FALLBACK;
    }
  };

  const name = meta?.instructorName ?? 'Instructor TDF';
  const bio =
    meta?.instructorBio ??
    'Instructor de TDF Records. Te acompañará con sesiones prácticas, seguimiento claro y ejercicios aplicables desde la primera clase.';
  const avatar = resolvePublicImageUrl(meta?.instructorAvatarUrl);

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
        image={avatar}
        alt={name}
        onError={handleImageError}
        sx={{ height: 220, objectFit: 'cover' }}
      />
      <CardContent sx={{ pb: 3 }}>
        <Stack direction="row" spacing={2} alignItems="center" mb={1}>
          <Avatar
            alt={name}
            src={avatar}
            imgProps={{ onError: handleImageError }}
          />
          <Box>
            <Typography variant="subtitle1" sx={{ color: '#f8fafc', fontWeight: 700 }}>
              {name}
            </Typography>
            <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.7)' }}>
              Instructor principal
            </Typography>
          </Box>
        </Stack>
        <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.75)' }}>
          {bio}
        </Typography>
      </CardContent>
    </Card>
  );
}

interface HeroOverrides {
  title?: string;
  subtitle?: string;
  cta?: string;
  whatsappCta?: string;
  badge1?: string;
  badge2?: string;
  badge3?: string;
}

function Hero({
  meta,
  loading,
  onPrimaryClick,
  whatsappHref,
  imageUrl,
  heroOverride,
  seatsLabel,
  isFull,
  dateRangeLabel,
}: {
  meta?: CourseMetadata;
  loading: boolean;
  onPrimaryClick: () => void;
  whatsappHref: string;
  imageUrl: string;
  heroOverride?: HeroOverrides;
  seatsLabel?: string;
  isFull: boolean;
  dateRangeLabel?: string;
}) {
  const title = loading ? 'Cargando curso...' : heroOverride?.title ?? meta?.title ?? 'Curso TDF Records';
  const subtitle =
    loading
      ? 'Preparando detalles...'
      : heroOverride?.subtitle ??
        meta?.subtitle ??
        'Programa presencial de TDF Records con cupos limitados, práctica guiada y seguimiento del instructor.';
  const primaryCta = heroOverride?.cta ?? 'Inscribirme';
  const whatsappCta = heroOverride?.whatsappCta ?? 'Inscribirme por WhatsApp';
  const badgeDate = heroOverride?.badge3 ?? dateRangeLabel ?? 'Fechas por confirmar';
  return (
    <Box
      sx={{
        borderRadius: { xs: 0, md: 2 },
        mx: { xs: -2, sm: 0 },
        minHeight: { xs: 560, md: 520 },
        p: { xs: 3, sm: 4, md: 5 },
        display: 'flex',
        alignItems: 'flex-end',
        backgroundImage: `linear-gradient(90deg, rgba(8,12,24,0.96) 0%, rgba(8,12,24,0.86) 48%, rgba(8,12,24,0.42) 100%), url(${imageUrl})`,
        backgroundSize: 'cover',
        backgroundPosition: { xs: 'center top', md: 'center right' },
        border: '1px solid rgba(255,255,255,0.08)',
        boxShadow: '0 20px 60px rgba(0,0,0,0.25)',
      }}
    >
      <Stack spacing={2} sx={{ maxWidth: 820 }}>
        <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
          <Chip icon={<VerifiedIcon />} label={heroOverride?.badge1 ?? 'Plazas limitadas'} color="default" sx={{ bgcolor: 'rgba(255,255,255,0.12)', color: '#e2e8f0' }} />
          <Chip icon={<HeadsetIcon />} label={heroOverride?.badge2 ?? 'Mentorías incluidas'} sx={{ bgcolor: 'rgba(255,255,255,0.12)', color: '#e2e8f0' }} />
          <Chip icon={<CalendarTodayIcon />} label={badgeDate} sx={{ bgcolor: 'rgba(255,255,255,0.12)', color: '#e2e8f0' }} />
        </Stack>
        <Typography variant="h3" fontWeight={700} sx={{ color: '#f8fafc' }}>
          {title}
        </Typography>
        <Typography variant="h6" sx={{ color: 'rgba(226,232,240,0.85)', maxWidth: 820 }}>
          {subtitle}
        </Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} alignItems={{ xs: 'stretch', sm: 'center' }}>
          <Typography variant="h4" fontWeight={800} sx={{ color: '#cbd5f5' }}>
            {loading ? '—' : `$${meta?.price?.toFixed(0) ?? '150'} ${meta?.currency ?? 'USD'}`}
          </Typography>
          <Stack spacing={0.5}>
            <Typography variant="body1" sx={{ color: 'rgba(226,232,240,0.75)' }}>
              {loading ? '—' : `${meta?.format ?? 'Presencial'} · ${meta?.duration ?? '16 horas'}`}
            </Typography>
            {seatsLabel && (
              <Typography
                variant="body2"
                sx={{ color: isFull ? '#fcd34d' : '#93c5fd', fontWeight: 700, letterSpacing: 0.2 }}
              >
                {isFull ? 'Cupos agotados' : seatsLabel}
              </Typography>
            )}
          </Stack>
        </Stack>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
          <Button
            variant="contained"
            size="large"
            onClick={onPrimaryClick}
            disabled={isFull}
            sx={{
              bgcolor: '#7c3aed',
              color: '#f8fafc',
              px: 3,
              boxShadow: '0 14px 30px rgba(124,58,237,0.35)',
            }}
          >
            {isFull ? 'Cupos agotados' : primaryCta}
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
            {whatsappCta}
          </Button>
        </Stack>
      </Stack>
    </Box>
  );
}

function Info({ meta, loading }: { meta?: CourseMetadata; loading: boolean }) {
  const sessions = meta?.sessions ?? [];
  const includesList =
    meta?.includes && meta.includes.length > 0
      ? meta.includes
      : ['Material de apoyo', 'Seguimiento del instructor', 'Certificado de participación', 'Grupo de WhatsApp'];
  const focusLabel = meta?.daws?.length ? `Enfoque: ${meta.daws.join(', ')}` : 'Programa práctico';
  const durationLabel = meta?.duration?.trim() || 'Duración por confirmar';
  const formatLabel = meta?.format?.trim() || 'Curso TDF';
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
            <Badge icon={<CelebrationIcon />} label={formatLabel} />
            <Badge icon={<HeadsetIcon />} label={durationLabel} />
            <Badge icon={<MusicNoteIcon />} label={focusLabel} />
            <Badge icon={<CheckCircleIcon />} label="Incluye seguimiento y certificado" />
          </Stack>
          <Divider sx={{ my: 2, borderColor: 'rgba(255,255,255,0.1)' }} />
          <Typography variant="subtitle1" gutterBottom sx={{ color: '#cbd5f5', fontWeight: 700 }}>
            Fechas
          </Typography>
          {loading && <Typography>Cargando fechas...</Typography>}
          {!loading && sessions.length === 0 && (
            <Typography>Fechas por confirmar.</Typography>
          )}
          {!loading && sessions.length > 0 && (
            <Stack spacing={1.2}>
              {sessions.map((session) => (
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
                    {formatCourseDate(session.date)}
                  </Typography>
                </Stack>
              ))}
            </Stack>
          )}
          <Divider sx={{ my: 2, borderColor: 'rgba(255,255,255,0.1)' }} />
          <Typography variant="subtitle1" gutterBottom sx={{ color: '#cbd5f5', fontWeight: 700 }}>
            Pensum
          </Typography>
          {loading && <Typography>Cargando pensum...</Typography>}
          {!loading && (
            <Stack spacing={1.5}>
              {!meta?.syllabus?.length && <Typography>Pensum por confirmar.</Typography>}
              {meta?.syllabus?.map((item) => {
                const topics = item.topics ?? [];
                return (
                <Box key={item.title} sx={{ p: 1.5, borderRadius: 2, bgcolor: 'rgba(255,255,255,0.02)' }}>
                  <Typography variant="subtitle2" sx={{ color: '#e2e8f0', fontWeight: 700 }}>
                    {item.title}
                  </Typography>
                  <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.8)', mt: 0.5 }}>
                    {topics.length ? topics.join(' · ') : 'Temas por confirmar'}
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
            {includesList.map((item) => (
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
  isFull,
  whatsappHref,
  cohortOptions,
  selectedSlug,
  onSlugChange,
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
  isFull: boolean;
  whatsappHref: string;
  cohortOptions: { slug: string; label: string }[];
  selectedSlug: string;
  onSlugChange: (slug: string) => void;
}) {
  const disableInputs = submitted || isFull || submitting;
  const disableCohortSelect = submitted || submitting;
  const seatsText = isFull ? 'Cupos agotados. Escríbenos y te avisamos si se libera un cupo.' : 'Cupos limitados.';
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
            Déjanos tus datos y te enviaremos los pasos para completar el pago. Cupos limitados.
          </Typography>
          {seatsText && (
            <Alert
              severity={isFull ? 'warning' : 'info'}
              action={
                <Button
                  size="small"
                  startIcon={<WhatsAppIcon />}
                  href={whatsappHref}
                  target="_blank"
                  rel="noreferrer"
                  variant="outlined"
                  color={isFull ? 'warning' : 'info'}
                >
                  {isFull ? 'Avísame' : 'Escríbenos'}
                </Button>
              }
            >
              {isFull ? 'Cupos agotados. Escríbenos y te avisamos si se libera un cupo.' : seatsText}
            </Alert>
          )}
          <Box component="form" onSubmit={onSubmit}>
            <Stack spacing={1.5}>
              {cohortOptions.length > 1 && (
                <TextField
                  select
                  label="Fecha de inicio"
                  value={selectedSlug}
                  onChange={(e) => onSlugChange(e.target.value)}
                  disabled={disableCohortSelect}
                  helperText="Elige la fecha en la que quieres iniciar."
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
                  SelectProps={{
                    MenuProps: {
                      PaperProps: {
                        sx: {
                          bgcolor: '#0b1224',
                          color: '#e2e8f0',
                          border: '1px solid rgba(255,255,255,0.08)',
                        },
                      },
                    },
                  }}
                >
                  {cohortOptions.map((option) => (
                    <MenuItem key={option.slug} value={option.slug}>
                      {option.label}
                    </MenuItem>
                  ))}
                </TextField>
              )}
              <TextField
                label="Nombre completo"
                required
                value={fullName}
                onChange={(e) => onFullNameChange(e.target.value)}
                disabled={disableInputs}
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
                disabled={disableInputs}
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
                disabled={disableInputs}
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
                disabled={disableInputs}
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
                disabled={disableInputs || submitting}
                startIcon={submitting ? <CircularProgress size={18} color="inherit" /> : <CelebrationIcon />}
                sx={{ mt: 1 }}
          >
            {isFull ? 'Cupos agotados' : submitted ? 'Inscripción recibida' : 'Enviar inscripción'}
          </Button>
        </Stack>
      </Box>
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
