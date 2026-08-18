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
  Checkbox,
  Chip,
  CircularProgress,
  Container,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  FormControlLabel,
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
import type { CourseCheckoutResponse, CourseMetadata, CourseRegistrationRequest } from '../api/courses';
import { Courses } from '../api/courses';
import type { DatafastCheckoutDTO } from '../api/types';
import EnrollmentSuccessDialog from '../components/EnrollmentSuccessDialog';
import PublicBrandBar from '../components/PublicBrandBar';
import { useCmsContent } from '../hooks/useCmsContent';
import { COURSE_COHORTS, COURSE_DEFAULTS, PUBLIC_BASE } from '../config/appConfig';
import { useLocation, useNavigate, useParams } from 'react-router-dom';
import {
  formatCurrencyForUser,
  formatDateForUser,
  resolveRuntimeCurrency,
  resolveRuntimeFormatOptions,
} from '../utils/formatters';

const isAbsoluteUrl = (url: string) => /^https?:\/\//i.test(url) || /^data:image\//i.test(url);
const normalizeCourseSlugs = (slugs: string[]) =>
  Array.from(new Set(slugs.map((slug) => slug.trim()).filter(Boolean)));
const trimToUndefined = (value?: string | null) => {
  const trimmed = value?.trim();
  return trimmed === undefined || trimmed === '' ? undefined : trimmed;
};

const formatCourseDate = (value?: string | null) => {
  if (!value) return '—';
  const match = /^(\d{4})-(\d{2})-(\d{2})$/.exec(value);
  if (match) {
    const [, y, m, d] = match;
    const dt = new Date(Date.UTC(Number(y), Number(m) - 1, Number(d), 12));
    return new Intl.DateTimeFormat(resolveRuntimeFormatOptions().locale, {
      day: '2-digit',
      month: 'short',
      year: 'numeric',
      timeZone: 'UTC',
    }).format(dt);
  }
  return formatDateForUser(value, { day: '2-digit', month: 'short', year: 'numeric' });
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

const createCourseIdempotencyKey = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `course-checkout-${crypto.randomUUID()}`;
  }
  return `course-checkout-${Date.now()}-${Math.random().toString(16).slice(2)}`;
};

const courseLookupStorageKey = (slug: string, registrationId: number) =>
  `tdf:course-checkout:${slug}:${registrationId}`;

const saveCourseLookupToken = (slug: string, registrationId: number, token: string) => {
  try {
    window.localStorage.setItem(courseLookupStorageKey(slug, registrationId), token);
  } catch {
    // Private browsing or storage policy may deny persistence; the live response still works.
  }
};

const loadCourseLookupToken = (slug: string, registrationId: number): string | null => {
  try {
    return window.localStorage.getItem(courseLookupStorageKey(slug, registrationId));
  } catch {
    return null;
  }
};

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
  const { slug: routeSlug, registrationId: routeRegistrationId } = useParams<{
    slug: string;
    registrationId: string;
  }>();
  const [fullName, setFullName] = useState('');
  const [email, setEmail] = useState('');
  const [phone, setPhone] = useState('');
  const [howHeard, setHowHeard] = useState('');
  const [termsAccepted, setTermsAccepted] = useState(false);
  const [showSuccessDialog, setShowSuccessDialog] = useState(false);
  const [checkout, setCheckout] = useState<CourseCheckoutResponse | null>(null);
  const [paymentBusy, setPaymentBusy] = useState(false);
  const [paymentError, setPaymentError] = useState<string | null>(null);
  const [datafastCheckout, setDatafastCheckout] = useState<DatafastCheckoutDTO | null>(null);
  const [datafastDialogOpen, setDatafastDialogOpen] = useState(false);
  const [datafastWidgetKey, setDatafastWidgetKey] = useState(0);
  const datafastFormRef = useRef<HTMLDivElement | null>(null);
  const [paypalReady, setPaypalReady] = useState(false);
  const [paypalDialogOpen, setPaypalDialogOpen] = useState(false);
  const [paypalOrderId, setPaypalOrderId] = useState<string | null>(null);
  const paypalButtonRef = useRef<HTMLDivElement | null>(null);
  const paypalClientId = import.meta.env?.VITE_PAYPAL_CLIENT_ID?.trim() ?? '';
  const checkoutIdempotency = useRef<{ fingerprint: string; key: string } | null>(null);
  const productionSlugs = useMemo(() => {
    const cleaned = normalizeCourseSlugs(COURSE_COHORTS);
    return cleaned.length ? cleaned : [COURSE_DEFAULTS.slug];
  }, []);
  const pathSlug = useMemo(() => {
    return trimToUndefined(routeSlug);
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
    const params = new URLSearchParams(location.search);
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
  }, [location.search]);

  const registrationMutation = useMutation({
    mutationFn: (payload: CourseRegistrationRequest) => {
      const fingerprint = JSON.stringify({ selectedSlug, payload });
      if (checkoutIdempotency.current?.fingerprint !== fingerprint) {
        checkoutIdempotency.current = { fingerprint, key: createCourseIdempotencyKey() };
      }
      return Courses.register(selectedSlug, payload, checkoutIdempotency.current.key);
    },
    onSuccess: (response) => {
      setCheckout(response);
      const token = response.lookupToken?.trim();
      if (token) saveCourseLookupToken(selectedSlug, response.registrationId, token);
      if (response.checkoutAvailable) {
        navigate(`/curso/${encodeURIComponent(selectedSlug)}/orden/${response.registrationId}`, {
          replace: true,
        });
      }
    },
  });
  const previousSelectedSlugRef = useRef(selectedSlug);
  useEffect(() => {
    if (previousSelectedSlugRef.current === selectedSlug) return;
    previousSelectedSlugRef.current = selectedSlug;
    registrationMutation.reset();
    setShowSuccessDialog(false);
    setCheckout(null);
    setPaymentError(null);
    setTermsAccepted(false);
    checkoutIdempotency.current = null;
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
      termsAccepted,
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

  const checkoutLookupToken = useMemo(() => {
    if (!checkout) return null;
    return checkout.lookupToken
      ?? loadCourseLookupToken(checkout.courseSlug, checkout.registrationId);
  }, [checkout]);

  useEffect(() => {
    const registrationId = Number(routeRegistrationId);
    if (!Number.isSafeInteger(registrationId) || registrationId <= 0 || !pathSlug) return;
    const token = loadCourseLookupToken(pathSlug, registrationId);
    if (!token) {
      setPaymentError('No encontramos el acceso seguro de esta orden en este navegador.');
      return;
    }
    const params = new URLSearchParams(location.search);
    const resourcePath = params.get('resourcePath') ?? params.get('id');
    setPaymentBusy(true);
    setPaymentError(null);
    const request = resourcePath
      ? Courses.confirmDatafastStatus(pathSlug, registrationId, resourcePath, token)
      : Courses.getCheckout(pathSlug, registrationId, token);
    request
      .then((response) => {
        setCheckout(response);
        if (resourcePath) {
          navigate(location.pathname, { replace: true });
        }
      })
      .catch(() => {
        setPaymentError(
          resourcePath
            ? 'No pudimos verificar la respuesta de Datafast. El pago no se muestra como confirmado.'
            : 'No pudimos consultar esta orden de curso.',
        );
      })
      .finally(() => setPaymentBusy(false));
  }, [location.pathname, location.search, navigate, pathSlug, routeRegistrationId]);

  const datafastReturnUrl = useMemo(() => {
    if (!checkout || typeof window === 'undefined') return '';
    return new URL(
      `/curso/${encodeURIComponent(checkout.courseSlug)}/orden/${checkout.registrationId}`,
      window.location.origin,
    ).toString();
  }, [checkout]);

  const handleDatafastPayment = async () => {
    if (!checkout || !checkoutLookupToken) {
      setPaymentError('No encontramos el acceso seguro de esta orden.');
      return;
    }
    setPaymentBusy(true);
    setPaymentError(null);
    try {
      const providerCheckout = await Courses.createDatafastCheckout(
        checkout.courseSlug,
        checkout.registrationId,
        checkoutLookupToken,
      );
      setDatafastCheckout(providerCheckout);
      setDatafastDialogOpen(true);
      setDatafastWidgetKey((current) => current + 1);
    } catch {
      setPaymentError('No pudimos iniciar Datafast. La inscripción sigue sin pago confirmado.');
    } finally {
      setPaymentBusy(false);
    }
  };

  const handlePaypalPayment = async () => {
    if (!checkout || !checkoutLookupToken) {
      setPaymentError('No encontramos el acceso seguro de esta orden.');
      return;
    }
    if (!paypalClientId) {
      setPaymentError('PayPal no está configurado en este navegador.');
      return;
    }
    setPaymentBusy(true);
    setPaymentError(null);
    try {
      const providerOrder = await Courses.createPaypalOrder(
        checkout.courseSlug,
        checkout.registrationId,
        checkoutLookupToken,
      );
      setPaypalOrderId(providerOrder.pcPaypalOrderId);
      setPaypalDialogOpen(true);
    } catch {
      setPaymentError('No pudimos iniciar PayPal. La inscripción sigue sin pago confirmado.');
    } finally {
      setPaymentBusy(false);
    }
  };

  useEffect(() => {
    if (!datafastDialogOpen || !datafastCheckout || typeof window === 'undefined') return;
    if (datafastFormRef.current) datafastFormRef.current.innerHTML = '';
    window.wpwlOptions = { locale: 'es', style: 'card' };
    const script = document.createElement('script');
    script.src = datafastCheckout.dcWidgetUrl;
    script.async = true;
    script.onerror = () => setPaymentError(
      'No se pudo cargar Datafast. No se confirmó ningún pago.',
    );
    document.body.appendChild(script);
    return () => script.remove();
  }, [datafastCheckout, datafastDialogOpen, datafastWidgetKey]);

  useEffect(() => {
    const paypalOffered = checkout?.paymentMethods.includes('paypal') ?? false;
    if (!paypalOffered || !paypalClientId || typeof window === 'undefined') return;
    if (window.paypal) {
      setPaypalReady(true);
      return;
    }
    const script = document.createElement('script');
    script.src = `https://www.paypal.com/sdk/js?client-id=${encodeURIComponent(paypalClientId)}&currency=${encodeURIComponent(checkout?.quote?.currency ?? 'USD')}`;
    script.async = true;
    script.onload = () => setPaypalReady(true);
    script.onerror = () => setPaymentError(
      'No se pudo cargar PayPal. La inscripción continúa sin pago.',
    );
    document.body.appendChild(script);
    return () => script.remove();
  }, [checkout?.paymentMethods, checkout?.quote?.currency, paypalClientId]);

  useEffect(() => {
    if (
      !paypalDialogOpen
      || !paypalReady
      || !paypalOrderId
      || !checkout
      || !checkoutLookupToken
      || !paypalButtonRef.current
      || typeof window === 'undefined'
      || !window.paypal
    ) return;
    paypalButtonRef.current.innerHTML = '';
    const buttons = window.paypal.Buttons({
      createOrder: () => paypalOrderId,
      onApprove: async (data) => {
        if (data.orderID !== paypalOrderId) {
          setPaymentError('PayPal devolvió una referencia distinta. No se capturó el pago.');
          return;
        }
        setPaymentBusy(true);
        try {
          const response = await Courses.capturePaypalOrder(
            checkout.courseSlug,
            checkout.registrationId,
            paypalOrderId,
            checkoutLookupToken,
          );
          setCheckout(response);
          setPaypalDialogOpen(false);
          setPaypalOrderId(null);
          setPaymentError(
            response.paymentStatus === 'paid'
              ? null
              : 'PayPal respondió, pero el servidor todavía no confirmó el pago.',
          );
        } catch {
          setPaymentError('No pudimos verificar PayPal. No mostramos la inscripción como pagada.');
        } finally {
          setPaymentBusy(false);
        }
      },
      onCancel: () => setPaymentError('Cancelaste PayPal. La inscripción continúa sin pago.'),
      onError: () => setPaymentError('PayPal no completó la operación. No se confirmó ningún pago.'),
    });
    void buttons.render(paypalButtonRef.current);
    return () => buttons.close?.();
  }, [checkout, checkoutLookupToken, paypalDialogOpen, paypalOrderId, paypalReady]);

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
        <EnrollmentSuccessDialog
          open={showSuccessDialog}
          onClose={() => setShowSuccessDialog(false)}
          title={checkout?.checkoutAvailable ? 'Cupo retenido temporalmente' : 'Solicitud recibida'}
          message={checkout?.checkoutAvailable
            ? 'Creamos una retención temporal. Tu inscripción sigue pendiente hasta que el servidor verifique el pago.'
            : 'Recibimos tus datos, pero el checkout no está habilitado y todavía no se reservó ni pagó un cupo.'}
        />
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
                termsAccepted={termsAccepted}
                onTermsAcceptedChange={setTermsAccepted}
                submitting={submitting}
                submitted={submitted}
                submitError={submitError}
                isFull={isFull}
                whatsappHref={whatsappHref}
                cohortOptions={cohortOptions}
                selectedSlug={selectedSlug}
                onSlugChange={handleSelectedSlugChange}
              />
              {checkout && (
                <CourseCheckoutCard
                  checkout={checkout}
                  paymentBusy={paymentBusy}
                  paymentError={paymentError}
                  paypalAvailable={Boolean(paypalClientId && paypalReady)}
                  onDatafast={() => void handleDatafastPayment()}
                  onPaypal={() => void handlePaypalPayment()}
                />
              )}
              <InstructorCard meta={meta} />
              {meta?.locationLabel && meta?.locationMapUrl && (
                <LocationCard label={meta.locationLabel} mapUrl={meta.locationMapUrl} />
              )}
            </Grid>
          </Grid>
        </Stack>
        <Dialog
          open={datafastDialogOpen}
          onClose={() => setDatafastDialogOpen(false)}
          maxWidth="xs"
          fullWidth
        >
          <DialogTitle>Pagar curso con Datafast</DialogTitle>
          <DialogContent dividers>
            <Stack spacing={1.5}>
              <Alert severity="info" variant="outlined">
                El formulario es alojado por Datafast. Al volver, TDF verificará importe, moneda, comercio y referencia en el servidor antes de confirmar el pago.
              </Alert>
              {paymentError && <Alert severity="warning">{paymentError}</Alert>}
              {datafastCheckout && datafastReturnUrl && (
                <Box ref={datafastFormRef} key={datafastWidgetKey} sx={{ minHeight: 360 }}>
                  <form
                    action={datafastReturnUrl}
                    className="paymentWidgets"
                    data-brands="VISA MASTER DINERS AMEX DISCOVER"
                  />
                </Box>
              )}
            </Stack>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => setDatafastWidgetKey((current) => current + 1)}>
              Reintentar carga
            </Button>
            <Button onClick={() => setDatafastDialogOpen(false)} color="inherit">Cerrar</Button>
          </DialogActions>
        </Dialog>
        <Dialog
          open={paypalDialogOpen}
          onClose={() => setPaypalDialogOpen(false)}
          maxWidth="xs"
          fullWidth
        >
          <DialogTitle>Pagar curso con PayPal</DialogTitle>
          <DialogContent dividers>
            <Stack spacing={1.5}>
              <Alert severity="info" variant="outlined">
                Aprobar en PayPal no basta: TDF captura y verifica la orden en el servidor antes de mostrar el pago como confirmado.
              </Alert>
              {paymentError && <Alert severity="warning">{paymentError}</Alert>}
              <Box ref={paypalButtonRef} sx={{ minHeight: 48 }} />
            </Stack>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => setPaypalDialogOpen(false)} color="inherit">Cerrar</Button>
          </DialogActions>
        </Dialog>
      </Container>
    </Box>
  );
}

function CourseCheckoutCard({
  checkout,
  paymentBusy,
  paymentError,
  paypalAvailable,
  onDatafast,
  onPaypal,
}: {
  checkout: CourseCheckoutResponse;
  paymentBusy: boolean;
  paymentError: string | null;
  paypalAvailable: boolean;
  onDatafast: () => void;
  onPaypal: () => void;
}) {
  const paid = checkout.paymentStatus === 'paid';
  const held = checkout.fulfillmentStatus === 'seat_held';
  const quote = checkout.quote;
  return (
    <Card
      sx={{
        mt: 3,
        background: 'rgba(15,23,42,0.94)',
        border: '1px solid rgba(147,197,253,0.28)',
        color: '#e2e8f0',
      }}
    >
      <CardContent>
        <Stack spacing={1.5}>
          <Typography variant="h6" fontWeight={800}>
            Estado de tu inscripción
          </Typography>
          {!checkout.checkoutAvailable && (
            <Alert severity="info" variant="outlined">
              Solicitud recibida. El checkout no está habilitado y no se reservó ni pagó un cupo.
            </Alert>
          )}
          {checkout.checkoutAvailable && paid && (
            <Alert severity="success" variant="outlined">
              Pago verificado por el servidor. Tu cupo está inscrito; esto no significa que el curso haya sido completado.
            </Alert>
          )}
          {checkout.checkoutAvailable && !paid && (
            <Alert severity={held ? 'warning' : 'info'} variant="outlined">
              {held
                ? 'Cupo retenido temporalmente. Todavía no está pagado ni inscrito.'
                : `Estado de cupo: ${checkout.fulfillmentStatus}. El pago no está confirmado.`}
            </Alert>
          )}
          {quote && (
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
              <Chip
                label={`Total: ${formatCurrencyForUser(quote.totalMinor / 100, quote.currency)}`}
                size="small"
              />
              <Chip
                label={`A pagar ahora: ${formatCurrencyForUser(quote.dueNowMinor / 100, quote.currency)}`}
                size="small"
              />
              {quote.balanceMinor > 0 && (
                <Chip
                  label={`Saldo posterior: ${formatCurrencyForUser(quote.balanceMinor / 100, quote.currency)}`}
                  size="small"
                />
              )}
            </Stack>
          )}
          {checkout.holdExpiresAt && !paid && (
            <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.78)' }}>
              La retención vence {formatDateForUser(checkout.holdExpiresAt, {
                dateStyle: 'medium',
                timeStyle: 'short',
              })}.
            </Typography>
          )}
          {paymentError && <Alert severity="warning">{paymentError}</Alert>}
          {checkout.checkoutAvailable && !paid && checkout.paymentMethods.length === 0 && (
            <Alert severity="info" variant="outlined">
              No hay un proveedor real habilitado para esta orden. La retención no equivale a pago.
            </Alert>
          )}
          {checkout.checkoutAvailable && !paid && (
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              {checkout.paymentMethods.includes('datafast') && (
                <Button variant="contained" disabled={paymentBusy} onClick={onDatafast}>
                  Pagar con Datafast
                </Button>
              )}
              {checkout.paymentMethods.includes('paypal') && paypalAvailable && (
                <Button variant="outlined" disabled={paymentBusy} onClick={onPaypal}>
                  Pagar con PayPal
                </Button>
              )}
            </Stack>
          )}
          <Typography variant="caption" sx={{ color: 'rgba(226,232,240,0.62)' }}>
            Orden de curso #{checkout.registrationId}. Pago y cumplimiento académico son estados separados.
          </Typography>
        </Stack>
      </CardContent>
    </Card>
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
            {loading
              ? '—'
              : formatCurrencyForUser(meta?.price ?? 150, meta?.currency ?? resolveRuntimeCurrency())}
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
  const durationLabel = trimToUndefined(meta?.duration) ?? 'Duración por confirmar';
  const formatLabel = trimToUndefined(meta?.format) ?? 'Curso TDF';
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
  termsAccepted,
  onTermsAcceptedChange,
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
  termsAccepted: boolean;
  onTermsAcceptedChange: (value: boolean) => void;
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
                type="tel"
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
              <FormControlLabel
                control={(
                  <Checkbox
                    checked={termsAccepted}
                    onChange={(event) => onTermsAcceptedChange(event.target.checked)}
                    required
                    disabled={disableInputs}
                    sx={{ color: 'rgba(226,232,240,0.72)' }}
                  />
                )}
                label="Acepto la versión de términos y política de cancelación que el servidor asociará a esta orden."
                sx={{
                  alignItems: 'flex-start',
                  color: 'rgba(226,232,240,0.78)',
                  '& .MuiFormControlLabel-label': { fontSize: '0.82rem', pt: 0.75 },
                }}
              />
              <Button
                type="submit"
                variant="contained"
                disabled={disableInputs || submitting || !termsAccepted}
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
