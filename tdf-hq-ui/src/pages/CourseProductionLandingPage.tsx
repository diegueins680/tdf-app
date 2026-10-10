import {
  forwardRef,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
  type FormEvent,
  type ReactElement,
  type Ref,
  type RefObject,
  type SyntheticEvent,
} from 'react';
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
  FormHelperText,
  Grid,
  IconButton,
  Link,
  MenuItem,
  Slide,
  Stack,
  TextField,
  Typography,
  useMediaQuery,
  useTheme,
} from '@mui/material';
import type { TransitionProps } from '@mui/material/transitions';
import CloseIcon from '@mui/icons-material/Close';
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
import HostedProviderCheckout from '../components/payments/HostedProviderCheckout';
import PublicBrandBar from '../components/PublicBrandBar';
import { useCmsContent } from '../hooks/useCmsContent';
import { COURSE_COHORTS, COURSE_DEFAULTS, PUBLIC_BASE } from '../config/appConfig';
import { Link as RouterLink, useLocation, useNavigate, useParams } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { buildLoginRedirectPath } from '../utils/loginRouting';
import { BOTTOM_DOCK_OFFSET } from '../utils/bottomDock';
import { normalizePhoneToE164, PHONE_EXAMPLE_HINT } from '../utils/phone';
import {
  describeCourseRegistrationError,
  type CourseRegistrationField,
} from '../utils/courseRegistrationErrors';
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

const ENROLL_QUERY_PARAM = 'inscribirme';
const EMAIL_PATTERN = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

interface EnrollmentDraft {
  fullName: string;
  email: string;
  phone: string;
  howHeard: string;
}

type EnrollmentFieldErrors = Partial<Record<CourseRegistrationField, string>>;

const enrollmentDraftStorageKey = (slug: string) => `tdf:course-enroll-draft:${slug}`;

// Only what the visitor typed into this form, kept for the login round-trip of this tab.
const saveEnrollmentDraft = (slug: string, draft: EnrollmentDraft) => {
  try {
    window.sessionStorage.setItem(enrollmentDraftStorageKey(slug), JSON.stringify(draft));
  } catch {
    // Storage can be unavailable (private mode, quota); the form still works without it.
  }
};

const readEnrollmentDraft = (slug: string): EnrollmentDraft | null => {
  try {
    const raw = window.sessionStorage.getItem(enrollmentDraftStorageKey(slug));
    if (!raw) return null;
    const parsed = JSON.parse(raw) as Partial<Record<keyof EnrollmentDraft, unknown>>;
    const field = (value: unknown) => (typeof value === 'string' ? value.slice(0, 500) : '');
    return {
      fullName: field(parsed.fullName),
      email: field(parsed.email),
      phone: field(parsed.phone),
      howHeard: field(parsed.howHeard),
    };
  } catch {
    return null;
  }
};

const clearEnrollmentDraft = (slug: string) => {
  try {
    window.sessionStorage.removeItem(enrollmentDraftStorageKey(slug));
  } catch {
    // Nothing to clean up when storage is unavailable.
  }
};

const looksLikeEmail = (value: string) => EMAIL_PATTERN.test(value.trim());

const resolveTermsUrl = (value: unknown): string | null => {
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  if (/^https:\/\//i.test(trimmed)) return trimmed;
  if (trimmed.startsWith('/') && !trimmed.startsWith('//')) return trimmed;
  return null;
};

const darkFieldInputSx = {
  color: '#f8fafc',
  '& .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.32)' },
  '&:hover .MuiOutlinedInput-notchedOutline': { borderColor: 'rgba(255,255,255,0.5)' },
  '&.Mui-focused .MuiOutlinedInput-notchedOutline': { borderColor: '#93c5fd' },
  '&.Mui-error .MuiOutlinedInput-notchedOutline': { borderColor: '#fca5a5' },
  '& input, & textarea': {
    color: '#f8fafc',
    caretColor: '#f8fafc',
    '::placeholder': { color: 'rgba(226,232,240,0.6)' },
  },
};

const darkFieldProps = {
  InputProps: { sx: darkFieldInputSx },
  InputLabelProps: { sx: { color: 'rgba(226,232,240,0.78)', '&.Mui-error': { color: '#fca5a5' } } },
  FormHelperTextProps: { sx: { color: 'rgba(226,232,240,0.72)', '&.Mui-error': { color: '#fca5a5' } } },
};

const SlideUpTransition = forwardRef(function SlideUpTransition(
  props: TransitionProps & { children: ReactElement },
  ref: Ref<unknown>,
) {
  return <Slide direction="up" ref={ref} {...props} />;
});

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
  termsUrl?: string | null;
}

interface RegistrationAttempt {
  fingerprint: string;
  /** A definitive rejection: a corrected payload may use a fresh key. */
  retireOnChange: boolean;
  /** The server holds different details under this key: always use a fresh key. */
  retireAlways: boolean;
}

export default function CourseProductionLandingPage() {
  const theme = useTheme();
  const isPhone = useMediaQuery(theme.breakpoints.down('sm'), { noSsr: true });
  const { session, loading: sessionLoading } = useSession();
  const heroCtaRef = useRef<HTMLButtonElement | null>(null);
  // Tracked in state too, so the sticky-CTA observer attaches whenever the
  // hero button actually mounts (it can render after the metadata query settles).
  const [heroCtaElement, setHeroCtaElement] = useState<HTMLButtonElement | null>(null);
  const setHeroCtaRef = useCallback((node: HTMLButtonElement | null) => {
    heroCtaRef.current = node;
    setHeroCtaElement(node);
  }, []);
  const checkoutCardRef = useRef<HTMLDivElement | null>(null);
  const fullNameInputRef = useRef<HTMLInputElement | null>(null);
  const emailInputRef = useRef<HTMLInputElement | null>(null);
  const phoneInputRef = useRef<HTMLInputElement | null>(null);
  const howHeardInputRef = useRef<HTMLTextAreaElement | null>(null);
  const termsInputRef = useRef<HTMLInputElement | null>(null);
  const submitButtonRef = useRef<HTMLButtonElement | null>(null);
  const location = useLocation();
  const navigate = useNavigate();
  const { slug: routeSlug, registrationId: routeRegistrationId } = useParams<{
    slug: string;
    registrationId: string;
  }>();
  // What the visitor typed before a login round-trip (see handleLoginForAutofill).
  const [initialDraft] = useState(() => {
    const slug = trimToUndefined(routeSlug);
    return slug ? readEnrollmentDraft(slug) : null;
  });
  const [fullName, setFullName] = useState(initialDraft?.fullName ?? '');
  const [email, setEmail] = useState(initialDraft?.email ?? '');
  const [phone, setPhone] = useState(initialDraft?.phone ?? '');
  const [howHeard, setHowHeard] = useState(initialDraft?.howHeard ?? '');
  const [termsAccepted, setTermsAccepted] = useState(false);
  const [fieldErrors, setFieldErrors] = useState<EnrollmentFieldErrors>({});
  const [accountFields, setAccountFields] = useState<('fullName' | 'email')[]>([]);
  const [editAccountFields, setEditAccountFields] = useState(false);
  const [enrollOpen, setEnrollOpen] = useState(false);
  const [showStickyCta, setShowStickyCta] = useState(false);
  const [pendingCheckoutFocus, setPendingCheckoutFocus] = useState(false);
  const [checkout, setCheckout] = useState<CourseCheckoutResponse | null>(null);
  const [paymentBusy, setPaymentBusy] = useState(false);
  const [hostedPaymentLocked, setHostedPaymentLocked] = useState(false);
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
  const checkoutIdempotency = useRef<string | null>(null);
  const lastAttemptRef = useRef<RegistrationAttempt | null>(null);
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
  // Return here after login with the enrollment step open, keeping the
  // visitor's campaign parameters (utm_*) so attribution survives the trip.
  const enrollmentResumePath = useMemo(() => {
    const params = new URLSearchParams(location.search);
    params.set(ENROLL_QUERY_PARAM, '1');
    return `/curso/${encodeURIComponent(selectedSlug)}?${params.toString()}`;
  }, [location.search, selectedSlug]);
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
      const termsUrl = resolveTermsUrl((payload as { termsUrl?: unknown }).termsUrl);
      return {
        ...(hero && typeof hero === 'object' ? { hero: hero as CourseCmsPayload['hero'] } : {}),
        termsUrl,
      };
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

  const fieldInputRefs = useMemo(
    () => ({
      fullName: fullNameInputRef,
      email: emailInputRef,
      phone: phoneInputRef,
      howHeard: howHeardInputRef,
      terms: termsInputRef,
    }),
    [],
  );
  // Inputs are disabled while a submission is pending, and focusing a disabled
  // input is a no-op. Queue the field and focus it from an effect once the
  // form has re-rendered enabled (and any collapsed account field is shown).
  const [pendingFocusField, setPendingFocusField] = useState<CourseRegistrationField | null>(null);
  const focusField = useCallback((field: CourseRegistrationField) => {
    setPendingFocusField(field);
  }, []);

  const registrationMutation = useMutation({
    mutationFn: ({ payload, idempotencyKey }: { payload: CourseRegistrationRequest; idempotencyKey: string }) =>
      Courses.register(selectedSlug, payload, idempotencyKey),
    onSuccess: (response) => {
      checkoutIdempotency.current = null;
      lastAttemptRef.current = null;
      clearEnrollmentDraft(selectedSlug);
      setCheckout(response);
      const token = response.lookupToken?.trim();
      if (token) saveCourseLookupToken(selectedSlug, response.registrationId, token);
      if (response.checkoutAvailable) {
        // The payment step lives on the order page; close the sheet and bring the order into view.
        enrollTriggerRef.current = null;
        setEnrollOpen(false);
        setPendingCheckoutFocus(true);
        navigate(`/curso/${encodeURIComponent(selectedSlug)}/orden/${response.registrationId}`, {
          replace: true,
        });
      }
    },
    onError: (error) => {
      const view = describeCourseRegistrationError(error);
      if (lastAttemptRef.current) {
        lastAttemptRef.current = {
          ...lastAttemptRef.current,
          retireOnChange: view.definitiveRejection,
          retireAlways: view.retireIdempotencyKey,
        };
      }
      if (view.field) {
        if (view.field === 'fullName' || view.field === 'email') setEditAccountFields(true);
        focusField(view.field);
      }
    },
  });
  const previousSelectedSlugRef = useRef(selectedSlug);
  useEffect(() => {
    if (previousSelectedSlugRef.current === selectedSlug) return;
    previousSelectedSlugRef.current = selectedSlug;
    registrationMutation.reset();
    setCheckout(null);
    setPaymentError(null);
    setTermsAccepted(false);
    setFieldErrors({});
    checkoutIdempotency.current = null;
    lastAttemptRef.current = null;
  }, [registrationMutation, selectedSlug]);

  useEffect(() => {
    const slug = trimToUndefined(routeSlug);
    if (initialDraft && slug) clearEnrollmentDraft(slug);
    // Mount-only: the draft is consumed once.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // Prefill from the signed-in account without blocking the form on any request.
  // Fields the visitor already typed (or restored from the draft) always win.
  // A cached session is only trusted after /session verifies it: on a shared
  // browser an expired cache must never put the previous person's identity
  // into the form, and values taken from an account that is no longer the
  // verified one are removed unless the visitor edited them.
  const verifiedSession = sessionLoading ? null : session;
  const sessionDisplayName = verifiedSession?.displayName.trim() ?? '';
  const sessionEmail = verifiedSession && looksLikeEmail(verifiedSession.username) ? verifiedSession.username.trim() : '';
  const prefillKeyRef = useRef<string | null>(null);
  const prefilledValuesRef = useRef<{ fullName?: string; email?: string }>({});
  useEffect(() => {
    if (sessionLoading) return;
    const key = `${sessionDisplayName}\n${sessionEmail}`;
    if (prefillKeyRef.current === key) return;
    prefillKeyRef.current = key;
    const previous = prefilledValuesRef.current;
    let nextFullName = previous.fullName !== undefined && fullName === previous.fullName ? '' : fullName;
    let nextEmail = previous.email !== undefined && email === previous.email ? '' : email;
    const current: { fullName?: string; email?: string } = {};
    const prefilled: ('fullName' | 'email')[] = [];
    if (sessionDisplayName && !nextFullName.trim()) {
      nextFullName = sessionDisplayName;
      current.fullName = sessionDisplayName;
      prefilled.push('fullName');
    }
    if (sessionEmail && !nextEmail.trim()) {
      nextEmail = sessionEmail;
      current.email = sessionEmail;
      prefilled.push('email');
    }
    prefilledValuesRef.current = current;
    if (nextFullName !== fullName) setFullName(nextFullName);
    if (nextEmail !== email) setEmail(nextEmail);
    setAccountFields(prefilled);
  }, [email, fullName, sessionDisplayName, sessionEmail, sessionLoading]);

  const clearFieldError = (field: CourseRegistrationField) => {
    setFieldErrors((current) => {
      if (!(field in current)) return current;
      const next = { ...current };
      delete next[field];
      return next;
    });
    if (registrationMutation.isError) registrationMutation.reset();
  };

  const validatePhone = (value: string): string | undefined => {
    if (!value.trim()) return undefined;
    return normalizePhoneToE164(value) ? undefined : `Revisa el número. ${PHONE_EXAMPLE_HINT}.`;
  };

  const validateEnrollment = (): EnrollmentFieldErrors => {
    const errors: EnrollmentFieldErrors = {};
    if (!fullName.trim()) errors.fullName = 'Escribe tu nombre completo.';
    if (!email.trim()) errors.email = 'Escribe tu correo para enviarte los pasos.';
    else if (!looksLikeEmail(email)) errors.email = 'Revisa tu correo: debe tener el formato nombre@dominio.com.';
    const phoneError = validatePhone(phone);
    if (phoneError) errors.phone = phoneError;
    if (!termsAccepted) errors.terms = 'Debes aceptar los términos y la política de cancelación para continuar.';
    return errors;
  };

  const handleSubmit = (evt: FormEvent<HTMLFormElement>) => {
    evt.preventDefault();
    if (registrationMutation.isPending || registrationMutation.isSuccess) return;
    const errors = validateEnrollment();
    setFieldErrors(errors);
    const fieldOrder: CourseRegistrationField[] = ['fullName', 'email', 'phone', 'howHeard', 'terms'];
    const firstInvalid = fieldOrder.find((field) => errors[field]);
    if (firstInvalid) {
      if (firstInvalid === 'fullName' || firstInvalid === 'email') setEditAccountFields(true);
      focusField(firstInvalid);
      return;
    }
    const payload: CourseRegistrationRequest = {
      fullName: fullName.trim(),
      email: email.trim(),
      phoneE164: normalizePhoneToE164(phone) ?? undefined,
      source: 'landing',
      howHeard: howHeard.trim() ? howHeard.trim() : undefined,
      utm: utmParams,
      termsAccepted,
    };
    const fingerprint = JSON.stringify([selectedSlug, payload]);
    const previous = lastAttemptRef.current;
    if (
      previous
      && (previous.retireAlways || (previous.retireOnChange && previous.fingerprint !== fingerprint))
    ) {
      // A corrected payload after a definitive rejection must not reuse the
      // key, or the server rejects it as a different request under the same key.
      checkoutIdempotency.current = null;
    }
    checkoutIdempotency.current ??= createCourseIdempotencyKey();
    lastAttemptRef.current = { fingerprint, retireOnChange: false, retireAlways: false };
    registrationMutation.mutate({ payload, idempotencyKey: checkoutIdempotency.current });
  };

  const enrollTriggerRef = useRef<HTMLElement | null>(null);
  const returnFocusOnExitRef = useRef(false);
  // Return focus explicitly once the dialog has left: Safari does not focus
  // buttons on click, so the dialog's own restore target can be <body>.
  const handleEnrollmentExited = () => {
    if (!returnFocusOnExitRef.current) return;
    returnFocusOnExitRef.current = false;
    const target = [enrollTriggerRef.current, heroCtaRef.current].find(
      (candidate) => candidate?.isConnected && !candidate.hasAttribute('disabled'),
    );
    target?.focus();
  };
  const openEnrollment = (trigger?: HTMLElement | null) => {
    enrollTriggerRef.current = trigger ?? null;
    setEnrollOpen(true);
  };

  const closeEnrollment = useCallback(() => {
    setEnrollOpen(false);
    returnFocusOnExitRef.current = true;
    const params = new URLSearchParams(location.search);
    if (params.has(ENROLL_QUERY_PARAM)) {
      params.delete(ENROLL_QUERY_PARAM);
      const search = params.toString();
      navigate({ pathname: location.pathname, search: search ? `?${search}` : '' }, { replace: true });
    }
  }, [location.pathname, location.search, navigate]);

  // Deep link (?inscribirme=1), e.g. when coming back from login.
  useEffect(() => {
    if (routeRegistrationId) return;
    const params = new URLSearchParams(location.search);
    if (params.get(ENROLL_QUERY_PARAM) === '1') {
      setEnrollOpen(true);
    }
  }, [location.search, routeRegistrationId]);

  const handleLoginForAutofill = () => {
    saveEnrollmentDraft(selectedSlug, { fullName, email, phone, howHeard });
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
  const courseTitle = cmsPayload?.hero?.title ?? meta?.title ?? 'el curso';
  const priceLabel = metaQuery.isLoading
    ? null
    : formatCurrencyForUser(meta?.price ?? 150, meta?.currency ?? resolveRuntimeCurrency());
  const enrollCtaLabel = cmsPayload?.hero?.cta ?? 'Inscribirme';

  const submitted = registrationMutation.isSuccess;
  const submitting = registrationMutation.isPending;
  useEffect(() => {
    if (!pendingFocusField || submitting) return;
    const element = fieldInputRefs[pendingFocusField].current;
    if (!element || element.disabled) return;
    setPendingFocusField(null);
    element.focus();
    if (typeof element.scrollIntoView === 'function') {
      element.scrollIntoView({ block: 'center', behavior: 'smooth' });
    }
  }, [pendingFocusField, submitting, fieldInputRefs, editAccountFields]);
  const serverError = registrationMutation.error
    ? describeCourseRegistrationError(registrationMutation.error)
    : null;
  const visibleFieldErrors: EnrollmentFieldErrors = serverError?.field
    ? { ...fieldErrors, [serverError.field]: serverError.message }
    : fieldErrors;
  const formError = serverError && !serverError.field ? serverError.message : null;
  const leadReceived = submitted && checkout !== null && !checkout.checkoutAvailable;

  // Focus the first empty required field as soon as the enrollment sheet opens.
  useEffect(() => {
    if (!enrollOpen) return;
    const timer = window.setTimeout(() => {
      if (registrationMutation.isSuccess) return;
      // Read the rendered inputs: account-prefilled fields may be collapsed into a summary.
      const emptyRequired = [fullNameInputRef.current, emailInputRef.current]
        .find((input) => input && !input.value.trim());
      const target = emptyRequired
        ?? (termsInputRef.current && !termsInputRef.current.checked ? termsInputRef.current : null)
        ?? submitButtonRef.current;
      target?.focus();
    }, 0);
    return () => window.clearTimeout(timer);
    // Only on open: later edits must not move focus.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [enrollOpen]);

  // Mobile sticky CTA: visible once the hero CTA has scrolled above the
  // viewport. A scroll check (not IntersectionObserver) so a fast fling that
  // jumps the button from below the fold to above it is still detected.
  useEffect(() => {
    const target = heroCtaElement;
    if (!target) return;
    let frame = 0;
    const update = () => {
      frame = 0;
      setShowStickyCta(target.getBoundingClientRect().bottom < 0);
    };
    const schedule = () => {
      if (!frame) frame = window.requestAnimationFrame(update);
    };
    update();
    window.addEventListener('scroll', schedule, { passive: true });
    window.addEventListener('resize', schedule);
    return () => {
      if (frame) window.cancelAnimationFrame(frame);
      window.removeEventListener('scroll', schedule);
      window.removeEventListener('resize', schedule);
    };
  }, [heroCtaElement]);

  useEffect(() => {
    if (!pendingCheckoutFocus || !checkout) return;
    const card = checkoutCardRef.current;
    if (!card) return;
    setPendingCheckoutFocus(false);
    if (typeof card.scrollIntoView === 'function') card.scrollIntoView({ block: 'start', behavior: 'smooth' });
    card.focus({ preventScroll: true });
  }, [checkout, pendingCheckoutFocus]);

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
      <Container
        maxWidth="lg"
        sx={{
          pt: { xs: 4, md: 6 },
          // Leave room for the mobile sticky CTA so it never covers the last card.
          pb: { xs: 'calc(96px + env(safe-area-inset-bottom, 0px))', md: 6 },
          // The radio bar's own height is reserved on <body> by RadioWidget.
        }}
      >
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
            onPrimaryClick={openEnrollment}
            primaryCtaRef={setHeroCtaRef}
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
            <Grid item xs={12} md={5} sx={{ order: { xs: checkout ? -1 : 0, md: 0 } }}>
              <EnrollSummaryCard
                onEnroll={openEnrollment}
                ctaLabel={enrollCtaLabel}
                submitted={submitted}
                checkoutAvailable={Boolean(checkout?.checkoutAvailable)}
                isFull={isFull}
                whatsappHref={whatsappHref}
                priceLabel={priceLabel}
                dateRangeLabel={dateRangeLabel}
              />
              {checkout && (
                <Box ref={checkoutCardRef} tabIndex={-1} sx={{ outline: 'none', scrollMarginTop: 16 }}>
                  <CourseCheckoutCard
                    checkout={checkout}
                    paymentBusy={paymentBusy}
                    hostedPaymentLocked={hostedPaymentLocked}
                    hostedPaymentDisabled={datafastDialogOpen || paypalDialogOpen}
                    paymentError={paymentError}
                    checkoutLookupToken={checkoutLookupToken}
                    initialBuyerPhone={normalizePhoneToE164(phone) ?? phone}
                    paypalAvailable={Boolean(paypalClientId && paypalReady)}
                    onDatafast={() => void handleDatafastPayment()}
                    onPaypal={() => void handlePaypalPayment()}
                    onHostedSafetyLockChange={setHostedPaymentLocked}
                    onHostedPaymentConfirmed={async () => {
                      if (!checkoutLookupToken) return;
                      setCheckout(await Courses.getCheckout(
                        checkout.courseSlug,
                        checkout.registrationId,
                        checkoutLookupToken,
                      ));
                    }}
                  />
                </Box>
              )}
              <InstructorCard meta={meta} />
              {meta?.locationLabel && meta?.locationMapUrl && (
                <LocationCard label={meta.locationLabel} mapUrl={meta.locationMapUrl} />
              )}
            </Grid>
          </Grid>
        </Stack>
        <StickyEnrollBar
          visible={showStickyCta && !checkout && !routeRegistrationId}
          onEnroll={openEnrollment}
          ctaLabel={enrollCtaLabel}
          isFull={isFull}
          whatsappHref={whatsappHref}
          priceLabel={priceLabel}
          seatsLabel={seatsLabel}
        />
        <EnrollmentDialog
          open={enrollOpen}
          onClose={closeEnrollment}
          onExited={handleEnrollmentExited}
          fullScreen={isPhone}
          courseTitle={courseTitle}
          priceLabel={priceLabel}
          dateRangeLabel={dateRangeLabel}
          onSubmit={handleSubmit}
          fullName={fullName}
          email={email}
          phone={phone}
          howHeard={howHeard}
          onFullNameChange={(value) => {
            setFullName(value);
            clearFieldError('fullName');
          }}
          onEmailChange={(value) => {
            setEmail(value);
            clearFieldError('email');
          }}
          onPhoneChange={(value) => {
            setPhone(value);
            clearFieldError('phone');
          }}
          onPhoneBlur={() => {
            const phoneError = validatePhone(phone);
            setFieldErrors((current) => {
              const next = { ...current };
              if (phoneError) next.phone = phoneError;
              else delete next.phone;
              return next;
            });
          }}
          onHowHeardChange={(value) => {
            setHowHeard(value);
            clearFieldError('howHeard');
          }}
          termsAccepted={termsAccepted}
          onTermsAcceptedChange={(value) => {
            setTermsAccepted(value);
            clearFieldError('terms');
          }}
          termsUrl={cmsPayload?.termsUrl ?? null}
          fieldErrors={visibleFieldErrors}
          formError={formError}
          submitting={submitting}
          leadReceived={leadReceived}
          isFull={isFull}
          whatsappHref={whatsappHref}
          cohortOptions={cohortOptions}
          selectedSlug={selectedSlug}
          onSlugChange={handleSelectedSlugChange}
          accountFields={editAccountFields ? [] : accountFields}
          onEditAccountFields={() => setEditAccountFields(true)}
          signedIn={Boolean(session)}
          loginHref={buildLoginRedirectPath(enrollmentResumePath)}
          onLoginForAutofill={handleLoginForAutofill}
          inputRefs={fieldInputRefs}
          submitButtonRef={submitButtonRef}
        />
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
  hostedPaymentLocked,
  hostedPaymentDisabled,
  paymentError,
  checkoutLookupToken,
  initialBuyerPhone,
  paypalAvailable,
  onDatafast,
  onPaypal,
  onHostedSafetyLockChange,
  onHostedPaymentConfirmed,
}: {
  checkout: CourseCheckoutResponse;
  paymentBusy: boolean;
  hostedPaymentLocked: boolean;
  hostedPaymentDisabled: boolean;
  paymentError: string | null;
  checkoutLookupToken: string | null;
  initialBuyerPhone?: string | null;
  paypalAvailable: boolean;
  onDatafast: () => void;
  onPaypal: () => void;
  onHostedSafetyLockChange: (locked: boolean) => void;
  onHostedPaymentConfirmed: () => void | Promise<void>;
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
          <Stack spacing={1.5}>
            {checkout.checkoutAvailable && !paid && <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                {checkout.paymentMethods.includes('datafast') && (
                  <Button variant="contained" disabled={paymentBusy || hostedPaymentLocked} onClick={onDatafast}>
                    Pagar con Datafast
                  </Button>
                )}
                {checkout.paymentMethods.includes('paypal') && paypalAvailable && (
                  <Button variant="outlined" disabled={paymentBusy || hostedPaymentLocked} onClick={onPaypal}>
                    Pagar con PayPal
                  </Button>
                )}
            </Stack>}
            {checkout.checkoutId && checkoutLookupToken && (
                <HostedProviderCheckout
                  checkout={{
                    checkoutId: checkout.checkoutId,
                    lookupToken: checkoutLookupToken,
                    returnPath: `/curso/${encodeURIComponent(checkout.courseSlug)}/orden/${checkout.registrationId}`,
                  }}
                  offeredMethods={checkout.checkoutAvailable && !paid ? checkout.paymentMethods : []}
                  disabled={!checkout.checkoutAvailable || paid || paymentBusy || hostedPaymentDisabled}
                  initialBuyerPhone={initialBuyerPhone}
                  onSafetyLockChange={onHostedSafetyLockChange}
                  onPaymentConfirmed={onHostedPaymentConfirmed}
                />
            )}
          </Stack>
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
  primaryCtaRef,
  whatsappHref,
  imageUrl,
  heroOverride,
  seatsLabel,
  isFull,
  dateRangeLabel,
}: {
  meta?: CourseMetadata;
  loading: boolean;
  onPrimaryClick: (trigger: HTMLElement) => void;
  primaryCtaRef?: Ref<HTMLButtonElement>;
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
            ref={primaryCtaRef}
            variant="contained"
            size="large"
            onClick={(event) => onPrimaryClick(event.currentTarget)}
            disabled={isFull}
            aria-haspopup="dialog"
            sx={{
              bgcolor: '#7c3aed',
              color: '#f8fafc',
              px: 3,
              minHeight: 48,
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
              minHeight: 48,
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
                  direction={{ xs: 'column', sm: 'row' }}
                  spacing={1}
                  alignItems={{ xs: 'flex-start', sm: 'center' }}
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

function EnrollSummaryCard({
  onEnroll,
  ctaLabel,
  submitted,
  checkoutAvailable,
  isFull,
  whatsappHref,
  priceLabel,
  dateRangeLabel,
}: {
  onEnroll: (trigger: HTMLElement) => void;
  ctaLabel: string;
  submitted: boolean;
  checkoutAvailable: boolean;
  isFull: boolean;
  whatsappHref: string;
  priceLabel: string | null;
  dateRangeLabel: string;
}) {
  return (
    <Card
      sx={{
        background: 'rgba(255,255,255,0.03)',
        border: '1px solid rgba(255,255,255,0.08)',
        color: '#e2e8f0',
      }}
    >
      <CardContent>
        <Stack spacing={2}>
          <Typography variant="h6" component="h2" sx={{ color: '#f8fafc', fontWeight: 700 }}>
            Reserva tu cupo
          </Typography>
          <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.78)' }}>
            Déjanos tus datos en un solo paso y te enviaremos cómo completar el pago. No necesitas crear una cuenta.
          </Typography>
          {priceLabel && (
            <Typography variant="body2" sx={{ color: '#cbd5f5', fontWeight: 700 }}>
              {priceLabel} · {dateRangeLabel}
            </Typography>
          )}
          <Alert
            severity={isFull ? 'warning' : 'info'}
            sx={{
              flexWrap: 'wrap',
              '& .MuiAlert-message': { minWidth: 0, overflow: 'visible' },
              '& .MuiAlert-action': { ml: { xs: 0, sm: 'auto' }, pl: { xs: 0, sm: 2 }, pb: 0.5 },
            }}
            action={
              <Button
                size="small"
                startIcon={<WhatsAppIcon />}
                href={whatsappHref}
                target="_blank"
                rel="noreferrer"
                variant="outlined"
                color={isFull ? 'warning' : 'info'}
                sx={{ minHeight: 44 }}
              >
                {isFull ? 'Avísame' : 'Escríbenos'}
              </Button>
            }
          >
            {isFull ? 'Cupos agotados. Escríbenos y te avisamos si se libera un cupo.' : 'Cupos limitados.'}
          </Alert>
          {submitted ? (
            <Alert severity="success" variant="outlined" icon={<CheckCircleIcon />}>
              {checkoutAvailable
                ? 'Inscripción recibida. Completa el pago en tu orden para confirmar el cupo.'
                : 'Inscripción recibida. Te escribiremos con los siguientes pasos.'}
            </Alert>
          ) : (
            <Button
              variant="contained"
              size="large"
              fullWidth
              onClick={(event) => onEnroll(event.currentTarget)}
              disabled={isFull}
              aria-haspopup="dialog"
              startIcon={<CelebrationIcon />}
              sx={{ minHeight: 48, bgcolor: '#7c3aed', color: '#f8fafc' }}
            >
              {isFull ? 'Cupos agotados' : ctaLabel}
            </Button>
          )}
        </Stack>
      </CardContent>
    </Card>
  );
}

function StickyEnrollBar({
  visible,
  onEnroll,
  ctaLabel,
  isFull,
  whatsappHref,
  priceLabel,
  seatsLabel,
}: {
  visible: boolean;
  onEnroll: (trigger: HTMLElement) => void;
  ctaLabel: string;
  isFull: boolean;
  whatsappHref: string;
  priceLabel: string | null;
  seatsLabel: string;
}) {
  return (
    <Box
      role="region"
      aria-label="Inscripción rápida"
      hidden={!visible}
      data-testid="course-sticky-enroll"
      sx={{
        display: { xs: visible ? 'flex' : 'none', md: 'none' },
        position: 'fixed',
        left: 0,
        right: 0,
        // Sit above the docked radio bar and global player instead of
        // competing for the same fixed position; 0 when neither is shown.
        bottom: BOTTOM_DOCK_OFFSET,
        zIndex: (theme) => theme.zIndex.appBar,
        alignItems: 'center',
        gap: 2,
        px: 2,
        pt: 1.5,
        pb: 'calc(12px + env(safe-area-inset-bottom, 0px))',
        bgcolor: 'rgba(11,15,27,0.97)',
        borderTop: '1px solid rgba(255,255,255,0.12)',
        boxShadow: '0 -12px 30px rgba(0,0,0,0.35)',
      }}
    >
      <Box sx={{ minWidth: 0, flex: 1 }}>
        {priceLabel && (
          <Typography variant="subtitle1" noWrap sx={{ color: '#f8fafc', fontWeight: 800, lineHeight: 1.2 }}>
            {priceLabel}
          </Typography>
        )}
        <Typography variant="caption" noWrap sx={{ color: isFull ? '#fcd34d' : '#93c5fd', fontWeight: 700 }}>
          {seatsLabel}
        </Typography>
      </Box>
      {isFull ? (
        <Button
          variant="contained"
          color="success"
          startIcon={<WhatsAppIcon />}
          href={whatsappHref}
          target="_blank"
          rel="noreferrer"
          sx={{ minHeight: 48, flexShrink: 0 }}
        >
          Avísame
        </Button>
      ) : (
        <Button
          variant="contained"
          onClick={(event) => onEnroll(event.currentTarget)}
          aria-haspopup="dialog"
          sx={{ minHeight: 48, px: 3, flexShrink: 0, bgcolor: '#7c3aed', color: '#f8fafc' }}
        >
          {ctaLabel}
        </Button>
      )}
    </Box>
  );
}

interface EnrollmentInputRefs {
  fullName: RefObject<HTMLInputElement>;
  email: RefObject<HTMLInputElement>;
  phone: RefObject<HTMLInputElement>;
  howHeard: RefObject<HTMLTextAreaElement>;
  terms: RefObject<HTMLInputElement>;
}

function EnrollmentDialog({
  open,
  onClose,
  onExited,
  fullScreen,
  courseTitle,
  priceLabel,
  dateRangeLabel,
  onSubmit,
  fullName,
  email,
  phone,
  howHeard,
  onFullNameChange,
  onEmailChange,
  onPhoneChange,
  onPhoneBlur,
  onHowHeardChange,
  termsAccepted,
  onTermsAcceptedChange,
  termsUrl,
  fieldErrors,
  formError,
  submitting,
  leadReceived,
  isFull,
  whatsappHref,
  cohortOptions,
  selectedSlug,
  onSlugChange,
  accountFields,
  onEditAccountFields,
  signedIn,
  loginHref,
  onLoginForAutofill,
  inputRefs,
  submitButtonRef,
}: {
  open: boolean;
  onClose: () => void;
  onExited: () => void;
  fullScreen: boolean;
  courseTitle: string;
  priceLabel: string | null;
  dateRangeLabel: string;
  onSubmit: (evt: FormEvent<HTMLFormElement>) => void;
  fullName: string;
  email: string;
  phone: string;
  howHeard: string;
  onFullNameChange: (val: string) => void;
  onEmailChange: (val: string) => void;
  onPhoneChange: (val: string) => void;
  onPhoneBlur: () => void;
  onHowHeardChange: (val: string) => void;
  termsAccepted: boolean;
  onTermsAcceptedChange: (value: boolean) => void;
  termsUrl: string | null;
  fieldErrors: EnrollmentFieldErrors;
  formError: string | null;
  submitting: boolean;
  leadReceived: boolean;
  isFull: boolean;
  whatsappHref: string;
  cohortOptions: { slug: string; label: string }[];
  selectedSlug: string;
  onSlugChange: (slug: string) => void;
  accountFields: ('fullName' | 'email')[];
  onEditAccountFields: () => void;
  signedIn: boolean;
  loginHref: string;
  onLoginForAutofill: () => void;
  inputRefs: EnrollmentInputRefs;
  submitButtonRef: RefObject<HTMLButtonElement>;
}) {
  const titleId = 'course-enroll-title';
  const termsErrorId = 'course-enroll-terms-error';
  const disableInputs = isFull || submitting;
  const normalizedPhone = phone.trim() ? normalizePhoneToE164(phone) : null;
  const phoneHelper = fieldErrors.phone
    ?? (normalizedPhone && normalizedPhone !== phone.replace(/[\s\-().]/g, '')
      ? `Lo enviaremos como ${normalizedPhone}.`
      : `Opcional. ${PHONE_EXAMPLE_HINT}.`);
  const showNameField = !accountFields.includes('fullName') || Boolean(fieldErrors.fullName);
  const showEmailField = !accountFields.includes('email') || Boolean(fieldErrors.email);
  const summarizedFields = accountFields.filter((field) =>
    field === 'fullName' ? !showNameField : !showEmailField);
  const safeAreaActionsSx = {
    px: 3,
    pt: 1.5,
    pb: fullScreen ? 'calc(16px + env(safe-area-inset-bottom, 0px))' : 2,
  };
  const termsLabel = termsUrl ? (
    <>
      Acepto los{' '}
      <Link href={termsUrl} target="_blank" rel="noreferrer" sx={{ color: '#93c5fd' }}>
        términos y la política de cancelación
      </Link>{' '}
      del curso.
    </>
  ) : (
    'Acepto los términos y la política de cancelación del curso.'
  );

  return (
    <Dialog
      open={open}
      onClose={onClose}
      fullScreen={fullScreen}
      fullWidth
      maxWidth="sm"
      scroll="paper"
      disableRestoreFocus
      TransitionProps={{ onExited }}
      aria-labelledby={titleId}
      {...(fullScreen ? { TransitionComponent: SlideUpTransition } : {})}
      PaperProps={{
        sx: {
          bgcolor: '#0f1629',
          backgroundImage: 'none',
          color: '#e2e8f0',
          border: fullScreen ? 'none' : '1px solid rgba(255,255,255,0.12)',
        },
      }}
    >
      <DialogTitle
        id={titleId}
        sx={{
          pr: 8,
          color: '#f8fafc',
          fontWeight: 800,
          pt: fullScreen ? 'calc(16px + env(safe-area-inset-top, 0px))' : 2,
        }}
      >
        {leadReceived ? 'Solicitud recibida' : `Inscríbete en ${courseTitle}`}
        <IconButton
          aria-label="Cerrar"
          onClick={onClose}
          sx={{
            position: 'absolute',
            right: 8,
            top: fullScreen ? 'calc(8px + env(safe-area-inset-top, 0px))' : 8,
            width: 48,
            height: 48,
            color: 'rgba(226,232,240,0.85)',
          }}
        >
          <CloseIcon />
        </IconButton>
      </DialogTitle>
      {leadReceived ? (
        <>
          <DialogContent dividers sx={{ borderColor: 'rgba(255,255,255,0.12)' }}>
            <Stack spacing={2} role="status">
              <CheckCircleIcon aria-hidden sx={{ fontSize: 48, color: '#86efac' }} />
              <Typography sx={{ color: '#f8fafc', fontWeight: 700 }}>
                Recibimos tu solicitud para {courseTitle}.
              </Typography>
              <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.85)' }}>
                Próximos pasos:
              </Typography>
              <Box component="ol" sx={{ m: 0, pl: 3, '& li': { mb: 1 }, color: 'rgba(226,232,240,0.85)' }}>
                <li>
                  Te escribiremos a <strong>{email.trim()}</strong>
                  {phone.trim() ? ' y por WhatsApp' : ''} para confirmar tu cupo.
                </li>
                <li>Te enviaremos las opciones de pago.</li>
                <li>Tu cupo queda confirmado cuando se verifique el pago.</li>
              </Box>
              <Alert severity="info" variant="outlined">
                Solicitud recibida. El checkout no está habilitado y no se reservó ni pagó un cupo todavía.
              </Alert>
            </Stack>
          </DialogContent>
          <DialogActions disableSpacing sx={{ ...safeAreaActionsSx, gap: 1, flexWrap: 'wrap' }}>
            <Button
              startIcon={<WhatsAppIcon />}
              href={whatsappHref}
              target="_blank"
              rel="noreferrer"
              sx={{ minHeight: 48, color: '#93c5fd' }}
            >
              Escríbenos por WhatsApp
            </Button>
            <Button variant="contained" onClick={onClose} sx={{ minHeight: 48, ml: 'auto' }}>
              Listo
            </Button>
          </DialogActions>
        </>
      ) : (
        <Box
          component="form"
          noValidate
          onSubmit={onSubmit}
          aria-busy={submitting}
          aria-labelledby={titleId}
          sx={{ display: 'flex', flexDirection: 'column', flex: '1 1 auto', minHeight: 0 }}
        >
          <DialogContent dividers sx={{ borderColor: 'rgba(255,255,255,0.12)' }}>
            <Stack spacing={2}>
              {priceLabel && (
                <Typography variant="body2" sx={{ color: '#cbd5f5', fontWeight: 700 }}>
                  {priceLabel} · {dateRangeLabel}
                </Typography>
              )}
              {isFull && (
                <Alert
                  severity="warning"
                  action={
                    <Button
                      size="small"
                      startIcon={<WhatsAppIcon />}
                      href={whatsappHref}
                      target="_blank"
                      rel="noreferrer"
                      color="warning"
                      sx={{ minHeight: 44 }}
                    >
                      Avísame
                    </Button>
                  }
                >
                  Cupos agotados. Escríbenos y te avisamos si se libera un cupo.
                </Alert>
              )}
              {!signedIn && (
                <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.8)' }}>
                  Puedes inscribirte sin cuenta. ¿Ya tienes cuenta?{' '}
                  <Link
                    component={RouterLink}
                    to={loginHref}
                    onClick={onLoginForAutofill}
                    sx={{ color: '#93c5fd', fontWeight: 600, display: 'inline-block', py: 1 }}
                  >
                    Inicia sesión para autocompletar
                  </Link>
                </Typography>
              )}
              {cohortOptions.length > 1 && (
                <TextField
                  select
                  id="course-enroll-cohort"
                  label="Fecha de inicio"
                  value={selectedSlug}
                  onChange={(e) => onSlugChange(e.target.value)}
                  disabled={submitting}
                  helperText="Elige la fecha en la que quieres iniciar."
                  fullWidth
                  {...darkFieldProps}
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
              {summarizedFields.length > 0 && (
                <Box
                  sx={{
                    p: 1.5,
                    borderRadius: 2,
                    bgcolor: 'rgba(255,255,255,0.04)',
                    border: '1px solid rgba(255,255,255,0.1)',
                  }}
                >
                  <Typography variant="body2" sx={{ color: 'rgba(226,232,240,0.78)' }}>
                    Usaremos los datos de tu cuenta:
                  </Typography>
                  {summarizedFields.includes('fullName') && (
                    <Typography sx={{ color: '#f8fafc', fontWeight: 600, overflowWrap: 'anywhere' }}>
                      {fullName}
                    </Typography>
                  )}
                  {summarizedFields.includes('email') && (
                    <Typography sx={{ color: '#f8fafc', overflowWrap: 'anywhere' }}>{email}</Typography>
                  )}
                  <Button
                    size="small"
                    onClick={onEditAccountFields}
                    disabled={submitting}
                    sx={{ mt: 0.5, minHeight: 44, color: '#93c5fd' }}
                  >
                    Editar datos
                  </Button>
                </Box>
              )}
              {showNameField && (
                <TextField
                  id="course-enroll-fullname"
                  label="Nombre completo"
                  required
                  value={fullName}
                  onChange={(e) => onFullNameChange(e.target.value)}
                  disabled={disableInputs}
                  error={Boolean(fieldErrors.fullName)}
                  helperText={fieldErrors.fullName}
                  inputRef={inputRefs.fullName}
                  autoComplete="name"
                  inputProps={{ enterKeyHint: 'next', maxLength: 160 }}
                  fullWidth
                  {...darkFieldProps}
                />
              )}
              {showEmailField && (
                <TextField
                  id="course-enroll-email"
                  label="Correo"
                  type="email"
                  required
                  value={email}
                  onChange={(e) => onEmailChange(e.target.value)}
                  disabled={disableInputs}
                  error={Boolean(fieldErrors.email)}
                  helperText={fieldErrors.email}
                  inputRef={inputRefs.email}
                  autoComplete="email"
                  inputProps={{ inputMode: 'email', enterKeyHint: 'next', maxLength: 254 }}
                  fullWidth
                  {...darkFieldProps}
                />
              )}
              <TextField
                id="course-enroll-phone"
                type="tel"
                label="WhatsApp (opcional)"
                placeholder="0991234567"
                value={phone}
                onChange={(e) => onPhoneChange(e.target.value)}
                onBlur={onPhoneBlur}
                disabled={disableInputs}
                error={Boolean(fieldErrors.phone)}
                helperText={phoneHelper}
                inputRef={inputRefs.phone}
                autoComplete="tel"
                inputProps={{ inputMode: 'tel', enterKeyHint: 'next', maxLength: 24 }}
                fullWidth
                {...darkFieldProps}
              />
              <TextField
                id="course-enroll-how-heard"
                label="¿Cómo te enteraste del curso? (opcional)"
                value={howHeard}
                onChange={(e) => onHowHeardChange(e.target.value)}
                disabled={disableInputs}
                error={Boolean(fieldErrors.howHeard)}
                helperText={fieldErrors.howHeard}
                inputRef={inputRefs.howHeard}
                inputProps={{ maxLength: 256 }}
                fullWidth
                multiline
                minRows={2}
                {...darkFieldProps}
              />
              <Box>
                <FormControlLabel
                  control={(
                    <Checkbox
                      checked={termsAccepted}
                      onChange={(event) => onTermsAcceptedChange(event.target.checked)}
                      required
                      disabled={disableInputs}
                      inputRef={inputRefs.terms}
                      inputProps={{
                        'aria-invalid': Boolean(fieldErrors.terms),
                        ...(fieldErrors.terms ? { 'aria-describedby': termsErrorId } : {}),
                      }}
                      sx={{
                        p: 1.25,
                        color: fieldErrors.terms ? '#fca5a5' : 'rgba(226,232,240,0.72)',
                      }}
                    />
                  )}
                  label={termsLabel}
                  sx={{
                    alignItems: 'flex-start',
                    mr: 0,
                    color: 'rgba(226,232,240,0.88)',
                    '& .MuiFormControlLabel-label': { fontSize: '0.9rem', pt: 1.25 },
                  }}
                />
                {fieldErrors.terms && (
                  <FormHelperText id={termsErrorId} error sx={{ color: '#fca5a5', ml: 1.5 }}>
                    {fieldErrors.terms}
                  </FormHelperText>
                )}
              </Box>
            </Stack>
          </DialogContent>
          <DialogActions
            disableSpacing
            sx={{ ...safeAreaActionsSx, flexDirection: 'column', alignItems: 'stretch', gap: 1 }}
          >
            {formError && (
              <Alert
                severity="error"
                sx={{
                  flexWrap: 'wrap',
                  '& .MuiAlert-message': { minWidth: 0 },
                  '& .MuiAlert-action': { ml: 0, pl: 0 },
                }}
                action={
                  <Button
                    size="small"
                    color="inherit"
                    startIcon={<WhatsAppIcon />}
                    href={whatsappHref}
                    target="_blank"
                    rel="noreferrer"
                    sx={{ minHeight: 44 }}
                  >
                    WhatsApp
                  </Button>
                }
              >
                {formError}
              </Alert>
            )}
            <Button
              ref={submitButtonRef}
              type="submit"
              variant="contained"
              size="large"
              fullWidth
              disabled={isFull || submitting}
              aria-busy={submitting}
              startIcon={submitting
                ? <CircularProgress size={18} color="inherit" aria-hidden />
                : <CelebrationIcon />}
              sx={{ minHeight: 48 }}
            >
              {isFull ? 'Cupos agotados' : submitting ? 'Enviando inscripción…' : 'Enviar inscripción'}
            </Button>
          </DialogActions>
        </Box>
      )}
    </Dialog>
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
