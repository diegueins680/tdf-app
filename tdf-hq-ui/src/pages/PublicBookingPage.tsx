import { logger } from '../utils/logger';
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useMetaTags } from '../hooks/useMetaTags';
import {
  clearSessionPersonalData,
  readSessionPersonalData,
  writeSessionPersonalData,
} from '../utils/sessionPersonalData';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Card,
  CardContent,
  Divider,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Chip,
  Checkbox,
  CircularProgress,
  Grid,
  MenuItem,
  Step,
  StepLabel,
  Stepper,
  Tooltip,
  Snackbar,
  Stack,
  TextField,
  Typography,
  useMediaQuery,
} from '@mui/material';
import AccessTimeIcon from '@mui/icons-material/AccessTime';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import LocalPhoneIcon from '@mui/icons-material/LocalPhone';
import PersonIcon from '@mui/icons-material/Person';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { DateTime } from 'luxon';
import {
  Bookings,
  loadPublicBookingLookupToken,
  storePublicBookingLookupToken,
  type PublicBookingCheckoutDTO,
  type PublicBookingQuoteDTO,
} from '../api/bookings';
import { API_BASE_URL } from '../api/client';
import { Meta } from '../api/meta';
import type { BookingDTO, DatafastCheckoutDTO, ServiceCatalogDTO } from '../api/types';
import { Engineers, type PublicEngineer } from '../api/engineers';
import { Services } from '../api/services';
import { STUDIO_MAP_URL, STUDIO_WHATSAPP_URL } from '../config/appConfig';
import { mergeServiceTypes, type ServiceType } from '../utils/serviceTypesStore';
import { env } from '../utils/env';
import { useSession } from '../session/SessionContext';
import { resolveRuntimeCurrency } from '../utils/formatters';
import ExperienceReviews from '../components/reviews/ExperienceReviews';

interface FormState {
  fullName: string;
  email: string;
  phone: string;
  serviceOfferingId: string;
  serviceType: string;
  startsAt: string;
  durationMinutes: number;
  notes: string;
  engineerId: number | null;
  engineerName: string;
  resourceLabels: string[];
}

export type PublicBookingPreset = 'dj-booth';

type BookingWithAliases = BookingDTO & {
  pbStartsAt?: string;
  cbStartsAt?: string;
  ubStartsAt?: string;
  pbDurationMinutes?: number;
  pbEngineerName?: string;
};

const toLocalInputValue = (date: Date) => {
  const pad = (val: number) => val.toString().padStart(2, '0');
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}T${pad(date.getHours())}:${pad(
    date.getMinutes(),
  )}`;
};

const PROFILE_STORAGE_KEY = 'tdf-public-booking-profile';
const OPEN_HOURS = { start: 8, end: 22 }; // 24h local time
const MAX_DURATION_MINUTES = (OPEN_HOURS.end - OPEN_HOURS.start) * 60;
const QUICK_SLOT_STEP_MINUTES = 30;
const BOOKING_STEPS = ['Contacto', 'Horario', 'Confirmación'] as const;
const EMAIL_PATTERN = /^\S+@\S+\.\S+$/;

const createBookingIdempotencyKey = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `service-booking-${crypto.randomUUID()}`;
  }
  return `service-booking-${Date.now()}-${Math.random().toString(16).slice(2)}`;
};

const formatMinorAmount = (currency: string, amountMinor: number): string =>
  `${currency} ${(amountMinor / 100).toLocaleString(undefined, {
    minimumFractionDigits: 2,
    maximumFractionDigits: 2,
  })}`;

const PUBLIC_BOOKING_PRESETS: Record<
  PublicBookingPreset,
  {
    path: string;
    serviceCode: string;
    eyebrow: string;
    title: string;
    description: string;
    introChips: string[];
    contactTitle: string;
    contactDescription: string;
    calendarNote: string;
    durationNote: string;
    notesPlaceholder: string;
  }
> = {
  'dj-booth': {
    path: '/dj-booth',
    serviceCode: 'dj-booth-practice',
    eyebrow: 'DJ Booth',
    title: 'Reserva práctica en DJ Booth',
    description:
      'Agenda horas de práctica o alquiler del DJ Booth. Este enlace usa el servicio Práctica en DJ Booth para asignar el booth automáticamente.',
    introChips: [
      '1. Elige horario para DJ Booth',
      '2. Reservamos por horas',
      '3. Confirmamos por email o WhatsApp',
    ],
    contactTitle: 'Datos para reservar tu booth',
    contactDescription: 'Usa un correo válido para recibir la confirmación de tu práctica en DJ Booth.',
    calendarNote: 'Bloque tentativo para el DJ Booth.',
    durationNote: 'Reserva por horas de práctica (30 min mínimo).',
    notesPlaceholder: 'Cuéntanos si traes USB/controlador, estilo musical o cualquier requerimiento para practicar.',
  },
};

const zoneLabel = (zone: string) => {
  try {
    return DateTime.now().setZone(zone).toFormat('ZZZZ');
  } catch {
    return zone;
  }
};

const START_STEP_MINUTES = 15;

const normalizeServiceToken = (value: string) => {
  return value
    .trim()
    .toLowerCase()
    .normalize('NFD')
    .replace(/\p{Diacritic}/gu, '')
    .replace(/[^a-z0-9]+/g, ' ')
    .trim();
};

const resolveServiceFromToken = (raw: string, list: ServiceType[]): ServiceType | null => {
  const trimmed = raw.trim();
  if (!trimmed) return null;
  const matchByIdentity = list.find((svc) => svc.id === trimmed || svc.code === trimmed);
  if (matchByIdentity) return matchByIdentity;
  const token = normalizeServiceToken(trimmed);
  if (!token) return null;
  const exact = list.find((svc) => normalizeServiceToken(svc.name) === token);
  if (exact) return exact;
  const partial = list.find((svc) => {
    const svcToken = normalizeServiceToken(svc.name);
    return svcToken.includes(token) || token.includes(svcToken);
  });
  return partial ?? null;
};

const resolvePresetService = (
  presetConfig: (typeof PUBLIC_BOOKING_PRESETS)[PublicBookingPreset],
  list: ServiceType[],
): ServiceType | null => list.find((service) => service.code === presetConfig.serviceCode) ?? null;

const serviceResourceLabels = (service: ServiceType | null | undefined): string[] =>
  (service?.defaultResources ?? []).map((resource) => resource.sdrResourceName);

const ensureDiegoOption = (list: PublicEngineer[]): PublicEngineer[] => {
  return list;
};

const alignToStepMinutes = (dt: DateTime, stepMinutes = START_STEP_MINUTES) => {
  if (!dt.isValid) return dt;
  const normalized = dt.set({ second: 0, millisecond: 0 });
  const remainder = normalized.minute % stepMinutes;
  if (remainder === 0) return normalized;
  return normalized.plus({ minutes: stepMinutes - remainder });
};

const normalizeDurationMinutes = (value: number, fallback = 60): number => {
  if (!Number.isFinite(value)) return fallback;
  const rounded = Math.round(value);
  return Math.min(MAX_DURATION_MINUTES, Math.max(30, rounded));
};

export const resolveFirstAvailableShortcut = ({
  dayOffset,
  studioTimeZone,
  userTimeZone,
  now = DateTime.now(),
}: {
  dayOffset: number;
  studioTimeZone: string;
  userTimeZone: string;
  now?: DateTime;
}) => {
  const nowStudio = now
    .setZone(studioTimeZone)
    .plus({ minutes: 15 })
    .set({ second: 0, millisecond: 0 });
  const targetDayStudio = nowStudio.startOf('day').plus({ days: dayOffset });
  const openStudio = targetDayStudio.set({ hour: OPEN_HOURS.start, minute: 0, second: 0, millisecond: 0 });
  const closeStudio = targetDayStudio.set({ hour: OPEN_HOURS.end, minute: 0, second: 0, millisecond: 0 });
  const baselineStudio = dayOffset === 0 && nowStudio > openStudio ? nowStudio : openStudio;
  const roundedStudio = alignToStepMinutes(baselineStudio, QUICK_SLOT_STEP_MINUTES);
  const latestStartStudio = closeStudio.minus({ minutes: QUICK_SLOT_STEP_MINUTES });
  const limitedStudio = roundedStudio >= closeStudio ? latestStartStudio : roundedStudio;
  return limitedStudio.setZone(userTimeZone);
};

const buildInitialForm = () => {
  const start = alignToStepMinutes(DateTime.now().plus({ minutes: 90 }));
  return {
    fullName: '',
    email: '',
    phone: '',
    serviceOfferingId: '',
    serviceType: '',
    startsAt: toLocalInputValue(start.toJSDate()),
    durationMinutes: 60,
    notes: '',
    engineerId: null,
    engineerName: '',
    resourceLabels: [],
  };
};

const buildGoogleCalendarUrl = (title: string, startIso: string, durationMinutes: number, location?: string, description?: string) => {
  const start = DateTime.fromISO(startIso);
  const end = start.plus({ minutes: durationMinutes });
  const fmt = (dt: DateTime) => dt.toUTC().toFormat("yyyyLLdd'T'HHmm'00'Z");
  const params = new URLSearchParams({
    action: 'TEMPLATE',
    text: title,
    dates: `${fmt(start)}/${fmt(end)}`,
    location: location ?? 'TDF Records',
    details: description ?? '',
  });
  return `https://www.google.com/calendar/render?${params.toString()}`;
};

const escapeIcsValue = (value: string) => {
  return value
    .replace(/\\/g, '\\\\')
    .replace(/\r?\n/g, '\\n')
    .replace(/,/g, '\\,')
    .replace(/;/g, '\\;');
};

const buildIcsDataUrl = (title: string, startIso: string, durationMinutes: number, location?: string, description?: string, uid?: string) => {
  const startUtc = DateTime.fromISO(startIso).toUTC();
  const endUtc = startUtc.plus({ minutes: durationMinutes });
  const stamp = DateTime.utc();
  const fmt = (dt: DateTime) => dt.toFormat("yyyyLLdd'T'HHmmss'Z'");
  const lines = [
    'BEGIN:VCALENDAR',
    'VERSION:2.0',
    'PRODID:-//TDF//Booking//ES',
    'CALSCALE:GREGORIAN',
    'METHOD:PUBLISH',
    'BEGIN:VEVENT',
    `UID:${escapeIcsValue(uid ?? `tdf-${Date.now()}@tdf`)}`,
    `DTSTAMP:${fmt(stamp)}`,
    `DTSTART:${fmt(startUtc)}`,
    `DTEND:${fmt(endUtc)}`,
    `SUMMARY:${escapeIcsValue(title)}`,
    `LOCATION:${escapeIcsValue(location ?? 'TDF Records')}`,
    `DESCRIPTION:${escapeIcsValue(description ?? '')}`,
    'END:VEVENT',
    'END:VCALENDAR',
  ];
  const ics = lines.join('\r\n');
  return `data:text/calendar;charset=utf8,${encodeURIComponent(ics)}`;
};

const toFriendlyBookingError = (error: unknown): string => {
  if (!(error instanceof Error)) return 'No pudimos crear la reserva. Intenta nuevamente.';
  const message = error.message.trim();
  if (message === '') return 'No pudimos crear la reserva. Intenta nuevamente.';
  const lowered = message.toLowerCase();
  if (
    lowered.includes('cors')
    || lowered.includes('v i t e')
    || lowered.includes('vite_api_base')
    || lowered.includes('origen app')
    || lowered.includes('network')
    || lowered.includes('fetch')
    || lowered.includes('conectar con el servicio')
  ) {
    return 'No pudimos conectar con el sistema de reservas. Intenta nuevamente en unos minutos.';
  }
  return message;
};

interface PublicBookingPageProps {
  preset?: PublicBookingPreset;
}

export default function PublicBookingPage({ preset }: PublicBookingPageProps = {}) {
  useMetaTags({
    title: 'Reservar',
    description: 'Reserva una sesión de estudio, clase o servicio en TDF Records.',
  });

  const location = useLocation();
  const presetConfig = preset ? PUBLIC_BOOKING_PRESETS[preset] : null;
  const healthQuery = useQuery({
    queryKey: ['health'],
    queryFn: Meta.health,
    staleTime: 30_000,
    refetchInterval: 30_000,
  });
  const serviceCatalogQuery = useQuery<ServiceCatalogDTO[]>({
    queryKey: ['service-catalog', 'public'],
    queryFn: () => Services.listPublic(),
    staleTime: 5 * 60 * 1000,
  });
  const baseServices = useMemo<ServiceType[]>(() => {
    return mergeServiceTypes(serviceCatalogQuery.data, { sort: false });
  }, [serviceCatalogQuery.data]);
  const presetService = useMemo(
    () => (presetConfig ? resolvePresetService(presetConfig, baseServices) : null),
    [baseServices, presetConfig],
  );
  const services = baseServices;
  const publicRoutePath = presetConfig?.path ?? '/reservar';
  const loginPath = `/login?redirect=${encodeURIComponent(publicRoutePath)}`;
  const signupPath = `/login?signup=1&redirect=${encodeURIComponent(publicRoutePath)}`;
  const { session, logout } = useSession();
  const isMobile = useMediaQuery('(max-width:600px)');
  const appliedServiceQuery = useRef(false);
  const appliedStoredProfile = useRef(false);
  const userTimeZone = useMemo(() => {
    if (typeof Intl === 'undefined' || !Intl.DateTimeFormat) return 'UTC';
    return Intl.DateTimeFormat().resolvedOptions().timeZone ?? 'UTC';
  }, []);
  const studioTimeZone = useMemo(
    () => env.read('VITE_DEFAULT_TIMEZONE') ?? 'UTC',
    [],
  );
  const studioZoneLabel = useMemo(() => zoneLabel(studioTimeZone), [studioTimeZone]);
  const userZoneLabel = useMemo(() => zoneLabel(userTimeZone), [userTimeZone]);
  const studioCurrency = useMemo(() => services[0]?.currency ?? resolveRuntimeCurrency(), [services]);
  const serviceCatalogUnavailable =
    serviceCatalogQuery.isFetched && (serviceCatalogQuery.isError || (serviceCatalogQuery.data?.length ?? 0) === 0);
  const requiresManualConfirmation =
    serviceCatalogUnavailable ||
    healthQuery.isError ||
    (Boolean(healthQuery.data?.status) && String(healthQuery.data?.status).toLowerCase() !== 'ok');
  const bookingStatusChip = useMemo(() => {
    if (requiresManualConfirmation) {
      return <Chip label="Te confirmamos por email" size="small" color="warning" variant="outlined" />;
    }
    if (healthQuery.isLoading && !healthQuery.data) {
      return <Chip label="Preparando agenda" size="small" variant="outlined" />;
    }
    if (String(healthQuery.data?.status ?? '').toLowerCase() === 'ok') {
      return <Chip label="Reserva en línea" size="small" color="success" variant="outlined" />;
    }
    return <Chip label="Reserva guiada" size="small" variant="outlined" />;
  }, [healthQuery.data, healthQuery.isLoading, requiresManualConfirmation]);
  const bookingReadinessNote = useMemo(() => {
    if (!requiresManualConfirmation) return null;
    if (serviceCatalogUnavailable) {
      return 'No pudimos cargar el catálogo de servicios. Reintenta en unos minutos; no enviaremos una reserva sin un servicio canónico.';
    }
    return 'Puedes dejar la solicitud ahora mismo. Confirmaremos disponibilidad y recursos contigo por correo o WhatsApp antes de bloquear la sesión.';
  }, [requiresManualConfirmation, serviceCatalogUnavailable]);
  const pageEyebrow = presetConfig?.eyebrow ?? 'Agenda pública';
  const pageTitle = presetConfig?.title ?? 'Reserva un servicio con TDF';
  const pageDescription =
    presetConfig?.description ??
    'Completa tus datos y agenda el horario que prefieras. Confirmaremos la reserva por correo y, si aún no tienes cuenta, crearemos tu acceso automáticamente.';
  const introChips = presetConfig?.introChips ?? [
    '1. Agenda sin crear cuenta',
    '2. Confirmamos por email',
    '3. Coordinamos por WhatsApp si lo dejas',
  ];
  const contactTitle = presetConfig?.contactTitle ?? 'Datos de contacto';
  const contactDescription =
    presetConfig?.contactDescription ??
    'Usa un correo válido para recibir la confirmación. Si eres nuevo, crearemos un perfil para ti.';
  const calendarNote = presetConfig?.calendarNote ?? 'Bloque tentativo en el calendario.';
  const durationNote = presetConfig?.durationNote ?? 'Duración estándar de 1h (ajústala si necesitas más tiempo).';
  const notesPlaceholder =
    presetConfig?.notesPlaceholder ?? 'Cuéntanos qué necesitas (ej: grabación de voz, mezcla, etc.)';
  const [form, setForm] = useState<FormState>(buildInitialForm);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<BookingDTO | null>(null);
  const [checkoutSuccess, setCheckoutSuccess] = useState<PublicBookingCheckoutDTO | null>(null);
  const [authoritativeQuote, setAuthoritativeQuote] = useState<PublicBookingQuoteDTO | null>(null);
  const [paymentBusy, setPaymentBusy] = useState(false);
  const [paymentError, setPaymentError] = useState<string | null>(null);
  const [datafastCheckout, setDatafastCheckout] = useState<DatafastCheckoutDTO | null>(null);
  const [datafastDialogOpen, setDatafastDialogOpen] = useState(false);
  const [datafastWidgetKey, setDatafastWidgetKey] = useState(0);
  const datafastFormRef = useRef<HTMLDivElement>(null);
  const [paypalReady, setPaypalReady] = useState(false);
  const [paypalDialogOpen, setPaypalDialogOpen] = useState(false);
  const [paypalOrderId, setPaypalOrderId] = useState<string | null>(null);
  const paypalButtonRef = useRef<HTMLDivElement>(null);
  const paypalClientId = useMemo(() => env.read('VITE_PAYPAL_CLIENT_ID') ?? '', []);
  const [manualDialogOpen, setManualDialogOpen] = useState(false);
  const [manualReference, setManualReference] = useState('');
  const [termsAccepted, setTermsAccepted] = useState(false);
  const checkoutIdempotency = useRef<{ fingerprint: string; key: string } | null>(null);
  const [rememberProfile, setRememberProfile] = useState(false);
  const [engineers, setEngineers] = useState<PublicEngineer[]>([]);
  const [engineersLoading, setEngineersLoading] = useState(false);
  const [engineersError, setEngineersError] = useState<string | null>(null);
  const [durationNotice, setDurationNotice] = useState<string | null>(null);
  const [availabilityNonce, setAvailabilityNonce] = useState(0);
  const [availabilityStatus, setAvailabilityStatus] = useState<'idle' | 'checking' | 'available' | 'unavailable' | 'unknown'>('idle');
  const [availabilityNote, setAvailabilityNote] = useState<string | null>(null);
  const [assignEngineerLater, setAssignEngineerLater] = useState(false);
  const [snackbar, setSnackbar] = useState<{ open: boolean; message: string }>({ open: false, message: '' });
  const [activeStep, setActiveStep] = useState(0);

  useEffect(() => {
    if (!services.length) return;
    setForm((prev) => {
      if (presetService) {
        if (prev.serviceOfferingId === presetService.id) return prev;
        return {
          ...prev,
          serviceOfferingId: presetService.id,
          serviceType: presetService.name,
          resourceLabels: serviceResourceLabels(presetService),
        };
      }
      const serviceStillValid = services.some((svc) => svc.id === prev.serviceOfferingId);
      if (serviceStillValid) return prev;
      const nextService = services[0];
      if (!nextService) return prev;
      return {
        ...prev,
        serviceOfferingId: nextService.id,
        serviceType: nextService.name,
        resourceLabels: serviceResourceLabels(nextService),
      };
    });
  }, [presetService, services]);

  useEffect(() => {
    if (appliedStoredProfile.current) return;
    if (typeof window === 'undefined') return;
    appliedStoredProfile.current = true;
    try {
      const raw = readSessionPersonalData(PROFILE_STORAGE_KEY);
      if (!raw) return;
      const stored = JSON.parse(raw) as Partial<FormState>;
      const nextService =
        presetService ??
        services.find((service) => service.id === stored.serviceOfferingId) ??
        services[0];
      setForm((prev) => ({
        ...prev,
        fullName: stored.fullName ?? prev.fullName,
        email: stored.email ?? prev.email,
        phone: stored.phone ?? prev.phone,
        serviceOfferingId: nextService?.id ?? '',
        serviceType: nextService?.name ?? '',
        resourceLabels: serviceResourceLabels(nextService),
      }));
      setRememberProfile(true);
    } catch {
      // ignore parsing issues
    }
  }, [presetService, services]);

  useEffect(() => {
    if (appliedServiceQuery.current) return;
    if (presetService) return;
    if (!services.length) return;
    const params = new URLSearchParams(location.search);
    const rawToken = params.get('service') ?? params.get('servicio');
    if (!rawToken) return;
    const match = resolveServiceFromToken(rawToken, services);
    if (!match) return;
    appliedServiceQuery.current = true;
    setForm((prev) => ({
      ...prev,
      serviceOfferingId: match.id,
      serviceType: match.name,
      resourceLabels: serviceResourceLabels(match),
    }));
  }, [location.search, presetService, services]);

  useEffect(() => {
    if (!session?.displayName) return;
    setForm((prev) => {
      if (prev.fullName.trim()) return prev;
      return { ...prev, fullName: session.displayName };
    });
  }, [session]);

  useEffect(() => {
    if (!session?.username) return;
    setForm((prev) => {
      const normalizedEmail = prev.email.trim() || (session.username.includes('@') ? session.username : '');
      if (!normalizedEmail || normalizedEmail === prev.email) return prev;
      return { ...prev, email: normalizedEmail };
    });
  }, [session]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    if (!rememberProfile) {
      clearSessionPersonalData(PROFILE_STORAGE_KEY);
      return;
    }
    const payload = {
      fullName: form.fullName.trim(),
      email: form.email.trim(),
      phone: form.phone.trim(),
      serviceOfferingId: form.serviceOfferingId,
    };
    writeSessionPersonalData(PROFILE_STORAGE_KEY, JSON.stringify(payload));
  }, [rememberProfile, form.fullName, form.email, form.phone, form.serviceOfferingId]);

  useEffect(() => {
    setEngineersLoading(true);
    Engineers.listPublic()
      .then((list) => {
        const withDiego = ensureDiegoOption(list);
        setEngineers(withDiego);
        setEngineersError(withDiego.length === 0 ? 'Escribe el nombre del ingeniero manualmente.' : null);
      })
      .catch(() => {
        setEngineers([]);
        setEngineersError('Ingresa el nombre manualmente (catálogo no disponible).');
      })
      .finally(() => setEngineersLoading(false));
  }, []);

  const formDisabled = submitting || Boolean(success) || serviceCatalogUnavailable;

  const sanitizeStart = useCallback(
    (candidate: DateTime, durationMinutes: number) => {
      if (!candidate.isValid) return candidate;
      const now = DateTime.now().setZone(userTimeZone).plus({ minutes: 15 });
      let next = candidate < now ? now : candidate;
      const openStudio = next.setZone(studioTimeZone).set({ hour: OPEN_HOURS.start, minute: 0, second: 0, millisecond: 0 });
      const closeStudio = next.setZone(studioTimeZone).set({ hour: OPEN_HOURS.end, minute: 0, second: 0, millisecond: 0 });

      let startStudio = next.setZone(studioTimeZone);
      if (startStudio < openStudio) {
        next = openStudio.setZone(userTimeZone);
        startStudio = next.setZone(studioTimeZone);
      }
      const latestStartStudio = closeStudio.minus({ minutes: durationMinutes });
      if (startStudio > latestStartStudio) {
        const clamped = latestStartStudio < openStudio ? openStudio : latestStartStudio;
        next = clamped.setZone(userTimeZone);
      }
      return alignToStepMinutes(next);
    },
    [studioTimeZone, userTimeZone],
  );

  const resetForm = useCallback(() => {
    setSuccess(null);
    setCheckoutSuccess(null);
    setAuthoritativeQuote(null);
    setPaymentError(null);
    setDatafastCheckout(null);
    setDatafastDialogOpen(false);
    setPaypalOrderId(null);
    setPaypalDialogOpen(false);
    setManualDialogOpen(false);
    setManualReference('');
    setTermsAccepted(false);
    checkoutIdempotency.current = null;
    setError(null);
    setSubmitting(false);
    setActiveStep(0);
    const nextService = presetService ?? services[0];
    setForm({
      ...buildInitialForm(),
      serviceOfferingId: nextService?.id ?? '',
      serviceType: nextService?.name ?? '',
      resourceLabels: serviceResourceLabels(nextService),
    });
    setAssignEngineerLater(false);
  }, [presetService, services]);

  useEffect(() => {
    const parsed = DateTime.fromISO(form.startsAt, { zone: userTimeZone });
    const duration = normalizeDurationMinutes(form.durationMinutes);
    if (!parsed.isValid) {
      setAvailabilityStatus('idle');
      setAvailabilityNote(null);
      setAuthoritativeQuote(null);
      return;
    }
    if (!form.serviceOfferingId) {
      setAvailabilityStatus('idle');
      setAvailabilityNote(null);
      setAuthoritativeQuote(null);
      return;
    }
    const controller = new AbortController();
    let didTimeout = false;
    const timeoutId = window.setTimeout(() => {
      didTimeout = true;
      controller.abort();
    }, 8000);
    const startsAtUtc = parsed.toUTC().toISO();
    if (!startsAtUtc) return () => window.clearTimeout(timeoutId);
    setAvailabilityStatus('checking');
    setAvailabilityNote(null);
    setTermsAccepted(false);
    checkoutIdempotency.current = null;
    const url = `${API_BASE_URL}/bookings/public/availability?serviceOfferingId=${encodeURIComponent(form.serviceOfferingId)}&startsAt=${encodeURIComponent(startsAtUtc)}&durationMinutes=${duration}`;
    fetch(url, { signal: controller.signal })
      .then(async (res) => {
        if (!res.ok) throw new Error(`status ${res.status}`);
        const data = (await res.json()) as {
          available?: boolean;
          isAvailable?: boolean;
          reason?: string;
          quote?: PublicBookingQuoteDTO | null;
        } | null;
        setAuthoritativeQuote(data?.quote ?? null);
        const isAvailable = data?.available ?? data?.isAvailable;
        if (isAvailable === false) {
          setAvailabilityStatus('unavailable');
          setAvailabilityNote(data?.reason ?? 'Ese horario ya está reservado.');
        } else if (isAvailable === true) {
          setAvailabilityStatus('available');
          setAvailabilityNote(null);
        } else {
          setAvailabilityStatus('unknown');
          setAvailabilityNote('No pudimos verificar disponibilidad ahora. Reintenta o confirmaremos contigo.');
        }
      })
      .catch((err) => {
        setAuthoritativeQuote(null);
        if (controller.signal.aborted) {
          if (!didTimeout) return;
          setAvailabilityStatus('unknown');
          setAvailabilityNote('La verificación tardó demasiado. Reintenta o coordinamos contigo por WhatsApp.');
          return;
        }
        logger.warn('No se pudo verificar disponibilidad', err);
        setAvailabilityStatus('unknown');
        setAvailabilityNote('No pudimos verificar disponibilidad ahora. Reintenta o confirmaremos contigo.');
      });
    return () => {
      window.clearTimeout(timeoutId);
      controller.abort();
    };
  }, [availabilityNonce, form.durationMinutes, form.serviceOfferingId, form.startsAt, userTimeZone]);

  const validateContactStep = () => {
    if (!form.fullName.trim()) return 'Agrega tu nombre para continuar.';
    const trimmedEmail = form.email.trim();
    if (!trimmedEmail) return 'Necesitamos un correo para confirmarte la reserva.';
    if (!EMAIL_PATTERN.test(trimmedEmail)) {
      return 'Ingresa un correo válido para enviarte la confirmación.';
    }
    return null;
  };

  const validateScheduleStep = () => {
    const selectedService = services.find((service) => service.id === form.serviceOfferingId);
    if (!selectedService) return 'Selecciona un servicio publicado.';
    const parsedStartLocal = DateTime.fromISO(form.startsAt, { zone: userTimeZone });
    if (!parsedStartLocal.isValid) return 'Selecciona una fecha y hora válida.';
    const now = DateTime.now().setZone(userTimeZone);
    if (parsedStartLocal < now.plus({ minutes: 15 })) {
      return 'Elige un horario al menos 15 minutos en el futuro.';
    }
    const durationMinutes = normalizeDurationMinutes(form.durationMinutes);
    const startStudio = parsedStartLocal.setZone(studioTimeZone);
    const openStudio = startStudio.set({ hour: OPEN_HOURS.start, minute: 0, second: 0, millisecond: 0 });
    const closeStudio = startStudio.set({ hour: OPEN_HOURS.end, minute: 0, second: 0, millisecond: 0 });
    const proposedEndStudio = startStudio.plus({ minutes: durationMinutes });
    const openUser = openStudio.setZone(userTimeZone);
    const closeUser = closeStudio.setZone(userTimeZone);
    if (startStudio < openStudio) {
      return `Nuestro horario es ${openStudio.toFormat('HH:mm')} - ${closeStudio.toFormat('HH:mm')} (${studioZoneLabel}). En tu zona (${userZoneLabel}) eso es ${openUser.toFormat('HH:mm')} - ${closeUser.toFormat('HH:mm')}.`;
    }
    if (proposedEndStudio > closeStudio) {
      const remaining = Math.max(0, Math.floor(closeStudio.diff(startStudio, 'minutes').minutes));
      return `La cita debe terminar antes de las ${closeStudio.toFormat('HH:mm')} (${studioZoneLabel}). Con esa hora, el máximo es ${remaining} min.`;
    }
    if (selectedService.requiresEngineer && !assignEngineerLater && !form.engineerId && !form.engineerName.trim()) {
      return 'Selecciona un ingeniero para grabación/mezcla/mastering.';
    }
    if (availabilityStatus === 'unavailable') {
      return availabilityNote ?? 'Ese horario ya está ocupado. Elige otro.';
    }
    return null;
  };

  const goToScheduleStep = () => {
    setError(null);
    const message = validateContactStep();
    if (message) {
      setError(message);
      return;
    }
    setActiveStep(1);
  };

  const goToConfirmStep = () => {
    setError(null);
    const message = validateScheduleStep();
    if (message) {
      setError(message);
      return;
    }
    setActiveStep(2);
  };

  const handleSubmit = async () => {
    setError(null);
    setSuccess(null);

    const contactError = validateContactStep();
    if (contactError) {
      setError(contactError);
      return;
    }
    const scheduleError = validateScheduleStep();
    if (scheduleError) {
      setError(scheduleError);
      return;
    }

    const parsedStartLocal = DateTime.fromISO(form.startsAt, { zone: userTimeZone });
    const durationMinutes = normalizeDurationMinutes(form.durationMinutes);
    if (!parsedStartLocal.isValid) {
      setError('Selecciona una fecha y hora válida.');
      return;
    }

    setSubmitting(true);
    const selectedService = services.find((service) => service.id === form.serviceOfferingId);
    if (!selectedService) {
      setError('El servicio seleccionado ya no está disponible. Elige otro servicio publicado.');
      setSubmitting(false);
      return;
    }
    const engineerPartyId = assignEngineerLater ? null : form.engineerId;
    const engineerName = assignEngineerLater ? null : form.engineerName.trim() || null;
    try {
      const startsAtIso = parsedStartLocal.toUTC().toISO();
      if (!startsAtIso) throw new Error('No pudimos normalizar la hora seleccionada.');
      if (authoritativeQuote) {
        if (!termsAccepted) {
          setError('Acepta la política y el precio de la reserva para crear el checkout del depósito.');
          return;
        }
        const checkoutPayload = {
          pbcFullName: form.fullName.trim(),
          pbcEmail: form.email.trim(),
          pbcPhone: form.phone.trim() || null,
          pbcServiceOfferingId: selectedService.id,
          pbcStartsAt: startsAtIso,
          pbcDurationMinutes: durationMinutes,
          pbcNotes: form.notes.trim() || null,
          pbcEngineerPartyId: engineerPartyId,
          pbcEngineerName: engineerName,
          pbcResourceIds: null,
          pbcTermsAccepted: true,
        };
        const fingerprint = JSON.stringify(checkoutPayload);
        if (checkoutIdempotency.current?.fingerprint !== fingerprint) {
          checkoutIdempotency.current = { fingerprint, key: createBookingIdempotencyKey() };
        }
        const checkout = await Bookings.createPublicCheckout(
          checkoutPayload,
          checkoutIdempotency.current.key,
        );
        storePublicBookingLookupToken(checkout.booking.bookingId, checkout.lookupToken);
        setCheckoutSuccess(checkout);
        setSuccess(checkout.booking);
      } else {
        const dto = await Bookings.createPublic({
          pbFullName: form.fullName.trim(),
          pbEmail: form.email.trim(),
          pbPhone: form.phone.trim() || null,
          pbServiceOfferingId: selectedService.id,
          pbStartsAt: startsAtIso,
          pbDurationMinutes: durationMinutes,
          pbNotes: form.notes.trim() || null,
          pbEngineerPartyId: engineerPartyId,
          pbEngineerName: engineerName,
          pbResourceIds: null,
        });
        setSuccess(dto);
      }
    } catch (err) {
      setError(toFriendlyBookingError(err));
    } finally {
      setSubmitting(false);
    }
  };

  const handleFormSubmit = (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    if (formDisabled) return;
    if (activeStep === 0) {
      goToScheduleStep();
      return;
    }
    if (activeStep === 1) {
      goToConfirmStep();
      return;
    }
    void handleSubmit();
  };

  const bookingWindow = useMemo(() => {
    if (!form.startsAt) return null;
    const startLocal = DateTime.fromISO(form.startsAt, { zone: userTimeZone });
    if (!startLocal.isValid) return null;
    const duration = normalizeDurationMinutes(form.durationMinutes);
    const startStudio = startLocal.setZone(studioTimeZone);
    const openStudio = startStudio.set({ hour: OPEN_HOURS.start, minute: 0, second: 0, millisecond: 0 });
    const closeStudio = startStudio.set({ hour: OPEN_HOURS.end, minute: 0, second: 0, millisecond: 0 });
    const endStudio = startStudio.plus({ minutes: duration });
    const endLocal = endStudio.setZone(userTimeZone);
    return { startLocal, endLocal, startStudio, endStudio, openStudio, closeStudio, duration };
  }, [form.durationMinutes, form.startsAt, studioTimeZone, userTimeZone]);

  const servicePriceLookup = useMemo(() => {
    const map = new Map<string, string>();
    services.forEach((svc) => {
      if (svc.priceCents == null) return;
      const display = `${svc.currency} ${(svc.priceCents / 100).toLocaleString(undefined, { minimumFractionDigits: 0, maximumFractionDigits: 2 })}`;
      const unit = svc.billingUnit ? ` / ${svc.billingUnit}` : '';
      map.set(svc.id, `${display}${unit}`);
    });
    return map;
  }, [services]);
  const estimatePriceLabel = useMemo(() => {
    if (authoritativeQuote?.durationMinutes === normalizeDurationMinutes(form.durationMinutes)) {
      return `${formatMinorAmount(authoritativeQuote.currency, authoritativeQuote.totalMinor)} total · depósito ${formatMinorAmount(authoritativeQuote.currency, authoritativeQuote.depositMinor)}`;
    }
    const svc = services.find((service) => service.id === form.serviceOfferingId);
    if (svc?.priceCents == null) return null;
    const base = `${svc.currency} ${(svc.priceCents / 100).toLocaleString(undefined, { minimumFractionDigits: 0, maximumFractionDigits: 2 })}`;
    if (svc.billingUnit?.toLowerCase().includes('hora')) {
      const hours = Math.max(0.5, normalizeDurationMinutes(form.durationMinutes) / 60);
      const total = (svc.priceCents / 100) * hours;
      return `${svc.currency} ${total.toLocaleString(undefined, { minimumFractionDigits: 0, maximumFractionDigits: 0 })} aprox (${hours.toFixed(1)}h)`;
    }
    return `${base}${svc.billingUnit ? ` / ${svc.billingUnit}` : ''}`;
  }, [authoritativeQuote, form.durationMinutes, form.serviceOfferingId, services]);
  const selectedPrice = servicePriceLookup.get(form.serviceOfferingId);

  const priceBanner = useMemo(() => {
    if (!form.serviceType) return null;
    const estimate = estimatePriceLabel ?? selectedPrice;
    if (!estimate) return null;
    return `Estimado para ${form.serviceType}: ${estimate}`;
  }, [estimatePriceLabel, form.serviceType, selectedPrice]);

  const minStartDate = useMemo(() => {
    const nowStudio = DateTime.now().setZone(studioTimeZone);
    const openToday = nowStudio.set({ hour: OPEN_HOURS.start, minute: 0, second: 0, millisecond: 0 });
    const closeToday = nowStudio.set({ hour: OPEN_HOURS.end, minute: 0, second: 0, millisecond: 0 });
    let candidate = nowStudio.plus({ minutes: START_STEP_MINUTES });
    if (candidate < openToday) candidate = openToday;
    if (candidate > closeToday) candidate = openToday.plus({ days: 1 });
    return alignToStepMinutes(candidate.setZone(userTimeZone));
  }, [studioTimeZone, userTimeZone]);

  const minStartValue = useMemo(() => toLocalInputValue(minStartDate.toJSDate()), [minStartDate]);
  const minStartValueForInput = useMemo(
    () => toLocalInputValue(alignToStepMinutes(minStartDate.startOf('day')).toJSDate()),
    [minStartDate],
  );

  useEffect(() => {
    const current = DateTime.fromISO(form.startsAt, { zone: userTimeZone });
    if (!current.isValid || current < minStartDate) {
      setForm((prev) => ({ ...prev, startsAt: minStartValue }));
      return;
    }
    const aligned = alignToStepMinutes(current);
    if (!aligned.equals(current)) {
      setForm((prev) => ({ ...prev, startsAt: toLocalInputValue(aligned.toJSDate()) }));
    }
  }, [form.startsAt, minStartDate, minStartValue, userTimeZone]);
  const formattedStart = useMemo(() => {
    if (!bookingWindow) return null;
    return bookingWindow.startLocal.toLocaleString(DateTime.DATETIME_MED_WITH_WEEKDAY);
  }, [bookingWindow]);
  const suggestedRooms = useMemo(
    () => serviceResourceLabels(services.find((service) => service.id === form.serviceOfferingId)),
    [form.serviceOfferingId, services],
  );
  const selectedServiceRequiresEngineer =
    services.find((service) => service.id === form.serviceOfferingId)?.requiresEngineer ?? false;

  const buildSummary = useCallback(
    (booking?: BookingDTO | null) => {
      const bookingWithAliases = booking as BookingWithAliases | undefined;
      const successStartIso =
        bookingWithAliases?.pbStartsAt ??
        bookingWithAliases?.cbStartsAt ??
        bookingWithAliases?.ubStartsAt ??
        booking?.startsAt;
      const startLabel =
        successStartIso && typeof successStartIso === 'string'
          ? DateTime.fromISO(successStartIso).setZone(userTimeZone).toLocaleString(DateTime.DATETIME_MED_WITH_WEEKDAY)
          : formattedStart ?? form.startsAt;
      const duration =
        (booking?.startsAt && booking?.endsAt
          ? Math.max(
              30,
              Math.round(DateTime.fromISO(booking.endsAt).diff(DateTime.fromISO(booking.startsAt), 'minutes').minutes),
            )
          : null) ?? bookingWithAliases?.pbDurationMinutes ?? form.durationMinutes;
      const engineerName = booking?.engineerName ?? bookingWithAliases?.pbEngineerName ?? form.engineerName;
      const roomsFromBooking =
        booking?.resources?.map((r) => r.brRoomName).filter((name): name is string => Boolean(name)) ??
        (form.resourceLabels.length ? form.resourceLabels : suggestedRooms);
      const price = estimatePriceLabel ?? selectedPrice ?? 'Por confirmar';
      const lines = [
        presetConfig ? `Reserva TDF - ${presetConfig.eyebrow}` : 'Reserva TDF',
        `Servicio: ${booking?.serviceType ?? form.serviceType}`,
        `Inicio: ${startLabel}`,
        `Duración: ${duration} min`,
        `Precio ref: ${price}`,
      ];
      if (roomsFromBooking.length) {
        lines.push(`Salas: ${roomsFromBooking.join(' + ')}`);
      }
      if (engineerName) {
        lines.push(`Ingeniero: ${engineerName}`);
      }
      return lines.join('\n');
    },
    [estimatePriceLabel, form.durationMinutes, form.engineerName, form.resourceLabels, form.serviceType, form.startsAt, formattedStart, presetConfig, selectedPrice, suggestedRooms, userTimeZone],
  );

  const copySummary = useCallback(
    async (booking?: BookingDTO | null) => {
      try {
        const summary = buildSummary(booking);
        await navigator.clipboard.writeText(summary);
        setSnackbar({ open: true, message: 'Resumen copiado' });
      } catch {
        setSnackbar({ open: true, message: 'No pudimos copiar el resumen.' });
      }
    },
    [buildSummary],
  );

  const firstAvailable = useCallback(
    (dayOffset: number) => {
      return resolveFirstAvailableShortcut({ dayOffset, studioTimeZone, userTimeZone });
    },
    [studioTimeZone, userTimeZone],
  );

  useEffect(() => {
    setForm((prev) => {
      if (
        prev.resourceLabels.length === suggestedRooms.length
        && prev.resourceLabels.every((room, index) => room === suggestedRooms[index])
      ) return prev;
      return { ...prev, resourceLabels: suggestedRooms };
    });
  }, [suggestedRooms]);

  const maxDurationUntilClose = useMemo(() => {
    if (!bookingWindow) return null;
    const minutes = Math.floor(bookingWindow.closeStudio.diff(bookingWindow.startStudio, 'minutes').minutes);
    if (minutes <= 0) return 0;
    return minutes;
  }, [bookingWindow]);

  const engineerValue =
    engineers.find((opt) => opt.peId === form.engineerId) ??
    (form.engineerName ? { peId: -1, peName: form.engineerName } : null);

  const exactEngineerIdByName = useMemo(() => {
    const lookup = new Map<string, number | null>();
    engineers.forEach((engineer) => {
      const normalized = engineer.peName.trim().toLowerCase();
      if (!normalized) return;
      const current = lookup.get(normalized);
      if (current === undefined) {
        lookup.set(normalized, engineer.peId);
        return;
      }
      if (current !== engineer.peId) {
        lookup.set(normalized, null);
      }
    });
    return lookup;
  }, [engineers]);

  const clearSavedProfile = () => {
    setRememberProfile(false);
    setForm((prev) => ({
      ...prev,
      fullName: '',
      email: '',
      phone: '',
    }));
  };

  const outOfHours = useMemo(() => {
    if (!bookingWindow) return null;
    const { startStudio, endStudio, openStudio, closeStudio } = bookingWindow;
    if (startStudio < openStudio) {
      return `Abrimos a las ${openStudio.toFormat('HH:mm')} (${studioZoneLabel}). En tu zona: ${openStudio
        .setZone(userTimeZone)
        .toFormat('HH:mm')} (${userZoneLabel}).`;
    }
    if (endStudio > closeStudio) {
      return `La duración seleccionada pasa el cierre (${closeStudio.toFormat('HH:mm')} ${studioZoneLabel}). Ajusta minutos u horario.`;
    }
    return null;
  }, [bookingWindow, studioZoneLabel, userTimeZone, userZoneLabel]);

  const durationLimitLabel = useMemo(() => {
    if (maxDurationUntilClose == null) return null;
    const closeLabel = bookingWindow?.closeStudio
      ? `${bookingWindow.closeStudio.toFormat('HH:mm')} (${studioZoneLabel})`
      : `${OPEN_HOURS.end}:00`;
    if (maxDurationUntilClose <= 0) {
      return `Elige otra hora: el cierre (${closeLabel}) es antes de este inicio.`;
    }
    return `Máximo ${maxDurationUntilClose} min con la hora elegida (cierre ${closeLabel}).`;
  }, [bookingWindow?.closeStudio, maxDurationUntilClose, studioZoneLabel]);

  const availabilityHelperText = useMemo(() => {
    const open = bookingWindow?.openStudio
      ?? DateTime.now()
        .setZone(studioTimeZone)
        .set({ hour: OPEN_HOURS.start, minute: 0, second: 0, millisecond: 0 });
    const close = bookingWindow?.closeStudio
      ?? DateTime.now()
        .setZone(studioTimeZone)
        .set({ hour: OPEN_HOURS.end, minute: 0, second: 0, millisecond: 0 });
    const openUser = open.setZone(userTimeZone);
    const closeUser = close.setZone(userTimeZone);
    const base = `Horario del estudio: ${open.toFormat('HH:mm')} - ${close.toFormat('HH:mm')} (${studioZoneLabel}). Tu zona: ${openUser.toFormat('HH:mm')} - ${closeUser.toFormat('HH:mm')} (${userZoneLabel}).`;
    if (availabilityStatus === 'checking') return `${base} Verificando disponibilidad…`;
    if (availabilityStatus === 'unavailable') return `${base} Ese horario parece ocupado.`;
    if (availabilityStatus === 'available') return `${base} Disponible.`;
    if (availabilityNote) return `${base} ${availabilityNote}`;
    return base;
  }, [availabilityNote, availabilityStatus, bookingWindow?.closeStudio, bookingWindow?.openStudio, studioTimeZone, studioZoneLabel, userZoneLabel, userTimeZone]);

  const timeWarnings = useMemo(() => {
    const warnings: string[] = [];
    const parsedStart = DateTime.fromISO(form.startsAt || '', { zone: userTimeZone });
    if (!parsedStart.isValid) {
      warnings.push('Selecciona una fecha y hora válidas para verificar disponibilidad.');
      return warnings;
    }
    const minutesAway = parsedStart.diff(DateTime.now().setZone(userTimeZone), 'minutes').minutes;
    if (minutesAway < 90) warnings.push('Agenda con al menos 90 minutos de anticipación para coordinar recursos.');
    const startStudio = parsedStart.setZone(studioTimeZone);
    if (startStudio.hour < OPEN_HOURS.start || startStudio.hour >= OPEN_HOURS.end) {
      warnings.push(`Horario del estudio: ${OPEN_HOURS.start}:00 - ${OPEN_HOURS.end}:00 (${studioZoneLabel}).`);
    }
    if (availabilityStatus === 'unavailable') warnings.push(availabilityNote ?? 'Ese horario parece ocupado.');
    return warnings;
  }, [availabilityNote, availabilityStatus, form.startsAt, studioTimeZone, studioZoneLabel, userTimeZone]);

  const computeMaxDurationForStart = useCallback(
    (startValue: string) => {
      const startLocal = DateTime.fromISO(startValue, { zone: userTimeZone });
      if (!startLocal.isValid) return null;
      const startStudio = startLocal.setZone(studioTimeZone);
      const closeStudio = startStudio.set({ hour: OPEN_HOURS.end, minute: 0, second: 0, millisecond: 0 });
      const diffMinutes = Math.floor(closeStudio.diff(startStudio, 'minutes').minutes);
      return diffMinutes > 0 ? diffMinutes : 0;
    },
    [studioTimeZone, userTimeZone],
  );

  const clampDurationForStart = useCallback(
    (startValue: string, desiredMinutes: number) => {
      const max = computeMaxDurationForStart(startValue);
      if (max == null || max <= 0) return desiredMinutes;
      return Math.max(30, Math.min(desiredMinutes, max));
    },
    [computeMaxDurationForStart],
  );

  const suggestedSlots = useMemo(() => {
    const slots: { value: string; label: string; helper: string }[] = [];
    const duration = normalizeDurationMinutes(form.durationMinutes);
    const nowUser = DateTime.now().setZone(userTimeZone).plus({ minutes: 15 });
    const baseDay = DateTime.fromISO(form.startsAt || '', { zone: userTimeZone });
    const day = baseDay.isValid ? baseDay.startOf('day') : nowUser.startOf('day');

    const openStudio = day
      .setZone(studioTimeZone)
      .set({ hour: OPEN_HOURS.start, minute: 0, second: 0, millisecond: 0 });
    const closeStudio = day
      .setZone(studioTimeZone)
      .set({ hour: OPEN_HOURS.end, minute: 0, second: 0, millisecond: 0 });
    let cursorUser = openStudio.setZone(userTimeZone);
    if (cursorUser < nowUser && cursorUser.hasSame(nowUser, 'day')) {
      cursorUser = nowUser.startOf('minute');
    }

    const pushSlot = (dtUser: DateTime) => {
      if (!dtUser.isValid) return;
      const startStudio = dtUser.setZone(studioTimeZone);
      const endStudio = startStudio.plus({ minutes: duration });
      if (startStudio < openStudio || endStudio > closeStudio) return;
      const helper = `${dtUser.toFormat('EEE dd HH:mm')} (${userZoneLabel}) · Estudio ${startStudio.toFormat('HH:mm')} (${studioZoneLabel})`;
      slots.push({
        value: toLocalInputValue(dtUser.toJSDate()),
        label: dtUser.toFormat('HH:mm'),
        helper,
      });
    };

    let guard = 0;
    while (guard < 48) {
      pushSlot(cursorUser);
      cursorUser = cursorUser.plus({ minutes: 30 });
      guard += 1;
      if (cursorUser.plus({ minutes: duration }) > closeStudio.setZone(userTimeZone)) break;
    }

    return slots.slice(0, 12);
  }, [form.durationMinutes, form.startsAt, studioTimeZone, studioZoneLabel, userTimeZone, userZoneLabel]);

  const checkoutLookupToken = useMemo(() => {
    if (!checkoutSuccess) return null;
    return checkoutSuccess.lookupToken
      ?? loadPublicBookingLookupToken(checkoutSuccess.booking.bookingId);
  }, [checkoutSuccess]);
  const datafastReturnUrl = useMemo(() => {
    if (!checkoutSuccess || typeof window === 'undefined') return '';
    return new URL(
      `/reservas/orden/${checkoutSuccess.booking.bookingId}`,
      window.location.origin,
    ).toString();
  }, [checkoutSuccess]);

  const handleDatafastDeposit = useCallback(async () => {
    if (!checkoutSuccess || !checkoutLookupToken) {
      setPaymentError('No encontramos el acceso seguro de esta orden. Crea una nueva reserva.');
      return;
    }
    setPaymentBusy(true);
    setPaymentError(null);
    try {
      const providerCheckout = await Bookings.createPublicDatafastCheckout(
        checkoutSuccess.booking.bookingId,
        checkoutLookupToken,
      );
      setDatafastCheckout(providerCheckout);
      setDatafastDialogOpen(true);
      setDatafastWidgetKey((current) => current + 1);
    } catch {
      setPaymentError('No pudimos iniciar Datafast. La reserva sigue sin pago confirmado.');
    } finally {
      setPaymentBusy(false);
    }
  }, [checkoutLookupToken, checkoutSuccess]);

  const handlePaypalDeposit = useCallback(async () => {
    if (!checkoutSuccess || !checkoutLookupToken) {
      setPaymentError('No encontramos el acceso seguro de esta orden. Crea una nueva reserva.');
      return;
    }
    if (!paypalClientId) {
      setPaymentError('PayPal no está disponible en este navegador. La reserva sigue sin pago.');
      return;
    }
    setPaymentBusy(true);
    setPaymentError(null);
    try {
      const providerOrder = await Bookings.createPublicPaypalOrder(
        checkoutSuccess.booking.bookingId,
        checkoutLookupToken,
      );
      setPaypalOrderId(providerOrder.pcPaypalOrderId);
      setPaypalDialogOpen(true);
    } catch {
      setPaymentError('No pudimos crear la orden PayPal. La reserva sigue sin pago confirmado.');
    } finally {
      setPaymentBusy(false);
    }
  }, [checkoutLookupToken, checkoutSuccess, paypalClientId]);

  const handleManualDeposit = useCallback(async () => {
    if (!checkoutSuccess || !checkoutLookupToken) {
      setPaymentError('No encontramos el acceso seguro de esta orden. Crea una nueva reserva.');
      return;
    }
    setPaymentBusy(true);
    setPaymentError(null);
    try {
      const updated = await Bookings.selectPublicManualPayment(
        checkoutSuccess.booking.bookingId,
        checkoutLookupToken,
      );
      setCheckoutSuccess({ ...updated, lookupToken: checkoutLookupToken });
      setManualDialogOpen(true);
    } catch {
      setPaymentError('No pudimos seleccionar transferencia. La reserva sigue sin pago confirmado.');
    } finally {
      setPaymentBusy(false);
    }
  }, [checkoutLookupToken, checkoutSuccess]);

  const handleManualEvidenceSubmit = useCallback(async () => {
    if (!checkoutSuccess || !checkoutLookupToken) return;
    const reference = manualReference.trim();
    if (reference.length < 3 || reference.length > 120) {
      setPaymentError('Ingresa una referencia bancaria de 3 a 120 caracteres.');
      return;
    }
    setPaymentBusy(true);
    setPaymentError(null);
    try {
      const updated = await Bookings.submitPublicManualEvidence(
        checkoutSuccess.booking.bookingId,
        reference,
        checkoutLookupToken,
      );
      setCheckoutSuccess({ ...updated, lookupToken: checkoutLookupToken });
      setManualDialogOpen(false);
      setManualReference('');
      setSnackbar({
        open: true,
        message: 'Referencia enviada para revisión. El depósito todavía no está confirmado.',
      });
    } catch {
      setPaymentError('No pudimos enviar la referencia. No se confirmó ningún pago.');
    } finally {
      setPaymentBusy(false);
    }
  }, [checkoutLookupToken, checkoutSuccess, manualReference]);

  useEffect(() => {
    if (!datafastDialogOpen || !datafastCheckout || typeof window === 'undefined') return;
    if (datafastFormRef.current) datafastFormRef.current.innerHTML = '';
    window.wpwlOptions = { locale: 'es', style: 'card' };
    const script = document.createElement('script');
    script.src = datafastCheckout.dcWidgetUrl;
    script.async = true;
    script.onerror = () => setPaymentError(
      'No se pudo cargar el formulario Datafast. No se confirmó ningún pago.',
    );
    document.body.appendChild(script);
    return () => script.remove();
  }, [datafastCheckout, datafastDialogOpen, datafastWidgetKey]);

  useEffect(() => {
    const paypalOffered = checkoutSuccess?.paymentMethods?.includes('paypal') ?? false;
    if (!paypalOffered || !paypalClientId || typeof window === 'undefined') return;
    if (window.paypal) {
      setPaypalReady(true);
      return;
    }
    const script = document.createElement('script');
    script.src = `https://www.paypal.com/sdk/js?client-id=${encodeURIComponent(paypalClientId)}&currency=${encodeURIComponent(checkoutSuccess?.quote.currency ?? 'USD')}`;
    script.async = true;
    script.onload = () => setPaypalReady(true);
    script.onerror = () => setPaymentError(
      'No se pudo cargar PayPal. La reserva continúa sin pago confirmado.',
    );
    document.body.appendChild(script);
    return () => script.remove();
  }, [checkoutSuccess?.paymentMethods, checkoutSuccess?.quote.currency, paypalClientId]);

  useEffect(() => {
    if (
      !paypalDialogOpen
      || !paypalReady
      || !paypalOrderId
      || !checkoutSuccess
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
          const updated = await Bookings.capturePublicPaypalOrder(
            checkoutSuccess.booking.bookingId,
            paypalOrderId,
            checkoutLookupToken,
          );
          setCheckoutSuccess({ ...updated, lookupToken: checkoutLookupToken });
          setSuccess(updated.booking);
          setPaypalDialogOpen(false);
          setPaypalOrderId(null);
          setSnackbar({
            open: true,
            message: updated.paymentStatus === 'paid'
              ? 'PayPal verificó el depósito en el servidor.'
              : 'PayPal respondió, pero el depósito todavía no está confirmado.',
          });
        } catch {
          setPaymentError('No pudimos verificar la captura PayPal. No mostramos el depósito como pagado.');
        } finally {
          setPaymentBusy(false);
        }
      },
      onCancel: () => setPaymentError('Cancelaste PayPal. La reserva continúa sin pago.'),
      onError: () => setPaymentError('PayPal no completó la operación. La reserva continúa sin pago.'),
    });
    void buttons.render(paypalButtonRef.current);
    return () => buttons.close?.();
  }, [checkoutLookupToken, checkoutSuccess, paypalDialogOpen, paypalOrderId, paypalReady]);

  if (success) {
    const successWithAliases = success as BookingWithAliases | null;
    const successStartIso =
      successWithAliases?.pbStartsAt ?? successWithAliases?.cbStartsAt ?? successWithAliases?.ubStartsAt ?? success.startsAt;
    const successStartLabel =
      successStartIso && typeof successStartIso === 'string'
        ? DateTime.fromISO(successStartIso).setZone(userTimeZone).toLocaleString(DateTime.DATETIME_MED_WITH_WEEKDAY)
        : formattedStart ?? form.startsAt;
    const successDuration =
      (success.startsAt && success.endsAt
        ? Math.max(
            30,
            Math.round(
              DateTime.fromISO(success.endsAt).diff(DateTime.fromISO(success.startsAt), 'minutes').minutes,
            ),
          )
        : null) ?? successWithAliases?.pbDurationMinutes ?? form.durationMinutes;
    const successEngineer = success.engineerName ?? successWithAliases?.pbEngineerName ?? form.engineerName;
    const successRooms =
      success.resources?.map((r) => r.brRoomName).filter((name): name is string => Boolean(name)) ??
      (form.resourceLabels.length ? form.resourceLabels : []);
    const calendarUrl =
      successStartIso && successDuration
        ? buildGoogleCalendarUrl(
            success.serviceType ?? form.serviceType,
            successStartIso,
            successDuration,
            successRooms.join(' · ') || 'TDF Records',
            'Reserva generada desde el portal público.',
          )
        : null;
    const icsUrl =
      successStartIso && successDuration
        ? buildIcsDataUrl(
            success.serviceType ?? form.serviceType,
            successStartIso,
            successDuration,
            successRooms.join(' · ') || 'TDF Records',
            'Reserva generada desde el portal público.',
            `tdf-booking-${success.bookingId}@tdf`,
          )
        : null;
    const depositPaid = checkoutSuccess?.paymentStatus === 'paid';
    const depositProcessing = checkoutSuccess?.paymentStatus === 'processing';
    const paymentMethods = checkoutSuccess?.paymentMethods ?? [];
    const manualPayment = checkoutSuccess?.manualPayment;

    return (
      <Box sx={{ minHeight: '80vh', display: 'flex', alignItems: 'center', justifyContent: 'center', py: 4 }}>
        <Card
          sx={{
            maxWidth: 880,
            width: '100%',
            borderRadius: 3,
            boxShadow: '0 18px 72px rgba(15,17,24,0.26)',
            border: '1px solid rgba(255,255,255,0.08)',
            background: 'linear-gradient(135deg, rgba(255,255,255,0.02), rgba(30,64,175,0.06))',
          }}
        >
          <CardContent sx={{ p: { xs: 3, md: 5 } }}>
            <Stack spacing={2.5}>
              <Stack spacing={0.6}>
                <Typography variant="overline" color="text.secondary">
                  {pageEyebrow}
                </Typography>
                <Typography variant="h4" fontWeight={800}>
                  {checkoutSuccess
                    ? depositPaid
                      ? 'Depósito verificado · reserva confirmada'
                      : depositProcessing
                        ? 'Depósito en verificación'
                        : 'Orden creada · depósito pendiente'
                    : 'Reserva enviada'}
                </Typography>
                <Typography variant="body1" color="text.secondary">
                  {checkoutSuccess
                    ? depositPaid
                      ? 'El servidor verificó el depósito. El saldo y la prestación del servicio permanecen en estados separados.'
                      : depositProcessing
                        ? 'El proveedor todavía no confirmó el resultado. Esta pantalla no representa un pago exitoso.'
                        : 'El horario está retenido temporalmente, pero todavía no está pagado ni confirmado. Solo una verificación del proveedor puede confirmar el depósito.'
                    : 'Revisa tu correo para la confirmación. Si necesitas ajustar horario o salas, responde al correo o escríbenos por WhatsApp y lo coordinamos contigo.'}
                </Typography>
              </Stack>

              <Grid container spacing={2}>
                <Grid item xs={12}>
                  <Alert severity={checkoutSuccess ? (depositPaid ? 'success' : 'info') : 'success'}>
                    {checkoutSuccess
                      ? depositPaid ? 'Depósito pagado y verificado' : depositProcessing ? 'Pago en verificación' : 'Orden creada, pago pendiente'
                      : 'Reserva creada'}. ID{' '}
                    <strong>{success.bookingId}</strong> · Servicio:{' '}
                    <strong>{success.serviceType ?? form.serviceType}</strong>
                    {checkoutSuccess && (
                      <>
                        {' '}· Depósito: <strong>{formatMinorAmount(checkoutSuccess.quote.currency, checkoutSuccess.quote.depositMinor)}</strong>
                      </>
                    )}
                  </Alert>
                </Grid>
                <Grid item xs={12}>
                  <Card
                    variant="outlined"
                    sx={{
                      bgcolor: 'rgba(255,255,255,0.02)',
                      borderColor: 'rgba(255,255,255,0.08)',
                    }}
                  >
                    <CardContent>
                      <Typography variant="subtitle2" color="text.secondary" gutterBottom>
                        Resumen
                      </Typography>
                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                        <Chip label={`Fecha: ${successStartLabel}`} size="small" />
                        <Chip label={`Duración: ${successDuration} min`} size="small" />
                        <Chip label={`Servicio: ${success.serviceType ?? form.serviceType}`} size="small" />
                        {successRooms.length > 0 && <Chip label={`Salas: ${successRooms.join(' + ')}`} size="small" />}
                        {successEngineer && <Chip label={`Ingeniero: ${successEngineer}`} size="small" />}
                        {checkoutSuccess && (
                          <Chip
                            label={`Saldo posterior: ${formatMinorAmount(checkoutSuccess.quote.currency, checkoutSuccess.quote.balanceMinor)}`}
                            size="small"
                          />
                        )}
                      </Stack>
                    </CardContent>
                  </Card>
                </Grid>
                {checkoutSuccess && !depositPaid && (
                  <Grid item xs={12}>
                    <Card variant="outlined">
                      <CardContent>
                        <Stack spacing={1.5}>
                          <Typography variant="subtitle1" fontWeight={800}>Pagar depósito</Typography>
                          <Typography variant="body2" color="text.secondary">
                            Elige únicamente un método habilitado por el servidor. Abrir un proveedor no confirma el pago.
                          </Typography>
                          {paymentError && <Alert severity="warning">{paymentError}</Alert>}
                          {manualPayment?.status === 'submitted' && (
                            <Alert severity="info" variant="outlined">
                              Referencia recibida. Permanece pendiente hasta que una persona autorizada la compare con el estado bancario.
                            </Alert>
                          )}
                          {manualPayment?.status === 'under_review' && (
                            <Alert severity="info" variant="outlined">
                              Transferencia en revisión. Este estado no significa pago confirmado.
                            </Alert>
                          )}
                          {manualPayment?.status === 'rejected' && (
                            <Alert severity="warning" variant="outlined">
                              La evidencia anterior fue rechazada. Verifica la referencia y vuelve a enviarla.
                            </Alert>
                          )}
                          {paymentMethods.length === 0 && (
                            <Alert severity="info" variant="outlined">
                              No hay un rail en línea habilitado para esta orden. El horario sigue solamente en retención temporal.
                            </Alert>
                          )}
                          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                            {paymentMethods.includes('datafast') && (
                              <Button
                                variant="contained"
                                disabled={paymentBusy}
                                onClick={() => void handleDatafastDeposit()}
                              >
                                Pagar con tarjeta · Datafast
                              </Button>
                            )}
                            {paymentMethods.includes('paypal') && paypalClientId && (
                              <Button
                                variant="outlined"
                                disabled={paymentBusy}
                                onClick={() => void handlePaypalDeposit()}
                              >
                                Pagar con PayPal
                              </Button>
                            )}
                            {paymentMethods.includes('bank_transfer') && (
                              <Button
                                variant="outlined"
                                disabled={paymentBusy}
                                onClick={() => void handleManualDeposit()}
                              >
                                Registrar transferencia
                              </Button>
                            )}
                          </Stack>
                        </Stack>
                      </CardContent>
                    </Card>
                  </Grid>
                )}
                <Grid item xs={12}>
                  <Alert severity="info" variant="outlined">
                    <Typography variant="subtitle2" fontWeight={800} gutterBottom>
                      Qué sigue
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      {checkoutSuccess
                        ? depositPaid
                          ? '• Depósito verificado por el servidor; revisa el saldo antes de la sesión.'
                          : `• Retención hasta ${DateTime.fromISO(checkoutSuccess.holdExpiresAt).setZone(userTimeZone).toLocaleString(DateTime.DATETIME_MED)}; no constituye pago.`
                        : '• Te confirmamos por correo (y te contactamos si necesitamos ajustar recursos).'}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      {checkoutSuccess
                        ? paymentMethods.length > 0
                          ? '• El estado cambia solo después de una verificación del servidor.'
                          : '• Datafast/PayPal permanecen ocultos mientras el servidor no habilite un rail real.'
                        : '• Llega 10 minutos antes para hacer check-in y preparar la sala.'}
                    </Typography>
                    <Typography variant="body2" color="text.secondary">
                      • Si vas tarde o necesitas mover el horario, escríbenos por WhatsApp.
                    </Typography>
                  </Alert>
                </Grid>
                <Grid item xs={12}>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                    {checkoutSuccess && (
                      <Button
                        variant="outlined"
                        component={RouterLink}
                        to={`/reservas/orden/${success.bookingId}`}
                        size="medium"
                      >
                        Seguir esta orden
                      </Button>
                    )}
                    <Button variant="contained" size="medium" onClick={resetForm}>
                      Crear otra reserva
                    </Button>
                    <Button
                      variant="text"
                      size="medium"
                      onClick={() => {
                        void copySummary(success);
                      }}
                    >
                      Copiar resumen
                    </Button>
                    <Button variant="text" size="medium" href={STUDIO_MAP_URL} target="_blank" rel="noreferrer">
                      Cómo llegar
                    </Button>
                    <Button variant="text" size="medium" href={STUDIO_WHATSAPP_URL} target="_blank" rel="noreferrer">
                      WhatsApp
                    </Button>
                    {calendarUrl && (
                      <Button variant="text" size="medium" href={calendarUrl} target="_blank" rel="noreferrer">
                        Agregar a Google Calendar
                      </Button>
                    )}
                    {icsUrl && (
                      <Button
                        variant="text"
                        size="medium"
                        href={icsUrl}
                        download={`tdf-reserva-${success.bookingId}.ics`}
                      >
                        Descargar .ics
                      </Button>
                    )}
                  </Stack>
                </Grid>
              </Grid>
            </Stack>
          </CardContent>
        </Card>
        <Dialog
          open={datafastDialogOpen}
          onClose={() => setDatafastDialogOpen(false)}
          maxWidth="xs"
          fullWidth
        >
          <DialogTitle>Pagar depósito con Datafast</DialogTitle>
          <DialogContent dividers>
            <Stack spacing={1.5}>
              <Alert severity="info" variant="outlined">
                El formulario es alojado por el proveedor. Al volver, TDF consultará el estado en el servidor antes de confirmar.
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
            <Button onClick={() => setDatafastWidgetKey((current) => current + 1)}>Reintentar carga</Button>
            <Button onClick={() => setDatafastDialogOpen(false)} color="inherit">Cerrar</Button>
          </DialogActions>
        </Dialog>
        <Dialog
          open={paypalDialogOpen}
          onClose={() => setPaypalDialogOpen(false)}
          maxWidth="xs"
          fullWidth
        >
          <DialogTitle>Pagar depósito con PayPal</DialogTitle>
          <DialogContent dividers>
            <Stack spacing={1.5}>
              <Alert severity="info" variant="outlined">
                Aprobar en PayPal no es confirmación. TDF capturará y verificará importe, moneda, comercio y referencia en el servidor.
              </Alert>
              {paymentError && <Alert severity="warning">{paymentError}</Alert>}
              <Box ref={paypalButtonRef} sx={{ minHeight: 48 }} />
            </Stack>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => setPaypalDialogOpen(false)} color="inherit">Cerrar</Button>
          </DialogActions>
        </Dialog>
        <Dialog
          open={manualDialogOpen}
          onClose={() => { if (!paymentBusy) setManualDialogOpen(false); }}
          maxWidth="xs"
          fullWidth
        >
          <DialogTitle>Registrar transferencia bancaria</DialogTitle>
          <DialogContent dividers>
            <Stack spacing={1.5}>
              <Alert severity="warning" variant="outlined">
                Enviar una referencia no confirma el depósito. TDF debe verificar el movimiento bancario y el importe exacto antes de confirmar la reserva.
              </Alert>
              <Typography variant="body2" color="text.secondary">
                Usa únicamente las instrucciones bancarias oficiales que TDF te haya proporcionado. No incluyas claves, números completos de cuenta ni datos de tarjeta.
              </Typography>
              <TextField
                label="Referencia o comprobante bancario"
                value={manualReference}
                onChange={(event) => setManualReference(event.target.value)}
                inputProps={{ maxLength: 120 }}
                helperText="3–120 caracteres. La referencia queda protegida para revisión financiera."
                autoComplete="off"
                fullWidth
              />
              {paymentError && <Alert severity="warning">{paymentError}</Alert>}
            </Stack>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => setManualDialogOpen(false)} disabled={paymentBusy} color="inherit">
              Cerrar
            </Button>
            <Button
              variant="contained"
              onClick={() => void handleManualEvidenceSubmit()}
              disabled={paymentBusy || manualReference.trim().length < 3}
            >
              Enviar para revisión
            </Button>
          </DialogActions>
        </Dialog>
      </Box>
    );
  }

  return (
    <Box sx={{ minHeight: '80vh', display: 'flex', alignItems: 'center', justifyContent: 'center', py: 4 }}>
      <Card
        sx={{
          maxWidth: 880,
          width: '100%',
          borderRadius: 3,
          boxShadow: '0 18px 72px rgba(15,17,24,0.26)',
          border: '1px solid rgba(255,255,255,0.08)',
          background: 'linear-gradient(135deg, rgba(255,255,255,0.02), rgba(30,64,175,0.06))',
        }}
      >
        <CardContent sx={{ p: { xs: 3, md: 5 } }}>
          <Stack spacing={2.5}>
            <Stack spacing={0.6}>
              <Typography variant="overline" color="text.secondary">
                {pageEyebrow}
              </Typography>
              <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                <Typography variant="h4" fontWeight={800}>
                  {pageTitle}
                </Typography>
                {bookingStatusChip}
              </Stack>
              <Typography variant="body1" color="text.secondary">
                {pageDescription}
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Horario del estudio: <strong>{studioZoneLabel}</strong>. Tu zona: <strong>{userZoneLabel}</strong>.
              </Typography>
              <Typography variant="body2" color="text.secondary">
                {authoritativeQuote ? (
                  <>Precio y depósito calculados por el servidor en <strong>{authoritativeQuote.currency}</strong>.</>
                ) : (
                  <>Precios de referencia en <strong>{studioCurrency}</strong>; confirmamos el total contigo antes de agendar.</>
                )}
              </Typography>
              {priceBanner && (
                <Alert severity="info" variant="outlined">
                  {priceBanner}
                </Alert>
              )}
              {bookingReadinessNote && (
                <Alert severity="info" variant="outlined">
                  {bookingReadinessNote}
                </Alert>
              )}
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                {introChips.map((label) => (
                  <Chip key={label} label={label} size="small" variant="outlined" />
                ))}
              </Stack>
              <Card
                variant="outlined"
                sx={{
                  mt: 1,
                  borderColor: 'rgba(255,255,255,0.12)',
                  bgcolor: 'rgba(255,255,255,0.02)',
                }}
              >
                <CardContent sx={{ py: 1.5, px: 2 }}>
                  <Stack
                    direction={{ xs: 'column', sm: 'row' }}
                    spacing={1}
                    alignItems={{ xs: 'flex-start', sm: 'center' }}
                    justifyContent="space-between"
                    useFlexGap
                    flexWrap="wrap"
                  >
                    <Stack spacing={0.3}>
                      <Typography variant="subtitle2">¿Ya tienes cuenta?</Typography>
                      <Typography variant="body2" color="text.secondary">
                        Inicia sesión y saltamos tus datos para esta reserva. Si no tienes cuenta, puedes crearla rápido.
                      </Typography>
                    </Stack>
                    <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                      <Button size="small" variant="outlined" component={RouterLink} to={loginPath}>
                        Iniciar sesión
                      </Button>
                      <Button size="small" variant="text" component={RouterLink} to={signupPath}>
                        Crear cuenta
                      </Button>
                      {session && (
                        <Chip
                          label={`Conectado como ${session.displayName}`}
                          color="primary"
                          onDelete={logout}
                          variant="outlined"
                          sx={{ borderRadius: 999 }}
                        />
                      )}
                    </Stack>
                  </Stack>
                </CardContent>
              </Card>
            </Stack>

            <Grid container spacing={2}>
              <Grid item xs={12} md={4}>
                <Stack spacing={1.2}>
                  <Stack direction="row" alignItems="center" spacing={1}>
                    <PersonIcon color="primary" fontSize="small" />
                    <Typography variant="subtitle2" color="text.secondary">
                      {contactTitle}
                    </Typography>
                  </Stack>
                  <Typography variant="body2" color="text.secondary">
                    {contactDescription}
                  </Typography>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <EventAvailableIcon color="primary" fontSize="small" />
                    <Typography variant="body2" color="text.secondary">
                      {calendarNote}
                    </Typography>
                  </Stack>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <AccessTimeIcon color="primary" fontSize="small" />
                    <Typography variant="body2" color="text.secondary">
                      {durationNote}
                    </Typography>
                  </Stack>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <LocalPhoneIcon color="primary" fontSize="small" />
                    <Typography variant="body2" color="text.secondary">
                      Añade tu WhatsApp si prefieres coordinar por ahí.
                    </Typography>
                  </Stack>
                </Stack>
              </Grid>

              <Grid item xs={12} md={8}>
                <form onSubmit={handleFormSubmit}>
                  <Stack spacing={2.5}>
                    {isMobile ? (
                      <Stack spacing={1}>
                        <Stack direction="row" justifyContent="space-between" alignItems="center">
                          <Typography variant="body2" color="text.secondary">
                            Paso {activeStep + 1} de {BOOKING_STEPS.length}
                          </Typography>
                          <Chip label={BOOKING_STEPS[activeStep]} size="small" color="primary" variant="outlined" />
                        </Stack>
                        <Stack direction="row" spacing={1}>
                          {BOOKING_STEPS.map((label, index) => (
                            <Box
                              key={label}
                              sx={(theme) => ({
                                flex: 1,
                                height: 6,
                                borderRadius: 999,
                                bgcolor: index <= activeStep ? 'primary.main' : theme.palette.action.disabledBackground,
                              })}
                            />
                          ))}
                        </Stack>
                      </Stack>
                    ) : (
                      <Stepper activeStep={activeStep} alternativeLabel>
                        {BOOKING_STEPS.map((label) => (
                          <Step key={label}>
                            <StepLabel>{label}</StepLabel>
                          </Step>
                        ))}
                      </Stepper>
                    )}
                    <Grid container spacing={2.5}>
                      {activeStep === 0 && (
                        <>
                          <Grid item xs={12} sm={6}>
                            <TextField
                              label="Nombre completo"
                              value={form.fullName}
                              onChange={(e) => setForm((prev) => ({ ...prev, fullName: e.target.value }))}
                              fullWidth
                              required
                              disabled={formDisabled}
                            />
                          </Grid>
                          <Grid item xs={12} sm={6}>
                            <TextField
                              label="Correo"
                              type="email"
                              value={form.email}
                              onChange={(e) => setForm((prev) => ({ ...prev, email: e.target.value }))}
                              fullWidth
                              required
                              disabled={formDisabled}
                            />
                          </Grid>
                          <Grid item xs={12}>
                            <TextField
                              type="tel"
                              label="WhatsApp / Teléfono"
                              value={form.phone}
                              onChange={(e) => setForm((prev) => ({ ...prev, phone: e.target.value }))}
                              fullWidth
                              disabled={formDisabled}
                            />
                          </Grid>
                          <Grid item xs={12}>
                            <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                              <Checkbox
                                checked={rememberProfile}
                                onChange={(e) => setRememberProfile(e.target.checked)}
                                size="small"
                                disabled={formDisabled}
                              />
                              <Typography variant="body2" color="text.secondary">
                                Recordar mis datos mientras esta pestaña siga abierta.
                              </Typography>
                              <Button
                                size="small"
                                variant="text"
                                onClick={clearSavedProfile}
                                sx={{ ml: 'auto' }}
                                disabled={formDisabled}
                              >
                                Limpiar datos guardados
                              </Button>
                            </Stack>
                          </Grid>
                          <Grid item xs={12}>
                            <Stack
                              direction={{ xs: 'column', sm: 'row' }}
                              justifyContent="flex-end"
                              alignItems={{ xs: 'stretch', sm: 'center' }}
                            >
                              <Button
                                variant="contained"
                                onClick={goToScheduleStep}
                                disabled={formDisabled}
                                fullWidth={isMobile}
                              >
                                Continuar
                              </Button>
                            </Stack>
                          </Grid>
                        </>
                      )}

                      {activeStep === 1 && (
                        <>
                          <Grid item xs={12}>
                            <TextField
                              label={presetService ? 'Servicio DJ Booth' : 'Servicio'}
                              select
                              value={form.serviceOfferingId}
                              onChange={(e) => {
                                const nextService = services.find((service) => service.id === e.target.value);
                                if (!nextService) return;
                                setForm((prev) => ({
                                  ...prev,
                                  serviceOfferingId: nextService.id,
                                  serviceType: nextService.name,
                                  resourceLabels: serviceResourceLabels(nextService),
                                }));
                              }}
                              fullWidth
                              required
                              disabled={formDisabled || Boolean(presetService)}
                              helperText={
                                presetService
                                  ? `Servicio preseleccionado para este enlace. ${
                                      estimatePriceLabel
                                        ? `Estimado: ${estimatePriceLabel} · Moneda: ${studioCurrency}`
                                        : `Moneda: ${studioCurrency}`
                                    }`
                                  : estimatePriceLabel
                                  ? `Estimado: ${estimatePriceLabel} · Moneda: ${studioCurrency}`
                                  : `Moneda: ${studioCurrency}`
                              }
                            >
                              {services.map((svc) => (
                                <MenuItem key={svc.id} value={svc.id}>
                                  <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between" sx={{ width: '100%' }}>
                                    <Typography>{svc.name}</Typography>
                                    <Typography variant="body2" color="text.secondary">
                                      {servicePriceLookup.get(svc.id)}
                                    </Typography>
                                  </Stack>
                                </MenuItem>
                              ))}
                              {services.length === 0 && <MenuItem value="" disabled>Catálogo no disponible</MenuItem>}
                            </TextField>
                          </Grid>
                          <Grid item xs={12} sm={7}>
                            <TextField
                              label="Fecha y hora"
                              type="datetime-local"
                              value={form.startsAt}
                              onChange={(e) => {
                                setDurationNotice(null);
                                const next = DateTime.fromISO(e.target.value, { zone: userTimeZone });
                                const duration = normalizeDurationMinutes(form.durationMinutes);
                                if (!next.isValid) {
                                  setForm((prev) => ({ ...prev, startsAt: e.target.value }));
                                  return;
                                }
                                const safe = sanitizeStart(next, duration);
                                setForm((prev) => ({ ...prev, startsAt: toLocalInputValue(safe.toJSDate()) }));
                              }}
                              fullWidth
                              InputLabelProps={{ shrink: true }}
                              inputProps={{ min: minStartValueForInput, step: START_STEP_MINUTES * 60 }}
                              required
                              helperText={availabilityHelperText}
                              disabled={formDisabled}
                            />
                            <Stack direction="row" spacing={1} sx={{ mt: 1 }} flexWrap="wrap">
                              <Button
                                size="small"
                                variant="outlined"
                                disabled={formDisabled}
                                onClick={() => {
                                  setDurationNotice(null);
                                  setForm((prev) => {
                                    const shortcut = sanitizeStart(firstAvailable(0), normalizeDurationMinutes(prev.durationMinutes));
                                    return {
                                      ...prev,
                                      startsAt: toLocalInputValue(shortcut.toJSDate()),
                                    };
                                  });
                                }}
                              >
                                Primer horario hoy
                              </Button>
                              <Button
                                size="small"
                                variant="text"
                                disabled={formDisabled}
                                onClick={() => {
                                  setDurationNotice(null);
                                  setForm((prev) => {
                                    const shortcut = sanitizeStart(firstAvailable(1), normalizeDurationMinutes(prev.durationMinutes));
                                    return {
                                      ...prev,
                                      startsAt: toLocalInputValue(shortcut.toJSDate()),
                                    };
                                  });
                                }}
                              >
                                Mañana
                              </Button>
                              {(availabilityStatus === 'unknown' || availabilityStatus === 'unavailable') && (
                                <Button
                                  size="small"
                                  variant="text"
                                  disabled={formDisabled}
                                  onClick={() => setAvailabilityNonce((v) => v + 1)}
                                >
                                  Reintentar
                                </Button>
                              )}
                              {availabilityStatus !== 'available' && (
                                <Button
                                  size="small"
                                  variant="text"
                                  disabled={formDisabled}
                                  href={STUDIO_WHATSAPP_URL}
                                  target="_blank"
                                  rel="noreferrer"
                                >
                                  WhatsApp
                                </Button>
                              )}
                            </Stack>
                            {timeWarnings.length > 0 && (
                              <Alert severity="info" sx={{ mt: 1 }}>
                                {timeWarnings.map((msg, idx) => (
                                  <Typography key={idx} variant="caption" display="block">
                                    • {msg}
                                  </Typography>
                                ))}
                              </Alert>
                            )}
                          </Grid>
                          <Grid item xs={12} sm={5}>
                            <Stack spacing={1}>
                              <TextField
                                label="Duración (min)"
                                type="number"
                                value={form.durationMinutes}
                                onChange={(e) => {
                                  setDurationNotice(null);
                                  setForm((prev) => ({
                                    ...prev,
                                    durationMinutes: normalizeDurationMinutes(Number(e.target.value), prev.durationMinutes),
                                  }));
                                }}
                                fullWidth
                                disabled={formDisabled}
                                inputProps={{
                                  min: 30,
                                  step: 15,
                                  max: maxDurationUntilClose != null && maxDurationUntilClose > 0 ? maxDurationUntilClose : undefined,
                                }}
                                helperText={outOfHours ?? durationLimitLabel ?? `Precios en ${studioCurrency}`}
                              />
                              <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                                {[30, 60, 90, 120].map((value) => (
                                  <Chip
                                    key={value}
                                    label={`${value} min`}
                                    size="small"
                                    color={form.durationMinutes === value ? 'primary' : 'default'}
                                    onClick={() => setForm((prev) => ({ ...prev, durationMinutes: value }))}
                                    sx={{ borderRadius: 999 }}
                                    disabled={formDisabled}
                                  />
                                ))}
                              </Stack>
                              {durationNotice && (
                                <Typography variant="caption" color="text.secondary" sx={{ display: 'block' }}>
                                  {durationNotice}
                                </Typography>
                              )}
                            </Stack>
                          </Grid>
                          {suggestedSlots.length > 0 && (
                            <Grid item xs={12}>
                              <Stack spacing={0.5}>
                                <Typography variant="body2" color="text.secondary">
                                  Sugerencias rápidas
                                </Typography>
                                <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                                  {suggestedSlots.map((slot) => (
                                    <Tooltip key={slot.value} title={slot.helper}>
                                      <Chip
                                        label={slot.label}
                                        onClick={() => {
                                          setDurationNotice(null);
                                          setForm((prev) => {
                                            const adjusted = clampDurationForStart(slot.value, prev.durationMinutes);
                                            if (adjusted < prev.durationMinutes) {
                                              setDurationNotice(`Ajustamos a ${adjusted} min para terminar antes del cierre.`);
                                            }
                                            return { ...prev, startsAt: slot.value, durationMinutes: adjusted };
                                          });
                                        }}
                                        variant="outlined"
                                        color="primary"
                                        sx={{ borderRadius: 999 }}
                                        disabled={formDisabled}
                                      />
                                    </Tooltip>
                                  ))}
                                </Stack>
                              </Stack>
                            </Grid>
                          )}
                          {selectedServiceRequiresEngineer && (
                            <Grid item xs={12}>
                              <Stack spacing={1}>
                                <Stack direction="row" spacing={1} alignItems="center">
                                  <Checkbox
                                    checked={assignEngineerLater}
                                    onChange={(e) => {
                                      const checked = e.target.checked;
                                      setAssignEngineerLater(checked);
                                      if (checked) {
                                        setForm((prev) => ({ ...prev, engineerId: null, engineerName: '' }));
                                      }
                                    }}
                                    size="small"
                                    disabled={formDisabled}
                                  />
                                  <Typography variant="body2" color="text.secondary">
                                    Asignar ingeniero después
                                  </Typography>
                                </Stack>
                                {!assignEngineerLater && (
                                  <Autocomplete<string | PublicEngineer, false, false, true>
                                    options={engineers}
                                    getOptionLabel={(opt) => (typeof opt === 'string' ? opt : opt.peName)}
                                    isOptionEqualToValue={(option, value) => {
                                      if (typeof option === 'string' || typeof value === 'string') {
                                        return typeof option === 'string' && typeof value === 'string' && option === value;
                                      }
                                      return option.peId === value.peId;
                                    }}
                                    loading={engineersLoading}
                                    freeSolo
                                    disabled={formDisabled}
                                    value={engineerValue}
                                    onChange={(_evt, value) => {
                                      if (!value) {
                                        setForm((prev) => ({ ...prev, engineerId: null, engineerName: '' }));
                                        return;
                                      }
                                      const id = typeof value === 'string' ? null : value.peId;
                                      const name = typeof value === 'string' ? value : value.peName;
                                      setForm((prev) => ({ ...prev, engineerId: id, engineerName: name }));
                                    }}
                                    inputValue={form.engineerName}
                                    onInputChange={(_evt, value, reason) => {
                                      if (reason === 'reset') return;
                                      setForm((prev) => {
                                        const normalized = value.trim().toLowerCase();
                                        const exactEngineerId =
                                          normalized === '' ? null : exactEngineerIdByName.get(normalized) ?? null;
                                        return {
                                          ...prev,
                                          engineerName: value,
                                          engineerId: exactEngineerId,
                                        };
                                      });
                                    }}
                                    renderOption={(props, option) => {
                                      const optionProps = { ...props };
                                      delete optionProps.key;
                                      const label = typeof option === 'string' ? option : option.peName;
                                      const optionKey = typeof option === 'string' ? `engineer-free-${label}` : `engineer-${option.peId}`;
                                      return (
                                        <li {...optionProps} key={optionKey}>
                                          {label}
                                        </li>
                                      );
                                    }}
                                    renderInput={(params) => (
                                      <TextField
                                        {...params}
                                        label="Ingeniero asignado"
                                        placeholder="Elige quién llevará la sesión"
                                        required
                                        InputProps={{
                                          ...params.InputProps,
                                          endAdornment: (
                                            <>
                                              {engineersLoading ? <CircularProgress size={16} /> : null}
                                              {params.InputProps.endAdornment}
                                            </>
                                          ),
                                        }}
                                        helperText={
                                          engineersError ??
                                          (engineers.length === 0 && !engineersLoading
                                            ? 'Escribe el nombre del ingeniero (catálogo no disponible).'
                                            : 'Selecciona o escribe el ingeniero asignado.')
                                        }
                                      />
                                    )}
                                  />
                                )}
                              </Stack>
                            </Grid>
                          )}
                          <Grid item xs={12}>
                            <TextField
                              label="Notas para el equipo"
                              value={form.notes}
                              onChange={(e) => setForm((prev) => ({ ...prev, notes: e.target.value }))}
                              fullWidth
                              multiline
                              minRows={3}
                              placeholder={notesPlaceholder}
                              disabled={formDisabled}
                            />
                          </Grid>
                          <Grid item xs={12}>
                            <Stack
                              direction={{ xs: 'column', sm: 'row' }}
                              justifyContent="space-between"
                              spacing={1}
                            >
                              <Button
                                variant="text"
                                onClick={() => setActiveStep(0)}
                                disabled={formDisabled}
                                fullWidth={isMobile}
                              >
                                Volver
                              </Button>
                              <Button
                                variant="contained"
                                onClick={goToConfirmStep}
                                disabled={formDisabled}
                                fullWidth={isMobile}
                              >
                                Revisar reserva
                              </Button>
                            </Stack>
                          </Grid>
                        </>
                      )}

                      {activeStep === 2 && (
                        <>
                          <Grid item xs={12}>
                            <Card
                              variant="outlined"
                              sx={{
                                bgcolor: 'rgba(255,255,255,0.02)',
                                borderColor: 'rgba(255,255,255,0.08)',
                              }}
                            >
                              <CardContent>
                                <Stack spacing={1}>
                                  <Typography variant="subtitle2" color="text.secondary">
                                    Resumen rápido
                                  </Typography>
                                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                                    <Chip label={form.serviceType || 'Servicio'} size="small" color="primary" variant="outlined" />
                                    {form.resourceLabels.length > 0 && (
                                      <Chip
                                        label={`Salas: ${form.resourceLabels.join(' + ')}`}
                                        size="small"
                                        variant="outlined"
                                      />
                                    )}
                                    <Chip
                                      label={formattedStart ? `Inicio: ${formattedStart}` : 'Elige fecha y hora'}
                                      size="small"
                                      variant="outlined"
                                    />
                                    <Chip
                                      label={`Duración: ${normalizeDurationMinutes(form.durationMinutes)} min`}
                                      size="small"
                                      variant="outlined"
                                    />
                                    <Chip
                                      label={
                                        authoritativeQuote
                                          ? `Total: ${formatMinorAmount(authoritativeQuote.currency, authoritativeQuote.totalMinor)}`
                                          : selectedPrice
                                            ? `Referencia: ${selectedPrice}`
                                            : 'Precio se confirma contigo'
                                      }
                                      size="small"
                                      variant="outlined"
                                    />
                                    <Chip
                                      label={`Zona: Estudio ${studioZoneLabel} · Tú ${userZoneLabel}`}
                                      size="small"
                                      variant="outlined"
                                    />
                                    {selectedServiceRequiresEngineer && (
                                      <Chip
                                        label={
                                          form.engineerName.trim()
                                            ? `Ingeniero: ${form.engineerName}`
                                            : 'Selecciona ingeniero'
                                        }
                                        size="small"
                                        color={form.engineerName.trim() ? 'primary' : 'default'}
                                        variant="outlined"
                                      />
                                    )}
                                  </Stack>
                                  <Divider sx={{ my: 1 }} />
                                  <Typography variant="body2" color="text.secondary">
                                    Te enviaremos la confirmación por correo y coordinaremos cualquier ajuste de horario o salas contigo.
                                  </Typography>
                                  {estimatePriceLabel && (
                                    <Typography variant="subtitle2" sx={{ mt: 1 }}>
                                      {authoritativeQuote ? 'Precio autorizado' : 'Estimado'}: {estimatePriceLabel}
                                    </Typography>
                                  )}
                                  {authoritativeQuote && (
                                    <Stack direction="row" spacing={1} alignItems="flex-start">
                                      <Checkbox
                                        checked={termsAccepted}
                                        onChange={(event) => setTermsAccepted(event.target.checked)}
                                        size="small"
                                        disabled={formDisabled}
                                        inputProps={{ 'aria-label': 'Aceptar precio y política de reserva' }}
                                      />
                                      <Typography variant="body2" color="text.secondary" sx={{ pt: 0.75 }}>
                                        Acepto la política {authoritativeQuote.termsVersion}, el total de{' '}
                                        <strong>{formatMinorAmount(authoritativeQuote.currency, authoritativeQuote.totalMinor)}</strong>{' '}
                                        y el depósito de{' '}
                                        <strong>{formatMinorAmount(authoritativeQuote.currency, authoritativeQuote.depositMinor)}</strong>.
                                        Crear la orden no significa que el depósito esté pagado.
                                      </Typography>
                                    </Stack>
                                  )}
                                </Stack>
                              </CardContent>
                            </Card>
                          </Grid>
                          <Grid item xs={12}>
                            <Stack
                              direction={{ xs: 'column', sm: 'row' }}
                              justifyContent="space-between"
                              spacing={1}
                            >
                              <Button
                                variant="text"
                                onClick={() => setActiveStep(1)}
                                disabled={formDisabled || Boolean(authoritativeQuote && !termsAccepted)}
                                fullWidth={isMobile}
                              >
                                Volver
                              </Button>
                              <Button
                                type="submit"
                                variant="contained"
                                size="large"
                                disabled={formDisabled}
                                fullWidth={isMobile}
                              >
                                {success
                                  ? 'Reserva enviada'
                                  : submitting
                                    ? 'Creando…'
                                    : authoritativeQuote
                                      ? 'Crear orden y retener horario'
                                      : 'Confirmar reserva'}
                              </Button>
                            </Stack>
                          </Grid>
                        </>
                      )}

                      {error && (
                        <Grid item xs={12}>
                          <Alert severity="error">{error}</Alert>
                        </Grid>
                      )}
                    </Grid>
                  </Stack>
                </form>
              </Grid>
            </Grid>
          </Stack>
        </CardContent>
      </Card>
      {form.serviceOfferingId && (
        <Box sx={{ mt: 3 }}>
          <ExperienceReviews
            targetKind="service_offering"
            targetId={form.serviceOfferingId}
            title="Reseñas del servicio"
          />
        </Box>
      )}
      <Snackbar
        open={snackbar.open}
        message={snackbar.message}
        autoHideDuration={2200}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
        onClose={() => setSnackbar({ open: false, message: '' })}
      />
    </Box>
  );
}
