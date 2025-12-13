import { useCallback, useEffect, useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Card,
  CardContent,
  Divider,
  Chip,
  Checkbox,
  CircularProgress,
  Grid,
  MenuItem,
  Tooltip,
  Snackbar,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import AccessTimeIcon from '@mui/icons-material/AccessTime';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import LocalPhoneIcon from '@mui/icons-material/LocalPhone';
import PersonIcon from '@mui/icons-material/Person';
import { DateTime } from 'luxon';
import { Bookings } from '../api/bookings';
import { API_BASE_URL } from '../api/client';
import type { BookingDTO, RoomDTO, ServiceCatalogDTO } from '../api/types';
import { Engineers, type PublicEngineer } from '../api/engineers';
import { Services } from '../api/services';
import { Rooms } from '../api/rooms';
import { defaultRoomsForService, sameRooms } from '../utils/publicBookingRooms';
import { mergeServiceTypes, type ServiceType } from '../utils/serviceTypesStore';
import { env } from '../utils/env';
import { useSession } from '../session/SessionContext';

interface FormState {
  fullName: string;
  email: string;
  phone: string;
  serviceType: string;
  startsAt: string;
  durationMinutes: number;
  notes: string;
  engineerId: number | null;
  engineerName: string;
  resourceLabels: string[];
}

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
const roundToNext = (date: Date, minutes: number) => {
  const ms = minutes * 60 * 1000;
  return new Date(Math.ceil(date.getTime() / ms) * ms);
};

const PROFILE_STORAGE_KEY = 'tdf-public-booking-profile';
const OPEN_HOURS = { start: 8, end: 22 }; // 24h local time
const ROOM_FALLBACKS = ['Live Room', 'Control Room', 'Vocal Booth', 'DJ Booth'] as const;

const zoneLabel = (zone: string) => {
  try {
    return DateTime.now().setZone(zone).toFormat('ZZZZ');
  } catch {
    return zone;
  }
};

const START_STEP_MINUTES = 15;

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

const buildInitialForm = (defaultService: string, roomOptions: string[]) => {
  const start = alignToStepMinutes(DateTime.now().plus({ minutes: 90 }));
  const initialRooms = defaultRoomsForService(defaultService, roomOptions);
  return {
    fullName: '',
    email: '',
    phone: '',
    serviceType: defaultService,
    startsAt: toLocalInputValue(start.toJSDate()),
    durationMinutes: 60,
    notes: '',
    engineerId: null,
    engineerName: '',
    resourceLabels: initialRooms,
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

export default function PublicBookingPage() {
  const serviceCatalogQuery = useQuery<ServiceCatalogDTO[]>({
    queryKey: ['service-catalog', 'public'],
    queryFn: () => Services.listPublic(),
    staleTime: 5 * 60 * 1000,
  });
  const roomsQuery = useQuery<RoomDTO[]>({
    queryKey: ['rooms', 'public'],
    queryFn: () => Rooms.listPublic(),
    staleTime: 5 * 60 * 1000,
  });
  const services = useMemo<ServiceType[]>(
    () => mergeServiceTypes(serviceCatalogQuery.data, { sort: false }),
    [serviceCatalogQuery.data],
  );
  const roomOptions = useMemo<string[]>(() => {
    const apiRooms = (roomsQuery.data ?? []).map((r) => r.rName).filter(Boolean);
    const unique = Array.from(new Set(apiRooms));
    return unique.length ? unique : [...ROOM_FALLBACKS];
  }, [roomsQuery.data]);
  const defaultService = services[0]?.name ?? 'Reserva';
  const { session, logout } = useSession();
  const userTimeZone = useMemo(() => {
    if (typeof Intl === 'undefined' || !Intl.DateTimeFormat) return 'UTC';
    return Intl.DateTimeFormat().resolvedOptions().timeZone ?? 'UTC';
  }, []);
  const studioTimeZone = useMemo(
    () => env.read('VITE_TZ') ?? 'America/Guayaquil',
    [],
  );
  const studioZoneLabel = useMemo(() => zoneLabel(studioTimeZone), [studioTimeZone]);
  const userZoneLabel = useMemo(() => zoneLabel(userTimeZone), [userTimeZone]);
  const studioCurrency = useMemo(() => services[0]?.currency ?? 'USD', [services]);
  const [form, setForm] = useState<FormState>(() => buildInitialForm(defaultService, roomOptions));
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<BookingDTO | null>(null);
  const [rememberProfile, setRememberProfile] = useState(false);
  const [engineers, setEngineers] = useState<PublicEngineer[]>([]);
  const [engineersLoading, setEngineersLoading] = useState(false);
  const [engineersError, setEngineersError] = useState<string | null>(null);
  const [durationNotice, setDurationNotice] = useState<string | null>(null);
  const [availabilityStatus, setAvailabilityStatus] = useState<'idle' | 'checking' | 'available' | 'unavailable' | 'unknown'>('idle');
  const [availabilityNote, setAvailabilityNote] = useState<string | null>(null);
  const [assignEngineerLater, setAssignEngineerLater] = useState(false);
  const [snackbar, setSnackbar] = useState<{ open: boolean; message: string }>({ open: false, message: '' });

  useEffect(() => {
    if (!services.length) return;
    setForm((prev) => {
      const serviceStillValid = services.some((svc) => svc.name === prev.serviceType);
      if (serviceStillValid) return prev;
      const nextService = services[0]?.name ?? prev.serviceType;
      if (!nextService || nextService === prev.serviceType) return prev;
      return { ...prev, serviceType: nextService, resourceLabels: defaultRoomsForService(nextService, roomOptions) };
    });
  }, [services, roomOptions]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const raw = window.localStorage.getItem(PROFILE_STORAGE_KEY);
      if (!raw) return;
      const stored = JSON.parse(raw) as Partial<FormState>;
      const allowedServices = new Set(services.map((s) => s.name));
      const storedService = stored.serviceType ?? defaultService;
      const nextService = allowedServices.has(storedService) ? storedService : defaultService;
      setForm((prev) => ({
        ...prev,
        fullName: stored.fullName ?? prev.fullName,
        email: stored.email ?? prev.email,
        phone: stored.phone ?? prev.phone,
        serviceType: nextService,
        resourceLabels: defaultRoomsForService(nextService, roomOptions),
      }));
      setRememberProfile(true);
    } catch {
      // ignore parsing issues
    }
  }, [defaultService, services, roomOptions]);

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
      window.localStorage.removeItem(PROFILE_STORAGE_KEY);
      return;
    }
    const payload = {
      fullName: form.fullName.trim(),
      email: form.email.trim(),
      phone: form.phone.trim(),
      serviceType: form.serviceType,
    };
    window.localStorage.setItem(PROFILE_STORAGE_KEY, JSON.stringify(payload));
  }, [rememberProfile, form.fullName, form.email, form.phone, form.serviceType]);

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

  const formDisabled = submitting || Boolean(success);

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
    setError(null);
    setSubmitting(false);
    setForm(buildInitialForm(defaultService, roomOptions));
    setAssignEngineerLater(false);
  }, [defaultService, roomOptions]);

  useEffect(() => {
    const parsed = DateTime.fromISO(form.startsAt, { zone: userTimeZone });
    const duration = Math.max(30, Number(form.durationMinutes) || 60);
    if (!parsed.isValid) {
      setAvailabilityStatus('idle');
      setAvailabilityNote(null);
      return;
    }
    const controller = new AbortController();
    const startsAtUtc = parsed.toUTC().toISO();
    if (!startsAtUtc) return;
    setAvailabilityStatus('checking');
    setAvailabilityNote(null);
    const url = `${API_BASE_URL}/bookings/public/availability?startsAt=${encodeURIComponent(startsAtUtc)}&durationMinutes=${duration}`;
    fetch(url, { signal: controller.signal })
      .then(async (res) => {
        if (!res.ok) throw new Error(`status ${res.status}`);
        const data = (await res.json()) as { available?: boolean; isAvailable?: boolean; reason?: string } | null;
        const isAvailable = data?.available ?? data?.isAvailable;
        if (isAvailable === false) {
          setAvailabilityStatus('unavailable');
          setAvailabilityNote(data?.reason ?? 'Ese horario ya está reservado.');
        } else if (isAvailable === true) {
          setAvailabilityStatus('available');
          setAvailabilityNote(null);
        } else {
          setAvailabilityStatus('unknown');
          setAvailabilityNote('No pudimos verificar disponibilidad, confirmaremos contigo.');
        }
      })
      .catch((err) => {
        if (controller.signal.aborted) return;
        console.warn('No se pudo verificar disponibilidad', err);
        setAvailabilityStatus('unknown');
        setAvailabilityNote('No pudimos verificar disponibilidad, confirmaremos contigo.');
      });
    return () => controller.abort();
  }, [form.durationMinutes, form.startsAt, userTimeZone]);

  const handleSubmit = async (evt: React.FormEvent) => {
    evt.preventDefault();
    setError(null);
    setSuccess(null);

    if (!form.fullName.trim()) {
      setError('Agrega tu nombre para continuar.');
      return;
    }
    if (!form.email.trim()) {
      setError('Necesitamos un correo para confirmarte la reserva.');
      return;
    }
    if (!form.serviceType.trim()) {
      setError('Selecciona un tipo de servicio.');
      return;
    }

    const parsedStartLocal = DateTime.fromISO(form.startsAt, { zone: userTimeZone });
    if (!parsedStartLocal.isValid) {
      setError('Selecciona una fecha y hora válida.');
      return;
    }
    const now = DateTime.now().setZone(userTimeZone);
    if (parsedStartLocal < now.plus({ minutes: 15 })) {
      setError('Elige un horario al menos 15 minutos en el futuro.');
      return;
    }
    const durationMinutes = Math.max(30, Number(form.durationMinutes) || 60);
    const startStudio = parsedStartLocal.setZone(studioTimeZone);
    const openStudio = startStudio.set({ hour: OPEN_HOURS.start, minute: 0, second: 0, millisecond: 0 });
    const closeStudio = startStudio.set({ hour: OPEN_HOURS.end, minute: 0, second: 0, millisecond: 0 });
    const proposedEndStudio = startStudio.plus({ minutes: durationMinutes });
    const openUser = openStudio.setZone(userTimeZone);
    const closeUser = closeStudio.setZone(userTimeZone);
    if (startStudio < openStudio) {
      setError(
        `Nuestro horario es ${openStudio.toFormat('HH:mm')} - ${closeStudio.toFormat('HH:mm')} (${studioZoneLabel}). En tu zona (${userZoneLabel}) eso es ${openUser.toFormat('HH:mm')} - ${closeUser.toFormat('HH:mm')}.`,
      );
      return;
    }
    if (proposedEndStudio > closeStudio) {
      const remaining = Math.max(0, Math.floor(closeStudio.diff(startStudio, 'minutes').minutes));
      setError(
        `La cita debe terminar antes de las ${closeStudio.toFormat('HH:mm')} (${studioZoneLabel}). Con esa hora, el máximo es ${remaining} min.`,
      );
      return;
    }
    if (requiresEngineer(form.serviceType) && !assignEngineerLater && !form.engineerId && !form.engineerName.trim()) {
      setError('Selecciona un ingeniero para grabación/mezcla/mastering.');
      return;
    }
    if (availabilityStatus === 'unavailable') {
      setError(availabilityNote ?? 'Ese horario ya está ocupado. Elige otro.');
      return;
    }

    setSubmitting(true);
    const autoRooms = defaultRoomsForService(form.serviceType, roomOptions);
    const roomsToSend =
      autoRooms.length > 0 ? autoRooms : roomOptions.length > 0 ? roomOptions.slice(0, 1) : [];
    if (roomsToSend.length > 0) {
      setForm((prev) => (sameRooms(prev.resourceLabels, roomsToSend) ? prev : { ...prev, resourceLabels: roomsToSend }));
    }
    const engineerPartyId = assignEngineerLater ? null : form.engineerId;
    const engineerName = assignEngineerLater ? null : form.engineerName.trim() || null;
    try {
      const startsAtIso = parsedStartLocal.toUTC().toISO();
      const dto = await Bookings.createPublic({
        pbFullName: form.fullName.trim(),
        pbEmail: form.email.trim(),
        pbPhone: form.phone.trim() || null,
        pbServiceType: form.serviceType.trim(),
        pbStartsAt: startsAtIso,
        pbDurationMinutes: durationMinutes,
        pbNotes: form.notes.trim() || null,
        pbEngineerPartyId: engineerPartyId,
        pbEngineerName: engineerName,
        pbResourceIds: roomsToSend.length ? roomsToSend : null,
      });
      setSuccess(dto);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'No pudimos crear la reserva.';
      setError(message);
    } finally {
      setSubmitting(false);
    }
  };

  const bookingWindow = useMemo(() => {
    if (!form.startsAt) return null;
    const startLocal = DateTime.fromISO(form.startsAt, { zone: userTimeZone });
    if (!startLocal.isValid) return null;
    const duration = Math.max(30, Number(form.durationMinutes) || 60);
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
      map.set(svc.name, `${display}${unit}`);
    });
    return map;
  }, [services]);
  const estimatePriceLabel = useMemo(() => {
    const svc = services.find((s) => s.name === form.serviceType);
    if (svc?.priceCents == null) return null;
    const base = `${svc.currency} ${(svc.priceCents / 100).toLocaleString(undefined, { minimumFractionDigits: 0, maximumFractionDigits: 2 })}`;
    if (svc.billingUnit?.toLowerCase().includes('hora')) {
      const hours = Math.max(0.5, (Number(form.durationMinutes) || 60) / 60);
      const total = (svc.priceCents / 100) * hours;
      return `${svc.currency} ${total.toLocaleString(undefined, { minimumFractionDigits: 0, maximumFractionDigits: 0 })} aprox (${hours.toFixed(1)}h)`;
    }
    return `${base}${svc.billingUnit ? ` / ${svc.billingUnit}` : ''}`;
  }, [form.durationMinutes, form.serviceType, services]);
  const selectedPrice = servicePriceLookup.get(form.serviceType);

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
    () => defaultRoomsForService(form.serviceType, roomOptions),
    [form.serviceType, roomOptions],
  );

  const requiresEngineer = (service: string) => {
    const lowered = service.toLowerCase();
    return lowered.includes('graba') || lowered.includes('mezcl') || lowered.includes('master');
  };

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
        'Reserva TDF',
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
    [estimatePriceLabel, form.durationMinutes, form.engineerName, form.resourceLabels, form.serviceType, form.startsAt, formattedStart, selectedPrice, suggestedRooms, userTimeZone],
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
      const now = new Date();
      now.setMinutes(now.getMinutes() + 15);
      now.setSeconds(0, 0);
      const targetDay = new Date(now);
      targetDay.setDate(targetDay.getDate() + dayOffset);
      const start = new Date(
        targetDay.getFullYear(),
        targetDay.getMonth(),
        targetDay.getDate(),
        OPEN_HOURS.start,
        0,
        0,
        0,
      );
      const baseline = dayOffset === 0 && now > start ? now : start;
      const rounded = roundToNext(baseline, 30);
      const limited =
        rounded.getHours() >= OPEN_HOURS.end
          ? new Date(targetDay.getFullYear(), targetDay.getMonth(), targetDay.getDate(), OPEN_HOURS.end - 1, 30, 0, 0)
          : rounded;
      return limited;
    },
    [],
  );

  useEffect(() => {
    setForm((prev) => {
      if (sameRooms(prev.resourceLabels, suggestedRooms)) return prev;
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
    const duration = Math.max(30, Number(form.durationMinutes) || 60);
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
                  Agenda pública
                </Typography>
                <Typography variant="h4" fontWeight={800}>
                  Reserva enviada
                </Typography>
                <Typography variant="body1" color="text.secondary">
                  Revisa tu correo para la confirmación. Si necesitas ajustes de horario o salas, responde al correo y lo
                  coordinamos contigo.
                </Typography>
              </Stack>

              <Grid container spacing={2}>
                <Grid item xs={12}>
                  <Alert severity="success">
                    Reserva creada. ID <strong>{success.bookingId}</strong> · Servicio:{' '}
                    <strong>{success.serviceType ?? form.serviceType}</strong>
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
                      </Stack>
                    </CardContent>
                  </Card>
                </Grid>
                <Grid item xs={12}>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                    <Button variant="outlined" href="/login?redirect=/estudio/calendario" size="medium">
                      Ver mi reserva
                    </Button>
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
                    {calendarUrl && (
                      <Button variant="text" size="medium" href={calendarUrl} target="_blank" rel="noreferrer">
                        Agregar a Google Calendar
                      </Button>
                    )}
                  </Stack>
                </Grid>
              </Grid>
            </Stack>
          </CardContent>
        </Card>
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
                Agenda pública
              </Typography>
              <Typography variant="h4" fontWeight={800}>
                Reserva un servicio con TDF
              </Typography>
              <Typography variant="body1" color="text.secondary">
                Completa tus datos y agenda el horario que prefieras. Confirmaremos la reserva por correo y, si aún no
                tienes cuenta, crearemos tu acceso automáticamente.
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Horario del estudio: <strong>{studioZoneLabel}</strong>. Tu zona: <strong>{userZoneLabel}</strong>.
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Precios de referencia en <strong>{studioCurrency}</strong>; confirmamos el total contigo antes de agendar.
              </Typography>
              {priceBanner && (
                <Alert severity="info" variant="outlined">
                  {priceBanner}
                </Alert>
              )}
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                <Chip label="1. Agenda sin crear cuenta" size="small" variant="outlined" />
                <Chip label="2. Confirmamos por email" size="small" variant="outlined" />
                <Chip label="3. Coordinamos por WhatsApp si lo dejas" size="small" variant="outlined" />
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
                      <Button size="small" variant="outlined" href="/login?redirect=/reservar">
                        Iniciar sesión
                      </Button>
                      <Button size="small" variant="text" href="/login?signup=1&redirect=/reservar">
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
                      Datos de contacto
                    </Typography>
                  </Stack>
                  <Typography variant="body2" color="text.secondary">
                    Usa un correo válido para recibir la confirmación. Si eres nuevo, crearemos un perfil para ti.
                  </Typography>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <EventAvailableIcon color="primary" fontSize="small" />
                    <Typography variant="body2" color="text.secondary">
                      Bloque tentativo en el calendario.
                    </Typography>
                  </Stack>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <AccessTimeIcon color="primary" fontSize="small" />
                    <Typography variant="body2" color="text.secondary">
                      Duración estándar de 1h (ajústala si necesitas más tiempo).
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
                <form
                  onSubmit={(event) => {
                    void handleSubmit(event);
                  }}
                >
                  <Grid container spacing={2.5}>
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
                    <Grid item xs={12} sm={6}>
                      <TextField
                        label="WhatsApp / Teléfono"
                        value={form.phone}
                        onChange={(e) => setForm((prev) => ({ ...prev, phone: e.target.value }))}
                        fullWidth
                        disabled={formDisabled}
                      />
                    </Grid>
                    <Grid item xs={12} sm={6}>
                      <TextField
                        label="Servicio"
                        select
                        value={form.serviceType}
                        onChange={(e) => {
                          const nextService = e.target.value;
                          setForm((prev) => ({
                            ...prev,
                            serviceType: nextService,
                            resourceLabels: defaultRoomsForService(nextService, roomOptions),
                          }));
                        }}
                        fullWidth
                        required
                        disabled={formDisabled}
                        helperText={
                          estimatePriceLabel
                            ? `Estimado: ${estimatePriceLabel} · Moneda: ${studioCurrency}`
                            : `Moneda: ${studioCurrency}`
                        }
                      >
                        {services.map((svc) => (
                          <MenuItem key={svc.id} value={svc.name}>
                            <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between" sx={{ width: '100%' }}>
                              <Typography>{svc.name}</Typography>
                              <Typography variant="body2" color="text.secondary">
                                {servicePriceLookup.get(svc.name)}
                              </Typography>
                            </Stack>
                          </MenuItem>
                        ))}
                        {services.length === 0 && <MenuItem value={defaultService}>{defaultService}</MenuItem>}
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
                          const duration = Math.max(30, Number(form.durationMinutes) || 60);
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
                          onClick={() =>
                            setForm((prev) => ({
                              ...prev,
                              startsAt: toLocalInputValue(firstAvailable(0)),
                            }))
                          }
                        >
                          Primer horario hoy
                        </Button>
                        <Button
                          size="small"
                          variant="text"
                          disabled={formDisabled}
                          onClick={() =>
                            setForm((prev) => ({
                              ...prev,
                              startsAt: toLocalInputValue(firstAvailable(1)),
                            }))
                          }
                    >
                      Mañana
                    </Button>
                    {availabilityStatus === 'unavailable' && (
                      <Chip
                        label={availabilityNote ?? 'Horario ocupado'}
                        color="warning"
                        size="small"
                        variant="outlined"
                      />
                    )}
                    {availabilityStatus === 'available' && (
                      <Chip label="Disponible" color="success" size="small" variant="outlined" />
                    )}
                    {availabilityStatus === 'unknown' && availabilityNote && (
                      <Chip label={availabilityNote} color="default" size="small" variant="outlined" />
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
                          setForm((prev) => ({ ...prev, durationMinutes: Number(e.target.value) }));
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
                      {estimatePriceLabel && (
                        <Typography variant="body2" color="text.secondary">
                          Estimado con duración actual: {estimatePriceLabel}
                        </Typography>
                      )}
                      <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                        {[30, 60, 90, 120].map((value) => {
                          const disabled =
                            formDisabled ||
                            Boolean(
                              maxDurationUntilClose !== null &&
                                maxDurationUntilClose > 0 &&
                                value > maxDurationUntilClose,
                            );
                          const tooltip =
                            disabled && maxDurationUntilClose
                              ? `Máximo ${maxDurationUntilClose} min con este horario`
                              : '';
                          return (
                            <Tooltip key={value} title={tooltip} disableInteractive>
                              <span>
                                <Chip
                                  label={`${value} min`}
                                  size="small"
                                  color={form.durationMinutes === value ? 'primary' : 'default'}
                                  onClick={() => setForm((prev) => ({ ...prev, durationMinutes: value }))}
                                  disabled={disabled}
                                  sx={{ borderRadius: 999, opacity: disabled ? 0.5 : 1 }}
                                />
                              </span>
                            </Tooltip>
                          );
                        })}
                        </Stack>
                        {durationNotice && (
                          <Typography variant="caption" color="text.secondary" sx={{ display: 'block' }}>
                            {durationNotice}
                          </Typography>
                        )}
                        {priceBanner && (
                          <Typography variant="caption" color="text.secondary" sx={{ display: 'block' }}>
                            {priceBanner}
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
                    {outOfHours && (
                      <Grid item xs={12}>
                        <Alert severity="warning">{outOfHours}</Alert>
                      </Grid>
                    )}
                    {requiresEngineer(form.serviceType) && (
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
                              onInputChange={(_evt, value) => setForm((prev) => ({ ...prev, engineerName: value }))}
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
                        <Typography variant="caption" color="text.secondary">
                          Requerido para grabación, mezcla o mastering. Si no encuentras a tu ingeniero, escríbenos en notas.
                        </Typography>
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
                        placeholder="Cuéntanos qué necesitas (ej: grabación de voz, mezcla, etc.)"
                        disabled={formDisabled}
                      />
                    </Grid>
                    <Grid item xs={12}>
                      <Stack direction="row" spacing={1} alignItems="center">
                        <Checkbox
                          checked={rememberProfile}
                          onChange={(e) => setRememberProfile(e.target.checked)}
                          size="small"
                          disabled={formDisabled}
                        />
                        <Typography variant="body2" color="text.secondary">
                          Recordar mis datos en este navegador para la próxima vez.
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
                    {error && (
                      <Grid item xs={12}>
                        <Alert severity="error">{error}</Alert>
                      </Grid>
                    )}
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
                              <Chip
                                label={form.serviceType || 'Servicio'}
                                size="small"
                                color="primary"
                                variant="outlined"
                              />
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
                                label={`Duración: ${form.durationMinutes || 60} min`}
                                size="small"
                                variant="outlined"
                              />
                              <Chip
                                label={selectedPrice ? `Referencia: ${selectedPrice}` : 'Precio se confirma contigo'}
                                size="small"
                                variant="outlined"
                              />
                              <Chip
                                label={`Zona: Estudio ${studioZoneLabel} · Tú ${userZoneLabel}`}
                                size="small"
                                variant="outlined"
                              />
                              {requiresEngineer(form.serviceType) && (
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
                              Te enviaremos la confirmación por correo y coordinaremos cualquier ajuste de horario o salas
                              contigo.
                            </Typography>
                            {estimatePriceLabel && (
                              <Typography variant="subtitle2" sx={{ mt: 1 }}>
                                Estimado: {estimatePriceLabel}
                              </Typography>
                            )}
                          </Stack>
                        </CardContent>
                      </Card>
                    </Grid>
                    <Grid item xs={12}>
                      <Button type="submit" variant="contained" size="large" disabled={formDisabled} fullWidth>
                        {success ? 'Reserva enviada' : submitting ? 'Enviando…' : 'Confirmar reserva'}
                      </Button>
                    </Grid>
                  </Grid>
                </form>
              </Grid>
            </Grid>
          </Stack>
        </CardContent>
      </Card>
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
