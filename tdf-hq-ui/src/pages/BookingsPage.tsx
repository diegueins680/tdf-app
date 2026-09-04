import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient, type UseQueryResult } from '@tanstack/react-query';
import { Bookings, type ServiceBookingCommerceDTO } from '../api/bookings';
import type { BookingDTO, PartyCreate, ServiceCatalogDTO } from '../api/types';
import {
  Typography,
  Paper,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  Stack,
  TextField,
  Alert,
  Collapse,
  MenuItem,
  FormControl,
  InputLabel,
  Select,
  Autocomplete,
  Chip,
} from '@mui/material';
import FullCalendar from '@fullcalendar/react';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import interactionPlugin from '@fullcalendar/interaction';
import { DateTime } from 'luxon';
import { mergeServiceTypes, type ServiceType } from '../utils/serviceTypesStore';
import { Rooms } from '../api/rooms';
import type { RoomDTO } from '../api/types';
import { Parties } from '../api/parties';
import type { PartySelectorOption } from '../api/partySelector';
import { PartySelector } from '../components/party-selector/PartySelector';
import { Services } from '../api/services';
import { Link as RouterLink, useLocation, useNavigate } from 'react-router-dom';
import {
  getBookingCalendarStatusState,
  getBookingConflictAlertText,
  getBookingEngineerFieldState,
  getBookingOptionalDetailsState,
  getBookingRoomsFieldState,
  getBookingServiceEntryGateState,
  getBookingServiceFieldState,
} from './bookingsPageLogic';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useCurrency } from '../contexts/CurrencyContext';

// FullCalendar v6 auto-injects its styles when the modules load, so importing the
// CSS bundles directly is unnecessary and breaks with Vite due to missing files.

const parsePositiveInt = (raw: string | null): number | null => {
  const trimmed = raw?.trim() ?? '';
  if (!/^\d+$/.test(trimmed)) return null;
  const parsed = Number(trimmed);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
};

export default function BookingsPage() {
  const { timezone: zone, locale } = useLocalePreferences();
  const { formatMoney } = useCurrency();
  const location = useLocation();
  const navigate = useNavigate();
  const calendarRef = useRef<FullCalendar | null>(null);
  const autoOpenHandled = useRef(false);
  const query = useMemo(() => new URLSearchParams(location.search), [location.search]);
  const bookingIdFilter = useMemo(() => {
    return parsePositiveInt(query.get('bookingId'));
  }, [query]);
  const partyIdFilter = useMemo(() => {
    return parsePositiveInt(query.get('partyId'));
  }, [query]);
  const engineerPartyIdFilter = useMemo(() => {
    return parsePositiveInt(query.get('engineerPartyId'));
  }, [query]);

  const bookingsQuery: UseQueryResult<BookingDTO[], Error> = useQuery<BookingDTO[], Error>({
    queryKey: ['bookings', bookingIdFilter, partyIdFilter, engineerPartyIdFilter],
    queryFn: () =>
      Bookings.list({
        bookingId: bookingIdFilter ?? undefined,
        partyId: partyIdFilter ?? undefined,
        engineerPartyId: engineerPartyIdFilter ?? undefined,
      }),
  });
  const roomsQuery = useQuery<RoomDTO[]>({
    queryKey: ['rooms'],
    queryFn: Rooms.list,
    staleTime: 5 * 60 * 1000,
  });
  const serviceCatalogQuery = useQuery<ServiceCatalogDTO[]>({
    queryKey: ['service-catalog', 'internal'],
    queryFn: () => Services.list(),
    staleTime: 5 * 60 * 1000,
  });
  const qc = useQueryClient();
  const bookings = useMemo<BookingDTO[]>(() => bookingsQuery.data ?? [], [bookingsQuery.data]);
  const rooms = useMemo<RoomDTO[]>(() => roomsQuery.data ?? [], [roomsQuery.data]);
  const hasActiveBookingFilter = bookingIdFilter != null || partyIdFilter != null || engineerPartyIdFilter != null;
  const handleClearBookingFilters = useCallback(() => {
    navigate({ pathname: location.pathname, search: '' }, { replace: true });
  }, [location.pathname, navigate]);
  const calendarStatusState = getBookingCalendarStatusState({
    bookingCount: bookings.length,
    hasActiveFilter: hasActiveBookingFilter,
    hasError: Boolean(bookingsQuery.error),
    isLoading: bookingsQuery.isLoading,
    roomCatalogLoading: roomsQuery.isLoading && roomsQuery.data == null,
    roomCount: rooms.length,
  });
  const statusOptions = [
    'Tentative',
    'Confirmed',
    'InProgress',
    'Completed',
    'Cancelled',
    'NoShow',
  ];
  const toIsoDate = (value: string): string => {
    const parsed = DateTime.fromISO(value);
    if (!parsed.isValid) {
      return value;
    }
    return parsed.toISO() ?? value;
  };
  const formatEventRange = (start?: Date | null, end?: Date | null) => {
    if (!start) return '';
    const startStr = DateTime.fromJSDate(start).setZone(zone).setLocale(locale).toFormat('ccc d LLL, HH:mm');
    if (!end) return startStr;
    const endStr = DateTime.fromJSDate(end).setZone(zone).toFormat('HH:mm');
    return `${startStr} - ${endStr}`;
  };

  const extractEngineerFromNotes = (raw?: string | null) => {
    if (!raw) return { engineer: '', engineerId: null as number | null, notesBody: '' };
    const lines = raw.split('\n');
    let engineer = '';
    let engineerId: number | null = null;
    const remaining: string[] = [];
    lines.forEach((line) => {
      const match = /^\s*engineer:\s*(.*)$/i.exec(line);
      if (match) {
        const value = match[1]?.trim() ?? '';
        engineer = value;
        const idMatch = /^\s*\[(\d+)\]\s*(.*)$/.exec(value);
        if (idMatch) {
          engineerId = Number(idMatch[1]);
          engineer = idMatch[2]?.trim() ?? '';
        }
      } else {
        remaining.push(line);
      }
    });
    return { engineer, engineerId, notesBody: remaining.join('\n').trim() };
  };

  const events = useMemo(
    () =>
      bookings.map((booking) => {
        const engineerMeta = booking.engineerName || booking.engineerPartyId
          ? { engineer: booking.engineerName ?? '', engineerId: booking.engineerPartyId ?? null, notesBody: booking.notes ?? '' }
          : extractEngineerFromNotes(booking.notes);
        const isCourse =
          Boolean(booking.courseSlug) ||
          (booking.bookingId ?? 0) < 0;
        const courseCapacity = booking.courseCapacity ?? undefined;
        const courseRemaining = booking.courseRemaining ?? undefined;
        const coursePrice = booking.coursePrice ?? undefined;
        const courseLocation = booking.courseLocation ?? undefined;
        const courseSubtitle = courseCapacity
          ? `Cupos: ${Math.max(0, courseRemaining ?? 0)}/${courseCapacity}`
          : null;
        const priceText = coursePrice ? formatMoney(coursePrice, booking.courseCurrency ?? undefined) : null;
        const locationText = courseLocation ?? null;
        return {
          id: String(booking.bookingId),
          title: booking.title,
          start: toIsoDate(booking.startsAt),
          end: toIsoDate(booking.endsAt),
          extendedProps: {
            ...booking,
            isCourse,
            courseSubtitle,
            priceText,
            locationText,
            engineerName: engineerMeta.engineer,
            engineerId: engineerMeta.engineerId,
          },
          backgroundColor: isCourse ? 'rgba(59,130,246,0.22)' : undefined,
          borderColor: isCourse ? 'rgba(59,130,246,0.4)' : undefined,
          editable: !isCourse,
          startEditable: !isCourse,
          durationEditable: !isCourse,
        };
      }),
    [bookings, formatMoney],
  );

  const [dialogOpen, setDialogOpen] = useState(false);
  const [mode, setMode] = useState<'create' | 'edit'>('create');
  const [editingId, setEditingId] = useState<number | null>(null);
  const [manualReviewNotes, setManualReviewNotes] = useState('');
  const [title, setTitle] = useState('Bloque de estudio');
  const [notes, setNotes] = useState('');
  const [startInput, setStartInput] = useState('');
  const [endInput, setEndInput] = useState('');
  const [formError, setFormError] = useState<string | null>(null);
  const [serviceOfferingId, setServiceOfferingId] = useState<string>('');
  const [engineerName, setEngineerName] = useState('');
  const [engineerPartyId, setEngineerPartyId] = useState<number | null>(null);
  const [selectedEngineer, setSelectedEngineer] = useState<PartySelectorOption | null>(null);
  const [customerPartyId, setCustomerPartyId] = useState<number | null>(null);
  const [selectedCustomer, setSelectedCustomer] = useState<PartySelectorOption | null>(null);
  const [assignedRoomIds, setAssignedRoomIds] = useState<string[]>([]);
  const [status, setStatus] = useState<string>('Confirmed');
  const [calendarError, setCalendarError] = useState<string | null>(null);
  const [courseNotice, setCourseNotice] = useState<string | null>(null);
  const [courseReadOnlyInfo, setCourseReadOnlyInfo] = useState<{
    title: string;
    range: string;
    subtitle?: string;
    price?: string;
    location?: string;
    slug?: string;
    shareUrl?: string;
  } | null>(null);
  const [createContactOpen, setCreateContactOpen] = useState(false);
  const [createContactForm, setCreateContactForm] = useState({ name: '', email: '', phone: '' });
  const [createContactError, setCreateContactError] = useState<string | null>(null);
  const serviceTypes = useMemo<ServiceType[]>(
    () => mergeServiceTypes(serviceCatalogQuery.data, { sort: false }),
    [serviceCatalogQuery.data],
  );
  const [prefillHandled, setPrefillHandled] = useState(false);
  const [prefillNotice, setPrefillNotice] = useState(false);
  const [autoAssignMessage, setAutoAssignMessage] = useState('');
  const [serviceLocked, setServiceLocked] = useState(false);
  const [duplicateDialogOpen, setDuplicateDialogOpen] = useState(false);
  const [duplicateStartInput, setDuplicateStartInput] = useState('');
  const [roomsManuallyAdjusted, setRoomsManuallyAdjusted] = useState(false);
  const [showOptionalDetails, setShowOptionalDetails] = useState(false);
  const bookingCommerceQuery = useQuery<ServiceBookingCommerceDTO, Error>({
    queryKey: ['booking-commerce', editingId],
    queryFn: () => Bookings.getCommerce(editingId!),
    enabled: dialogOpen && mode === 'edit' && editingId != null && editingId > 0,
    retry: false,
  });
  const manualReviewMutation = useMutation<
    ServiceBookingCommerceDTO,
    Error,
    'approve' | 'reject'
  >({
    mutationFn: (action) => Bookings.reviewManualPayment(editingId!, action, manualReviewNotes),
    onSuccess: (commerce) => {
      qc.setQueryData(['booking-commerce', editingId], commerce);
      setManualReviewNotes('');
      void qc.invalidateQueries({ queryKey: ['bookings'] });
    },
  });
  const defaultService = serviceTypes[0] ?? null;
  const formatServiceLabel = useCallback(
    (svc: ServiceType) => {
      if (svc.priceCents == null) return svc.name;
      const price = (svc.priceCents / 100).toLocaleString(undefined, { minimumFractionDigits: 0, maximumFractionDigits: 2 });
      const unit = svc.billingUnit ? ` / ${svc.billingUnit}` : '';
      return `${svc.name} — ${svc.currency} ${price}${unit}`;
    },
    [],
  );
  const conflicts = useMemo(() => {
    if (!startInput || !endInput) return [];
    const start = DateTime.fromFormat(startInput, "yyyy-LL-dd'T'HH:mm", { zone });
    const end = DateTime.fromFormat(endInput, "yyyy-LL-dd'T'HH:mm", { zone });
    if (!start.isValid || !end.isValid) return [];
    const assigned = new Set(assignedRoomIds);
    const isOverlap = (aStart: string, aEnd: string) => {
      const s = DateTime.fromISO(aStart);
      const e = DateTime.fromISO(aEnd);
      if (!s.isValid || !e.isValid) return false;
      return start < e && end > s;
    };
    const isActive = (status: string) => {
      const low = status.toLowerCase();
      return !(low.includes('cancel') || low.includes('no show'));
    };
    return bookings.filter((b) => {
      if (!isActive(b.status ?? '')) return false;
      if (editingId && b.bookingId === editingId) return false;
      if (!isOverlap(b.startsAt, b.endsAt)) return false;
      const roomIds = (b.resources ?? []).map((r) => r.brRoomId);
      if (roomIds.length === 0) return false;
      return roomIds.some((rid) => assigned.has(rid));
    });
  }, [assignedRoomIds, bookings, editingId, endInput, startInput, zone]);

  const selectedService = useMemo(
    () => serviceTypes.find((service) => service.id === serviceOfferingId) ?? null,
    [serviceOfferingId, serviceTypes],
  );
  const defaultRoomsForService = useCallback((offeringId: string): RoomDTO[] => {
    const service = serviceTypes.find((candidate) => candidate.id === offeringId);
    if (!service) return [];
    const requiredIds = service.defaultResources
      .filter((resource) => resource.sdrSelectionMode === 'all')
      .map((resource) => resource.sdrResourceId);
    const firstAvailableId = service.defaultResources
      .find((resource) => resource.sdrSelectionMode === 'first-available')?.sdrResourceId;
    const selectedIds = new Set(firstAvailableId ? [...requiredIds, firstAvailableId] : requiredIds);
    return rooms.filter((room) => selectedIds.has(room.roomId));
  }, [rooms, serviceTypes]);

  const assignedRooms = useMemo(
    () => rooms.filter((room) => assignedRoomIds.includes(room.roomId)),
    [rooms, assignedRoomIds],
  );

  const serviceCatalogReady = !serviceCatalogQuery.isLoading;
  const serviceEntryGateState = useMemo(
    () => getBookingServiceEntryGateState({
      serviceCatalogReady,
      serviceLocked,
      serviceOfferingId,
    }),
    [serviceCatalogReady, serviceLocked, serviceOfferingId],
  );
  const serviceFieldState = useMemo(
    () =>
      getBookingServiceFieldState({
        hasServiceCatalog: serviceTypes.length > 0,
        serviceCatalogReady,
        serviceLocked,
      }),
    [serviceCatalogReady, serviceLocked, serviceTypes.length],
  );
  const serviceFieldHelperText = selectedService
    ? [
        selectedService.defaultResources.length > 0
          ? `Recursos publicados: ${selectedService.defaultResources.map((resource) => resource.sdrResourceName).join(' · ')}`
          : 'Sin recursos predeterminados.',
        selectedService.requiresEngineer ? 'Requiere ingeniero.' : 'Ingeniero opcional.',
      ].join(' ')
    : serviceFieldState.helperText || 'Selecciona un servicio publicado.';
  const engineerFieldState = useMemo(
    () => getBookingEngineerFieldState({
      engineerCount: 1,
      hasAssignedEngineer: engineerPartyId != null || engineerName.trim() !== '',
      hasSelectedService: selectedService != null,
      requiresEngineer: selectedService?.requiresEngineer ?? false,
    }),
    [engineerName, engineerPartyId, selectedService],
  );
  const roomsFieldState = useMemo(
    () => getBookingRoomsFieldState({
      hasAssignedRooms: assignedRoomIds.length > 0,
      hasSelectedService: selectedService != null,
      roomCatalogLoading: roomsQuery.isLoading && roomsQuery.data == null,
      roomCount: rooms.length,
    }),
    [assignedRoomIds.length, rooms.length, roomsQuery.data, roomsQuery.isLoading, selectedService],
  );
  const optionalDetailsState = useMemo(
    () => getBookingOptionalDetailsState({
      mode,
      notes,
      status,
    }),
    [mode, notes, status],
  );
  const optionalDetailsExpanded = showOptionalDetails || optionalDetailsState.defaultExpanded;
  const conflictAlertText = useMemo(
    () => getBookingConflictAlertText(conflicts.map((conflict) => conflict.title)),
    [conflicts],
  );
  const missingEngineer = engineerFieldState.showField
    && Boolean(selectedService?.requiresEngineer)
    && !(engineerName.trim() || engineerPartyId);
  const handleCloseBookingDialog = useCallback(() => {
    setDialogOpen(false);
    setShowOptionalDetails(false);
  }, []);
  const openCreateContactDialog = useCallback(() => {
    setCreateContactError(null);
    setCreateContactOpen(true);
  }, []);
  const createPartyMutation = useMutation({
    mutationFn: (payload: PartyCreate) => Parties.create(payload),
    onSuccess: (party) => {
      setCustomerPartyId(party.partyId);
      setSelectedCustomer({ partyId: party.partyId, partyType: party.isOrg ? 'organization' : 'person', displayName: party.displayName, username: null, avatarUrl: null, secondaryLabel: 'Contacto nuevo', accountStatus: 'no-account' });
      setCreateContactOpen(false);
      setCreateContactForm({ name: '', email: '', phone: '' });
      setCreateContactError(null);
    },
    onError: (err) => setCreateContactError(err instanceof Error ? err.message : 'No se pudo crear el contacto.'),
  });

useEffect(() => {
  if (!serviceOfferingId || rooms.length === 0 || assignedRoomIds.length > 0) return;
  const defaults = defaultRoomsForService(serviceOfferingId);
  if (defaults.length) {
    setAssignedRoomIds(defaults.map((room) => room.roomId));
    setRoomsManuallyAdjusted(false);
  }
}, [serviceOfferingId, rooms, assignedRoomIds.length, defaultRoomsForService]);

useEffect(() => {
  if (serviceOfferingId || !defaultService) return;
  setServiceOfferingId(defaultService.id);
  const defaults = defaultRoomsForService(defaultService.id);
  if (defaults.length) {
    setAssignedRoomIds(defaults.map((room) => room.roomId));
    setRoomsManuallyAdjusted(false);
  }
}, [defaultRoomsForService, defaultService, serviceOfferingId]);

  const formatForInput = useCallback(
    (date: Date) => DateTime.fromJSDate(date, { zone }).toFormat("yyyy-LL-dd'T'HH:mm"),
    [zone],
  );

  useEffect(() => {
    if (prefillHandled || dialogOpen) return;
    try {
      const raw = typeof window !== 'undefined' ? window.sessionStorage.getItem('booking-prefill') : null;
      if (!raw) return;
      const parsed = JSON.parse(raw) as Partial<{ title?: string; startAt?: string; endAt?: string; notes?: string; hint?: string }>;
      if (parsed.startAt) setStartInput(formatForInput(new Date(parsed.startAt)));
      if (parsed.endAt) setEndInput(formatForInput(new Date(parsed.endAt)));
      if (parsed.title) setTitle(parsed.title);
      if (parsed.notes) setNotes(parsed.notes);
      setStatus('Tentative');
      setServiceOfferingId(defaultService?.id ?? '');
      setDialogOpen(true);
      setAutoAssignMessage('Datos precargados desde la última acción.');
      setPrefillNotice(true);
      if (typeof window !== 'undefined') {
        window.sessionStorage.removeItem('booking-prefill');
      }
    } catch {
      // ignore malformed prefill
    } finally {
      setPrefillHandled(true);
    }
  }, [defaultService, dialogOpen, formatForInput, prefillHandled]);

const openDialogForRange = (start: Date, end: Date) => {
  setStartInput(formatForInput(start));
  setEndInput(formatForInput(end));
  setShowOptionalDetails(false);
  setDialogOpen(true);
  setRoomsManuallyAdjusted(false);
};

  const handleDateClick = (info: { date: Date }) => {
    const start = info.date;
    const initialService = defaultService;
    const duration = initialService?.defaultDurationMinutes ?? 60;
    const end = DateTime.fromJSDate(start).plus({ minutes: duration }).toJSDate();
    setMode('create');
    setEditingId(null);
    setTitle('Bloque de estudio');
    setNotes('');
    setServiceOfferingId(initialService?.id ?? '');
    setEngineerName('');
    setEngineerPartyId(null);
    setSelectedEngineer(null);
    setCustomerPartyId(null);
    setSelectedCustomer(null);
    const defaults = defaultRoomsForService(initialService?.id ?? '');
    setAssignedRoomIds(defaults.map((room) => room.roomId));
    setStatus('Confirmed');
    openDialogForRange(start, end);
  };

  const handleSelect = (info: { start: Date; end: Date }) => {
    const initialService = defaultService;
    const defaultDuration = initialService?.defaultDurationMinutes ?? 60;
    setMode('create');
    setEditingId(null);
    setTitle('Bloque de estudio');
    setNotes('');
    setServiceOfferingId(initialService?.id ?? '');
    setEngineerName('');
    setEngineerPartyId(null);
    setSelectedEngineer(null);
    setCustomerPartyId(null);
    setSelectedCustomer(null);
    const defaults = defaultRoomsForService(initialService?.id ?? '');
    setAssignedRoomIds(defaults.map((room) => room.roomId));
    setStatus('Confirmed');
    openDialogForRange(
      info.start,
      info.end ?? DateTime.fromJSDate(info.start).plus({ minutes: defaultDuration }).toJSDate(),
    );
  };

  const toUtcIso = (value: string) => {
    const dt = DateTime.fromFormat(value, "yyyy-LL-dd'T'HH:mm", { zone });
    return dt.isValid ? dt.toUTC().toISO() : null;
  };

  const resetPrefill = () => {
    setStartInput('');
    setEndInput('');
    setTitle('Bloque de estudio');
    setNotes('');
    setAssignedRoomIds([]);
    setEngineerName('');
    setEngineerPartyId(null);
    setSelectedEngineer(null);
    setPrefillNotice(false);
    setAutoAssignMessage('');
  };

  const datetimeFormat = "yyyy-LL-dd'T'HH:mm";

  const openDuplicateModal = () => {
    const base = DateTime.fromFormat(startInput, datetimeFormat, { zone });
    const fallback = DateTime.now().setZone(zone).plus({ days: 1 });
    const suggested = base.isValid ? base.plus({ days: 7 }) : fallback;
    setDuplicateStartInput(suggested.toFormat(datetimeFormat));
    setDuplicateDialogOpen(true);
  };

  const confirmDuplicate = () => {
    const newStart = DateTime.fromFormat(duplicateStartInput, datetimeFormat, { zone });
    if (!newStart.isValid) {
      setFormError('Elige una fecha/hora válida para duplicar.');
      return;
    }
    const baseStart = DateTime.fromFormat(startInput, datetimeFormat, { zone });
    const baseEnd = DateTime.fromFormat(endInput, datetimeFormat, { zone });
    const durationMinutes =
      baseStart.isValid && baseEnd.isValid
        ? Math.max(15, Math.round(baseEnd.diff(baseStart, 'minutes').as('minutes')))
        : 60;
    const newEnd = newStart.plus({ minutes: durationMinutes });
    setStartInput(newStart.toFormat(datetimeFormat));
    setEndInput(newEnd.toFormat(datetimeFormat));
    setMode('create');
    setEditingId(null);
    setFormError(null);
    setPrefillNotice(false);
    setDuplicateDialogOpen(false);
  };

  const buildCombinedNotes = () => {
    const trimmed = notes.trim();
    // Keep notes purely for free text; engineer is now a first-class field.
    return trimmed === '' ? null : trimmed;
  };

  const createMutation = useMutation({
    mutationFn: () =>
      Bookings.create({
        cbTitle: title.trim() === '' ? 'Bloque de estudio' : title.trim(),
        cbStartsAt: toUtcIso(startInput) ?? '',
        cbEndsAt: toUtcIso(endInput) ?? '',
        cbStatus: status,
        cbNotes: buildCombinedNotes(),
        cbServiceOfferingId: serviceOfferingId,
        cbPartyId: customerPartyId,
        cbResourceIds: assignedRoomIds,
        cbEngineerPartyId: engineerPartyId,
        cbEngineerName: engineerName.trim() || null,
      }),
    onSuccess: () => {
      setDialogOpen(false);
      setShowOptionalDetails(false);
      setFormError(null);
      setTitle('Bloque de estudio');
      setNotes('');
      setServiceOfferingId('');
      setStatus('Confirmed');
      setEditingId(null);
      setMode('create');
      setAssignedRoomIds([]);
      setEngineerName('');
      setEngineerPartyId(null);
      setSelectedEngineer(null);
      setCustomerPartyId(null);
      setSelectedCustomer(null);
      setPrefillNotice(false);
      setAutoAssignMessage('');
      void qc.invalidateQueries({ queryKey: ['bookings'] });
    },
    onError: (err) => {
      setFormError(err instanceof Error ? err.message : 'No se pudo crear la sesión.');
    },
  });

  const updateMutation = useMutation({
    mutationFn: (payload: { id: number; body: Record<string, unknown> }) =>
      Bookings.update(payload.id, payload.body),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['bookings'] });
      setDialogOpen(false);
      setShowOptionalDetails(false);
      setEditingId(null);
      setMode('create');
      setFormError(null);
      setPrefillNotice(false);
      setAutoAssignMessage('');
    },
    onError: (err) => {
      setFormError(err instanceof Error ? err.message : 'No se pudo actualizar la sesión.');
    },
  });

  const handleCreate = (evt: React.FormEvent) => {
    evt.preventDefault();
    const startIso = toUtcIso(startInput);
    const endIso = toUtcIso(endInput);
    if (!startIso || !endIso) {
      setFormError('Revisa las fechas seleccionadas.');
      return;
    }
    if (selectedService?.id !== serviceOfferingId) {
      setFormError('Selecciona un servicio para la sesión.');
      return;
    }
    if (DateTime.fromISO(endIso) <= DateTime.fromISO(startIso)) {
      setFormError('La hora de fin debe ser mayor que la de inicio.');
      return;
    }
    if (conflicts.length > 0) {
      const titles = conflicts.map((c) => c.title ?? 'otra reserva');
      const list = titles.slice(0, 3).join(', ');
      setFormError(`Conflicto de horario con: ${list}. Ajusta la hora o las salas.`);
      return;
    }
    if (!customerPartyId) {
      setFormError('Selecciona un cliente para la sesión.');
      return;
    }
    if (rooms.length === 0) {
      setFormError('Todavía no hay salas registradas. Abre Salas y recursos antes de guardar la sesión.');
      return;
    }
    if (assignedRoomIds.length === 0) {
      setFormError('Asigna al menos una sala para la sesión.');
      return;
    }
    const combinedNotes = buildCombinedNotes();
    if (mode === 'edit' && editingId) {
      updateMutation.mutate({
        id: editingId,
        body: {
          ubTitle: title.trim(),
          ubServiceOfferingId: serviceOfferingId,
          ubNotes: combinedNotes,
          ubStatus: status,
          ubStartsAt: startIso,
          ubEndsAt: endIso,
          ubEngineerPartyId: engineerPartyId,
          ubEngineerName: engineerName.trim() || null,
        },
      });
    } else {
      createMutation.mutate();
    }
  };

  const openBooking = useCallback(
    (booking: BookingDTO) => {
      setMode('edit');
      setEditingId(booking.bookingId);
      setManualReviewNotes('');
      setTitle(booking.title ?? 'Sesión');
      const parsedNotes = extractEngineerFromNotes(booking.notes);
      const engineerFromBooking = booking.engineerName ?? parsedNotes.engineer;
      const engineerIdFromBooking = booking.engineerPartyId ?? parsedNotes.engineerId;
      setNotes(parsedNotes.notesBody);
      setEngineerName(engineerFromBooking);
      setEngineerPartyId(engineerIdFromBooking);
      setSelectedEngineer(engineerIdFromBooking ? { partyId: engineerIdFromBooking, partyType: 'person', displayName: engineerFromBooking || 'Ingeniero asignado', username: null, avatarUrl: null, secondaryLabel: 'Asignación existente', accountStatus: 'no-account' } : null);
      setCustomerPartyId(booking.partyId ?? null);
      const customerLabel = booking.customerName
        ?? booking.partyDisplayName
        ?? '';
      setSelectedCustomer(booking.partyId ? { partyId: booking.partyId, partyType: 'person', displayName: customerLabel || 'Cliente asignado', username: null, avatarUrl: null, secondaryLabel: 'Reserva existente', accountStatus: 'no-account' } : null);
      setServiceOfferingId(booking.serviceOfferingId ?? '');
      setServiceLocked(Boolean(booking.courseSlug));
      setStatus(booking.status ?? 'Confirmed');
      setStartInput(formatForInput(new Date(booking.startsAt)));
      setEndInput(formatForInput(new Date(booking.endsAt)));
      setAssignedRoomIds((booking.resources ?? []).map((r) => r.brRoomId));
      const baseStart = DateTime.fromISO(booking.startsAt).setZone(zone);
      const suggestedDuplicate = baseStart.isValid ? baseStart.plus({ days: 7 }) : DateTime.now().setZone(zone);
      setDuplicateStartInput(suggestedDuplicate.toFormat("yyyy-LL-dd'T'HH:mm"));
      setDialogOpen(true);
    },
    [formatForInput, zone],
  );

  useEffect(() => {
    if (autoOpenHandled.current) return;
    if (!bookingIdFilter) return;
    if (bookings.length === 0) return;
    const booking = bookings.find((b) => b.bookingId === bookingIdFilter);
    if (!booking) return;
    autoOpenHandled.current = true;
    const start = new Date(booking.startsAt);
    if (!Number.isNaN(start.getTime())) {
      (calendarRef.current as unknown as { getApi?: () => { gotoDate?: (date: Date) => void } } | null)
        ?.getApi?.()
        ?.gotoDate?.(start);
    }
    openBooking(booking);
  }, [bookingIdFilter, bookings, openBooking]);

  const handleEventClick = (info: {
    event: {
      id: string;
      title?: string;
      extendedProps?: Record<string, unknown>;
      start?: Date | null;
      end?: Date | null;
    };
  }) => {
    const ext = info.event.extendedProps ?? {};
    if (ext['isCourse']) {
      handleCloseBookingDialog();
      const slug = (ext['courseSlug'] as string | undefined) ?? undefined;
      const shareUrl =
        slug && typeof window !== 'undefined' ? `${window.location.origin}/inscripcion/${slug}` : undefined;
      setCourseReadOnlyInfo({
        title: info.event.title ?? 'Bloque de curso',
        range: formatEventRange(info.event.start ?? null, info.event.end ?? null),
        subtitle: (ext['courseSubtitle'] as string | undefined) ?? undefined,
        price: (ext['priceText'] as string | undefined) ?? undefined,
        location: (ext['locationText'] as string | undefined) ?? undefined,
        slug,
        shareUrl,
      });
      setCourseNotice('Los bloques del curso son de solo lectura. Revisa los detalles del horario aquí.');
      return;
    }
    const bookingId = parsePositiveInt(info.event.id);
    if (bookingId == null) return;
    const booking = bookings.find((b) => b.bookingId === bookingId);
    if (!booking) return;
    openBooking(booking);
  };

  const handleEventDropOrResize = (arg: { event: { id: string; start: Date | null; end: Date | null; extendedProps?: { isCourse?: boolean } }; revert?: () => void }) => {
    if (arg.event.extendedProps?.isCourse) {
      arg.revert?.();
      return;
    }
    const bookingId = parsePositiveInt(arg.event.id);
    if (bookingId == null) {
      arg.revert?.();
      return;
    }
    if (!arg.event.start || !arg.event.end) return;
    const startIso = toUtcIso(formatForInput(arg.event.start));
    const endIso = toUtcIso(formatForInput(arg.event.end));
    if (!startIso || !endIso) return;
    updateMutation.mutate(
      {
        id: bookingId,
        body: {
          ubStartsAt: startIso,
          ubEndsAt: endIso,
        },
      },
      {
        onError: (err) => {
          setCalendarError(err instanceof Error ? err.message : 'No pudimos mover la sesión.');
          arg.revert?.();
        },
        onSuccess: () => {
          setCalendarError(null);
        },
      },
    );
  };
  const handleCreateFirstSession = () => {
    const start = DateTime.now().setZone(zone).plus({ hours: 1 }).startOf('hour').toJSDate();
    handleDateClick({ date: start });
  };
  const showCalendar = calendarStatusState?.showCalendar ?? true;

  return (
    <>
      <Typography variant="h5" gutterBottom>Agenda</Typography>
      {courseNotice && (
        <Alert severity="info" sx={{ mb: 1 }} onClose={() => setCourseNotice(null)}>
          {courseNotice}
        </Alert>
      )}
      {calendarError && <Alert severity="warning" sx={{ mb: 1 }}>{calendarError}</Alert>}
      {calendarStatusState && showCalendar && (
        <Alert
          severity={calendarStatusState.severity}
          sx={{ mb: 1 }}
          action={calendarStatusState.clearFilterActionLabel ? (
            <Button color="inherit" size="small" onClick={handleClearBookingFilters}>
              {calendarStatusState.clearFilterActionLabel}
            </Button>
          ) : undefined}
        >
          {calendarStatusState.message}
        </Alert>
      )}
      {bookingsQuery.error && <Alert severity="error" sx={{ mb: 1 }}>Error al cargar agenda: {bookingsQuery.error.message}</Alert>}
      {!showCalendar && calendarStatusState ? (
        <Paper variant="outlined" sx={{ p: 3 }}>
          <Stack spacing={1.5} alignItems="flex-start">
            {calendarStatusState.title && (
              <Typography variant="h6">{calendarStatusState.title}</Typography>
            )}
            <Typography variant="body2" color="text.secondary">
              {calendarStatusState.message}
            </Typography>
            {calendarStatusState.primaryActionLabel && (
              calendarStatusState.primaryActionHref ? (
                <Button variant="contained" component={RouterLink} to={calendarStatusState.primaryActionHref}>
                  {calendarStatusState.primaryActionLabel}
                </Button>
              ) : (
                <Button variant="contained" onClick={handleCreateFirstSession}>
                  {calendarStatusState.primaryActionLabel}
                </Button>
              )
            )}
            {calendarStatusState.clearFilterActionLabel && (
              <Button variant="outlined" onClick={handleClearBookingFilters}>
                {calendarStatusState.clearFilterActionLabel}
              </Button>
            )}
          </Stack>
        </Paper>
      ) : (
        <Paper sx={{ p: 1 }}>
          <FullCalendar
            ref={calendarRef}
            plugins={[dayGridPlugin, timeGridPlugin, interactionPlugin]}
            initialView="timeGridWeek"
            height="auto"
            allDaySlot={false}
            slotDuration="00:30:00"
            editable
            selectable
            selectMirror
            select={handleSelect}
            dateClick={handleDateClick}
            eventClick={handleEventClick}
            eventDrop={handleEventDropOrResize}
            eventResize={handleEventDropOrResize}
            eventClassNames={(arg) => {
              const ext = (arg.event.extendedProps ?? {}) as Record<string, unknown>;
              return ext['isCourse'] ? ['course-event'] : [];
            }}
            eventContent={(arg) => {
              const ext = (arg.event.extendedProps ?? {}) as Record<string, unknown>;
              const isCourse = Boolean(ext['isCourse']);
              const courseSubtitle = (ext['courseSubtitle'] as string | undefined) ?? undefined;
              const priceText = (ext['priceText'] as string | undefined) ?? undefined;
              const locationText = (ext['locationText'] as string | undefined) ?? undefined;
              const engineerLabel = (ext['engineerName'] as string | undefined) ?? undefined;
              return (
                <div style={{ display: 'flex', flexDirection: 'column', gap: 4 }}>
                  <div style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
                    {isCourse && (
                      <span
                        style={{
                          background: 'rgba(255,255,255,0.22)',
                          color: 'inherit',
                          fontSize: 11,
                          fontWeight: 700,
                          padding: '2px 8px',
                          borderRadius: 999,
                        }}
                      >
                        Curso
                      </span>
                    )}
                    <span>{arg.event.title}</span>
                  </div>
                  {isCourse && (
                    <span style={{ fontSize: 11, color: 'inherit', opacity: 0.9 }}>
                      {[courseSubtitle, priceText, locationText].filter(Boolean).join(' · ')}
                    </span>
                  )}
                  {engineerLabel && (
                    <span style={{ fontSize: 11, color: 'inherit', opacity: 0.9 }}>
                      Ingeniero: {engineerLabel}
                    </span>
                  )}
                </div>
              );
            }}
            events={events}
            nowIndicator
            timeZone={zone}
            locale={locale}
            headerToolbar={{
              left: 'prev,next today',
              center: 'title',
              right: 'dayGridMonth,timeGridWeek,timeGridDay'
            }}
          />
        </Paper>
      )}

      <Dialog open={Boolean(courseReadOnlyInfo)} onClose={() => { setCourseReadOnlyInfo(null); setCourseNotice(null); }} maxWidth="xs" fullWidth>
        <DialogTitle>Bloque de curso</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={1}>
            <Typography variant="subtitle1" fontWeight={700}>
              {courseReadOnlyInfo?.title ?? 'Curso'}
            </Typography>
            {courseReadOnlyInfo?.range && (
              <Typography variant="body2" color="text.secondary">
                {courseReadOnlyInfo.range}
              </Typography>
            )}
            {[courseReadOnlyInfo?.subtitle, courseReadOnlyInfo?.location, courseReadOnlyInfo?.price]
              .filter(Boolean)
              .map((line) => (
                <Typography key={line} variant="body2">
                  {line}
                </Typography>
              ))}
            <Alert severity="info" variant="outlined">
              Este bloque es de solo lectura. Para editarlo, ajusta el calendario del curso.
            </Alert>
            {Boolean(courseReadOnlyInfo?.slug ?? courseReadOnlyInfo?.shareUrl) && (
              <Stack direction="row" spacing={1}>
                {courseReadOnlyInfo?.slug && (
                  <Button
                    variant="contained"
                    size="small"
                    component="a"
                    href={`/inscripcion/${courseReadOnlyInfo.slug}`}
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    Abrir landing
                  </Button>
                )}
                {courseReadOnlyInfo?.shareUrl && (
                  <Button
                    variant="outlined"
                    size="small"
                    onClick={() => {
                      void (async () => {
                        try {
                          await navigator.clipboard.writeText(courseReadOnlyInfo.shareUrl ?? '');
                          setCourseNotice('Link copiado. Compártelo con estudiantes desde aquí.');
                        } catch {
                          setCourseNotice('No pudimos copiar el link. Intenta de nuevo.');
                        }
                      })();
                    }}
                  >
                    Copiar link
                  </Button>
                )}
              </Stack>
            )}
            <Button
              variant="text"
              size="small"
              component={RouterLink}
              to="/estudio/live-sessions"
            >
              Abrir gestión de cursos
            </Button>
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => { setCourseReadOnlyInfo(null); setCourseNotice(null); }} color="inherit">
            Entendido
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog open={createContactOpen} onClose={() => { if (!createPartyMutation.isPending) setCreateContactOpen(false); }} maxWidth="xs" fullWidth>
        <DialogTitle>Nuevo contacto</DialogTitle>
        <DialogContent>
          <Stack spacing={2} sx={{ pt: 1 }}>
            <TextField
              label="Nombre completo"
              value={createContactForm.name}
              onChange={(e) => setCreateContactForm((prev) => ({ ...prev, name: e.target.value }))}
              required
            />
            <TextField
              label="Correo"
              type="email"
              value={createContactForm.email}
              onChange={(e) => setCreateContactForm((prev) => ({ ...prev, email: e.target.value }))}
            />
            <TextField
              label="Teléfono"
              value={createContactForm.phone}
              onChange={(e) => setCreateContactForm((prev) => ({ ...prev, phone: e.target.value }))}
            />
            {createContactError && <Alert severity="error">{createContactError}</Alert>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button
            onClick={() => {
              setCreateContactOpen(false);
              setCreateContactError(null);
            }}
            disabled={createPartyMutation.isPending}
          >
            Cancelar
          </Button>
          <Button
            variant="contained"
            onClick={() => {
              const name = createContactForm.name.trim();
              if (!name) {
                setCreateContactError('Agrega un nombre para el contacto.');
                return;
              }
              const payload: PartyCreate = {
                cDisplayName: name,
                cIsOrg: false,
                cPrimaryEmail: createContactForm.email.trim() || null,
                cPrimaryPhone: createContactForm.phone.trim() || null,
              };
              createPartyMutation.mutate(payload);
            }}
            disabled={createPartyMutation.isPending}
          >
            Crear y asignar
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog open={dialogOpen} onClose={handleCloseBookingDialog} maxWidth="sm" fullWidth>
        <DialogTitle>
          {mode === 'edit' ? 'Editar sesión' : 'Nueva sesión en el calendario'}
          {startInput && (
            <Typography variant="caption" color="text.secondary" sx={{ display: 'block' }}>
              {formatEventRange(new Date(startInput), endInput ? new Date(endInput) : null)}
            </Typography>
          )}
        </DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2} component="form" onSubmit={handleCreate}>
            {formError && <Alert severity="error">{formError}</Alert>}
            {prefillNotice && (
              <Alert
                severity="info"
                action={
                  <Button color="inherit" size="small" onClick={resetPrefill}>
                    Deshacer
                  </Button>
                }
              >
                Datos precargados desde la última acción.
              </Alert>
            )}
            <TextField
              label="Título"
              value={title}
              onChange={(e) => setTitle(e.target.value)}
              fullWidth
            />
            <PartySelector
              value={selectedCustomer}
              onChange={(party) => {
                setSelectedCustomer(party);
                setCustomerPartyId(party?.partyId ?? null);
              }}
              field={{ label: 'Cliente', required: true, helperText: 'Busca por nombre o @username. El texto escrito no se guarda como cliente.' }}
              search={{ context: 'booking', kind: 'any', accountOnly: false }}
            />
            <Button variant="outlined" size="small" onClick={openCreateContactDialog} sx={{ alignSelf: { xs: 'stretch', sm: 'flex-start' } }}>
              Crear contacto nuevo
            </Button>
            <TextField
              label="Inicio"
              type="datetime-local"
              value={startInput}
              onChange={(e) => {
                setStartInput(e.target.value);
              }}
              fullWidth
              InputLabelProps={{ shrink: true }}
            />
            <TextField
              label="Fin"
              type="datetime-local"
              value={endInput}
              onChange={(e) => {
                setEndInput(e.target.value);
              }}
              fullWidth
              InputLabelProps={{ shrink: true }}
            />
            {serviceEntryGateState.showServiceField ? (
              <>
                <TextField
                    select
                    label="Servicio"
                    value={serviceOfferingId}
                    disabled={serviceLocked}
                    onChange={(e) => {
                      const value = e.target.value;
                      const service = serviceTypes.find((candidate) => candidate.id === value);
                      if (!service) return;
                      const wasRoomsManual = roomsManuallyAdjusted;
                      setRoomsManuallyAdjusted(false);
                      setServiceOfferingId(service.id);
                      const messageParts: string[] = [];
                      if (!wasRoomsManual || assignedRoomIds.length === 0) {
                        const defaults = defaultRoomsForService(service.id);
                        if (defaults.length) {
                          setAssignedRoomIds(defaults.map((room) => room.roomId));
                          messageParts.push(`Salas sugeridas: ${defaults.map((r) => r.rName).join(' + ')}`);
                          setRoomsManuallyAdjusted(false);
                        }
                      }
                      setAutoAssignMessage(messageParts.join(' · '));
                    }}
                    helperText={serviceFieldHelperText}
                  >
                    <MenuItem value="">(Sin asignar)</MenuItem>
                    {serviceTypes.map((svc) => (
                      <MenuItem key={svc.id} value={svc.id}>
                        {formatServiceLabel(svc)}
                      </MenuItem>
                    ))}
                </TextField>
                {serviceLocked && (
                  <Alert severity="info" variant="outlined">
                    Este servicio está sincronizado con un curso/prueba y no se puede cambiar aquí.
                  </Alert>
                )}
              </>
            ) : (
              <Alert severity="info" variant="outlined">
                {serviceEntryGateState.helperText}
              </Alert>
            )}
            <Stack spacing={0.75}>
              <Stack
                direction={{ xs: 'column', sm: 'row' }}
                spacing={1}
                justifyContent="space-between"
                alignItems={{ sm: 'center' }}
              >
                <div>
                  <Typography variant="subtitle2">Notas y estado</Typography>
                  {!optionalDetailsExpanded && (
                    <Typography variant="body2" color="text.secondary">
                      {optionalDetailsState.collapsedHelperText}
                    </Typography>
                  )}
                </div>
                {!optionalDetailsState.defaultExpanded && (
                  <Button
                    variant={optionalDetailsExpanded ? 'text' : 'outlined'}
                    size="small"
                    onClick={() => setShowOptionalDetails((current) => !current)}
                    sx={{ alignSelf: { xs: 'stretch', sm: 'center' } }}
                  >
                    {optionalDetailsExpanded ? 'Ocultar' : optionalDetailsState.toggleLabel}
                  </Button>
                )}
              </Stack>
              <Collapse in={optionalDetailsExpanded} unmountOnExit>
                <Stack spacing={2} sx={{ pt: 0.5 }}>
                  <TextField
                    label="Notas (opcional)"
                    value={notes}
                    onChange={(e) => setNotes(e.target.value)}
                    fullWidth
                    multiline
                    minRows={2}
                  />
                  <FormControl>
                    <InputLabel id="booking-status-label">Estado</InputLabel>
                    <Select
                      labelId="booking-status-label"
                      label="Estado"
                      value={status}
                      onChange={(e) => setStatus(e.target.value)}
                    >
                      {statusOptions.map((option) => (
                        <MenuItem key={option} value={option}>
                          {option}
                        </MenuItem>
                      ))}
                    </Select>
                  </FormControl>
                </Stack>
              </Collapse>
            </Stack>
            {conflictAlertText && (
              <Alert severity="warning" variant="outlined">
                {conflictAlertText}
              </Alert>
            )}
            {serviceEntryGateState.showDependentFields && missingEngineer && (
              <Alert severity="warning">
                Este servicio normalmente usa un ingeniero. Asigna uno o continúa bajo tu criterio.
              </Alert>
            )}
            {serviceEntryGateState.showDependentFields ? (
              engineerFieldState.showField ? (
                <PartySelector
                  value={selectedEngineer}
                  onChange={(party) => {
                    setSelectedEngineer(party);
                    setEngineerPartyId(party?.partyId ?? null);
                    setEngineerName(party?.displayName ?? '');
                  }}
                  field={{ label: engineerFieldState.label, helperText: engineerFieldState.helperText }}
                  search={{ context: 'booking', kind: 'person', accountOnly: false }}
                />
              ) : engineerFieldState.helperText ? (
                <Alert severity="info" variant="outlined">
                  {engineerFieldState.helperText}
                </Alert>
              ) : null
            ) : null}
            {serviceEntryGateState.showDependentFields ? (
              roomsFieldState.showField ? (
                <Autocomplete
                  multiple
                  options={rooms}
                  getOptionLabel={(option) => option.rName}
                  value={assignedRooms}
                  onChange={(_, value) => {
                    setAssignedRoomIds(value.map((room) => room.roomId));
                    setRoomsManuallyAdjusted(true);
                  }}
                  renderTags={(value, getTagProps) =>
                    value.map((option, index) => (
                      <Chip {...getTagProps({ index })} key={option.roomId} label={option.rName} />
                    ))
                  }
                  renderInput={(params) => (
                    <TextField
                      {...params}
                      label="Salas asignadas"
                      placeholder="Agregar/ajustar salas"
                      helperText={roomsFieldState.helperText}
                    />
                  )}
                  noOptionsText="No hay salas registradas"
                />
              ) : (
                <Alert
                  severity="info"
                  variant="outlined"
                  action={roomsFieldState.setupActionLabel ? (
                    <Button
                      color="inherit"
                      size="small"
                      component={RouterLink}
                      to="/estudio/salas"
                    >
                      {roomsFieldState.setupActionLabel}
                    </Button>
                  ) : undefined}
                >
                  {roomsFieldState.helperText}
                </Alert>
              )
            ) : null}
            {autoAssignMessage && (
              <Typography variant="caption" color="primary">
                {autoAssignMessage}
              </Typography>
            )}
            {bookingCommerceQuery.data && (
              <Paper variant="outlined" sx={{ p: 2 }}>
                <Stack spacing={1.25}>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                    <Chip label={`Pago: ${bookingCommerceQuery.data.paymentStatus}`} size="small" />
                    <Chip label={`Prestación: ${bookingCommerceQuery.data.fulfillmentStatus}`} size="small" />
                    <Chip
                      label={`Depósito: ${bookingCommerceQuery.data.currency} ${(bookingCommerceQuery.data.depositMinor / 100).toFixed(2)}`}
                      size="small"
                    />
                  </Stack>
                  {bookingCommerceQuery.data.manualEvidence && (
                    <>
                      <Alert
                        severity={bookingCommerceQuery.data.manualEvidence.status === 'rejected' ? 'warning' : 'info'}
                        variant="outlined"
                      >
                        Transferencia: <strong>{bookingCommerceQuery.data.manualEvidence.status}</strong>
                        {bookingCommerceQuery.data.manualEvidence.customerReference
                          ? <> · Referencia: <strong>{bookingCommerceQuery.data.manualEvidence.customerReference}</strong></>
                          : null}
                      </Alert>
                      {['submitted', 'under_review'].includes(bookingCommerceQuery.data.manualEvidence.status) && (
                        <>
                          <TextField
                            label="Notas de revisión financiera"
                            value={manualReviewNotes}
                            onChange={(event) => setManualReviewNotes(event.target.value)}
                            inputProps={{ maxLength: 2000 }}
                            helperText="Compara importe, moneda y referencia con el estado bancario. No pegues credenciales ni datos completos de cuenta."
                            multiline
                            minRows={2}
                            fullWidth
                          />
                          {manualReviewMutation.isError && (
                            <Alert severity="error">
                              No se pudo registrar la decisión. La evidencia y el pago conservaron su estado anterior.
                            </Alert>
                          )}
                          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                            <Button
                              variant="contained"
                              color="success"
                              disabled={manualReviewMutation.isPending || manualReviewNotes.trim().length < 3}
                              onClick={() => manualReviewMutation.mutate('approve')}
                            >
                              Aprobar depósito verificado
                            </Button>
                            <Button
                              variant="outlined"
                              color="warning"
                              disabled={manualReviewMutation.isPending || manualReviewNotes.trim().length < 3}
                              onClick={() => manualReviewMutation.mutate('reject')}
                            >
                              Rechazar evidencia
                            </Button>
                          </Stack>
                        </>
                      )}
                    </>
                  )}
                </Stack>
              </Paper>
            )}
          </Stack>
        </DialogContent>
        <DialogActions sx={{ px: 3, pb: 2 }}>
          <Button onClick={handleCloseBookingDialog}>Cancelar</Button>
          {mode === 'edit' && (
            <Button onClick={openDuplicateModal} color="inherit">
              Duplicar
            </Button>
          )}
          <Button
            variant="contained"
            onClick={handleCreate}
            disabled={createMutation.isPending || updateMutation.isPending}
            sx={{ textTransform: 'none' }}
          >
            {createMutation.isPending || updateMutation.isPending
              ? 'Guardando…'
              : mode === 'edit'
                ? 'Actualizar'
                : 'Crear sesión'}
          </Button>
        </DialogActions>
      </Dialog>
      <Dialog open={duplicateDialogOpen} onClose={() => setDuplicateDialogOpen(false)} maxWidth="xs" fullWidth>
        <DialogTitle>Duplicar sesión</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={1.5}>
            <TextField
              label="Nuevo inicio"
              type="datetime-local"
              value={duplicateStartInput}
              onChange={(e) => setDuplicateStartInput(e.target.value)}
              fullWidth
              InputLabelProps={{ shrink: true }}
            />
            <Typography variant="body2" color="text.secondary">
              Mantendremos salas, cliente e ingeniero. Ajusta la hora final conservando la duración original.
            </Typography>
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDuplicateDialogOpen(false)}>Cancelar</Button>
          <Button variant="contained" onClick={confirmDuplicate}>
            Aplicar
          </Button>
        </DialogActions>
      </Dialog>
    </>
  );
}
