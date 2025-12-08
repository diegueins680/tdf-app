import { useCallback, useEffect, useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient, type UseQueryResult } from '@tanstack/react-query';
import { Bookings } from '../api/bookings';
import type { BookingDTO, PartyCreate, PartyDTO } from '../api/types';
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
  MenuItem,
  FormControl,
  InputLabel,
  Select,
  Autocomplete,
  Chip,
  Box,
} from '@mui/material';
import FullCalendar from '@fullcalendar/react';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import interactionPlugin from '@fullcalendar/interaction';
import { DateTime } from 'luxon';
import { loadServiceTypes } from '../utils/serviceTypesStore';
import { Rooms } from '../api/rooms';
import type { RoomDTO } from '../api/types';
import { Parties } from '../api/parties';

// FullCalendar v6 auto-injects its styles when the modules load, so importing the
// CSS bundles directly is unnecessary and breaks with Vite due to missing files.

export default function BookingsPage() {
  const bookingsQuery: UseQueryResult<BookingDTO[], Error> = useQuery<BookingDTO[], Error>({
    queryKey: ['bookings'],
    queryFn: Bookings.list,
  });
  const roomsQuery = useQuery<RoomDTO[]>({
    queryKey: ['rooms'],
    queryFn: Rooms.list,
    staleTime: 5 * 60 * 1000,
  });
  const partiesQuery = useQuery<PartyDTO[]>({
    queryKey: ['parties', 'all'],
    queryFn: () => Parties.list(),
    staleTime: 5 * 60 * 1000,
  });
  const qc = useQueryClient();
  const zone = (import.meta.env['VITE_TZ'] as string | undefined) ?? 'America/Guayaquil';
  const bookings = useMemo<BookingDTO[]>(() => bookingsQuery.data ?? [], [bookingsQuery.data]);
  const rooms = useMemo<RoomDTO[]>(() => roomsQuery.data ?? [], [roomsQuery.data]);
  const parties = useMemo<PartyDTO[]>(() => partiesQuery.data ?? [], [partiesQuery.data]);
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
    const startStr = DateTime.fromJSDate(start).setZone(zone).toFormat('ccc d LLL, HH:mm');
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
        const engineerMeta = extractEngineerFromNotes(booking.notes);
        const isCourse =
          Boolean(booking.courseSlug) ||
          (booking.bookingId ?? 0) < 0 ||
          (booking.serviceType ?? '').toLowerCase().includes('curso');
        const courseCapacity = booking.courseCapacity ?? undefined;
        const courseRemaining = booking.courseRemaining ?? undefined;
        const coursePrice = booking.coursePrice ?? undefined;
        const courseLocation = booking.courseLocation ?? undefined;
        const courseSubtitle = courseCapacity
          ? `Cupos: ${Math.max(0, courseRemaining ?? 0)}/${courseCapacity}`
          : null;
        const priceText = coursePrice ? `USD ${Math.round(coursePrice)}` : null;
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
    [bookings],
  );

  const [dialogOpen, setDialogOpen] = useState(false);
  const [mode, setMode] = useState<'create' | 'edit'>('create');
  const [editingId, setEditingId] = useState<number | null>(null);
  const [title, setTitle] = useState('Bloque de estudio');
  const [notes, setNotes] = useState('');
  const [startInput, setStartInput] = useState('');
  const [endInput, setEndInput] = useState('');
  const [formError, setFormError] = useState<string | null>(null);
  const [serviceType, setServiceType] = useState<string>('');
  const [engineerName, setEngineerName] = useState('');
  const [engineerPartyId, setEngineerPartyId] = useState<number | null>(null);
  const [customerPartyId, setCustomerPartyId] = useState<number | null>(null);
  const [customerName, setCustomerName] = useState('');
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
  const serviceTypes = useMemo(() => loadServiceTypes(), []);

  const requiresEngineer = (svc: string) => {
    const lowered = svc.toLowerCase();
    return ['recording', 'grabacion', 'grabación', 'mezcla', 'mixing', 'master', 'mastering'].some((keyword) =>
      lowered.includes(keyword),
    );
  };

  const categorizeRooms = useMemo(() => {
    const map = {
      djBooth: [] as RoomDTO[],
      liveRoom: [] as RoomDTO[],
      controlRoom: [] as RoomDTO[],
      vocalBooth: [] as RoomDTO[],
      other: [] as RoomDTO[],
    };
    rooms.forEach((room) => {
      const name = room.rName.toLowerCase();
      if (name.includes('dj')) {
        map.djBooth.push(room);
      } else if (name.includes('live')) {
        map.liveRoom.push(room);
      } else if (name.includes('control')) {
        map.controlRoom.push(room);
      } else if (name.includes('vocal')) {
        map.vocalBooth.push(room);
      } else {
        map.other.push(room);
      }
    });
    return map;
  }, [rooms]);

  const pickFirst = (list: RoomDTO[]) => (list.length > 0 ? [list[0]!] : []);

  const defaultRoomsForService = useCallback((svc: string): RoomDTO[] => {
    const lowered = svc.toLowerCase();
    if (lowered.includes('dj')) {
      const defaults = pickFirst(categorizeRooms.djBooth);
      return defaults.length ? defaults : pickFirst(categorizeRooms.other);
    }
    if (lowered.includes('band') && lowered.includes('record')) {
      return [...categorizeRooms.liveRoom.slice(0, 1), ...categorizeRooms.controlRoom.slice(0, 1)];
    }
    if (lowered.includes('vocal') && lowered.includes('record')) {
      return [...categorizeRooms.vocalBooth.slice(0, 1), ...categorizeRooms.controlRoom.slice(0, 1)];
    }
    if (lowered.includes('rehearsal') || lowered.includes('ensayo') || (lowered.includes('band') && lowered.includes('rehe'))) {
      return categorizeRooms.liveRoom.slice(0, 1);
    }
    if (lowered.includes('mix') || lowered.includes('master')) {
      return categorizeRooms.controlRoom.slice(0, 1);
    }
    if (lowered.includes('record')) {
      return [...categorizeRooms.controlRoom.slice(0, 1), ...categorizeRooms.liveRoom.slice(0, 1)];
    }
    return categorizeRooms.other.slice(0, 1);
  }, [categorizeRooms]);

  const assignedRooms = useMemo(
    () => rooms.filter((room) => assignedRoomIds.includes(room.roomId)),
    [rooms, assignedRoomIds],
  );

  const engineerOptions = useMemo(
    () =>
      parties.filter((party) =>
        (party.roles ?? []).some((role) => role.toLowerCase().includes('engineer')),
      ),
    [parties],
  );
  const customerOptions = parties;
  const missingEngineer = requiresEngineer(serviceType) && !(engineerName.trim() || engineerPartyId);
  const createPartyMutation = useMutation({
    mutationFn: (payload: PartyCreate) => Parties.create(payload),
    onSuccess: (party) => {
      setCustomerPartyId(party.partyId);
      setCustomerName(party.displayName);
      setCreateContactOpen(false);
      setCreateContactForm({ name: '', email: '', phone: '' });
      setCreateContactError(null);
      void qc.invalidateQueries({ queryKey: ['parties'] });
    },
    onError: (err) => setCreateContactError(err instanceof Error ? err.message : 'No se pudo crear el contacto.'),
  });

  useEffect(() => {
    if (!serviceType || rooms.length === 0 || assignedRoomIds.length > 0) return;
    const defaults = defaultRoomsForService(serviceType);
    if (defaults.length) {
      setAssignedRoomIds(defaults.map((room) => room.roomId));
    }
  }, [serviceType, rooms, assignedRoomIds.length, defaultRoomsForService]);

  const formatForInput = (date: Date) =>
    DateTime.fromJSDate(date, { zone }).toFormat("yyyy-LL-dd'T'HH:mm");

  const openDialogForRange = (start: Date, end: Date) => {
    setStartInput(formatForInput(start));
    setEndInput(formatForInput(end));
    setDialogOpen(true);
  };

  const handleDateClick = (info: { date: Date }) => {
    const start = info.date;
    const end = DateTime.fromJSDate(start).plus({ minutes: 60 }).toJSDate();
    setMode('create');
    setEditingId(null);
    setTitle('Bloque de estudio');
    setNotes('');
    setServiceType('');
    setEngineerName('');
    setCustomerName('');
    setCustomerPartyId(null);
    const defaults = defaultRoomsForService('');
    setAssignedRoomIds(defaults.map((room) => room.roomId));
    setStatus('Confirmed');
    openDialogForRange(start, end);
  };

  const handleSelect = (info: { start: Date; end: Date }) => {
    setMode('create');
    setEditingId(null);
    setTitle('Bloque de estudio');
    setNotes('');
    setServiceType('');
    setEngineerName('');
    setCustomerName('');
    setCustomerPartyId(null);
    const defaults = defaultRoomsForService('');
    setAssignedRoomIds(defaults.map((room) => room.roomId));
    setStatus('Confirmed');
    openDialogForRange(info.start, info.end ?? DateTime.fromJSDate(info.start).plus({ minutes: 60 }).toJSDate());
  };

  const toUtcIso = (value: string) => {
    const dt = DateTime.fromFormat(value, "yyyy-LL-dd'T'HH:mm", { zone });
    return dt.isValid ? dt.toUTC().toISO() : null;
  };

  const buildCombinedNotes = () => {
    const trimmed = notes.trim();
    const engineerLabel = (() => {
      if (engineerPartyId) {
        const display =
          engineerOptions.find((opt) => opt.partyId === engineerPartyId)?.displayName ??
          engineerName.trim();
        return display ? `[${engineerPartyId}] ${display}` : `[${engineerPartyId}]`;
      }
      return engineerName.trim();
    })();
    const engineerLine = engineerLabel ? `Engineer: ${engineerLabel}` : '';
    if (trimmed && engineerLine) return `${trimmed}\n${engineerLine}`;
    if (engineerLine) return engineerLine;
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
        cbServiceType: (() => {
          const trimmed = serviceType.trim();
          return trimmed === '' ? null : trimmed;
        })(),
        cbPartyId: customerPartyId,
        cbResourceIds: assignedRoomIds,
      }),
    onSuccess: () => {
      setDialogOpen(false);
      setFormError(null);
      setTitle('Bloque de estudio');
      setNotes('');
      setServiceType('');
      setStatus('Confirmed');
      setEditingId(null);
      setMode('create');
      setAssignedRoomIds([]);
      setEngineerName('');
      setEngineerPartyId(null);
      setCustomerName('');
      setCustomerPartyId(null);
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
      setEditingId(null);
      setMode('create');
      setFormError(null);
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
    if (DateTime.fromISO(endIso) <= DateTime.fromISO(startIso)) {
      setFormError('La hora de fin debe ser mayor que la de inicio.');
      return;
    }
    if (!customerPartyId) {
      setFormError('Selecciona un cliente para la sesión.');
      return;
    }
    if (assignedRoomIds.length === 0) {
      setFormError('Asigna al menos una sala para la sesión.');
      return;
    }
    const combinedNotes = buildCombinedNotes();
    if (mode === 'edit' && editingId) {
      const trimmedService = serviceType.trim();
      updateMutation.mutate({
        id: editingId,
        body: {
          ubTitle: title.trim(),
          ubServiceType: trimmedService === '' ? null : trimmedService,
          ubNotes: combinedNotes,
          ubStatus: status,
          ubStartsAt: startIso,
          ubEndsAt: endIso,
          ubResourceIds: assignedRoomIds,
          ubPartyId: customerPartyId,
        },
      });
    } else {
      createMutation.mutate();
    }
  };

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
      setDialogOpen(false);
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
    const bookingId = Number.parseInt(info.event.id, 10);
    const booking = bookings.find((b) => b.bookingId === bookingId);
    if (!booking) return;
    setMode('edit');
    setEditingId(booking.bookingId);
    setTitle(booking.title ?? 'Sesión');
    const { engineer, engineerId, notesBody } = extractEngineerFromNotes(booking.notes);
    setNotes(notesBody);
    setEngineerName(engineer);
    const matchedEngineerId =
      engineerId ??
      (engineer
        ? parties.find((p) => p.displayName.toLowerCase() === engineer.toLowerCase())?.partyId ?? null
        : null);
    setEngineerPartyId(matchedEngineerId);
    setCustomerPartyId(booking.partyId ?? null);
    const customerLabel =
      booking.partyId && parties.find((p) => p.partyId === booking.partyId)?.displayName
        ? parties.find((p) => p.partyId === booking.partyId)?.displayName ?? ''
        : booking.customerName ?? booking.partyDisplayName ?? '';
    setCustomerName(customerLabel);
    setServiceType(booking.serviceType ?? '');
    setStatus(booking.status ?? 'Confirmed');
    setStartInput(formatForInput(new Date(booking.startsAt)));
    setEndInput(formatForInput(new Date(booking.endsAt)));
    setAssignedRoomIds((booking.resources ?? []).map((r) => r.brRoomId));
    setDialogOpen(true);
  };

  const handleEventDropOrResize = (arg: { event: { id: string; start: Date | null; end: Date | null; extendedProps?: { isCourse?: boolean } }; revert?: () => void }) => {
    if (arg.event.extendedProps?.isCourse) {
      arg.revert?.();
      return;
    }
    const bookingId = Number.parseInt(arg.event.id, 10);
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

  return (
    <>
      <Typography variant="h5" gutterBottom>Agenda</Typography>
      {courseNotice && (
        <Alert severity="info" sx={{ mb: 1 }} onClose={() => setCourseNotice(null)}>
          {courseNotice}
        </Alert>
      )}
      {calendarError && <Alert severity="warning" sx={{ mb: 1 }}>{calendarError}</Alert>}
      {bookingsQuery.isLoading && <div>Cargando...</div>}
      {bookingsQuery.error && <div>Error: {bookingsQuery.error.message}</div>}
      <Paper sx={{ p: 1 }}>
        <FullCalendar
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
                        background: 'rgba(59,130,246,0.18)',
                        color: '#0f172a',
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
                  <span style={{ fontSize: 11, color: '#0f172a', opacity: 0.8 }}>
                    {[courseSubtitle, priceText, locationText].filter(Boolean).join(' · ')}
                  </span>
                )}
                {engineerLabel && (
                  <span style={{ fontSize: 11, color: '#0f172a', opacity: 0.85 }}>
                    Ingeniero: {engineerLabel}
                  </span>
                )}
              </div>
            );
          }}
          events={events}
          nowIndicator
          timeZone={zone}
          headerToolbar={{
            left: 'prev,next today',
            center: 'title',
            right: 'dayGridMonth,timeGridWeek,timeGridDay'
          }}
        />
      </Paper>

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
              component="a"
              href="/estudio/live-sessions"
              target="_blank"
              rel="noopener noreferrer"
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

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} maxWidth="sm" fullWidth>
        <DialogTitle>Nueva sesión en el calendario</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2} component="form" onSubmit={handleCreate}>
            {formError && <Alert severity="error">{formError}</Alert>}
            {roomsQuery.isLoading && <Alert severity="info">Cargando salas disponibles…</Alert>}
            {missingEngineer && (
              <Alert severity="warning">
                Este servicio normalmente usa un ingeniero. Asigna uno o continúa bajo tu criterio.
              </Alert>
            )}
            <TextField
              label="Título"
              value={title}
              onChange={(e) => setTitle(e.target.value)}
              fullWidth
            />
            <Autocomplete
              options={customerOptions}
              getOptionLabel={(option) => option.displayName}
              loading={partiesQuery.isFetching}
              value={customerOptions.find((opt) => opt.partyId === customerPartyId) ?? null}
              onChange={(_, value) => {
                setCustomerPartyId(value?.partyId ?? null);
                setCustomerName(value?.displayName ?? '');
              }}
              inputValue={customerName}
              onInputChange={(_, value, reason) => {
                if (reason === 'clear') {
                  setCustomerPartyId(null);
                  setCustomerName('');
                }
              }}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Cliente"
                  required
                  helperText={
                    customerOptions.length === 0
                      ? 'No hay clientes en el catálogo.'
                      : 'Selecciona un cliente para la sesión.'
                  }
                />
              )}
              noOptionsText="Sin clientes en el catálogo"
            />
            <Button
              variant="outlined"
              size="small"
              onClick={() => {
                setCreateContactError(null);
                setCreateContactOpen(true);
              }}
              sx={{ alignSelf: { xs: 'stretch', sm: 'flex-start' } }}
            >
              Crear contacto rápido
            </Button>
            <TextField
              label="Inicio"
              type="datetime-local"
              value={startInput}
              onChange={(e) => setStartInput(e.target.value)}
              fullWidth
              InputLabelProps={{ shrink: true }}
            />
            <TextField
              label="Fin"
              type="datetime-local"
              value={endInput}
              onChange={(e) => setEndInput(e.target.value)}
              fullWidth
              InputLabelProps={{ shrink: true }}
            />
            <TextField
              label="Notas (opcional)"
              value={notes}
              onChange={(e) => setNotes(e.target.value)}
              fullWidth
              multiline
              minRows={2}
            />
            <Autocomplete
              options={engineerOptions}
              getOptionLabel={(option) => option.displayName}
              loading={partiesQuery.isFetching}
              value={engineerOptions.find((opt) => opt.partyId === engineerPartyId) ?? null}
              onChange={(_, value) => {
                setEngineerPartyId(value?.partyId ?? null);
                setEngineerName(value?.displayName ?? '');
              }}
              inputValue={engineerName}
              onInputChange={(_, value, reason) => {
                if (reason === 'input') {
                  setEngineerName(value);
                  setEngineerPartyId(null);
                }
                if (reason === 'clear') {
                  setEngineerName('');
                  setEngineerPartyId(null);
                }
              }}
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Ingeniero (sugerido para recording/mixing/mastering)"
                  helperText={
                    engineerOptions.length === 0
                      ? 'No hay ingenieros en el catálogo de contactos.'
                      : requiresEngineer(serviceType)
                        ? 'Recomendado para recording/mixing/mastering.'
                        : 'Opcional.'
                  }
                />
              )}
              noOptionsText="Sin ingenieros en el catálogo"
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
            <TextField
              select
              label="Servicio"
              value={serviceType}
              onChange={(e) => {
                const value = e.target.value;
                setServiceType(value);
                if (mode === 'create' || assignedRoomIds.length === 0) {
                  const defaults = defaultRoomsForService(value);
                  if (defaults.length) {
                    setAssignedRoomIds(defaults.map((room) => room.roomId));
                  }
                }
                if (requiresEngineer(value) && !engineerName) {
                  setEngineerName('');
                }
              }}
              helperText="Elige el tipo de servicio asociado a la sesión."
            >
              <MenuItem value="">(Sin asignar)</MenuItem>
              {serviceTypes.map((svc) => (
                <MenuItem key={svc.id} value={svc.name}>
                  {svc.name} — {svc.currency} {svc.price}
                  {svc.billingUnit ? ` / ${svc.billingUnit}` : ''}
                </MenuItem>
              ))}
            </TextField>
            <Autocomplete
              multiple
              options={rooms}
              getOptionLabel={(option) => option.rName}
              value={assignedRooms}
              onChange={(_, value) => setAssignedRoomIds(value.map((room) => room.roomId))}
              renderTags={(value, getTagProps) =>
                value.map((option, index) => (
                  <Chip key={option.roomId} label={option.rName} {...getTagProps({ index })} />
                ))
              }
              renderInput={(params) => (
                <TextField
                  {...params}
                  label="Salas asignadas"
                  placeholder="Agregar/ajustar salas"
                  helperText="Se precargan según el tipo de servicio."
                />
              )}
              noOptionsText="No hay salas registradas"
            />
            <Box>
              <Typography variant="caption" color="text.secondary">
                Reglas: DJ Practice → DJ Booth · Band recording → Live + Control · Vocal recording → Vocal Booth + Control · Band rehearsal → Live · Mixing/Mastering → Control. Recording/Mixing/Mastering requieren ingeniero.
              </Typography>
            </Box>
          </Stack>
        </DialogContent>
        <DialogActions sx={{ px: 3, pb: 2 }}>
          <Button onClick={() => setDialogOpen(false)}>Cancelar</Button>
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
    </>
  );
}
