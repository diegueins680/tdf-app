import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient, type UseQueryResult } from '@tanstack/react-query';
import { Bookings } from '../api/bookings';
import type { BookingDTO } from '../api/types';
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
} from '@mui/material';
import FullCalendar from '@fullcalendar/react';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import interactionPlugin from '@fullcalendar/interaction';
import { DateTime } from 'luxon';
import { loadServiceTypes } from '../utils/serviceTypesStore';

// FullCalendar v6 auto-injects its styles when the modules load, so importing the
// CSS bundles directly is unnecessary and breaks with Vite due to missing files.

export default function BookingsPage() {
  const bookingsQuery: UseQueryResult<BookingDTO[], Error> = useQuery<BookingDTO[], Error>({
    queryKey: ['bookings'],
    queryFn: Bookings.list,
  });
  const qc = useQueryClient();
  const zone = import.meta.env['VITE_TZ'] ?? 'America/Guayaquil';
  const bookings = useMemo<BookingDTO[]>(() => bookingsQuery.data ?? [], [bookingsQuery.data]);
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

  const events = useMemo(
    () =>
      bookings.map((booking) => ({
        id: String(booking.bookingId),
        title: booking.title,
        start: toIsoDate(booking.startsAt),
        end: toIsoDate(booking.endsAt),
        extendedProps: booking,
      })),
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
  const [status, setStatus] = useState<string>('Confirmed');
  const serviceTypes = useMemo(() => loadServiceTypes(), []);

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
    setStatus('Confirmed');
    openDialogForRange(start, end);
  };

  const handleSelect = (info: { start: Date; end: Date }) => {
    setMode('create');
    setEditingId(null);
    setTitle('Bloque de estudio');
    setNotes('');
    setServiceType('');
    setStatus('Confirmed');
    openDialogForRange(info.start, info.end ?? DateTime.fromJSDate(info.start).plus({ minutes: 60 }).toJSDate());
  };

  const toUtcIso = (value: string) => {
    const dt = DateTime.fromFormat(value, "yyyy-LL-dd'T'HH:mm", { zone });
    return dt.isValid ? dt.toUTC().toISO() : null;
  };

  const createMutation = useMutation({
    mutationFn: () =>
      Bookings.create({
        cbTitle: title.trim() || 'Bloque de estudio',
        cbStartsAt: toUtcIso(startInput) ?? '',
        cbEndsAt: toUtcIso(endInput) ?? '',
        cbStatus: 'Confirmed',
        cbNotes: notes.trim() || null,
        cbServiceType: serviceType.trim() || null,
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
    if (mode === 'edit' && editingId) {
      updateMutation.mutate({
        id: editingId,
        body: {
          ubTitle: title.trim(),
          ubServiceType: serviceType.trim() || null,
          ubNotes: notes.trim() || null,
          ubStatus: status,
          ubStartsAt: startIso,
          ubEndsAt: endIso,
        },
      });
    } else {
      createMutation.mutate();
    }
  };

  const handleEventClick = (info: { event: { id: string; extendedProps?: unknown } }) => {
    const bookingId = Number.parseInt(info.event.id, 10);
    const booking = bookings.find((b) => b.bookingId === bookingId);
    if (!booking) return;
    setMode('edit');
    setEditingId(booking.bookingId);
    setTitle(booking.title ?? 'Sesión');
    setNotes(booking.notes ?? '');
    setServiceType(booking.serviceType ?? '');
    setStatus(booking.status ?? 'Confirmed');
    setStartInput(formatForInput(new Date(booking.startsAt)));
    setEndInput(formatForInput(new Date(booking.endsAt)));
    setDialogOpen(true);
  };

  const handleEventDropOrResize = (arg: { event: { id: string; start: Date | null; end: Date | null } }) => {
    const bookingId = Number.parseInt(arg.event.id, 10);
    if (!arg.event.start || !arg.event.end) return;
    const startIso = toUtcIso(formatForInput(arg.event.start));
    const endIso = toUtcIso(formatForInput(arg.event.end));
    if (!startIso || !endIso) return;
    void updateMutation.mutate({
      id: bookingId,
      body: {
        ubStartsAt: startIso,
        ubEndsAt: endIso,
      },
    });
  };

  return (
    <>
      <Typography variant="h5" gutterBottom>Agenda</Typography>
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

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} maxWidth="sm" fullWidth>
        <DialogTitle>Nueva sesión en el calendario</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={2} component="form" onSubmit={handleCreate}>
            {formError && <Alert severity="error">{formError}</Alert>}
            <TextField
              label="Título"
              value={title}
              onChange={(e) => setTitle(e.target.value)}
              fullWidth
            />
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
              onChange={(e) => setServiceType(e.target.value)}
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
