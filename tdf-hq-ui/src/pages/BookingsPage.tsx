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
      })),
    [bookings],
  );

  const [dialogOpen, setDialogOpen] = useState(false);
  const [title, setTitle] = useState('Bloque de estudio');
  const [notes, setNotes] = useState('');
  const [startInput, setStartInput] = useState('');
  const [endInput, setEndInput] = useState('');
  const [formError, setFormError] = useState<string | null>(null);
  const [serviceType, setServiceType] = useState<string>('');
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
    openDialogForRange(start, end);
  };

  const handleSelect = (info: { start: Date; end: Date }) => {
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
      void qc.invalidateQueries({ queryKey: ['bookings'] });
    },
    onError: (err) => {
      setFormError(err instanceof Error ? err.message : 'No se pudo crear la sesión.');
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
    createMutation.mutate();
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
          selectable
          selectMirror
          select={handleSelect}
          dateClick={handleDateClick}
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
              autoFocus
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
            disabled={createMutation.isPending}
            sx={{ textTransform: 'none' }}
          >
            {createMutation.isPending ? 'Creando…' : 'Crear sesión'}
          </Button>
        </DialogActions>
      </Dialog>
    </>
  );
}
