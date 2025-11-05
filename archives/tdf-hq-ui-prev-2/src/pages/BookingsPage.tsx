
import { useMemo, useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Bookings, CreateBookingReq } from '../api/bookings';
import type { BookingDTO } from '../api/types';
import { Typography, Paper, Stack, Button, Dialog, DialogTitle, DialogContent, DialogActions, TextField, MenuItem } from '@mui/material';
import FullCalendar from '@fullcalendar/core';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import interactionPlugin from '@fullcalendar/interaction';
import { Calendar } from '@fullcalendar/core';
import { DateTime } from 'luxon';
import '@fullcalendar/daygrid/index.css';
import '@fullcalendar/timegrid/index.css';

// A lightweight wrapper component for FullCalendar to play nice with React strict mode
import { useEffect, useRef } from 'react';

function CalendarView({ events, onSelect }: { events: any[], onSelect: (start: string, end: string) => void }) {
  const ref = useRef<HTMLDivElement>(null);
  const calRef = useRef<Calendar | null>(null);

  useEffect(() => {
    if (!ref.current) return;
    if (calRef.current) { calRef.current.destroy(); calRef.current = null; }

    calRef.current = new FullCalendar.Calendar(ref.current, {
      plugins: [dayGridPlugin, timeGridPlugin, interactionPlugin],
      initialView: 'timeGridWeek',
      height: 'auto',
      selectable: true,
      selectMirror: true,
      headerToolbar: { left: 'prev,next today', center: 'title', right: 'dayGridMonth,timeGridWeek,timeGridDay' },
      events,
      select: (info) => {
        onSelect(info.startStr, info.endStr);
      },
    });
    calRef.current.render();

    return () => { calRef.current?.destroy(); calRef.current = null; };
  }, [events, onSelect]);

  return <div ref={ref} />;
}

function CreateBookingDialog({ open, onClose, initialStart, initialEnd }: {
  open: boolean, onClose: () => void, initialStart?: string, initialEnd?: string
}) {
  const qc = useQueryClient();
  const [title, setTitle] = useState('New Booking');
  const [start, setStart] = useState(initialStart || '');
  const [end, setEnd] = useState(initialEnd || '');
  const [status, setStatus] = useState('Confirmed');

  const m = useMutation({
    mutationFn: (body: CreateBookingReq) => Bookings.create(body),
    onSuccess: () => { qc.invalidateQueries({ queryKey: ['bookings'] }); onClose(); }
  });

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Crear Reserva</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField label="Título" value={title} onChange={e => setTitle(e.target.value)} fullWidth />
          <TextField label="Inicio" type="datetime-local" value={start ? start.slice(0,16) : ''}
            onChange={e => setStart(e.target.value)} InputLabelProps={{ shrink: true }} />
          <TextField label="Fin" type="datetime-local" value={end ? end.slice(0,16) : ''}
            onChange={e => setEnd(e.target.value)} InputLabelProps={{ shrink: true }} />
          <TextField select label="Estado" value={status} onChange={e => setStatus(e.target.value)}>
            {['Confirmed','Tentative','Cancelled','Completed'].map(s => <MenuItem key={s} value={s}>{s}</MenuItem>)}
          </TextField>
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button variant="contained" onClick={() => m.mutate({
          cbTitle: title,
          cbStartsAt: new Date(start).toISOString(),
          cbEndsAt: new Date(end).toISOString(),
          cbStatus: status,
        })} disabled={m.isPending}>Guardar</Button>
      </DialogActions>
    </Dialog>
  );
}

export default function BookingsPage() {
  const { data } = useQuery({ queryKey: ['bookings'], queryFn: Bookings.list });
  const [open, setOpen] = useState(false);
  const [start, setStart] = useState<string | undefined>();
  const [end, setEnd] = useState<string | undefined>();

  const events = useMemo(() => (data || []).map((b: BookingDTO) => ({
    id: String(b.bookingId),
    title: b.title,
    start: b.startsAt,
    end: b.endsAt,
  })), [data]);

  const handleSelect = (s: string, e: string) => {
    setStart(s); setEnd(e); setOpen(true);
  };

  return (
    <>
      <Typography variant="h5" gutterBottom>Agenda</Typography>
      <Paper sx={{ p: 1.5 }}>
        <CalendarView events={events} onSelect={handleSelect} />
      </Paper>
      <CreateBookingDialog open={open} onClose={() => setOpen(false)} initialStart={start} initialEnd={end} />
    </>
  );
}
