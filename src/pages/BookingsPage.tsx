import { useMemo, useState, useRef, useEffect } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Bookings, CreateBookingReq } from '../api/bookings';
import { Parties } from '../api/parties';
import type { BookingDTO, PartyDTO } from '../api/types';
import { Typography, Paper, Stack, Button, Dialog, DialogTitle, DialogContent, DialogActions, TextField, MenuItem } from '@mui/material';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useForm } from 'react-hook-form';

// FullCalendar JS
import { Calendar } from '@fullcalendar/core';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import interactionPlugin from '@fullcalendar/interaction';

// FullCalendar CSS



const schema = z.object({
  cbTitle: z.string().min(2, 'Título requerido'),
  cbStartsAt: z.string(),
  cbEndsAt: z.string(),
  cbStatus: z.enum(['Confirmed','Tentative','Cancelled','Completed']).default('Confirmed'),
  cbPartyId: z.number().nullable().optional(),
});

function formatDateTimeLocal(value?: string) {
  if (!value) return '';
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return '';
  const tzOffsetMs = date.getTimezoneOffset() * 60 * 1000;
  const local = new Date(date.getTime() - tzOffsetMs);
  return local.toISOString().slice(0, 16);
}

function CalendarView({ events, onSelect }: { events: any[], onSelect: (start: string, end: string) => void }) {
  const ref = useRef<HTMLDivElement>(null);
  const calRef = useRef<Calendar | null>(null);

  useEffect(() => {
    if (!ref.current) return;
    if (calRef.current) { calRef.current.destroy(); calRef.current = null; }

    calRef.current = new Calendar(ref.current, {
      plugins: [dayGridPlugin, timeGridPlugin, interactionPlugin],
      initialView: 'timeGridWeek',
      height: 'auto',
      selectable: true,
      selectMirror: true,
      headerToolbar: { left: 'prev,next today', center: 'title', right: 'dayGridMonth,timeGridWeek,timeGridDay' },
      events,
      select: (info) => onSelect(info.startStr, info.endStr),
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
  const { handleSubmit, register, setValue, formState: { errors } } = useForm<CreateBookingReq>({
    resolver: zodResolver(schema),
    defaultValues: { cbTitle: 'New Booking', cbStartsAt: '', cbEndsAt: '', cbStatus: 'Confirmed', cbPartyId: null }
  });

  const [artistOptions, setArtistOptions] = useState<PartyDTO[]>([]);
  const { data: artistContacts } = useQuery({
    queryKey: ['parties', 'artists'],
    queryFn: () => Parties.listByRole('Artist'),
    enabled: open,
  });

  useEffect(() => {
    if (artistContacts) {
      setArtistOptions(artistContacts);
    }
  }, [artistContacts]);

  useEffect(()=>{
    setValue('cbStartsAt', formatDateTimeLocal(initialStart));
    setValue('cbEndsAt', formatDateTimeLocal(initialEnd));
  }, [initialStart, initialEnd, setValue]);

  const m = useMutation({
    mutationFn: (body: CreateBookingReq) => Bookings.create(body),
    onSuccess: () => { qc.invalidateQueries({ queryKey: ['bookings'] }); onClose(); }
  });

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>Crear Reserva</DialogTitle>
      <DialogContent>
        <Stack gap={2} sx={{ mt: 1 }}>
          <TextField label="Título" {...register('cbTitle')} error={!!errors.cbTitle} helperText={errors.cbTitle?.message} />
          <TextField label="Inicio" type="datetime-local" {...register('cbStartsAt')} InputLabelProps={{ shrink: true }} />
          <TextField label="Fin" type="datetime-local" {...register('cbEndsAt')} InputLabelProps={{ shrink: true }} />
          <TextField select label="Estado" {...register('cbStatus')} defaultValue="Confirmed">
            {['Confirmed','Tentative','Cancelled','Completed'].map(s => <MenuItem key={s} value={s}>{s}</MenuItem>)}
          </TextField>
          <TextField
            select
            label="Artista"
            defaultValue=""
            {...register('cbPartyId', {
              setValueAs: (value) => (value === '' ? null : Number(value)),
            })}
          >
            <MenuItem value="">Sin artista</MenuItem>
            {artistOptions.map((artist) => (
              <MenuItem key={artist.partyId} value={String(artist.partyId)}>
                {artist.displayName}
              </MenuItem>
            ))}
          </TextField>
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cancelar</Button>
        <Button variant="contained" onClick={handleSubmit((vals)=>m.mutate({
          ...vals,
          cbStartsAt: new Date(vals.cbStartsAt).toISOString(),
          cbEndsAt: new Date(vals.cbEndsAt).toISOString(),
          cbPartyId: vals.cbPartyId ?? null,
        }))} disabled={m.isPending}>Guardar</Button>
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

  const handleSelect = (s: string, e: string) => { setStart(s); setEnd(e); setOpen(true); };

  return (
    <>
      <Stack direction="row" justifyContent="space-between" alignItems="center" sx={{ mb: 2 }}>
        <Typography variant="h5">Agenda</Typography>
        <Button variant="contained" onClick={()=>{ setStart(new Date().toISOString()); setEnd(new Date(Date.now()+60*60*1000).toISOString()); setOpen(true); }}>Nuevo</Button>
      </Stack>

      <Paper variant="outlined" sx={{ p: 1.5 }}>
        <CalendarView events={events} onSelect={handleSelect} />
      </Paper>

      <CreateBookingDialog open={open} onClose={()=>setOpen(false)} initialStart={start} initialEnd={end} />
    </>
  );
}
