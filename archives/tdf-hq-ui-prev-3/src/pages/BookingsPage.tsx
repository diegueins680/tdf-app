import { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Bookings } from '../api/bookings';
import type { BookingDTO } from '../api/types';
import { Typography, Paper } from '@mui/material';
import FullCalendar from '@fullcalendar/react';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import interactionPlugin from '@fullcalendar/interaction';
import { DateTime } from 'luxon';

import '@fullcalendar/core/index.css';
import '@fullcalendar/daygrid/index.css';
import '@fullcalendar/timegrid/index.css';

export default function BookingsPage() {
  const { data, isLoading, error } = useQuery({ queryKey: ['bookings'], queryFn: Bookings.list });
  const zone = import.meta.env.VITE_TZ || 'America/Guayaquil';

  const events = useMemo(() => (data || []).map((b: BookingDTO) => ({
    id: String(b.bookingId),
    title: b.title,
    start: DateTime.fromISO(b.startsAt).toISO(),
    end: DateTime.fromISO(b.endsAt).toISO(),
  })), [data]);

  return (
    <>
      <Typography variant="h5" gutterBottom>Agenda</Typography>
      {isLoading && <div>Cargando...</div>}
      {error && <div>Error: {(error as Error).message}</div>}
      <Paper sx={{ p: 1 }}>
        <FullCalendar
          plugins={[dayGridPlugin, timeGridPlugin, interactionPlugin]}
          initialView="timeGridWeek"
          height="auto"
          allDaySlot={false}
          slotDuration="00:30:00"
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
    </>
  );
}
