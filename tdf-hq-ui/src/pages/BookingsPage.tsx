import { useMemo } from 'react';
import { useQuery, type UseQueryResult } from '@tanstack/react-query';
import { Bookings } from '../api/bookings';
import type { BookingDTO } from '../api/types';
import { Typography, Paper } from '@mui/material';
import FullCalendar from '@fullcalendar/react';
import dayGridPlugin from '@fullcalendar/daygrid';
import timeGridPlugin from '@fullcalendar/timegrid';
import interactionPlugin from '@fullcalendar/interaction';
import { DateTime } from 'luxon';

// FullCalendar v6 auto-injects its styles when the modules load, so importing the
// CSS bundles directly is unnecessary and breaks with Vite due to missing files.

export default function BookingsPage() {
  const bookingsQuery: UseQueryResult<BookingDTO[], Error> = useQuery<BookingDTO[], Error>({
    queryKey: ['bookings'],
    queryFn: Bookings.list,
  });
  const zone = import.meta.env.VITE_TZ ?? 'America/Guayaquil';
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
