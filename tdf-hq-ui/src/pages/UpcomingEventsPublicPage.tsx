import { useMemo, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import CalendarMonthIcon from '@mui/icons-material/CalendarMonth';
import SearchIcon from '@mui/icons-material/Search';
import {
  Alert,
  Box,
  Card,
  CardContent,
  CircularProgress,
  InputAdornment,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { DateTime } from 'luxon';
import { SocialEventsAPI, type PublicUpcomingEventDTO } from '../api/socialEvents';
import { useDocumentTitle } from '../hooks/useDocumentTitle';

const formatEventDate = (value?: string | null) => {
  if (!value) return 'Fecha por confirmar';
  const parsed = DateTime.fromISO(value).setLocale('es');
  return parsed.isValid ? parsed.toFormat("cccc d 'de' LLLL, HH:mm") : 'Fecha por confirmar';
};

const eventPath = (event: PublicUpcomingEventDTO) => `/eventos/${encodeURIComponent(event.publicUpcomingEventId)}`;

export default function UpcomingEventsPublicPage() {
  useDocumentTitle('Próximos eventos');
  const [city, setCity] = useState('');
  const startAfter = useMemo(() => new Date().toISOString(), []);
  const cityFilter = city.trim();

  const eventsQuery = useQuery({
    queryKey: ['public-upcoming-events', startAfter, cityFilter],
    queryFn: () => SocialEventsAPI.listPublicUpcomingEvents({
      city: cityFilter || undefined,
      startAfter,
      limit: 50,
    }),
    staleTime: 60_000,
  });

  const events = useMemo(() => {
    const needle = city.trim().toLocaleLowerCase();
    return (eventsQuery.data ?? [])
      .filter((event) => !needle || (event.publicUpcomingEventCity ?? '').toLocaleLowerCase().includes(needle))
      .sort((a, b) => a.publicUpcomingEventStart.localeCompare(b.publicUpcomingEventStart));
  }, [city, eventsQuery.data]);

  return (
    <Box component="main" id="main-content" sx={{ maxWidth: 1100, mx: 'auto', px: { xs: 2, md: 4 }, py: { xs: 4, md: 7 } }}>
      <Stack spacing={1} sx={{ mb: 4 }}>
        <Stack direction="row" spacing={1} alignItems="center">
          <CalendarMonthIcon color="primary" />
          <Typography variant="overline" color="primary" fontWeight={800}>Agenda TDF</Typography>
        </Stack>
        <Typography variant="h2" component="h1" sx={{ fontSize: { xs: '2rem', md: '3rem' }, fontWeight: 800 }}>
          Próximos eventos
        </Typography>
        <Typography color="text.secondary" sx={{ maxWidth: 720 }}>
          Descubre eventos que vienen en tu ciudad. No necesitas iniciar sesión para consultar la agenda.
        </Typography>
      </Stack>

      <TextField
        fullWidth
        label="Filtrar por ciudad"
        value={city}
        onChange={(event) => setCity(event.target.value)}
        placeholder="Quito, Guayaquil…"
        inputProps={{ 'aria-label': 'Filtrar próximos eventos por ciudad' }}
        InputProps={{ startAdornment: <InputAdornment position="start"><SearchIcon /></InputAdornment> }}
        sx={{ mb: 4, maxWidth: 520 }}
      />

      {eventsQuery.isLoading && <CircularProgress aria-label="Cargando próximos eventos" />}
      {eventsQuery.isError && <Alert severity="error">No pudimos cargar los próximos eventos. Intenta de nuevo.</Alert>}
      {eventsQuery.isSuccess && events.length === 0 && <Alert severity="info">No hay eventos próximos para esa ciudad.</Alert>}
      <Stack spacing={2}>
        {events.map((event) => (
          <Card key={event.publicUpcomingEventId} component={RouterLink} to={eventPath(event)} sx={{ textDecoration: 'none', color: 'inherit', '&:hover': { boxShadow: 5 } }}>
            <CardContent>
              <Typography variant="h6" component="h2" fontWeight={750}>{event.publicUpcomingEventTitle}</Typography>
              <Typography color="primary" sx={{ mt: 0.75 }}>{formatEventDate(event.publicUpcomingEventStart)}</Typography>
              {event.publicUpcomingEventCity && <Typography color="text.secondary" sx={{ mt: 0.5 }}>{event.publicUpcomingEventCity}</Typography>}
              {event.publicUpcomingEventDescription && <Typography color="text.secondary" sx={{ mt: 1 }}>{event.publicUpcomingEventDescription}</Typography>}
            </CardContent>
          </Card>
        ))}
      </Stack>
    </Box>
  );
}
