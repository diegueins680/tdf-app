import { useMemo, useState } from 'react';
import { useMutation, useQuery } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Chip,
  Divider,
  Grid,
  LinearProgress,
  Paper,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import SyncIcon from '@mui/icons-material/Sync';
import LinkIcon from '@mui/icons-material/Link';
import EventIcon from '@mui/icons-material/Event';
import { CalendarApi } from '../api/calendar';

export default function CalendarSyncPage() {
  const [calendarId, setCalendarId] = useState('');
  const [code, setCode] = useState('');
  const [from, setFrom] = useState('');
  const [to, setTo] = useState('');

  const eventsQuery = useQuery({
    queryKey: ['calendar-events', calendarId, from, to],
    queryFn: () =>
      CalendarApi.listEvents({
        calendarId: calendarId.trim() || undefined,
        from: from || undefined,
        to: to || undefined,
      }),
    enabled: Boolean(calendarId.trim()),
  });

  const authUrlMutation = useMutation({
    mutationFn: CalendarApi.getAuthUrl,
    onSuccess: (data) => {
      if (data.url && typeof window !== 'undefined') {
        window.open(data.url, '_blank', 'noopener,noreferrer');
      }
    },
  });

  const exchangeMutation = useMutation({
    mutationFn: () => CalendarApi.exchangeCode({ code: code.trim(), calendarId: calendarId.trim() }),
    onSuccess: () => {
      setCode('');
      void eventsQuery.refetch();
    },
  });

  const syncMutation = useMutation({
    mutationFn: () =>
      CalendarApi.sync({
        calendarId: calendarId.trim(),
        from: from || undefined,
        to: to || undefined,
      }),
    onSuccess: () => void eventsQuery.refetch(),
  });

  const events = useMemo(() => eventsQuery.data ?? [], [eventsQuery.data]);

  return (
    <Stack spacing={3}>
      <Paper variant="outlined" sx={{ p: 2.5, borderRadius: 2.5 }}>
        <Stack spacing={2}>
          <Typography variant="h5" fontWeight={800}>
            Integración Google Calendar
          </Typography>
          <Typography color="text.secondary">
            Conecta tu calendario y sincroniza eventos a la base de datos para usarlos en reportes, agenda interna y posteriores
            automatizaciones.
          </Typography>
          <Grid container spacing={2}>
            <Grid item xs={12} md={4}>
              <TextField
                label="Calendar ID"
                fullWidth
                value={calendarId}
                onChange={(e) => setCalendarId(e.target.value)}
                helperText="Ej: primary o calendar-id@group.calendar.google.com"
              />
            </Grid>
            <Grid item xs={12} md={4}>
              <TextField
                label="Desde (opcional, ISO)"
                fullWidth
                value={from}
                onChange={(e) => setFrom(e.target.value)}
                placeholder="2025-12-01T00:00:00Z"
              />
            </Grid>
            <Grid item xs={12} md={4}>
              <TextField
                label="Hasta (opcional, ISO)"
                fullWidth
                value={to}
                onChange={(e) => setTo(e.target.value)}
                placeholder="2026-01-31T23:59:59Z"
              />
            </Grid>
          </Grid>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
            <Button
              variant="outlined"
              startIcon={<LinkIcon />}
              onClick={() => authUrlMutation.mutate()}
              disabled={authUrlMutation.isPending}
            >
              Obtener URL de consentimiento
            </Button>
            <TextField
              label="Code (pegado desde Google)"
              value={code}
              onChange={(e) => setCode(e.target.value)}
              sx={{ minWidth: 320 }}
            />
            <Button
              variant="contained"
              onClick={() => exchangeMutation.mutate()}
              disabled={!code.trim() || !calendarId.trim() || exchangeMutation.isPending}
            >
              Guardar tokens
            </Button>
            <Button
              variant="contained"
              color="secondary"
              startIcon={<SyncIcon />}
              onClick={() => syncMutation.mutate()}
              disabled={!calendarId.trim() || syncMutation.isPending}
            >
              Sincronizar ahora
            </Button>
          </Stack>
          {exchangeMutation.isError && (
            <Alert severity="error">No se pudo intercambiar el code. Revisa el client_id/secret y el redirect.</Alert>
          )}
          {exchangeMutation.isSuccess && <Alert severity="success">Tokens guardados.</Alert>}
          {syncMutation.isError && <Alert severity="error">La sincronización falló.</Alert>}
          {syncMutation.isSuccess && (
            <Alert severity="success">
              Sync OK: {syncMutation.data.updated} actualizados, {syncMutation.data.created} creados, {syncMutation.data.deleted} cancelados.
            </Alert>
          )}
        </Stack>
      </Paper>

      <Paper variant="outlined" sx={{ p: 2.5, borderRadius: 2.5 }}>
        <Stack spacing={2}>
          <Stack direction="row" spacing={1} alignItems="center">
            <EventIcon color="primary" />
            <Typography variant="h6" fontWeight={800}>
              Eventos sincronizados
            </Typography>
            <Chip label={`${events.length}`} size="small" />
          </Stack>
          {eventsQuery.isLoading && <LinearProgress />}
          {eventsQuery.isError && <Alert severity="error">No se pudieron cargar los eventos.</Alert>}
          <Divider />
          <Stack spacing={1.5}>
            {events.map((ev) => (
              <Paper key={ev.eventId} variant="outlined" sx={{ p: 1.5, borderRadius: 2 }}>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} justifyContent="space-between">
                  <Box sx={{ flexGrow: 1 }}>
                    <Typography fontWeight={700}>{ev.summary || '(Sin título)'}</Typography>
                    <Typography variant="body2" color="text.secondary">
                      {ev.startAt ? new Date(ev.startAt).toLocaleString() : 'Sin fecha'} —{' '}
                      {ev.endAt ? new Date(ev.endAt).toLocaleString() : 'Sin fin'}
                    </Typography>
                    {ev.location && (
                      <Typography variant="body2" color="text.secondary">
                        {ev.location}
                      </Typography>
                    )}
                    {ev.description && (
                      <Typography variant="body2" color="text.secondary" sx={{ mt: 0.5 }}>
                        {ev.description}
                      </Typography>
                    )}
                  </Box>
                  <Stack spacing={0.5} alignItems={{ xs: 'flex-start', sm: 'flex-end' }}>
                    <Chip label={ev.status} size="small" />
                    {ev.htmlLink && (
                      <Button href={ev.htmlLink} target="_blank" rel="noreferrer" size="small">
                        Ver en Google
                      </Button>
                    )}
                  </Stack>
                </Stack>
              </Paper>
            ))}
            {events.length === 0 && !eventsQuery.isLoading && (
              <Typography color="text.secondary">Sin eventos sincronizados para este calendario.</Typography>
            )}
          </Stack>
        </Stack>
      </Paper>
    </Stack>
  );
}
