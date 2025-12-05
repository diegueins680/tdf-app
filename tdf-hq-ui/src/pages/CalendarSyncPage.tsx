import { useCallback, useEffect, useMemo, useState } from 'react';
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
import { DateTime } from 'luxon';
import { CalendarApi } from '../api/calendar';

export default function CalendarSyncPage() {
  const zone: string = import.meta.env['VITE_TZ'] ?? 'America/Guayaquil';
  const [calendarId, setCalendarId] = useState('');
  const [code, setCode] = useState('');
  const [fromInput, setFromInput] = useState('');
  const [toInput, setToInput] = useState('');
  const [connectedCalendar, setConnectedCalendar] = useState<string | null>(null);
  const [lastSyncAt, setLastSyncAt] = useState<string | null>(null);
  const [showValidation, setShowValidation] = useState(false);
  const [appliedRemoteConfig, setAppliedRemoteConfig] = useState(false);

  const trimmedCalendarId = calendarId.trim();

  const formatForInput = useCallback(
    (dt: DateTime) => dt.setZone(zone).toFormat("yyyy-LL-dd'T'HH:mm"),
    [zone],
  );

  const toUtcIso = useCallback(
    (value: string) => {
      if (!value) return null;
      const dt = DateTime.fromFormat(value, "yyyy-LL-dd'T'HH:mm", { zone });
      return dt.isValid ? dt.toUTC().toISO() : null;
    },
    [zone],
  );

  type RangePreset = 'next30' | 'last30' | 'thisMonth';

  const applyRangePreset = useCallback(
    (preset: RangePreset) => {
      const now = DateTime.now().setZone(zone);
      const ranges: Record<RangePreset, { start: DateTime; end: DateTime }> = {
        next30: { start: now.startOf('day'), end: now.plus({ days: 30 }).endOf('day') },
        last30: { start: now.minus({ days: 30 }).startOf('day'), end: now.endOf('day') },
        thisMonth: { start: now.startOf('month'), end: now.endOf('month') },
      };
      const range = ranges[preset];
      setFromInput(formatForInput(range.start));
      setToInput(formatForInput(range.end));
    },
    [formatForInput, zone],
  );

  const clearRange = useCallback(() => {
    setFromInput('');
    setToInput('');
  }, []);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    const storedId = window.localStorage.getItem('calendar-sync.calendarId');
    const storedRange = window.localStorage.getItem('calendar-sync.range');
    const storedConnected = window.localStorage.getItem('calendar-sync.connected');
    const storedLastSync = window.localStorage.getItem('calendar-sync.lastSyncAt');

    if (storedId) setCalendarId(storedId);
    if (storedConnected) setConnectedCalendar(storedConnected);
    if (storedLastSync) setLastSyncAt(storedLastSync);

    if (storedRange) {
      try {
        const parsed = JSON.parse(storedRange) as Partial<{ from: unknown; to: unknown }>;
        const fromVal = typeof parsed.from === 'string' ? parsed.from : '';
        const toVal = typeof parsed.to === 'string' ? parsed.to : '';
        if (fromVal) setFromInput(fromVal);
        if (toVal) setToInput(toVal);
      } catch {
        applyRangePreset('next30');
      }
    } else {
      applyRangePreset('next30');
    }
  }, [applyRangePreset]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    window.localStorage.setItem('calendar-sync.calendarId', calendarId);
  }, [calendarId]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    window.localStorage.setItem('calendar-sync.range', JSON.stringify({ from: fromInput, to: toInput }));
  }, [fromInput, toInput]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    if (connectedCalendar) window.localStorage.setItem('calendar-sync.connected', connectedCalendar);
  }, [connectedCalendar]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    if (lastSyncAt) window.localStorage.setItem('calendar-sync.lastSyncAt', lastSyncAt);
  }, [lastSyncAt]);

  const fromIso = useMemo(() => toUtcIso(fromInput), [fromInput, toUtcIso]);
  const toIso = useMemo(() => toUtcIso(toInput), [toInput, toUtcIso]);

  const rangeError = useMemo(() => {
    if (fromIso && toIso) {
      return DateTime.fromISO(fromIso) > DateTime.fromISO(toIso)
        ? 'La fecha "Desde" no puede ser mayor a "Hasta".'
        : null;
    }
    return null;
  }, [fromIso, toIso]);

  const eventsQuery = useQuery({
    queryKey: ['calendar-events', trimmedCalendarId, fromIso, toIso],
    queryFn: () =>
      CalendarApi.listEvents({
        calendarId: trimmedCalendarId ?? undefined,
        from: fromIso ?? undefined,
        to: toIso ?? undefined,
      }),
    enabled:
      Boolean(trimmedCalendarId) &&
      !rangeError &&
      (!fromInput || Boolean(fromIso)) &&
      (!toInput || Boolean(toIso)),
  });

  const configQuery = useQuery({
    queryKey: ['calendar-config'],
    queryFn: () => CalendarApi.getConfig(),
    staleTime: 5 * 60 * 1000,
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
    mutationFn: () => CalendarApi.exchangeCode({ code: code.trim(), calendarId: trimmedCalendarId }),
    onSuccess: () => {
      setCode('');
      setShowValidation(false);
      setConnectedCalendar(trimmedCalendarId);
      setLastSyncAt(null);
      void eventsQuery.refetch();
    },
  });

  const syncMutation = useMutation({
    mutationFn: () =>
      CalendarApi.sync({
        calendarId: trimmedCalendarId,
        from: fromIso ?? undefined,
        to: toIso ?? undefined,
      }),
    onSuccess: () => {
      setShowValidation(false);
      setLastSyncAt(new Date().toISOString());
      void eventsQuery.refetch();
    },
  });

  const events = useMemo(() => eventsQuery.data ?? [], [eventsQuery.data]);
  const calendarIdError = showValidation && !trimmedCalendarId ? 'Ingresa el Calendar ID o usa "primary".' : '';
  const codeError = showValidation && !code.trim() ? 'Pega el code que te devuelve Google tras consentir.' : '';

  const handleSaveTokens = () => {
    setShowValidation(true);
    if (!trimmedCalendarId || !code.trim()) return;
    exchangeMutation.mutate();
  };

  const handleSync = () => {
    setShowValidation(true);
    if (!trimmedCalendarId || rangeError || (fromInput && !fromIso) || (toInput && !toIso)) return;
    syncMutation.mutate();
  };

  useEffect(() => {
    if (!configQuery.isSuccess || appliedRemoteConfig) return;
    const cfg = configQuery.data;
    if (!cfg) {
      setAppliedRemoteConfig(true);
      return;
    }
    setConnectedCalendar(cfg.calendarId);
    setCalendarId((prev) => (prev.trim() ? prev : cfg.calendarId));
    setLastSyncAt(cfg.syncedAt ?? null);
    setAppliedRemoteConfig(true);
  }, [appliedRemoteConfig, configQuery.data, configQuery.isSuccess]);

  return (
    <Stack spacing={3}>
      <Paper variant="outlined" sx={{ p: 2.5, borderRadius: 2.5 }}>
        <Stack spacing={2}>
          <Typography variant="h5" fontWeight={800}>
            Integración Google Calendar
          </Typography>
          <Typography color="text.secondary">
            Conecta tu calendario y sincroniza eventos a la base de datos para usarlos en reportes, agenda interna y
            posteriores automatizaciones. Usa los pasos: 1) abre la URL de consentimiento, 2) pega el code, 3) sincroniza.
          </Typography>
          <Stack direction="row" spacing={1} flexWrap="wrap">
            <Chip
              color={connectedCalendar ? 'success' : 'default'}
              label={connectedCalendar ? `Tokens guardados para ${connectedCalendar}` : 'Tokens pendientes'}
              size="small"
            />
            <Chip variant="outlined" label={`Zona local: ${zone}`} size="small" />
            {lastSyncAt && (
              <Chip
                variant="outlined"
                color="secondary"
                size="small"
                label={`Última sync: ${new Date(lastSyncAt).toLocaleString()}`}
              />
            )}
          </Stack>
          {configQuery.isError && (
            <Alert severity="warning">No pudimos leer la configuración guardada. Intenta recargar o revisar permisos.</Alert>
          )}
          <Grid container spacing={2}>
            <Grid item xs={12} md={4}>
              <TextField
                label="Calendar ID"
                fullWidth
                value={calendarId}
                onChange={(e) => setCalendarId(e.target.value)}
                helperText={calendarIdError || 'Ej: primary o calendar-id@group.calendar.google.com'}
                required
                error={Boolean(calendarIdError)}
                FormHelperTextProps={calendarIdError ? { sx: { color: 'error.main' } } : undefined}
                placeholder="primary"
              />
            </Grid>
            <Grid item xs={12} md={4}>
              <TextField
                label="Desde (opcional)"
                fullWidth
                type="datetime-local"
                value={fromInput}
                onChange={(e) => setFromInput(e.target.value)}
                InputLabelProps={{ shrink: true }}
                error={Boolean(fromInput && !fromIso)}
                helperText={
                  fromInput && !fromIso
                    ? 'Fecha inválida, usa el selector.'
                    : 'Se convierte a UTC automáticamente.'
                }
              />
            </Grid>
            <Grid item xs={12} md={4}>
              <TextField
                label="Hasta (opcional)"
                fullWidth
                type="datetime-local"
                value={toInput}
                onChange={(e) => setToInput(e.target.value)}
                InputLabelProps={{ shrink: true }}
                error={Boolean(toInput && !toIso)}
                helperText={toInput && !toIso ? 'Fecha inválida, usa el selector.' : 'Déjalo vacío para traer todo.'}
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
              error={Boolean(codeError)}
              helperText={codeError || 'Se genera al aceptar en la ventana de Google.'}
            />
            <Button
              variant="contained"
              onClick={handleSaveTokens}
              disabled={!code.trim() || !trimmedCalendarId || exchangeMutation.isPending}
            >
              Guardar tokens
            </Button>
            <Button
              variant="contained"
              color="secondary"
              startIcon={<SyncIcon />}
              onClick={handleSync}
              disabled={!trimmedCalendarId || syncMutation.isPending || Boolean(rangeError)}
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
          <Divider />
          <Stack spacing={1}>
            <Typography variant="h6" fontWeight={700}>
              Rango a sincronizar (opcional)
            </Typography>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} flexWrap="wrap">
              <Button size="small" variant="outlined" onClick={() => applyRangePreset('next30')}>
                Próximos 30 días
              </Button>
              <Button size="small" variant="outlined" onClick={() => applyRangePreset('last30')}>
                Últimos 30 días
              </Button>
              <Button size="small" variant="outlined" onClick={() => applyRangePreset('thisMonth')}>
                Mes en curso
              </Button>
              <Button size="small" onClick={clearRange}>
                Sin filtro
              </Button>
            </Stack>
            {rangeError && <Alert severity="warning">{rangeError}</Alert>}
            <Typography variant="body2" color="text.secondary">
              Las fechas usan tu zona local ({zone}) y se guardan en UTC. Deja ambos campos vacíos para sincronizar todos los
              eventos disponibles.
            </Typography>
          </Stack>
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
                    <Typography fontWeight={700}>{ev.summary ?? '(Sin título)'}</Typography>
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
