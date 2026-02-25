import { useCallback, useEffect, useMemo, useState } from 'react';
import { useLocation } from 'react-router-dom';
import { useMutation, useQuery } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Autocomplete,
  Chip,
  Divider,
  Grid,
  LinearProgress,
  Paper,
  Stack,
  TextField,
  Typography,
  Snackbar,
} from '@mui/material';
import SyncIcon from '@mui/icons-material/Sync';
import LinkIcon from '@mui/icons-material/Link';
import EventIcon from '@mui/icons-material/Event';
import { DateTime } from 'luxon';
import { CalendarApi } from '../api/calendar';

const normalizeStoredText = (value: string | null): string => value?.trim() ?? '';

const normalizeHistoryEntries = (value: unknown): string[] => {
  if (!Array.isArray(value)) return [];
  return Array.from(
    new Set(
      value
        .map((item) => (typeof item === 'string' ? item.trim() : ''))
        .filter((item) => item.length > 0),
    ),
  ).slice(0, 5);
};

const sameStringArray = (a: string[], b: string[]) =>
  a.length === b.length && a.every((value, idx) => value === b[idx]);

export default function CalendarSyncPage() {
  const zone: string = import.meta.env['VITE_TZ'] ?? 'America/Guayaquil';
  const [calendarId, setCalendarId] = useState('');
  const [code, setCode] = useState('');
  const [fromInput, setFromInput] = useState('');
  const [toInput, setToInput] = useState('');
  const [connectedCalendar, setConnectedCalendar] = useState<string | null>(null);
  const [accountEmail, setAccountEmail] = useState('');
  const [calendarHistory, setCalendarHistory] = useState<string[]>([]);
  const [lastSyncAt, setLastSyncAt] = useState<string | null>(null);
  const [showValidation, setShowValidation] = useState(false);
  const [appliedRemoteConfig, setAppliedRemoteConfig] = useState(false);
  const [autoExchanging, setAutoExchanging] = useState(false);
  const [copyToast, setCopyToast] = useState<string | null>(null);
  const [syncToast, setSyncToast] = useState<{ message: string; severity: 'success' | 'error' | 'info' } | null>(null);

  const trimmedCalendarId = calendarId.trim();
  const location = useLocation();
  const icsUrl = useMemo(() => {
    if (typeof window === 'undefined') return '';
    const base = (import.meta.env['VITE_CALENDAR_ICS_BASE'] ?? `${window.location.origin}/calendar/v1/ics`).trim();
    const cal = trimmedCalendarId || 'primary';
    const separator = base.includes('?') ? '&' : '?';
    return `${base}${separator}calendarId=${encodeURIComponent(cal)}`;
  }, [trimmedCalendarId]);

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
    const storedAccount = window.localStorage.getItem('calendar-sync.account');
    const storedHistory = window.localStorage.getItem('calendar-sync.history');

    const normalizedStoredId = normalizeStoredText(storedId);
    setCalendarId(normalizedStoredId || 'primary');
    const normalizedConnected = normalizeStoredText(storedConnected);
    if (normalizedConnected) setConnectedCalendar(normalizedConnected);
    const normalizedLastSync = normalizeStoredText(storedLastSync);
    if (normalizedLastSync) setLastSyncAt(normalizedLastSync);
    const normalizedAccount = normalizeStoredText(storedAccount);
    if (normalizedAccount) setAccountEmail(normalizedAccount);
    if (storedHistory) {
      try {
        const parsed = JSON.parse(storedHistory) as unknown;
        const normalizedHistory = normalizeHistoryEntries(parsed);
        if (normalizedHistory.length > 0) setCalendarHistory(normalizedHistory);
      } catch {
        // ignore malformed history
      }
    }

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
    const normalizedCalendarId = trimmedCalendarId || 'primary';
    window.localStorage.setItem('calendar-sync.calendarId', normalizedCalendarId);
    setCalendarHistory((prev) => {
      const nextHistory = normalizeHistoryEntries([normalizedCalendarId, ...prev]);
      if (sameStringArray(prev, nextHistory)) return prev;
      window.localStorage.setItem('calendar-sync.history', JSON.stringify(nextHistory));
      return nextHistory;
    });
  }, [trimmedCalendarId]);

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

  useEffect(() => {
    if (typeof window === 'undefined') return;
    window.localStorage.setItem('calendar-sync.account', accountEmail.trim());
  }, [accountEmail]);

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
        calendarId: trimmedCalendarId || undefined,
        from: fromIso ?? undefined,
        to: toIso ?? undefined,
      }),
    enabled:
      Boolean(trimmedCalendarId) &&
      !rangeError &&
      (!fromInput || Boolean(fromIso)) &&
      (!toInput || Boolean(toIso)),
  });
  const syncMutation = useMutation({
    mutationFn: () =>
      CalendarApi.sync({
        calendarId: trimmedCalendarId || 'primary',
        from: fromIso ?? undefined,
        to: toIso ?? undefined,
      }),
    onSuccess: (res) => {
      const ts = new Date().toISOString();
      setLastSyncAt(ts);
      void eventsQuery.refetch();
      if (typeof window !== 'undefined') {
        window.localStorage.setItem('calendar-sync.lastSyncAt', ts);
      }
      setSyncToast({
        severity: 'success',
        message: `Sync OK (${new Date(ts).toLocaleString()}): ${res.created} creados, ${res.updated} actualizados.`,
      });
    },
    onError: () =>
      setSyncToast({
        severity: 'error',
        message: 'No pudimos sincronizar ahora. Revisa credenciales y rango.',
      }),
  });

  const configQuery = useQuery({
    queryKey: ['calendar-config'],
    queryFn: () => CalendarApi.getConfig(),
    staleTime: 5 * 60 * 1000,
  });
  const configErrorMessage =
    configQuery.error instanceof Error
      ? configQuery.error.message
      : configQuery.isError
        ? 'No se pudo cargar la configuración de calendario.'
        : null;

  const testConnection = useCallback(async () => {
    const res = await configQuery.refetch();
    setSyncToast(
      res.data
        ? { severity: 'info', message: 'Conexión verificada con el proveedor de calendario.' }
        : { severity: 'error', message: 'No pudimos validar la conexión. Revisa las credenciales.' },
    );
  }, [configQuery]);

  const authUrlMutation = useMutation({
    mutationFn: CalendarApi.getAuthUrl,
    onSuccess: (data) => {
      if (data.url && typeof window !== 'undefined') {
        window.open(data.url, '_blank', 'noopener,noreferrer');
      }
    },
  });

  const exchangeMutation = useMutation({
    mutationFn: (payload: { code: string; calendarId: string }) => CalendarApi.exchangeCode(payload),
    onSuccess: (_, variables) => {
      setCode('');
      setShowValidation(false);
      setConnectedCalendar(variables.calendarId);
      setLastSyncAt(null);
      void eventsQuery.refetch();
    },
  });

  const lastSyncSummary = useMemo(() => {
    if (!lastSyncAt) return 'Sin sincronizar';
    const formatted = new Date(lastSyncAt).toLocaleString();
    const fromLabel = fromInput ? new Date(fromInput).toLocaleString() : 'Sin fecha inicio';
    const toLabel = toInput ? new Date(toInput).toLocaleString() : 'Sin fecha fin';
    return `${formatted} · Rango: ${fromLabel} → ${toLabel}`;
  }, [fromInput, lastSyncAt, toInput]);

  const events = useMemo(() => eventsQuery.data ?? [], [eventsQuery.data]);
  const calendarIdError = showValidation && !trimmedCalendarId ? 'Ingresa el Calendar ID o usa "primary".' : '';
  const codeError = showValidation && !code.trim() ? 'Pega el code que te devuelve Google tras consentir.' : '';

  const handleQuickConnect = () => {
    if (!calendarId.trim()) {
      setCalendarId('primary');
    }
    setShowValidation(true);
    authUrlMutation.mutate();
  };

  const handleSaveTokens = () => {
    setShowValidation(true);
    if (!trimmedCalendarId || !code.trim()) return;
    const payload = { code: code.trim(), calendarId: trimmedCalendarId || 'primary' };
    exchangeMutation.mutate(payload);
    const nextHistory = Array.from(new Set([payload.calendarId, ...calendarHistory])).slice(0, 5);
    setCalendarHistory(nextHistory);
    if (typeof window !== 'undefined') {
      window.localStorage.setItem('calendar-sync.history', JSON.stringify(nextHistory));
    }
  };

  const handleSync = () => {
    setShowValidation(true);
    if (!trimmedCalendarId || !connectedCalendar || rangeError || (fromInput && !fromIso) || (toInput && !toIso)) return;
    syncMutation.mutate();
  };

  const handleDisconnect = () => {
    setConnectedCalendar(null);
    setLastSyncAt(null);
    setCode('');
    if (typeof window !== 'undefined') {
      window.localStorage.removeItem('calendar-sync.calendarId');
      window.localStorage.removeItem('calendar-sync.range');
      window.localStorage.removeItem('calendar-sync.connected');
      window.localStorage.removeItem('calendar-sync.lastSyncAt');
      window.localStorage.removeItem('calendar-sync.account');
      window.localStorage.removeItem('calendar-sync.history');
    }
    setCalendarId('primary');
    setAccountEmail('');
    setCalendarHistory([]);
  };

  // Auto-handle OAuth redirect ?code=... so users don't paste manually
  useEffect(() => {
    const params = new URLSearchParams(location.search);
    const codeParam = params.get('code');
    const calendarParam = params.get('calendarId');
    if (!codeParam || autoExchanging) return;
    setAutoExchanging(true);
    setCode(codeParam);
    if (calendarParam) setCalendarId(calendarParam);
    setShowValidation(true);
    const targetCalendar = calendarParam ?? (trimmedCalendarId || 'primary');
    exchangeMutation.mutate(
      { code: codeParam, calendarId: targetCalendar },
      {
        onSettled: () => setAutoExchanging(false),
      },
    );
  }, [autoExchanging, exchangeMutation, location.search, trimmedCalendarId]);

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
          <Alert severity="info" variant="outlined">
            {`Última sync: ${lastSyncSummary}`}
          </Alert>
          <Typography color="text.secondary">
            Conecta tu calendario y sincroniza eventos a la base de datos para usarlos en reportes, agenda interna y
            posteriores automatizaciones. Usa los pasos: 1) conectar con Google, 2) pegar el code, 3) guardar tokens, 4) sincronizar.
          </Typography>
          <Alert severity="info" variant="outlined">
            Tip rápido: el botón &quot;Conectar con Google&quot; abre el consentimiento y asume Calendar ID &quot;primary&quot;. Luego pega el
            code que devuelve Google, guarda tokens y ejecuta sincronizar.
          </Alert>
          <Stack direction="row" spacing={1} flexWrap="wrap">
            <Chip
              color={connectedCalendar ? 'success' : 'default'}
              label={connectedCalendar ? `Tokens guardados para ${connectedCalendar}` : 'Tokens pendientes'}
              size="small"
            />
            <Chip variant="outlined" label={`Zona local: ${zone}`} size="small" />
            <Chip
              variant="outlined"
              color={lastSyncAt ? 'secondary' : 'default'}
              size="small"
              label={`Última sync: ${lastSyncAt ? new Date(lastSyncAt).toLocaleString() : 'Sin sincronizar'}`}
            />
            <Button size="small" onClick={handleDisconnect} variant="outlined" color="inherit">
              Desconectar y limpiar
            </Button>
          </Stack>
          {configErrorMessage && (
            <Alert severity="warning">{configErrorMessage}</Alert>
          )}
          <Typography variant="subtitle1" fontWeight={700}>
            Pasos guiados
          </Typography>
          <Grid container spacing={2}>
            <Grid item xs={12} md={4}>
              <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
                <Stack spacing={1}>
                  <Typography variant="subtitle2" fontWeight={700}>
                    Paso 1 · Conectar con Google
                  </Typography>
                  <Autocomplete
                    freeSolo
                    options={calendarHistory}
                    value={calendarId}
                    onChange={(_, value) => setCalendarId(value ?? '')}
                    inputValue={calendarId}
                    onInputChange={(_, value) => setCalendarId(value)}
                    renderInput={(params) => (
                      <TextField
                        {...params}
                        label="Calendar ID"
                        fullWidth
                        helperText={calendarIdError || 'Ej: primary o calendar-id@group.calendar.google.com'}
                        required
                        error={Boolean(calendarIdError)}
                        FormHelperTextProps={calendarIdError ? { sx: { color: 'error.main' } } : undefined}
                        placeholder="primary"
                      />
                    )}
                  />
                  <Stack spacing={0.5}>
                    <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
                      <Typography variant="caption" color="text.secondary">
                        URL ICS:
                      </Typography>
                      <Typography variant="caption" sx={{ wordBreak: 'break-all', flex: 1 }}>
                        {icsUrl || '—'}
                      </Typography>
                      <Button
                        size="small"
                        variant="outlined"
                        onClick={() => {
                          if (!icsUrl) return;
                          navigator.clipboard.writeText(icsUrl).then(
                            () => setCopyToast('ICS copiado al portapapeles.'),
                            () => setCopyToast('No se pudo copiar el ICS, intenta manualmente.'),
                          );
                        }}
                      >
                        Copiar
                      </Button>
                    </Stack>
                    <Typography variant="caption" color="text.secondary">
                      Usa este enlace para suscribirte al calendario en Outlook/Apple/Google.
                    </Typography>
                  </Stack>
                  <TextField
                    label="Cuenta Google (opcional)"
                    fullWidth
                    value={accountEmail}
                    onChange={(e) => setAccountEmail(e.target.value)}
                    placeholder="tu.correo@gmail.com"
                    helperText="Solo referencia; ayuda a recordar qué cuenta está conectada."
                  />
                  <Button
                    variant="contained"
                    startIcon={<LinkIcon />}
                    onClick={handleQuickConnect}
                    disabled={authUrlMutation.isPending}
                  >
                    Abrir consentimiento
                  </Button>
                  <Button
                    variant="text"
                    startIcon={<LinkIcon />}
                    onClick={() => authUrlMutation.mutate()}
                    disabled={authUrlMutation.isPending}
                  >
                    Reabrir URL
                  </Button>
                </Stack>
              </Paper>
            </Grid>
            <Grid item xs={12} md={4}>
              <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
                <Stack spacing={1}>
                  <Typography variant="subtitle2" fontWeight={700}>
                    Paso 2 · Guardar tokens
                  </Typography>
                  <TextField
                    label="Code (pegado desde Google)"
                    value={code}
                    onChange={(e) => setCode(e.target.value)}
                    error={Boolean(codeError)}
                    helperText={codeError || 'Pega el code mostrado por Google tras aceptar el consentimiento.'}
                  />
                  <Button
                    variant="contained"
                    onClick={handleSaveTokens}
                    disabled={!code.trim() || !trimmedCalendarId || exchangeMutation.isPending}
                  >
                    Guardar tokens
                  </Button>
                  {exchangeMutation.isSuccess && <Alert severity="success">Tokens guardados.</Alert>}
                  {exchangeMutation.isError && (
                    <Alert severity="error">No se pudo intercambiar el code. Revisa el client_id/secret y el redirect.</Alert>
                  )}
                </Stack>
              </Paper>
            </Grid>
            <Grid item xs={12} md={4}>
              <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
                <Stack spacing={1}>
                  <Typography variant="subtitle2" fontWeight={700}>
                    Paso 3 · Sincronizar rango
                  </Typography>
                  <Stack direction="row" spacing={1} flexWrap="wrap">
                    <Chip label="Este mes" onClick={() => applyRangePreset('thisMonth')} variant="outlined" />
                    <Chip label="Próximos 30 días" onClick={() => applyRangePreset('next30')} variant="outlined" />
                    <Chip label="Últimos 30 días" onClick={() => applyRangePreset('last30')} variant="outlined" />
                    <Button size="small" onClick={clearRange}>
                      Limpiar rango
                    </Button>
                  </Stack>
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
                  <TextField
                    label="Hasta (opcional)"
                    fullWidth
                    type="datetime-local"
                    value={toInput}
                    onChange={(e) => setToInput(e.target.value)}
                    InputLabelProps={{ shrink: true }}
                    error={Boolean(toInput && !toIso)}
                    helperText={
                      toInput && !toIso ? 'Fecha inválida, usa el selector.' : 'Déjalo vacío para traer todo.'
                    }
                  />
                  {rangeError && <Alert severity="warning">{rangeError}</Alert>}
                  <Button
                    variant="contained"
                    color="secondary"
                      startIcon={<SyncIcon />}
                      onClick={handleSync}
                      disabled={Boolean(
                        !trimmedCalendarId ||
                        !connectedCalendar ||
                        syncMutation.isPending ||
                        Boolean(rangeError) ||
                        (fromInput && !fromIso) ||
                        (toInput && !toIso)
                      )}
                    >
                    Sincronizar ahora
                  </Button>
                  <Button
                    href="/configuracion/logs"
                    size="small"
                    startIcon={<LinkIcon />}
                    target="_blank"
                    rel="noreferrer"
                  >
                    Ver últimos logs
                  </Button>
                  <Typography variant="caption" color="text.secondary">
                    Última sync: {lastSyncAt ? new Date(lastSyncAt).toLocaleString() : 'Sin sincronizar'}
                  </Typography>
                </Stack>
              </Paper>
            </Grid>
          </Grid>
          <Divider />
          {exchangeMutation.isError && (
            <Alert severity="error">No se pudo intercambiar el code. Revisa el client_id/secret y el redirect.</Alert>
          )}
          {exchangeMutation.isSuccess && <Alert severity="success">Tokens guardados.</Alert>}
          {syncMutation.isError && <Alert severity="error">La sincronización falló.</Alert>}
          <Stack direction="row" spacing={1}>
            <Button
              variant="outlined"
              size="small"
              startIcon={<LinkIcon />}
              onClick={() => {
                void testConnection();
              }}
            >
              Probar conexión
            </Button>
          </Stack>
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
      <Snackbar
        open={Boolean(syncToast)}
        autoHideDuration={3200}
        onClose={() => setSyncToast(null)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
      >
        {syncToast ? (
          <Alert severity={syncToast.severity} onClose={() => setSyncToast(null)} variant="filled" sx={{ width: '100%' }}>
            {syncToast.message}
          </Alert>
        ) : undefined}
      </Snackbar>

      <Snackbar
        open={Boolean(copyToast)}
        autoHideDuration={2200}
        onClose={() => setCopyToast(null)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
        message={copyToast ?? ''}
      />
    </Stack>
  );
}
