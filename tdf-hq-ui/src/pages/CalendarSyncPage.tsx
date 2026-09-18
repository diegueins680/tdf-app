import { useCallback, useEffect, useMemo, useState, useRef, useId } from 'react';
import { Link as RouterLink, useLocation, useNavigate } from 'react-router-dom';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
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
import { useSession, getActiveSession, type SessionUser } from '../session/SessionContext';
import { CalendarApi } from '../api/calendar';
import { isSessionAuthFailureMessage } from '../session/authEvents';
import { buildLoginRedirectPath } from '../utils/loginRouting';
import LazyPaginatedList from '../components/LazyPaginatedList';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { formatDateTime } from '../utils/formatters';

// Calendar preferences are optional; denial/quota failures must not block the
// authoritative API result or the form's in-memory state.
const readCalendarPreference = (key: string): string | null => {
  try { return window.localStorage.getItem(key); } catch { return null; }
};
const writeCalendarPreference = (key: string, value: string): void => {
  try { window.localStorage.setItem(key, value); } catch { /* Keep the form usable. */ }
};
const removeCalendarPreference = (key: string): void => {
  try { window.localStorage.removeItem(key); } catch { /* Best-effort preference cleanup. */ }
};

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

const SESSION_EXPIRED_MESSAGE =
  'La sesión del CMS expiró o el navegador no está enviando la cookie. Vuelve a iniciar sesión y reabre esta integración.';

const getCalendarPageErrorMessage = (error: unknown, fallback: string): string | null => {
  if (!(error instanceof Error)) return fallback;
  return isSessionAuthFailureMessage(error.message) ? SESSION_EXPIRED_MESSAGE : error.message;
};

export default function CalendarSyncPage() {
  const { session } = useSession();
  const occurrence = useRef({ session, generation: 0 });
  if (occurrence.current.session !== session) occurrence.current = { session, generation: occurrence.current.generation + 1 };
  return <CalendarSyncForm key={occurrence.current.generation} authority={session} />;
}

function CalendarSyncForm({ authority }: { authority: SessionUser | null }) {
  const scope = useId();
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const mounted = useRef(true);
  useEffect(() => { mounted.current = true; return () => { mounted.current = false; }; }, []);
  const isCurrent = useCallback(() => mounted.current && authority !== null && getActiveSession() === authority, [authority]);
  const exchangeBusy = useRef(false);
  const syncBusy = useRef(false);
  const consumedRedirect = useRef<string | null>(null);
  const { timezone: zone, locale } = useLocalePreferences();
  const displayDateTime = useCallback((value: Date | string | number) =>
    formatDateTime(value, { locale, timeZone: zone }), [locale, zone]);
  const [calendarId, setCalendarId] = useState('');
  const [code, setCode] = useState('');
  const [fromInput, setFromInput] = useState('');
  const [toInput, setToInput] = useState('');
  const [accountEmail, setAccountEmail] = useState('');
  const [calendarHistory, setCalendarHistory] = useState<string[]>([]);
  const [showValidation, setShowValidation] = useState(false);
  const [copyToast, setCopyToast] = useState<string | null>(null);
  const [syncToast, setSyncToast] = useState<{ message: string; severity: 'success' | 'error' | 'info' } | null>(null);

  const trimmedCalendarId = calendarId.trim();
  const selectedCalendar = useRef(trimmedCalendarId);
  selectedCalendar.current = trimmedCalendarId;
  const location = useLocation();
  const loginRedirectPath = useMemo(
    () => buildLoginRedirectPath(`${location.pathname}${location.search}${location.hash}`),
    [location.hash, location.pathname, location.search],
  );
  const icsUrl = useMemo(() => {
    if (typeof window === 'undefined') return '';
    const base = (import.meta.env?.VITE_CALENDAR_ICS_BASE ?? `${window.location.origin}/calendar/v1/ics`).trim();
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
    const storedId = readCalendarPreference('calendar-sync.calendarId');
    const storedRange = readCalendarPreference('calendar-sync.range');
    const storedAccount = readCalendarPreference('calendar-sync.account');
    const storedHistory = readCalendarPreference('calendar-sync.history');

    const normalizedStoredId = normalizeStoredText(storedId);
    setCalendarId(normalizedStoredId || 'primary');
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
    writeCalendarPreference('calendar-sync.calendarId', normalizedCalendarId);
    setCalendarHistory((prev) => {
      const nextHistory = normalizeHistoryEntries([normalizedCalendarId, ...prev]);
      if (sameStringArray(prev, nextHistory)) return prev;
      writeCalendarPreference('calendar-sync.history', JSON.stringify(nextHistory));
      return nextHistory;
    });
  }, [trimmedCalendarId]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    writeCalendarPreference('calendar-sync.range', JSON.stringify({ from: fromInput, to: toInput }));
  }, [fromInput, toInput]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    writeCalendarPreference('calendar-sync.account', accountEmail.trim());
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
    queryKey: ['calendar-events', scope, trimmedCalendarId, fromIso, toIso],
    queryFn: () =>
      CalendarApi.listEvents({
        calendarId: trimmedCalendarId || undefined,
        from: fromIso ?? undefined,
        to: toIso ?? undefined,
      }),
    enabled:
      Boolean(authority) && Boolean(trimmedCalendarId) &&
      !rangeError &&
      (!fromInput || Boolean(fromIso)) &&
      (!toInput || Boolean(toIso)),
  });
  const configKey = ['calendar-config', scope, trimmedCalendarId];
  const configQuery = useQuery({
    queryKey: configKey,
    queryFn: () => CalendarApi.getConfig(trimmedCalendarId),
    enabled: Boolean(authority) && Boolean(trimmedCalendarId),
    staleTime: 0,
  });
  const connectedCalendar = !configQuery.isError && configQuery.data?.calendarId === trimmedCalendarId
    ? configQuery.data.calendarId : null;
  const lastSyncAt = connectedCalendar ? configQuery.data?.syncedAt ?? null : null;
  const syncMutation = useMutation({
    mutationFn: (payload: { calendarId: string; from?: string; to?: string }) => CalendarApi.sync(payload),
    onSuccess: (res, payload) => {
      if (!isCurrent() || selectedCalendar.current !== payload.calendarId) return;
      void eventsQuery.refetch();
      void configQuery.refetch();
      setSyncToast({ severity: 'success', message: `Sincronización guardada: ${res.created} creados, ${res.updated} actualizados.` });
    },
    onSettled: () => { syncBusy.current = false; },
    onError: (_, payload) => {
      if (!isCurrent() || selectedCalendar.current !== payload.calendarId) return;
      setSyncToast({ severity: 'error', message: 'No pudimos sincronizar ahora. Revisa credenciales y rango.' });
    },
  });

  const configAuthError =
    configQuery.error instanceof Error && isSessionAuthFailureMessage(configQuery.error.message);
  const configErrorMessage = configQuery.isError
    ? getCalendarPageErrorMessage(configQuery.error, 'No se pudo cargar la configuración de calendario.')
    : null;

  const refreshConfig = async () => {
    const calendar = trimmedCalendarId;
    const res = await configQuery.refetch();
    if (!isCurrent() || selectedCalendar.current !== calendar) return;
    setSyncToast(res.isError
      ? { severity: 'error', message: 'No pudimos consultar la configuración. Inténtalo de nuevo.' }
      : res.data
        ? { severity: 'info', message: 'Configuración guardada en TDF. Sincroniza para comprobar el acceso a Google.' }
        : { severity: 'info', message: 'Este calendario no tiene una conexión guardada en TDF.' });
  };

  const authUrlMutation = useMutation({
    mutationFn: CalendarApi.getAuthUrl,
    onSuccess: (data) => {
      if (isCurrent() && data.url && typeof window !== 'undefined') {
        window.open(data.url, '_blank', 'noopener,noreferrer');
      }
    },
  });

  const exchangeMutation = useMutation({
    mutationFn: (payload: { code: string; calendarId: string }) => CalendarApi.exchangeCode(payload),
    onSuccess: async (config, variables) => {
      if (!isCurrent() || selectedCalendar.current !== variables.calendarId) return;
      const key = ['calendar-config', scope, variables.calendarId];
      await queryClient.cancelQueries({ queryKey: key, exact: true });
      if (!isCurrent() || selectedCalendar.current !== variables.calendarId) return;
      queryClient.setQueryData(key, config);
      setCode('');
      setShowValidation(false);
      void eventsQuery.refetch();
    },
    onSettled: () => { exchangeBusy.current = false; },
  });
  const { mutate: exchangeMutate } = exchangeMutation;
  const exchange = useCallback((payload: { code: string; calendarId: string }) => {
    if (!isCurrent() || exchangeBusy.current) return;
    exchangeBusy.current = true;
    exchangeMutate(payload);
  }, [exchangeMutate, isCurrent]);

  const lastSyncSummary = lastSyncAt ? displayDateTime(lastSyncAt) : 'Sin sincronizar';

  const events = useMemo(() => eventsQuery.data ?? [], [eventsQuery.data]);
  const eventsErrorMessage = eventsQuery.isError
    ? getCalendarPageErrorMessage(eventsQuery.error, 'No se pudieron cargar los eventos.')
    : null;
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
    exchange(payload);
    const nextHistory = Array.from(new Set([payload.calendarId, ...calendarHistory])).slice(0, 5);
    setCalendarHistory(nextHistory);
    if (typeof window !== 'undefined') {
      writeCalendarPreference('calendar-sync.history', JSON.stringify(nextHistory));
    }
  };

  const handleSync = () => {
    setShowValidation(true);
    if (!trimmedCalendarId || !connectedCalendar || rangeError || (fromInput && !fromIso) || (toInput && !toIso)) return;
    if (!isCurrent() || syncBusy.current) return;
    syncBusy.current = true;
    syncMutation.mutate({ calendarId: trimmedCalendarId, from: fromIso ?? undefined, to: toIso ?? undefined });
  };

  const handleClearPreferences = () => {
    setCode('');
    setFromInput('');
    setToInput('');
    setSyncToast(null);
    if (typeof window !== 'undefined') {
      removeCalendarPreference('calendar-sync.calendarId');
      removeCalendarPreference('calendar-sync.range');
      removeCalendarPreference('calendar-sync.connected');
      removeCalendarPreference('calendar-sync.lastSyncAt');
      removeCalendarPreference('calendar-sync.account');
      removeCalendarPreference('calendar-sync.history');
    }
    setCalendarId('primary');
    setAccountEmail('');
    setCalendarHistory([]);
  };

  // Consume the returned code once, before asynchronous work. The retained input
  // allows an explicit retry; a render, error or StrictMode replay cannot resubmit.
  useEffect(() => {
    if (!isCurrent()) return;
    const params = new URLSearchParams(location.search);
    const returnedCode = params.get('code');
    if (!returnedCode || consumedRedirect.current === returnedCode) return;
    consumedRedirect.current = returnedCode;
    params.delete('code');
    const calendar = (params.get('calendarId') ?? '').trim() || trimmedCalendarId || 'primary';
    navigate({ pathname: location.pathname, search: params.toString() ? `?${params}` : '', hash: location.hash }, { replace: true });
    setCalendarId(calendar);
    selectedCalendar.current = calendar;
    setCode(returnedCode);
    setShowValidation(true);
    // Let the mounted mutation observer settle before dispatch (including the
    // development StrictMode setup/cleanup cycle). Authority is checked again.
    queueMicrotask(() => exchange({ code: returnedCode, calendarId: calendar }));
  }, [exchange, isCurrent, location.hash, location.pathname, location.search, navigate, trimmedCalendarId]);

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
              label={connectedCalendar ? `Configuración guardada para ${connectedCalendar}` : 'Sin conexión confirmada'}
              size="small"
            />
            <Chip variant="outlined" label={`Zona local: ${zone}`} size="small" />
            <Chip
              variant="outlined"
              color={lastSyncAt ? 'secondary' : 'default'}
              size="small"
              label={`Última sync: ${lastSyncAt ? displayDateTime(lastSyncAt) : 'Sin sincronizar'}`}
            />
            <Button size="small" onClick={handleClearPreferences} disabled={exchangeMutation.isPending || syncMutation.isPending} variant="outlined" color="inherit">
              Limpiar preferencias locales
            </Button>
          </Stack>
          <Typography variant="caption" color="text.secondary">
            Limpiar preferencias sólo borra el formulario en este navegador. La conexión guardada permanece en TDF.
            Para revocar el acceso, usa los permisos de tu cuenta de Google.
          </Typography>
          {configErrorMessage && (
            <Alert
              severity="warning"
              action={
                configAuthError ? (
                  <Button component={RouterLink} to={loginRedirectPath} color="inherit" size="small">
                    Iniciar sesión
                  </Button>
                ) : undefined
              }
            >
              {configErrorMessage}
            </Alert>
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
                    disabled={exchangeMutation.isPending || syncMutation.isPending}
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
                    disabled={exchangeMutation.isPending}
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
                  {exchangeMutation.isSuccess && exchangeMutation.variables?.calendarId === trimmedCalendarId && connectedCalendar && <Alert severity="success">Conexión guardada en TDF.</Alert>}
                  {exchangeMutation.isError && exchangeMutation.variables?.calendarId === trimmedCalendarId && (
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
                    component={RouterLink}
                    to="/configuracion/logs"
                    size="small"
                    startIcon={<LinkIcon />}
                    target="_blank"
                    rel="noreferrer"
                  >
                    Ver últimos logs
                  </Button>
                  <Typography variant="caption" color="text.secondary">
                    Última sync: {lastSyncAt ? displayDateTime(lastSyncAt) : 'Sin sincronizar'}
                  </Typography>
                </Stack>
              </Paper>
            </Grid>
          </Grid>
          <Divider />
          {exchangeMutation.isError && exchangeMutation.variables?.calendarId === trimmedCalendarId && (
            <Alert severity="error">No se pudo intercambiar el code. Revisa el client_id/secret y el redirect.</Alert>
          )}
          {exchangeMutation.isSuccess && exchangeMutation.variables?.calendarId === trimmedCalendarId && connectedCalendar && <Alert severity="success">Conexión guardada en TDF.</Alert>}
          {syncMutation.isError && syncMutation.variables?.calendarId === trimmedCalendarId && <Alert severity="error">La sincronización falló.</Alert>}
          <Stack direction="row" spacing={1}>
            <Button
              variant="outlined"
              size="small"
              startIcon={<LinkIcon />}
              onClick={() => {
                void refreshConfig();
              }}
            >
              Actualizar configuración
            </Button>
          </Stack>
          {syncMutation.isSuccess && syncMutation.variables?.calendarId === trimmedCalendarId && (
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
          {eventsQuery.isLoading && <LinearProgress aria-label="Cargando eventos del calendario" />}
          {eventsErrorMessage && <Alert severity="error">{eventsErrorMessage}</Alert>}
          <Divider />
          <Stack spacing={1.5}>
            <LazyPaginatedList
              items={events}
              pagination={{ itemLabel: 'eventos', initialRowsPerPage: 10 }}
              renderItems={(visibleEvents) => (
                <Stack spacing={1.5}>
                  {visibleEvents.map((ev) => (
                    <Paper key={ev.eventId} variant="outlined" sx={{ p: 1.5, borderRadius: 2 }}>
                      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} justifyContent="space-between">
                        <Box sx={{ flexGrow: 1 }}>
                          <Typography fontWeight={700}>{ev.summary ?? '(Sin título)'}</Typography>
                          <Typography variant="body2" color="text.secondary">
                            {ev.startAt ? displayDateTime(ev.startAt) : 'Sin fecha'} —{' '}
                            {ev.endAt ? displayDateTime(ev.endAt) : 'Sin fin'}
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
                </Stack>
              )}
            />
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
