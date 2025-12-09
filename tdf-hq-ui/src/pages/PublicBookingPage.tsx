import { useEffect, useMemo, useState } from 'react';
import {
  Alert,
  Autocomplete,
  type AutocompleteValue,
  Box,
  Button,
  Card,
  CardContent,
  Divider,
  Chip,
  Checkbox,
  CircularProgress,
  Grid,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import AccessTimeIcon from '@mui/icons-material/AccessTime';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import LocalPhoneIcon from '@mui/icons-material/LocalPhone';
import PersonIcon from '@mui/icons-material/Person';
import { Bookings } from '../api/bookings';
import type { BookingDTO } from '../api/types';
import { Engineers, type PublicEngineer } from '../api/engineers';
import { loadServiceTypes } from '../utils/serviceTypesStore';
import { useSession } from '../session/SessionContext';

interface FormState {
  fullName: string;
  email: string;
  phone: string;
  serviceType: string;
  startsAt: string;
  durationMinutes: number;
  notes: string;
  engineerId: number | null;
  engineerName: string;
}

const toLocalInputValue = (date: Date) => {
  const pad = (val: number) => val.toString().padStart(2, '0');
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}T${pad(date.getHours())}:${pad(
    date.getMinutes(),
  )}`;
};

const PROFILE_STORAGE_KEY = 'tdf-public-booking-profile';
const OPEN_HOURS = { start: 8, end: 22 }; // 24h local time

const localTimezoneLabel = () => {
  if (typeof Intl === 'undefined' || !Intl.DateTimeFormat) return 'tu zona horaria';
  const dtf = Intl.DateTimeFormat(undefined, { timeZoneName: 'short' });
  const parts = dtf.formatToParts(new Date());
  const zone = parts.find((p) => p.type === 'timeZoneName')?.value;
  return zone ?? 'tu zona horaria';
};

export default function PublicBookingPage() {
  const services = useMemo(() => loadServiceTypes(), []);
  const defaultService = services[0]?.name ?? 'Reserva';
  const { session, logout } = useSession();
  const [form, setForm] = useState<FormState>(() => {
    const start = new Date();
    start.setMinutes(start.getMinutes() + 90);
    start.setSeconds(0, 0);
    return {
      fullName: '',
      email: '',
      phone: '',
      serviceType: defaultService,
      startsAt: toLocalInputValue(start),
      durationMinutes: 60,
      notes: '',
      engineerId: null,
      engineerName: '',
    };
  });
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<BookingDTO | null>(null);
  const [rememberProfile, setRememberProfile] = useState(false);
  const [engineers, setEngineers] = useState<PublicEngineer[]>([]);
  const [engineersLoading, setEngineersLoading] = useState(false);
  const [engineersError, setEngineersError] = useState<string | null>(null);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    try {
      const raw = window.localStorage.getItem(PROFILE_STORAGE_KEY);
      if (!raw) return;
      const stored = JSON.parse(raw) as Partial<FormState>;
      setForm((prev) => ({
        ...prev,
        fullName: stored.fullName ?? prev.fullName,
        email: stored.email ?? prev.email,
        phone: stored.phone ?? prev.phone,
        serviceType: stored.serviceType ?? prev.serviceType,
      }));
      setRememberProfile(true);
    } catch {
      // ignore parsing issues
    }
  }, []);

  useEffect(() => {
    if (!session?.displayName) return;
    setForm((prev) => {
      if (prev.fullName.trim()) return prev;
      return { ...prev, fullName: session.displayName };
    });
  }, [session]);

  useEffect(() => {
    if (typeof window === 'undefined') return;
    if (!rememberProfile) {
      window.localStorage.removeItem(PROFILE_STORAGE_KEY);
      return;
    }
    const payload = {
      fullName: form.fullName.trim(),
      email: form.email.trim(),
      phone: form.phone.trim(),
      serviceType: form.serviceType,
    };
    window.localStorage.setItem(PROFILE_STORAGE_KEY, JSON.stringify(payload));
  }, [rememberProfile, form.fullName, form.email, form.phone, form.serviceType]);

  useEffect(() => {
    setEngineersLoading(true);
    Engineers.listPublic()
      .then((list) => {
        setEngineers(list);
        setEngineersError(list.length === 0 ? 'Escribe el nombre del ingeniero manualmente.' : null);
      })
      .catch(() => {
        setEngineers([]);
        setEngineersError('Ingresa el nombre manualmente (catálogo no disponible).');
      })
      .finally(() => setEngineersLoading(false));
  }, []);

  const handleSubmit = async (evt: React.FormEvent) => {
    evt.preventDefault();
    setError(null);
    setSuccess(null);

    if (!form.fullName.trim()) {
      setError('Agrega tu nombre para continuar.');
      return;
    }
    if (!form.email.trim()) {
      setError('Necesitamos un correo para confirmarte la reserva.');
      return;
    }
    if (!form.serviceType.trim()) {
      setError('Selecciona un tipo de servicio.');
      return;
    }

    const parsedStart = new Date(form.startsAt);
    if (Number.isNaN(parsedStart.getTime())) {
      setError('Selecciona una fecha y hora válida.');
      return;
    }
    const now = new Date();
    if (parsedStart.getTime() < now.getTime() + 15 * 60 * 1000) {
      setError('Elige un horario al menos 15 minutos en el futuro.');
      return;
    }
    const proposedEnd = new Date(parsedStart.getTime() + Math.max(30, Number(form.durationMinutes) || 60) * 60 * 1000);
    const startsBeforeOpen = parsedStart.getHours() < OPEN_HOURS.start;
    const endsAfterClose = proposedEnd.getHours() >= OPEN_HOURS.end;
    if (startsBeforeOpen || endsAfterClose) {
      setError(`Nuestro horario es ${OPEN_HOURS.start}:00 - ${OPEN_HOURS.end}:00. Ajusta la hora o la duración.`);
      return;
    }
    if (requiresEngineer(form.serviceType) && !form.engineerId && !form.engineerName.trim()) {
      setError('Selecciona un ingeniero para grabación/mezcla/mastering.');
      return;
    }

    setSubmitting(true);
    try {
      const startsAtIso = parsedStart.toISOString();
      const dto = await Bookings.createPublic({
        pbFullName: form.fullName.trim(),
        pbEmail: form.email.trim(),
        pbPhone: form.phone.trim() || null,
        pbServiceType: form.serviceType.trim(),
        pbStartsAt: startsAtIso,
        pbDurationMinutes: Math.max(30, Number(form.durationMinutes) || 60),
        pbNotes: form.notes.trim() || null,
        pbEngineerPartyId: form.engineerId,
        pbEngineerName: form.engineerName.trim() || null,
      });
      setSuccess(dto);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'No pudimos crear la reserva.';
      setError(message);
    } finally {
      setSubmitting(false);
    }
  };

  const servicePriceLookup = useMemo(() => {
    const map = new Map<string, string>();
    services.forEach((svc) => {
      const price = `${svc.currency} ${svc.price.toLocaleString(undefined, { minimumFractionDigits: 0 })}`;
      const unit = svc.billingUnit ? ` / ${svc.billingUnit}` : '';
      map.set(svc.name, `${price}${unit}`);
    });
    return map;
  }, [services]);
  const selectedPrice = servicePriceLookup.get(form.serviceType);
  const formattedStart = useMemo(() => {
    if (!form.startsAt) return null;
    const dt = new Date(form.startsAt);
    if (Number.isNaN(dt.getTime())) return null;
    return dt.toLocaleString(undefined, {
      weekday: 'short',
      month: 'short',
      day: 'numeric',
      hour: 'numeric',
      minute: '2-digit',
    });
  }, [form.startsAt]);

  const requiresEngineer = (service: string) => {
    const lowered = service.toLowerCase();
    return lowered.includes('graba') || lowered.includes('mezcl') || lowered.includes('master');
  };

  const engineerValue =
    (engineers.find((opt) => opt.peId === form.engineerId) as PublicEngineer | undefined) ??
    (form.engineerName ? { peId: -1, peName: form.engineerName } : null);
  const engineerHelper =
    engineersError ??
    (engineers.length === 0 ? 'Escribe el nombre del ingeniero asignado (catálogo no disponible).' : 'Selecciona o escribe el ingeniero asignado.');

  const clearSavedProfile = () => {
    setRememberProfile(false);
    setForm((prev) => ({
      ...prev,
      fullName: '',
      email: '',
      phone: '',
    }));
  };

  const outOfHours = useMemo(() => {
    const dt = new Date(form.startsAt);
    if (Number.isNaN(dt.getTime())) return null;
    const end = new Date(dt.getTime() + Math.max(30, Number(form.durationMinutes) || 60) * 60 * 1000);
    const startsBefore = dt.getHours() < OPEN_HOURS.start;
    const endsAfter = end.getHours() >= OPEN_HOURS.end;
    if (startsBefore || endsAfter) {
      return `Horario operativo: ${OPEN_HOURS.start}:00 - ${OPEN_HOURS.end}:00 (${localTimezoneLabel()}). Te sugerimos mover la cita.`;
    }
    return null;
  }, [form.durationMinutes, form.startsAt]);

  return (
    <Box sx={{ minHeight: '80vh', display: 'flex', alignItems: 'center', justifyContent: 'center', py: 4 }}>
      <Card
        sx={{
          maxWidth: 880,
          width: '100%',
          borderRadius: 3,
          boxShadow: '0 18px 72px rgba(15,17,24,0.26)',
          border: '1px solid rgba(255,255,255,0.08)',
          background: 'linear-gradient(135deg, rgba(255,255,255,0.02), rgba(30,64,175,0.06))',
        }}
      >
        <CardContent sx={{ p: { xs: 3, md: 5 } }}>
          <Stack spacing={2.5}>
            <Stack spacing={0.6}>
              <Typography variant="overline" color="text.secondary">
                Agenda pública
              </Typography>
              <Typography variant="h4" fontWeight={800}>
                Reserva un servicio con TDF
              </Typography>
              <Typography variant="body1" color="text.secondary">
                Completa tus datos y agenda el horario que prefieras. Confirmaremos la reserva por correo y, si aún no
                tienes cuenta, crearemos tu acceso automáticamente.
              </Typography>
              <Typography variant="body2" color="text.secondary">
                Horarios mostrados en <strong>{localTimezoneLabel()}</strong>.
              </Typography>
              <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                <Chip label="1. Agenda sin crear cuenta" size="small" variant="outlined" />
                <Chip label="2. Confirmamos por email" size="small" variant="outlined" />
                <Chip label="3. Coordinamos por WhatsApp si lo dejas" size="small" variant="outlined" />
              </Stack>
              <Card
                variant="outlined"
                sx={{
                  mt: 1,
                  borderColor: 'rgba(255,255,255,0.12)',
                  bgcolor: 'rgba(255,255,255,0.02)',
                }}
              >
                <CardContent sx={{ py: 1.5, px: 2 }}>
                  <Stack
                    direction={{ xs: 'column', sm: 'row' }}
                    spacing={1}
                    alignItems={{ xs: 'flex-start', sm: 'center' }}
                    justifyContent="space-between"
                    useFlexGap
                    flexWrap="wrap"
                  >
                    <Stack spacing={0.3}>
                      <Typography variant="subtitle2">¿Ya tienes cuenta?</Typography>
                      <Typography variant="body2" color="text.secondary">
                        Inicia sesión y saltamos tus datos para esta reserva. Si no tienes cuenta, puedes crearla rápido.
                      </Typography>
                    </Stack>
                    <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap">
                      <Button size="small" variant="outlined" href="/login?redirect=/reservar">
                        Iniciar sesión
                      </Button>
                      <Button size="small" variant="text" href="/login?redirect=/reservar">
                        Crear cuenta
                      </Button>
                      {session && (
                        <Chip
                          label={`Conectado como ${session.displayName}`}
                          color="primary"
                          onDelete={logout}
                          variant="outlined"
                          sx={{ borderRadius: 999 }}
                        />
                      )}
                    </Stack>
                  </Stack>
                </CardContent>
              </Card>
            </Stack>

            <Grid container spacing={2}>
              <Grid item xs={12} md={4}>
                <Stack spacing={1.2}>
                  <Stack direction="row" alignItems="center" spacing={1}>
                    <PersonIcon color="primary" fontSize="small" />
                    <Typography variant="subtitle2" color="text.secondary">
                      Datos de contacto
                    </Typography>
                  </Stack>
                  <Typography variant="body2" color="text.secondary">
                    Usa un correo válido para recibir la confirmación. Si eres nuevo, crearemos un perfil para ti.
                  </Typography>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <EventAvailableIcon color="primary" fontSize="small" />
                    <Typography variant="body2" color="text.secondary">
                      Bloque tentativo en el calendario.
                    </Typography>
                  </Stack>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <AccessTimeIcon color="primary" fontSize="small" />
                    <Typography variant="body2" color="text.secondary">
                      Duración estándar de 1h (ajústala si necesitas más tiempo).
                    </Typography>
                  </Stack>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <LocalPhoneIcon color="primary" fontSize="small" />
                    <Typography variant="body2" color="text.secondary">
                      Añade tu WhatsApp si prefieres coordinar por ahí.
                    </Typography>
                  </Stack>
                </Stack>
              </Grid>

              <Grid item xs={12} md={8}>
                <form onSubmit={handleSubmit}>
                  <Grid container spacing={2.5}>
                    <Grid item xs={12} sm={6}>
                      <TextField
                        label="Nombre completo"
                        value={form.fullName}
                        onChange={(e) => setForm((prev) => ({ ...prev, fullName: e.target.value }))}
                        fullWidth
                        required
                      />
                    </Grid>
                    <Grid item xs={12} sm={6}>
                      <TextField
                        label="Correo"
                        type="email"
                        value={form.email}
                        onChange={(e) => setForm((prev) => ({ ...prev, email: e.target.value }))}
                        fullWidth
                        required
                      />
                    </Grid>
                    <Grid item xs={12} sm={6}>
                      <TextField
                        label="WhatsApp / Teléfono"
                        value={form.phone}
                        onChange={(e) => setForm((prev) => ({ ...prev, phone: e.target.value }))}
                        fullWidth
                      />
                    </Grid>
                    <Grid item xs={12} sm={6}>
                      <TextField
                        label="Servicio"
                        select
                        value={form.serviceType}
                        onChange={(e) => setForm((prev) => ({ ...prev, serviceType: e.target.value }))}
                        fullWidth
                        required
                      >
                        {services.map((svc) => (
                          <MenuItem key={svc.id} value={svc.name}>
                            <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between" sx={{ width: '100%' }}>
                              <Typography>{svc.name}</Typography>
                              <Typography variant="body2" color="text.secondary">
                                {servicePriceLookup.get(svc.name)}
                              </Typography>
                            </Stack>
                          </MenuItem>
                        ))}
                        {services.length === 0 && <MenuItem value={defaultService}>{defaultService}</MenuItem>}
                      </TextField>
                    </Grid>
                    <Grid item xs={12} sm={7}>
                      <TextField
                        label="Fecha y hora"
                        type="datetime-local"
                        value={form.startsAt}
                        onChange={(e) => setForm((prev) => ({ ...prev, startsAt: e.target.value }))}
                        fullWidth
                        InputLabelProps={{ shrink: true }}
                        required
                        helperText={`Horario disponible: ${OPEN_HOURS.start}:00 - ${OPEN_HOURS.end}:00 (${localTimezoneLabel()})`}
                      />
                    </Grid>
                    <Grid item xs={12} sm={5}>
                      <Stack spacing={1}>
                        <TextField
                          label="Duración (min)"
                          type="number"
                          value={form.durationMinutes}
                          onChange={(e) => setForm((prev) => ({ ...prev, durationMinutes: Number(e.target.value) }))}
                          fullWidth
                          inputProps={{ min: 30, step: 15 }}
                        />
                        <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                          {[30, 60, 90, 120].map((value) => (
                            <Chip
                              key={value}
                              label={`${value} min`}
                              size="small"
                              color={form.durationMinutes === value ? 'primary' : 'default'}
                              onClick={() => setForm((prev) => ({ ...prev, durationMinutes: value }))}
                              sx={{ borderRadius: 999 }}
                            />
                          ))}
                        </Stack>
                      </Stack>
                    </Grid>
                    {outOfHours && (
                      <Grid item xs={12}>
                        <Alert severity="warning">{outOfHours}</Alert>
                      </Grid>
                    )}
                    {requiresEngineer(form.serviceType) && (
                      <Grid item xs={12}>
                    <Autocomplete<string | PublicEngineer, false, false, true>
                      options={engineers}
                      getOptionLabel={(opt) => (typeof opt === 'string' ? opt : opt.peName)}
                      loading={engineersLoading && engineers.length > 0}
                      freeSolo
                      value={engineerValue}
                      onChange={(_evt, value) => {
                        if (!value) {
                          setForm((prev) => ({ ...prev, engineerId: null, engineerName: '' }));
                              return;
                            }
                            const id = typeof value === 'string' ? null : value.peId;
                            const name = typeof value === 'string' ? value : value.peName;
                            setForm((prev) => ({ ...prev, engineerId: id, engineerName: name }));
                          }}
                          inputValue={form.engineerName}
                          onInputChange={(_evt, value) => setForm((prev) => ({ ...prev, engineerName: value }))}
                          renderInput={(params) => (
                            <TextField
                              {...params}
                              label="Ingeniero asignado"
                              placeholder="Elige quién llevará la sesión"
                              required
                              InputProps={{
                                ...params.InputProps,
                                endAdornment: (
                                  <>
                                    {engineersLoading ? <CircularProgress size={16} /> : null}
                                    {params.InputProps.endAdornment}
                                  </>
                                ),
                              }}
                              helperText={
                                engineersError ??
                                (engineers.length === 0 && !engineersLoading
                                  ? 'Escribe el nombre del ingeniero (catálogo no disponible).'
                                  : 'Selecciona o escribe el ingeniero asignado.')
                              }
                            />
                          )}
                        />
                        <Typography variant="caption" color="text.secondary">
                          Requerido para grabación, mezcla o mastering. Si no encuentras a tu ingeniero, escríbenos en notas.
                        </Typography>
                      </Grid>
                    )}
                    <Grid item xs={12}>
                      <TextField
                        label="Notas para el equipo"
                        value={form.notes}
                        onChange={(e) => setForm((prev) => ({ ...prev, notes: e.target.value }))}
                        fullWidth
                        multiline
                        minRows={3}
                        placeholder="Cuéntanos qué necesitas (ej: grabación de voz, mezcla, etc.)"
                      />
                    </Grid>
                    <Grid item xs={12}>
                      <Stack direction="row" spacing={1} alignItems="center">
                        <Checkbox
                          checked={rememberProfile}
                          onChange={(e) => setRememberProfile(e.target.checked)}
                          size="small"
                        />
                        <Typography variant="body2" color="text.secondary">
                          Recordar mis datos en este navegador para la próxima vez.
                        </Typography>
                        <Button
                          size="small"
                          variant="text"
                          onClick={clearSavedProfile}
                          sx={{ ml: 'auto' }}
                        >
                          Limpiar datos guardados
                        </Button>
                      </Stack>
                    </Grid>
                    {error && (
                      <Grid item xs={12}>
                        <Alert severity="error">{error}</Alert>
                      </Grid>
                    )}
                    {success && (
                      <Grid item xs={12}>
                        <Alert severity="success">
                          Reserva creada. Revisa tu correo para la confirmación. ID:{' '}
                          <strong>{success.bookingId}</strong> · Servicio: <strong>{success.serviceType ?? form.serviceType}</strong>
                        </Alert>
                      </Grid>
                    )}
                    <Grid item xs={12}>
                      <Card
                        variant="outlined"
                        sx={{
                          bgcolor: 'rgba(255,255,255,0.02)',
                          borderColor: 'rgba(255,255,255,0.08)',
                        }}
                      >
                        <CardContent>
                          <Stack spacing={1}>
                            <Typography variant="subtitle2" color="text.secondary">
                              Resumen rápido
                            </Typography>
                            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                              <Chip
                                label={form.serviceType || 'Servicio'}
                                size="small"
                                color="primary"
                                variant="outlined"
                              />
                              <Chip
                                label={formattedStart ? `Inicio: ${formattedStart}` : 'Elige fecha y hora'}
                                size="small"
                                variant="outlined"
                              />
                              <Chip
                                label={`Duración: ${form.durationMinutes || 60} min`}
                                size="small"
                                variant="outlined"
                              />
                              <Chip
                                label={selectedPrice ? `Referencia: ${selectedPrice}` : 'Precio se confirma contigo'}
                                size="small"
                                variant="outlined"
                              />
                              <Chip label={`Zona: ${localTimezoneLabel()}`} size="small" variant="outlined" />
                              {requiresEngineer(form.serviceType) && (
                                <Chip
                                  label={
                                    form.engineerName.trim()
                                      ? `Ingeniero: ${form.engineerName}`
                                      : 'Selecciona ingeniero'
                                  }
                                  size="small"
                                  color={form.engineerName.trim() ? 'primary' : 'default'}
                                  variant="outlined"
                                />
                              )}
                            </Stack>
                            <Divider sx={{ my: 1 }} />
                            <Typography variant="body2" color="text.secondary">
                              Te enviaremos la confirmación por correo y coordinaremos cualquier ajuste de horario o salas
                              contigo.
                            </Typography>
                          </Stack>
                        </CardContent>
                      </Card>
                    </Grid>
                    <Grid item xs={12}>
                      <Button type="submit" variant="contained" size="large" disabled={submitting} fullWidth>
                        {submitting ? 'Enviando…' : 'Confirmar reserva'}
                      </Button>
                    </Grid>
                  </Grid>
                </form>
              </Grid>
            </Grid>
          </Stack>
        </CardContent>
      </Card>
    </Box>
  );
}
