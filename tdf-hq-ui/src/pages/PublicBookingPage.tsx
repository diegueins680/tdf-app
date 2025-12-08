import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
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
import { loadServiceTypes } from '../utils/serviceTypesStore';

interface FormState {
  fullName: string;
  email: string;
  phone: string;
  serviceType: string;
  startsAt: string;
  durationMinutes: number;
  notes: string;
}

const toLocalInputValue = (date: Date) => {
  const pad = (val: number) => val.toString().padStart(2, '0');
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}T${pad(date.getHours())}:${pad(
    date.getMinutes(),
  )}`;
};

export default function PublicBookingPage() {
  const services = useMemo(() => loadServiceTypes(), []);
  const defaultService = services[0]?.name ?? 'Reserva';
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
    };
  });
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<BookingDTO | null>(null);

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

    setSubmitting(true);
    try {
      const startsAtIso = new Date(form.startsAt).toISOString();
      const dto = await Bookings.createPublic({
        pbFullName: form.fullName.trim(),
        pbEmail: form.email.trim(),
        pbPhone: form.phone.trim() || null,
        pbServiceType: form.serviceType.trim(),
        pbStartsAt: startsAtIso,
        pbDurationMinutes: Math.max(30, Number(form.durationMinutes) || 60),
        pbNotes: form.notes.trim() || null,
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
                      />
                    </Grid>
                    <Grid item xs={12} sm={5}>
                      <TextField
                        label="Duración (min)"
                        type="number"
                        value={form.durationMinutes}
                        onChange={(e) => setForm((prev) => ({ ...prev, durationMinutes: Number(e.target.value) }))}
                        fullWidth
                        inputProps={{ min: 30, step: 15 }}
                      />
                    </Grid>
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
