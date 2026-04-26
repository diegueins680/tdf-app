import { useMemo, useState } from 'react';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  Divider,
  FormControlLabel,
  Grid,
  InputAdornment,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import AccessTimeIcon from '@mui/icons-material/AccessTime';
import CalculateIcon from '@mui/icons-material/Calculate';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import GroupsIcon from '@mui/icons-material/Groups';
import LandscapeIcon from '@mui/icons-material/Landscape';
import RequestQuoteIcon from '@mui/icons-material/RequestQuote';
import { DateTime } from 'luxon';
import { Bookings } from '../api/bookings';
import { PUBLIC_BASE } from '../config/appConfig';

type EventType = 'wedding' | 'corporate' | 'retreat' | 'concert' | 'workshop' | 'photo';

interface EventTypeConfig {
  label: string;
  serviceType: string;
  baseCents: number;
  perGuestCents: number;
  minimumHours: number;
  includedGuests: number;
}

interface BookingFormState {
  fullName: string;
  email: string;
  phone: string;
  eventType: EventType;
  guests: number;
  startsAt: string;
  durationHours: number;
  setupHours: number;
  catering: boolean;
  production: boolean;
  transport: boolean;
  notes: string;
}

interface QuoteLine {
  label: string;
  amountCents: number;
}

const DOMO_IMAGE_URL = `${PUBLIC_BASE}/assets/tdf-ui/domo-pululahua-hero-cozy.jpg`;
const TAX_RATE = 0.12;
const CURRENCY = 'USD';
const MAX_QUOTE_GUESTS = 220;
const DOMO_GALLERY_IMAGES = [
  {
    src: `${PUBLIC_BASE}/assets/tdf-ui/domo-pululahua-terrace-evening.jpg`,
    alt: 'Domo del Pululahua con terraza limpia, jardines iluminados y sala exterior',
  },
  {
    src: `${PUBLIC_BASE}/assets/tdf-ui/domo-pululahua-entry-lanterns.jpg`,
    alt: 'Entrada iluminada del Domo del Pululahua con faroles y vegetacion',
  },
  {
    src: `${PUBLIC_BASE}/assets/tdf-ui/domo-pululahua-garden-evening.jpg`,
    alt: 'Jardin iluminado y camino de piedra hacia el Domo del Pululahua',
  },
] as const;
const EVENT_TYPES: Record<EventType, EventTypeConfig> = {
  wedding: {
    label: 'Boda',
    serviceType: 'Domo del Pululahua - boda',
    baseCents: 180000,
    perGuestCents: 800,
    minimumHours: 8,
    includedGuests: 60,
  },
  corporate: {
    label: 'Evento corporativo',
    serviceType: 'Domo del Pululahua - evento corporativo',
    baseCents: 120000,
    perGuestCents: 600,
    minimumHours: 6,
    includedGuests: 40,
  },
  retreat: {
    label: 'Retiro o taller',
    serviceType: 'Domo del Pululahua - retiro',
    baseCents: 95000,
    perGuestCents: 500,
    minimumHours: 6,
    includedGuests: 25,
  },
  concert: {
    label: 'Concierto',
    serviceType: 'Domo del Pululahua - concierto',
    baseCents: 150000,
    perGuestCents: 700,
    minimumHours: 7,
    includedGuests: 80,
  },
  workshop: {
    label: 'Workshop',
    serviceType: 'Domo del Pululahua - workshop',
    baseCents: 70000,
    perGuestCents: 450,
    minimumHours: 4,
    includedGuests: 20,
  },
  photo: {
    label: 'Sesion fotografica',
    serviceType: 'Domo del Pululahua - sesion fotografica',
    baseCents: 45000,
    perGuestCents: 300,
    minimumHours: 3,
    includedGuests: 8,
  },
};

const initialStart = () =>
  DateTime.now()
    .setZone('America/Guayaquil')
    .plus({ days: 14 })
    .set({ hour: 10, minute: 0, second: 0, millisecond: 0 })
    .toFormat("yyyy-LL-dd'T'HH:mm");

const initialForm: BookingFormState = {
  fullName: '',
  email: '',
  phone: '',
  eventType: 'wedding',
  guests: 80,
  startsAt: initialStart(),
  durationHours: 8,
  setupHours: 2,
  catering: true,
  production: true,
  transport: false,
  notes: '',
};

const money = (amountCents: number) =>
  new Intl.NumberFormat('es-EC', {
    style: 'currency',
    currency: CURRENCY,
    maximumFractionDigits: 0,
  }).format(amountCents / 100);

const clampNumber = (value: number, min: number, max: number) => {
  if (!Number.isFinite(value)) return min;
  return Math.min(max, Math.max(min, Math.round(value)));
};

const calculateQuote = (form: BookingFormState) => {
  const config = EVENT_TYPES[form.eventType];
  const guests = clampNumber(form.guests, 1, MAX_QUOTE_GUESTS);
  const billableHours = Math.max(config.minimumHours, clampNumber(form.durationHours, 1, 24));
  const setupHours = clampNumber(form.setupHours, 0, 12);
  const extraGuests = Math.max(0, guests - config.includedGuests);
  const lines: QuoteLine[] = [
    { label: `${config.label} en Domo del Pululahua`, amountCents: config.baseCents },
    { label: `Uso del venue por ${billableHours} horas`, amountCents: billableHours * 18000 },
  ];

  if (setupHours > 0) {
    lines.push({ label: `Montaje y desmontaje (${setupHours} horas)`, amountCents: setupHours * 7000 });
  }
  if (extraGuests > 0) {
    lines.push({ label: `${extraGuests} invitados adicionales`, amountCents: extraGuests * config.perGuestCents });
  }
  if (form.catering) {
    lines.push({ label: 'Catering y barra operados por Domo', amountCents: Math.max(35000, guests * 650) });
  }
  if (form.production) {
    lines.push({ label: 'Sonido e iluminacion base', amountCents: 42000 });
  }
  if (form.transport) {
    lines.push({ label: 'Coordinacion de transporte Quito - Pululahua', amountCents: 30000 });
  }

  const subtotalCents = lines.reduce((sum, line) => sum + line.amountCents, 0);
  const taxCents = Math.round(subtotalCents * TAX_RATE);
  const totalCents = subtotalCents + taxCents;
  const depositCents = Math.round(totalCents * 0.4);

  return { lines, subtotalCents, taxCents, totalCents, depositCents, billableHours, guests };
};

const toBookingIso = (value: string) => {
  const parsed = DateTime.fromFormat(value, "yyyy-LL-dd'T'HH:mm", { zone: 'America/Guayaquil' });
  if (!parsed.isValid) return null;
  return parsed.toUTC().toISO({ suppressMilliseconds: true });
};

const buildBookingNotes = (form: BookingFormState, quote: ReturnType<typeof calculateQuote>) => {
  const selectedAddons = [
    form.catering ? 'catering/barra' : null,
    form.production ? 'sonido e iluminacion' : null,
    form.transport ? 'transporte' : null,
  ].filter(Boolean);

  return [
    'Solicitud publica Domo del Pululahua',
    `Tipo: ${EVENT_TYPES[form.eventType].label}`,
    `Invitados: ${quote.guests}`,
    `Duracion: ${quote.billableHours} horas + ${form.setupHours} horas de montaje`,
    `Cotizacion estimada: ${money(quote.totalCents)} IVA incluido`,
    `Reserva sugerida: ${money(quote.depositCents)}`,
    selectedAddons.length ? `Adicionales: ${selectedAddons.join(', ')}` : 'Adicionales: ninguno',
    form.notes.trim() ? `Notas del cliente: ${form.notes.trim()}` : null,
  ]
    .filter(Boolean)
    .join('\n');
};

export default function DomoVenuePage() {
  const [form, setForm] = useState<BookingFormState>(initialForm);
  const [submitting, setSubmitting] = useState(false);
  const [status, setStatus] = useState<{ severity: 'success' | 'error'; message: string } | null>(null);
  const quote = useMemo(() => calculateQuote(form), [form]);
  const bookingIso = toBookingIso(form.startsAt);

  const updateForm = <Key extends keyof BookingFormState>(key: Key, value: BookingFormState[Key]) => {
    setForm((prev) => ({ ...prev, [key]: value }));
  };

  const submitBooking = async () => {
    setStatus(null);
    if (!form.fullName.trim() || !form.email.trim()) {
      setStatus({ severity: 'error', message: 'Necesitamos nombre y correo para preparar la reserva.' });
      return;
    }
    if (!bookingIso) {
      setStatus({ severity: 'error', message: 'Elige una fecha y hora valida para la reserva.' });
      return;
    }

    setSubmitting(true);
    try {
      await Bookings.createPublic({
        pbFullName: form.fullName.trim(),
        pbEmail: form.email.trim(),
        pbPhone: form.phone.trim() || null,
        pbServiceType: EVENT_TYPES[form.eventType].serviceType,
        pbStartsAt: bookingIso,
        pbDurationMinutes: quote.billableHours * 60,
        pbNotes: buildBookingNotes(form, quote),
      });
      setStatus({
        severity: 'success',
        message: 'Solicitud enviada. El equipo revisara disponibilidad, confirmara la cotizacion y te contactara para separar la fecha.',
      });
    } catch (err) {
      setStatus({
        severity: 'error',
        message: err instanceof Error ? err.message : 'No pudimos enviar la solicitud. Intenta nuevamente.',
      });
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <Box
      sx={{
        width: '100vw',
        position: 'relative',
        left: '50%',
        ml: '-50vw',
        mt: { xs: -2, md: -4 },
      }}
    >
      <Box
        sx={{
          minHeight: { xs: 620, md: 700 },
          display: 'flex',
          alignItems: 'flex-end',
          backgroundImage: `linear-gradient(90deg, rgba(4,8,12,0.82), rgba(4,8,12,0.52), rgba(4,8,12,0.22)), url(${DOMO_IMAGE_URL})`,
          backgroundSize: 'cover',
          backgroundPosition: { xs: '56% center', md: 'center' },
          color: '#fff',
          px: { xs: 2, md: 6 },
          py: { xs: 7, md: 9 },
        }}
      >
        <Stack spacing={3} sx={{ maxWidth: 820 }}>
          <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
            <Chip icon={<LandscapeIcon />} label="Pululahua" sx={{ bgcolor: 'rgba(255,255,255,0.16)', color: '#fff' }} />
            <Chip icon={<EventAvailableIcon />} label="Eventos privados" sx={{ bgcolor: 'rgba(255,255,255,0.16)', color: '#fff' }} />
          </Stack>
          <Typography component="h1" variant="h2" sx={{ fontWeight: 900, maxWidth: 720 }}>
            Domo del Pululahua
          </Typography>
          <Typography variant="h5" sx={{ maxWidth: 760, color: 'rgba(255,255,255,0.88)' }}>
            Cotiza y solicita tu fecha para bodas, eventos corporativos, conciertos, talleres, retiros y sesiones fotograficas en el borde del crater.
          </Typography>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            <Button
              variant="contained"
              size="large"
              startIcon={<RequestQuoteIcon />}
              href="#cotizar"
              sx={{ alignSelf: { xs: 'stretch', sm: 'flex-start' }, textTransform: 'none' }}
            >
              Cotizar evento
            </Button>
            <Button
              variant="outlined"
              size="large"
              startIcon={<EventAvailableIcon />}
              href="#reservar"
              sx={{
                alignSelf: { xs: 'stretch', sm: 'flex-start' },
                color: '#fff',
                borderColor: 'rgba(255,255,255,0.65)',
                textTransform: 'none',
              }}
            >
              Solicitar reserva
            </Button>
          </Stack>
        </Stack>
      </Box>

      <Box sx={{ px: { xs: 2, md: 6 }, py: { xs: 4, md: 6 }, bgcolor: '#f7f4ed' }}>
        <Grid container spacing={3} alignItems="stretch">
          {[
            ['Bodas y celebraciones', 'Ceremonia, recepcion y fotografia con vista natural.'],
            ['Empresas y workshops', 'Jornadas fuera de Quito con privacidad, montaje flexible y agenda por horas.'],
            ['Cultura y contenido', 'Conciertos, sesiones audiovisuales y retiros con una identidad clara de destino.'],
          ].map(([title, body]) => (
            <Grid item xs={12} md={4} key={title}>
              <Card sx={{ height: '100%', borderRadius: 1, boxShadow: 'none', border: '1px solid rgba(44,35,24,0.12)' }}>
                <CardContent>
                  <Typography variant="h6" fontWeight={800}>{title}</Typography>
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1 }}>{body}</Typography>
                </CardContent>
              </Card>
            </Grid>
          ))}
        </Grid>
      </Box>

      <Box sx={{ px: { xs: 2, md: 6 }, py: { xs: 4, md: 7 }, bgcolor: '#10151d', color: '#fff' }}>
        <Grid container spacing={3} alignItems="stretch">
          <Grid item xs={12} md={4}>
            <Stack spacing={2} sx={{ maxWidth: 440, height: '100%', justifyContent: 'center' }}>
              <Typography variant="overline" sx={{ color: 'rgba(255,255,255,0.62)', letterSpacing: 0 }}>
                Ambientacion
              </Typography>
              <Typography variant="h4" fontWeight={900}>
                Un lugar preparado para recibir bien.
              </Typography>
              <Typography sx={{ color: 'rgba(255,255,255,0.78)' }}>
                Terraza limpia, jardines cuidados, luz calida y mobiliario sobrio para que cada evento se sienta privado, elegante y cercano al paisaje.
              </Typography>
            </Stack>
          </Grid>
          <Grid item xs={12} md={8}>
            <Grid container spacing={2} sx={{ height: '100%' }}>
              <Grid item xs={12} sm={7}>
                <Stack spacing={2} sx={{ height: '100%' }}>
                  {[DOMO_GALLERY_IMAGES[0], DOMO_GALLERY_IMAGES[2]].map((image) => (
                    <Box
                      key={image.src}
                      component="img"
                      src={image.src}
                      alt={image.alt}
                      loading="lazy"
                      sx={{
                        width: '100%',
                        flex: 1,
                        minHeight: { xs: 220, md: 248 },
                        aspectRatio: '16 / 9',
                        objectFit: 'cover',
                        borderRadius: 1,
                        display: 'block',
                      }}
                    />
                  ))}
                </Stack>
              </Grid>
              <Grid item xs={12} sm={5}>
                <Box
                  component="img"
                  src={DOMO_GALLERY_IMAGES[1].src}
                  alt={DOMO_GALLERY_IMAGES[1].alt}
                  loading="lazy"
                  sx={{
                    width: '100%',
                    height: '100%',
                    minHeight: { xs: 420, sm: 520 },
                    aspectRatio: { xs: '4 / 5', sm: 'auto' },
                    objectFit: 'cover',
                    borderRadius: 1,
                    display: 'block',
                  }}
                />
              </Grid>
            </Grid>
          </Grid>
        </Grid>
      </Box>

      <Box id="cotizar" sx={{ px: { xs: 2, md: 6 }, py: { xs: 4, md: 7 } }}>
        <Grid container spacing={3}>
          <Grid item xs={12} lg={7}>
            <Card id="reservar" sx={{ borderRadius: 1 }}>
              <CardContent>
                <Stack spacing={2.5}>
                  <Stack spacing={0.75}>
                    <Typography variant="h4" fontWeight={900}>Cotizacion y reserva</Typography>
                    <Typography color="text.secondary">
                      La cifra es estimada. La confirmacion final depende de disponibilidad, montaje, proveedores y permisos.
                    </Typography>
                  </Stack>

                  {status && <Alert severity={status.severity}>{status.message}</Alert>}

                  <Grid container spacing={2}>
                    <Grid item xs={12} md={6}>
                      <TextField
                        label="Nombre"
                        value={form.fullName}
                        onChange={(event) => updateForm('fullName', event.target.value)}
                        fullWidth
                        required
                      />
                    </Grid>
                    <Grid item xs={12} md={6}>
                      <TextField
                        label="Correo"
                        value={form.email}
                        onChange={(event) => updateForm('email', event.target.value)}
                        type="email"
                        fullWidth
                        required
                      />
                    </Grid>
                    <Grid item xs={12} md={6}>
                      <TextField
                        label="WhatsApp"
                        value={form.phone}
                        onChange={(event) => updateForm('phone', event.target.value)}
                        fullWidth
                      />
                    </Grid>
                    <Grid item xs={12} md={6}>
                      <TextField
                        label="Tipo de evento"
                        value={form.eventType}
                        onChange={(event) => updateForm('eventType', event.target.value as EventType)}
                        select
                        fullWidth
                      >
                        {Object.entries(EVENT_TYPES).map(([key, config]) => (
                          <MenuItem key={key} value={key}>{config.label}</MenuItem>
                        ))}
                      </TextField>
                    </Grid>
                    <Grid item xs={12} md={4}>
                      <TextField
                        label="Invitados"
                        value={form.guests}
                        onChange={(event) => updateForm('guests', clampNumber(Number(event.target.value), 1, MAX_QUOTE_GUESTS))}
                        type="number"
                        fullWidth
                        helperText="Capacidad final sujeta a permisos, montaje y plan de seguridad."
                        inputProps={{ min: 1, max: MAX_QUOTE_GUESTS }}
                        InputProps={{ startAdornment: <InputAdornment position="start"><GroupsIcon fontSize="small" /></InputAdornment> }}
                      />
                    </Grid>
                    <Grid item xs={12} md={4}>
                      <TextField
                        label="Horas de evento"
                        value={form.durationHours}
                        onChange={(event) => updateForm('durationHours', clampNumber(Number(event.target.value), 1, 24))}
                        type="number"
                        fullWidth
                        InputProps={{ startAdornment: <InputAdornment position="start"><AccessTimeIcon fontSize="small" /></InputAdornment> }}
                      />
                    </Grid>
                    <Grid item xs={12} md={4}>
                      <TextField
                        label="Horas de montaje"
                        value={form.setupHours}
                        onChange={(event) => updateForm('setupHours', clampNumber(Number(event.target.value), 0, 12))}
                        type="number"
                        fullWidth
                      />
                    </Grid>
                    <Grid item xs={12} md={6}>
                      <TextField
                        label="Fecha y hora"
                        value={form.startsAt}
                        onChange={(event) => updateForm('startsAt', event.target.value)}
                        type="datetime-local"
                        fullWidth
                        InputLabelProps={{ shrink: true }}
                      />
                    </Grid>
                    <Grid item xs={12} md={6}>
                      <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                        <FormControlLabel
                          control={<Checkbox checked={form.catering} onChange={(event) => updateForm('catering', event.target.checked)} />}
                          label="Catering"
                        />
                        <FormControlLabel
                          control={<Checkbox checked={form.production} onChange={(event) => updateForm('production', event.target.checked)} />}
                          label="Sonido y luces"
                        />
                        <FormControlLabel
                          control={<Checkbox checked={form.transport} onChange={(event) => updateForm('transport', event.target.checked)} />}
                          label="Transporte"
                        />
                      </Stack>
                    </Grid>
                    <Grid item xs={12}>
                      <TextField
                        label="Notas"
                        value={form.notes}
                        onChange={(event) => updateForm('notes', event.target.value)}
                        fullWidth
                        multiline
                        minRows={3}
                        placeholder="Cuéntanos objetivo, formato, horario ideal, montaje, proveedores, restricciones o referencias."
                      />
                    </Grid>
                  </Grid>

                  <Button
                    variant="contained"
                    size="large"
                    startIcon={<EventAvailableIcon />}
                    onClick={() => {
                      void submitBooking();
                    }}
                    disabled={submitting}
                    sx={{ alignSelf: { xs: 'stretch', sm: 'flex-start' }, textTransform: 'none' }}
                  >
                    {submitting ? 'Enviando...' : 'Solicitar reserva'}
                  </Button>
                </Stack>
              </CardContent>
            </Card>
          </Grid>

          <Grid item xs={12} lg={5}>
            <Card sx={{ borderRadius: 1, position: { lg: 'sticky' }, top: 24 }}>
              <CardContent>
                <Stack spacing={2}>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <CalculateIcon color="primary" />
                    <Typography variant="h5" fontWeight={900}>Quote estimado</Typography>
                  </Stack>
                  <Stack spacing={1}>
                    {quote.lines.map((line) => (
                      <Stack key={line.label} direction="row" justifyContent="space-between" spacing={2}>
                        <Typography variant="body2" color="text.secondary">{line.label}</Typography>
                        <Typography variant="body2" fontWeight={700}>{money(line.amountCents)}</Typography>
                      </Stack>
                    ))}
                  </Stack>
                  <Divider />
                  <Stack direction="row" justifyContent="space-between">
                    <Typography color="text.secondary">Subtotal</Typography>
                    <Typography fontWeight={800}>{money(quote.subtotalCents)}</Typography>
                  </Stack>
                  <Stack direction="row" justifyContent="space-between">
                    <Typography color="text.secondary">IVA 12%</Typography>
                    <Typography fontWeight={800}>{money(quote.taxCents)}</Typography>
                  </Stack>
                  <Stack direction="row" justifyContent="space-between" alignItems="baseline">
                    <Typography variant="h6" fontWeight={900}>Total</Typography>
                    <Typography variant="h4" fontWeight={900}>{money(quote.totalCents)}</Typography>
                  </Stack>
                  <Alert severity="info">
                    Para separar fecha se estima una reserva de {money(quote.depositCents)}. El equipo puede ajustar el plan si el evento requiere permisos, carpa, seguridad, hospedaje o produccion especial.
                  </Alert>
                </Stack>
              </CardContent>
            </Card>
          </Grid>
        </Grid>
      </Box>
    </Box>
  );
}
