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
import LocalDiningIcon from '@mui/icons-material/LocalDining';
import LightbulbIcon from '@mui/icons-material/Lightbulb';
import VolumeUpIcon from '@mui/icons-material/VolumeUp';
import LocalParkingIcon from '@mui/icons-material/LocalParking';
import EventSeatIcon from '@mui/icons-material/EventSeat';
import WifiIcon from '@mui/icons-material/Wifi';
import SpaIcon from '@mui/icons-material/Spa';
import NaturePeopleIcon from '@mui/icons-material/NaturePeople';
import CameraAltIcon from '@mui/icons-material/CameraAlt';
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

const INFO_BAND = [
  { number: '01', label: 'Ubicación', value: 'Reserva Geobotánica Pululahua, Ecuador (0.027°N)' },
  { number: '02', label: 'Altitud', value: '2,800 metros sobre el nivel del mar' },
  { number: '03', label: 'Ecosistema', value: 'Bosque nublado andino, único en el mundo' },
  { number: '04', label: 'Capacidad', value: 'Hasta 120 personas en el geodomo' },
] as const;

const AMENITIES = [
  { icon: <LocalDiningIcon />, title: 'Catering', desc: 'Menú personalizado para tu evento' },
  { icon: <LightbulbIcon />, title: 'Iluminación', desc: 'Sistema de luces profesional' },
  { icon: <VolumeUpIcon />, title: 'Sonido', desc: 'Equipo de audio de alta fidelidad' },
  { icon: <LocalParkingIcon />, title: 'Parqueo', desc: 'Estacionamiento para 50 vehículos' },
  { icon: <EventSeatIcon />, title: 'Capacidad', desc: 'Hasta 120 personas' },
  { icon: <WifiIcon />, title: 'WiFi', desc: 'Conexión de alta velocidad' },
  { icon: <SpaIcon />, title: 'Bienestar', desc: 'Espacios para yoga y meditación' },
  { icon: <NaturePeopleIcon />, title: 'Naturaleza', desc: 'Vistas al cráter volcánico' },
  { icon: <CameraAltIcon />, title: 'Contenido', desc: 'Locación ideal para producciones' },
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
    label: 'Sesión fotográfica',
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
    lines.push({ label: 'Sonido e iluminación base', amountCents: 42000 });
  }
  if (form.transport) {
    lines.push({ label: 'Coordinación de transporte Quito - Pululahua', amountCents: 30000 });
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
    form.production ? 'sonido e iluminación' : null,
    form.transport ? 'transporte' : null,
  ].filter(Boolean);

  return [
    'Solicitud pública Domo del Pululahua',
    `Tipo: ${EVENT_TYPES[form.eventType].label}`,
    `Invitados: ${quote.guests}`,
    `Duración: ${quote.billableHours} horas + ${form.setupHours} horas de montaje`,
    `Cotización estimada: ${money(quote.totalCents)} IVA incluido`,
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
      setStatus({ severity: 'error', message: 'Elige una fecha y hora válida para la reserva.' });
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
        message: 'Solicitud enviada. El equipo revisará disponibilidad, confirmará la cotización y te contactará para separar la fecha.',
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
      {/* HERO */}
      <Box
        sx={{
          minHeight: { xs: '100vh', md: '100vh' },
          display: 'flex',
          alignItems: 'flex-end',
          backgroundImage: `linear-gradient(180deg, rgba(4,8,12,0.45) 0%, rgba(4,8,12,0.65) 60%, rgba(4,8,12,0.92) 100%), url(${DOMO_IMAGE_URL})`,
          backgroundSize: 'cover',
          backgroundPosition: { xs: '56% center', md: 'center' },
          color: '#fff',
          px: { xs: 2, md: 6 },
          py: { xs: 8, md: 10 },
          position: 'relative',
        }}
      >
        <Stack spacing={3} sx={{ maxWidth: 820, position: 'relative', zIndex: 2 }}>
          <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
            <Chip icon={<LandscapeIcon />} label="Pululahua" sx={{ bgcolor: 'rgba(255,255,255,0.16)', color: '#fff', backdropFilter: 'blur(8px)' }} />
            <Chip icon={<EventAvailableIcon />} label="Eventos privados" sx={{ bgcolor: 'rgba(255,255,255,0.16)', color: '#fff', backdropFilter: 'blur(8px)' }} />
            <Chip label="2,800 msnm" sx={{ bgcolor: 'rgba(255,255,255,0.16)', color: '#fff', backdropFilter: 'blur(8px)' }} />
          </Stack>
          <Typography component="h1" variant="h2" sx={{ fontWeight: 900, maxWidth: 720, fontSize: { xs: '2.5rem', md: '3.75rem' }, lineHeight: 1.1, textShadow: '0 2px 40px rgba(0,0,0,0.4)' }}>
            Domo del Pululahua
          </Typography>
          <Typography variant="h5" sx={{ maxWidth: 760, color: 'rgba(255,255,255,0.88)', fontSize: { xs: '1.125rem', md: '1.5rem' }, lineHeight: 1.5, textShadow: '0 2px 20px rgba(0,0,0,0.3)' }}>
            Un geodomo en el borde del cráter. Cotiza y solicita tu fecha para bodas, eventos corporativos, conciertos, talleres, retiros y sesiones fotográficas.
          </Typography>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            <Button
              variant="contained"
              size="large"
              startIcon={<RequestQuoteIcon />}
              href="#cotizar"
              sx={{ alignSelf: { xs: 'stretch', sm: 'flex-start' }, textTransform: 'none', px: 4, py: 1.5, fontSize: '1rem' }}
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
                px: 4,
                py: 1.5,
                fontSize: '1rem',
              }}
            >
              Solicitar reserva
            </Button>
          </Stack>
        </Stack>

        {/* Scroll indicator */}
        <Box sx={{ position: 'absolute', bottom: 24, right: { xs: 16, md: 48 }, display: { xs: 'none', md: 'flex' }, flexDirection: 'column', alignItems: 'center', gap: 1 }}>
          <Box sx={{ width: '1px', height: 40, bgcolor: 'rgba(255,255,255,0.4)', position: 'relative' }}>
            <Box sx={{ position: 'absolute', top: 0, left: '50%', transform: 'translateX(-50%)', width: 6, height: 6, borderRadius: '50%', bgcolor: '#fff', opacity: 0.8 }} />
          </Box>
          <Typography sx={{ fontSize: '0.7rem', textTransform: 'uppercase', letterSpacing: 1.5, color: 'rgba(255,255,255,0.5)' }}>
            Pululahua, Ecuador
          </Typography>
        </Box>
      </Box>

      {/* INFO BAND */}
      <Box sx={{ bgcolor: '#1B4332', color: '#fff', py: 5, px: { xs: 2, md: 6 } }}>
        <Grid container spacing={4} sx={{ maxWidth: 1200, mx: 'auto' }}>
          {INFO_BAND.map((item) => (
            <Grid item xs={6} md={3} key={item.number}>
              <Typography variant="h3" sx={{ fontWeight: 300, fontSize: '2.5rem', color: 'rgba(255,255,255,0.3)', fontFamily: 'Geist, sans-serif' }}>
                {item.number}
              </Typography>
              <Typography sx={{ mt: 1, fontSize: '0.75rem', textTransform: 'uppercase', letterSpacing: 1.5, color: 'rgba(255,255,255,0.6)' }}>
                {item.label}
              </Typography>
              <Typography sx={{ mt: 0.5, fontSize: '1rem', color: '#fff' }}>
                {item.value}
              </Typography>
            </Grid>
          ))}
        </Grid>
      </Box>

      {/* EVENT TYPES */}
      <Box sx={{ px: { xs: 2, md: 6 }, py: { xs: 5, md: 7 }, bgcolor: '#f7f4ed' }}>
        <Typography variant="overline" sx={{ color: 'rgba(27,67,50,0.6)', letterSpacing: 2, display: 'block', textAlign: 'center', mb: 1 }}>
          Experiencias
        </Typography>
        <Typography variant="h3" sx={{ fontWeight: 900, textAlign: 'center', mb: 5, color: '#1a1a1a', fontSize: { xs: '1.75rem', md: '2.5rem' } }}>
          Tu evento, elevado
        </Typography>
        <Grid container spacing={3} alignItems="stretch">
          {[
            ['Bodas y celebraciones', 'Ceremonia, recepción y fotografía con vista natural. Cada detalle diseñado para que tu celebración sea inolvidable.'],
            ['Empresas y workshops', 'Jornadas fuera de Quito con privacidad, montaje flexible y agenda por horas. Retiros de equipo en la montaña.'],
            ['Cultura y contenido', 'Conciertos, sesiones audiovisuales y retiros con una identidad clara de destino. Festivales que resuenan en el cráter.'],
          ].map(([title, body]) => (
            <Grid item xs={12} md={4} key={title}>
              <Card sx={{ height: '100%', borderRadius: 2, boxShadow: 'none', border: '1px solid rgba(44,35,24,0.12)', bgcolor: '#fff' }}>
                <CardContent sx={{ p: 3 }}>
                  <Typography variant="h6" fontWeight={800} sx={{ color: '#1a1a1a' }}>{title}</Typography>
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1.5, lineHeight: 1.6 }}>{body}</Typography>
                </CardContent>
              </Card>
            </Grid>
          ))}
        </Grid>
      </Box>

      {/* GALLERY / AMBIENTACIÓN */}
      <Box sx={{ px: { xs: 2, md: 6 }, py: { xs: 5, md: 7 }, bgcolor: '#10151d', color: '#fff' }}>
        <Grid container spacing={3} alignItems="stretch">
          <Grid item xs={12} md={4}>
            <Stack spacing={2} sx={{ maxWidth: 440, height: '100%', justifyContent: 'center' }}>
              <Typography variant="overline" sx={{ color: 'rgba(255,255,255,0.62)', letterSpacing: 0 }}>
                Ambientación
              </Typography>
              <Typography variant="h4" fontWeight={900} sx={{ fontSize: { xs: '1.75rem', md: '2.25rem' } }}>
                Un lugar preparado para recibir bien.
              </Typography>
              <Typography sx={{ color: 'rgba(255,255,255,0.78)', lineHeight: 1.7 }}>
                Terraza limpia, jardines cuidados, luz cálida y mobiliario sobrio para que cada evento se sienta privado, elegante y cercano al paisaje.
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

      {/* AMENITIES */}
      <Box sx={{ bgcolor: '#0F0F0F', color: '#fff', py: { xs: 6, md: 8 }, px: { xs: 2, md: 6 } }}>
        <Box sx={{ maxWidth: 1200, mx: 'auto' }}>
          <Box sx={{ textAlign: 'center', mb: 5 }}>
            <Typography variant="overline" sx={{ color: '#1B4332', letterSpacing: 2, display: 'block' }}>
              Lo que ofrecemos
            </Typography>
            <Typography variant="h3" sx={{ fontWeight: 300, mt: 1, fontSize: { xs: '1.75rem', md: '2.5rem' }, color: '#fff' }}>
              Comodidades del domo
            </Typography>
          </Box>
          <Grid container spacing={2}>
            {AMENITIES.map((amenity, i) => (
              <Grid item xs={6} md={4} key={i}>
                <Card
                  sx={{
                    bgcolor: 'rgba(255,255,255,0.03)',
                    border: '1px solid rgba(255,255,255,0.06)',
                    borderRadius: 2,
                    color: '#fff',
                    p: 3,
                    height: '100%',
                    transition: 'all 0.3s ease',
                    '&:hover': {
                      bgcolor: 'rgba(255,255,255,0.06)',
                      borderColor: 'rgba(27,67,50,0.3)',
                    },
                  }}
                >
                  <Box sx={{ color: '#1B4332', mb: 1.5 }}>{amenity.icon}</Box>
                  <Typography variant="subtitle1" fontWeight={600} sx={{ mb: 0.5, color: '#fff' }}>
                    {amenity.title}
                  </Typography>
                  <Typography variant="body2" sx={{ color: 'rgba(255,255,255,0.5)', fontSize: '0.8125rem', lineHeight: 1.5 }}>
                    {amenity.desc}
                  </Typography>
                </Card>
              </Grid>
            ))}
          </Grid>
        </Box>
      </Box>

      {/* QUOTE & BOOKING */}
      <Box id="cotizar" sx={{ px: { xs: 2, md: 6 }, py: { xs: 5, md: 7 }, bgcolor: '#faf8f3' }}>
        <Grid container spacing={3}>
          <Grid item xs={12} lg={7}>
            <Card id="reservar" sx={{ borderRadius: 2, boxShadow: '0 4px 24px rgba(0,0,0,0.06)' }}>
              <CardContent sx={{ p: { xs: 2.5, md: 4 } }}>
                <Stack spacing={2.5}>
                  <Stack spacing={0.75}>
                    <Typography variant="h4" fontWeight={900} sx={{ fontSize: { xs: '1.5rem', md: '2rem' } }}>
                      Cotización y reserva
                    </Typography>
                    <Typography color="text.secondary">
                      La cifra es estimada. La confirmación final depende de disponibilidad, montaje, proveedores y permisos.
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
                      <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap alignItems="center" sx={{ height: '100%' }}>
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
                    sx={{ alignSelf: { xs: 'stretch', sm: 'flex-start' }, textTransform: 'none', px: 4, py: 1.5, fontSize: '1rem' }}
                  >
                    {submitting ? 'Enviando...' : 'Solicitar reserva'}
                  </Button>
                </Stack>
              </CardContent>
            </Card>
          </Grid>

          <Grid item xs={12} lg={5}>
            <Card sx={{ borderRadius: 2, position: { lg: 'sticky' }, top: 24, boxShadow: '0 4px 24px rgba(0,0,0,0.06)' }}>
              <CardContent sx={{ p: { xs: 2.5, md: 4 } }}>
                <Stack spacing={2}>
                  <Stack direction="row" spacing={1} alignItems="center">
                    <CalculateIcon color="primary" />
                    <Typography variant="h5" fontWeight={900}>
                      Quote estimado
                    </Typography>
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
                    Para separar fecha se estima una reserva de {money(quote.depositCents)}. El equipo puede ajustar el plan si el evento requiere permisos, carpa, seguridad, hospedaje o producción especial.
                  </Alert>
                </Stack>
              </CardContent>
            </Card>
          </Grid>
        </Grid>
      </Box>

      {/* LOCATION MAP */}
      <Box sx={{ bgcolor: '#0F0F0F', color: '#fff', py: { xs: 6, md: 8 }, px: { xs: 2, md: 6 } }}>
        <Box sx={{ maxWidth: 1200, mx: 'auto' }}>
          <Box sx={{ mb: 4 }}>
            <Typography variant="overline" sx={{ color: '#1B4332', letterSpacing: 2, display: 'block' }}>
              Ubicación
            </Typography>
            <Typography variant="h3" sx={{ fontWeight: 300, mt: 1, fontSize: { xs: '1.75rem', md: '2.5rem' }, color: '#fff' }}>
              En el borde del cráter
            </Typography>
            <Typography sx={{ mt: 1.5, color: 'rgba(255,255,255,0.6)', maxWidth: 500 }}>
              A solo 30 minutos de Quito, en la Reserva Geobotánica Pululahua, uno de los dos cráteres volcánicos habitados del mundo.
            </Typography>
          </Box>

          <Grid container spacing={3}>
            <Grid item xs={12} lg={8}>
              <Box sx={{ position: 'relative', overflow: 'hidden', borderRadius: 2, height: 400 }}>
                <iframe
                  src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d3988.9676!2d-78.4810162!3d0.0271897!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x8e2a79001eab74eb%3A0xbc2b47c9c62deab0!2sDomo+del+Pululahua!5e0!3m2!1ses!2sec!4v1700000000000!5m2!1ses!2sec"
                  width="100%"
                  height="100%"
                  style={{ border: 0, filter: 'grayscale(100%) contrast(1.1) opacity(0.85)' }}
                  allowFullScreen
                  loading="lazy"
                  referrerPolicy="no-referrer-when-downgrade"
                  title="Domo del Pululahua - Google Maps"
                />
                <Box sx={{ position: 'absolute', inset: 0, pointerEvents: 'none', boxShadow: 'inset 0 0 60px rgba(0,0,0,0.4)' }} />
              </Box>
            </Grid>
            <Grid item xs={12} lg={4}>
              <Stack spacing={2}>
                <Card sx={{ bgcolor: 'rgba(255,255,255,0.04)', border: '1px solid rgba(255,255,255,0.08)', borderRadius: 2, color: '#fff' }}>
                  <CardContent>
                    <Typography variant="overline" sx={{ color: 'rgba(255,255,255,0.4)', letterSpacing: 1, display: 'block', mb: 0.5 }}>
                      Dirección
                    </Typography>
                    <Typography sx={{ color: 'rgba(255,255,255,0.8)', fontSize: '0.9375rem' }}>
                      2GG9+VH, Quito<br />
                      Reserva Geobotánica Pululahua
                    </Typography>
                  </CardContent>
                </Card>
                <Card sx={{ bgcolor: 'rgba(255,255,255,0.04)', border: '1px solid rgba(255,255,255,0.08)', borderRadius: 2, color: '#fff' }}>
                  <CardContent>
                    <Typography variant="overline" sx={{ color: 'rgba(255,255,255,0.4)', letterSpacing: 1, display: 'block', mb: 0.5 }}>
                      Coordenadas
                    </Typography>
                    <Typography sx={{ color: 'rgba(255,255,255,0.8)', fontSize: '0.9375rem' }}>
                      0.027°N, 78.481°W<br />
                      <Box component="span" sx={{ color: '#1B4332' }}>A pasos de la línea del Ecuador</Box>
                    </Typography>
                  </CardContent>
                </Card>
                <Card sx={{ bgcolor: 'rgba(255,255,255,0.04)', border: '1px solid rgba(255,255,255,0.08)', borderRadius: 2, color: '#fff' }}>
                  <CardContent>
                    <Typography variant="overline" sx={{ color: 'rgba(255,255,255,0.4)', letterSpacing: 1, display: 'block', mb: 0.5 }}>
                      Desde Quito
                    </Typography>
                    <Typography sx={{ color: 'rgba(255,255,255,0.8)', fontSize: '0.9375rem' }}>
                      ~30 minutos por la calle Pululahua<br />
                      Vía a la Mitad del Mundo
                    </Typography>
                  </CardContent>
                </Card>
                <Button
                  component="a"
                  href="https://maps.app.goo.gl/z4tkBi8o33uY7rT79"
                  target="_blank"
                  rel="noopener noreferrer"
                  fullWidth
                  sx={{
                    py: 1.5,
                    borderRadius: 2,
                    textTransform: 'none',
                    fontWeight: 600,
                    bgcolor: '#1B4332',
                    color: '#0F0F0F',
                    '&:hover': { bgcolor: '#2d6a4f' },
                  }}
                >
                  Ver en Google Maps
                </Button>
              </Stack>
            </Grid>
          </Grid>
        </Box>
      </Box>

      {/* CONTACT CTA */}
      <Box
        sx={{
          py: { xs: 6, md: 8 },
          px: { xs: 2, md: 6 },
          textAlign: 'center',
          bgcolor: '#0a0a0a',
          background: 'linear-gradient(180deg, rgba(27,67,50,0.12) 0%, rgba(27,67,50,0.04) 100%)',
        }}
      >
        <Box sx={{ maxWidth: 700, mx: 'auto' }}>
          <Typography variant="h3" sx={{ fontWeight: 300, color: '#fff', fontSize: { xs: '1.75rem', md: '2.75rem' }, lineHeight: 1.1 }}>
            Hagamos tu evento realidad
          </Typography>
          <Typography sx={{ mt: 2, color: 'rgba(255,255,255,0.6)', fontSize: '1.0625rem' }}>
            Contáctanos para planificar tu celebración en el cráter.
          </Typography>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2} justifyContent="center" mt={4}>
            <Button
              component="a"
              href="https://wa.me/593984755301"
              target="_blank"
              rel="noopener noreferrer"
              variant="contained"
              size="large"
              sx={{
                px: 4,
                py: 1.5,
                borderRadius: 8,
                textTransform: 'none',
                fontSize: '1rem',
                bgcolor: '#1B4332',
                color: '#0F0F0F',
                fontWeight: 600,
                '&:hover': { bgcolor: '#2d6a4f', transform: 'scale(1.02)' },
                transition: 'all 0.3s',
              }}
            >
              WhatsApp
            </Button>
            <Button
              component="a"
              href="https://instagram.com/domo.pululahua"
              target="_blank"
              rel="noopener noreferrer"
              variant="outlined"
              size="large"
              sx={{
                px: 4,
                py: 1.5,
                borderRadius: 8,
                textTransform: 'none',
                fontSize: '1rem',
                borderColor: 'rgba(255,255,255,0.3)',
                color: '#fff',
                '&:hover': { borderColor: '#1B4332', color: '#1B4332' },
                transition: 'all 0.3s',
              }}
            >
              Instagram
            </Button>
          </Stack>
          <Box sx={{ mt: 3 }}>
            <Typography
              component="a"
              href="mailto:info@domopululahua.com"
              sx={{ color: 'rgba(255,255,255,0.4)', fontSize: '0.875rem', textDecoration: 'none', '&:hover': { color: 'rgba(255,255,255,0.7)' } }}
            >
              info@domopululahua.com
            </Typography>
          </Box>
        </Box>
      </Box>

      {/* DOMO FOOTER */}
      <Box sx={{ bgcolor: '#0F0F0F', color: '#fff', py: 4, px: { xs: 2, md: 6 }, borderTop: '1px solid rgba(255,255,255,0.06)' }}>
        <Box sx={{ maxWidth: 1200, mx: 'auto', display: 'flex', flexDirection: { xs: 'column', sm: 'row' }, justifyContent: 'space-between', alignItems: 'center', gap: 2 }}>
          <Typography sx={{ color: 'rgba(255,255,255,0.3)', fontSize: '0.8125rem' }}>
            © 2025 Domo del Pululahua
          </Typography>
          <Typography sx={{ color: 'rgba(255,255,255,0.3)', fontSize: '0.8125rem' }}>
            Hecho con intención en el corazón del cráter
          </Typography>
        </Box>
      </Box>
    </Box>
  );
}
