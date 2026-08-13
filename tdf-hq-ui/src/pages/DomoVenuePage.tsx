import { useMemo, useState, type ReactNode } from 'react';
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
import { useMetaTags } from '../hooks/useMetaTags';

const DOMO_TIMEZONE = (import.meta.env as Record<string, string | undefined> | undefined)?.VITE_DOMO_TIMEZONE ?? 'UTC';

type EventType = 'wedding' | 'corporate' | 'retreat' | 'concert' | 'workshop' | 'photo';
type DomoExperienceKey = 'naturaleza' | 'eventos' | 'musica' | 'ceremonias';

interface EventTypeConfig {
  label: string;
  serviceType: string;
  minimumHours: number;
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

interface DomoExperience {
  navLabel: string;
  accentColor: string;
  accentTextColor: string;
  videoSrc: string;
  mobileVideoSrc?: string;
  title: string;
  subtitle: string;
  ctaLabel: string;
  bookingEventType: EventType;
  infoBandBackground: string;
  infoBandNumberColor: string;
  infoBandLabelColor: string;
  infoBandValueColor: string;
  infoBand: readonly { number: string; label: string; value: string }[];
  galleryTitle: string;
  gallerySubtitle: string;
  galleryImages: readonly { src: string; alt: string }[];
  amenities: readonly { icon: ReactNode; title: string; desc: string }[];
  contactTitle: string;
  contactSubtitle: string;
}

const DOMO_IMAGE_URL = `${PUBLIC_BASE}/assets/tdf-ui/domo-pululahua-hero-cozy.jpg`;
const MAX_QUOTE_GUESTS = 220;

const DOMO_EXPERIENCE_ORDER: readonly DomoExperienceKey[] = ['naturaleza', 'eventos', 'musica', 'ceremonias'];

const DOMO_EXPERIENCES: Record<DomoExperienceKey, DomoExperience> = {
  naturaleza: {
    navLabel: 'Naturaleza',
    accentColor: '#1B4332',
    accentTextColor: '#FFFFFF',
    videoSrc: `${PUBLIC_BASE}/videos/nature-hero.mp4`,
    mobileVideoSrc: `${PUBLIC_BASE}/videos/nature-hero-mobile.mp4`,
    title: 'El Cráter',
    subtitle: 'Un geodomo en el borde del Pululahua, a 2.800 metros sobre el nivel del mar y a pasos de la línea del Ecuador.',
    ctaLabel: 'Explorar',
    bookingEventType: 'photo',
    infoBandBackground: '#1B4332',
    infoBandNumberColor: 'rgba(255,255,255,0.3)',
    infoBandLabelColor: 'rgba(255,255,255,0.6)',
    infoBandValueColor: '#FFFFFF',
    infoBand: [
      { number: '01', label: 'Ubicación', value: 'Reserva Geobotánica Pululahua, Ecuador (0.027°N)' },
      { number: '02', label: 'Altitud', value: '2.800 metros sobre el nivel del mar' },
      { number: '03', label: 'Ecosistema', value: 'Bosque nublado andino, único en el mundo' },
      { number: '04', label: 'Clima', value: 'Niebla tropical de altura, entre 12 y 18 °C' },
    ],
    galleryTitle: 'El paisaje es parte de la experiencia',
    gallerySubtitle: 'El valle del cráter, el bosque nublado y la terraza se convierten en el escenario de cada encuentro.',
    galleryImages: [
      {
        src: `${PUBLIC_BASE}/assets/tdf-ui/domo-crater-landscape.jpg`,
        alt: 'Panorámica del cráter Pululahua con el valle verde bajo un cielo azul',
      },
      {
        src: `${PUBLIC_BASE}/assets/tdf-ui/domo-deck-crater-view.jpg`,
        alt: 'Terraza de madera con vista al valle del cráter Pululahua',
      },
    ],
    amenities: [
      { icon: <NaturePeopleIcon />, title: 'Bosque nublado', desc: 'Ecosistema andino de altura en el Pululahua' },
      { icon: <LandscapeIcon />, title: 'Cráter volcánico', desc: 'Una vista abierta hacia el valle habitado' },
      { icon: <SpaIcon />, title: 'Niebla tropical', desc: 'Clima fresco de montaña durante todo el año' },
      { icon: <CameraAltIcon />, title: 'Amanecer andino', desc: 'Luz natural sobre el cráter y la cordillera' },
      { icon: <NaturePeopleIcon />, title: 'Avistamiento', desc: 'Aves y vegetación nativas de la reserva' },
      { icon: <LandscapeIcon />, title: 'Senderos', desc: 'Rutas para recorrer el entorno del cráter' },
    ],
    contactTitle: 'Vive el cráter',
    contactSubtitle: 'Reserva tu experiencia en Domo del Pululahua.',
  },
  eventos: {
    navLabel: 'Eventos',
    accentColor: '#C9A227',
    accentTextColor: '#0F0F0F',
    videoSrc: `${PUBLIC_BASE}/videos/events-hero.mp4`,
    title: 'Celebraciones',
    subtitle: 'Cenas al borde del cráter. Reuniones donde la montaña es testigo de cada brindis.',
    ctaLabel: 'Planificar',
    bookingEventType: 'wedding',
    infoBandBackground: '#F3EBCF',
    infoBandNumberColor: 'rgba(132,99,0,0.28)',
    infoBandLabelColor: 'rgba(27,67,50,0.58)',
    infoBandValueColor: '#1B4332',
    infoBand: [
      { number: '01', label: 'Bodas', value: 'Ceremonia y recepción con vista al cráter' },
      { number: '02', label: 'Cumpleaños', value: 'Celebraciones íntimas bajo las estrellas' },
      { number: '03', label: 'Corporativos', value: 'Retiros de equipo en la montaña' },
      { number: '04', label: 'Capacidad', value: 'Hasta 120 personas, según montaje y permisos' },
    ],
    galleryTitle: 'Tu evento, elevado',
    gallerySubtitle: 'Cada detalle diseñado para que tu celebración sea inolvidable.',
    galleryImages: [
      {
        src: `${PUBLIC_BASE}/assets/tdf-ui/domo-evening-fog.png`,
        alt: 'Domo del Pululahua al atardecer, rodeado de niebla y luces cálidas',
      },
    ],
    amenities: [
      { icon: <LocalDiningIcon />, title: 'Catering', desc: 'Menú personalizado para tu evento' },
      { icon: <LightbulbIcon />, title: 'Iluminación', desc: 'Sistema de luces para ambientación y producción' },
      { icon: <VolumeUpIcon />, title: 'Sonido', desc: 'Equipo de audio para música y presentaciones' },
      { icon: <LocalParkingIcon />, title: 'Parqueo', desc: 'Estacionamiento sujeto al plan de cada evento' },
      { icon: <EventSeatIcon />, title: 'Capacidad', desc: 'Hasta 120 personas, según montaje y permisos' },
      { icon: <WifiIcon />, title: 'Conectividad', desc: 'Opciones de conexión coordinadas con el equipo' },
    ],
    contactTitle: 'Hagamos tu evento realidad',
    contactSubtitle: 'Contáctanos para planificar tu celebración en el cráter.',
  },
  musica: {
    navLabel: 'Música',
    accentColor: '#4F46E5',
    accentTextColor: '#FFFFFF',
    videoSrc: `${PUBLIC_BASE}/videos/music-hero.mp4`,
    mobileVideoSrc: `${PUBLIC_BASE}/videos/music-hero-mobile.mp4`,
    title: 'Ritmo en la Cima',
    subtitle: 'Donde el ritmo encuentra la montaña. Festivales que resuenan en el cráter.',
    ctaLabel: 'Planificar concierto',
    bookingEventType: 'concert',
    infoBandBackground: '#0F0F0F',
    infoBandNumberColor: 'rgba(79,70,229,0.46)',
    infoBandLabelColor: 'rgba(255,255,255,0.48)',
    infoBandValueColor: '#FFFFFF',
    infoBand: [
      { number: '01', label: 'SpaceTrip Fest', value: 'Festival electrónico en el cráter' },
      { number: '02', label: 'Conciertos', value: 'Propuestas nacionales e internacionales' },
      { number: '03', label: 'Acústica', value: 'Sesiones íntimas bajo el domo geodésico' },
      { number: '04', label: 'Producción', value: 'Sonido e iluminación coordinados para cada presentación' },
    ],
    galleryTitle: 'La montaña también tiene ritmo',
    gallerySubtitle: 'Conciertos, sesiones y festivales encuentran una atmósfera propia bajo el domo.',
    galleryImages: [
      {
        src: `${PUBLIC_BASE}/assets/tdf-ui/domo-night-stars.jpg`,
        alt: 'Domo del Pululahua iluminado bajo un cielo estrellado',
      },
      {
        src: `${PUBLIC_BASE}/assets/tdf-ui/domo-interior-people.jpg`,
        alt: 'Interior del domo preparado para recibir una producción',
      },
    ],
    amenities: [
      { icon: <VolumeUpIcon />, title: 'Sonido profesional', desc: 'Sistema adaptable a conciertos, sesiones y DJ sets' },
      { icon: <CalculateIcon />, title: 'Consola digital', desc: 'Mezcla coordinada según la ficha técnica' },
      { icon: <GroupsIcon />, title: 'Equipo de escenario', desc: 'Equipamiento coordinado para bandas y DJ' },
      { icon: <LightbulbIcon />, title: 'Luces LED', desc: 'Diseño de iluminación para cada formato' },
      { icon: <AccessTimeIcon />, title: 'Horarios', desc: 'Agenda definida según permisos y operación' },
      { icon: <LandscapeIcon />, title: 'Entorno', desc: 'Una puesta en escena conectada con el cráter' },
    ],
    contactTitle: 'El escenario te espera',
    contactSubtitle: 'Reserva el domo para tu próximo concierto, sesión o festival.',
  },
  ceremonias: {
    navLabel: 'Ceremonias',
    accentColor: '#C8B6FF',
    accentTextColor: '#0F0F0F',
    videoSrc: `${PUBLIC_BASE}/videos/ceremonies-hero.mp4`,
    title: 'Santuario',
    subtitle: 'Retiros de meditación, sanación sonora y yoga en el vientre del volcán.',
    ctaLabel: 'Reservar retiro',
    bookingEventType: 'retreat',
    infoBandBackground: '#F2EEFF',
    infoBandNumberColor: 'rgba(95,72,160,0.24)',
    infoBandLabelColor: 'rgba(15,15,15,0.48)',
    infoBandValueColor: '#0F0F0F',
    infoBand: [
      { number: '01', label: 'Meditación', value: 'Retiros guiados de atención plena' },
      { number: '02', label: 'Sanación', value: 'Prácticas sonoras con cuencos y gongs' },
      { number: '03', label: 'Yoga', value: 'Prácticas con vista al cráter' },
      { number: '04', label: 'Ceremonias', value: 'Encuentros íntimos en conexión con la montaña' },
    ],
    galleryTitle: 'Un espacio para regresar a ti',
    gallerySubtitle: 'Cada práctica sostenida por la energía del volcán.',
    galleryImages: [
      {
        src: `${PUBLIC_BASE}/assets/tdf-ui/domo-entrance-lanterns.png`,
        alt: 'Entrada del Domo del Pululahua al atardecer con faroles y vegetación',
      },
    ],
    amenities: [
      { icon: <SpaIcon />, title: 'Yoga', desc: 'Espacio interior para prácticas en grupo' },
      { icon: <VolumeUpIcon />, title: 'Sanación sonora', desc: 'Ambiente para cuencos, gongs y escucha profunda' },
      { icon: <NaturePeopleIcon />, title: 'Jardín', desc: 'Áreas exteriores para contemplación' },
      { icon: <LandscapeIcon />, title: 'Cráter', desc: 'Vista abierta para prácticas al amanecer' },
      { icon: <GroupsIcon />, title: 'Retiros', desc: 'Formatos íntimos coordinados con el equipo' },
      { icon: <LocalDiningIcon />, title: 'Alimentación', desc: 'Menús conscientes disponibles bajo coordinación' },
    ],
    contactTitle: 'Regresa a tu centro',
    contactSubtitle: 'Reserva tu retiro de meditación, yoga o sanación sonora.',
  },
};

const EVENT_TYPES: Record<EventType, EventTypeConfig> = {
  wedding: {
    label: 'Boda',
    serviceType: 'Domo del Pululahua - boda',
    minimumHours: 8,
  },
  corporate: {
    label: 'Evento corporativo',
    serviceType: 'Domo del Pululahua - evento corporativo',
    minimumHours: 6,
  },
  retreat: {
    label: 'Retiro o taller',
    serviceType: 'Domo del Pululahua - retiro',
    minimumHours: 6,
  },
  concert: {
    label: 'Concierto',
    serviceType: 'Domo del Pululahua - concierto',
    minimumHours: 7,
  },
  workshop: {
    label: 'Taller',
    serviceType: 'Domo del Pululahua - taller',
    minimumHours: 4,
  },
  photo: {
    label: 'Sesión fotográfica',
    serviceType: 'Domo del Pululahua - sesion fotografica',
    minimumHours: 3,
  },
};

const initialStart = () =>
  DateTime.now()
    .setZone(DOMO_TIMEZONE)
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

const clampNumber = (value: number, min: number, max: number) => {
  if (!Number.isFinite(value)) return min;
  return Math.min(max, Math.max(min, Math.round(value)));
};

const summarizeRequest = (form: BookingFormState) => {
  const config = EVENT_TYPES[form.eventType];
  const guests = clampNumber(form.guests, 1, MAX_QUOTE_GUESTS);
  const billableHours = Math.max(config.minimumHours, clampNumber(form.durationHours, 1, 24));
  const setupHours = clampNumber(form.setupHours, 0, 12);
  const selectedAddons = [
    form.catering ? 'Catering y barra' : null,
    form.production ? 'Sonido e iluminación' : null,
    form.transport ? 'Transporte desde Quito' : null,
  ].filter((value): value is string => Boolean(value));

  return { billableHours, guests, setupHours, selectedAddons };
};

const toBookingIso = (value: string) => {
  const parsed = DateTime.fromFormat(value, "yyyy-LL-dd'T'HH:mm", { zone: DOMO_TIMEZONE });
  if (!parsed.isValid) return null;
  return parsed.toUTC().toISO({ suppressMilliseconds: true });
};

const buildBookingNotes = (form: BookingFormState, summary: ReturnType<typeof summarizeRequest>) => {
  return [
    'Solicitud pública Domo del Pululahua',
    `Tipo: ${EVENT_TYPES[form.eventType].label}`,
    `Invitados: ${summary.guests}`,
    `Duración solicitada: ${summary.billableHours} horas + ${summary.setupHours} horas de montaje`,
    'Precio: pendiente de cotización autoritativa y versionada',
    'Disponibilidad: no verificada; esta solicitud no retiene la fecha',
    summary.selectedAddons.length ? `Adicionales: ${summary.selectedAddons.join(', ')}` : 'Adicionales: ninguno',
    form.notes.trim() ? `Notas del cliente: ${form.notes.trim()}` : null,
  ]
    .filter(Boolean)
    .join('\n');
};

export default function DomoVenuePage() {
  useMetaTags({
    title: 'Domo del Pululahua',
    description: 'Solicita una cotización para eventos, música y experiencias en el Domo del Pululahua.',
  });
  const [activeExperienceKey, setActiveExperienceKey] = useState<DomoExperienceKey>('naturaleza');
  const [form, setForm] = useState<BookingFormState>(initialForm);
  const [submitting, setSubmitting] = useState(false);
  const [status, setStatus] = useState<{ severity: 'success' | 'error'; message: string } | null>(null);
  const activeExperience = DOMO_EXPERIENCES[activeExperienceKey];
  const requestSummary = useMemo(() => summarizeRequest(form), [form]);
  const bookingIso = toBookingIso(form.startsAt);

  const updateForm = <Key extends keyof BookingFormState>(key: Key, value: BookingFormState[Key]) => {
    setForm((prev) => ({ ...prev, [key]: value }));
  };

  const prepareExperienceBooking = () => {
    updateForm('eventType', activeExperience.bookingEventType);
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
        pbDurationMinutes: requestSummary.billableHours * 60,
        pbNotes: buildBookingNotes(form, requestSummary),
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
        overflowX: 'hidden',
      }}
    >
      {/* HERO */}
      <Box
        sx={{
          minHeight: { xs: '100vh', md: '100vh' },
          display: 'flex',
          alignItems: 'flex-end',
          color: '#fff',
          px: { xs: 2, md: 6 },
          py: { xs: 8, md: 10 },
          position: 'relative',
          overflow: 'hidden',
        }}
      >
        <Box
          component="nav"
          aria-label="Experiencias del Domo"
          sx={{
            position: 'absolute',
            top: { xs: 18, md: 28 },
            left: 0,
            right: 0,
            zIndex: 3,
            px: 2,
            display: 'flex',
            justifyContent: 'center',
          }}
        >
          <Stack
            direction="row"
            spacing={0.5}
            sx={{
              maxWidth: '100%',
              p: 0.75,
              overflowX: 'auto',
              border: '1px solid rgba(255,255,255,0.18)',
              borderRadius: 99,
              bgcolor: 'rgba(8,12,10,0.46)',
              boxShadow: '0 8px 32px rgba(0,0,0,0.18)',
              backdropFilter: 'blur(18px) saturate(140%)',
            }}
          >
            {DOMO_EXPERIENCE_ORDER.map((experienceKey) => {
              const experience = DOMO_EXPERIENCES[experienceKey];
              const isActive = experienceKey === activeExperienceKey;
              return (
                <Button
                  key={experienceKey}
                  type="button"
                  aria-pressed={isActive}
                  onClick={() => setActiveExperienceKey(experienceKey)}
                  sx={{
                    flex: '0 0 auto',
                    minWidth: 0,
                    px: { xs: 1.5, sm: 2.25 },
                    py: 1,
                    borderRadius: 99,
                    textTransform: 'none',
                    fontSize: { xs: '0.76rem', sm: '0.875rem' },
                    fontWeight: isActive ? 800 : 600,
                    color: isActive ? experience.accentTextColor : 'rgba(255,255,255,0.72)',
                    bgcolor: isActive ? experience.accentColor : 'transparent',
                    '&:hover': {
                      bgcolor: isActive ? experience.accentColor : 'rgba(255,255,255,0.1)',
                    },
                  }}
                >
                  {experience.navLabel}
                </Button>
              );
            })}
          </Stack>
        </Box>

        {/* Video background */}
        <Box
          key={activeExperienceKey}
          component="video"
          autoPlay
          muted
          loop
          playsInline
          preload="auto"
          poster={DOMO_IMAGE_URL}
          sx={{
            position: 'absolute',
            inset: 0,
            width: '100%',
            height: '100%',
            objectFit: 'cover',
            zIndex: 0,
          }}
        >
          {activeExperience.mobileVideoSrc && (
            <source src={activeExperience.mobileVideoSrc} type="video/mp4" media="(max-width: 767px)" />
          )}
          <source src={activeExperience.videoSrc} type="video/mp4" />
        </Box>

        {/* Top gradient shield for nav readability */}
        <Box
          sx={{
            position: 'absolute',
            top: 0,
            left: 0,
            right: 0,
            height: '25%',
            background: 'linear-gradient(to bottom, rgba(0,0,0,0.35) 0%, transparent 100%)',
            zIndex: 1,
            pointerEvents: 'none',
          }}
        />

        {/* Bottom gradient shield for text readability */}
        <Box
          sx={{
            position: 'absolute',
            bottom: 0,
            left: 0,
            right: 0,
            height: '60%',
            background: 'linear-gradient(to top, rgba(4,8,12,0.92) 0%, rgba(4,8,12,0.5) 50%, transparent 100%)',
            zIndex: 1,
            pointerEvents: 'none',
          }}
        />
        <Stack spacing={3} sx={{ maxWidth: 820, position: 'relative', zIndex: 2 }}>
          <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
            <Chip icon={<LandscapeIcon />} label="Domo del Pululahua" sx={{ bgcolor: 'rgba(255,255,255,0.16)', color: '#fff', backdropFilter: 'blur(8px)' }} />
            <Chip label={activeExperience.navLabel} sx={{ bgcolor: activeExperience.accentColor, color: activeExperience.accentTextColor, fontWeight: 700 }} />
            <Chip label="2.800 m s. n. m." sx={{ bgcolor: 'rgba(255,255,255,0.16)', color: '#fff', backdropFilter: 'blur(8px)' }} />
          </Stack>
          <Typography component="h1" variant="h2" sx={{ fontWeight: 900, maxWidth: 720, fontSize: { xs: '2.5rem', md: '3.75rem' }, lineHeight: 1.1, textShadow: '0 2px 40px rgba(0,0,0,0.4)' }}>
            {activeExperience.title}
          </Typography>
          <Typography variant="h5" sx={{ maxWidth: 760, color: 'rgba(255,255,255,0.88)', fontSize: { xs: '1.125rem', md: '1.5rem' }, lineHeight: 1.5, textShadow: '0 2px 20px rgba(0,0,0,0.3)' }}>
            {activeExperience.subtitle}
          </Typography>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            <Button
              variant="contained"
              size="large"
              startIcon={activeExperienceKey === 'naturaleza' ? <LandscapeIcon /> : <EventAvailableIcon />}
              href={activeExperienceKey === 'naturaleza' ? '#experiencia' : '#reservar'}
              onClick={activeExperienceKey === 'naturaleza' ? undefined : prepareExperienceBooking}
              sx={{
                alignSelf: { xs: 'stretch', sm: 'flex-start' },
                textTransform: 'none',
                px: 4,
                py: 1.5,
                fontSize: '1rem',
                bgcolor: activeExperience.accentColor,
                color: activeExperience.accentTextColor,
                '&:hover': { bgcolor: activeExperience.accentColor, filter: 'brightness(1.08)' },
              }}
            >
              {activeExperience.ctaLabel}
            </Button>
            <Button
              variant="outlined"
              size="large"
              startIcon={<RequestQuoteIcon />}
              href="#cotizar"
              onClick={prepareExperienceBooking}
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
              Cotizar experiencia
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
      <Box sx={{ bgcolor: activeExperience.infoBandBackground, py: 5, px: { xs: 2, md: 6 }, transition: 'background-color 0.35s ease' }}>
        <Grid container spacing={4} sx={{ maxWidth: 1200, mx: 'auto' }}>
          {activeExperience.infoBand.map((item) => (
            <Grid item xs={6} md={3} key={item.number}>
              <Typography variant="h3" sx={{ fontWeight: 300, fontSize: '2.5rem', color: activeExperience.infoBandNumberColor, fontFamily: 'Geist, sans-serif' }}>
                {item.number}
              </Typography>
              <Typography sx={{ mt: 1, fontSize: '0.75rem', textTransform: 'uppercase', letterSpacing: 1.5, color: activeExperience.infoBandLabelColor }}>
                {item.label}
              </Typography>
              <Typography sx={{ mt: 0.5, fontSize: '1rem', color: activeExperience.infoBandValueColor }}>
                {item.value}
              </Typography>
            </Grid>
          ))}
        </Grid>
      </Box>

      {/* EXPERIENCE SELECTOR */}
      <Box id="experiencia" sx={{ px: { xs: 2, md: 6 }, py: { xs: 5, md: 7 }, bgcolor: '#f7f4ed' }}>
        <Typography variant="overline" sx={{ color: 'rgba(27,67,50,0.6)', letterSpacing: 2, display: 'block', textAlign: 'center', mb: 1 }}>
          Experiencias
        </Typography>
        <Typography variant="h3" sx={{ fontWeight: 900, textAlign: 'center', mb: 5, color: '#1a1a1a', fontSize: { xs: '1.75rem', md: '2.5rem' } }}>
          Cuatro formas de vivir el Domo
        </Typography>
        <Grid container spacing={3} alignItems="stretch">
          {DOMO_EXPERIENCE_ORDER.map((experienceKey) => {
            const experience = DOMO_EXPERIENCES[experienceKey];
            const isActive = experienceKey === activeExperienceKey;
            return (
            <Grid item xs={12} sm={6} lg={3} key={experienceKey}>
              <Card
                component="button"
                type="button"
                aria-pressed={isActive}
                onClick={() => setActiveExperienceKey(experienceKey)}
                sx={{
                  width: '100%',
                  height: '100%',
                  borderRadius: 2,
                  boxShadow: isActive ? `0 12px 28px ${experience.accentColor}24` : 'none',
                  border: `1px solid ${isActive ? experience.accentColor : 'rgba(44,35,24,0.12)'}`,
                  borderTop: `4px solid ${experience.accentColor}`,
                  bgcolor: '#fff',
                  color: 'inherit',
                  cursor: 'pointer',
                  textAlign: 'left',
                  transition: 'transform 0.2s ease, box-shadow 0.2s ease',
                  '&:hover': { transform: 'translateY(-3px)', boxShadow: `0 12px 28px ${experience.accentColor}24` },
                  '&:focus-visible': { outline: `3px solid ${experience.accentColor}`, outlineOffset: 3 },
                }}
              >
                <CardContent sx={{ p: 3 }}>
                  <Typography variant="overline" sx={{ color: experience.accentColor, fontWeight: 800, letterSpacing: 1.5 }}>
                    {experience.navLabel}
                  </Typography>
                  <Typography variant="h6" fontWeight={800} sx={{ mt: 0.5, color: '#1a1a1a' }}>{experience.title}</Typography>
                  <Typography variant="body2" color="text.secondary" sx={{ mt: 1.5, lineHeight: 1.6 }}>{experience.subtitle}</Typography>
                </CardContent>
              </Card>
            </Grid>
            );
          })}
        </Grid>
      </Box>

      {/* EXPERIENCE GALLERY */}
      <Box sx={{ px: { xs: 2, md: 6 }, py: { xs: 5, md: 7 }, bgcolor: '#10151d', color: '#fff' }}>
        <Grid container spacing={3} alignItems="stretch">
          <Grid item xs={12} md={4}>
            <Stack spacing={2} sx={{ maxWidth: 440, height: '100%', justifyContent: 'center' }}>
              <Typography variant="overline" sx={{ color: activeExperience.accentColor, letterSpacing: 1.5, fontWeight: 800 }}>
                {activeExperience.navLabel}
              </Typography>
              <Typography variant="h4" fontWeight={900} sx={{ fontSize: { xs: '1.75rem', md: '2.25rem' } }}>
                {activeExperience.galleryTitle}
              </Typography>
              <Typography sx={{ color: 'rgba(255,255,255,0.78)', lineHeight: 1.7 }}>
                {activeExperience.gallerySubtitle}
              </Typography>
            </Stack>
          </Grid>
          <Grid item xs={12} md={8}>
            <Grid container spacing={2} sx={{ height: '100%' }} key={activeExperienceKey}>
              {activeExperience.galleryImages.map((image) => (
                <Grid item xs={12} sm={activeExperience.galleryImages.length > 1 ? 6 : 12} key={image.src}>
                <Box
                  component="img"
                  src={image.src}
                  alt={image.alt}
                  loading="lazy"
                  sx={{
                    width: '100%',
                    height: '100%',
                    minHeight: { xs: 360, md: 520 },
                    aspectRatio: activeExperience.galleryImages.length > 1 ? '4 / 5' : '16 / 9',
                    objectFit: 'cover',
                    borderRadius: 2,
                    display: 'block',
                  }}
                />
                </Grid>
              ))}
            </Grid>
          </Grid>
        </Grid>
      </Box>

      {/* AMENITIES */}
      <Box sx={{ bgcolor: '#0F0F0F', color: '#fff', py: { xs: 6, md: 8 }, px: { xs: 2, md: 6 } }}>
        <Box sx={{ maxWidth: 1200, mx: 'auto' }}>
          <Box sx={{ textAlign: 'center', mb: 5 }}>
            <Typography variant="overline" sx={{ color: activeExperience.accentColor, letterSpacing: 2, display: 'block' }}>
              Lo que ofrecemos
            </Typography>
            <Typography variant="h3" sx={{ fontWeight: 300, mt: 1, fontSize: { xs: '1.75rem', md: '2.5rem' }, color: '#fff' }}>
              {activeExperience.navLabel} en el Domo
            </Typography>
          </Box>
          <Grid container spacing={2}>
            {activeExperience.amenities.map((amenity) => (
              <Grid item xs={6} md={4} key={amenity.title}>
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
                      borderColor: `${activeExperience.accentColor}66`,
                    },
                  }}
                >
                  <Box sx={{ color: activeExperience.accentColor, mb: 1.5 }}>{amenity.icon}</Box>
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
                      Solicitud de cotización
                    </Typography>
                    <Typography color="text.secondary">
                      Cuéntanos el plan. Enviar esta solicitud no confirma disponibilidad, no retiene la fecha y no crea un pago.
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
                        type="tel"
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
                      Resumen de solicitud
                    </Typography>
                  </Stack>
                  <Stack spacing={1.25}>
                    <Stack direction="row" justifyContent="space-between" spacing={2}>
                      <Typography variant="body2" color="text.secondary">Experiencia</Typography>
                      <Typography variant="body2" fontWeight={700}>{EVENT_TYPES[form.eventType].label}</Typography>
                    </Stack>
                    <Stack direction="row" justifyContent="space-between" spacing={2}>
                      <Typography variant="body2" color="text.secondary">Invitados</Typography>
                      <Typography variant="body2" fontWeight={700}>{requestSummary.guests}</Typography>
                    </Stack>
                    <Stack direction="row" justifyContent="space-between" spacing={2}>
                      <Typography variant="body2" color="text.secondary">Evento y montaje</Typography>
                      <Typography variant="body2" fontWeight={700}>{requestSummary.billableHours} h + {requestSummary.setupHours} h</Typography>
                    </Stack>
                    <Stack direction="row" justifyContent="space-between" spacing={2}>
                      <Typography variant="body2" color="text.secondary">Adicionales</Typography>
                      <Typography variant="body2" fontWeight={700} textAlign="right">
                        {requestSummary.selectedAddons.length ? requestSummary.selectedAddons.join(', ') : 'Ninguno'}
                      </Typography>
                    </Stack>
                  </Stack>
                  <Divider />
                  <Alert severity="warning">
                    El precio, impuestos, depósito y políticas vendrán en una cotización versionada emitida por TDF. Solo una cotización aprobada y un pago verificado podrán separar la fecha.
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
            <Typography variant="overline" sx={{ color: activeExperience.accentColor, letterSpacing: 2, display: 'block' }}>
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
                      <Box component="span" sx={{ color: activeExperience.accentColor }}>A pasos de la línea del Ecuador</Box>
                    </Typography>
                  </CardContent>
                </Card>
                <Card sx={{ bgcolor: 'rgba(255,255,255,0.04)', border: '1px solid rgba(255,255,255,0.08)', borderRadius: 2, color: '#fff' }}>
                  <CardContent>
                    <Typography variant="overline" sx={{ color: 'rgba(255,255,255,0.4)', letterSpacing: 1, display: 'block', mb: 0.5 }}>
                      Desde Quito
                    </Typography>
                    <Typography sx={{ color: 'rgba(255,255,255,0.8)', fontSize: '0.9375rem' }}>
                      Aproximadamente 30 minutos por la calle Pululahua<br />
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
                    bgcolor: activeExperience.accentColor,
                    color: activeExperience.accentTextColor,
                    '&:hover': { bgcolor: activeExperience.accentColor, filter: 'brightness(1.08)' },
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
          background: `linear-gradient(180deg, ${activeExperience.accentColor}1F 0%, ${activeExperience.accentColor}0A 100%)`,
        }}
      >
        <Box sx={{ maxWidth: 700, mx: 'auto' }}>
          <Typography variant="h3" sx={{ fontWeight: 300, color: '#fff', fontSize: { xs: '1.75rem', md: '2.75rem' }, lineHeight: 1.1 }}>
            {activeExperience.contactTitle}
          </Typography>
          <Typography sx={{ mt: 2, color: 'rgba(255,255,255,0.6)', fontSize: '1.0625rem' }}>
            {activeExperience.contactSubtitle}
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
                bgcolor: activeExperience.accentColor,
                color: activeExperience.accentTextColor,
                fontWeight: 600,
                '&:hover': { bgcolor: activeExperience.accentColor, filter: 'brightness(1.08)', transform: 'scale(1.02)' },
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
                '&:hover': { borderColor: activeExperience.accentColor, color: activeExperience.accentColor },
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
            © {new Date().getFullYear()} Domo del Pululahua
          </Typography>
          <Typography sx={{ color: 'rgba(255,255,255,0.3)', fontSize: '0.8125rem' }}>
            Hecho con intención en el corazón del cráter
          </Typography>
        </Box>
      </Box>
    </Box>
  );
}
