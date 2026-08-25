import { useState } from 'react';
import { Alert, Box, Button, Card, CardActions, CardContent, Chip, Grid, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { useMetaTags } from '../hooks/useMetaTags';

type Locale = 'es' | 'en';
type Availability = 'checkout' | 'request' | 'pilot' | 'unavailable';

const copy = {
  es: {
    title: 'Servicios y experiencias TDF',
    intro: 'Un solo lugar para conocer qué puedes comprar hoy, qué requiere cotización y qué continúa en piloto.',
    notice: 'Cada flujo conserva su propio pedido y cumplimiento. Un pago verificado no significa entrega, sesión completada ni distribución en tiendas.',
    open: 'Abrir',
    statuses: { checkout: 'Checkout disponible', request: 'Solicitud', pilot: 'Piloto privado', unavailable: 'No disponible' },
  },
  en: {
    title: 'TDF services and experiences',
    intro: 'One place to see what you can buy today, what needs a quote, and what remains in pilot.',
    notice: 'Each flow keeps its own order and fulfillment lifecycle. Verified payment never means delivered work, a completed session, or a live DSP release.',
    open: 'Open',
    statuses: { checkout: 'Checkout available', request: 'Request only', pilot: 'Private pilot', unavailable: 'Unavailable' },
  },
} as const;

const offers: readonly {
  title: Record<Locale, string>;
  description: Record<Locale, string>;
  route?: string;
  availability: Availability;
}[] = [
  {
    title: { es: 'Mezcla y mastering', en: 'Mixing and mastering' },
    description: { es: 'Pedido con precio y cantidad validados por el servidor; seguimiento específico del servicio.', en: 'Server-validated package and quantity with service-specific tracking.' },
    route: '/mezcla-mastering',
    availability: 'checkout',
  },
  {
    title: { es: 'Equipos', en: 'Equipment' },
    description: { es: 'Catálogo público. Confirma las condiciones de venta o alquiler dentro de cada publicación.', en: 'Public catalog. Check sale or rental terms on each listing.' },
    route: '/marketplace',
    availability: 'checkout',
  },
  {
    title: { es: 'Estudio y producción', en: 'Studio and production' },
    description: { es: 'Solicitud de horario; todavía no retiene la sala ni confirma un depósito.', en: 'Schedule request; it does not yet hold a room or confirm a deposit.' },
    route: '/reservar',
    availability: 'request',
  },
  {
    title: { es: 'DJ booth', en: 'DJ booth' },
    description: { es: 'Solicitud pública para la cabina; confirmación y pago siguen pendientes.', en: 'Public booth request; confirmation and payment remain pending.' },
    route: '/dj-booth',
    availability: 'request',
  },
  {
    title: { es: 'Domo del Pululahua', en: 'Pululahua Dome' },
    description: { es: 'Solicitud de cotización sin precio client-side ni retención automática de fecha.', en: 'Quote request without client-side pricing or an automatic date hold.' },
    route: '/domo-del-pululahua',
    availability: 'request',
  },
  {
    title: { es: 'Cursos', en: 'Courses' },
    description: { es: 'Registro público; el cobro integrado y la retención de cupo siguen en preparación.', en: 'Public registration; integrated payment and seat holds are still being prepared.' },
    route: '/curso/produccion-musical-jun-2026',
    availability: 'request',
  },
  {
    title: { es: 'Distribución musical', en: 'Music distribution' },
    description: { es: 'Intake ERN 4.3.2 en piloto. Entrega real depende de un socio contratado y evidencia del destinatario.', en: 'ERN 4.3.2 intake pilot. Real delivery requires a contracted partner and recipient evidence.' },
    route: '/distribucion',
    availability: 'pilot',
  },
  {
    title: { es: 'Eventos y entradas', en: 'Events and tickets' },
    description: { es: 'La tienda pública y el checkout de invitado todavía no están habilitados.', en: 'The public storefront and guest checkout are not enabled yet.' },
    availability: 'unavailable',
  },
  {
    title: { es: 'Apoyo a artistas', en: 'Artist support' },
    description: { es: 'Donación Cardano disponible; un hash enviado por el usuario no se considera pago verificado.', en: 'Cardano donation option; a user-submitted hash is not treated as verified payment.' },
    route: '/donar',
    availability: 'request',
  },
];

const statusColor: Record<Availability, 'success' | 'info' | 'warning' | 'default'> = {
  checkout: 'success',
  request: 'info',
  pilot: 'warning',
  unavailable: 'default',
};

export default function CommerceHubPage() {
  const [locale, setLocale] = useState<Locale>('es');
  const text = copy[locale];
  useMetaTags({
    title: locale === 'es' ? 'Servicios TDF' : 'TDF Services',
    description: text.intro,
  });

  return (
    <Box component="section" aria-labelledby="commerce-hub-title" lang={locale} sx={{ maxWidth: 1200, mx: 'auto', py: { xs: 4, md: 7 }, px: 2 }}>
      <Stack spacing={4}>
        <Stack spacing={2} alignItems="flex-start">
          <Stack direction="row" spacing={1} aria-label="Language selector">
            <Button size="small" variant={locale === 'es' ? 'contained' : 'outlined'} onClick={() => setLocale('es')}>ES</Button>
            <Button size="small" variant={locale === 'en' ? 'contained' : 'outlined'} onClick={() => setLocale('en')}>EN</Button>
          </Stack>
          <Typography id="commerce-hub-title" component="h1" variant="h3" fontWeight={900}>{text.title}</Typography>
          <Typography variant="h6" color="text.secondary" sx={{ maxWidth: 800 }}>{text.intro}</Typography>
          <Alert severity="info" sx={{ maxWidth: 900 }}>{text.notice}</Alert>
        </Stack>

        <Grid container spacing={2.5} alignItems="stretch">
          {offers.map((offer) => (
            <Grid item xs={12} sm={6} lg={4} key={offer.title.es}>
              <Card variant="outlined" sx={{ height: '100%', display: 'flex', flexDirection: 'column', borderRadius: 3 }}>
                <CardContent sx={{ flex: 1 }}>
                  <Stack spacing={1.5} alignItems="flex-start">
                    <Chip size="small" color={statusColor[offer.availability]} label={text.statuses[offer.availability]} />
                    <Typography component="h2" variant="h6" fontWeight={800}>{offer.title[locale]}</Typography>
                    <Typography color="text.secondary">{offer.description[locale]}</Typography>
                  </Stack>
                </CardContent>
                <CardActions sx={{ px: 2, pb: 2 }}>
                  {offer.route ? (
                    <Button component={RouterLink} to={offer.route}>{text.open}</Button>
                  ) : (
                    <Button disabled>{text.statuses.unavailable}</Button>
                  )}
                </CardActions>
              </Card>
            </Grid>
          ))}
        </Grid>
      </Stack>
    </Box>
  );
}
