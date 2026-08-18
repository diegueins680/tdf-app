import { useMemo, useState } from 'react';
import { Alert, Box, Button, Card, CardContent, Checkbox, FormControlLabel, Grid, MenuItem, Stack, TextField, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { useMetaTags } from '../hooks/useMetaTags';

type Locale = 'es' | 'en';
type ReleaseType = 'single' | 'ep' | 'album';

const content = {
  es: {
    title: 'Distribución musical con evidencia, no promesas',
    subtitle: 'Prepara metadatos, activos y derechos para un piloto TDF. La entrega de producción permanece deshabilitada hasta contratar y verificar un destinatario.',
    gate: 'No hay una tarifa pública aprobada ni checkout de distribución activo. TDF no cobrará ni consumirá una entrega hasta que el release pase validación y revisión de derechos.',
    calculator: 'Calculadora de release',
    calculatorResult: 'Total no calculado: falta una versión de precios aprobada. Tu selección sirve para preparar la solicitud, no constituye una cotización.',
    type: 'Formato',
    tracks: 'Número de tracks',
    rush: 'Revisión prioritaria (sujeta a capacidad)',
    start: 'Ingresar al piloto',
    catalog: 'Ver catálogo público',
    steps: ['Carga privada y validación técnica', 'Declaraciones de derechos y splits aceptados al 100%', 'Revisión TDF antes de cualquier cobro', 'Paquete y entrega solo con perfil de socio verificado'],
    evidence: 'Estados separados: generado, enviado, reconocido, aceptado y live. Un archivo XML o una respuesta simulada nunca significa distribución completada.',
  },
  en: {
    title: 'Music distribution backed by evidence, not promises',
    subtitle: 'Prepare metadata, assets, and rights for a TDF pilot. Production delivery stays disabled until a recipient is contracted and verified.',
    gate: 'There is no approved public rate or active distribution checkout. TDF will not charge or consume a delivery before validation and rights review pass.',
    calculator: 'Release calculator',
    calculatorResult: 'No total calculated: an approved pricing version is missing. Your selection prepares a request and is not a quote.',
    type: 'Format',
    tracks: 'Track count',
    rush: 'Priority review (capacity permitting)',
    start: 'Enter the pilot',
    catalog: 'View public catalog',
    steps: ['Private upload and technical validation', 'Rights declarations and accepted 100% splits', 'TDF review before any charge', 'Package and delivery only through a verified partner profile'],
    evidence: 'Generated, sent, acknowledged, accepted, and live are separate states. XML or a simulated response never means completed distribution.',
  },
} as const;

const releaseLabels: Record<Locale, Record<ReleaseType, string>> = {
  es: { single: 'Single', ep: 'EP', album: 'Álbum' },
  en: { single: 'Single', ep: 'EP', album: 'Album' },
};

export default function DistributionLandingPage() {
  const [locale, setLocale] = useState<Locale>('es');
  const [releaseType, setReleaseType] = useState<ReleaseType>('single');
  const [trackCount, setTrackCount] = useState(1);
  const [rush, setRush] = useState(false);
  const text = content[locale];
  const selection = useMemo(
    () => `${releaseLabels[locale][releaseType]} · ${trackCount} ${trackCount === 1 ? 'track' : 'tracks'}${rush ? ' · priority review' : ''}`,
    [locale, releaseType, rush, trackCount],
  );

  useMetaTags({
    title: locale === 'es' ? 'Distribución musical' : 'Music Distribution',
    description: text.subtitle,
  });

  return (
    <Box component="section" aria-labelledby="distribution-title" lang={locale} sx={{ maxWidth: 1120, mx: 'auto', py: { xs: 4, md: 8 }, px: 2 }}>
      <Stack spacing={5}>
        <Stack spacing={2} alignItems="flex-start">
          <Stack direction="row" spacing={1} aria-label="Language selector">
            <Button size="small" variant={locale === 'es' ? 'contained' : 'outlined'} onClick={() => setLocale('es')}>ES</Button>
            <Button size="small" variant={locale === 'en' ? 'contained' : 'outlined'} onClick={() => setLocale('en')}>EN</Button>
          </Stack>
          <Typography id="distribution-title" component="h1" variant="h2" fontWeight={900} sx={{ maxWidth: 900 }}>{text.title}</Typography>
          <Typography variant="h6" color="text.secondary" sx={{ maxWidth: 850 }}>{text.subtitle}</Typography>
          <Alert severity="warning" sx={{ maxWidth: 900 }}>{text.gate}</Alert>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
            <Button component={RouterLink} to="/login?next=/label/ddex" variant="contained" size="large">{text.start}</Button>
            <Button component={RouterLink} to="/records" variant="outlined" size="large">{text.catalog}</Button>
          </Stack>
        </Stack>

        <Grid container spacing={3}>
          <Grid item xs={12} md={7}>
            <Card variant="outlined" sx={{ height: '100%', borderRadius: 3 }}>
              <CardContent sx={{ p: { xs: 2.5, md: 4 } }}>
                <Stack spacing={2.5}>
                  <Typography component="h2" variant="h4" fontWeight={850}>{text.calculator}</Typography>
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
                    <TextField select label={text.type} value={releaseType} onChange={(event) => setReleaseType(event.target.value as ReleaseType)} fullWidth>
                      {(Object.keys(releaseLabels[locale]) as ReleaseType[]).map((value) => (
                        <MenuItem key={value} value={value}>{releaseLabels[locale][value]}</MenuItem>
                      ))}
                    </TextField>
                    <TextField
                      label={text.tracks}
                      type="number"
                      value={trackCount}
                      onChange={(event) => setTrackCount(Math.min(100, Math.max(1, Math.round(Number(event.target.value) || 1))))}
                      inputProps={{ min: 1, max: 100 }}
                      fullWidth
                    />
                  </Stack>
                  <FormControlLabel control={<Checkbox checked={rush} onChange={(event) => setRush(event.target.checked)} />} label={text.rush} />
                  <Typography fontWeight={800}>{selection}</Typography>
                  <Alert severity="info">{text.calculatorResult}</Alert>
                </Stack>
              </CardContent>
            </Card>
          </Grid>
          <Grid item xs={12} md={5}>
            <Card variant="outlined" sx={{ height: '100%', borderRadius: 3 }}>
              <CardContent sx={{ p: { xs: 2.5, md: 4 } }}>
                <Stack component="ol" spacing={2} sx={{ pl: 2.5, m: 0 }}>
                  {text.steps.map((step) => (
                    <Typography component="li" key={step}>{step}</Typography>
                  ))}
                </Stack>
              </CardContent>
            </Card>
          </Grid>
        </Grid>

        <Alert severity="info">{text.evidence}</Alert>
      </Stack>
    </Box>
  );
}
