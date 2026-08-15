import {
  Alert,
  Box,
  Button,
  Chip,
  CircularProgress,
  Container,
  Divider,
  Paper,
  Stack,
  Typography,
} from '@mui/material';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import LoginIcon from '@mui/icons-material/Login';
import ShareIcon from '@mui/icons-material/Share';
import WhatsAppIcon from '@mui/icons-material/WhatsApp';
import { useQuery } from '@tanstack/react-query';
import { Link as RouterLink, useLocation, useParams } from 'react-router-dom';

import { Directory, type DirectoryEntityType } from '../api/directory';
import { useMetaTags } from '../hooks/useMetaTags';
import { useSession } from '../session/SessionContext';
import { buildLoginRedirectPath } from '../utils/loginRouting';

type DetailKind = Exclude<DirectoryEntityType, never>;

const text = (value: unknown): string | undefined => typeof value === 'string' ? value : undefined;
const record = (value: unknown): Record<string, unknown> | undefined =>
  typeof value === 'object' && value !== null && !Array.isArray(value) ? value as Record<string, unknown> : undefined;
const rows = (value: unknown): Record<string, unknown>[] =>
  Array.isArray(value) ? value.filter((item): item is Record<string, unknown> => Boolean(record(item))) : [];

export default function DirectoryPublicDetailPage({ kind }: { kind: DetailKind }) {
  const params = useParams();
  const location = useLocation();
  const { session } = useSession();
  const identifier = params['slug'] ?? params['eventId'] ?? params['venueId'] ?? '';
  const detail = useQuery({
    queryKey: ['directory', kind, identifier],
    queryFn: async (): Promise<Record<string, unknown>> => {
      if (kind === 'profile') return await Directory.profile(identifier) as unknown as Record<string, unknown>;
      if (kind === 'classified') return await Directory.classified(identifier) as unknown as Record<string, unknown>;
      if (kind === 'event') return await Directory.event(identifier) as unknown as Record<string, unknown>;
      return await Directory.venue(identifier) as unknown as Record<string, unknown>;
    },
    enabled: Boolean(identifier),
  });
  const value = detail.data ?? {};
  const title = text(value['name']) ?? text(value['title']) ?? 'Directorio musical';
  const description = text(value['bio']) ?? text(value['description']) ?? text(value['creditsSummary']) ?? 'Perfil público en TDF.';
  const canonicalPath = text(value['canonicalUrl']) ?? location.pathname;
  const canonical = `${window.location.origin}${canonicalPath}`;
  const categoryCode = text(record(value['category'])?.['code']);
  const profileKind = text(value['kind']);
  const schemaType = kind === 'event' ? 'MusicEvent'
    : kind === 'venue' ? 'MusicVenue'
      : kind === 'classified' && categoryCode === 'paid-work' ? 'JobPosting'
        : kind === 'classified' && ['offering-services', 'equipment-sale-rental', 'room-studio-available', 'classes'].includes(categoryCode ?? '') ? 'Offer'
          : kind === 'classified' ? 'CreativeWork'
            : ['organization', 'company', 'venue', 'studio', 'agency', 'label', 'distributor', 'school', 'band', 'project'].includes(profileKind ?? '') ? 'Organization'
              : 'Person';

  useMetaTags({
    title,
    description,
    canonical,
    ogType: kind === 'profile' ? 'profile' : 'website',
    structuredData: {
      '@context': 'https://schema.org',
      '@type': schemaType,
      name: title,
      description,
      url: canonical,
    },
  });

  const share = async () => {
    if (navigator.share) await navigator.share({ title, text: description, url: canonical });
    else await navigator.clipboard.writeText(canonical);
  };
  const whatsapp = `https://wa.me/?text=${encodeURIComponent(`${title} · ${canonical}`)}`;

  if (detail.isLoading) return <Stack minHeight="55vh" alignItems="center" justifyContent="center"><CircularProgress /></Stack>;
  if (detail.isError) return <Container maxWidth="md" sx={{ py: 8 }}><Alert severity="error">Este contenido no está publicado, vigente o disponible.</Alert></Container>;

  const locationValue = record(value['location']) ?? rows(value['locations'])[0];
  const professions = rows(value['professions']);
  const instruments = rows(value['instruments']);
  const genres = rows(value['genres']);
  const author = record(value['author']);
  const category = record(value['category']);
  const venue = record(value['venue']);
  const targetId = text(value['id']) ?? identifier;
  const authenticatedAction = kind === 'classified'
    ? `/mis-clasificados?apply=${encodeURIComponent(targetId)}`
    : `/mis-clasificados?contact=${encodeURIComponent(targetId)}&contextKind=profile`;

  return (
    <Box component="main" id="main-content" sx={{ py: { xs: 4, md: 7 } }}>
      <Container maxWidth="lg">
        <Stack spacing={3}>
          <Button component={RouterLink} to="/buscar" startIcon={<ArrowBackIcon />} sx={{ alignSelf: 'flex-start' }}>Volver a buscar</Button>
          <Paper variant="outlined" sx={{ p: { xs: 3, md: 5 }, borderRadius: 4 }}>
            <Stack spacing={3}>
              <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={3}>
                <Box>
                  <Stack direction="row" gap={1} flexWrap="wrap" mb={1}>
                    <Chip color="primary" label={kind === 'profile' ? 'Perfil profesional' : kind === 'classified' ? 'Clasificado musical' : kind === 'event' ? 'Evento' : 'Venue'} />
                    {category && <Chip label={text(category['name']) ?? 'Oportunidad'} />}
                  </Stack>
                  <Typography component="h1" variant="h2" fontWeight={900} sx={{ fontSize: { xs: '2.2rem', md: '3.7rem' } }}>{title}</Typography>
                  {author && <Typography variant="h6" color="text.secondary" mt={1}>Publicado por {text(author['name'])}</Typography>}
                  {venue && <Typography variant="h6" color="text.secondary" mt={1}>{text(venue['name'])}</Typography>}
                </Box>
                <Stack direction="row" gap={1} flexWrap="wrap" alignSelf={{ md: 'flex-start' }}>
                  <Button onClick={() => { void share(); }} startIcon={<ShareIcon />}>Compartir</Button>
                  <Button component="a" href={whatsapp} target="_blank" rel="noreferrer" startIcon={<WhatsAppIcon />}>WhatsApp</Button>
                </Stack>
              </Stack>

              {locationValue && (
                <Alert severity="info">
                  {text(locationValue['city']) ?? 'Ubicación amplia'}{text(locationValue['countryCode']) ? `, ${text(locationValue['countryCode'])}` : ''}. La ubicación pública es aproximada; no incluye direcciones residenciales.
                </Alert>
              )}
              <Divider />
              <Typography sx={{ whiteSpace: 'pre-wrap', fontSize: '1.08rem', lineHeight: 1.75 }}>{description}</Typography>

              {(professions.length > 0 || instruments.length > 0 || genres.length > 0) && (
                <Stack spacing={2}>
                  {professions.length > 0 && <TagSection title="Profesiones" values={professions.map((item) => text(item['name']) ?? text(item['code']) ?? '').filter(Boolean)} />}
                  {instruments.length > 0 && <TagSection title="Instrumentos" values={instruments.map((item) => text(item['name']) ?? text(item['code']) ?? '').filter(Boolean)} />}
                  {genres.length > 0 && <TagSection title="Géneros" values={genres.map((item) => text(item['name']) ?? text(item['code']) ?? '').filter(Boolean)} />}
                </Stack>
              )}

              <Paper sx={{ p: 3, bgcolor: 'action.hover', borderRadius: 3 }} elevation={0}>
                <Typography variant="h5" fontWeight={800}>{kind === 'classified' ? '¿Te interesa esta oportunidad?' : '¿Quieres contactar este perfil?'}</Typography>
                <Typography color="text.secondary" mt={1}>TDF mantiene tu correo y teléfono ocultos hasta que decidas compartirlos.</Typography>
                <Button
                  component={RouterLink}
                  to={session ? authenticatedAction : buildLoginRedirectPath(location.pathname)}
                  variant="contained"
                  startIcon={<LoginIcon />}
                  sx={{ mt: 2 }}
                >
                  {session ? 'Contactar desde uno de mis perfiles' : 'Ingresar para contactar'}
                </Button>
              </Paper>
            </Stack>
          </Paper>
        </Stack>
      </Container>
    </Box>
  );
}

function TagSection({ title, values }: { title: string; values: string[] }) {
  return <Box><Typography variant="subtitle2" gutterBottom>{title}</Typography><Stack direction="row" gap={1} flexWrap="wrap">{values.map((value) => <Chip key={value} label={value} />)}</Stack></Box>;
}
