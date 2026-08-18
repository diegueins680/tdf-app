import {
  Alert,
  Box,
  Button,
  Chip,
  CircularProgress,
  Container,
  Divider,
  FormControl,
  InputLabel,
  MenuItem,
  Paper,
  Rating,
  Select,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import LoginIcon from '@mui/icons-material/Login';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import ShareIcon from '@mui/icons-material/Share';
import WhatsAppIcon from '@mui/icons-material/WhatsApp';
import { useInfiniteQuery, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState } from 'react';
import { Link as RouterLink, useLocation, useParams } from 'react-router-dom';

import { Directory, type DirectoryEntityType, type DirectoryReviewEligibility, type DirectoryReviewPage } from '../api/directory';
import { useMetaTags } from '../hooks/useMetaTags';
import { useSession } from '../session/SessionContext';
import { buildLoginRedirectPath } from '../utils/loginRouting';

type DetailKind = Exclude<DirectoryEntityType, never>;

const text = (value: unknown): string | undefined => typeof value === 'string' ? value : undefined;
const number = (value: unknown): number | undefined => typeof value === 'number' ? value : undefined;
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
  const reputation = record(value['reputation']);
  const reviewAverage = number(reputation?.['reviewAverage']);
  const reviewCount = number(reputation?.['reviewCount']) ?? 0;
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
      ...(reviewAverage && reviewCount > 0 ? { aggregateRating: { '@type': 'AggregateRating', ratingValue: reviewAverage, reviewCount } } : {}),
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
  const invitationAction = `/mis-clasificados?invite=${encodeURIComponent(targetId)}`;

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

              {kind === 'event' && (
                <Paper sx={{ p: 3, bgcolor: 'action.hover', borderRadius: 3 }} elevation={0}>
                  <Typography variant="h5" fontWeight={800}>Entradas del evento</Typography>
                  <Typography color="text.secondary" mt={1}>Consulta disponibilidad y paga como invitado con un checkout verificado por el servidor.</Typography>
                  <Button
                    component={RouterLink}
                    to={`/eventos/${encodeURIComponent(identifier)}/entradas`}
                    variant="contained"
                    sx={{ mt: 2 }}
                  >
                    Ver entradas
                  </Button>
                </Paper>
              )}

              {(professions.length > 0 || instruments.length > 0 || genres.length > 0) && (
                <Stack spacing={2}>
                  {professions.length > 0 && <TagSection title="Profesiones" values={professions.map((item) => text(item['name']) ?? text(item['code']) ?? '').filter(Boolean)} />}
                  {instruments.length > 0 && <TagSection title="Instrumentos" values={instruments.map((item) => text(item['name']) ?? text(item['code']) ?? '').filter(Boolean)} />}
                  {genres.length > 0 && <TagSection title="Géneros" values={genres.map((item) => text(item['name']) ?? text(item['code']) ?? '').filter(Boolean)} />}
                </Stack>
              )}

              {kind === 'profile' && <ProfileReviews slug={identifier} profileId={targetId} authenticated={Boolean(session)} />}

              <Paper sx={{ p: 3, bgcolor: 'action.hover', borderRadius: 3 }} elevation={0}>
                <Typography variant="h5" fontWeight={800}>{kind === 'classified' ? '¿Te interesa esta oportunidad?' : '¿Quieres contactar este perfil?'}</Typography>
                <Typography color="text.secondary" mt={1}>TDF mantiene tu correo y teléfono ocultos hasta que decidas compartirlos.</Typography>
                <Stack direction="row" gap={1} flexWrap="wrap" mt={2}>
                  <Button
                    component={RouterLink}
                    to={session ? authenticatedAction : buildLoginRedirectPath(location.pathname)}
                    variant="contained"
                    startIcon={<LoginIcon />}
                  >
                    {session ? 'Contactar desde uno de mis perfiles' : 'Ingresar para contactar'}
                  </Button>
                  {kind === 'profile' && session && <Button component={RouterLink} to={invitationAction} variant="outlined" startIcon={<PersonAddAltIcon />}>Invitar a una oportunidad</Button>}
                </Stack>
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

function ProfileReviews({ slug, profileId, authenticated }: { slug: string; profileId: string; authenticated: boolean }) {
  const queryClient = useQueryClient();
  const [interactionId, setInteractionId] = useState('');
  const [rating, setRating] = useState<number | null>(5);
  const [body, setBody] = useState('');
  const reviews = useInfiniteQuery({
    queryKey: ['directory-profile-reviews', slug],
    queryFn: ({ pageParam }) => Directory.profileReviews(slug, pageParam),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (page) => page.nextCursor ?? undefined,
  });
  const eligibility = useQuery({
    queryKey: ['directory-review-eligibility'],
    queryFn: () => Directory.reviewEligibility(),
    enabled: authenticated,
  });
  const eligible = (eligibility.data ?? []).filter((item) => item.subjectProfile.id === profileId);
  const selected = eligible.find((item) => item.interactionId === interactionId);
  const create = useMutation({
    mutationFn: () => {
      if (!selected || !rating) throw new Error('Selecciona una interacción y una calificación.');
      return Directory.createReview({
        interactionId: selected.interactionId,
        authorProfileId: selected.authorProfile.id,
        subjectProfileId: selected.subjectProfile.id,
        rating,
        body: body.trim() || null,
      });
    },
    onSuccess: async () => {
      setInteractionId('');
      setRating(5);
      setBody('');
      await Promise.all([
        queryClient.invalidateQueries({ queryKey: ['directory-profile-reviews', slug] }),
        queryClient.invalidateQueries({ queryKey: ['directory-review-eligibility'] }),
        queryClient.invalidateQueries({ queryKey: ['directory', 'profile', slug] }),
      ]);
    },
  });
  const items = reviews.data?.pages.flatMap((page) => page.items) ?? [];
  const summary = reviews.data?.pages[0]?.summary;
  const validBody = body.trim().length === 0 || (body.trim().length >= 10 && body.length <= 2000);

  return <Paper component="section" aria-labelledby="directory-reviews-title" variant="outlined" sx={{ p: { xs: 2.5, md: 3 }, borderRadius: 3 }}>
    <Stack spacing={2.5}>
      <Box>
        <Typography id="directory-reviews-title" variant="h4" fontWeight={850}>Reseñas verificadas</Typography>
        <Typography color="text.secondary">
          {summary?.count ? `${summary.average?.toFixed(1) ?? '—'} de 5 · ${summary.count} reseña${summary.count === 1 ? '' : 's'}` : 'Todavía no hay reseñas públicas.'}
        </Typography>
      </Box>
      {reviews.isLoading && <CircularProgress size={24} />}
      {reviews.isError && <Alert severity="error">No pudimos cargar las reseñas.</Alert>}
      {items.map((review) => <ReviewCard key={review.id} review={review} authenticated={authenticated} />)}
      {reviews.hasNextPage && <Button onClick={() => { void reviews.fetchNextPage(); }} disabled={reviews.isFetchingNextPage}>{reviews.isFetchingNextPage ? 'Cargando…' : 'Ver más reseñas'}</Button>}
      <Divider />
      {!authenticated ? (
        <Alert severity="info">Inicia sesión para reseñar después de una reserva, orden o colaboración completada y verificada.</Alert>
      ) : eligibility.isLoading ? <CircularProgress size={24} /> : eligibility.isError ? (
        <Alert severity="error">No pudimos comprobar tus interacciones elegibles.</Alert>
      ) : eligible.length === 0 ? (
        <Alert severity="info">No tienes una interacción completada y verificada pendiente de reseña con este perfil.</Alert>
      ) : (
        <Stack component="form" spacing={2} onSubmit={(event) => { event.preventDefault(); create.mutate(); }}>
          <Typography variant="h5" fontWeight={800}>Escribir una reseña</Typography>
          <FormControl required>
            <InputLabel>Interacción verificada</InputLabel>
            <Select label="Interacción verificada" value={interactionId} onChange={(event) => setInteractionId(event.target.value)}>
              <MenuItem value="" disabled>Selecciona el perfil con el que actuaste</MenuItem>
              {eligible.map((item) => <MenuItem key={item.interactionId} value={item.interactionId}>{reviewEligibilityLabel(item)}</MenuItem>)}
            </Select>
          </FormControl>
          <Box><Typography component="legend">Calificación</Typography><Rating value={rating} onChange={(_event, value) => setRating(value)} /></Box>
          <TextField label="Comentario opcional" multiline minRows={3} value={body} onChange={(event) => setBody(event.target.value)} inputProps={{ minLength: 10, maxLength: 2000 }} helperText="Si escribes un comentario, usa entre 10 y 2.000 caracteres." error={!validBody} />
          {create.error && <Alert severity="error">{create.error.message}</Alert>}
          {create.isSuccess && <Alert severity="success">Reseña publicada y vinculada a la interacción verificada.</Alert>}
          <Button type="submit" variant="contained" disabled={!selected || !rating || !validBody || create.isPending}>{create.isPending ? 'Publicando…' : 'Publicar reseña'}</Button>
        </Stack>
      )}
    </Stack>
  </Paper>;
}

function ReviewCard({ review, authenticated }: { review: DirectoryReviewPage['items'][number]; authenticated: boolean }) {
  const report = useMutation({ mutationFn: () => Directory.report({ targetKind: 'review', targetId: review.id, reasonCode: 'community-report' }) });
  return <Paper variant="outlined" sx={{ p: 2, borderRadius: 2 }}>
    <Stack spacing={1}>
      <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" gap={1}>
        <Box><Typography component={RouterLink} to={`/directorio/${review.authorProfile.slug}`} color="text.primary" fontWeight={800} sx={{ textDecoration: 'none' }}>{review.authorProfile.name}</Typography><Typography variant="body2" color="text.secondary">Interacción completada verificada</Typography></Box>
        <Rating value={review.rating} readOnly size="small" aria-label={`${review.rating} de 5 estrellas`} />
      </Stack>
      {review.body && <Typography sx={{ whiteSpace: 'pre-wrap' }}>{review.body}</Typography>}
      <Typography variant="caption" color="text.secondary">{new Date(review.createdAt).toLocaleDateString()}</Typography>
      {authenticated && <Button size="small" color="inherit" sx={{ alignSelf: 'flex-start' }} disabled={report.isPending || report.isSuccess} onClick={() => report.mutate()}>{report.isSuccess ? 'Reportada' : 'Reportar reseña'}</Button>}
    </Stack>
  </Paper>;
}

function reviewEligibilityLabel(item: DirectoryReviewEligibility): string {
  return `${item.authorProfile.name} · ${item.interactionKind.replace(/_/g, ' ')}`;
}
