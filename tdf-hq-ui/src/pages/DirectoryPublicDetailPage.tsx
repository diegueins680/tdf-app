import MarkdownText from '../components/MarkdownText';
import { InteractionPanel } from '../features/interactions/InteractionPanel';
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
  Link,
  MenuItem,
  Paper,
  Rating,
  Select,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import LoginIcon from '@mui/icons-material/Login';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import ShareIcon from '@mui/icons-material/Share';
import WhatsAppIcon from '@mui/icons-material/WhatsApp';
import { useInfiniteQuery, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useRef, useState } from 'react';
import { Link as RouterLink, Navigate, useLocation, useParams } from 'react-router-dom';

import { Directory, type DirectoryEntityType, type DirectoryReviewEligibility, type DirectoryReviewPage } from '../api/directory';
import { EventTickets } from '../api/eventTickets';
import { findExternalTicketLink, localizeTicketUnavailableReason } from '../utils/externalTicketing';
import { useMetaTags } from '../hooks/useMetaTags';
import { useSession } from '../session/SessionContext';
import { ApiError } from '../api/client';
import { buildEventSignupPath, buildLoginRedirectPath } from '../utils/loginRouting';
import {
  buildDirectoryContactLoginPath,
  shouldResumeDirectoryContact,
} from '../utils/directoryContactRouting';
import EventRsvpControls from '../components/events/EventRsvpControls';
import EventRsvpFeed from '../components/events/EventRsvpFeed';
import { canonicalEventUrl } from '../utils/eventSharing';
import { resolveDirectoryPreviewImage } from '../utils/directoryPreviewImage';
import DirectoryPreviewImage from '../components/directory/DirectoryPreviewImage';
import { useAnalytics } from '../analytics/useAnalytics';
import { useTicketFunnel } from '../analytics/useTicketFunnel';
import { captureGrowthEvent } from '../analytics/growthAttribution';
import { BOTTOM_DOCK_OFFSET } from '../utils/bottomDock';

type DetailKind = Exclude<DirectoryEntityType, never>;

const text = (value: unknown): string | undefined => typeof value === 'string' ? value : undefined;
const number = (value: unknown): number | undefined => typeof value === 'number' ? value : undefined;
const record = (value: unknown): Record<string, unknown> | undefined =>
  typeof value === 'object' && value !== null && !Array.isArray(value) ? value as Record<string, unknown> : undefined;
const rows = (value: unknown): Record<string, unknown>[] =>
  Array.isArray(value) ? value.filter((item): item is Record<string, unknown> => Boolean(record(item))) : [];
const formatEventDate = (value: unknown, locale: string, timezone?: string): string | undefined => {
  const raw = text(value);
  if (!raw) return undefined;
  const parsed = new Date(raw);
  if (Number.isNaN(parsed.getTime())) return undefined;
  try {
    return parsed.toLocaleString(locale, { dateStyle: 'full', timeStyle: 'short', ...(timezone ? { timeZone: timezone } : {}) });
  } catch {
    return parsed.toLocaleString(locale, { dateStyle: 'full', timeStyle: 'short' });
  }
};
const PUBLIC_EVENT_ID = /^[1-9]\d{0,17}$/;
// Lowest price among tiers that still have stock, as "desde $X" for the buy block.
const lowestTicketPrice = (tiers: readonly unknown[] | undefined, locale: string): string | undefined => {
  let best: { minor: number; currency: string } | undefined;
  for (const tier of rows(tiers)) {
    const minor = number(tier['unitPriceMinor']);
    const currency = text(tier['currency']);
    if (minor === undefined || !currency || (number(tier['remaining']) ?? 1) <= 0) continue;
    if (!best || (currency === best.currency && minor < best.minor)) best = { minor, currency };
  }
  if (!best) return undefined;
  try {
    return new Intl.NumberFormat(locale, { style: 'currency', currency: best.currency }).format(best.minor / 100);
  } catch {
    return undefined;
  }
};
export default function DirectoryPublicDetailPage({ kind, authenticationFallback }: { kind: DetailKind; authenticationFallback?: string }) {
  const params = useParams();
  const location = useLocation();
  const { session } = useSession();
  const analytics = useAnalytics();
  const trackFunnel = useTicketFunnel();
  const trackedEventView = useRef<string | null>(null);
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
    ...(authenticationFallback ? { retry: false } : {}),
  });
  // Whether the TDF checkout actually sells this event. Imported events
  // (Eventbrite and similar) have no storefront, so the ticket card must
  // point at the external vendor instead of an empty order form.
  const storefront = useQuery({
    queryKey: ['public-event-ticket-storefront', Number(identifier)],
    queryFn: () => EventTickets.getStorefront(Number(identifier)),
    enabled: kind === 'event' && PUBLIC_EVENT_ID.test(identifier),
    retry: false,
    staleTime: 60_000,
  });
  const value = detail.data ?? {};
  const displayLocale = navigator.language || 'es';
  const english = displayLocale.toLowerCase().startsWith('en');
  const title = text(value['name']) ?? text(value['title']) ?? 'Directorio musical';
  const description = text(value['bio']) ?? text(value['description']) ?? text(value['creditsSummary']) ?? 'Perfil público en TDF.';
  const externalTickets = kind === 'event' ? findExternalTicketLink(text(value['description'])) : null;
  const checkoutOpen = storefront.data?.checkoutAvailable === true;
  // The backend resolves the canonical preview image for every entity kind.
  const absolutePreviewImage = resolveDirectoryPreviewImage(
    kind === 'profile' ? value['previewImageUrl'] : value['imageUrl'],
  );
  const sourceProfile = kind === 'classified' ? record(value['sourceProfile']) : undefined;
  const eventDate = kind === 'event' ? formatEventDate(value['startTime'], displayLocale, text(value['timezone'])) : undefined;
  const eventCancelled = kind === 'event' && text(value['workflowStateCode']) === 'cancelled';
  const canonicalPath = text(value['canonicalUrl']) ?? location.pathname;
  const canonical = kind === 'event'
    ? (() => {
        try { return canonicalEventUrl(window.location.origin, identifier); } catch { return `${window.location.origin}${canonicalPath}`; }
      })()
    : `${window.location.origin}${canonicalPath}`;
  const categoryCode = text(record(value['category'])?.['code']);
  const profileKind = text(value['kind']);
  const reputation = record(value['reputation']);
  const reviewAverage = number(reputation?.['reviewAverage']);
  const reviewCount = number(reputation?.['reviewCount']) ?? 0;
  const schemaType = kind === 'event' ? 'Event'
    : kind === 'venue' ? 'MusicVenue'
      : kind === 'classified' && categoryCode === 'paid-work' ? 'JobPosting'
        : kind === 'classified' && ['offering-services', 'equipment-sale-rental', 'room-studio-available', 'classes'].includes(categoryCode ?? '') ? 'Offer'
          : kind === 'classified' ? 'CreativeWork'
            : ['organization', 'company', 'venue', 'studio', 'agency', 'label', 'distributor', 'school', 'band', 'project'].includes(profileKind ?? '') ? 'Organization'
              : 'Person';

  useEffect(() => {
    if (kind !== 'event' || !identifier || detail.isLoading || detail.isError || trackedEventView.current === identifier) return;
    trackedEventView.current = identifier;
    const params = new URLSearchParams(location.search);
    const source = params.get('utm_source');
    if (params.get('utm_campaign') !== 'event_rsvp' || !['tdf_web', 'tdf_mobile'].includes(source ?? '')) return;
    captureGrowthEvent(analytics, 'event_shared_viewed', {
      platform: 'web',
      event_id: identifier,
      attributed: true,
      source,
    });
  }, [analytics, detail.isError, detail.isLoading, identifier, kind, location.search]);

  useEffect(() => {
    if (kind === 'event' && detail.data && !detail.isError) trackFunnel('event_view', { eventId: Number(identifier) });
  }, [detail.data, detail.isError, identifier, kind, trackFunnel]);

  useMetaTags({
    title,
    description,
    canonical,
    ogImage: absolutePreviewImage,
    ogType: kind === 'profile' ? 'profile' : 'website',
    structuredData: {
      '@context': 'https://schema.org',
      '@type': schemaType,
      name: title,
      description,
      url: canonical,
      ...(absolutePreviewImage ? { image: absolutePreviewImage } : {}),
      ...(reviewAverage && reviewCount > 0 ? { aggregateRating: { '@type': 'AggregateRating', ratingValue: reviewAverage, reviewCount } } : {}),
    },
  });

  const share = async () => {
    if (navigator.share) await navigator.share({ title, text: description, url: canonical });
    else await navigator.clipboard.writeText(canonical);
  };
  const whatsapp = `https://wa.me/?text=${encodeURIComponent(`${title} · ${canonical}`)}`;

  // On phones the buy button stays reachable: once the block under the title
  // scrolls away, a bar pinned to the bottom repeats it. Never both at once.
  const [buyBlock, setBuyBlock] = useState<HTMLElement | null>(null);
  const [buyBlockOffscreen, setBuyBlockOffscreen] = useState(false);
  useEffect(() => {
    if (!buyBlock || typeof IntersectionObserver === 'undefined') {
      setBuyBlockOffscreen(false);
      return;
    }
    const observer = new IntersectionObserver(([entry]) => setBuyBlockOffscreen(entry ? !entry.isIntersecting : false));
    observer.observe(buyBlock);
    return () => observer.disconnect();
  }, [buyBlock]);

  if (detail.isLoading) return <Stack minHeight="55vh" alignItems="center" justifyContent="center"><CircularProgress aria-label="Cargando perfil del directorio" /></Stack>;
  if (detail.isError) {
    const status = detail.error instanceof ApiError ? detail.error.status : undefined;
    if (authenticationFallback && (status === 401 || status === 403 || status === 404)) {
      return <Navigate to={authenticationFallback} replace />;
    }
    return <Container maxWidth="md" sx={{ py: 8 }}>
      <Alert severity="error">Este contenido no está publicado, vigente o disponible.</Alert>
      {authenticationFallback && <Stack direction="row" spacing={2} sx={{ mt: 2 }}>
        <Button onClick={() => { void detail.refetch(); }} disabled={detail.isFetching}>Reintentar</Button>
        <Button component={RouterLink} to={authenticationFallback}>Iniciar sesión</Button>
      </Stack>}
    </Container>;
  }

  // Moment media uses the existing authenticated event endpoint. Preserve old
  // public discussion URLs while sending them through the normal login route.
  const requestedMoment = new URLSearchParams(location.search).get('moment');
  if (kind === 'event' && requestedMoment && /^[1-9][0-9]{0,17}$/.test(requestedMoment)) {
    return <Navigate replace to={`/social/eventos/${encodeURIComponent(identifier)}?moment=${requestedMoment}`} />;
  }

  // A shareable invitation link (`/eventos/:id?signup=1`) sends a visitor
  // without a session straight to account creation and back to this event.
  // Signed-in visitors just see the event.
  if (kind === 'event' && !session && new URLSearchParams(location.search).get('signup') === '1') {
    return <Navigate replace to={buildEventSignupPath(identifier)} />;
  }

  const composeSearch = new URLSearchParams(location.search);
  composeSearch.set('compose', '1');
  const composePath = `/social/eventos/${encodeURIComponent(identifier)}?${composeSearch.toString()}#momentos`;

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
  const ticketsPath = `/eventos/${encodeURIComponent(identifier)}/entradas`;
  const ticketPrice = checkoutOpen ? lowestTicketPrice(storefront.data?.tiers, displayLocale) : undefined;
  const ticketPriceLabel = ticketPrice ? (english ? `From ${ticketPrice}` : `Desde ${ticketPrice}`) : undefined;
  const stickyBuyBar = kind === 'event' && checkoutOpen && !eventCancelled && buyBlockOffscreen;
  const shouldResumeContact = kind === 'profile'
    && Boolean(session)
    && shouldResumeDirectoryContact(location.search, targetId);
  const unauthenticatedAction = kind === 'profile'
    ? buildDirectoryContactLoginPath(location.pathname, targetId)
    : buildLoginRedirectPath(location.pathname);

  return (
    <Box sx={{ py: { xs: 4, md: 7 }, ...(stickyBuyBar ? { pb: { xs: 14, md: 7 } } : {}) }}>
      <Container maxWidth="lg">
        <Stack spacing={3}>
          <Button component={RouterLink} to="/buscar" startIcon={<ArrowBackIcon />} sx={{ alignSelf: 'flex-start' }}>Volver a buscar</Button>
          <Paper variant="outlined" sx={{ p: { xs: 3, md: 5 }, borderRadius: 4 }}>
            <Stack spacing={3}>
              <Stack direction={{ xs: 'column', md: 'row' }} justifyContent="space-between" gap={3}>
                <Stack direction={{ xs: 'column', sm: 'row' }} gap={3} sx={{ minWidth: 0 }}>
                  {/* Always mounted: without media it shows the kind's placeholder. */}
                  <DirectoryPreviewImage
                    kind={kind === 'profile' || kind === 'classified' || kind === 'event' ? kind : 'venue'}
                    imageUrl={absolutePreviewImage}
                    alt={kind === 'event' ? `Afiche de ${title}` : `Foto de ${title}`}
                    fallbackAlt={`Imagen de referencia de ${title}`}
                    width={440}
                    height={440}
                    sizes="(min-width: 600px) 220px, 100vw"
                    eager
                    sx={{
                      width: { xs: '100%', sm: 220 },
                      // A shorter poster on phones keeps an event's buy button in the first screen.
                      height: { xs: kind === 'event' ? 200 : 300, sm: 220 },
                      aspectRatio: 'auto',
                      borderRadius: 3,
                    }}
                  />
                  <Box>
                    <Stack direction="row" gap={1} flexWrap="wrap" mb={1}>
                      <Chip color="primary" label={kind === 'profile' ? 'Perfil profesional' : kind === 'classified' ? 'Clasificado musical' : kind === 'event' ? 'Evento' : 'Venue'} />
                      {category && <Chip label={text(category['name']) ?? 'Oportunidad'} />}
                      {eventCancelled && <Chip color="error" label={english ? 'Cancelled' : 'Cancelado'} />}
                    </Stack>
                    <Typography component="h1" variant="h2" fontWeight={900} sx={{ fontSize: { xs: '2.2rem', md: '3.7rem' } }}>{title}</Typography>
                    {eventDate && <Typography variant="h6" color="text.secondary" mt={1}>{eventDate}</Typography>}
                    {author && <Typography variant="h6" color="text.secondary" mt={1}>Publicado por {text(author['name'])}</Typography>}
                    {sourceProfile && text(sourceProfile['canonicalUrl']) && (
                      <Button
                        component={RouterLink}
                        to={text(sourceProfile['canonicalUrl']) ?? '/buscar'}
                        variant="contained"
                        sx={{ mt: 2 }}
                      >
                        Ver perfil
                      </Button>
                    )}
                    {venue && <Typography variant="h6" color="text.secondary" mt={1}>{text(venue['name'])}</Typography>}
                  </Box>
                </Stack>
                {kind !== 'event' && <Stack direction="row" gap={1} flexWrap="wrap" alignSelf={{ md: 'flex-start' }}>
                  <Button onClick={() => { void share(); }} startIcon={<ShareIcon />}>Compartir</Button>
                  <Button component="a" href={whatsapp} target="_blank" rel="noreferrer" startIcon={<WhatsAppIcon />}>WhatsApp</Button>
                </Stack>}
              </Stack>

              {kind === 'event' && (checkoutOpen ? (
                <Paper ref={setBuyBlock} sx={{ p: { xs: 2, md: 3 }, bgcolor: 'action.hover', borderRadius: 3 }} elevation={0}>
                  <Stack direction="row" alignItems="center" justifyContent="space-between" gap={2} flexWrap="wrap">
                    <Box>
                      <Typography variant="h5" component="h2" fontWeight={800}>{english ? 'Tickets' : 'Entradas'}</Typography>
                      {ticketPriceLabel && <Typography color="text.secondary">{ticketPriceLabel}</Typography>}
                    </Box>
                    <Button component={RouterLink} to={ticketsPath} variant="contained" size="large" sx={{ minHeight: 48, px: 4 }}>
                      Ver entradas
                    </Button>
                  </Stack>
                </Paper>
              ) : externalTickets ? (
                <Paper sx={{ p: { xs: 2, md: 3 }, bgcolor: 'action.hover', borderRadius: 3 }} elevation={0}>
                  <Typography variant="h5" component="h2" fontWeight={800}>Entradas</Typography>
                  <Typography color="text.secondary" mt={1}>
                    {`Las entradas de este evento se venden en ${externalTickets.vendor ?? 'la página del organizador'}.`}
                  </Typography>
                  <Button
                    href={externalTickets.url}
                    target="_blank"
                    rel="noopener noreferrer"
                    variant="contained"
                    endIcon={<OpenInNewIcon />}
                    sx={{ mt: 2 }}
                  >
                    {`Comprar entradas en ${externalTickets.vendor ?? 'la venta oficial'}`}
                  </Button>
                </Paper>
              ) : storefront.data ? (
                <Paper sx={{ p: { xs: 2, md: 3 }, bgcolor: 'action.hover', borderRadius: 3 }} elevation={0}>
                  <Typography variant="h5" component="h2" fontWeight={800}>Entradas</Typography>
                  <Typography color="text.secondary" mt={1}>{localizeTicketUnavailableReason(storefront.data.unavailableReason, english)}</Typography>
                </Paper>
              ) : null)}

              {locationValue && (
                <Alert severity="info">
                  {text(locationValue['city']) ?? 'Ubicación amplia'}{text(locationValue['countryCode']) ? `, ${text(locationValue['countryCode'])}` : ''}. La ubicación pública es aproximada; no incluye direcciones residenciales.
                </Alert>
              )}
              <Divider />
              <MarkdownText sx={{ fontSize: '1.08rem', lineHeight: 1.75 }}>{description}</MarkdownText>

              {kind === 'event' && (
                <Stack spacing={2}>
                  <Paper variant="outlined" sx={{ p: 3, borderRadius: 3 }}>
                    <EventRsvpControls
                      eventId={identifier}
                      title={title}
                      start={text(value['startTime'])}
                      timezone={text(value['timezone'])}
                      venue={text(venue?.['name']) ?? text(record(value['location'])?.['city'])}
                      locale={displayLocale}
                      eligible={value['rsvpEligible'] === true}
                      cancelled={eventCancelled}
                      publicShareEligible={value['publicShareEligible'] === true}
                      initialSummary={{
                        rsvpAcceptedCount: number(record(value['rsvpSummary'])?.['acceptedCount']) ?? 0,
                        rsvpMaybeCount: number(record(value['rsvpSummary'])?.['maybeCount']) ?? 0,
                      }}
                      origin="public_event_detail"
                    />
                  </Paper>
                  <Typography color="text.secondary">
                    {english ? 'Were you there? ' : '¿Estuviste ahí? '}
                    <Link component={RouterLink} to={composePath}>Compartir mis momentos</Link>
                  </Typography>
                </Stack>
              )}

              {kind === 'profile' && profileKind === 'person' && session && (
                <Box component="section" aria-labelledby="directory-profile-rsvp-activity">
                  <Typography id="directory-profile-rsvp-activity" variant="h5" fontWeight={800} mb={1.5}>
                    {english ? 'Activity' : 'Actividad'}
                  </Typography>
                  <EventRsvpFeed directorySlug={identifier} isSelf={false} locale={displayLocale} />
                </Box>
              )}

              {(professions.length > 0 || instruments.length > 0 || genres.length > 0) && (
                <Stack spacing={2}>
                  {professions.length > 0 && <TagSection title="Profesiones" values={professions.map((item) => text(item['name']) ?? text(item['code']) ?? '').filter(Boolean)} />}
                  {instruments.length > 0 && <TagSection title="Instrumentos" values={instruments.map((item) => text(item['name']) ?? text(item['code']) ?? '').filter(Boolean)} />}
                  {genres.length > 0 && <TagSection title="Géneros" values={genres.map((item) => text(item['name']) ?? text(item['code']) ?? '').filter(Boolean)} />}
                </Stack>
              )}

              {kind !== 'venue' && targetId && <InteractionPanel kind={kind === 'profile' ? 'directory_profile' : kind} entityKey={targetId} />}
              {kind === 'profile' && <ProfileReviews slug={identifier} profileId={targetId} authenticated={Boolean(session)} />}

              {shouldResumeContact ? (
                <Alert severity="info">
                  <Stack spacing={1.5}>
                    <Box>
                      <Typography variant="h5" fontWeight={800}>
                        {english ? `Continue your contact with ${title}` : `Continúa tu contacto con ${title}`}
                      </Typography>
                      <Typography color="text.secondary" mt={0.5}>
                        {english
                          ? 'Review the sender profile and write your message before sending it. Nothing will be sent automatically.'
                          : 'Revisa el perfil remitente y escribe tu mensaje antes de enviarlo. Nada se enviará automáticamente.'}
                      </Typography>
                    </Box>
                    <Stack direction="row" gap={1} flexWrap="wrap">
                      <Button component={RouterLink} to={authenticatedAction} variant="contained">
                        {english ? 'Review and write a message' : 'Revisar y escribir mensaje'}
                      </Button>
                      <Button component={RouterLink} to={location.pathname} variant="text">
                        {english ? 'Not now' : 'Ahora no'}
                      </Button>
                    </Stack>
                  </Stack>
                </Alert>
              ) : kind === 'event' ? null : (
                <Paper sx={{ p: 3, bgcolor: 'action.hover', borderRadius: 3 }} elevation={0}>
                  <Typography variant="h5" fontWeight={800}>{kind === 'classified' ? '¿Te interesa esta oportunidad?' : '¿Quieres contactar este perfil?'}</Typography>
                  <Typography color="text.secondary" mt={1}>TDF mantiene tu correo y teléfono ocultos hasta que decidas compartirlos.</Typography>
                  <Stack direction="row" gap={1} flexWrap="wrap" mt={2}>
                    <Button
                      component={RouterLink}
                      to={session ? authenticatedAction : unauthenticatedAction}
                      variant="contained"
                      startIcon={<LoginIcon />}
                    >
                      {session ? 'Contactar desde uno de mis perfiles' : 'Ingresar para contactar'}
                    </Button>
                    {kind === 'profile' && session && <Button component={RouterLink} to={invitationAction} variant="outlined" startIcon={<PersonAddAltIcon />}>Invitar a una oportunidad</Button>}
                  </Stack>
                </Paper>
              )}
            </Stack>
          </Paper>
        </Stack>
      </Container>
      {stickyBuyBar && (
        <Box
          role="region"
          aria-label={english ? 'Buy tickets' : 'Comprar entradas'}
          data-testid="event-sticky-buy"
          sx={{
            display: { xs: 'flex', md: 'none' },
            position: 'fixed',
            left: 0,
            right: 0,
            bottom: BOTTOM_DOCK_OFFSET,
            zIndex: (theme) => theme.zIndex.appBar,
            alignItems: 'center',
            gap: 2,
            px: 2,
            pt: 1.5,
            pb: 'calc(12px + env(safe-area-inset-bottom, 0px))',
            bgcolor: 'background.paper',
            borderTop: '1px solid',
            borderColor: 'divider',
            boxShadow: '0 -8px 28px rgba(0,0,0,0.22)',
          }}
        >
          <Box sx={{ minWidth: 0, flex: 1 }}>
            <Typography variant="subtitle2" noWrap fontWeight={800}>{title}</Typography>
            {ticketPriceLabel && <Typography variant="body2" color="text.secondary" noWrap>{ticketPriceLabel}</Typography>}
          </Box>
          <Button component={RouterLink} to={ticketsPath} variant="contained" sx={{ minHeight: 48, px: 3, flexShrink: 0 }}>
            Ver entradas
          </Button>
        </Box>
      )}
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
      {reviews.isLoading && <CircularProgress size={24} aria-label="Cargando reseñas" />}
      {reviews.isError && <Alert severity="error">No pudimos cargar las reseñas.</Alert>}
      {items.map((review) => <ReviewCard key={review.id} review={review} authenticated={authenticated} />)}
      {reviews.hasNextPage && <Button onClick={() => { void reviews.fetchNextPage(); }} disabled={reviews.isFetchingNextPage}>{reviews.isFetchingNextPage ? 'Cargando…' : 'Ver más reseñas'}</Button>}
      <Divider />
      {!authenticated ? (
        <Alert severity="info">Inicia sesión para reseñar después de una reserva, orden o colaboración completada y verificada.</Alert>
      ) : eligibility.isLoading ? <CircularProgress size={24} aria-label="Comprobando interacciones elegibles" /> : eligibility.isError ? (
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
      {review.body && <MarkdownText>{review.body}</MarkdownText>}
      <Typography variant="caption" color="text.secondary">{new Date(review.createdAt).toLocaleDateString()}</Typography>
      {authenticated && <Button size="small" color="inherit" sx={{ alignSelf: 'flex-start' }} disabled={report.isPending || report.isSuccess} onClick={() => report.mutate()}>{report.isSuccess ? 'Reportada' : 'Reportar reseña'}</Button>}
    </Stack>
  </Paper>;
}

function reviewEligibilityLabel(item: DirectoryReviewEligibility): string {
  return `${item.authorProfile.name} · ${item.interactionKind.replace(/_/g, ' ')}`;
}
