import { InteractionPanel } from '../features/interactions/InteractionPanel';
import { EventMemoryModerationPanel } from '../components/events/EventMemoryModeration';
import { useEffect, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import ArrowBackIcon from '@mui/icons-material/ArrowBack';
import ImageIcon from '@mui/icons-material/Image';
import LinkIcon from '@mui/icons-material/Link';
import RouteIcon from '@mui/icons-material/Route';
import ShareIcon from '@mui/icons-material/Share';
import { Alert, Avatar, Box, Button, ButtonBase, Card, CardContent, Chip, CircularProgress, Divider, Dialog, DialogActions, DialogContent, DialogTitle, MenuItem, Stack, TextField, Typography } from '@mui/material';
import { Link as RouterLink, useLocation, useParams } from 'react-router-dom';
import PageShell, { EmptyState } from '../components/PageShell';
import { SocialEventsAPI, type SocialEventMomentCreateDTO, type SocialTicketTierDTO } from '../api/socialEvents';
import { EventTickets } from '../api/eventTickets';
import { Catalogs } from '../api/catalogs';
import { useSession } from '../session/SessionContext';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { formatCurrency } from '../utils/formatters';
import ExperienceReviews from '../components/reviews/ExperienceReviews';
import EventRsvpControls from '../components/events/EventRsvpControls';
import EventMemoryInvitationControls from '../components/events/EventMemoryInvitations';
import { EventMemoryInvitations } from '../api/eventMemoryInvitations';
import { getAuthSessionEpoch } from '../session/authEvents';
import { useOwnedSessionDraft } from '../session/useOwnedSessionDraft';
import { buildLoginRedirectPath } from '../utils/loginRouting';

const formatDate = (value: string | null | undefined, locale: string, timezone: string) => {
  if (!value) return '';
  const date = new Date(value);
  return Number.isNaN(date.getTime())
    ? value
    : date.toLocaleString(locale, { timeZone: timezone, dateStyle: 'medium', timeStyle: 'short' });
};

const inferredMediaType = (url: string): 'image' | 'video' => /\.(mp4|mov|webm|m4v)(?:$|[?#])/i.test(url) ? 'video' : 'image';

const ticketFee = (faceValueCents: number) => {
  const total = Math.max(0, Math.floor((faceValueCents * 400) / 10000));
  const buyer = Math.ceil(total / 2);
  return { buyer, organizer: total - buyer, checkout: faceValueCents + buyer };
};

interface MomentDraft { caption: string; mediaKind: 'media' | 'audio' | 'text'; mediaUrl: string; file: File | null }
const emptyMomentDraft: MomentDraft = { caption: '', mediaKind: 'media', mediaUrl: '', file: null };
interface PublicationAttempt { eventId: string; scope: string; draft: MomentDraft; extended: boolean; ready: boolean; isCurrentOwner: () => boolean }
interface OwnMomentAction { id: string; mode: 'edit' | 'delete'; caption: string; updatedAt: string }
interface OwnMomentAttempt { eventId: string; action: OwnMomentAction; isCurrentOwner: () => boolean }
const sessionChanged = () => new Error('Tu sesión cambió. Vuelve al evento antes de publicar.');

export default function SocialEventDetailPage() {
  const { eventId = '' } = useParams();
  const { search, pathname, hash } = useLocation();
  const captionInput = useRef<HTMLInputElement>(null);
  const requestedMoment = new URLSearchParams(search).get('moment');
  const focusedMomentRef = useRef<HTMLDivElement>(null);
  const lastFocusedMoment = useRef<string | null>(null);
  const { session } = useSession();
  const accountScope = session ? `account:${session.partyId ?? session.username}` : 'anonymous';
  const { currency: preferredCurrency, locale, timezone } = useLocalePreferences();
  const queryClient = useQueryClient();
  const fileInputRef = useRef<HTMLInputElement>(null);
  const editorScope = `${eventId}:${accountScope}:${getAuthSessionEpoch()}`;
  const composer = useOwnedSessionDraft({ ownerId: session?.partyId ?? null, scope: eventId,
    baseline: emptyMomentDraft, empty: emptyMomentDraft });
  const { caption, mediaKind, mediaUrl, file } = composer.draft;
  const setCaption = (value: string) => composer.setDraft((previous) => ({ ...previous, caption: value }));
  const setMediaKind = (value: MomentDraft['mediaKind']) => composer.setDraft((previous) => ({ ...previous, mediaKind: value }));
  const setMediaUrl = (value: string) => composer.setDraft((previous) => ({ ...previous, mediaUrl: value }));
  const setFile = (value: File | null) => composer.setDraft((previous) => ({ ...previous, file: value }));
  const [storedAction, setStoredAction] = useState<(OwnMomentAction & { scope: string }) | null>(null);
  const ownAction = storedAction?.scope === editorScope ? storedAction : null;
  const setOwnAction = (action: OwnMomentAction | null) => setStoredAction(action ? { ...action, scope: editorScope } : null);
  const uploadedFile = useRef<{ scope: string; file: File; url: string } | null>(null);
  const [publishingPhase, setPublishingPhase] = useState('');
  useEffect(() => { uploadedFile.current = null; if (fileInputRef.current) fileInputRef.current.value = ''; }, [editorScope]);
  const [ticketName, setTicketName] = useState('General');
  const [ticketPrice, setTicketPrice] = useState('');
  const [ticketQuantity, setTicketQuantity] = useState('100');
  const [shareFeedback, setShareFeedback] = useState<{ severity: 'success' | 'error'; message: string } | null>(null);
  const eventQuery = useQuery({
    queryKey: ['social-event', eventId, accountScope],
    queryFn: () => SocialEventsAPI.getEvent(eventId),
    enabled: Boolean(eventId),
  });
  const eventTypeQuery = useQuery({
    queryKey: ['catalog', 'event-types', eventQuery.data?.eventTypeId, locale],
    queryFn: () => Catalogs.getItem('event-types', eventQuery.data!.eventTypeId!, locale),
    enabled: Boolean(eventQuery.data?.eventTypeId),
  });
  const momentsQuery = useQuery({
    queryKey: ['social-event-moments', eventId, accountScope],
    queryFn: () => SocialEventsAPI.listMoments(eventId),
    enabled: Boolean(eventId),
  });
  useEffect(() => {
    const key = `${eventId}:${requestedMoment ?? ''}`;
    if (!requestedMoment || !focusedMomentRef.current || lastFocusedMoment.current === key) return;
    lastFocusedMoment.current = key;
    focusedMomentRef.current.scrollIntoView({ block: 'center' });
    focusedMomentRef.current.focus({ preventScroll: true });
  }, [eventId, eventQuery.data, requestedMoment, momentsQuery.data]);
  const tiersQuery = useQuery({
    queryKey: ['social-event-ticket-tiers', eventId],
    queryFn: () => SocialEventsAPI.listTicketTiers(eventId),
    enabled: Boolean(eventId),
  });
  const numericEventId = Number(eventId);
  const ticketStorefrontPrerequisitesMet = Number.isSafeInteger(numericEventId)
    && numericEventId > 0
    && Boolean(tiersQuery.data?.length)
    && eventQuery.data?.eventPublicListable === true
    && eventQuery.data?.eventTicketPurchaseEnabled === true
    && !(eventQuery.data?.eventEnd && Date.parse(eventQuery.data.eventEnd) <= Date.now());
  const storefrontQuery = useQuery({
    queryKey: ['public-event-ticket-storefront', eventId],
    queryFn: () => EventTickets.getStorefront(numericEventId),
    enabled: ticketStorefrontPrerequisitesMet,
    retry: false,
  });
  const capabilities = useQuery({ queryKey: ['event-memory-preferences', session?.partyId], queryFn: EventMemoryInvitations.preferences, enabled: Boolean(session?.partyId), retry: false });
  const supportsExtended = capabilities.data?.supportsExtendedMoments === true;
  const ownMutation = useMutation({
    mutationFn: async (attempt: OwnMomentAttempt) => {
      if (!attempt.isCurrentOwner()) throw sessionChanged();
      const action = attempt.action;
      if (action.mode === 'delete') await SocialEventsAPI.deleteMoment(attempt.eventId, action.id);
      else await SocialEventsAPI.editMoment(attempt.eventId, action.id, action.caption, action.updatedAt);
    },
    onSuccess: (_value, attempt) => { if (attempt.isCurrentOwner()) { setOwnAction(null); void queryClient.invalidateQueries({ queryKey: ['social-event-moments', attempt.eventId] }); } },
  });
  const postMutation = useMutation({
    mutationFn: async (attempt: PublicationAttempt) => {
      if (!attempt.ready || !attempt.isCurrentOwner()) throw sessionChanged();
      const { caption: submittedCaption, mediaKind: kind, mediaUrl: submittedUrl, file: submittedFile } = attempt.draft;
      let url = submittedUrl.trim();
      if (submittedFile) {
        const maxBytes = submittedFile.type.startsWith('image/') ? 10 * 1024 * 1024 : 25 * 1024 * 1024;
        if (!submittedFile.size || submittedFile.size > maxBytes) throw new Error('El archivo supera el límite: 10 MB para imágenes y 25 MB para audio o video.');
        if (!attempt.extended && !submittedFile.type.startsWith('image/')) throw new Error('Este servidor todavía solo permite subir imágenes.');
        setPublishingPhase('Subiendo archivo…');
        if (uploadedFile.current?.scope === attempt.scope && uploadedFile.current.file === submittedFile) url = uploadedFile.current.url;
        else {
          const uploaded = await (attempt.extended ? SocialEventsAPI.uploadMomentMedia(attempt.eventId, submittedFile) : SocialEventsAPI.uploadMomentImage(attempt.eventId, submittedFile));
          if (!attempt.isCurrentOwner()) throw sessionChanged();
          if (!uploaded || typeof uploaded.eiuPublicUrl !== 'string' || !uploaded.eiuPublicUrl.trim()) {
            throw new Error('No se pudo confirmar la carga. Conservamos el archivo para reintentar.');
          }
          url = uploaded.eiuPublicUrl;
          uploadedFile.current = { scope: attempt.scope, file: submittedFile, url };
        }
      }
      if (kind !== 'text' && !url) throw new Error('Selecciona una imagen o pega un enlace al archivo.');
      if (kind === 'text' && !submittedCaption.trim()) throw new Error('Escribe tu recuerdo antes de publicar.');
      if (submittedCaption.trim().length > 280) throw new Error('La descripción debe tener como máximo 280 caracteres.');
      const payload: SocialEventMomentCreateDTO = {
        emCreateCaption: submittedCaption.trim() || undefined,
        emCreateMediaUrl: kind === 'text' ? '' : url,
        emCreateMediaType: kind === 'text' ? 'text' : kind === 'audio' ? 'audio' : inferredMediaType(url),
      };
      if (!attempt.isCurrentOwner()) throw sessionChanged();
      setPublishingPhase('Publicando recuerdo…');
      return SocialEventsAPI.createMoment(attempt.eventId, payload);
    },
    onSuccess: (_value, attempt) => {
      if (!attempt.isCurrentOwner()) return;
      uploadedFile.current = null;
      composer.setDraft({ ...emptyMomentDraft, mediaKind: attempt.draft.mediaKind });
      if (fileInputRef.current) fileInputRef.current.value = '';
      void queryClient.invalidateQueries({ queryKey: ['social-event-moments', attempt.eventId] });
    },
    onSettled: (_value, _error, attempt) => { if (attempt.isCurrentOwner()) setPublishingPhase(''); },
  });
  const publicationPending = postMutation.variables?.scope === editorScope && postMutation.isPending;
  const ticketTierMutation = useMutation({
    mutationFn: () => {
      const price = Number(ticketPrice);
      const quantity = Number(ticketQuantity);
      if (!Number.isFinite(price) || price < 0) throw new Error('Ingresa un precio válido.');
      if (!Number.isSafeInteger(quantity) || quantity <= 0) throw new Error('Ingresa una cantidad válida.');
      const name = ticketName.trim();
      if (!name) throw new Error('El nombre del ticket es obligatorio.');
      const payload: SocialTicketTierDTO = {
        ticketTierCode: name.toUpperCase().replace(/[^A-Z0-9]+/g, '-').replace(/^-|-$/g, '') || 'GENERAL',
        ticketTierName: name,
        ticketTierPriceCents: Math.round(price * 100),
        ticketTierCurrency: event?.eventCurrency ?? preferredCurrency,
        ticketTierQuantityTotal: quantity,
        ticketTierQuantitySold: 0,
        ticketTierActive: true,
      };
      return SocialEventsAPI.createTicketTier(eventId, payload);
    },
    onSuccess: () => {
      setTicketName('General');
      setTicketPrice('');
      void queryClient.invalidateQueries({ queryKey: ['social-event-ticket-tiers', eventId] });
    },
  });

  const event = eventQuery.data;
  const eventEnded = Boolean(event?.eventEnd && Date.parse(event.eventEnd) <= Date.now());
  useEffect(() => {
    if (event && session && new URLSearchParams(search).get('compose') === '1') {
      captionInput.current?.scrollIntoView?.({ block: 'center' });
      captionInput.current?.focus();
    }
  }, [event, session, search]);
  const isOrganizer = Boolean(session?.partyId && event?.eventOrganizerPartyId && String(session.partyId) === String(event.eventOrganizerPartyId));
  const canShareTickets = ticketStorefrontPrerequisitesMet
    && storefrontQuery.data?.checkoutAvailable === true;
  const ticketPurchaseUrl = typeof window === 'undefined'
    ? ''
    : new URL(`/eventos/${encodeURIComponent(eventId)}/entradas`, window.location.origin).toString();
  const sessionBoundaryError = postMutation.error instanceof Error && postMutation.error.message === sessionChanged().message;
  const error = eventQuery.error ?? momentsQuery.error ?? (postMutation.variables?.scope === editorScope || sessionBoundaryError ? postMutation.error : null);

  const shareTicketPurchaseLink = async () => {
    if (!event || !ticketPurchaseUrl) return;
    setShareFeedback(null);

    if (navigator.share) {
      try {
        await navigator.share({
          title: `Entradas para ${event.eventTitle}`,
          text: `Compra tus entradas para ${event.eventTitle}.`,
          url: ticketPurchaseUrl,
        });
        setShareFeedback({ severity: 'success', message: 'Enlace de compra compartido.' });
        return;
      } catch (shareError) {
        if (shareError instanceof Error && shareError.name === 'AbortError') return;
      }
    }

    try {
      if (!navigator.clipboard?.writeText) throw new Error('Este navegador no permite compartir ni copiar el enlace.');
      await navigator.clipboard.writeText(ticketPurchaseUrl);
      setShareFeedback({ severity: 'success', message: 'Enlace de compra copiado al portapapeles.' });
    } catch (clipboardError) {
      setShareFeedback({
        severity: 'error',
        message: clipboardError instanceof Error ? clipboardError.message : 'No se pudo compartir el enlace de compra.',
      });
    }
  };

  const memories = <>
        <Card id="momentos" variant="outlined">
          <CardContent>
            <Stack spacing={1.5}>
              <Typography variant="h6">Compartir un recuerdo</Typography>
              {!session ? (
                <Stack spacing={1}><Typography>Comparte imágenes o enlaces de video con la comunidad de este evento.</Typography><Button component={RouterLink} to={buildLoginRedirectPath(`${pathname}${search}${hash}`)} variant="contained">Iniciar sesión o crear una cuenta</Button></Stack>
              ) : (
                <>
                  <Alert severity="info">Los archivos y enlaces compartidos aquí son públicos. Publica solo contenido que tengas permiso de compartir. No incluyas información privada de otras personas.</Alert>
                  {supportsExtended && <TextField select label="Tipo de recuerdo" value={mediaKind} disabled={!composer.ready || publicationPending} onChange={(event) => { setMediaKind(event.target.value as typeof mediaKind); setFile(null); setMediaUrl(''); }}><MenuItem value="media">Foto o video</MenuItem><MenuItem value="audio">Audio</MenuItem><MenuItem value="text">Solo texto</MenuItem></TextField>}
                  <TextField disabled={!composer.ready || publicationPending} inputRef={captionInput} label="Describe tu publicación" value={caption} onChange={(event) => setCaption(event.target.value)} multiline minRows={2} inputProps={{ maxLength: 280 }} helperText={`${caption.length}/280`} />
                  <TextField sx={{ display: mediaKind === 'text' ? 'none' : undefined }} label={mediaKind === 'audio' ? 'Enlace público de audio' : 'Enlace de imagen o video'} value={mediaUrl} onChange={(event) => setMediaUrl(event.target.value)} disabled={Boolean(file) || !composer.ready || publicationPending} placeholder="https://…" InputProps={{ startAdornment: <LinkIcon sx={{ mr: 1, color: 'text.secondary' }} /> }} />
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }}>
                    <Button disabled={!composer.ready || publicationPending} sx={{ display: mediaKind === 'text' ? 'none' : undefined }} component="label" variant="outlined" startIcon={<ImageIcon />}>
                      {supportsExtended ? 'Seleccionar archivo' : 'Seleccionar imagen'}
                      <input ref={fileInputRef} hidden type="file" accept={supportsExtended ? "image/jpeg,image/png,image/webp,image/gif,image/bmp,video/mp4,video/webm,video/quicktime,audio/mpeg,audio/mp4,audio/wav,audio/x-wav,audio/ogg,audio/flac" : "image/jpeg,image/png,image/webp,image/gif,image/bmp"} onChange={(event) => { const next = event.target.files?.[0] ?? null; setFile(next); uploadedFile.current = null; if (next) setMediaKind(next.type.startsWith('audio/') ? 'audio' : 'media'); }} />
                    </Button>
                    <Typography variant="caption" color="text.secondary" sx={{ flex: 1 }}>{file ? file.name : supportsExtended ? 'Fotos: hasta 10 MB. Video y audio: hasta 25 MB.' : 'Imágenes de hasta 10 MB. Para video, pega un enlace público.'}</Typography>
                    <Button variant="contained" onClick={() => postMutation.mutate({ eventId, scope: editorScope, draft: composer.draft, extended: supportsExtended, ready: composer.ready, isCurrentOwner: composer.isCurrentOwner })} disabled={!composer.ready || publicationPending || (mediaKind === 'text' ? !caption.trim() : !file && !mediaUrl.trim())}>
                      {publicationPending ? <><CircularProgress size={20} color="inherit" sx={{ mr: 1 }} /><span role="status">{publishingPhase || 'Preparando…'}</span></> : 'Publicar'}
                    </Button>
                  </Stack>
                </>
              )}
            </Stack>
          </CardContent>
        </Card>

        <Stack spacing={1.5}>
          <Typography variant="h5">Recuerdos de la comunidad</Typography>
          <InteractionPanel kind="event" entityKey={eventId} />
          {requestedMoment && momentsQuery.isSuccess && !momentsQuery.data.some((moment) => moment.emId === requestedMoment) && (
            <Alert severity="info" role="status">Esta publicación ya no está disponible o no tienes acceso.</Alert>
          )}
          {momentsQuery.isLoading ? <CircularProgress size={24} /> : momentsQuery.data?.length === 0 ? (
            <EmptyState icon={<ImageIcon fontSize="inherit" />} title="Todavía no hay publicaciones" description="Sé la primera persona en compartir un momento de este evento." />
          ) : momentsQuery.data?.map((moment) => (
            <Card key={moment.emId ?? `${moment.emAuthorPartyId}-${moment.emCreatedAt}`} variant="outlined"
              ref={moment.emId === requestedMoment ? focusedMomentRef : undefined}
              tabIndex={moment.emId === requestedMoment ? -1 : undefined}
              role="region" aria-label={`Publicación de ${moment.emAuthorName}`}
              sx={{ '&:focus': { outline: '3px solid', outlineColor: 'primary.main', outlineOffset: 2 } }}
            >
              <CardContent>
                <Stack spacing={1.25}>
                  {moment.emAuthorPartyId ? (
                    <ButtonBase
                      component={RouterLink}
                      to={`/perfil/${encodeURIComponent(moment.emAuthorPartyId)}`}
                      sx={{ alignSelf: 'flex-start', borderRadius: 2, textAlign: 'left' }}
                    >
                      <Stack direction="row" spacing={1} alignItems="center">
                        <Avatar>{moment.emAuthorName.slice(0, 1).toUpperCase()}</Avatar>
                        <Box>
                          <Typography fontWeight={700}>{moment.emAuthorName}</Typography>
                          <Typography variant="caption" color="text.secondary">Publicado {formatDate(moment.emCreatedAt, locale, timezone)}</Typography>
                        </Box>
                      </Stack>
                    </ButtonBase>
                  ) : (
                    <Stack direction="row" spacing={1} alignItems="center">
                      <Avatar>{moment.emAuthorName.slice(0, 1).toUpperCase()}</Avatar>
                      <Box>
                        <Typography fontWeight={700}>{moment.emAuthorName}</Typography>
                        <Typography variant="caption" color="text.secondary">Publicado {formatDate(moment.emCreatedAt, locale, timezone)}</Typography>
                      </Box>
                    </Stack>
                  )}
                  {moment.emMediaType === 'text' ? null : moment.emMediaType === 'audio' ? <Box component="audio" src={moment.emMediaUrl} controls preload="none" aria-label={`Audio de ${moment.emAuthorName}`} sx={{ width: '100%' }} /> : moment.emMediaType === 'video' ? <Box component="video" src={moment.emMediaUrl} controls preload="metadata" playsInline sx={{ width: '100%', maxHeight: 520, borderRadius: 2, bgcolor: 'black' }} /> : <Box component="img" loading="lazy" src={moment.emMediaUrl} alt={moment.emCaption ?? `Publicación de ${moment.emAuthorName}`} sx={{ width: '100%', maxHeight: 520, objectFit: 'contain', borderRadius: 2, bgcolor: 'action.hover' }} />}
                  {moment.emCaption && <><Divider /><Typography sx={{ whiteSpace: 'pre-wrap' }}>{moment.emCaption}</Typography></>}
                  {event?.eventPublicListable === true && moment.emId && <Button component={RouterLink} to={`/eventos/${encodeURIComponent(eventId)}?moment=${encodeURIComponent(moment.emId)}`} startIcon={<LinkIcon />}>Enlace a esta publicación</Button>}
                  {supportsExtended && moment.emId && String(session?.partyId) === moment.emAuthorPartyId && moment.emUpdatedAt && <Stack direction="row" spacing={1}>
                    <Button onClick={() => { ownMutation.reset(); setOwnAction({ id: moment.emId!, mode: 'edit', caption: moment.emCaption ?? '', updatedAt: moment.emUpdatedAt! }); }}>Editar mi recuerdo</Button>
                    <Button color="error" onClick={() => { ownMutation.reset(); setOwnAction({ id: moment.emId!, mode: 'delete', caption: moment.emCaption ?? '', updatedAt: moment.emUpdatedAt! }); }}>Eliminar mi publicación</Button>
                  </Stack>}
                  {moment.emId && <InteractionPanel kind="event_moment" entityKey={moment.emId} />}
                </Stack>
              </CardContent>
            </Card>
          ))}
        </Stack>
  </>;

  return (
    <PageShell
      title={event?.eventTitle ?? 'Evento'}
      subtitle={event ? `${formatDate(event.eventStart, locale, timezone)} · ${eventTypeQuery.data?.name ?? 'Evento'}` : undefined}
      loading={eventQuery.isLoading}
      actions={<Stack direction="row" spacing={1}>
        <Button component={RouterLink} to="/social/eventos" startIcon={<ArrowBackIcon />}>Eventos</Button>
        {canShareTickets && (
          <Button
            onClick={() => { void shareTicketPurchaseLink(); }}
            startIcon={<ShareIcon />}
            variant="outlined"
          >
            Compartir entradas
          </Button>
        )}
        {session && <Button component={RouterLink} to={`/social/eventos/${eventId}/logistica`} startIcon={<RouteIcon />} variant="contained">Logística</Button>}
      </Stack>}
    >
      <Stack spacing={2.5}>
        {eventEnded && <Alert severity="info">Este evento ya terminó. Comparte tus recuerdos, conversa con la comunidad y descubre próximas experiencias.</Alert>}
        {error && <Alert severity="error">{error instanceof Error ? error.message : 'No se pudo cargar el evento.'}</Alert>}
        {shareFeedback && (
          <Alert severity={shareFeedback.severity} onClose={() => setShareFeedback(null)}>
            {shareFeedback.message}
          </Alert>
        )}
        {event && (
          <Card variant="outlined">
            <CardContent>
              <Stack spacing={1.5}>
                {event.eventImageUrl && <Box component="img" src={event.eventImageUrl} alt={`Afiche de ${event.eventTitle}`} sx={{ width: '100%', maxHeight: 360, objectFit: 'cover', borderRadius: 2 }} />}
                <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                  <Chip label={formatDate(event.eventStart, locale, timezone)} color="primary" />
                  {event.eventWorkflowStateId && (
                    <Chip
                      label={locale.toLowerCase().startsWith('en') ? event.eventWorkflowStateNameEn : event.eventWorkflowStateNameEs}
                      variant="outlined"
                    />
                  )}
                  {event.eventCapacity && <Chip label={`${event.eventCapacity} personas`} variant="outlined" />}
                </Stack>
                {event.eventDescription && <Typography sx={{ whiteSpace: 'pre-wrap' }}>{event.eventDescription}</Typography>}
                {event.eventArtists.length > 0 && <>
                  <Typography component="h2" variant="h6">Artistas participantes</Typography>
                  <Stack direction="row" spacing={1} flexWrap="wrap" useFlexGap>
                    {event.eventArtists.map((artist, index) => artist.artistPartyId && /^[1-9][0-9]*$/.test(artist.artistPartyId)
                      ? <Button key={artist.artistId ?? index} component={RouterLink} to={`/artista/${encodeURIComponent(artist.artistPartyId)}`} variant="outlined">{artist.artistName}</Button>
                      : <Chip key={artist.artistId ?? index} label={artist.artistName} />)}
                  </Stack>
                  <Typography variant="caption" color="text.secondary">Visita sus perfiles para descubrir su música y decidir a quién seguir.</Typography>
                </>}
                <Button component={RouterLink} to="/social/eventos" sx={{ alignSelf: 'flex-start' }}>Descubrir próximos eventos</Button>
              </Stack>
            </CardContent>
          </Card>
        )}

        {eventEnded && memories}
        {event && <Card variant="outlined">
          <CardContent>
            <EventRsvpControls
              eventId={eventId}
              title={event.eventTitle}
              start={event.eventStart}
              timezone={event.eventTimezone ?? timezone}
              locale={locale}
              eligible={event.eventRsvpEligible === true && !eventEnded}
              cancelled={event.eventWorkflowStateCode === 'cancelled'}
              publicShareEligible={event.eventIsPublic === true && event.eventPublicListable === true}
              origin="internal_event_detail"
            />
          </CardContent>
        </Card>}

        <Card variant="outlined">
          <CardContent>
            <Stack spacing={1.5}>
              <Typography variant="h6">Tickets</Typography>
              <Alert severity="info">TDF cobra una comisión del 4%: la mitad se suma al checkout y la otra mitad se descuenta del pago al organizador.</Alert>
              {tiersQuery.isLoading ? <CircularProgress size={22} /> : tiersQuery.data?.length ? (
                <Stack spacing={1}>
                  {tiersQuery.data.map((tier) => {
                    const fee = ticketFee(tier.ticketTierPriceCents);
                    return <Stack key={tier.ticketTierId ?? tier.ticketTierCode} direction={{ xs: 'column', sm: 'row' }} spacing={1} justifyContent="space-between" sx={{ p: 1.25, border: '1px solid', borderColor: 'divider', borderRadius: 2 }}>
                      <Box><Typography fontWeight={700}>{tier.ticketTierName}</Typography><Typography variant="body2" color="text.secondary">{Math.max(0, tier.ticketTierQuantityTotal - tier.ticketTierQuantitySold)} disponibles</Typography></Box>
                      <Box sx={{ textAlign: { sm: 'right' } }}><Typography>{formatCurrency(tier.ticketTierPriceCents / 100, tier.ticketTierCurrency, locale)}</Typography><Typography variant="caption" color="text.secondary">Checkout: {formatCurrency(fee.checkout / 100, tier.ticketTierCurrency, locale)} (incluye {formatCurrency(fee.buyer / 100, tier.ticketTierCurrency, locale)} de tarifa)</Typography></Box>
                    </Stack>;
                  })}
                </Stack>
              ) : <Typography color="text.secondary">Aún no hay tickets para este evento.</Typography>}
              {isOrganizer && <>
                <Divider />
                <Typography variant="subtitle1" fontWeight={700}>Crear tipo de ticket</Typography>
                {ticketTierMutation.error && <Alert severity="error">{ticketTierMutation.error instanceof Error ? ticketTierMutation.error.message : 'No se pudo crear el ticket.'}</Alert>}
                <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
                  <TextField label="Nombre" value={ticketName} onChange={(event) => setTicketName(event.target.value)} sx={{ flex: 1 }} />
                  <TextField label="Precio" type="number" value={ticketPrice} onChange={(event) => setTicketPrice(event.target.value)} inputProps={{ min: 0, step: '0.01' }} />
                  <TextField label="Cantidad" type="number" value={ticketQuantity} onChange={(event) => setTicketQuantity(event.target.value)} inputProps={{ min: 1, step: 1 }} />
                  <Button variant="contained" onClick={() => ticketTierMutation.mutate()} disabled={ticketTierMutation.isPending}>{ticketTierMutation.isPending ? <CircularProgress size={20} color="inherit" /> : 'Crear tickets'}</Button>
                </Stack>
              </>}
            </Stack>
          </CardContent>
        </Card>

        {event && <ExperienceReviews targetKind="event" targetId={eventId} title="Reseñas del evento" />}

        {!eventEnded && memories}
        <EventMemoryInvitationControls eventId={eventId} organizer={isOrganizer} />
        <EventMemoryModerationPanel eventId={eventId} organizer={isOrganizer} />
        <Dialog open={ownAction !== null} onClose={() => { if (!ownMutation.isPending) setOwnAction(null); }} aria-labelledby="own-memory-action">
          <DialogTitle id="own-memory-action">{ownAction?.mode === 'delete' ? 'Eliminar mi publicación' : 'Editar mi recuerdo'}</DialogTitle>
          <DialogContent>
            {ownAction?.mode === 'edit' ? <TextField fullWidth multiline label="Descripción del recuerdo" value={ownAction.caption} inputProps={{ maxLength: 280 }} onChange={(event) => setOwnAction({ ...ownAction, caption: event.target.value })} />
              : <Typography>La publicación dejará de estar disponible en el evento. Los archivos públicos y las copias externas pueden seguir existiendo.</Typography>}
            {ownMutation.isError && <Alert severity="error">No se pudo completar la acción. Actualiza el evento antes de reintentar.</Alert>}
          </DialogContent>
          <DialogActions><Button disabled={ownMutation.isPending} onClick={() => setOwnAction(null)}>Cancelar</Button><Button disabled={ownMutation.isPending} onClick={() => { if (ownAction) ownMutation.mutate({ eventId, action: ownAction, isCurrentOwner: composer.isCurrentOwner }); }}>{ownMutation.isPending ? 'Guardando…' : 'Confirmar'}</Button></DialogActions>
        </Dialog>
      </Stack>
    </PageShell>
  );
}
