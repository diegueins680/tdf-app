import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import ShareIcon from '@mui/icons-material/Share';
import WhatsAppIcon from '@mui/icons-material/WhatsApp';
import {
  Alert,
  Box,
  Button,
  Checkbox,
  CircularProgress,
  FormControlLabel,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useCallback, useEffect, useRef, useState, type FocusEvent } from 'react';
import { useNavigate } from 'react-router-dom';

import {
  SocialEventsAPI,
  type SocialRsvpDTO,
  type SocialRsvpStatus,
  type SocialRsvpSummaryDTO,
} from '../../api/socialEvents';
import { captureGrowthEvent } from '../../analytics/growthAttribution';
import { useAnalytics } from '../../analytics/useAnalytics';
import { useSession } from '../../session/SessionContext';
import { clearEventRsvpIntent, readEventRsvpIntent, saveEventRsvpIntent } from '../../utils/eventRsvpIntent';
import { buildEventShareMessage, canonicalEventUrl } from '../../utils/eventSharing';

export const eventRsvpQueryKeys = {
  mine: (eventId: string, partyId?: string | number | null) => ['event-rsvp', 'mine', String(partyId ?? 'anonymous'), eventId] as const,
  summary: (eventId: string) => ['event-rsvp', 'summary', eventId] as const,
  feed: (partyId: string) => ['event-rsvp', 'feed', partyId] as const,
};

interface Props {
  eventId: string;
  title: string;
  start?: string | null;
  timezone?: string | null;
  venue?: string | null;
  locale?: string | null;
  eligible: boolean;
  cancelled?: boolean;
  publicShareEligible: boolean;
  initialSummary?: SocialRsvpSummaryDTO;
  compact?: boolean;
  origin: 'public_event_detail' | 'internal_event_detail' | 'event_card';
}

const copyFor = (locale?: string | null) => {
  const en = locale?.toLowerCase().startsWith('en') ?? false;
  return en ? {
    title: 'Your RSVP', going: 'Going', maybe: 'Interested', declined: "Can't go", remove: 'Remove my RSVP',
    profile: 'Show this RSVP on my profile', privacyHint: "Can't go is never published.", manualLink: 'Public event link',
    share: 'Share', copy: 'Copy link', copied: 'Link copied.',
    saved: 'Your RSVP was saved.', removed: 'Your RSVP was removed.', unavailable: 'This event is not accepting RSVPs.',
    cancelled: 'Cancelled events do not accept new RSVPs.', error: 'We could not update your RSVP. Try again.',
    loadError: 'We could not load your RSVP. Check your connection.', retry: 'Retry',
    summaryError: 'We could not refresh RSVP counts.', saving: 'Saving RSVP',
    sharePrompt: 'Want to invite someone?', discard: 'Discard pending RSVP', shareCancelled: 'Sharing was cancelled.', shareError: 'We could not open sharing.',
  } : {
    title: 'Tu RSVP', going: 'Voy', maybe: 'Me interesa', declined: 'No iré', remove: 'Eliminar mi RSVP',
    profile: 'Mostrar este RSVP en mi perfil', privacyHint: '“No iré” nunca se publica.', manualLink: 'Enlace público del evento',
    share: 'Compartir', copy: 'Copiar enlace', copied: 'Enlace copiado.',
    saved: 'Guardamos tu RSVP.', removed: 'Eliminamos tu RSVP.', unavailable: 'Este evento no está aceptando RSVPs.',
    cancelled: 'Los eventos cancelados no aceptan nuevos RSVPs.', error: 'No pudimos actualizar tu RSVP. Inténtalo otra vez.',
    loadError: 'No pudimos cargar tu RSVP. Comprueba tu conexión.', retry: 'Reintentar',
    summaryError: 'No pudimos actualizar los conteos de RSVP.', saving: 'Guardando RSVP',
    sharePrompt: '¿Quieres invitar a alguien?', discard: 'Descartar RSVP pendiente', shareCancelled: 'Cancelaste la acción de compartir.', shareError: 'No pudimos abrir las opciones para compartir.',
  };
};

export default function EventRsvpControls({
  eventId,
  title,
  start,
  timezone,
  venue,
  locale,
  eligible,
  cancelled = false,
  publicShareEligible,
  initialSummary,
  compact = false,
  origin,
}: Props) {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const { session } = useSession();
  const analytics = useAnalytics();
  const labels = copyFor(locale);
  const [showOnProfile, setShowOnProfile] = useState(session?.preferences?.showEventRsvpsOnProfile ?? true);
  const [feedback, setFeedback] = useState<{ severity: 'success' | 'info' | 'error'; message: string } | null>(null);
  const [showSharePrompt, setShowSharePrompt] = useState(false);
  const [mutationPending, setMutationPending] = useState(false);
  const resumedNonce = useRef<string | null>(null);
  const sharedRsvpConversionCaptured = useRef(false);
  const mutationLock = useRef(false);
  const mineKey = eventRsvpQueryKeys.mine(eventId, session?.partyId);
  const summaryKey = eventRsvpQueryKeys.summary(eventId);

  const mine = useQuery({
    queryKey: mineKey,
    queryFn: () => SocialEventsAPI.getMyRsvp(eventId),
    enabled: Boolean(session?.partyId && eventId),
    staleTime: 10_000,
  });
  const summary = useQuery({
    queryKey: summaryKey,
    queryFn: () => SocialEventsAPI.getRsvpSummary(eventId),
    enabled: Boolean(session?.partyId && eventId),
    initialData: initialSummary,
    staleTime: 10_000,
  });

  useEffect(() => {
    if (mine.data) setShowOnProfile(mine.data.rsvpShowOnProfile);
  }, [mine.data]);

  const updateSummary = useCallback((previous: SocialRsvpDTO | null | undefined, next: SocialRsvpDTO | null) => {
    queryClient.setQueryData<SocialRsvpSummaryDTO | undefined>(summaryKey, (current) => {
      if (!current) return current;
      const delta = (status: SocialRsvpStatus) => Number(next?.rsvpStatus === status) - Number(previous?.rsvpStatus === status);
      return {
        rsvpAcceptedCount: Math.max(0, current.rsvpAcceptedCount + delta('accepted')),
        rsvpMaybeCount: Math.max(0, current.rsvpMaybeCount + delta('maybe')),
      };
    });
  }, [queryClient, summaryKey]);

  const upsert = useMutation({
    mutationFn: ({ status, profile }: { status: SocialRsvpStatus; profile: boolean }) =>
      SocialEventsAPI.upsertMyRsvp(eventId, { rsvpStatus: status, rsvpShowOnProfile: profile }),
    onMutate: async ({ status, profile }) => {
      mutationLock.current = true;
      setMutationPending(true);
      await queryClient.cancelQueries({ queryKey: mineKey });
      const previous = queryClient.getQueryData<SocialRsvpDTO | null>(mineKey);
      const optimistic: SocialRsvpDTO = {
        rsvpEventId: eventId,
        rsvpStatus: status,
        rsvpShowOnProfile: status === 'declined' ? false : profile,
      };
      queryClient.setQueryData(mineKey, optimistic);
      updateSummary(previous, optimistic);
      return { previous };
    },
    onError: (_error, _input, context) => {
      mutationLock.current = false;
      setMutationPending(false);
      const optimistic = queryClient.getQueryData<SocialRsvpDTO | null>(mineKey);
      queryClient.setQueryData(mineKey, context?.previous ?? null);
      updateSummary(optimistic, context?.previous ?? null);
      setFeedback({ severity: 'error', message: labels.error });
    },
    onSuccess: (authoritative, input, context) => {
      mutationLock.current = false;
      setMutationPending(false);
      const pendingIntent = readEventRsvpIntent(eventId);
      queryClient.setQueryData(mineKey, authoritative);
      setShowOnProfile(authoritative.rsvpShowOnProfile);
      if (pendingIntent) clearEventRsvpIntent();
      captureGrowthEvent(analytics, context?.previous ? 'event_rsvp_updated' : 'event_rsvp_created', {
        platform: 'web', event_id: eventId, rsvp_status: authoritative.rsvpStatus, origin,
      });
      const shareParams = new URLSearchParams(window.location.search);
      const sharedVisit = shareParams.get('utm_campaign') === 'event_rsvp'
        && ['tdf_web', 'tdf_mobile'].includes(shareParams.get('utm_source') ?? '');
      if (!sharedRsvpConversionCaptured.current && (pendingIntent?.sharedAttribution || sharedVisit)) {
        sharedRsvpConversionCaptured.current = true;
        captureGrowthEvent(analytics, 'event_shared_visit_to_rsvp', {
          platform: 'web', event_id: eventId, rsvp_status: authoritative.rsvpStatus,
        });
      }
      setFeedback({ severity: 'success', message: labels.saved });
      if (input.status === 'accepted' || input.status === 'maybe') {
        setShowSharePrompt(true);
        captureGrowthEvent(analytics, 'event_share_prompt_shown', { platform: 'web', event_id: eventId, rsvp_status: input.status });
      }
      if (session?.partyId) void queryClient.invalidateQueries({ queryKey: eventRsvpQueryKeys.feed(String(session.partyId)) });
      void queryClient.invalidateQueries({ queryKey: summaryKey });
    },
    onSettled: () => {
      mutationLock.current = false;
      setMutationPending(false);
    },
  });

  const remove = useMutation({
    mutationFn: () => SocialEventsAPI.deleteMyRsvp(eventId),
    onMutate: async () => {
      mutationLock.current = true;
      setMutationPending(true);
      await queryClient.cancelQueries({ queryKey: mineKey });
      const previous = queryClient.getQueryData<SocialRsvpDTO | null>(mineKey);
      queryClient.setQueryData(mineKey, null);
      updateSummary(previous, null);
      return { previous };
    },
    onError: (_error, _input, context) => {
      mutationLock.current = false;
      setMutationPending(false);
      queryClient.setQueryData(mineKey, context?.previous ?? null);
      updateSummary(null, context?.previous ?? null);
      setFeedback({ severity: 'error', message: labels.error });
    },
    onSuccess: () => {
      mutationLock.current = false;
      setMutationPending(false);
      queryClient.setQueryData(mineKey, null);
      setFeedback({ severity: 'success', message: labels.removed });
      captureGrowthEvent(analytics, 'event_rsvp_deleted', { platform: 'web', event_id: eventId, origin });
      if (session?.partyId) void queryClient.invalidateQueries({ queryKey: eventRsvpQueryKeys.feed(String(session.partyId)) });
      void queryClient.invalidateQueries({ queryKey: summaryKey });
    },
    onSettled: () => {
      mutationLock.current = false;
      setMutationPending(false);
    },
  });

  useEffect(() => {
    if (!session?.partyId || mutationPending) return;
    const intent = readEventRsvpIntent(eventId);
    if (!intent || resumedNonce.current === intent.nonce) return;
    resumedNonce.current = intent.nonce;
    if (!eligible) {
      setFeedback({ severity: 'info', message: cancelled ? labels.cancelled : labels.unavailable });
      return;
    }
    setShowOnProfile(intent.showOnProfile);
    captureGrowthEvent(analytics, 'event_rsvp_post_auth_resumed', {
      platform: 'web', event_id: eventId, origin: intent.origin,
    });
    mutationLock.current = true;
    upsert.mutate({ status: intent.status, profile: intent.showOnProfile });
  }, [analytics, cancelled, eligible, eventId, labels.cancelled, labels.unavailable, mutationPending, session?.partyId, upsert]);

  const choose = (status: SocialRsvpStatus) => {
    if (mutationLock.current) return;
    setFeedback(null);
    captureGrowthEvent(analytics, 'event_rsvp_started', { platform: 'web', event_id: eventId, rsvp_status: status, origin });
    if (!eligible) {
      setFeedback({ severity: 'info', message: cancelled ? labels.cancelled : labels.unavailable });
      return;
    }
    if (!session?.partyId) {
      mutationLock.current = true;
      const shareParams = new URLSearchParams(window.location.search);
      const intent = saveEventRsvpIntent({
        eventId,
        status,
        showOnProfile,
        origin,
        sharedAttribution: shareParams.get('utm_campaign') === 'event_rsvp'
          && ['tdf_web', 'tdf_mobile'].includes(shareParams.get('utm_source') ?? ''),
      });
      const params = new URLSearchParams({ signup: '1', intent: 'events', redirect: intent.returnTo });
      captureGrowthEvent(analytics, 'event_rsvp_auth_redirected', { platform: 'web', event_id: eventId, rsvp_status: status, origin });
      navigate(`/login?${params.toString()}`);
      return;
    }
    mutationLock.current = true;
    upsert.mutate({ status, profile: showOnProfile });
  };

  const share = async (method: 'native' | 'copy') => {
    if (!publicShareEligible) {
      setFeedback({ severity: 'info', message: labels.unavailable });
      return;
    }
    const url = canonicalEventUrl(window.location.origin, eventId, {
      utm_source: 'tdf_web', utm_medium: method === 'copy' ? 'copy' : 'share', utm_campaign: 'event_rsvp',
    });
    const message = buildEventShareMessage({ eventId, title, start, timezone, venue, status: mine.data?.rsvpStatus === 'declined' ? null : mine.data?.rsvpStatus, locale });
    captureGrowthEvent(analytics, 'event_share_started', { platform: 'web', event_id: eventId, method });
    try {
      if (method === 'native' && navigator.share) {
        await navigator.share({ title, text: message, url });
        captureGrowthEvent(analytics, 'event_share_completed', { platform: 'web', event_id: eventId, method: 'native' });
      } else {
        if (navigator.clipboard?.writeText) {
          await navigator.clipboard.writeText(url);
        } else {
          const input = document.createElement('textarea');
          input.value = url;
          input.setAttribute('readonly', '');
          input.style.position = 'fixed';
          input.style.opacity = '0';
          document.body.appendChild(input);
          input.select();
          const copied = document.execCommand('copy');
          input.remove();
          if (!copied) throw new Error('Clipboard unavailable');
        }
        setFeedback({ severity: 'success', message: labels.copied });
        captureGrowthEvent(analytics, 'event_link_copied', { platform: 'web', event_id: eventId, method: 'copy' });
      }
      setShowSharePrompt(false);
    } catch (error) {
      if (error instanceof DOMException && error.name === 'AbortError') {
        setFeedback({ severity: 'info', message: labels.shareCancelled });
        captureGrowthEvent(analytics, 'event_share_cancelled', { platform: 'web', event_id: eventId, method });
      } else {
        setFeedback({ severity: 'error', message: labels.shareError });
        captureGrowthEvent(analytics, 'event_share_failed', { platform: 'web', event_id: eventId, method });
      }
    }
  };

  const current = mine.data?.rsvpStatus;
  const pending = mutationPending;
  const loading = Boolean(session?.partyId && mine.isLoading);
  const busy = pending || loading;
  const summaryValue = summary.data ?? initialSummary ?? { rsvpAcceptedCount: 0, rsvpMaybeCount: 0 };
  const whatsapp = publicShareEligible
    ? `https://wa.me/?text=${encodeURIComponent(`${buildEventShareMessage({ eventId, title, start, timezone, venue, status: current === 'declined' ? null : current, locale })} ${canonicalEventUrl(window.location.origin, eventId, { utm_source: 'tdf_web', utm_medium: 'whatsapp', utm_campaign: 'event_rsvp' })}`)}`
    : undefined;
  const publicUrl = publicShareEligible
    ? canonicalEventUrl(window.location.origin, eventId)
    : '';

  const changeProfileVisibility = (checked: boolean) => {
    if (mutationLock.current) return;
    setShowOnProfile(checked);
    if (current) {
      mutationLock.current = true;
      upsert.mutate({ status: current, profile: checked });
    }
  };

  const removeRsvp = () => {
    if (mutationLock.current) return;
    mutationLock.current = true;
    remove.mutate();
  };

  return <Box component="section" aria-labelledby={`event-rsvp-title-${eventId}`} aria-busy={busy}>
    <Stack spacing={compact ? 1 : 1.5}>
      <Box>
        <Typography id={`event-rsvp-title-${eventId}`} variant={compact ? 'subtitle1' : 'h6'} fontWeight={800}>{labels.title}</Typography>
        <Typography variant="body2" color="text.secondary">
          {summaryValue.rsvpAcceptedCount} {labels.going.toLowerCase()} · {summaryValue.rsvpMaybeCount} {labels.maybe.toLowerCase()}
        </Typography>
      </Box>
      <Stack direction="row" gap={1} flexWrap="wrap" role="group" aria-label={labels.title}>
        {([
          ['accepted', labels.going], ['maybe', labels.maybe], ['declined', labels.declined],
        ] as const).map(([status, label]) => <Button
          key={status}
          size={compact ? 'small' : 'medium'}
          variant={current === status ? 'contained' : 'outlined'}
          aria-pressed={current === status}
          disabled={busy || !eligible}
          onClick={() => choose(status)}
        >{label}</Button>)}
        {current && <Button size={compact ? 'small' : 'medium'} color="inherit" startIcon={<DeleteOutlineIcon />} disabled={busy} onClick={removeRsvp}>{labels.remove}</Button>}
        {busy && <CircularProgress size={22} aria-label={loading ? labels.title : labels.saving} />}
      </Stack>
      {mine.isError && <Alert severity="error" action={<Button color="inherit" size="small" onClick={() => { void mine.refetch(); }}>{labels.retry}</Button>}>{labels.loadError}</Alert>}
      {summary.isError && <Alert severity="error" action={<Button color="inherit" size="small" onClick={() => { void summary.refetch(); }}>{labels.retry}</Button>}>{labels.summaryError}</Alert>}
      <FormControlLabel
        control={<Checkbox checked={showOnProfile} onChange={(event) => changeProfileVisibility(event.target.checked)} disabled={busy} />}
        label={labels.profile}
      />
      <Typography variant="caption" color="text.secondary">{labels.privacyHint}</Typography>
      <Stack direction="row" gap={1} flexWrap="wrap">
        <Button startIcon={<ShareIcon />} disabled={!publicShareEligible} onClick={() => { void share('native'); }}>{labels.share}</Button>
        <Button startIcon={<ContentCopyIcon />} disabled={!publicShareEligible} onClick={() => { void share('copy'); }}>{labels.copy}</Button>
        {whatsapp && <Button component="a" href={whatsapp} target="_blank" rel="noreferrer" startIcon={<WhatsAppIcon />} onClick={() => captureGrowthEvent(analytics, 'event_share_started', { platform: 'web', event_id: eventId, method: 'whatsapp' })}>WhatsApp</Button>}
      </Stack>
      {!compact && publicUrl && (
        <TextField
          label={labels.manualLink}
          value={publicUrl}
          size="small"
          fullWidth
          slotProps={{ htmlInput: { readOnly: true, onFocus: (event: FocusEvent<HTMLInputElement>) => event.currentTarget.select() } }}
        />
      )}
      {!eligible && <Alert severity="info">{cancelled ? labels.cancelled : labels.unavailable}</Alert>}
      {session?.partyId && readEventRsvpIntent(eventId) && !eligible && (
        <Button
          size="small"
          color="inherit"
          onClick={() => {
            clearEventRsvpIntent();
            setFeedback(null);
          }}
        >
          {labels.discard}
        </Button>
      )}
      {showSharePrompt && <Alert severity="success" action={<Button color="inherit" size="small" onClick={() => { void share('native'); }}>{labels.share}</Button>}>{labels.sharePrompt}</Alert>}
      {feedback && <Alert severity={feedback.severity} role="status" aria-live="polite" onClose={() => setFeedback(null)}>{feedback.message}</Alert>}
    </Stack>
  </Box>;
}
