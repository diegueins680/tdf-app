import ContentCopyIcon from '@mui/icons-material/ContentCopy';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import OpenInNewIcon from '@mui/icons-material/OpenInNew';
import ShareIcon from '@mui/icons-material/Share';
import { Alert, Box, Button, Card, CardContent, CircularProgress, Stack, Typography } from '@mui/material';
import { useInfiniteQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { useState } from 'react';
import { Link as RouterLink } from 'react-router-dom';

import { SocialEventsAPI, type SocialRsvpFeedItemDTO } from '../../api/socialEvents';
import { captureGrowthEvent } from '../../analytics/growthAttribution';
import { useAnalytics } from '../../analytics/useAnalytics';
import { buildEventShareMessage, canonicalEventPath, canonicalEventUrl, safePublicImageUrl } from '../../utils/eventSharing';
import { eventRsvpQueryKeys } from './EventRsvpControls';

export default function EventRsvpFeed({ partyId, directorySlug, isSelf, locale }: { partyId?: string; directorySlug?: string; isSelf: boolean; locale?: string }) {
  const queryClient = useQueryClient();
  const analytics = useAnalytics();
  const feedScope = directorySlug ? `directory:${directorySlug}` : String(partyId ?? 'missing');
  const feed = useInfiniteQuery({
    queryKey: eventRsvpQueryKeys.feed(feedScope),
    queryFn: ({ pageParam }) => directorySlug
      ? SocialEventsAPI.listDirectoryProfileRsvpFeed(directorySlug, pageParam, 20)
      : SocialEventsAPI.listRsvpFeed(String(partyId), pageParam, 20),
    initialPageParam: undefined as string | undefined,
    getNextPageParam: (page) => page.feedNextCursor ?? undefined,
    enabled: Boolean(directorySlug ?? partyId),
  });
  const remove = useMutation({
    mutationFn: (eventId: string) => SocialEventsAPI.deleteMyRsvp(eventId),
    onSuccess: (_value, eventId) => {
      void queryClient.invalidateQueries({ queryKey: eventRsvpQueryKeys.feed(feedScope) });
      queryClient.setQueryData(eventRsvpQueryKeys.mine(eventId, partyId), null);
      captureGrowthEvent(analytics, 'event_rsvp_deleted', { platform: 'web', event_id: eventId, origin: 'profile_feed' });
    },
  });
  const items = feed.data?.pages.flatMap((page) => page.feedItems) ?? [];
  const english = locale?.toLowerCase().startsWith('en') ?? false;

  if (feed.isLoading) return <CircularProgress size={24} aria-label={english ? 'Loading activity' : 'Cargando actividad'} />;
  if (feed.isError) return <Alert severity="error" action={<Button color="inherit" size="small" onClick={() => { void feed.refetch(); }}>{english ? 'Retry' : 'Reintentar'}</Button>}>{english ? 'We could not load RSVP activity.' : 'No pudimos cargar la actividad de RSVP.'}</Alert>;
  if (items.length === 0) return <Alert severity="info">{english ? 'There is no public RSVP activity yet.' : 'Todavía no hay actividad de RSVP visible.'}</Alert>;

  return <Stack spacing={1.5}>
    {items.map((item) => <FeedCard
      key={`${item.feedEventId}-${item.feedActionAt}`}
      item={item}
      locale={locale}
      onRemove={isSelf && item.feedCanEdit ? () => remove.mutate(item.feedEventId) : undefined}
      removing={remove.isPending && remove.variables === item.feedEventId}
      analytics={analytics}
    />)}
    {feed.hasNextPage && <Button onClick={() => { void feed.fetchNextPage(); }} disabled={feed.isFetchingNextPage}>
      {feed.isFetchingNextPage ? (english ? 'Loading…' : 'Cargando…') : (english ? 'Load more' : 'Ver más')}
    </Button>}
  </Stack>;
}

function FeedCard({
  item,
  locale,
  onRemove,
  removing,
  analytics,
}: {
  item: SocialRsvpFeedItemDTO;
  locale?: string;
  onRemove?: () => void;
  removing: boolean;
  analytics: ReturnType<typeof useAnalytics>;
}) {
  const english = locale?.toLowerCase().startsWith('en') ?? false;
  const isCancelled = item.feedWorkflowStateCode === 'cancelled';
  const eventHasEnded = Date.parse(item.feedEventStart) < Date.now();
  const [shareFeedback, setShareFeedback] = useState<{ severity: 'success' | 'info' | 'error'; message: string } | null>(null);
  let eventPath: string | null = null;
  try {
    eventPath = canonicalEventPath(item.feedEventId);
  } catch {
    eventPath = null;
  }
  const verb = item.feedStatus === 'accepted'
    ? eventHasEnded
      ? (english ? 'Marked that they planned to attend' : 'Marcó que asistiría a')
      : (english ? 'Is going to' : 'Va a')
    : (english ? 'Is interested in' : 'Le interesa');
  const image = safePublicImageUrl(item.feedEventImageUrl, window.location.origin);
  const share = async (copyOnly = false) => {
    const url = canonicalEventUrl(window.location.origin, item.feedEventId, {
      utm_source: 'tdf_web', utm_medium: copyOnly ? 'copy' : 'share', utm_campaign: 'event_rsvp',
    });
    const message = buildEventShareMessage({
      eventId: item.feedEventId,
      title: item.feedEventTitle,
      start: item.feedEventStart,
      timezone: item.feedEventTimezone,
      venue: item.feedVenueName ?? item.feedCity,
      status: item.feedStatus,
      locale,
    });
    const method = copyOnly || !navigator.share ? 'copy' : 'native';
    captureGrowthEvent(analytics, 'event_share_started', { platform: 'web', event_id: item.feedEventId, method, origin: 'profile_feed' });
    try {
      if (method === 'native') {
        await navigator.share({ title: item.feedEventTitle, text: message, url });
        captureGrowthEvent(analytics, 'event_share_completed', { platform: 'web', event_id: item.feedEventId, method, origin: 'profile_feed' });
      } else {
        await navigator.clipboard.writeText(url);
        setShareFeedback({ severity: 'success', message: english ? 'Link copied.' : 'Enlace copiado.' });
        captureGrowthEvent(analytics, 'event_link_copied', { platform: 'web', event_id: item.feedEventId, method, origin: 'profile_feed' });
      }
    } catch (error) {
      if (error instanceof DOMException && error.name === 'AbortError') {
        setShareFeedback({ severity: 'info', message: english ? 'Sharing was cancelled.' : 'Cancelaste la acción de compartir.' });
        captureGrowthEvent(analytics, 'event_share_cancelled', { platform: 'web', event_id: item.feedEventId, method, origin: 'profile_feed' });
      } else {
        setShareFeedback({ severity: 'error', message: english ? 'We could not share this event.' : 'No pudimos compartir este evento.' });
        captureGrowthEvent(analytics, 'event_share_failed', { platform: 'web', event_id: item.feedEventId, method, origin: 'profile_feed' });
      }
    }
  };

  if (!eventPath) return null;

  return <Card variant="outlined">
    <CardContent>
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={2}>
        {image && <Box component="img" src={image} alt="" sx={{ width: { xs: '100%', sm: 144 }, height: 112, borderRadius: 2, objectFit: 'cover' }} />}
        <Stack spacing={0.75} flex={1}>
          <Typography variant="overline">{verb}</Typography>
          <Typography variant="h6" fontWeight={800}>{item.feedEventTitle}</Typography>
          <Typography variant="body2" color="text.secondary">
            {new Date(item.feedEventStart).toLocaleString(locale, { dateStyle: 'medium', timeStyle: 'short', ...(item.feedEventTimezone ? { timeZone: item.feedEventTimezone } : {}) })}
            {(item.feedVenueName ?? item.feedCity) ? ` · ${item.feedVenueName ?? item.feedCity}` : ''}
          </Typography>
          {isCancelled && <Alert severity="warning">{english ? 'Cancelled' : 'Cancelado'}</Alert>}
          {shareFeedback && <Alert severity={shareFeedback.severity} role="status" onClose={() => setShareFeedback(null)}>{shareFeedback.message}</Alert>}
          <Typography variant="caption" color="text.secondary">
            {english ? 'Updated' : 'Actualizado'} {new Date(item.feedActionAt).toLocaleString(locale)}
          </Typography>
          <Stack direction="row" gap={1} flexWrap="wrap">
            <Button component={RouterLink} to={eventPath} startIcon={<OpenInNewIcon />}>{english ? 'Open event' : 'Abrir evento'}</Button>
            {item.feedCanShare && <Button onClick={() => { void share(false); }} startIcon={<ShareIcon />}>{english ? 'Share' : 'Compartir'}</Button>}
            {item.feedCanShare && <Button onClick={() => { void share(true); }} startIcon={<ContentCopyIcon />}>{english ? 'Copy link' : 'Copiar enlace'}</Button>}
            {onRemove && <Button color="inherit" onClick={onRemove} disabled={removing} startIcon={<DeleteOutlineIcon />}>{english ? 'Remove RSVP' : 'Eliminar RSVP'}</Button>}
          </Stack>
        </Stack>
      </Stack>
    </CardContent>
  </Card>;
}
