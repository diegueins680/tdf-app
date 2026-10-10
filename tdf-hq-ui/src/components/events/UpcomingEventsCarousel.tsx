import { useEffect, useState } from 'react';
import { useMutation, useQueries, useQuery, useQueryClient } from '@tanstack/react-query';
import CloseIcon from '@mui/icons-material/Close';
import { Alert, Box, Button, Card, CardActionArea, CardMedia, Chip, IconButton, Stack, Typography } from '@mui/material';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { useTranslation } from 'react-i18next';
import { API_BASE_URL } from '../../api/client';
import {
  SocialEventsAPI,
  type PublicUpcomingEventDTO,
  type SocialRsvpDTO,
  type SocialRsvpStatus,
} from '../../api/socialEvents';
import { useSession } from '../../session/SessionContext';
import { eventRsvpQueryKeys } from './eventRsvpQueryKeys';

const CAROUSEL_LIMIT = 8;
const DISMISS_KEY = 'tdf.upcomingEventsCarousel.dismissed';
const EVENT_IMAGE_FALLBACK = '/event-fallback.svg';
export const EVENTS_PATH = '/social/eventos';

const rsvpLabelKey: Record<SocialRsvpStatus, string> = {
  accepted: 'authEntry.eventsCarousel.going',
  maybe: 'authEntry.eventsCarousel.maybe',
  declined: 'authEntry.eventsCarousel.notGoing',
};

const resolveImageUrl = (value: string | null | undefined): string => {
  if (!value) return EVENT_IMAGE_FALLBACK;
  try { return new URL(value, API_BASE_URL || window.location.origin).toString(); } catch { return EVENT_IMAGE_FALLBACK; }
};

// Shown in the event's own timezone, like the event detail, so the hour and day match it.
const startFormatter = (locale: string, timeZone?: string) => new Intl.DateTimeFormat(locale, {
  weekday: 'short', day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit', hourCycle: 'h23', timeZone,
});

const formatStart = (value: string, locale: string, timeZone?: string | null): string | null => {
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return null;
  try {
    return startFormatter(locale, timeZone ?? undefined).format(parsed);
  } catch {
    // An unknown timezone name must not hide the date.
    try { return startFormatter(locale).format(parsed); } catch { return null; }
  }
};

const REFRESH_MS = 5 * 60_000;
const notStarted = (event: PublicUpcomingEventDTO, now: number) => {
  const start = Date.parse(event.publicUpcomingEventStart);
  return Number.isNaN(start) || start > now;
};

const readDismissed = () => {
  try { return window.sessionStorage.getItem(DISMISS_KEY) === '1'; } catch { return false; }
};

// Shown to every signed-in user right after login: the next public events and
// the viewer's own RSVP for each, with a one-tap "Asistiré".
export default function UpcomingEventsCarousel() {
  const { t, i18n } = useTranslation();
  const { session, loading: sessionLoading } = useSession();
  const location = useLocation();
  const queryClient = useQueryClient();
  const [dismissed, setDismissed] = useState(readDismissed);
  const onEventsPage = location.pathname === EVENTS_PATH || location.pathname.startsWith(`${EVENTS_PATH}/`);
  // A stored session is only a hint until the server confirms it.
  const enabled = Boolean(session) && !sessionLoading && !dismissed && !onEventsPage;
  // Wall-clock time, so a started event leaves even if no refetch succeeds.
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    if (!enabled) return undefined;
    const timer = window.setInterval(() => setNow(Date.now()), 60_000);
    return () => window.clearInterval(timer);
  }, [enabled]);
  // Each card keeps its own write state: a second tap elsewhere must not hide a failure here.
  const [attendState, setAttendState] = useState<Record<string, 'pending' | 'failed'>>({});
  const settleAttend = (eventId: string, state?: 'pending' | 'failed') => setAttendState((current) => {
    const next = { ...current };
    if (state) next[eventId] = state; else delete next[eventId];
    return next;
  });

  const eventsQuery = useQuery({
    queryKey: ['upcoming-events-carousel'],
    // The shell can stay mounted for days: every fetch asks from the current time.
    queryFn: ({ signal }) => SocialEventsAPI.listPublicUpcomingEvents({
      startAfter: new Date().toISOString(), limit: CAROUSEL_LIMIT, signal,
    }),
    enabled,
    staleTime: REFRESH_MS,
    refetchInterval: REFRESH_MS,
  });
  // This sits in both shells: an unexpected response must hide the carousel, never break the page.
  const events = (Array.isArray(eventsQuery.data) ? eventsQuery.data : [])
    .filter((event) => notStarted(event, now));

  const rsvpQueries = useQueries({
    queries: events.map((event) => ({
      queryKey: eventRsvpQueryKeys.mine(event.publicUpcomingEventId, session?.partyId),
      queryFn: () => SocialEventsAPI.getMyRsvp(event.publicUpcomingEventId),
      enabled,
      staleTime: 60_000,
    })),
  });

  const attend = useMutation({
    mutationFn: async (eventId: string) => {
      // A read still in flight must not land after the write and restore the old answer.
      await queryClient.cancelQueries({ queryKey: eventRsvpQueryKeys.mine(eventId, session?.partyId) });
      return SocialEventsAPI.upsertMyRsvp(eventId, {
        rsvpStatus: 'accepted',
        rsvpShowOnProfile: session?.preferences?.showEventRsvpsOnProfile ?? true,
      });
    },
    onMutate: (eventId) => settleAttend(eventId, 'pending'),
    onError: (_error, eventId) => settleAttend(eventId, 'failed'),
    onSuccess: (rsvp, eventId) => {
      settleAttend(eventId);
      queryClient.setQueryData<SocialRsvpDTO | null>(eventRsvpQueryKeys.mine(eventId, session?.partyId), rsvp);
      void queryClient.invalidateQueries({ queryKey: eventRsvpQueryKeys.summary(eventId) });
      if (session?.partyId) {
        void queryClient.invalidateQueries({ queryKey: eventRsvpQueryKeys.feed(String(session.partyId)) });
      }
    },
  });

  if (!enabled || events.length === 0) return null;

  const dismiss = () => {
    setDismissed(true);
    try { window.sessionStorage.setItem(DISMISS_KEY, '1'); } catch { /* per-session convenience only */ }
  };

  return (
    <Box component="section" aria-labelledby="upcoming-events-carousel-title" sx={{ mb: 2 }}>
      <Stack direction="row" alignItems="center" spacing={1} sx={{ mb: 1 }}>
        <Typography id="upcoming-events-carousel-title" variant="subtitle1" fontWeight={700} sx={{ flex: 1 }}>
          {t('authEntry.eventsCarousel.title')}
        </Typography>
        <Button component={RouterLink} to={EVENTS_PATH} size="small">{t('authEntry.eventsCarousel.viewAll')}</Button>
        <IconButton size="small" aria-label={t('authEntry.eventsCarousel.dismiss')} onClick={dismiss}>
          <CloseIcon fontSize="small" />
        </IconButton>
      </Stack>
      <Box
        role="list"
        sx={{
          display: 'grid',
          gridAutoFlow: 'column',
          gridAutoColumns: { xs: '72%', sm: '240px' },
          gap: 1.5,
          overflowX: 'auto',
          scrollSnapType: 'x mandatory',
          pb: 1,
        }}
      >
        {events.map((event: PublicUpcomingEventDTO, index) => {
          const rsvpQuery = rsvpQueries[index];
          // Until the viewer's answer is known, one tap could overwrite a "maybe" with "going".
          const rsvpKnown = rsvpQuery?.isSuccess === true && !rsvpQuery.isFetching;
          const status = rsvpQuery?.data?.rsvpStatus;
          const pending = attendState[event.publicUpcomingEventId] === 'pending';
          const failed = attendState[event.publicUpcomingEventId] === 'failed';
          return (
            <Card key={event.publicUpcomingEventId} role="listitem" variant="outlined" sx={{ scrollSnapAlign: 'start' }}>
              <CardActionArea component={RouterLink} to={`/eventos/${encodeURIComponent(event.publicUpcomingEventId)}`}>
                <CardMedia component="img" height="96" image={resolveImageUrl(event.publicUpcomingEventImageUrl)} alt="" />
                <Box sx={{ p: 1.25 }}>
                  <Typography variant="body2" fontWeight={700} noWrap title={event.publicUpcomingEventTitle}>
                    {event.publicUpcomingEventTitle}
                  </Typography>
                  <Typography variant="caption" color="text.secondary" component="p" noWrap>
                    {formatStart(event.publicUpcomingEventStart, i18n.language, event.publicUpcomingEventTimezone) ?? t('authEntry.eventsCarousel.dateTbc')}
                    {event.publicUpcomingEventVenueName ? ` · ${event.publicUpcomingEventVenueName}` : ''}
                  </Typography>
                </Box>
              </CardActionArea>
              <Box sx={{ px: 1.25, pb: 1.25 }}>
                {status ? (
                  <Chip size="small" color={status === 'accepted' ? 'success' : 'default'} label={t(rsvpLabelKey[status])} />
                ) : (
                  <Button
                    size="small"
                    variant="outlined"
                    disabled={pending || !rsvpKnown}
                    onClick={() => attend.mutate(event.publicUpcomingEventId)}
                  >
                    {t(failed ? 'authEntry.eventsCarousel.retry' : 'authEntry.eventsCarousel.attend')}
                  </Button>
                )}
                {failed && (
                  <Alert severity="error" sx={{ mt: 1, py: 0, '& .MuiAlert-message': { fontSize: '0.75rem' } }}>
                    {t('authEntry.eventsCarousel.attendFailed')}
                  </Alert>
                )}
              </Box>
            </Card>
          );
        })}
      </Box>
    </Box>
  );
}
