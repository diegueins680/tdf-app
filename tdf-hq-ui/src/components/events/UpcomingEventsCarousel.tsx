import { useMemo, useState } from 'react';
import { useMutation, useQueries, useQuery, useQueryClient } from '@tanstack/react-query';
import CloseIcon from '@mui/icons-material/Close';
import { Box, Button, Card, CardActionArea, CardMedia, Chip, IconButton, Stack, Typography } from '@mui/material';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { DateTime } from 'luxon';
import { API_BASE_URL } from '../../api/client';
import {
  SocialEventsAPI,
  type PublicUpcomingEventDTO,
  type SocialRsvpDTO,
  type SocialRsvpStatus,
} from '../../api/socialEvents';
import { useSession } from '../../session/SessionContext';

const CAROUSEL_LIMIT = 8;
const DISMISS_KEY = 'tdf.upcomingEventsCarousel.dismissed';
const EVENT_IMAGE_FALLBACK = '/event-fallback.svg';
export const EVENTS_PATH = '/social/eventos';

const rsvpLabel: Record<SocialRsvpStatus, string> = {
  accepted: 'Vas',
  maybe: 'Quizás',
  declined: 'No vas',
};

const resolveImageUrl = (value: string | null | undefined): string => {
  if (!value) return EVENT_IMAGE_FALLBACK;
  try { return new URL(value, API_BASE_URL || window.location.origin).toString(); } catch { return EVENT_IMAGE_FALLBACK; }
};

const formatStart = (value: string) => {
  const parsed = DateTime.fromISO(value).setLocale('es');
  return parsed.isValid ? parsed.toFormat("ccc d LLL, HH:mm") : 'Fecha por confirmar';
};

const readDismissed = () => {
  try { return window.sessionStorage.getItem(DISMISS_KEY) === '1'; } catch { return false; }
};

// Shown to every signed-in user right after login: the next public events and
// the viewer's own RSVP for each, with a one-tap "Asistiré".
export default function UpcomingEventsCarousel() {
  const { session } = useSession();
  const location = useLocation();
  const queryClient = useQueryClient();
  const [dismissed, setDismissed] = useState(readDismissed);
  const startAfter = useMemo(() => new Date().toISOString(), []);
  const onEventsPage = location.pathname === EVENTS_PATH || location.pathname.startsWith(`${EVENTS_PATH}/`);
  const enabled = Boolean(session) && !dismissed && !onEventsPage;

  const eventsQuery = useQuery({
    queryKey: ['upcoming-events-carousel', startAfter],
    queryFn: ({ signal }) => SocialEventsAPI.listPublicUpcomingEvents({ startAfter, limit: CAROUSEL_LIMIT, signal }),
    enabled,
    staleTime: 5 * 60_000,
  });
  const events = eventsQuery.data ?? [];

  const rsvpQueries = useQueries({
    queries: events.map((event) => ({
      queryKey: ['my-rsvp', session?.partyId, event.publicUpcomingEventId],
      queryFn: () => SocialEventsAPI.getMyRsvp(event.publicUpcomingEventId),
      enabled,
      staleTime: 60_000,
    })),
  });

  const attend = useMutation({
    mutationFn: (eventId: string) => SocialEventsAPI.upsertMyRsvp(eventId, {
      rsvpStatus: 'accepted',
      rsvpShowOnProfile: session?.preferences?.showEventRsvpsOnProfile ?? true,
    }),
    onSuccess: (rsvp, eventId) => {
      queryClient.setQueryData<SocialRsvpDTO | null>(['my-rsvp', session?.partyId, eventId], rsvp);
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
          Próximos eventos
        </Typography>
        <Button component={RouterLink} to={EVENTS_PATH} size="small">Ver todos</Button>
        <IconButton size="small" aria-label="Ocultar próximos eventos" onClick={dismiss}>
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
          const rsvp = rsvpQueries[index]?.data ?? null;
          const status = rsvp?.rsvpStatus;
          const pending = attend.isPending && attend.variables === event.publicUpcomingEventId;
          return (
            <Card key={event.publicUpcomingEventId} role="listitem" variant="outlined" sx={{ scrollSnapAlign: 'start' }}>
              <CardActionArea component={RouterLink} to={`/eventos/${encodeURIComponent(event.publicUpcomingEventId)}`}>
                <CardMedia component="img" height="96" image={resolveImageUrl(event.publicUpcomingEventImageUrl)} alt="" />
                <Box sx={{ p: 1.25 }}>
                  <Typography variant="body2" fontWeight={700} noWrap title={event.publicUpcomingEventTitle}>
                    {event.publicUpcomingEventTitle}
                  </Typography>
                  <Typography variant="caption" color="text.secondary" component="p" noWrap>
                    {formatStart(event.publicUpcomingEventStart)}
                    {event.publicUpcomingEventVenueName ? ` · ${event.publicUpcomingEventVenueName}` : ''}
                  </Typography>
                </Box>
              </CardActionArea>
              <Box sx={{ px: 1.25, pb: 1.25 }}>
                {status && status !== 'declined' ? (
                  <Chip size="small" color={status === 'accepted' ? 'success' : 'default'} label={rsvpLabel[status]} />
                ) : (
                  <Button
                    size="small"
                    variant="outlined"
                    disabled={pending}
                    onClick={() => attend.mutate(event.publicUpcomingEventId)}
                  >
                    Asistiré
                  </Button>
                )}
              </Box>
            </Card>
          );
        })}
      </Box>
    </Box>
  );
}
