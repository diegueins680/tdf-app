import { useId } from 'react';
import { useQuery } from '@tanstack/react-query';
import AddIcon from '@mui/icons-material/Add';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import { Alert, Box, Button, Card, Chip, CircularProgress, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { useTranslation } from 'react-i18next';

import { SocialEventsAPI, type SocialEventDTO } from '../../api/socialEvents';

export const MY_EVENTS_CREATE_PATH = '/social/eventos/nuevo';
export const MY_EVENTS_MANAGE_PATH = '/social/eventos';
// The dashboard previews the next few; the events page carries the full list.
const PREVIEW_LIMIT = 3;
// Largest page the events list serves. Ownership is matched on the client
// because the list has no organizer filter yet.
const LOOKUP_LIMIT = 500;

const formatStart = (value: string, locale: string, timeZone?: string | null): string | null => {
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) return null;
  const format = (zone?: string) => new Intl.DateTimeFormat(locale, {
    weekday: 'short', day: 'numeric', month: 'short', hour: '2-digit', minute: '2-digit', hourCycle: 'h23', timeZone: zone,
  }).format(parsed);
  try {
    return format(timeZone ?? undefined);
  } catch {
    // An unknown timezone name must not hide the date.
    try { return format(); } catch { return null; }
  }
};

type OwnedEvent = SocialEventDTO & { eventId: string };

/**
 * Event creation and management entry on the signed-in home: the two primary
 * actions plus the next events the viewer organizes. The actions never depend
 * on the list loading.
 */
export default function MyEventsPanel({ partyId }: { partyId: string | number }) {
  const { t, i18n } = useTranslation();
  const headingId = useId();
  const owner = String(partyId);
  const eventsQuery = useQuery({
    queryKey: ['dashboard-my-events', owner],
    queryFn: ({ signal }) => SocialEventsAPI.listEvents({
      startAfter: new Date().toISOString(),
      limit: LOOKUP_LIMIT,
      signal,
    }),
    staleTime: 60_000,
  });
  const owned = (eventsQuery.data ?? []).filter((event): event is OwnedEvent =>
    Boolean(event.eventId) && event.eventOrganizerPartyId != null && String(event.eventOrganizerPartyId) === owner);
  const preview = owned.slice(0, PREVIEW_LIMIT);
  const remaining = owned.length - preview.length;
  const stateName = (event: SocialEventDTO) => (i18n.language?.startsWith('en')
    ? event.eventWorkflowStateNameEn ?? event.eventWorkflowStateNameEs
    : event.eventWorkflowStateNameEs ?? event.eventWorkflowStateNameEn);

  return (
    <Card
      component="section"
      aria-labelledby={headingId}
      variant="outlined"
      data-testid="my-events-panel"
      sx={{
        p: { xs: 2, md: 3 },
        borderRadius: 3,
        background: 'linear-gradient(135deg, rgba(236,72,153,0.08), rgba(99,102,241,0.08))',
      }}
    >
      <Stack spacing={2}>
        <Stack direction={{ xs: 'column', md: 'row' }} spacing={2} justifyContent="space-between" alignItems={{ md: 'center' }}>
          <Stack spacing={0.5} sx={{ minWidth: 0 }}>
            <Stack direction="row" spacing={1} alignItems="center">
              <EventAvailableIcon color="secondary" />
              <Typography id={headingId} variant="h5" component="h2" fontWeight={700} color="text.primary">
                {t('eventDashboard.title')}
              </Typography>
            </Stack>
            <Typography variant="body2" color="text.secondary">
              {t('eventDashboard.subtitle')}
            </Typography>
          </Stack>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} sx={{ flexShrink: 0 }}>
            <Button component={RouterLink} to={MY_EVENTS_CREATE_PATH} variant="contained" color="secondary" startIcon={<AddIcon />}>
              {t('eventDashboard.create')}
            </Button>
            <Button component={RouterLink} to={MY_EVENTS_MANAGE_PATH} variant="outlined" color="secondary">
              {t('eventDashboard.manageAll')}
            </Button>
          </Stack>
        </Stack>

        {eventsQuery.isLoading && <CircularProgress size={20} aria-label={t('eventDashboard.loading')} />}
        {eventsQuery.isError && (
          <Alert
            severity="warning"
            action={<Button color="inherit" size="small" onClick={() => void eventsQuery.refetch()}>{t('eventDashboard.retry')}</Button>}
          >
            {t('eventDashboard.error')}
          </Alert>
        )}
        {eventsQuery.isSuccess && owned.length === 0 && (
          <Typography variant="body2" color="text.secondary">{t('eventDashboard.empty')}</Typography>
        )}
        {preview.length > 0 && (
          <Stack spacing={1}>
            <Typography variant="subtitle2" component="h3" color="text.secondary">{t('eventDashboard.upcoming')}</Typography>
            <Stack component="ul" spacing={1} sx={{ listStyle: 'none', m: 0, p: 0 }}>
              {preview.map((event) => {
                const state = stateName(event);
                return (
                  <Box
                    component="li"
                    key={event.eventId}
                    sx={{
                      display: 'flex',
                      flexDirection: { xs: 'column', sm: 'row' },
                      alignItems: { sm: 'center' },
                      justifyContent: 'space-between',
                      gap: 1,
                      p: 1.5,
                      borderRadius: 2,
                      border: '1px solid',
                      borderColor: 'divider',
                      bgcolor: 'background.paper',
                    }}
                  >
                    <Box sx={{ minWidth: 0 }}>
                      <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap" useFlexGap>
                        <Typography variant="subtitle1" fontWeight={700} color="text.primary" sx={{ overflowWrap: 'anywhere' }}>
                          {event.eventTitle}
                        </Typography>
                        {state && <Chip size="small" label={state} />}
                      </Stack>
                      <Typography variant="body2" color="text.secondary">
                        {formatStart(event.eventStart, i18n.language ?? 'es', event.eventTimezone) ?? t('eventDashboard.dateTbc')}
                      </Typography>
                    </Box>
                    <Stack direction="row" spacing={1} sx={{ flexShrink: 0 }}>
                      <Button
                        component={RouterLink}
                        to={`${MY_EVENTS_MANAGE_PATH}/${encodeURIComponent(event.eventId)}`}
                        size="small"
                        variant="contained"
                        aria-label={t('eventDashboard.manageNamed', { title: event.eventTitle })}
                      >
                        {t('eventDashboard.manage')}
                      </Button>
                      <Button
                        component={RouterLink}
                        to={`${MY_EVENTS_MANAGE_PATH}/${encodeURIComponent(event.eventId)}/logistica`}
                        size="small"
                        variant="text"
                        aria-label={t('eventDashboard.logisticsNamed', { title: event.eventTitle })}
                      >
                        {t('eventDashboard.logistics')}
                      </Button>
                    </Stack>
                  </Box>
                );
              })}
            </Stack>
            {remaining > 0 && (
              <Typography variant="body2" color="text.secondary">{t('eventDashboard.more', { count: remaining })}</Typography>
            )}
          </Stack>
        )}
      </Stack>
    </Card>
  );
}
