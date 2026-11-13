import { useEffect, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, CircularProgress, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { Catalogs, type WorkflowState } from '../../api/catalogs';
import { SocialEventsAPI, type SocialEventDTO } from '../../api/socialEvents';
import { captureGrowthEvent } from '../../analytics/growthAttribution';
import { getAnalyticsClient } from '../../analytics/posthog';

const PUBLIC_LISTABLE = 'public-listable';

/**
 * What publishing has to change for this event, or null when the organizer
 * cannot publish it from its current state (cancelled, finished, under review).
 * An empty object means the state is already public and only visibility is off.
 */
export function resolvePublishTarget(
  event: Pick<SocialEventDTO, 'eventWorkflowStateId'>,
  states: readonly WorkflowState[],
): { workflowStateId?: string } | null {
  const current = states.find((state) => state.id === event.eventWorkflowStateId);
  if (!current?.active) return null;
  if (current.capabilities.includes(PUBLIC_LISTABLE)) return {};
  const target = current.transitions
    .filter((transition) => transition.directExecutionAllowed && !transition.requiresReview)
    .flatMap((transition) => states.filter((state) => state.id === transition.toStateId))
    .filter((state) => state.active && state.capabilities.includes(PUBLIC_LISTABLE))
    .sort((a, b) => a.sortOrder - b.sortOrder)[0];
  return target ? { workflowStateId: target.id } : null;
}

export default function EventPublishControl({ event }: { event: SocialEventDTO }) {
  const eventId = event.eventId!;
  const queryClient = useQueryClient();
  const mounted = useRef(true);
  useEffect(() => { mounted.current = true; return () => { mounted.current = false; }; }, []);
  const [confirming, setConfirming] = useState(false);
  const published = event.eventIsPublic === true && event.eventPublicListable === true;
  const statesQuery = useQuery({
    // Only ids, capabilities and transitions are read: no localized names.
    queryKey: ['catalog', 'workflow-states', 'social-event-lifecycle', 'transitions'],
    queryFn: () => Catalogs.getPublicWorkflowStates('social-event-lifecycle'),
    staleTime: 5 * 60 * 1000,
    enabled: !published,
  });
  const states = statesQuery.data?.states ?? [];
  const target = published ? null : resolvePublishTarget(event, states);
  const mutation = useMutation({
    mutationFn: async () => {
      // Read current fields immediately before the existing full-event PUT, so
      // publishing never writes back a title, date or lineup that someone else changed.
      const latest = await SocialEventsAPI.getEvent(eventId);
      const latestTarget = resolvePublishTarget(latest, states);
      if (!latestTarget) throw new Error('El estado del evento cambió. Recarga la página antes de publicar.');
      return SocialEventsAPI.updateEvent(eventId, {
        eventTitle: latest.eventTitle,
        eventStart: latest.eventStart,
        eventEnd: latest.eventEnd,
        eventDescription: latest.eventDescription,
        eventTimezone: latest.eventTimezone,
        eventVenueId: latest.eventVenueId,
        eventPriceCents: latest.eventPriceCents,
        eventCapacity: latest.eventCapacity,
        eventTypeId: latest.eventTypeId,
        eventArtists: latest.eventArtists.flatMap((artist) => artist.artistId ? [{ artistId: artist.artistId, artistName: '' }] : []),
        eventIsPublic: true,
        ...(latestTarget.workflowStateId ? { eventWorkflowStateId: latestTarget.workflowStateId } : {}),
      });
    },
    onSuccess: () => {
      try {
        captureGrowthEvent(getAnalyticsClient(), 'event_published', { platform: 'web', event_id: eventId });
      } catch {
        // Optional measurement must not interrupt publishing.
      }
      void queryClient.invalidateQueries({ queryKey: ['social-event', eventId] });
      void queryClient.invalidateQueries({ queryKey: ['social-events'] });
      void queryClient.invalidateQueries({ queryKey: ['public-upcoming-events'] });
      void queryClient.invalidateQueries({ queryKey: ['upcoming-events-carousel'] });
      if (mounted.current) setConfirming(false);
    },
  });

  if (published) {
    return (
      <Alert
        severity="success"
        action={<Button component={RouterLink} to={`/eventos/${encodeURIComponent(eventId)}`} color="inherit" size="small">Ver página pública</Button>}
      >
        Tu evento es público: cualquier persona puede verlo y confirmar asistencia.
      </Alert>
    );
  }
  if (!target) return null;
  return (
    <Stack spacing={1} component="section" aria-labelledby="event-publish-title">
      <Typography id="event-publish-title" component="h3" variant="subtitle1" fontWeight={700}>Publicar el evento</Typography>
      <Typography variant="body2" color="text.secondary">
        Hoy solo tú y tu equipo pueden verlo. Al publicarlo tendrá una página pública para compartir y aceptará confirmaciones de asistencia.
      </Typography>
      {mutation.isError && (
        <Alert severity="error">{mutation.error instanceof Error ? mutation.error.message : 'No se pudo publicar. Inténtalo de nuevo.'}</Alert>
      )}
      {confirming ? (
        <Alert
          severity="warning"
          action={(
            <Stack direction="row" spacing={1}>
              <Button color="inherit" size="small" disabled={mutation.isPending} onClick={() => { mutation.reset(); setConfirming(false); }}>Cancelar</Button>
              <Button color="inherit" size="small" variant="outlined" disabled={mutation.isPending} onClick={() => mutation.mutate()}>
                {mutation.isPending ? <><CircularProgress size={14} sx={{ mr: 1 }} color="inherit" />Publicando…</> : 'Publicar ahora'}
              </Button>
            </Stack>
          )}
        >
          Cualquier persona podrá ver el nombre, la fecha, el lugar y el afiche. Revísalos antes de publicar.
        </Alert>
      ) : (
        <Stack direction="row"><Button variant="contained" color="secondary" onClick={() => setConfirming(true)}>Publicar evento</Button></Stack>
      )}
    </Stack>
  );
}
