import { useEffect, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, CircularProgress, Dialog, DialogActions, DialogContent, DialogContentText, DialogTitle, Stack } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { Catalogs, type WorkflowState } from '../../api/catalogs';
import { SocialEventsAPI, type SocialEventDTO } from '../../api/socialEvents';
import { captureGrowthEvent } from '../../analytics/growthAttribution';
import { getAnalyticsClient } from '../../analytics/posthog';
import { getAuthSessionEpoch } from '../../auth/authEvents';

const PUBLIC_LISTABLE = 'public-listable';

/**
 * What publishing has to change for this event, or null when the organizer
 * cannot publish it from its current state (cancelled, finished, under review).
 * An empty object means the state is already public and only visibility is off.
 */
export function resolvePublishTarget(
  event: Pick<SocialEventDTO, 'eventWorkflowStateId'>,
  states: readonly WorkflowState[],
  now: number = Date.now(),
): { workflowStateId?: string } | null {
  const current = states.find((state) => state.id === event.eventWorkflowStateId);
  if (!current?.active) return null;
  if (current.capabilities.includes(PUBLIC_LISTABLE)) return {};
  // The server only accepts a transition inside its effective window, so one
  // that has not started or has already ended is not a way to publish.
  const inEffect = (transition: WorkflowState['transitions'][number]) =>
    (!transition.effectiveFrom || Date.parse(transition.effectiveFrom) <= now)
    && (!transition.effectiveUntil || Date.parse(transition.effectiveUntil) > now);
  const target = current.transitions
    .filter((transition) => transition.directExecutionAllowed && !transition.requiresReview && inEffect(transition))
    .flatMap((transition) => states.filter((state) => state.id === transition.toStateId))
    .filter((state) => state.active && state.capabilities.includes(PUBLIC_LISTABLE))
    .sort((a, b) => a.sortOrder - b.sortOrder)[0];
  return target ? { workflowStateId: target.id } : null;
}

interface EventPublishControlProps {
  event: SocialEventDTO;
  promptAfterSave?: boolean;
  artUploadFailed?: boolean;
  onPublished?: (event: SocialEventDTO) => void;
}

export default function EventPublishControl({ event, promptAfterSave = false, artUploadFailed = false, onPublished }: EventPublishControlProps) {
  const eventId = event.eventId!;
  const queryClient = useQueryClient();
  const mounted = useRef(true);
  useEffect(() => { mounted.current = true; return () => { mounted.current = false; }; }, []);
  const [confirming, setConfirming] = useState(promptAfterSave);
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
      // Each request is authorized with whichever account is signed in when it
      // is sent, so the publication must stay with the account that confirmed it.
      const session = getAuthSessionEpoch();
      const ensureSameSession = () => {
        if (!mounted.current || session !== getAuthSessionEpoch()) {
          throw new Error('Tu sesión cambió. Vuelve al evento antes de publicar.');
        }
      };
      ensureSameSession();
      const latest = await SocialEventsAPI.getEvent(eventId);
      ensureSameSession();
      if (latest.eventIsPublic && latest.eventPublicListable) return latest;
      const latestTarget = resolvePublishTarget(latest, states);
      if (!latestTarget) throw new Error('El estado del evento cambió. Recarga la página antes de publicar.');
      const updated = await SocialEventsAPI.updateEvent(eventId, {
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
      ensureSameSession();
      if (!updated.eventIsPublic || !updated.eventPublicListable) {
        throw new Error('No se pudo confirmar la publicación. Revisa el evento e inténtalo de nuevo.');
      }
      return updated;
    },
    onSuccess: (updated) => {
      try {
        captureGrowthEvent(getAnalyticsClient(), 'event_published', { platform: 'web', event_id: eventId });
      } catch {
        // Optional measurement must not interrupt publishing.
      }
      void queryClient.invalidateQueries({ queryKey: ['social-event', eventId] });
      void queryClient.invalidateQueries({ queryKey: ['social-events'] });
      void queryClient.invalidateQueries({ queryKey: ['public-upcoming-events'] });
      void queryClient.invalidateQueries({ queryKey: ['upcoming-events-carousel'] });
      void queryClient.invalidateQueries({ queryKey: ['dashboard-my-events'] });
      if (mounted.current) {
        setConfirming(false);
        onPublished?.(updated);
      }
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
  if (!target && !confirming) return null;
  const dismiss = () => {
    if (mutation.isPending) return;
    mutation.reset();
    setConfirming(false);
  };
  return (
    <>
      {target && (
        <Button variant="contained" color="primary" sx={{ alignSelf: 'flex-start' }} onClick={() => setConfirming(true)}>
          Publicar evento
        </Button>
      )}
      <Dialog
        open={confirming}
        onClose={dismiss}
        disableEscapeKeyDown={mutation.isPending}
        aria-labelledby="event-publish-title"
        aria-describedby="event-publish-description"
        fullWidth
        maxWidth="xs"
      >
        <DialogTitle id="event-publish-title">¿Publicar el evento?</DialogTitle>
        <DialogContent>
          <Stack spacing={2}>
            {promptAfterSave && <DialogContentText>El evento se guardó en planificación.</DialogContentText>}
            <DialogContentText id="event-publish-description">
              Cualquier persona podrá ver el nombre, la fecha, el lugar y el afiche. El evento aparecerá en el catálogo y podrá recibir confirmaciones de asistencia.
            </DialogContentText>
            {artUploadFailed && <Alert severity="warning">El afiche aún no se ha subido. Puedes agregarlo desde el evento.</Alert>}
            {statesQuery.isPending && <CircularProgress size={20} aria-label="Comprobando si el evento se puede publicar" />}
            {statesQuery.isError && (
              <Alert severity="error" action={<Button color="inherit" onClick={() => { void statesQuery.refetch(); }}>Reintentar</Button>}>
                No se pudo comprobar si el evento se puede publicar.
              </Alert>
            )}
            {statesQuery.isSuccess && !target && <Alert severity="info">La publicación no está disponible para el estado actual del evento.</Alert>}
            {mutation.isError && (
              <Alert severity="error">{mutation.error instanceof Error ? mutation.error.message : 'No se pudo publicar. Inténtalo de nuevo.'}</Alert>
            )}
          </Stack>
        </DialogContent>
        <DialogActions sx={{ flexWrap: 'wrap' }}>
          <Button disabled={mutation.isPending} onClick={dismiss}>
            {promptAfterSave ? 'Seguir en planificación' : 'Cancelar'}
          </Button>
          <Button
            variant="contained"
            disabled={!target || mutation.isPending}
            onClick={() => mutation.mutate()}
          >
            {mutation.isPending ? <><CircularProgress size={14} sx={{ mr: 1 }} color="inherit" />Publicando…</> : 'Publicar ahora'}
          </Button>
        </DialogActions>
      </Dialog>
    </>
  );
}
