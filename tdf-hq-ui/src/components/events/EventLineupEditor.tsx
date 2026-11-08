import { useEffect, useMemo, useRef } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, Card, CardContent, Stack, Typography } from '@mui/material';
import { SocialEventsAPI, type SocialEventDTO } from '../../api/socialEvents';
import { useOwnedSessionDraft } from '../../session/useOwnedSessionDraft';
import EventArtistSelector from './EventArtistSelector';

export default function EventLineupEditor({ event, ownerId }: { event: SocialEventDTO; ownerId: number }) {
  const queryClient = useQueryClient();
  const mounted = useRef(true);
  useEffect(() => { mounted.current = true; return () => { mounted.current = false; }; }, []);
  const baseline = useMemo(() => event.eventArtists.flatMap(a => a.artistId ? [a.artistId] : []), [event.eventArtists]);
  const editor = useOwnedSessionDraft({ ownerId, scope: `lineup:${event.eventId}`, baseline, empty: baseline });
  const mutation = useMutation({
    mutationFn: async (attempt: { ids: string[]; isCurrentOwner: () => boolean }) => {
      const ensureOwner = () => { if (!mounted.current || !attempt.isCurrentOwner()) throw new Error('Tu sesión cambió. Vuelve al evento antes de guardar.'); };
      ensureOwner();
      // Read current fields immediately before the existing full-event PUT.
      const latest = await SocialEventsAPI.getEvent(event.eventId!);
      ensureOwner();
      return SocialEventsAPI.updateEvent(event.eventId!, {
        eventTitle: latest.eventTitle,
        eventStart: latest.eventStart,
        eventEnd: latest.eventEnd,
        eventDescription: latest.eventDescription,
        eventTimezone: latest.eventTimezone,
        eventVenueId: latest.eventVenueId,
        eventPriceCents: latest.eventPriceCents,
        eventCapacity: latest.eventCapacity,
        eventTypeId: latest.eventTypeId,
        eventArtists: attempt.ids.map(id => ({ artistId: id, artistName: '' })),
      });
    },
    onSuccess: (_saved, attempt) => {
      if (!mounted.current || !attempt.isCurrentOwner()) return;
      editor.acknowledgeSave(attempt.ids);
      void queryClient.invalidateQueries({ queryKey: ['social-event', event.eventId] });
      void queryClient.invalidateQueries({ queryKey: ['social-events'] });
      void queryClient.invalidateQueries({ queryKey: ['public-upcoming-events'] });
    },
  });
  return <Card variant="outlined" component="section" aria-label="Artistas del evento">
    <CardContent><Stack spacing={1.5}>
      <Typography component="h2" variant="h6">Artistas del evento</Typography>
      <Typography variant="body2">Selecciona a quienes se presentan. Esta lista permite encontrar el evento al filtrar por artista.</Typography>
      <EventArtistSelector artistIds={editor.draft} onChange={editor.setDraft} label="Artistas que se presentan" />
      <Button variant="outlined" disabled={!editor.ready || mutation.isPending}
        onClick={() => mutation.mutate({ ids: editor.draft, isCurrentOwner: editor.isCurrentOwner })}>Guardar artistas</Button>
      {mutation.isError && <Alert severity="error">{mutation.error instanceof Error ? mutation.error.message : 'No se pudo guardar la lista de artistas.'}</Alert>}
      {mutation.isSuccess && <Alert severity="success">Artistas guardados.</Alert>}
    </Stack></CardContent>
  </Card>;
}
