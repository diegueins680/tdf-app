import { useMemo, useState } from 'react';
import { useMutation, useQuery, useQueryClient, useQueries } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Divider,
  Grid,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import CalendarMonthIcon from '@mui/icons-material/CalendarMonth';
import PersonAddAltIcon from '@mui/icons-material/PersonAddAlt';
import CheckIcon from '@mui/icons-material/Check';
import ClearIcon from '@mui/icons-material/Clear';
import HelpOutlineIcon from '@mui/icons-material/HelpOutline';
import RefreshIcon from '@mui/icons-material/Refresh';
import { DateTime } from 'luxon';
import { SocialEventsAPI, type SocialInvitationDTO } from '../api/socialEvents';
import { useSession } from '../session/SessionContext';

type InvitationState = { partyId: string; message: string };

const formatDate = (iso: string) =>
  DateTime.fromISO(iso).setLocale('es').toFormat("EEE d LLL, HH:mm");

export default function SocialEventsPage() {
  const qc = useQueryClient();
  const { session } = useSession();
  const [city, setCity] = useState('');
  const [feedback, setFeedback] = useState<{ kind: 'success' | 'error'; message: string } | null>(null);
  const [invites, setInvites] = useState<Record<string, InvitationState>>({});
  const startAfter = useMemo(() => new Date().toISOString(), []);
  const hasSession = Boolean(session?.partyId);

  const eventsQuery = useQuery({
    queryKey: ['social-events', city, startAfter],
    queryFn: () => SocialEventsAPI.listEvents({ city: city.trim() || undefined, startAfter }),
  });

  const venuesQuery = useQuery({
    queryKey: ['social-venues', city],
    queryFn: () => SocialEventsAPI.listVenues({ city: city.trim() || undefined }),
  });

  const venueById = useMemo(() => {
    const map = new Map<string, string>();
    (venuesQuery.data ?? []).forEach((v) => {
      if (v.venueId) map.set(v.venueId, v.venueName);
    });
    return map;
  }, [venuesQuery.data]);

  const rsvpMutation = useMutation({
    mutationFn: ({ eventId, status }: { eventId: string; status: 'Accepted' | 'Maybe' | 'Declined' }) => {
      if (!session?.partyId) throw new Error('Inicia sesión para confirmar asistencia.');
      return SocialEventsAPI.rsvp(eventId, String(session.partyId), status);
    },
    onSuccess: () => {
      setFeedback({ kind: 'success', message: 'RSVP registrado.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const inviteMutation = useMutation({
    mutationFn: ({ eventId }: { eventId: string }) => {
      if (!session?.partyId) throw new Error('Inicia sesión para enviar invitaciones.');
      const draft = invites[eventId] ?? { partyId: '', message: '' };
      const toId = draft.partyId.trim();
      if (!toId) throw new Error('Ingresa el ID de la persona a invitar.');
      return SocialEventsAPI.sendInvitation(eventId, {
        invitationToPartyId: toId,
        invitationMessage: draft.message.trim() || null,
      });
    },
    onSuccess: (_resp, { eventId }) => {
      setInvites((prev) => ({ ...prev, [eventId]: { partyId: '', message: '' } }));
      void qc.invalidateQueries({ queryKey: ['social-invitations', eventId] });
      setFeedback({ kind: 'success', message: 'Invitación enviada.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const respondInvitationMutation = useMutation({
    mutationFn: ({ eventId, invitationId, status }: { eventId: string; invitationId: string; status: string }) => {
      if (!session?.partyId) throw new Error('Inicia sesión para responder invitaciones.');
      return SocialEventsAPI.respondInvitation(eventId, invitationId, status);
    },
    onSuccess: (_resp, { eventId }) => {
      void qc.invalidateQueries({ queryKey: ['social-invitations', eventId] });
      setFeedback({ kind: 'success', message: 'Respuesta enviada.' });
    },
    onError: (err: Error) => setFeedback({ kind: 'error', message: err.message }),
  });

  const renderEventCard = (
    eventId: string,
    title: string,
    start: string,
    end: string,
    venueName: string | null,
    artists: string[],
    priceCents?: number | null,
    capacity?: number | null,
    description?: string | null,
    invitationStatus?: string | null,
    invitationId?: string | null,
  ) => {
    const inviteDraft = invites[eventId] ?? { partyId: '', message: '' };
    return (
      <Card key={eventId} sx={{ height: '100%', display: 'flex', flexDirection: 'column', borderRadius: 3 }}>
        <CardContent sx={{ display: 'flex', flexDirection: 'column', gap: 1.25, flex: 1 }}>
          <Stack direction="row" spacing={1} alignItems="center" justifyContent="space-between" flexWrap="wrap">
            <Stack direction="row" spacing={1} alignItems="center">
              <Chip icon={<CalendarMonthIcon />} label={formatDate(start)} size="small" />
              <Typography variant="caption" color="text.secondary">
                Termina {formatDate(end)}
              </Typography>
            </Stack>
            {capacity ? <Chip label={`Cupo: ${capacity}`} size="small" /> : null}
          </Stack>
          <Typography variant="h6" fontWeight={800}>{title}</Typography>
          {invitationStatus && (
            <Stack direction="row" spacing={1} alignItems="center" flexWrap="wrap">
              <Chip label={`Invitación: ${invitationStatus}`} size="small" color="secondary" sx={{ width: 'fit-content' }} />
              {invitationId && (
                <>
                  <Button
                    variant="contained"
                    size="small"
                    onClick={() => respondInvitationMutation.mutate({ eventId, invitationId, status: 'Accepted' })}
                    disabled={respondInvitationMutation.isPending}
                  >
                    Aceptar
                  </Button>
                  <Button
                    variant="text"
                    size="small"
                    onClick={() => respondInvitationMutation.mutate({ eventId, invitationId, status: 'Declined' })}
                    disabled={respondInvitationMutation.isPending}
                  >
                    Rechazar
                  </Button>
                </>
              )}
            </Stack>
          )}
          {venueName && (
            <Typography variant="body2" color="text.secondary">
              {venueName}
            </Typography>
          )}
          {artists.length > 0 && (
            <Typography variant="body2" color="text.secondary">
              Artistas: {artists.join(', ')}
            </Typography>
          )}
          {typeof priceCents === 'number' && (
            <Typography variant="body2" color="text.secondary">
              Cover: ${(priceCents / 100).toFixed(2)}
            </Typography>
          )}
          {description && (
            <Typography variant="body2" color="text.secondary">
              {description}
            </Typography>
          )}
          <Stack direction="row" spacing={1} flexWrap="wrap">
            <Button
              variant="contained"
              size="small"
              startIcon={<CheckIcon />}
              onClick={() => rsvpMutation.mutate({ eventId, status: 'Accepted' })}
              disabled={rsvpMutation.isPending || !hasSession}
            >
              Asistiré
            </Button>
            <Button
              variant="outlined"
              size="small"
              startIcon={<HelpOutlineIcon />}
              onClick={() => rsvpMutation.mutate({ eventId, status: 'Maybe' })}
              disabled={rsvpMutation.isPending || !hasSession}
            >
              Tal vez
            </Button>
            <Button
              variant="text"
              size="small"
              startIcon={<ClearIcon />}
              onClick={() => rsvpMutation.mutate({ eventId, status: 'Declined' })}
              disabled={rsvpMutation.isPending || !hasSession}
            >
              No puedo
            </Button>
          </Stack>
          <Divider />
          <Stack spacing={1}>
            <Typography variant="subtitle2" fontWeight={700}>Invitar</Typography>
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
              <TextField
                label="Party ID"
                size="small"
                value={inviteDraft.partyId}
                onChange={(e) => setInvites((prev) => ({ ...prev, [eventId]: { ...inviteDraft, partyId: e.target.value } }))}
                sx={{ flex: 1 }}
              />
              <TextField
                label="Mensaje (opcional)"
                size="small"
                value={inviteDraft.message}
                onChange={(e) => setInvites((prev) => ({ ...prev, [eventId]: { ...inviteDraft, message: e.target.value } }))}
                sx={{ flex: 2 }}
              />
              <Button
                variant="contained"
                size="small"
                startIcon={<PersonAddAltIcon />}
                onClick={() => inviteMutation.mutate({ eventId })}
                disabled={inviteMutation.isPending || !hasSession}
              >
                Enviar
              </Button>
            </Stack>
          </Stack>
        </CardContent>
      </Card>
    );
  };

  const events = eventsQuery.data ?? [];
  const invitationQueries = useQueries({
    queries:
      hasSession && events.length > 0
        ? events.map((ev) => ({
            queryKey: ['social-invitations', ev.eventId ?? ev.eventTitle],
            queryFn: () => SocialEventsAPI.listInvitations(ev.eventId ?? ev.eventTitle),
            enabled: Boolean(ev.eventId ?? ev.eventTitle),
            select: (list: SocialInvitationDTO[]) =>
              list?.filter(
                (inv) => String(inv.invitationToPartyId) === String(session?.partyId ?? ''),
              ) ?? [],
          }))
        : [],
  });

  return (
    <Box>
      <Stack spacing={2} sx={{ mb: 3 }}>
        <Stack direction="row" alignItems="center" spacing={1}>
          <Typography variant="h4" fontWeight={800}>Eventos sociales</Typography>
          <Chip label="Beta" size="small" color="info" />
        </Stack>
        <Typography color="text.secondary">
          Descubre eventos creados en TDF, confirma asistencia y envía invitaciones rápidas a tus contactos.
        </Typography>
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ xs: 'stretch', sm: 'center' }}>
          <TextField
            label="Filtrar por ciudad"
            size="small"
            value={city}
            onChange={(e) => setCity(e.target.value)}
            sx={{ minWidth: 200 }}
          />
          <Button
            variant="outlined"
            startIcon={<RefreshIcon />}
            onClick={() => {
              void eventsQuery.refetch();
              void venuesQuery.refetch();
            }}
            disabled={eventsQuery.isFetching}
          >
            Refrescar
          </Button>
          {!hasSession && (
            <Alert severity="info" sx={{ m: 0, py: 0.5 }}>
              Inicia sesión para confirmar asistencia o enviar invitaciones.
            </Alert>
          )}
        </Stack>
        {feedback && (
          <Alert severity={feedback.kind} onClose={() => setFeedback(null)}>
            {feedback.message}
          </Alert>
        )}
      </Stack>

      {eventsQuery.error ? (
        <Alert severity="error">No pudimos cargar los eventos. Intenta de nuevo.</Alert>
      ) : eventsQuery.isLoading || venuesQuery.isLoading ? (
        <Stack direction="row" spacing={1.5} alignItems="center">
          <CircularProgress size={18} />
          <Typography>Cargando eventos y venues...</Typography>
        </Stack>
      ) : events.length === 0 ? (
        <Alert severity="info">No hay eventos disponibles para este filtro.</Alert>
      ) : (
        <Grid container spacing={2}>
          {events.map((ev, index) => {
            const invitationForMe = invitationQueries[index]?.data?.[0] ?? null;
            return renderEventCard(
              ev.eventId ?? ev.eventTitle,
              ev.eventTitle,
              ev.eventStart,
              ev.eventEnd,
              (ev.eventVenueId && venueById.get(ev.eventVenueId)) || null,
              ev.eventArtists?.map((a) => a.artistName) ?? [],
              ev.eventPriceCents,
              ev.eventCapacity ?? null,
              ev.eventDescription ?? null,
              invitationForMe?.invitationStatus ?? null,
              invitationForMe?.invitationId ?? null,
            );
          })}
        </Grid>
      )}
    </Box>
  );
}
