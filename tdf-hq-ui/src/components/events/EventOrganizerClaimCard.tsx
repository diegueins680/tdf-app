import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, Card, CardContent, CircularProgress, Stack, TextField, Typography } from '@mui/material';
import { ApiError } from '../../api/client';
import {
  SocialEventsAPI,
  type EventOrganizerClaimStateDTO,
} from '../../api/socialEvents';
import { EVENT_ORGANIZER_CLAIM_EVIDENCE_MIN_LENGTH, EVENT_ORGANIZER_CLAIM_TEXT_MAX_LENGTH } from '../../utils/eventOrganizerClaimLimits';

const statusOf = (error: unknown) => (error instanceof ApiError ? error.status : null);

interface EventOrganizerClaimCardProps {
  eventId: string;
  /** Separates cached claim states between accounts on a shared device. */
  accountScope: string;
  formatDate: (value: string) => string;
}

// EVT-ORGANIZER-CLAIM-001: an event without an organizer is claimed like an
// artist profile without an account. The request grants nothing; a strict
// administrator reviews the evidence before the event is assigned.
export default function EventOrganizerClaimCard({ eventId, accountScope, formatDate }: EventOrganizerClaimCardProps) {
  const queryClient = useQueryClient();
  const stateKey = ['social-event-organizer-claim', eventId, accountScope];
  const [evidence, setEvidence] = useState('');
  const stateQuery = useQuery({
    queryKey: stateKey,
    queryFn: () => SocialEventsAPI.getEventOrganizerClaim(eventId),
    enabled: Boolean(eventId),
    retry: false,
  });
  const submitMutation = useMutation({
    mutationFn: (text: string) => SocialEventsAPI.submitEventOrganizerClaim(eventId, text),
    onSuccess: (claim) => {
      queryClient.setQueryData<EventOrganizerClaimStateDTO>(stateKey, (current) => ({
        eocsEventId: current?.eocsEventId ?? claim.eocEventId,
        eocsClaimable: current?.eocsClaimable ?? true,
        eocsClaim: claim,
      }));
      setEvidence('');
    },
    // A 409 means the state on screen is stale: the event was assigned meanwhile.
    onError: (error) => { if (statusOf(error) === 409) void stateQuery.refetch(); },
  });

  const state = stateQuery.data;
  const claim = state?.eocsClaim ?? null;
  const trimmed = evidence.trim();
  const canSubmit = trimmed.length >= EVENT_ORGANIZER_CLAIM_EVIDENCE_MIN_LENGTH
    && trimmed.length <= EVENT_ORGANIZER_CLAIM_TEXT_MAX_LENGTH && !submitMutation.isPending;
  const submitError = submitMutation.error
    ? statusOf(submitMutation.error) === 409
      ? 'No se pudo registrar la solicitud: el evento ya tiene quien lo organice o tienes demasiadas solicitudes pendientes.'
      : statusOf(submitMutation.error) === 400
        ? `Describe tu relación con el evento en ${EVENT_ORGANIZER_CLAIM_EVIDENCE_MIN_LENGTH} a ${EVENT_ORGANIZER_CLAIM_TEXT_MAX_LENGTH} caracteres.`
        : 'No pudimos confirmar el envío. Puedes reintentar sin cerrar tu sesión.'
    : null;

  // Once the event has an organizer there is nothing left to claim here.
  if (state && !state.eocsClaimable) return null;

  return <Card variant="outlined" component="section" aria-labelledby="event-organizer-claim-title">
    <CardContent><Stack spacing={1.5}>
      <Typography id="event-organizer-claim-title" component="h2" variant="h6">Este evento no tiene quien lo administre</Typography>
      <Typography variant="body2" color="text.secondary">
        Si lo organizas tú, solicita su administración. El equipo revisará tus pruebas antes de asignártelo;
        mientras tanto nada cambia en el evento ni en tu cuenta.
      </Typography>
      {stateQuery.isLoading && <CircularProgress size={22} aria-label="Cargando el estado de tu solicitud" />}
      {stateQuery.isError && <Alert severity="error" action={<Button color="inherit" size="small" onClick={() => { void stateQuery.refetch(); }}>Reintentar</Button>}>
        No se pudo cargar el estado de tu solicitud.
      </Alert>}
      {state && claim?.eocStatus === 'submitted' && <Alert severity="success" role="status">
        Solicitud registrada el {formatDate(claim.eocSubmittedAt)}. Está en revisión: te asignaremos el evento solo después de una aprobación verificada.
      </Alert>}
      {state && claim?.eocStatus !== 'submitted' && <>
        {claim?.eocStatus === 'rejected' && <Alert severity="warning">
          Tu solicitud anterior no fue aprobada. Puedes enviar una nueva con más pruebas.
        </Alert>}
        {submitError && <Alert severity="error">{submitError}</Alert>}
        <Stack component="form" spacing={1.5} onSubmit={(submit) => { submit.preventDefault(); if (canSubmit) submitMutation.mutate(trimmed); }}>
          <TextField
            label="Pruebas de que organizas este evento"
            multiline
            minRows={3}
            value={evidence}
            onChange={(change) => setEvidence(change.target.value)}
            disabled={submitMutation.isPending}
            inputProps={{ maxLength: EVENT_ORGANIZER_CLAIM_TEXT_MAX_LENGTH }}
            helperText={`Incluye enlaces oficiales (afiche, publicación del local, página de entradas) y explica tu relación con el evento (${EVENT_ORGANIZER_CLAIM_EVIDENCE_MIN_LENGTH}–${EVENT_ORGANIZER_CLAIM_TEXT_MAX_LENGTH} caracteres). No envíes contraseñas ni documentos sensibles.`}
          />
          <Button type="submit" variant="contained" disabled={!canSubmit} sx={{ alignSelf: 'flex-start' }}>
            {submitMutation.isPending ? 'Enviando…' : 'Solicitar administración del evento'}
          </Button>
        </Stack>
      </>}
    </Stack></CardContent>
  </Card>;
}
