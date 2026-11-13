import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Card, CardActions, CardContent, Chip, CircularProgress, Link, Stack, TextField, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { ApiError } from '../../api/client';
import {
  SocialEventsAPI,
  type EventOrganizerClaimDecision,
  type EventOrganizerClaimReviewDTO,
  type EventOrganizerClaimStatus,
} from '../../api/socialEvents';
import { EVENT_ORGANIZER_CLAIM_NOTES_MIN_LENGTH, EVENT_ORGANIZER_CLAIM_TEXT_MAX_LENGTH } from '../../utils/eventOrganizerClaimLimits';

const queueKey = ['social-event-organizer-claims', 'admin'];

const statusChip: Record<EventOrganizerClaimStatus, { label: string; color: 'warning' | 'success' | 'default' }> = {
  submitted: { label: 'Pendiente', color: 'warning' },
  approved: { label: 'Aprobada', color: 'success' },
  rejected: { label: 'Rechazada', color: 'default' },
  superseded: { label: 'Cerrada: se aprobó otra solicitud', color: 'default' },
};

const formatInstant = (value: string) => {
  const parsed = new Date(value);
  return Number.isNaN(parsed.getTime()) ? value : parsed.toLocaleString('es-EC', { dateStyle: 'medium', timeStyle: 'short' });
};

// EVT-ORGANIZER-CLAIM-001: review queue for strict administrators. The
// server refuses the claimant's own review and decides every race.
export default function EventOrganizerClaimsQueue() {
  const claims = useQuery({ queryKey: queueKey, queryFn: SocialEventsAPI.listEventOrganizerClaims, retry: false });
  return <Stack spacing={2}>
    <Alert severity="warning">
      Aprobar una solicitud convierte a la cuenta solicitante en organizadora del evento: podrá editarlo, vender entradas y ver órdenes y datos de compradores. Contrasta las pruebas con fuentes oficiales antes de decidir.
    </Alert>
    {claims.isLoading && <CircularProgress aria-label="Cargando solicitudes de eventos" />}
    {claims.isError && <Alert severity="error" action={<Button color="inherit" size="small" onClick={() => { void claims.refetch(); }}>Reintentar</Button>}>
      No tienes acceso a esta cola o no se pudo cargar.
    </Alert>}
    {claims.data?.length === 0 && <Alert severity="info">No hay solicitudes sobre eventos sin organizador.</Alert>}
    {(claims.data ?? []).map((claim) => <ClaimReviewCard key={claim.eocrId} claim={claim} />)}
  </Stack>;
}

function ClaimReviewCard({ claim }: { claim: EventOrganizerClaimReviewDTO }) {
  const client = useQueryClient();
  const [notes, setNotes] = useState('');
  const decide = useMutation({
    mutationFn: (decision: EventOrganizerClaimDecision) => SocialEventsAPI.decideEventOrganizerClaim(claim.eocrId, decision, notes),
    // Also after a refusal: a 409 means the queue on screen is stale.
    onSettled: () => client.invalidateQueries({ queryKey: queueKey }),
  });
  const chip = statusChip[claim.eocrStatus] ?? { label: claim.eocrStatus, color: 'default' as const };
  const open = claim.eocrStatus === 'submitted';
  const claimant = claim.eocrClaimantName?.trim()
    ? `${claim.eocrClaimantName.trim()} (cuenta ${claim.eocrClaimantPartyId})`
    : `Cuenta ${claim.eocrClaimantPartyId}`;
  const trimmedNotes = notes.trim();
  const canDecide = open && !decide.isPending && trimmedNotes.length >= EVENT_ORGANIZER_CLAIM_NOTES_MIN_LENGTH;
  const failure = decide.error instanceof ApiError ? decide.error.status : decide.error ? 0 : null;
  const decideError = failure === null ? null
    : failure === 403 ? 'No puedes decidir esta solicitud: la revisión corresponde a una persona administradora distinta de quien la envió.'
      : failure === 409 ? 'La solicitud ya fue decidida, el evento ya tiene quien lo organice o la cuenta solicitante ya no existe. Se actualizó la cola.'
        : 'No se pudo registrar la decisión. Inténtalo de nuevo.';

  return <Card variant="outlined" component="article" aria-label={`Solicitud sobre ${claim.eocrEventTitle}`}>
    <CardContent><Stack spacing={1.5}>
      <Stack direction={{ xs: 'column', sm: 'row' }} justifyContent="space-between" gap={1}>
        <Typography component="h3" variant="h6" fontWeight={800}>
          <Link component={RouterLink} to={`/social/eventos/${encodeURIComponent(claim.eocrEventId)}`}>{claim.eocrEventTitle}</Link>
        </Typography>
        <Chip label={chip.label} color={chip.color} size="small" />
      </Stack>
      <Typography color="text.secondary">Evento del {formatInstant(claim.eocrEventStart)} · solicitud enviada el {formatInstant(claim.eocrSubmittedAt)}</Typography>
      <Typography>Solicitante: {claimant}</Typography>
      {open && claim.eocrEventHasOrganizer && <Alert severity="info">
        El evento ya tiene quien lo organice: esta solicitud solo puede rechazarse.
      </Alert>}
      <Box>
        <Typography component="h4" variant="subtitle2">Pruebas presentadas</Typography>
        <Box component="pre" sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere', maxHeight: 320, overflow: 'auto', m: 0, fontFamily: 'inherit' }}>
          {claim.eocrEvidence}
        </Box>
      </Box>
      {!open && claim.eocrReviewerNotes && <Typography color="text.secondary">
        Decisión{claim.eocrReviewedAt ? ` del ${formatInstant(claim.eocrReviewedAt)}` : ''}{claim.eocrReviewerPartyId ? ` por la cuenta ${claim.eocrReviewerPartyId}` : ''}: {claim.eocrReviewerNotes}
      </Typography>}
      {open && <TextField
        label="Notas de revisión"
        value={notes}
        onChange={(change) => setNotes(change.target.value)}
        multiline
        minRows={2}
        disabled={decide.isPending}
        inputProps={{ maxLength: EVENT_ORGANIZER_CLAIM_TEXT_MAX_LENGTH }}
        helperText={`Qué contrastaste y por qué decides así (mínimo ${EVENT_ORGANIZER_CLAIM_NOTES_MIN_LENGTH} caracteres). Queda registrado con la decisión.`}
      />}
      {decideError && <Alert severity="error">{decideError}</Alert>}
    </Stack></CardContent>
    {open && <CardActions sx={{ flexWrap: 'wrap', px: 2, pb: 2 }}>
      <Button variant="contained" disabled={!canDecide || claim.eocrEventHasOrganizer} onClick={() => decide.mutate('approved')}>Aprobar y asignar el evento</Button>
      <Button color="error" disabled={!canDecide} onClick={() => decide.mutate('rejected')}>Rechazar</Button>
    </CardActions>}
  </Card>;
}
