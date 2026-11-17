import { useState } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Card, CardContent, CircularProgress, MenuItem, Stack, TextField, Typography } from '@mui/material';
import { ApiError } from '../../api/client';
import { SocialEventsAPI, type EventCourtesyTicketDTO, type SocialTicketOrderDTO, type SocialTicketTierDTO } from '../../api/socialEvents';

export const MAX_COURTESY_TICKETS = 10;
const EMAIL_SHAPE = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

const courtesyError = (error: unknown): string => {
  // Local validation already carries a Spanish message.
  if (!(error instanceof ApiError)) return error instanceof Error ? error.message : 'No se pudieron emitir las cortesías.';
  if (error.status === 409) return 'No quedan suficientes entradas disponibles en este tipo. Reduce la cantidad o elige otro.';
  if (error.status === 404) return 'Ese tipo de entrada ya no existe. Actualiza el evento y elige otro.';
  if (error.status === 403) return 'Tu cuenta ya no puede emitir cortesías para este evento.';
  if (error.status === 400) return `Revisa los datos: nombre obligatorio y entre 1 y ${MAX_COURTESY_TICKETS} entradas.`;
  return 'No se pudieron emitir las cortesías. Inténtalo de nuevo.';
};

interface EventCourtesyTicketsCardProps {
  eventId: string;
  tiers: readonly SocialTicketTierDTO[];
  tiersLoading: boolean;
}

export default function EventCourtesyTicketsCard({ eventId, tiers, tiersLoading }: EventCourtesyTicketsCardProps) {
  const queryClient = useQueryClient();
  const options = tiers.filter((tier): tier is SocialTicketTierDTO & { ticketTierId: string } => Boolean(tier.ticketTierId));
  const [chosenTierId, setChosenTierId] = useState('');
  const [holderName, setHolderName] = useState('');
  const [holderEmail, setHolderEmail] = useState('');
  const [quantity, setQuantity] = useState('1');
  const [issued, setIssued] = useState<{ order: SocialTicketOrderDTO; holderName: string; emailedTo: string | null } | null>(null);
  // Fall back to the first tier until a choice exists, and when a choice disappears.
  const tierId = options.some((tier) => tier.ticketTierId === chosenTierId) ? chosenTierId : options[0]?.ticketTierId ?? '';

  const mutation = useMutation({
    mutationFn: (payload: EventCourtesyTicketDTO) => {
      if (!payload.ectTierId) throw new Error('Elige un tipo de entrada.');
      if (!payload.ectHolderName) throw new Error('Escribe el nombre de quien recibe la cortesía.');
      if (payload.ectHolderEmail && !EMAIL_SHAPE.test(payload.ectHolderEmail)) throw new Error('Escribe un correo válido o deja el campo vacío.');
      if (!Number.isInteger(payload.ectQuantity) || payload.ectQuantity < 1 || payload.ectQuantity > MAX_COURTESY_TICKETS) {
        throw new Error(`La cantidad debe estar entre 1 y ${MAX_COURTESY_TICKETS}.`);
      }
      return SocialEventsAPI.issueCourtesyTickets(eventId, payload);
    },
    onSuccess: (order, payload) => {
      setIssued({ order, holderName: payload.ectHolderName, emailedTo: payload.ectHolderEmail ?? null });
      setHolderName('');
      setHolderEmail('');
      setQuantity('1');
      // Detail page, events list and public storefront each cache availability.
      for (const key of ['social-event-ticket-tiers', 'social-ticket-tiers', 'social-ticket-orders', 'public-event-ticket-storefront']) {
        void queryClient.invalidateQueries({ queryKey: [key, eventId] });
      }
    },
  });
  const submit = () => {
    if (mutation.isPending) return;
    setIssued(null);
    mutation.mutate({
      ectTierId: tierId,
      ectQuantity: Number(quantity),
      ectHolderName: holderName.trim(),
      ectHolderEmail: holderEmail.trim() || null,
    });
  };

  return <Card variant="outlined" component="section" aria-labelledby="event-courtesy-title">
    <CardContent><Stack spacing={1.5}>
      <Typography id="event-courtesy-title" component="h2" variant="h6">Entradas de cortesía</Typography>
      <Typography variant="body2" color="text.secondary">
        Emite entradas sin costo para invitados. Se descuentan del cupo del tipo de entrada elegido y quedan registradas como una orden pagada de valor cero.
      </Typography>
      {tiersLoading ? <CircularProgress size={22} aria-label="Cargando tipos de entrada" /> : options.length === 0 ? (
        <Alert severity="info">Primero crea un tipo de ticket en la sección «Tickets». Las cortesías se emiten sobre un tipo existente.</Alert>
      ) : <>
        {mutation.error && <Alert severity="error">{courtesyError(mutation.error)}</Alert>}
        <Stack component="form" spacing={1.5} noValidate onSubmit={(event) => { event.preventDefault(); submit(); }}>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
            <TextField select label="Tipo de entrada" value={tierId} onChange={(change) => setChosenTierId(change.target.value)} disabled={mutation.isPending} sx={{ flex: 1, minWidth: 180 }}>
              {options.map((tier) => <MenuItem key={tier.ticketTierId} value={tier.ticketTierId}>
                {tier.ticketTierName} · {Math.max(0, tier.ticketTierQuantityTotal - tier.ticketTierQuantitySold)} disponibles
              </MenuItem>)}
            </TextField>
            <TextField label="Cantidad" type="number" value={quantity} onChange={(change) => setQuantity(change.target.value)} inputProps={{ min: 1, max: MAX_COURTESY_TICKETS, step: 1 }} helperText={`De 1 a ${MAX_COURTESY_TICKETS}`} disabled={mutation.isPending} />
          </Stack>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={1}>
            <TextField label="Nombre de quien recibe" required value={holderName} onChange={(change) => setHolderName(change.target.value)} inputProps={{ maxLength: 120 }} disabled={mutation.isPending} sx={{ flex: 1 }} />
            <TextField label="Correo (opcional)" type="email" value={holderEmail} onChange={(change) => setHolderEmail(change.target.value)} helperText="Si lo indicas, enviaremos los códigos a este correo." disabled={mutation.isPending} sx={{ flex: 1 }} />
          </Stack>
          <Button type="submit" variant="contained" disabled={mutation.isPending || !holderName.trim()} sx={{ alignSelf: 'flex-start' }}>
            {mutation.isPending ? <CircularProgress size={20} color="inherit" /> : 'Emitir cortesía'}
          </Button>
        </Stack>
      </>}
      {issued && <Alert severity="success" role="status" onClose={() => setIssued(null)}>
        <Typography fontWeight={700}>
          {issued.order.ticketOrderTickets.length === 1 ? 'Cortesía emitida' : `${issued.order.ticketOrderTickets.length} cortesías emitidas`} para {issued.holderName}
        </Typography>
        <Box component="ul" sx={{ pl: 2.5, my: 0.5 }} aria-label="Códigos de las entradas emitidas">
          {issued.order.ticketOrderTickets.map((ticket) => (
            <Box component="li" key={ticket.ticketId ?? ticket.ticketCode} sx={{ fontFamily: 'monospace', overflowWrap: 'anywhere' }}>{ticket.ticketCode}</Box>
          ))}
        </Box>
        <Typography variant="body2">
          {issued.emailedTo
            ? `También enviamos los códigos a ${issued.emailedTo}.`
            : 'No indicaste un correo: comparte estos códigos directamente con la persona invitada.'}
        </Typography>
      </Alert>}
    </Stack></CardContent>
  </Card>;
}
