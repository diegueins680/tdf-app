import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Button, Card, CardContent, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { SocialEventsAPI, type EventTicketSaleRequestDTO } from '../../api/socialEvents';
import { TICKET_SALE_REQUESTS_QUERY_KEY, ticketSaleQueryKey } from './ticketSaleQueryKeys';

const money = (cents: number, currency: string): string =>
  new Intl.NumberFormat('es', { style: 'currency', currency }).format(cents / 100);

const tierSummary = (request: EventTicketSaleRequestDTO): string =>
  request.etsrTiers.length === 0
    ? 'Sin tipos de ticket activos'
    : request.etsrTiers.map((tier) => `${tier.etstiName}: ${money(tier.etstiPriceCents, tier.etstiCurrency)} × ${tier.etstiQuantityTotal}`).join(' · ');

/**
 * EVT-TICKET-SALE-001: pending paid-sale requests for strict administrators.
 * Renders nothing when there is nothing to decide or the list cannot load.
 */
export default function EventTicketSaleRequestsQueue() {
  const queryClient = useQueryClient();
  const requestsQuery = useQuery({
    queryKey: TICKET_SALE_REQUESTS_QUERY_KEY,
    queryFn: () => SocialEventsAPI.listTicketSaleRequests(),
    retry: false,
  });
  const decide = useMutation({
    mutationFn: (input: { requestId: string; decision: 'approve' | 'decline' }) =>
      SocialEventsAPI.decideTicketSaleRequest(input.requestId, input.decision),
    onSuccess: (sale) => {
      queryClient.setQueryData(ticketSaleQueryKey(sale.etsEventId), sale);
      void queryClient.invalidateQueries({ queryKey: TICKET_SALE_REQUESTS_QUERY_KEY });
    },
  });
  const requests = requestsQuery.data ?? [];
  if (requests.length === 0 && !decide.error) return null;

  return <Card variant="outlined" component="section" aria-labelledby="ticket-sale-requests-title">
    <CardContent><Stack spacing={1.5}>
      <Typography id="ticket-sale-requests-title" component="h2" variant="h6">
        Solicitudes de venta de entradas ({requests.length})
      </Typography>
      {decide.error && <Alert severity="error">No se pudo registrar la decisión. Abre el evento y vuelve a intentarlo.</Alert>}
      {requests.map((request) => <Stack
        key={request.etsrRequestId}
        direction={{ xs: 'column', md: 'row' }}
        spacing={1}
        alignItems={{ md: 'center' }}
        justifyContent="space-between"
      >
        <Stack spacing={0.25} sx={{ minWidth: 0 }}>
          <Typography fontWeight={700} sx={{ overflowWrap: 'anywhere' }}>{request.etsrEventTitle}</Typography>
          <Typography variant="body2" color="text.secondary">
            {request.etsrOrganizerName ? `Organiza ${request.etsrOrganizerName} · ` : ''}{tierSummary(request)}
          </Typography>
        </Stack>
        <Stack direction="row" spacing={1} flexShrink={0}>
          <Button component={RouterLink} to={`/social/eventos/${encodeURIComponent(request.etsrEventId)}#venta-entradas`} size="small">Ver evento</Button>
          <Button size="small" variant="contained" color="success" disabled={decide.isPending} onClick={() => decide.mutate({ requestId: request.etsrRequestId, decision: 'approve' })}>
            Aprobar
          </Button>
          <Button size="small" variant="outlined" color="inherit" disabled={decide.isPending} onClick={() => decide.mutate({ requestId: request.etsrRequestId, decision: 'decline' })}>
            Rechazar
          </Button>
        </Stack>
      </Stack>)}
    </Stack></CardContent>
  </Card>;
}
