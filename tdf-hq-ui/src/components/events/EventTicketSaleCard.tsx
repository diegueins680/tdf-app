import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Box, Button, Card, CardContent, CircularProgress, Stack, Typography } from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { ApiError } from '../../api/client';
import { SocialEventsAPI, type EventTicketSaleDTO, type EventTicketSaleTermsDTO } from '../../api/socialEvents';
import { TICKET_SALE_REQUESTS_QUERY_KEY, ticketSaleQueryKey } from './ticketSaleQueryKeys';


const percent = (bps: number): string => `${(bps / 100).toLocaleString('es', { maximumFractionDigits: 2 })} %`;

export const ticketSaleTermsLines = (terms: EventTicketSaleTermsDTO): string[] => [
  `TDF cobra al público en ${terms.etstCurrency} y te liquida lo vendido.`,
  `Quien compra paga ${percent(terms.etstBuyerFeeBps)} adicional de servicio; de tu venta se descuenta ${percent(terms.etstOrganizerFeeBps)}.`,
  `Política de reembolso: ${terms.etstRefundPolicy}`,
  `Cada compra reserva las entradas ${terms.etstHoldMinutes} minutos y admite hasta ${terms.etstMaxTicketsPerOrder} entradas.`,
];

const formatWhen = (value?: string | null): string | null => {
  if (!value) return null;
  const date = new Date(value);
  return Number.isNaN(date.getTime()) ? null : date.toLocaleDateString('es', { day: 'numeric', month: 'long', year: 'numeric' });
};

const saleError = (error: unknown): string => {
  if (!(error instanceof ApiError)) return 'No se pudo completar la acción. Inténtalo de nuevo.';
  if (error.status === 409) return 'El evento cambió mientras tanto. Revisa que tenga un tipo de ticket activo con precio en USD e inténtalo de nuevo.';
  if (error.status === 403) return 'Tu cuenta no puede hacer esto en este evento.';
  if (error.status === 404) return 'La solicitud o el evento ya no existe. Actualiza la página.';
  return 'No se pudo completar la acción. Inténtalo de nuevo.';
};

interface EventTicketSaleCardProps {
  eventId: string;
}

/**
 * EVT-TICKET-SALE-001: the event manager asks to sell paid tickets under
 * TDF's standard terms; a strict administrator approves or declines here.
 */
export default function EventTicketSaleCard({ eventId }: EventTicketSaleCardProps) {
  const queryClient = useQueryClient();
  const saleQuery = useQuery({
    queryKey: ticketSaleQueryKey(eventId),
    queryFn: () => SocialEventsAPI.getTicketSale(eventId),
    retry: false,
  });
  const settle = (sale: EventTicketSaleDTO) => {
    queryClient.setQueryData(ticketSaleQueryKey(eventId), sale);
    void queryClient.invalidateQueries({ queryKey: TICKET_SALE_REQUESTS_QUERY_KEY });
    void queryClient.invalidateQueries({ queryKey: ['public-event-ticket-storefront', eventId] });
  };
  const request = useMutation({ mutationFn: () => SocialEventsAPI.requestTicketSale(eventId), onSuccess: settle });
  const decide = useMutation({
    mutationFn: (input: { requestId: string; decision: 'approve' | 'decline' }) =>
      SocialEventsAPI.decideTicketSaleRequest(input.requestId, input.decision),
    onSuccess: settle,
  });
  const busy = request.isPending || decide.isPending;
  const failure = request.error ?? decide.error;
  const sale = saleQuery.data;

  // Viewers the server does not admit see nothing rather than an error.
  if (saleQuery.error instanceof ApiError && (saleQuery.error.status === 403 || saleQuery.error.status === 404)) return null;

  return <Card variant="outlined" component="section" id="venta-entradas" aria-labelledby="event-ticket-sale-title">
    <CardContent><Stack spacing={1.5}>
      <Typography id="event-ticket-sale-title" component="h2" variant="h6">Venta de entradas pagadas</Typography>
      {saleQuery.isLoading && <CircularProgress size={22} aria-label="Cargando el estado de la venta" />}
      {saleQuery.error && <Alert severity="error">No se pudo cargar el estado de la venta. Actualiza la página.</Alert>}
      {failure && <Alert severity="error">{saleError(failure)}</Alert>}
      {sale && <>
        {sale.etsStatus === 'approved' && <Alert severity="success" role="status">
          <Typography fontWeight={700}>Venta aprobada{formatWhen(sale.etsDecidedAt) ? ` el ${formatWhen(sale.etsDecidedAt)}` : ''}</Typography>
          <Typography variant="body2">
            El público puede comprar en la página de entradas cuando el evento esté publicado y a la venta.
          </Typography>
          <Button component={RouterLink} to={`/eventos/${encodeURIComponent(eventId)}/entradas`} size="small" sx={{ mt: 0.5, px: 0 }}>
            Abrir la página de entradas
          </Button>
        </Alert>}
        {sale.etsStatus === 'requested' && <Alert severity="info" role="status">
          <Typography fontWeight={700}>Solicitud enviada{formatWhen(sale.etsRequestedAt) ? ` el ${formatWhen(sale.etsRequestedAt)}` : ''}</Typography>
          <Typography variant="body2">TDF la revisará y te avisará por correo. Mientras tanto puedes seguir preparando el evento.</Typography>
        </Alert>}
        {sale.etsStatus === 'declined' && <Alert severity="warning" role="status">
          TDF no aprobó la solicitud anterior. Puedes escribirnos para revisar el caso y volver a pedirla.
        </Alert>}
        {(sale.etsStatus === 'none' || sale.etsStatus === 'declined') && <>
          <Typography variant="body2" color="text.secondary">
            Para cobrar entradas a través de TDF necesitas la aprobación de TDF. Estas son las condiciones:
          </Typography>
          <Box component="ul" sx={{ pl: 2.5, my: 0 }} aria-label="Condiciones de la venta">
            {ticketSaleTermsLines(sale.etsTerms).map((line) => <Typography component="li" variant="body2" key={line}>{line}</Typography>)}
          </Box>
          {sale.etsBlockedReason === 'no_paid_tier' && <Alert severity="info">
            Primero crea en «Tickets» un tipo de ticket activo con precio en {sale.etsTerms.etstCurrency}.
          </Alert>}
          {sale.etsBlockedReason === 'not_manager' && <Alert severity="info">
            Aún no hay una solicitud. La pide quien organiza el evento o sus administradores.
          </Alert>}
          {sale.etsBlockedReason !== 'not_manager' && <Button
            variant="contained"
            disabled={busy || !sale.etsCanRequest}
            onClick={() => { if (!busy) request.mutate(); }}
            sx={{ alignSelf: 'flex-start' }}
          >
            {request.isPending ? <CircularProgress size={20} color="inherit" /> : 'Solicitar venta de entradas pagadas'}
          </Button>}
        </>}
        {sale.etsCanDecide && sale.etsRequestId && <Stack direction="row" spacing={1} aria-label="Decisión de TDF">
          <Button variant="contained" color="success" disabled={busy} onClick={() => decide.mutate({ requestId: sale.etsRequestId ?? '', decision: 'approve' })}>
            Aprobar venta
          </Button>
          <Button variant="outlined" color="inherit" disabled={busy} onClick={() => decide.mutate({ requestId: sale.etsRequestId ?? '', decision: 'decline' })}>
            Rechazar
          </Button>
        </Stack>}
      </>}
    </Stack></CardContent>
  </Card>;
}
