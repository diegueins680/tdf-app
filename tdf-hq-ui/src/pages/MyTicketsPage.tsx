import { useMemo } from 'react';
import { Link as RouterLink } from 'react-router-dom';
import { useQueries, useQuery } from '@tanstack/react-query';
import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Container,
  Link,
  Stack,
  Typography,
} from '@mui/material';
import { EventTickets } from '../api/eventTickets';
import { SocialEventsAPI } from '../api/socialEvents';
import { EVENTS_PATH } from '../components/events/UpcomingEventsCarousel';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useMetaTags } from '../hooks/useMetaTags';
import { useSession } from '../session/SessionContext';
import { listDeviceTicketOrders, ticketOrderStage, type TicketOrderStage } from '../utils/deviceTicketOrders';

interface TicketOrderRow {
  key: string;
  eventId: number;
  orderId: number;
  quantity: number;
  totalMinor: number;
  currency: string;
  stage: TicketOrderStage;
  codes: string[];
  /** Guest orders open their private order page; account orders open the event. */
  href: string;
}

const positiveId = (value: string | null | undefined): number | null => {
  const parsed = Number(value);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : null;
};

export default function MyTicketsPage() {
  const { session } = useSession();
  const { locale, timezone } = useLocalePreferences();
  const english = locale.toLowerCase().startsWith('en');
  const signedIn = Boolean(session);

  useMetaTags({
    title: english ? 'My tickets' : 'Mis entradas',
    description: english ? 'Your TDF Records event tickets.' : 'Tus entradas para eventos de TDF Records.',
    robots: 'noindex,nofollow',
  });

  const deviceRefs = useMemo(() => listDeviceTicketOrders(), []);
  const deviceOrders = useQueries({
    queries: deviceRefs.map((ref) => ({
      queryKey: ['my-tickets-device-order', ref.eventId, ref.orderId],
      queryFn: () => EventTickets.getCheckout(ref.eventId, ref.orderId, ref.lookupToken),
      retry: false,
    })),
  });

  const accountOrders = useQuery({
    queryKey: ['my-ticket-orders', session?.partyId ?? null],
    queryFn: () => SocialEventsAPI.listMyTicketOrders(),
    enabled: signedIn,
    retry: false,
  });

  const rows = useMemo<TicketOrderRow[]>(() => {
    const byKey = new Map<string, TicketOrderRow>();
    deviceRefs.forEach((ref, index) => {
      const order = deviceOrders[index]?.data;
      if (!order) return;
      const key = `${ref.eventId}:${ref.orderId}`;
      byKey.set(key, {
        key,
        eventId: ref.eventId,
        orderId: ref.orderId,
        quantity: order.quote.quantity,
        totalMinor: order.quote.checkoutTotalMinor,
        currency: order.quote.currency,
        stage: ticketOrderStage(order.paymentStatus),
        codes: [],
        href: `/eventos/${ref.eventId}/orden/${ref.orderId}`,
      });
    });
    for (const order of accountOrders.data ?? []) {
      const eventId = positiveId(order.ticketOrderEventId);
      const orderId = positiveId(order.ticketOrderId);
      if (!eventId || !orderId) continue;
      const key = `${eventId}:${orderId}`;
      if (byKey.has(key)) continue;
      const stage = ticketOrderStage(order.ticketOrderStatusValue);
      byKey.set(key, {
        key,
        eventId,
        orderId,
        quantity: order.ticketOrderQuantity,
        totalMinor: order.ticketOrderAmountCents,
        currency: order.ticketOrderCurrency,
        stage,
        codes: stage === 'paid' ? order.ticketOrderTickets.map((ticket) => ticket.ticketCode) : [],
        href: `/eventos/${eventId}`,
      });
    }
    return [...byKey.values()].sort((left, right) => right.orderId - left.orderId);
  }, [accountOrders.data, deviceOrders, deviceRefs]);

  const eventIds = useMemo(() => [...new Set(rows.map((row) => row.eventId))], [rows]);
  const storefronts = useQueries({
    queries: eventIds.map((eventId) => ({
      queryKey: ['public-event-ticket-storefront', eventId],
      queryFn: () => EventTickets.getStorefront(eventId),
      retry: false,
      staleTime: 5 * 60 * 1000,
    })),
  });
  const eventById = new Map(eventIds.map((eventId, index) => [eventId, storefronts[index]?.data]));

  const loading = deviceOrders.some((query) => query.isLoading) || (signedIn && accountOrders.isLoading);
  const unreadableDevice = deviceOrders.filter((query) => query.isError).length;

  const money = (minor: number, currency: string) => {
    try {
      return new Intl.NumberFormat(locale, { style: 'currency', currency }).format(minor / 100);
    } catch {
      return `${(minor / 100).toFixed(2)} ${currency}`;
    }
  };
  const when = (value: string, eventTimezone?: string | null) => new Intl.DateTimeFormat(locale, {
    dateStyle: 'medium',
    timeStyle: 'short',
    timeZone: eventTimezone ?? timezone,
  }).format(new Date(value));

  const stageChip = (stage: TicketOrderStage) => {
    if (stage === 'paid') return <Chip size="small" color="success" label={english ? 'Paid' : 'Pagada'} />;
    if (stage === 'pending') return <Chip size="small" color="warning" label={english ? 'Payment pending' : 'Pago pendiente'} />;
    return <Chip size="small" variant="outlined" label={english ? 'No valid tickets' : 'Sin entradas válidas'} />;
  };

  return (
    <Container maxWidth="sm" sx={{ py: { xs: 3, md: 5 } }}>
      <Stack spacing={2.5}>
        <Box>
          <Typography variant="h4" component="h1" fontWeight={700}>
            {english ? 'My tickets' : 'Mis entradas'}
          </Typography>
          <Typography color="text.secondary" sx={{ mt: 0.5 }}>
            {signedIn
              ? (english
                ? 'Tickets from your account and from purchases made in this browser.'
                : 'Entradas de tu cuenta y de las compras hechas en este navegador.')
              : (english
                ? 'Tickets bought in this browser. For a purchase made elsewhere, open the link in its confirmation email.'
                : 'Entradas compradas en este navegador. Si compraste en otro dispositivo, abre el enlace del correo de confirmación.')}
          </Typography>
        </Box>

        {loading && (
          <Box sx={{ display: 'flex', justifyContent: 'center', py: 4 }}>
            <CircularProgress aria-label={english ? 'Loading tickets' : 'Cargando entradas'} />
          </Box>
        )}

        {signedIn && accountOrders.isError && (
          <Alert
            severity="warning"
            action={(
              <Button color="inherit" size="small" onClick={() => { void accountOrders.refetch(); }}>
                {english ? 'Retry' : 'Reintentar'}
              </Button>
            )}
          >
            {english
              ? 'We could not load the tickets from your account.'
              : 'No pudimos cargar las entradas de tu cuenta.'}
          </Alert>
        )}

        {unreadableDevice > 0 && (
          <Alert severity="info">
            {english
              ? `${unreadableDevice} purchase saved in this browser could not be read. Its confirmation email has the link.`
              : `No pudimos leer ${unreadableDevice === 1 ? 'una compra guardada' : `${unreadableDevice} compras guardadas`} en este navegador. El correo de confirmación tiene el enlace.`}
          </Alert>
        )}

        {!loading && rows.length === 0 && (
          <Card variant="outlined">
            <CardContent>
              <Stack spacing={1.5} alignItems="flex-start">
                <Typography>
                  {english ? 'You have no tickets here yet.' : 'Todavía no tienes entradas aquí.'}
                </Typography>
                <Button component={RouterLink} to={EVENTS_PATH} variant="contained">
                  {english ? 'See events' : 'Ver eventos'}
                </Button>
              </Stack>
            </CardContent>
          </Card>
        )}

        {rows.map((row) => {
          const event = eventById.get(row.eventId);
          const title = event?.title ?? (english ? `Event #${row.eventId}` : `Evento #${row.eventId}`);
          const guestOrder = row.href.includes('/orden/');
          return (
            <Card key={row.key} variant="outlined">
              <CardContent>
                <Stack spacing={1}>
                  <Stack direction="row" spacing={1} justifyContent="space-between" alignItems="flex-start">
                    <Typography variant="h6" component="h2" sx={{ overflowWrap: 'anywhere' }}>
                      {title}
                    </Typography>
                    {stageChip(row.stage)}
                  </Stack>
                  {event?.startsAt && (
                    <Typography variant="body2" color="text.secondary">
                      {when(event.startsAt, event.timezone)}{event.venueName ? ` · ${event.venueName}` : ''}
                    </Typography>
                  )}
                  <Typography variant="body2">
                    {english
                      ? `${row.quantity} ${row.quantity === 1 ? 'ticket' : 'tickets'} · ${money(row.totalMinor, row.currency)}`
                      : `${row.quantity} ${row.quantity === 1 ? 'entrada' : 'entradas'} · ${money(row.totalMinor, row.currency)}`}
                  </Typography>
                  {row.codes.length > 0 && (
                    <Typography variant="body2" sx={{ fontFamily: 'monospace', overflowWrap: 'anywhere' }}>
                      {(english ? 'Codes: ' : 'Códigos: ') + row.codes.join(', ')}
                    </Typography>
                  )}
                  <Box>
                    {guestOrder && row.stage === 'pending' ? (
                      <Button component={RouterLink} to={row.href} variant="contained" size="small">
                        {english ? 'Finish payment' : 'Completar pago'}
                      </Button>
                    ) : (
                      <Link component={RouterLink} to={row.href}>
                        {guestOrder
                          ? (english ? 'Open order' : 'Ver la orden')
                          : (english ? 'Open event' : 'Ver el evento')}
                      </Link>
                    )}
                  </Box>
                </Stack>
              </CardContent>
            </Card>
          );
        })}

        {!signedIn && (
          <Typography variant="body2" color="text.secondary">
            {english ? 'Bought in the TDF app? ' : '¿Compraste en la app de TDF? '}
            <Link component={RouterLink} to="/login?redirect=%2Fmis-entradas">
              {english ? 'Sign in to see those tickets here.' : 'Inicia sesión para verlas aquí.'}
            </Link>
          </Typography>
        )}
      </Stack>
    </Container>
  );
}
