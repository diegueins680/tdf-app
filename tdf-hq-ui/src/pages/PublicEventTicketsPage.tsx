import {
  Alert,
  Box,
  Button,
  Card,
  CardContent,
  Checkbox,
  Chip,
  CircularProgress,
  Container,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControlLabel,
  MenuItem,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import ConfirmationNumberIcon from '@mui/icons-material/ConfirmationNumber';
import EventIcon from '@mui/icons-material/Event';
import PlaceIcon from '@mui/icons-material/Place';
import { useQuery } from '@tanstack/react-query';
import { useEffect, useMemo, useRef, useState } from 'react';
import { Link as RouterLink, useLocation, useNavigate, useParams } from 'react-router-dom';

import {
  EventTickets,
  type PublicEventTicketCheckout,
  type PublicEventTicketCheckoutRequest,
} from '../api/eventTickets';
import type { DatafastCheckoutDTO } from '../api/types';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useMetaTags } from '../hooks/useMetaTags';

const makeIdempotencyKey = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `event-ticket-checkout-${crypto.randomUUID()}`;
  }
  return `event-ticket-checkout-${Date.now()}-${Math.random().toString(16).slice(2)}`;
};

const lookupStorageKey = (eventId: number, orderId: number) =>
  `tdf:event-ticket-checkout:${eventId}:${orderId}`;

const saveLookupToken = (eventId: number, orderId: number, token: string) => {
  try {
    window.localStorage.setItem(lookupStorageKey(eventId, orderId), token);
  } catch {
    // The live response remains usable if browser storage is unavailable.
  }
};

const loadLookupToken = (eventId: number, orderId: number): string | null => {
  try {
    return window.localStorage.getItem(lookupStorageKey(eventId, orderId));
  } catch {
    return null;
  }
};

export default function PublicEventTicketsPage() {
  const params = useParams<{ eventId: string; orderId?: string }>();
  const eventId = Number(params.eventId);
  const routeOrderId = params.orderId ? Number(params.orderId) : null;
  const validEventId = Number.isSafeInteger(eventId) && eventId > 0;
  const validOrderId = routeOrderId == null
    || (Number.isSafeInteger(routeOrderId) && routeOrderId > 0);
  const location = useLocation();
  const navigate = useNavigate();
  const { locale, timezone } = useLocalePreferences();
  const english = locale.toLowerCase().startsWith('en');
  const [tierId, setTierId] = useState('');
  const [quantity, setQuantity] = useState('1');
  const [buyerName, setBuyerName] = useState('');
  const [buyerEmail, setBuyerEmail] = useState('');
  const [buyerPhone, setBuyerPhone] = useState('');
  const [promoCode, setPromoCode] = useState('');
  const [termsAccepted, setTermsAccepted] = useState(false);
  const [checkout, setCheckout] = useState<PublicEventTicketCheckout | null>(null);
  const [submitting, setSubmitting] = useState(false);
  const [paymentBusy, setPaymentBusy] = useState(false);
  const [message, setMessage] = useState<string | null>(null);
  const idempotency = useRef<{ fingerprint: string; key: string } | null>(null);
  const [datafastCheckout, setDatafastCheckout] = useState<DatafastCheckoutDTO | null>(null);
  const [datafastOpen, setDatafastOpen] = useState(false);
  const [datafastWidgetKey, setDatafastWidgetKey] = useState(0);
  const datafastFormRef = useRef<HTMLDivElement | null>(null);
  const [paypalReady, setPaypalReady] = useState(false);
  const [paypalOpen, setPaypalOpen] = useState(false);
  const [paypalOrderId, setPaypalOrderId] = useState<string | null>(null);
  const paypalButtonRef = useRef<HTMLDivElement | null>(null);
  const paypalClientId = import.meta.env?.VITE_PAYPAL_CLIENT_ID?.trim() ?? '';

  const storefront = useQuery({
    queryKey: ['public-event-ticket-storefront', eventId],
    queryFn: () => EventTickets.getStorefront(eventId),
    enabled: validEventId,
    retry: false,
  });

  useEffect(() => {
    if (tierId) return;
    const firstTier = storefront.data?.tiers[0];
    if (firstTier) setTierId(String(firstTier.tierId));
  }, [storefront.data?.tiers, tierId]);

  const checkoutLookupToken = useMemo(() => {
    if (!checkout) return null;
    return checkout.lookupToken ?? loadLookupToken(checkout.eventId, checkout.orderId);
  }, [checkout]);

  useEffect(() => {
    if (!validEventId || !validOrderId || routeOrderId == null) return;
    const token = loadLookupToken(eventId, routeOrderId);
    if (!token) {
      setMessage(english
        ? 'This browser does not have the secure access token for that order.'
        : 'Este navegador no tiene el acceso seguro de esa orden.');
      return;
    }
    const query = new URLSearchParams(location.search);
    const resourcePath = query.get('resourcePath') ?? query.get('id');
    setPaymentBusy(true);
    setMessage(null);
    const request = resourcePath
      ? EventTickets.confirmDatafastStatus(eventId, routeOrderId, resourcePath, token)
      : EventTickets.getCheckout(eventId, routeOrderId, token);
    request
      .then((response) => {
        setCheckout(response);
        if (resourcePath) navigate(location.pathname, { replace: true });
      })
      .catch(() => setMessage(english
        ? 'The server could not verify this order. No payment is shown as successful.'
        : 'El servidor no pudo verificar esta orden. No mostramos ningún pago como exitoso.'))
      .finally(() => setPaymentBusy(false));
  }, [english, eventId, location.pathname, location.search, navigate, routeOrderId, validEventId, validOrderId]);

  const title = storefront.data?.title ?? (english ? 'Event tickets' : 'Entradas para eventos');
  const description = storefront.data?.description
    ?? (english ? 'Secure guest ticket checkout from TDF Records.' : 'Checkout seguro de entradas de TDF Records.');
  useMetaTags({
    title: `${title} · TDF Records`,
    description,
    canonical: typeof window === 'undefined' ? undefined : `${window.location.origin}/eventos/${params.eventId}/entradas`,
    ogType: 'website',
    structuredData: storefront.data ? {
      '@context': 'https://schema.org',
      '@type': 'MusicEvent',
      name: storefront.data.title,
      description,
      startDate: storefront.data.startsAt,
      endDate: storefront.data.endsAt,
      offers: storefront.data.tiers.map((tier) => ({
        '@type': 'Offer',
        price: (tier.unitPriceMinor / 100).toFixed(2),
        priceCurrency: tier.currency,
        availability: tier.remaining > 0 ? 'https://schema.org/InStock' : 'https://schema.org/SoldOut',
      })),
    } : undefined,
  });

  const money = (minor: number, currency: string) => new Intl.NumberFormat(locale, {
    style: 'currency',
    currency,
  }).format(minor / 100);
  const date = (value: string) => new Intl.DateTimeFormat(locale, {
    dateStyle: 'full',
    timeStyle: 'short',
    timeZone: storefront.data?.timezone ?? timezone,
  }).format(new Date(value));

  const handleCreateCheckout = async () => {
    if (!storefront.data?.checkoutAvailable) return;
    if (!termsAccepted) {
      setMessage(english
        ? 'Accept the event terms before holding tickets.'
        : 'Acepta los términos del evento antes de retener entradas.');
      return;
    }
    const selectedTierId = Number(tierId);
    const selectedQuantity = Number(quantity);
    if (!Number.isSafeInteger(selectedTierId) || selectedTierId <= 0
        || !Number.isSafeInteger(selectedQuantity) || selectedQuantity <= 0) {
      setMessage(english ? 'Choose a valid ticket and quantity.' : 'Elige una entrada y cantidad válidas.');
      return;
    }
    const payload: PublicEventTicketCheckoutRequest = {
      tierId: selectedTierId,
      quantity: selectedQuantity,
      buyerName: buyerName.trim(),
      buyerEmail: buyerEmail.trim(),
      ...(buyerPhone.trim() ? { buyerPhone: buyerPhone.trim() } : {}),
      ...(promoCode.trim() ? { promoCode: promoCode.trim() } : {}),
      termsAccepted,
    };
    const fingerprint = JSON.stringify(payload);
    if (idempotency.current?.fingerprint !== fingerprint) {
      idempotency.current = { fingerprint, key: makeIdempotencyKey() };
    }
    setSubmitting(true);
    setMessage(null);
    try {
      const response = await EventTickets.createCheckout(
        eventId,
        payload,
        idempotency.current.key,
      );
      if (!response.lookupToken) throw new Error('Secure lookup token missing');
      saveLookupToken(response.eventId, response.orderId, response.lookupToken);
      setCheckout(response);
      navigate(`/eventos/${response.eventId}/orden/${response.orderId}`, { replace: false });
    } catch {
      setMessage(english
        ? 'We could not hold these tickets. No order or payment success is assumed.'
        : 'No pudimos retener estas entradas. No asumimos que exista una orden ni un pago exitoso.');
    } finally {
      setSubmitting(false);
    }
  };

  const handleDatafast = async () => {
    if (!checkout || !checkoutLookupToken) return;
    setPaymentBusy(true);
    setMessage(null);
    try {
      const provider = await EventTickets.createDatafastCheckout(
        checkout.eventId,
        checkout.orderId,
        checkoutLookupToken,
      );
      setDatafastCheckout(provider);
      setDatafastOpen(true);
      setDatafastWidgetKey((current) => current + 1);
    } catch {
      setMessage(english
        ? 'Datafast could not be started. The order remains unpaid.'
        : 'No pudimos iniciar Datafast. La orden sigue sin pago confirmado.');
    } finally {
      setPaymentBusy(false);
    }
  };

  const handlePaypal = async () => {
    if (!checkout || !checkoutLookupToken || !paypalClientId) return;
    setPaymentBusy(true);
    setMessage(null);
    try {
      const provider = await EventTickets.createPaypalOrder(
        checkout.eventId,
        checkout.orderId,
        checkoutLookupToken,
      );
      setPaypalOrderId(provider.pcPaypalOrderId);
      setPaypalOpen(true);
    } catch {
      setMessage(english
        ? 'PayPal could not be started. The order remains unpaid.'
        : 'No pudimos iniciar PayPal. La orden sigue sin pago confirmado.');
    } finally {
      setPaymentBusy(false);
    }
  };

  useEffect(() => {
    if (!datafastOpen || !datafastCheckout || typeof window === 'undefined') return;
    if (datafastFormRef.current) datafastFormRef.current.innerHTML = '';
    window.wpwlOptions = { locale: english ? 'en' : 'es', style: 'card' };
    const script = document.createElement('script');
    script.src = datafastCheckout.dcWidgetUrl;
    script.async = true;
    script.onerror = () => setMessage(english
      ? 'The hosted Datafast form did not load. No payment was confirmed.'
      : 'El formulario alojado de Datafast no cargó. No se confirmó ningún pago.');
    document.body.appendChild(script);
    return () => script.remove();
  }, [datafastCheckout, datafastOpen, datafastWidgetKey, english]);

  useEffect(() => {
    if (!checkout?.paymentMethods.includes('paypal') || !paypalClientId || typeof window === 'undefined') return;
    if (window.paypal) {
      setPaypalReady(true);
      return;
    }
    const script = document.createElement('script');
    script.src = `https://www.paypal.com/sdk/js?client-id=${encodeURIComponent(paypalClientId)}&currency=${encodeURIComponent(checkout.quote.currency)}`;
    script.async = true;
    script.onload = () => setPaypalReady(true);
    script.onerror = () => setMessage(english
      ? 'PayPal did not load. No payment was confirmed.'
      : 'PayPal no cargó. No se confirmó ningún pago.');
    document.body.appendChild(script);
    return () => script.remove();
  }, [checkout?.paymentMethods, checkout?.quote.currency, english, paypalClientId]);

  useEffect(() => {
    if (!paypalOpen || !paypalReady || !paypalOrderId || !checkout || !checkoutLookupToken
        || !paypalButtonRef.current || typeof window === 'undefined' || !window.paypal) return;
    paypalButtonRef.current.innerHTML = '';
    const buttons = window.paypal.Buttons({
      createOrder: () => paypalOrderId,
      onApprove: async (data) => {
        if (data.orderID !== paypalOrderId) {
          setMessage(english
            ? 'PayPal returned a different reference. Nothing was captured.'
            : 'PayPal devolvió otra referencia. No se capturó ningún pago.');
          return;
        }
        setPaymentBusy(true);
        try {
          const response = await EventTickets.capturePaypalOrder(
            checkout.eventId,
            checkout.orderId,
            paypalOrderId,
            checkoutLookupToken,
          );
          setCheckout(response);
          setPaypalOpen(false);
          setPaypalOrderId(null);
          setMessage(response.paymentStatus === 'paid' ? null : (english
            ? 'PayPal returned, but the server has not confirmed payment.'
            : 'PayPal respondió, pero el servidor todavía no confirmó el pago.'));
        } catch {
          setMessage(english
            ? 'The server could not verify PayPal. The ticket is not shown as paid.'
            : 'El servidor no pudo verificar PayPal. No mostramos la entrada como pagada.');
        } finally {
          setPaymentBusy(false);
        }
      },
      onCancel: () => setMessage(english
        ? 'PayPal was cancelled. The order remains unpaid.'
        : 'Cancelaste PayPal. La orden sigue sin pago confirmado.'),
      onError: () => setMessage(english
        ? 'PayPal did not complete. No payment was confirmed.'
        : 'PayPal no completó la operación. No se confirmó ningún pago.'),
    });
    void buttons.render(paypalButtonRef.current);
    return () => buttons.close?.();
  }, [checkout, checkoutLookupToken, english, paypalOpen, paypalOrderId, paypalReady]);

  if (!validEventId || !validOrderId) {
    return <Container sx={{ py: 8 }}><Alert severity="error">Invalid event or order.</Alert></Container>;
  }
  if (storefront.isLoading) {
    return <Stack minHeight="60vh" alignItems="center" justifyContent="center"><CircularProgress /></Stack>;
  }
  if (storefront.isError || !storefront.data) {
    return <Container sx={{ py: 8 }}><Alert severity="error">{english
      ? 'This event ticket storefront is not available.'
      : 'La boletería de este evento no está disponible.'}</Alert></Container>;
  }

  const selectedTier = storefront.data.tiers.find((tier) => String(tier.tierId) === tierId);
  const paid = checkout?.paymentStatus === 'paid';
  const issued = checkout?.fulfillmentStatus === 'issued';
  const datafastReturnUrl = checkout && typeof window !== 'undefined'
    ? new URL(`/eventos/${checkout.eventId}/orden/${checkout.orderId}`, window.location.origin).toString()
    : '';

  return (
    <Box component="main" id="main-content" sx={{ bgcolor: 'background.default', minHeight: '100vh', py: { xs: 4, md: 7 } }}>
      <Container maxWidth="md">
        <Stack spacing={3}>
          <Button component={RouterLink} to={`/eventos/${eventId}`} sx={{ alignSelf: 'flex-start' }}>
            {english ? 'Back to event' : 'Volver al evento'}
          </Button>
          <Card variant="outlined" sx={{ borderRadius: 4 }}>
            <CardContent sx={{ p: { xs: 3, md: 5 } }}>
              <Stack spacing={2}>
                <Chip icon={<ConfirmationNumberIcon />} label={english ? 'Official TDF checkout' : 'Checkout oficial TDF'} color="primary" sx={{ alignSelf: 'flex-start' }} />
                <Typography component="h1" variant="h3" fontWeight={900}>{storefront.data.title}</Typography>
                {storefront.data.description && <Typography color="text.secondary">{storefront.data.description}</Typography>}
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1.5}>
                  <Chip icon={<EventIcon />} label={date(storefront.data.startsAt)} variant="outlined" />
                  {storefront.data.venueName && <Chip icon={<PlaceIcon />} label={storefront.data.venueName} variant="outlined" />}
                </Stack>
                <Alert severity="info">{english
                  ? 'Prices, discounts, fees, taxes, capacity, and the temporary hold are calculated and enforced by the server.'
                  : 'Precios, descuentos, tarifas, impuestos, capacidad y retención temporal se calculan y validan en el servidor.'}</Alert>
              </Stack>
            </CardContent>
          </Card>

          {!checkout ? (
            <Card variant="outlined">
              <CardContent>
                <Stack spacing={2} component="form" onSubmit={(event) => { event.preventDefault(); void handleCreateCheckout(); }}>
                  <Typography variant="h5" fontWeight={800}>{english ? 'Choose tickets' : 'Elige tus entradas'}</Typography>
                  {!storefront.data.checkoutAvailable && (
                    <Alert severity="warning">{storefront.data.unavailableReason
                      ?? (english ? 'Checkout is currently disabled.' : 'El checkout está deshabilitado.')}</Alert>
                  )}
                  <TextField select required label={english ? 'Ticket type' : 'Tipo de entrada'} value={tierId} onChange={(event) => setTierId(event.target.value)}>
                    {storefront.data.tiers.map((tier) => (
                      <MenuItem key={tier.tierId} value={String(tier.tierId)} disabled={tier.remaining <= 0}>
                        {tier.name} · {money(tier.unitPriceMinor, tier.currency)} · {tier.remaining} {english ? 'left' : 'disponibles'}
                      </MenuItem>
                    ))}
                  </TextField>
                  <TextField required type="number" label={english ? 'Quantity' : 'Cantidad'} value={quantity} onChange={(event) => setQuantity(event.target.value)} inputProps={{ min: 1, max: Math.min(100, selectedTier?.remaining ?? 1), step: 1 }} />
                  <TextField required label={english ? 'Full name' : 'Nombre completo'} value={buyerName} onChange={(event) => setBuyerName(event.target.value)} inputProps={{ maxLength: 160 }} />
                  <TextField required type="email" label="Email" value={buyerEmail} onChange={(event) => setBuyerEmail(event.target.value)} inputProps={{ maxLength: 254 }} />
                  <TextField label={english ? 'Phone (optional)' : 'Teléfono (opcional)'} value={buyerPhone} onChange={(event) => setBuyerPhone(event.target.value)} inputProps={{ maxLength: 24 }} />
                  <TextField label={english ? 'Promo code (optional)' : 'Código promocional (opcional)'} value={promoCode} onChange={(event) => setPromoCode(event.target.value)} inputProps={{ maxLength: 50 }} />
                  <FormControlLabel control={<Checkbox checked={termsAccepted} onChange={(event) => setTermsAccepted(event.target.checked)} />} label={english
                    ? 'I accept the versioned ticket terms and refund policy shown in the final server quote.'
                    : 'Acepto los términos versionados de entradas y reembolso incluidos en la cotización final del servidor.'} />
                  {message && <Alert severity="warning">{message}</Alert>}
                  <Button type="submit" variant="contained" size="large" disabled={!storefront.data.checkoutAvailable || !termsAccepted || submitting || !selectedTier || selectedTier.remaining <= 0}>
                    {submitting ? <CircularProgress size={22} color="inherit" /> : (english ? 'Hold tickets and review total' : 'Retener entradas y revisar total')}
                  </Button>
                </Stack>
              </CardContent>
            </Card>
          ) : (
            <Card variant="outlined">
              <CardContent>
                <Stack spacing={2}>
                  <Typography variant="h5" fontWeight={800}>{english ? 'Order status' : 'Estado de la orden'} #{checkout.orderId}</Typography>
                  {paid ? <Alert severity="success">{issued
                    ? (english ? 'Payment was verified by the server and the tickets were issued.' : 'El servidor verificó el pago y emitió las entradas.')
                    : (english ? 'Payment was verified. Ticket fulfillment is still pending.' : 'El pago fue verificado. La emisión de entradas todavía está pendiente.')}</Alert>
                    : <Alert severity="warning">{english
                      ? 'Tickets are held temporarily. The order is not paid and no ticket has been issued.'
                      : 'Las entradas están retenidas temporalmente. La orden no está pagada y no se emitió ninguna entrada.'}</Alert>}
                  <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                    <Chip label={`${english ? 'Face value' : 'Valor entradas'}: ${money(checkout.quote.netFaceValueMinor, checkout.quote.currency)}`} />
                    <Chip label={`${english ? 'Buyer fee' : 'Tarifa comprador'}: ${money(checkout.quote.buyerPlatformFeeMinor, checkout.quote.currency)}`} />
                    {checkout.quote.taxMinor > 0 && <Chip label={`${english ? 'Tax' : 'Impuesto'}: ${money(checkout.quote.taxMinor, checkout.quote.currency)}`} />}
                    <Chip color="primary" label={`${english ? 'Total' : 'Total'}: ${money(checkout.quote.checkoutTotalMinor, checkout.quote.currency)}`} />
                  </Stack>
                  {!paid && <Typography color="text.secondary">{english ? 'Hold expires' : 'La retención vence'} {date(checkout.holdExpiresAt)}.</Typography>}
                  {message && <Alert severity="warning">{message}</Alert>}
                  {!paid && checkout.paymentMethods.length === 0 && <Alert severity="info">{english
                    ? 'No real payment provider is enabled for this order. The hold does not mean payment.'
                    : 'No hay un proveedor real habilitado para esta orden. La retención no equivale a pago.'}</Alert>}
                  {!paid && (
                    <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                      {checkout.paymentMethods.includes('datafast') && <Button variant="contained" disabled={paymentBusy} onClick={() => void handleDatafast()}>Datafast</Button>}
                      {checkout.paymentMethods.includes('paypal') && <Button variant="outlined" disabled={paymentBusy || !paypalClientId || !paypalReady} onClick={() => void handlePaypal()}>PayPal</Button>}
                    </Stack>
                  )}
                  {paid && checkout.tickets.length > 0 && (
                    <Stack spacing={1}>
                      <Typography variant="h6">{english ? 'Issued tickets' : 'Entradas emitidas'}</Typography>
                      {checkout.tickets.map((ticket) => <Alert key={ticket.ticketId} severity="success" icon={<ConfirmationNumberIcon />}>
                        {ticket.ticketCode} · {ticket.status}
                      </Alert>)}
                    </Stack>
                  )}
                  <Typography variant="caption" color="text.secondary">{english
                    ? `Payment status: ${checkout.paymentStatus}. Fulfillment status: ${checkout.fulfillmentStatus}. These states are independent.`
                    : `Pago: ${checkout.paymentStatus}. Cumplimiento: ${checkout.fulfillmentStatus}. Son estados independientes.`}</Typography>
                </Stack>
              </CardContent>
            </Card>
          )}
        </Stack>
      </Container>

      <Dialog open={datafastOpen} onClose={() => setDatafastOpen(false)} maxWidth="xs" fullWidth>
        <DialogTitle>{english ? 'Pay with Datafast' : 'Pagar con Datafast'}</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={1.5}>
            <Alert severity="info">{english
              ? 'Datafast hosts the card form. Returning to TDF does not mean payment; the server verifies the provider resource, amount, currency, merchant, and order.'
              : 'Datafast aloja el formulario. Volver a TDF no significa pago; el servidor verifica recurso, importe, moneda, comercio y orden.'}</Alert>
            {datafastCheckout && datafastReturnUrl && <Box ref={datafastFormRef} key={datafastWidgetKey} sx={{ minHeight: 360 }}>
              <form action={datafastReturnUrl} className="paymentWidgets" data-brands="VISA MASTER DINERS AMEX DISCOVER" />
            </Box>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDatafastWidgetKey((current) => current + 1)}>{english ? 'Reload' : 'Recargar'}</Button>
          <Button color="inherit" onClick={() => setDatafastOpen(false)}>{english ? 'Close' : 'Cerrar'}</Button>
        </DialogActions>
      </Dialog>

      <Dialog open={paypalOpen} onClose={() => setPaypalOpen(false)} maxWidth="xs" fullWidth>
        <DialogTitle>{english ? 'Pay with PayPal' : 'Pagar con PayPal'}</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={1.5}>
            <Alert severity="info">{english
              ? 'PayPal approval is not payment success. TDF captures and verifies the immutable order on the server.'
              : 'Aprobar en PayPal no significa pago exitoso. TDF captura y verifica la orden inmutable en el servidor.'}</Alert>
            <Box ref={paypalButtonRef} sx={{ minHeight: 48 }} />
          </Stack>
        </DialogContent>
        <DialogActions><Button color="inherit" onClick={() => setPaypalOpen(false)}>{english ? 'Close' : 'Cerrar'}</Button></DialogActions>
      </Dialog>
    </Box>
  );
}
