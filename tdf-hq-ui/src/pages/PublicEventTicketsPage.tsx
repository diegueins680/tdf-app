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
import HostedProviderCheckout from '../components/payments/HostedProviderCheckout';
import TicketBankTransferPanel from '../components/payments/TicketBankTransferPanel';
import TicketCredentialQR from '../components/TicketCredentialQR';
import { LegalDisclosure } from '../components/legal/LegalDisclosure';
import MobilePromo from '../mobile/MobilePromo';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useMetaTags } from '../hooks/useMetaTags';
import { env } from '../utils/env';
import { useTicketFunnel } from '../analytics/useTicketFunnel';

const makeIdempotencyKey = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `event-ticket-checkout-${crypto.randomUUID()}`;
  }
  return `event-ticket-checkout-${Date.now()}-${Math.random().toString(16).slice(2)}`;
};

const lookupStorageKey = (eventId: number, orderId: number) =>
  `tdf:event-ticket-checkout:${eventId}:${orderId}`;

const percentage = (basisPoints: number, locale: string): string =>
  new Intl.NumberFormat(locale, {
    style: 'percent',
    minimumFractionDigits: 0,
    maximumFractionDigits: 2,
  }).format(basisPoints / 10_000);

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
  const trackFunnel = useTicketFunnel();
  const navigate = useNavigate();
  const { locale, timezone } = useLocalePreferences();
  const english = locale.toLowerCase().startsWith('en');
  const checkoutPrefill = useMemo(() => new URLSearchParams(location.search), [location.search]);
  const prefilledTierId = checkoutPrefill.get('tierId') ?? '';
  const prefilledQuantity = checkoutPrefill.get('quantity') ?? '1';
  const [tierId, setTierId] = useState(() =>
    /^[1-9]\d*$/.test(prefilledTierId) ? prefilledTierId : '');
  const [quantity, setQuantity] = useState(() => {
    const parsed = Number(prefilledQuantity);
    return Number.isSafeInteger(parsed) && parsed >= 1 && parsed <= 10 ? String(parsed) : '1';
  });
  const [buyerName, setBuyerName] = useState('');
  const [buyerEmail, setBuyerEmail] = useState('');
  const [buyerPhone, setBuyerPhone] = useState('');
  const [promoCode, setPromoCode] = useState('');
  const [billingIdType, setBillingIdType] = useState<'consumidor_final' | 'cedula' | 'ruc' | 'pasaporte'>('consumidor_final');
  const [billingIdNumber, setBillingIdNumber] = useState('');
  const [billingName, setBillingName] = useState('');
  const [termsAccepted, setTermsAccepted] = useState(false);
  const [checkout, setCheckout] = useState<PublicEventTicketCheckout | null>(null);
  const [submitting, setSubmitting] = useState(false);
  const [paymentBusy, setPaymentBusy] = useState(false);
  const [hostedPaymentLocked, setHostedPaymentLocked] = useState(false);
  const [message, setMessage] = useState<string | null>(null);
  const idempotency = useRef<{ fingerprint: string; key: string } | null>(null);
  const [datafastCheckout, setDatafastCheckout] = useState<DatafastCheckoutDTO | null>(null);
  const [datafastOpen, setDatafastOpen] = useState(false);
  const [datafastWidgetKey, setDatafastWidgetKey] = useState(0);
  const datafastFormRef = useRef<HTMLDivElement | null>(null);
  const [paypalReady, setPaypalReady] = useState(false);
  const [paypalOpen, setPaypalOpen] = useState(false);
  const [paypalOrderId, setPaypalOrderId] = useState<string | null>(null);
  const [paypalButtonContainer, setPaypalButtonContainer] = useState<HTMLDivElement | null>(null);
  const paypalClientId = env.read('VITE_PAYPAL_CLIENT_ID') ?? '';

  const storefront = useQuery({
    queryKey: ['public-event-ticket-storefront', eventId],
    queryFn: () => EventTickets.getStorefront(eventId),
    enabled: validEventId,
    retry: false,
  });

  useEffect(() => {
    if (tierId && storefront.data?.tiers.some((tier) => String(tier.tierId) === tierId)) return;
    const firstTier = storefront.data?.tiers[0];
    if (firstTier) setTierId(String(firstTier.tierId));
  }, [storefront.data?.tiers, tierId]);

  useEffect(() => {
    const selected = storefront.data?.tiers.find((tier) => String(tier.tierId) === tierId);
    const count = Number(quantity);
    if (routeOrderId != null || checkout || !storefront.data?.checkoutAvailable || !selected || !Number.isSafeInteger(count)
        || count < 1 || count > selected.remaining || count > (storefront.data.policy?.maxTicketsPerOrder ?? 100)) return;
    trackFunnel('ticket_selected', { eventId, tierId: selected.tierId, quantity: count });
  }, [checkout, eventId, quantity, routeOrderId, storefront.data, tierId, trackFunnel]);

  useEffect(() => {
    if (checkout?.paymentStatus !== 'paid') return;
    const observation = { eventId: checkout.eventId, quantity: checkout.quote.quantity,
      privateScope: `order:${checkout.orderId}` };
    trackFunnel('payment_completed', observation);
    if (checkout.fulfillmentStatus === 'issued' && checkout.tickets.some((ticket) => ticket.status === 'issued')) {
      trackFunnel('ticket_issued', observation);
      trackFunnel('ticket_opened', observation);
    }
  }, [checkout, trackFunnel]);

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
    title,
    description,
    canonical: validEventId && typeof window !== 'undefined'
      ? `${window.location.origin}/eventos/${eventId}` : undefined,
    ogType: 'website',
    // The event detail is the discovery page. Checkout/receipt routes must not
    // advertise face value as the final price or stock as verified payment access.
    robots: 'noindex,follow',
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
    const maximumQuantity = storefront.data.policy?.maxTicketsPerOrder ?? 100;
    if (selectedQuantity > maximumQuantity) {
      setMessage(english
        ? `You can buy up to ${maximumQuantity} tickets per order.`
        : `Puedes comprar hasta ${maximumQuantity} entradas por orden.`);
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
      ...(storefront.data.policy?.taxInvoiceIssued
        ? {
          billingIdType,
          ...(billingIdType !== 'consumidor_final'
            ? { billingIdNumber: billingIdNumber.trim(), billingName: billingName.trim() }
            : {}),
        }
        : {}),
    };
    const fingerprint = JSON.stringify(payload);
    if (idempotency.current?.fingerprint !== fingerprint) {
      idempotency.current = { fingerprint, key: makeIdempotencyKey() };
    }
    trackFunnel('checkout_started', { eventId, tierId: selectedTierId, quantity: selectedQuantity,
      hasPromotion: Boolean(promoCode.trim()), privateScope: idempotency.current.key });
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
      trackFunnel('payment_initiated', { eventId: checkout.eventId, quantity: checkout.quote.quantity,
        provider: 'datafast', privateScope: `order:${checkout.orderId}` });
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
      trackFunnel('payment_initiated', { eventId: checkout.eventId, quantity: checkout.quote.quantity,
        provider: 'paypal', privateScope: `order:${checkout.orderId}` });
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

  const handleBankTransfer = async () => {
    if (!checkout || !checkoutLookupToken) return;
    setPaymentBusy(true);
    setMessage(null);
    try {
      const response = await EventTickets.selectBankTransfer(
        checkout.eventId,
        checkout.orderId,
        checkoutLookupToken,
      );
      trackFunnel('payment_initiated', { eventId: checkout.eventId, quantity: checkout.quote.quantity,
        provider: 'bank_transfer', privateScope: `order:${checkout.orderId}` });
      setCheckout(response);
    } catch {
      setMessage(english
        ? 'Bank transfer could not be selected. The order remains unpaid.'
        : 'No pudimos seleccionar la transferencia bancaria. La orden sigue sin pago confirmado.');
    } finally {
      setPaymentBusy(false);
    }
  };

  const handleBankTransferEvidence = async (reference: string) => {
    if (!checkout || !checkoutLookupToken) return;
    setPaymentBusy(true);
    setMessage(null);
    try {
      setCheckout(await EventTickets.submitBankTransferEvidence(
        checkout.eventId,
        checkout.orderId,
        reference,
        checkoutLookupToken,
      ));
    } catch {
      setMessage(english
        ? 'We could not record your transfer reference. Nothing is shown as paid.'
        : 'No pudimos registrar la referencia de tu transferencia. No mostramos nada como pagado.');
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
        || !paypalButtonContainer || typeof window === 'undefined' || !window.paypal) return;
    paypalButtonContainer.innerHTML = '';
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
    void buttons.render(paypalButtonContainer);
    return () => buttons.close?.();
  }, [checkout, checkoutLookupToken, english, paypalButtonContainer, paypalOpen, paypalOrderId, paypalReady]);

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
    <Box sx={{ bgcolor: 'background.default', minHeight: '100vh', py: { xs: 4, md: 7 } }}>
      <Container maxWidth="md">
        <Stack spacing={3}>
          <Button component={RouterLink} to={`/eventos/${eventId}`} sx={{ alignSelf: 'flex-start' }}>
            {english ? 'Back to event' : 'Volver al evento'}
          </Button>
          <Card variant="outlined" sx={{ borderRadius: 4, overflow: 'hidden' }}>
            {storefront.data.imageUrl && (
              <Box
                component="img"
                src={storefront.data.imageUrl}
                alt={english ? `${storefront.data.title} artwork` : `Arte de ${storefront.data.title}`}
                loading="eager"
                sx={{ display: 'block', width: '100%', maxHeight: { xs: 520, md: 640 }, objectFit: 'contain', bgcolor: 'grey.900' }}
              />
            )}
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
                  ? 'Review the full total before paying. Your tickets are held for the time shown.'
                  : 'Revisa el total antes de pagar. Tus entradas se reservan durante el tiempo indicado.'}</Alert>
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
                  <TextField required type="number" label={english ? 'Quantity' : 'Cantidad'} value={quantity} onChange={(event) => setQuantity(event.target.value)} inputProps={{ min: 1, max: Math.min(storefront.data.policy?.maxTicketsPerOrder ?? 100, selectedTier?.remaining ?? 1), step: 1 }}
                    helperText={english
                      ? `Up to ${storefront.data.policy?.maxTicketsPerOrder ?? 100} tickets per order.`
                      : `Hasta ${storefront.data.policy?.maxTicketsPerOrder ?? 100} entradas por orden.`}
                  />
                  <TextField required label={english ? 'Full name' : 'Nombre completo'} value={buyerName} onChange={(event) => setBuyerName(event.target.value)} inputProps={{ maxLength: 160 }} />
                  <TextField required type="email" label="Email" value={buyerEmail} onChange={(event) => setBuyerEmail(event.target.value)} inputProps={{ maxLength: 254 }} />
                  <TextField label={english ? 'Phone (optional)' : 'Teléfono (opcional)'} value={buyerPhone} onChange={(event) => setBuyerPhone(event.target.value)} inputProps={{ maxLength: 24 }} />
                  <TextField label={english ? 'Promo code (optional)' : 'Código promocional (opcional)'} value={promoCode} onChange={(event) => setPromoCode(event.target.value)} inputProps={{ maxLength: 50 }} />
                  {storefront.data.policy?.taxInvoiceIssued && (
                    <Stack spacing={1.5} component="fieldset" sx={{ border: 0, p: 0, m: 0 }}>
                      <Typography component="legend" variant="subtitle2" fontWeight={800}>
                        {english ? 'Electronic invoice details' : 'Datos para tu factura electrónica'}
                      </Typography>
                      <TextField select label={english ? 'Invoice to' : 'Facturar a'} value={billingIdType}
                        onChange={(event) => setBillingIdType(event.target.value as typeof billingIdType)}
                        helperText={english
                          ? 'Final consumer is allowed up to USD 50. Above that, enter an ID.'
                          : 'Consumidor final se permite hasta USD 50. Sobre ese valor, ingresa una identificación.'}>
                        <MenuItem value="consumidor_final">{english ? 'Final consumer' : 'Consumidor final'}</MenuItem>
                        <MenuItem value="cedula">{english ? 'Ecuadorian ID (cédula)' : 'Cédula'}</MenuItem>
                        <MenuItem value="ruc">RUC</MenuItem>
                        <MenuItem value="pasaporte">{english ? 'Passport' : 'Pasaporte'}</MenuItem>
                      </TextField>
                      {billingIdType !== 'consumidor_final' && (
                        <>
                          <TextField required label={english ? 'ID number' : 'Número de identificación'} value={billingIdNumber}
                            onChange={(event) => setBillingIdNumber(event.target.value)} inputProps={{ maxLength: 20, inputMode: billingIdType === 'pasaporte' ? 'text' : 'numeric' }} />
                          <TextField required label={english ? 'Name or business name' : 'Nombre o razón social'} value={billingName}
                            onChange={(event) => setBillingName(event.target.value)} inputProps={{ maxLength: 300 }} />
                        </>
                      )}
                    </Stack>
                  )}
                  {storefront.data.policy ? (
                    <Alert severity="info">
                      <Stack spacing={0.75}>
                        <Typography variant="subtitle2" fontWeight={800}>
                          {english ? 'Ticket terms and fees' : 'Términos y tarifas de las entradas'}
                        </Typography>
                        <Typography variant="body2">
                          {english ? 'Buyer fee' : 'Tarifa al comprador'}: {percentage(storefront.data.policy.buyerFeeBps, locale)} ·{' '}
                          {english ? 'Organizer fee (deducted from payout)' : 'Tarifa al organizador (descontada del pago)'}: {percentage(storefront.data.policy.organizerFeeBps, locale)} ·{' '}
                          {storefront.data.policy.taxIncluded ? (english ? 'Included tax' : 'Impuesto incluido') : (english ? 'Additional tax' : 'Impuesto adicional')}: {percentage(storefront.data.policy.taxBps, locale)}
                        </Typography>
                        <Typography variant="body2">
                          {english ? 'Temporary inventory hold' : 'Retención temporal de inventario'}: {storefront.data.policy.holdMinutes} {english ? 'minutes' : 'minutos'} ·{' '}
                          {english ? 'Transfers' : 'Transferencias'}: {storefront.data.policy.transferAllowed
                            ? (english ? 'allowed' : 'permitidas')
                            : (english ? 'not allowed' : 'no permitidas')}
                        </Typography>
                        {storefront.data.policy.bankTransferAvailableUntil && (
                          <Typography variant="body2">
                            {english
                              ? `Bank transfer available until ${date(storefront.data.policy.bankTransferAvailableUntil)}; tickets are issued after the deposit is confirmed.`
                              : `Transferencia bancaria disponible hasta el ${date(storefront.data.policy.bankTransferAvailableUntil)}; las entradas se emiten al confirmar el depósito.`}
                          </Typography>
                        )}
                      </Stack>
                    </Alert>
                  ) : (
                    <Alert severity="warning">
                      {english
                        ? 'Ticket terms are not available, so checkout cannot continue.'
                        : 'Los términos de las entradas no están disponibles, por lo que el checkout no puede continuar.'}
                    </Alert>
                  )}
                  {storefront.data.policy && (
                    <Stack spacing={1}>
                      <LegalDisclosure
                        id="ticket-terms"
                        language={english ? 'en' : 'es'}
                        title={english ? 'Ticket terms' : 'Términos de las entradas'}
                        summary={`${english ? 'Terms version' : 'Versión de términos'}: ${storefront.data.policy.termsVersion}`}
                      >
                        <Typography variant="body2" sx={{ whiteSpace: 'pre-wrap' }}>{storefront.data.policy.termsSummary}</Typography>
                      </LegalDisclosure>
                      <LegalDisclosure
                        id="ticket-refund-policy"
                        language={english ? 'en' : 'es'}
                        title={english ? 'Refund policy' : 'Política de reembolso'}
                      >
                        <Typography variant="body2" sx={{ whiteSpace: 'pre-wrap' }}>{storefront.data.policy.refundPolicy}</Typography>
                      </LegalDisclosure>
                    </Stack>
                  )}
                  <FormControlLabel control={<Checkbox disabled={!storefront.data.policy} checked={termsAccepted} onChange={(event) => setTermsAccepted(event.target.checked)} inputProps={{ 'aria-describedby': storefront.data.policy ? 'ticket-terms-button ticket-refund-policy-button' : undefined }} />} label={english
                    ? 'I accept the versioned ticket terms, fees, and refund policy above.'
                    : 'Acepto los términos versionados, las tarifas y la política de reembolso indicados arriba.'} />
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
                    {checkout.quote.taxMinor > 0 && <Chip label={`${checkout.quote.taxIncluded ? (english ? 'Included tax' : 'Impuesto incluido') : (english ? 'Tax' : 'Impuesto')}: ${money(checkout.quote.taxMinor, checkout.quote.currency)}`} />}
                    <Chip color="primary" label={`${english ? 'Total' : 'Total'}: ${money(checkout.quote.checkoutTotalMinor, checkout.quote.currency)}`} />
                  </Stack>
                  {!paid && <Typography color="text.secondary">{english ? 'Hold expires' : 'La retención vence'} {date(checkout.holdExpiresAt)}.</Typography>}
                  {message && <Alert severity="warning">{message}</Alert>}
                  {!paid && checkout.paymentMethods.length === 0 && <Alert severity="info">{english
                    ? 'No real payment provider is enabled for this order. The hold does not mean payment.'
                    : 'No hay un proveedor real habilitado para esta orden. La retención no equivale a pago.'}</Alert>}
                  <Stack spacing={1.5}>
                    {!paid && <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                        {checkout.paymentMethods.includes('datafast') && <Button variant="contained" disabled={paymentBusy || hostedPaymentLocked} onClick={() => void handleDatafast()}>Datafast</Button>}
                        {checkout.paymentMethods.includes('paypal') && <Button variant="outlined" disabled={paymentBusy || hostedPaymentLocked || !paypalClientId || !paypalReady} onClick={() => void handlePaypal()}>PayPal</Button>}
                        {checkout.paymentMethods.includes('bank_transfer') && !checkout.bankTransfer && <Button variant="outlined" disabled={paymentBusy || hostedPaymentLocked} onClick={() => void handleBankTransfer()}>{english ? 'Bank transfer' : 'Transferencia bancaria'}</Button>}
                    </Stack>}
                    {!paid && checkout.bankTransfer && (
                      <TicketBankTransferPanel
                        transfer={checkout.bankTransfer}
                        english={english}
                        amountLabel={money(checkout.bankTransfer.amountMinor, checkout.bankTransfer.currency)}
                        holdExpiresLabel={date(checkout.holdExpiresAt)}
                        busy={paymentBusy}
                        onSubmitReference={(reference) => void handleBankTransferEvidence(reference)}
                      />
                    )}
                    {checkoutLookupToken && (
                        <HostedProviderCheckout
                          checkout={{
                            checkoutId: checkout.checkoutId,
                            lookupToken: checkoutLookupToken,
                            returnPath: `/eventos/${checkout.eventId}/orden/${checkout.orderId}`,
                          }}
                          offeredMethods={paid ? [] : checkout.paymentMethods}
                          disabled={paid || paymentBusy || datafastOpen || paypalOpen}
                          english={english}
                          initialBuyerPhone={buyerPhone}
                          onSafetyLockChange={setHostedPaymentLocked}
                          onSessionChange={(session) => {
                            trackFunnel('payment_initiated', { eventId: checkout.eventId,
                              quantity: checkout.quote.quantity, provider: session.provider,
                              privateScope: `order:${checkout.orderId}` });
                          }}
                          onPaymentConfirmed={async () => {
                            setCheckout(await EventTickets.getCheckout(
                              checkout.eventId,
                              checkout.orderId,
                              checkoutLookupToken,
                            ));
                          }}
                        />
                    )}
                  </Stack>
                  {paid && issued && checkout.tickets.length > 0 && (
                    <Stack spacing={1}>
                      <Typography variant="h6">{english ? 'Issued tickets' : 'Entradas emitidas'}</Typography>
                      {checkout.tickets.map((ticket) => <Card key={ticket.ticketId} variant="outlined">
                        <CardContent>
                          <Typography gutterBottom>{ticket.holderName ?? title}</Typography>
                          {ticket.status === 'issued' && ticket.ticketCode
                            ? <TicketCredentialQR code={ticket.ticketCode} english={english} />
                            : <Alert severity="info">{ticket.status === 'checked_in'
                              ? (english ? 'Already used' : 'Entrada ya utilizada')
                              : (english ? 'Not available for entry' : 'No disponible para el acceso')}</Alert>}
                        </CardContent>
                      </Card>)}
                      <MobilePromo surface="ticket_confirmation" />
                    </Stack>
                  )}
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
              ? 'Complete payment in the secure form. Your tickets will appear here once payment is confirmed.'
              : 'Completa el pago en el formulario seguro. Tus entradas aparecerán aquí cuando se confirme el pago.'}</Alert>
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
              ? 'Complete payment with PayPal. Your tickets will appear here once payment is confirmed.'
              : 'Completa el pago con PayPal. Tus entradas aparecerán aquí cuando se confirme el pago.'}</Alert>
            <Box ref={setPaypalButtonContainer} sx={{ minHeight: 48 }} />
          </Stack>
        </DialogContent>
        <DialogActions><Button color="inherit" onClick={() => setPaypalOpen(false)}>{english ? 'Close' : 'Cerrar'}</Button></DialogActions>
      </Dialog>
    </Box>
  );
}
