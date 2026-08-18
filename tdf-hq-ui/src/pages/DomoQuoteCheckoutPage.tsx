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
  Divider,
  FormControlLabel,
  Stack,
  Typography,
} from '@mui/material';
import EventAvailableIcon from '@mui/icons-material/EventAvailable';
import LockIcon from '@mui/icons-material/Lock';
import ReceiptLongIcon from '@mui/icons-material/ReceiptLong';
import { useEffect, useMemo, useRef, useState } from 'react';
import { Link as RouterLink, useLocation, useNavigate, useParams } from 'react-router-dom';

import { DomoQuotes, type PublicDomoQuote } from '../api/domoQuotes';
import type { DatafastCheckoutDTO } from '../api/types';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { useMetaTags } from '../hooks/useMetaTags';
import { loadDomoQuoteLookupToken } from '../utils/domoQuoteAccess';

const quoteIdPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;

export default function DomoQuoteCheckoutPage() {
  const { quoteId = '' } = useParams<{ quoteId: string }>();
  const validQuoteId = quoteIdPattern.test(quoteId);
  const lookupToken = useMemo(
    () => validQuoteId ? loadDomoQuoteLookupToken(quoteId) : null,
    [quoteId, validQuoteId],
  );
  const location = useLocation();
  const navigate = useNavigate();
  const { locale } = useLocalePreferences();
  const english = locale.toLowerCase().startsWith('en');
  const [quote, setQuote] = useState<PublicDomoQuote | null>(null);
  const [termsAccepted, setTermsAccepted] = useState(false);
  const [loading, setLoading] = useState(validQuoteId && Boolean(lookupToken));
  const [busy, setBusy] = useState(false);
  const [message, setMessage] = useState<string | null>(null);
  const [datafastCheckout, setDatafastCheckout] = useState<DatafastCheckoutDTO | null>(null);
  const [datafastOpen, setDatafastOpen] = useState(false);
  const [datafastWidgetKey, setDatafastWidgetKey] = useState(0);
  const datafastFormRef = useRef<HTMLDivElement | null>(null);
  const [paypalReady, setPaypalReady] = useState(false);
  const [paypalOpen, setPaypalOpen] = useState(false);
  const [paypalOrderId, setPaypalOrderId] = useState<string | null>(null);
  const paypalButtonRef = useRef<HTMLDivElement | null>(null);
  const paypalClientId = import.meta.env?.VITE_PAYPAL_CLIENT_ID?.trim() ?? '';

  useMetaTags({
    title: english ? 'Domo quote · TDF Records' : 'Cotización Domo · TDF Records',
    description: english
      ? 'Secure review, acceptance, deposit payment, and status for a Domo del Pululahua quote.'
      : 'Revisión, aceptación, pago de depósito y estado seguro de una cotización del Domo del Pululahua.',
    robots: 'noindex,nofollow',
  });

  useEffect(() => {
    if (!validQuoteId || !lookupToken) {
      setLoading(false);
      return;
    }
    const query = new URLSearchParams(location.search);
    const resourcePath = query.get('resourcePath') ?? query.get('id');
    setLoading(true);
    setMessage(null);
    const request = resourcePath
      ? DomoQuotes.confirmDatafastStatus(quoteId, resourcePath, lookupToken)
      : DomoQuotes.getQuote(quoteId, lookupToken);
    request
      .then((response) => {
        setQuote(response);
        if (resourcePath) navigate(location.pathname, { replace: true });
      })
      .catch(() => setMessage(english
        ? 'The server could not verify this quote. No date or payment is shown as confirmed.'
        : 'El servidor no pudo verificar esta cotización. No mostramos fecha ni pago como confirmados.'))
      .finally(() => setLoading(false));
  }, [english, location.pathname, location.search, lookupToken, navigate, quoteId, validQuoteId]);

  const money = (minor: number, currency: string) => new Intl.NumberFormat(locale, {
    style: 'currency',
    currency,
  }).format(minor / 100);
  const date = (value: string, timezone = 'America/Guayaquil') => new Intl.DateTimeFormat(locale, {
    dateStyle: 'full',
    timeStyle: 'short',
    timeZone: timezone,
  }).format(new Date(value));

  const handleAccept = async () => {
    if (!quote || !lookupToken || !termsAccepted) return;
    setBusy(true);
    setMessage(null);
    try {
      setQuote(await DomoQuotes.acceptQuote(quote.quoteId, lookupToken));
    } catch {
      setMessage(english
        ? 'The quote could not be accepted. It remains unpaid and the hold may have expired.'
        : 'No pudimos aceptar la cotización. Sigue sin pago y la retención puede haber vencido.');
    } finally {
      setBusy(false);
    }
  };

  const handleDatafast = async () => {
    if (!quote || !lookupToken) return;
    setBusy(true);
    setMessage(null);
    try {
      setDatafastCheckout(await DomoQuotes.createDatafastCheckout(quote.quoteId, lookupToken));
      setDatafastOpen(true);
      setDatafastWidgetKey((current) => current + 1);
    } catch {
      setMessage(english
        ? 'Datafast could not be started. The deposit remains unpaid.'
        : 'No pudimos iniciar Datafast. El depósito sigue sin pago confirmado.');
    } finally {
      setBusy(false);
    }
  };

  const handlePaypal = async () => {
    if (!quote || !lookupToken || !paypalClientId) return;
    setBusy(true);
    setMessage(null);
    try {
      const provider = await DomoQuotes.createPaypalOrder(quote.quoteId, lookupToken);
      setPaypalOrderId(provider.pcPaypalOrderId);
      setPaypalOpen(true);
    } catch {
      setMessage(english
        ? 'PayPal could not be started. The deposit remains unpaid.'
        : 'No pudimos iniciar PayPal. El depósito sigue sin pago confirmado.');
    } finally {
      setBusy(false);
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
    if (!quote?.paymentMethods.includes('paypal') || !paypalClientId || typeof window === 'undefined') return;
    if (window.paypal) {
      setPaypalReady(true);
      return;
    }
    const script = document.createElement('script');
    script.src = `https://www.paypal.com/sdk/js?client-id=${encodeURIComponent(paypalClientId)}&currency=${encodeURIComponent(quote.currency)}`;
    script.async = true;
    script.onload = () => setPaypalReady(true);
    script.onerror = () => setMessage(english
      ? 'PayPal did not load. No payment was confirmed.'
      : 'PayPal no cargó. No se confirmó ningún pago.');
    document.body.appendChild(script);
    return () => script.remove();
  }, [english, paypalClientId, quote?.currency, quote?.paymentMethods]);

  useEffect(() => {
    if (!paypalOpen || !paypalReady || !paypalOrderId || !quote || !lookupToken
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
        setBusy(true);
        try {
          const response = await DomoQuotes.capturePaypalOrder(
            quote.quoteId,
            paypalOrderId,
            lookupToken,
          );
          setQuote(response);
          setPaypalOpen(false);
          setPaypalOrderId(null);
          setMessage(response.paymentStatus === 'paid' ? null : (english
            ? 'PayPal returned, but the server has not confirmed the deposit.'
            : 'PayPal respondió, pero el servidor todavía no confirmó el depósito.'));
        } catch {
          setMessage(english
            ? 'The server could not verify PayPal. The deposit is not shown as paid.'
            : 'El servidor no pudo verificar PayPal. No mostramos el depósito como pagado.');
        } finally {
          setBusy(false);
        }
      },
      onCancel: () => setMessage(english
        ? 'PayPal was cancelled. The deposit remains unpaid.'
        : 'Cancelaste PayPal. El depósito sigue sin pago confirmado.'),
      onError: () => setMessage(english
        ? 'PayPal did not complete. No payment was confirmed.'
        : 'PayPal no completó la operación. No se confirmó ningún pago.'),
    });
    void buttons.render(paypalButtonRef.current);
    return () => buttons.close?.();
  }, [english, lookupToken, paypalOpen, paypalOrderId, paypalReady, quote]);

  if (!validQuoteId || !lookupToken) {
    return <Container sx={{ py: 8 }}><Alert severity="error">{english
      ? 'This browser does not have secure access to that quote.'
      : 'Este navegador no tiene el acceso seguro de esa cotización.'}</Alert></Container>;
  }
  if (loading) {
    return <Stack minHeight="60vh" alignItems="center" justifyContent="center"><CircularProgress /></Stack>;
  }
  if (!quote) {
    return <Container sx={{ py: 8 }}><Alert severity="error">{message ?? (english
      ? 'The quote is unavailable.'
      : 'La cotización no está disponible.')}</Alert></Container>;
  }

  const paid = quote.paymentStatus === 'paid' && quote.quoteStatus === 'deposit_paid';
  const expired = quote.quoteStatus === 'expired' || quote.paymentStatus === 'expired';
  const canAccept = ['sent', 'viewed'].includes(quote.quoteStatus) && !expired;
  const awaitingPayment = quote.quoteStatus === 'deposit_due' && !paid && !expired;
  const returnUrl = typeof window === 'undefined'
    ? ''
    : new URL(`/domo-del-pululahua/cotizaciones/${quote.quoteId}`, window.location.origin).toString();

  return (
    <Box component="main" id="main-content" sx={{ bgcolor: '#faf8f3', minHeight: '100vh', py: { xs: 4, md: 7 } }}>
      <Container maxWidth="md">
        <Stack spacing={3}>
          <Button component={RouterLink} to="/domo-del-pululahua" sx={{ alignSelf: 'flex-start' }}>
            {english ? 'Back to the Domo' : 'Volver al Domo'}
          </Button>
          <Card variant="outlined" sx={{ borderRadius: 4 }}>
            <CardContent sx={{ p: { xs: 3, md: 5 } }}>
              <Stack spacing={2}>
                <Chip icon={<LockIcon />} label={english ? 'Private server quote' : 'Cotización privada del servidor'} color="primary" sx={{ alignSelf: 'flex-start' }} />
                <Typography component="h1" variant="h3" fontWeight={900}>{english ? 'Domo del Pululahua quote' : 'Cotización Domo del Pululahua'}</Typography>
                <Typography color="text.secondary">{english
                  ? `Rate card ${quote.rateCardVersion}. Quote ${quote.quoteId}.`
                  : `Tarifario ${quote.rateCardVersion}. Cotización ${quote.quoteId}.`}</Typography>
                <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
                  <Chip icon={<EventAvailableIcon />} label={date(quote.startsAt, quote.timezone)} variant="outlined" />
                  <Chip label={`${quote.guests} ${english ? 'guests' : 'invitados'}`} variant="outlined" />
                  <Chip label={quote.eventType} variant="outlined" />
                </Stack>
                {paid ? <Alert severity="success">{english
                  ? 'The server verified the deposit and reserved the date. This does not mean the event is completed; the remaining balance and fulfillment continue separately.'
                  : 'El servidor verificó el depósito y reservó la fecha. Esto no significa que el evento esté completado; el saldo y el cumplimiento continúan por separado.'}</Alert>
                  : expired ? <Alert severity="error">{english
                    ? 'The quote hold expired without a verified deposit. The date is not reserved.'
                    : 'La retención venció sin depósito verificado. La fecha no está reservada.'}</Alert>
                    : <Alert severity="warning">{english
                      ? 'The date is held only until the stated expiry. It is not reserved and the deposit is not paid.'
                      : 'La fecha solo está retenida hasta el vencimiento indicado. No está reservada y el depósito no está pagado.'}</Alert>}
              </Stack>
            </CardContent>
          </Card>

          <Card variant="outlined">
            <CardContent>
              <Stack spacing={2}>
                <Typography variant="h5" fontWeight={800} display="flex" gap={1} alignItems="center"><ReceiptLongIcon />{english ? 'Authoritative breakdown' : 'Desglose autoritativo'}</Typography>
                {quote.lines.map((line) => (
                  <Stack key={line.code} direction="row" justifyContent="space-between" spacing={2}>
                    <Typography>{line.description} × {line.quantity}</Typography>
                    <Typography fontWeight={700}>{money(line.subtotalMinor, quote.currency)}</Typography>
                  </Stack>
                ))}
                <Divider />
                <Stack direction="row" justifyContent="space-between"><Typography>{english ? 'Subtotal' : 'Subtotal'}</Typography><Typography>{money(quote.subtotalMinor, quote.currency)}</Typography></Stack>
                <Stack direction="row" justifyContent="space-between"><Typography>{english ? 'Tax' : 'Impuesto'}</Typography><Typography>{money(quote.taxMinor, quote.currency)}</Typography></Stack>
                <Stack direction="row" justifyContent="space-between"><Typography fontWeight={800}>{english ? 'Total event quote' : 'Total cotizado'}</Typography><Typography fontWeight={800}>{money(quote.totalMinor, quote.currency)}</Typography></Stack>
                <Stack direction="row" justifyContent="space-between"><Typography color="primary" fontWeight={900}>{english ? 'Initial deposit due' : 'Depósito inicial'}</Typography><Typography color="primary" fontWeight={900}>{money(quote.depositMinor, quote.currency)}</Typography></Stack>
                <Stack direction="row" justifyContent="space-between"><Typography>{english ? 'Remaining balance' : 'Saldo restante'}</Typography><Typography>{money(quote.balanceMinor, quote.currency)}</Typography></Stack>
                {!paid && <Typography color="text.secondary">{english ? 'Hold expires' : 'La retención vence'}: {date(quote.holdExpiresAt, quote.timezone)}.</Typography>}
                {canAccept && <>
                  <FormControlLabel control={<Checkbox checked={termsAccepted} onChange={(event) => setTermsAccepted(event.target.checked)} />} label={english
                    ? `I accept the exact quote and terms version ${quote.termsVersion}. Acceptance does not mean payment.`
                    : `Acepto esta cotización exacta y la versión de términos ${quote.termsVersion}. Aceptar no significa pagar.`} />
                  <Button variant="contained" disabled={!termsAccepted || busy} onClick={() => void handleAccept()}>{english ? 'Accept quote and choose deposit method' : 'Aceptar cotización y elegir método de depósito'}</Button>
                </>}
                {message && <Alert severity="warning">{message}</Alert>}
                {awaitingPayment && quote.paymentMethods.length === 0 && <Alert severity="info">{english
                  ? 'No real payment provider is enabled. The date remains only temporarily held.'
                  : 'No hay un proveedor real habilitado. La fecha sigue únicamente retenida de forma temporal.'}</Alert>}
                {awaitingPayment && <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
                  {quote.paymentMethods.includes('datafast') && <Button variant="contained" disabled={busy} onClick={() => void handleDatafast()}>Datafast</Button>}
                  {quote.paymentMethods.includes('paypal') && <Button variant="outlined" disabled={busy || !paypalClientId || !paypalReady} onClick={() => void handlePaypal()}>PayPal</Button>}
                </Stack>}
                <Typography variant="caption" color="text.secondary">{english
                  ? `Quote: ${quote.quoteStatus}. Payment: ${quote.paymentStatus}. Venue fulfillment: ${quote.fulfillmentStatus}. These states are independent.`
                  : `Cotización: ${quote.quoteStatus}. Pago: ${quote.paymentStatus}. Cumplimiento del espacio: ${quote.fulfillmentStatus}. Son estados independientes.`}</Typography>
              </Stack>
            </CardContent>
          </Card>
        </Stack>
      </Container>

      <Dialog open={datafastOpen} onClose={() => setDatafastOpen(false)} maxWidth="xs" fullWidth>
        <DialogTitle>{english ? 'Pay deposit with Datafast' : 'Pagar depósito con Datafast'}</DialogTitle>
        <DialogContent dividers>
          <Stack spacing={1.5}>
            <Alert severity="info">{english
              ? 'Datafast hosts the card form. Returning here is not payment; the server verifies the exact provider resource, amount, currency, merchant, quote, and checkout.'
              : 'Datafast aloja el formulario. Volver aquí no significa pago; el servidor verifica recurso, importe, moneda, comercio, cotización y checkout exactos.'}</Alert>
            {datafastCheckout && returnUrl && <Box ref={datafastFormRef} key={datafastWidgetKey} sx={{ minHeight: 360 }}>
              <form action={returnUrl} className="paymentWidgets" data-brands="VISA MASTER DINERS AMEX DISCOVER" />
            </Box>}
          </Stack>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDatafastWidgetKey((current) => current + 1)}>{english ? 'Reload' : 'Recargar'}</Button>
          <Button color="inherit" onClick={() => setDatafastOpen(false)}>{english ? 'Close' : 'Cerrar'}</Button>
        </DialogActions>
      </Dialog>

      <Dialog open={paypalOpen} onClose={() => setPaypalOpen(false)} maxWidth="xs" fullWidth>
        <DialogTitle>{english ? 'Pay deposit with PayPal' : 'Pagar depósito con PayPal'}</DialogTitle>
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
