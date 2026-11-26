import { Alert, Box, Button, Card, CardContent, CircularProgress, Container, Stack, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useEffect } from 'react';
import { Link as RouterLink, useLocation } from 'react-router-dom';
import { loadCheckoutPaymentAlternatives } from '../api/checkoutPaymentAlternatives';
import PaymentAlternativesNotice from '../components/payments/PaymentAlternativesNotice';

import { getProviderPaymentSession, safeProviderRedirect, submitPayPhoneButtonReturn } from '../api/providerPaymentSessions';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
import { env } from '../utils/env';
import {
  clearPaymentIdempotencyKey,
  clearProviderPaymentResume,
  loadProviderPaymentResume,
  markProviderPaymentResumeCompleted,
  paymentAttemptCanBeReleased,
} from '../utils/providerPaymentResume';

const POLLABLE_STATES = new Set([
  'prepared',
  'in_flight',
  'requires_customer_action',
  'processing',
  'ambiguous',
]);

export default function ProviderPaymentReturnPage() {
  const { locale } = useLocalePreferences();
  const english = locale.toLowerCase().startsWith('en');
  const { search } = useLocation();
  // The redirect can append its parameters with '?' after the checkout ID.
  const params = new URLSearchParams(search.replace(/([?&]tdf_checkout=[0-9a-f-]{36})\?/i, '$1&'));
  const checkoutId = params.get('tdf_checkout');
  const ids = params.getAll('id');
  const refs = params.getAll('clientTransactionId');
  const transactionId = Number(ids[0]);
  const clientReference = refs[0] ?? '';
  const hasButtonReturn = ids.length === 1 && /^[1-9]\d*$/.test(ids[0] ?? '')
    && Number.isSafeInteger(transactionId) && refs.length === 1 && /^TDFC-[A-Za-z0-9._-]{1,45}$/.test(clientReference);
  // The inbox deduplicates these hints. Only the server's confirmation can
  // change the order, including when the buyer returns in another browser.
  const buttonReturn = useQuery({
    queryKey: ['payphone-button-return', transactionId, clientReference],
    queryFn: () => submitPayPhoneButtonReturn(transactionId, clientReference),
    enabled: hasButtonReturn,
    staleTime: Infinity,
  });
  const resume = loadProviderPaymentResume(checkoutId ?? undefined);
  const status = useQuery({
    queryKey: ['provider-payment-session-return', resume?.checkoutId, resume?.attemptId],
    queryFn: () => getProviderPaymentSession(
      resume!.checkoutId,
      resume!.attemptId,
      resume!.lookupToken,
    ),
    enabled: Boolean(resume),
    // Each return needs fresh evidence. The session query client retries
    // transient read failures and stops on client errors.
    staleTime: 0,
    refetchOnWindowFocus: true,
    refetchInterval: (query) => {
      const state = query.state.data?.state;
      return state && POLLABLE_STATES.has(state) ? 4_000 : false;
    },
  });

  const resumedCheckoutId = resume?.checkoutId;
  const session = status.isSuccess && status.fetchStatus === 'idle' ? status.data : undefined;
  const knownNoCharge = paymentAttemptCanBeReleased(session?.state, session?.canRetryOrFallback);
  const alternatives = useQuery({
    queryKey: ['payment-return-alternatives', resumedCheckoutId, resume?.attemptId, resume?.returnPath],
    queryFn: () => loadCheckoutPaymentAlternatives(resume!),
    enabled: Boolean(resume && knownNoCharge),
    retry: false,
  });
  useEffect(() => {
    if (resumedCheckoutId && session?.state === 'succeeded') {
      markProviderPaymentResumeCompleted(resumedCheckoutId);
    }
  }, [resumedCheckoutId, session?.state]);

  const returnDeliveryError = hasButtonReturn && buttonReturn.isError && (
    <Alert severity="warning" action={
      <Button onClick={() => void buttonReturn.refetch()} disabled={buttonReturn.isFetching}>
        {english ? 'Retry' : 'Reintentar'}
      </Button>
    }>
      {english
        ? 'TDF could not receive the PayPhone reference. Retry sending it so TDF can verify the payment.'
        : 'TDF no pudo recibir la referencia de PayPhone. Reintenta el envío para que TDF pueda verificar el pago.'}
    </Alert>
  );

  if (!resume) {
    return (
      <Container maxWidth="sm" sx={{ py: 8 }}>
        {returnDeliveryError}
        <Alert severity="warning">
          {english
            ? 'This tab does not have the private payment recovery capability. Return to the original order or contact support.'
            : 'Desde esta pestaña no podemos consultar el pago. Vuelve a la orden original o contacta a soporte.'}
        </Alert>
      </Container>
    );
  }

  const succeeded = session?.state === 'succeeded';
  const ambiguous = Boolean(session && !knownNoCharge && !succeeded);
  const redirect = safeProviderRedirect(session?.provider, session?.redirectUrl);
  const paypalConfigured = Boolean(env.read('VITE_PAYPAL_CLIENT_ID'));
  const paypalAvailable = paypalConfigured
    && typeof window !== 'undefined' && Boolean(window.paypal?.Buttons);

  const releaseAttemptIfAuthoritative = () => {
    if (!knownNoCharge) return;
    clearProviderPaymentResume(resume.checkoutId);
    clearPaymentIdempotencyKey(resume.checkoutId, resume.provider, resume.paymentMethod);
  };

  return (
    <Box component="main" sx={{ minHeight: '70vh', py: { xs: 5, md: 9 } }}>
      <Container maxWidth="sm">
        <Card variant="outlined" sx={{ borderRadius: 4 }}>
          <CardContent sx={{ p: { xs: 3, md: 5 } }}>
            <Stack spacing={2.5}>
              {returnDeliveryError}
              <Typography component="h1" variant="h4" fontWeight={900}>
                {english ? 'Verifying payment' : 'Verificando el pago'}
              </Typography>
              {(status.isLoading || status.isFetching) && !session && (
                <Stack direction="row" spacing={1.5} alignItems="center">
                  <CircularProgress size={24} />
                  <Typography>{english ? 'Confirming your payment…' : 'Confirmando tu pago…'}</Typography>
                </Stack>
              )}
              {status.fetchStatus === 'paused' && (
                <Alert severity="info">{english
                  ? 'We’ll check this payment when your connection returns.'
                  : 'Consultaremos este pago cuando vuelva la conexión.'}</Alert>
              )}
              {status.isError && (
                <>
                  <Alert severity="warning">
                    {english
                      ? 'The verified status is temporarily unavailable. This attempt stays locked and no payment is shown as successful.'
                      : 'No pudimos consultar el estado del pago en este momento. Este intento sigue en espera y todavía no lo damos por pagado.'}
                  </Alert>
                  <Button variant="outlined" onClick={() => void status.refetch()} disabled={status.isFetching}>
                    {english ? 'Retry verified status' : 'Volver a consultar'}
                  </Button>
                </>
              )}
              {succeeded && (
                <Alert severity="success">
                  {english
                    ? 'The server verified the provider payment. Return to the order for its current fulfillment status.'
                    : 'Pago confirmado. Vuelve a la orden para ver el siguiente paso.'}
                </Alert>
              )}
              {knownNoCharge && (
                <Alert severity="info">
                  {english
                    ? 'The server confirmed that no charge completed. Return to the order to choose an available method.'
                    : 'No se realizó ningún cobro. Vuelve a la orden para elegir un método disponible.'}
                </Alert>
              )}
              {knownNoCharge && alternatives.isSuccess && (
                <PaymentAlternativesNotice
                  methods={alternatives.data.filter((method) => method !== 'paypal' || paypalAvailable)}
                  availabilityUnknown={status.isError || status.isFetching}
                  methodsToCheck={paypalConfigured && !paypalAvailable
                    ? alternatives.data.filter((method) => method === 'paypal') : []}
                  failedMethod={resume.paymentMethod === 'payphone_wallet'
                    ? 'payphone_wallet' : `${resume.provider}_${resume.paymentMethod}`}
                  blocked={status.isError || status.isFetching}
                  english={english}
                />
              )}
              {knownNoCharge && alternatives.isError && (
                <Alert severity="info">{english
                  ? 'Return to the original order to refresh its available payment methods.'
                  : 'Vuelve a la orden original para consultar sus medios de pago disponibles.'}</Alert>
              )}
              {ambiguous && (
                <Alert severity="warning">
                  {english
                    ? 'The result is pending or ambiguous. Do not pay again or use another provider while TDF reconciles it.'
                    : 'Todavía no sabemos si el pago se completó. No pagues otra vez ni uses otro método mientras lo revisamos.'}
                </Alert>
              )}
              {session?.state === 'requires_customer_action' && !hasButtonReturn && redirect && (
                <Button component="a" href={redirect} variant="contained"
                  referrerPolicy={session.provider === 'payphone' ? 'origin' : 'no-referrer'}>
                  {english ? 'Continue secure provider checkout' : 'Continuar con el pago'}
                </Button>
              )}
              {session && POLLABLE_STATES.has(session.state) && (
                <Button variant="outlined" onClick={() => void status.refetch()} disabled={status.isFetching}>
                  {english ? 'Refresh verified status' : 'Actualizar estado del pago'}
                </Button>
              )}
              <Button
                component={RouterLink}
                to={resume.returnPath}
                onClick={releaseAttemptIfAuthoritative}
              >
                {english ? 'Return to order' : 'Volver a la orden'}
              </Button>
              <Typography variant="caption" color="text.secondary">
                {english
                  ? 'Returning from a provider never proves payment by itself. TDF trusts only verified server-side evidence.'
                  : 'Un pago solo cuenta cuando lo confirmamos aquí, no por haber vuelto desde el procesador.'}
              </Typography>
            </Stack>
          </CardContent>
        </Card>
      </Container>
    </Box>
  );
}
