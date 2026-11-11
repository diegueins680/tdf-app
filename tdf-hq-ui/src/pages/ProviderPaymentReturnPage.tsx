import { Alert, Box, Button, Card, CardContent, CircularProgress, Container, Stack, Typography } from '@mui/material';
import { useQuery } from '@tanstack/react-query';
import { useEffect } from 'react';
import { Link as RouterLink, useSearchParams } from 'react-router-dom';

import { getProviderPaymentSession, safePlaceToPayRedirect } from '../api/providerPaymentSessions';
import { useLocalePreferences } from '../contexts/LocalePreferencesContext';
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
  const [searchParams] = useSearchParams();
  const checkoutId = searchParams.get('tdf_checkout');
  const resume = loadProviderPaymentResume(checkoutId ?? undefined);
  const status = useQuery({
    queryKey: ['provider-payment-session-return', resume?.checkoutId, resume?.attemptId],
    queryFn: () => getProviderPaymentSession(
      resume!.checkoutId,
      resume!.attemptId,
      resume!.lookupToken,
    ),
    enabled: Boolean(resume),
    retry: false,
    refetchOnWindowFocus: true,
    refetchInterval: (query) => {
      const state = query.state.data?.state;
      return state && POLLABLE_STATES.has(state) ? 4_000 : false;
    },
  });

  const resumedCheckoutId = resume?.checkoutId;
  useEffect(() => {
    if (resumedCheckoutId && status.data?.state === 'succeeded') {
      markProviderPaymentResumeCompleted(resumedCheckoutId);
    }
  }, [resumedCheckoutId, status.data?.state]);

  if (!resume) {
    return (
      <Container maxWidth="sm" sx={{ py: 8 }}>
        <Alert severity="warning">
          {english
            ? 'This tab does not have the private payment recovery capability. Return to the original order or contact support.'
            : 'Desde esta pestaña no podemos consultar el pago. Vuelve a la orden original o contacta a soporte.'}
        </Alert>
      </Container>
    );
  }

  const session = status.data;
  const succeeded = session?.state === 'succeeded';
  const knownNoCharge = session?.state === 'confirmed_no_charge'
    || (session?.state === 'failed' && session.canRetryOrFallback);
  const ambiguous = session?.state === 'ambiguous'
    || Boolean(session && !session.canRetryOrFallback && !succeeded);
  const redirect = session?.provider === 'placetopay'
    ? safePlaceToPayRedirect(session.redirectUrl)
    : null;
  const canReleaseAttempt = paymentAttemptCanBeReleased(
    session?.state,
    session?.canRetryOrFallback,
  );

  const releaseAttemptIfAuthoritative = () => {
    if (!canReleaseAttempt) return;
    clearProviderPaymentResume(resume.checkoutId);
    if (knownNoCharge) {
      clearPaymentIdempotencyKey(
        resume.checkoutId,
        resume.provider,
        resume.paymentMethod,
      );
    }
  };

  return (
    <Box component="main" sx={{ minHeight: '70vh', py: { xs: 5, md: 9 } }}>
      <Container maxWidth="sm">
        <Card variant="outlined" sx={{ borderRadius: 4 }}>
          <CardContent sx={{ p: { xs: 3, md: 5 } }}>
            <Stack spacing={2.5}>
              <Typography component="h1" variant="h4" fontWeight={900}>
                {english ? 'Verifying payment' : 'Verificando el pago'}
              </Typography>
              {(status.isLoading || status.isFetching) && !session && (
                <Stack direction="row" spacing={1.5} alignItems="center">
                  <CircularProgress size={24} />
                  <Typography>{english ? 'Confirming your payment…' : 'Confirmando tu pago…'}</Typography>
                </Stack>
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
              {ambiguous && (
                <Alert severity="warning">
                  {english
                    ? 'The result is pending or ambiguous. Do not pay again or use another provider while TDF reconciles it.'
                    : 'Todavía no sabemos si el pago se completó. No pagues otra vez ni uses otro método mientras lo revisamos.'}
                </Alert>
              )}
              {session?.state === 'requires_customer_action' && redirect && (
                <Button variant="contained" onClick={() => window.location.assign(redirect)}>
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
