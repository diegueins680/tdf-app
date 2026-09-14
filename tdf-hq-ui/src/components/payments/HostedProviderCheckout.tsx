import {
  Alert,
  Button,
  CircularProgress,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';

import { ApiError } from '../../api/client';
import {
  createProviderPaymentSession,
  getProviderPaymentSession,
  HOSTED_PAYMENT_METHODS,
  safePlaceToPayRedirect,
  type HostedPaymentMethodDefinition,
  type HostedPaymentMethodLabel,
  type ProviderPaymentSession,
} from '../../api/providerPaymentSessions';
import {
  clearPaymentIdempotencyKey,
  clearProviderPaymentPending,
  clearProviderPaymentResume,
  loadOrCreatePaymentIdempotencyKey,
  loadExistingPaymentIdempotencyKey,
  loadProviderPaymentPending,
  loadProviderPaymentResume,
  safePaymentReturnPath,
  saveProviderPaymentPending,
  saveProviderPaymentResume,
  type ProviderPaymentPending,
} from '../../utils/providerPaymentResume';

export interface HostedCheckoutContext {
  checkoutId: string;
  lookupToken: string;
  returnPath: string;
}

interface HostedProviderCheckoutProps {
  checkout?: HostedCheckoutContext | null;
  offeredMethods: readonly string[];
  disabled?: boolean;
  english?: boolean;
  initialBuyerPhone?: string | null;
  prepareCheckout?: (
    method: HostedPaymentMethodDefinition,
  ) => Promise<HostedCheckoutContext>;
  pendingReturnPathPrefix?: string;
  onSafetyLockChange?: (locked: boolean) => void;
  onPaymentConfirmed?: () => void | Promise<void>;
  onSessionChange?: (session: ProviderPaymentSession) => void;
  navigateToProvider?: (url: string) => void;
}

const POLL_INTERVAL_MS = 4_000;
const POLLABLE_STATES = new Set<ProviderPaymentSession['state']>([
  'prepared',
  'in_flight',
  'requires_customer_action',
  'processing',
  'ambiguous',
]);

class PaymentStartNotSentError extends Error {}
class PaymentRecoveryUnavailableError extends Error {}

const methodCopy = (
  label: HostedPaymentMethodLabel,
  english: boolean,
): string => {
  const copy: Record<HostedPaymentMethodLabel, [string, string]> = {
    placetopay_card: ['Card · PlaceToPay', 'Tarjeta · PlaceToPay'],
    placetopay_bank_redirect: ['Online bank payment · PlaceToPay', 'Pago bancario en línea · PlaceToPay'],
    placetopay_deuna_qr: ['DeUna! QR · PlaceToPay', 'QR DeUna! · PlaceToPay'],
    payphone_wallet: ['PayPhone wallet', 'Billetera PayPhone'],
  };
  return copy[label][english ? 0 : 1];
};

const digitsOnly = (value: string): string => value.replace(/\D/g, '');

const isPotentiallyAmbiguous = (error: unknown): boolean =>
  !(error instanceof PaymentStartNotSentError)
  && !(error instanceof ApiError && (error.status === 400 || error.status === 404));

const publicError = (english: boolean, potentiallyAmbiguous: boolean): string => {
  if (potentiallyAmbiguous) {
    return english
      ? 'The provider result is not yet known. Do not use another payment method. Retry this exact option to recover its durable status.'
      : 'Todavía no conocemos el resultado del proveedor. No uses otro método. Reintenta esta misma opción para recuperar su estado durable.';
  }
  return english
    ? 'This payment method could not be started. No charge is shown as successful.'
    : 'No pudimos iniciar este método. No mostramos ningún cobro como exitoso.';
};

const stateCopy = (session: ProviderPaymentSession, english: boolean): {
  severity: 'success' | 'info' | 'warning' | 'error';
  message: string;
} => {
  switch (session.state) {
    case 'succeeded':
      return {
        severity: 'success',
        message: english
          ? 'The provider payment was verified by the server.'
          : 'El servidor verificó el pago del proveedor.',
      };
    case 'confirmed_no_charge':
      return {
        severity: 'info',
        message: english
          ? 'The server confirmed that no charge was completed. You may try again or choose another available method.'
          : 'El servidor confirmó que no se completó un cobro. Puedes reintentar o elegir otro método disponible.',
      };
    case 'failed':
      return {
        severity: session.canRetryOrFallback ? 'warning' : 'error',
        message: session.canRetryOrFallback
          ? (english
            ? 'The payment was declined before a charge was created. You may try again.'
            : 'El pago fue rechazado antes de crear un cobro. Puedes reintentar.')
          : (english
            ? 'The result needs review. Do not use another payment method.'
            : 'El resultado necesita revisión. No uses otro método de pago.'),
      };
    case 'ambiguous':
      return {
        severity: 'warning',
        message: english
          ? 'The result is ambiguous and is being reconciled. Do not retry or use another provider.'
          : 'El resultado es ambiguo y está en conciliación. No reintentes ni uses otro proveedor.',
      };
    case 'requires_customer_action':
      return {
        severity: 'info',
        message: english
          ? 'Complete the payment on the provider-hosted page. Returning to TDF is not proof of payment.'
          : 'Completa el pago en la página alojada por el proveedor. Volver a TDF no prueba el pago.',
      };
    default:
      return {
        severity: 'info',
        message: session.provider === 'payphone'
          ? (english
            ? 'Approve the request in PayPhone. TDF is waiting for verified provider confirmation.'
            : 'Aprueba la solicitud en PayPhone. TDF espera la confirmación verificada del proveedor.')
          : (english
            ? 'The payment is processing. TDF is waiting for verified provider confirmation.'
            : 'El pago está en proceso. TDF espera la confirmación verificada del proveedor.'),
      };
  }
};

export default function HostedProviderCheckout({
  checkout,
  offeredMethods,
  disabled = false,
  english = false,
  initialBuyerPhone,
  prepareCheckout,
  pendingReturnPathPrefix,
  onSafetyLockChange,
  onPaymentConfirmed,
  onSessionChange,
  navigateToProvider,
}: HostedProviderCheckoutProps) {
  const offeredMethodKey = HOSTED_PAYMENT_METHODS
    .filter((method) => offeredMethods.includes(method.label))
    .map((method) => method.label)
    .join('|');
  const methods = useMemo(() => HOSTED_PAYMENT_METHODS.filter(
    (method) => offeredMethodKey.split('|').includes(method.label),
  ), [offeredMethodKey]);
  const onPaymentConfirmedRef = useRef(onPaymentConfirmed);
  const onSessionChangeRef = useRef(onSessionChange);
  const onSafetyLockChangeRef = useRef(onSafetyLockChange);
  const [activeContext, setActiveContext] = useState<HostedCheckoutContext | null>(checkout ?? null);
  const [selected, setSelected] = useState<HostedPaymentMethodDefinition | null>(null);
  const [session, setSession] = useState<ProviderPaymentSession | null>(null);
  const [pendingCreation, setPendingCreation] = useState<ProviderPaymentPending | null>(null);
  const [busy, setBusy] = useState(false);
  const [potentiallyAmbiguous, setPotentiallyAmbiguous] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [buyerCountryCode, setBuyerCountryCode] = useState('593');
  const [buyerPhone, setBuyerPhone] = useState(() => digitsOnly(initialBuyerPhone ?? ''));
  const recoveryScope = checkout?.checkoutId ?? pendingReturnPathPrefix ?? '';
  const recoveryScopeRef = useRef(recoveryScope);
  recoveryScopeRef.current = recoveryScope;

  useEffect(() => {
    onPaymentConfirmedRef.current = onPaymentConfirmed;
    onSessionChangeRef.current = onSessionChange;
    onSafetyLockChangeRef.current = onSafetyLockChange;
  }, [onPaymentConfirmed, onSafetyLockChange, onSessionChange]);

  useEffect(() => {
    if (checkout) setActiveContext(checkout);
  }, [checkout]);

  const publishSession = useCallback((next: ProviderPaymentSession) => {
    setSession(next);
    setPotentiallyAmbiguous(false);
    onSessionChangeRef.current?.(next);
    if (next.state === 'succeeded') {
      try {
        const refresh = onPaymentConfirmedRef.current?.();
        if (refresh) void refresh.catch(() => undefined);
      } catch {
        // Payment truth remains the durable session; order refresh is recoverable.
      }
    }
  }, []);

  const refreshSession = useCallback(async (
    context: HostedCheckoutContext,
    attemptId: string,
  ) => {
    const requestScope = recoveryScopeRef.current;
    const next = await getProviderPaymentSession(
      context.checkoutId,
      attemptId,
      context.lookupToken,
    );
    if (recoveryScopeRef.current === requestScope) publishSession(next);
    return next;
  }, [publishSession]);

  useEffect(() => {
    let cancelled = false;
    setSession(null);
    setSelected(null);
    setPendingCreation(null);
    setPotentiallyAmbiguous(false);
    setError(null);
    setBusy(false);
    const expectedCheckoutId = checkout?.checkoutId;
    const resume = loadProviderPaymentResume(expectedCheckoutId);
    if (resume) {
      const context = {
        checkoutId: resume.checkoutId,
        lookupToken: resume.lookupToken,
        returnPath: resume.returnPath,
      };
      const resumedMethod = HOSTED_PAYMENT_METHODS.find((candidate) =>
        candidate.provider === resume.provider && candidate.paymentMethod === resume.paymentMethod);
      if (!resumedMethod || (!expectedCheckoutId
        && (!pendingReturnPathPrefix || !resume.returnPath.startsWith(pendingReturnPathPrefix)))) return;
      clearProviderPaymentPending(resume.checkoutId);
      setPendingCreation(null);
      setActiveContext(context);
      setSelected(resumedMethod);
      setBusy(true);
      getProviderPaymentSession(context.checkoutId, resume.attemptId, context.lookupToken)
        .then((next) => { if (!cancelled) publishSession(next); })
        .catch(() => {
          if (cancelled) return;
          setPotentiallyAmbiguous(true);
          setError(publicError(english, true));
        })
        .finally(() => { if (!cancelled) setBusy(false); });
      return () => { cancelled = true; };
    }

    const pending = loadProviderPaymentPending(expectedCheckoutId);
    if (!pending
        || (!expectedCheckoutId
          && (!pendingReturnPathPrefix
            || !pending.returnPath.startsWith(pendingReturnPathPrefix)))) return;
    const pendingMethod = HOSTED_PAYMENT_METHODS.find((candidate) =>
      candidate.provider === pending.provider
      && candidate.paymentMethod === pending.paymentMethod);
    if (!pendingMethod) return;
    setPendingCreation(pending);
    setActiveContext({
      checkoutId: pending.checkoutId,
      lookupToken: pending.lookupToken,
      returnPath: pending.returnPath,
    });
    setSelected(pendingMethod);
    if (pending.buyerPhone) setBuyerPhone(pending.buyerPhone);
    if (pending.buyerCountryCode) setBuyerCountryCode(pending.buyerCountryCode);
    setPotentiallyAmbiguous(true);
    setError(publicError(english, true));
  }, [checkout?.checkoutId, english, pendingReturnPathPrefix, publishSession]);

  useEffect(() => {
    if (!session || !activeContext || !POLLABLE_STATES.has(session.state)) return;
    let cancelled = false;
    const timeout = window.setTimeout(() => {
      refreshSession(activeContext, session.attemptId)
        .catch(() => {
          if (!cancelled) setError(english
            ? 'Status could not be refreshed. The payment remains unconfirmed.'
            : 'No pudimos actualizar el estado. El pago sigue sin confirmar.');
        });
    }, POLL_INTERVAL_MS);
    return () => {
      cancelled = true;
      window.clearTimeout(timeout);
    };
  }, [activeContext, english, refreshSession, session]);

  const safetyLocked = busy || potentiallyAmbiguous
    || Boolean(session && !session.canRetryOrFallback);

  useEffect(() => {
    onSafetyLockChangeRef.current?.(safetyLocked);
    return () => onSafetyLockChangeRef.current?.(false);
  }, [safetyLocked]);

  if (methods.length === 0 && !pendingCreation && !selected && !session) return null;

  const navigate = navigateToProvider ?? ((url: string) => window.location.assign(url));

  const startPayment = async (method: HostedPaymentMethodDefinition) => {
    if (busy) return;
    const requestScope = recoveryScopeRef.current;
    const original = pendingCreation?.provider === method.provider
      && pendingCreation.paymentMethod === method.paymentMethod ? pendingCreation : null;
    if (!original && (disabled || !methods.some((candidate) => candidate.label === method.label))) return;
    if (safetyLocked && selected?.label !== method.label) {
      setError(publicError(english, true));
      return;
    }
    const phone = original?.buyerPhone ?? digitsOnly(buyerPhone);
    const countryCode = original?.buyerCountryCode ?? digitsOnly(buyerCountryCode);
    if (method.provider === 'payphone'
        && (phone.length < 6 || phone.length > 15
          || countryCode.length < 1 || countryCode.length > 3)) {
      setError(english
        ? 'Enter a valid country calling code and phone number for PayPhone.'
        : 'Ingresa un código de país y número de teléfono válidos para PayPhone.');
      return;
    }
    setBusy(true);
    setPotentiallyAmbiguous(true);
    setSelected(method);
    setError(null);
    let attemptedContext: HostedCheckoutContext | null = null;
    try {
      const recoveredContext = pendingCreation
        && pendingCreation.provider === method.provider
        && pendingCreation.paymentMethod === method.paymentMethod
        ? {
          checkoutId: pendingCreation.checkoutId,
          lookupToken: pendingCreation.lookupToken,
          returnPath: pendingCreation.returnPath,
        }
        : null;
      const context = recoveredContext ?? checkout ?? await prepareCheckout?.(method);
      if (!context || !safePaymentReturnPath(context.returnPath)) {
        throw new PaymentStartNotSentError('A secure canonical checkout is required.');
      }
      attemptedContext = context;
      setActiveContext(context);
      const idempotencyKey = (original
        ? loadExistingPaymentIdempotencyKey : loadOrCreatePaymentIdempotencyKey)(
        context.checkoutId,
        method.provider,
        method.paymentMethod,
      );
      if (!idempotencyKey) {
        throw new PaymentRecoveryUnavailableError();
      }
      const pending: ProviderPaymentPending = {
        version: 1,
        checkoutId: context.checkoutId,
        provider: method.provider,
        paymentMethod: method.paymentMethod,
        lookupToken: context.lookupToken,
        returnPath: context.returnPath,
        ...(method.provider === 'payphone' ? {
          buyerPhone: phone,
          buyerCountryCode: countryCode,
        } : {}),
        createdAt: Date.now(),
      };
      if (!saveProviderPaymentPending(pending)) {
        if (!original) clearPaymentIdempotencyKey(context.checkoutId, method.provider, method.paymentMethod);
        throw new PaymentStartNotSentError('Durable browser recovery is unavailable.');
      }
      setPendingCreation(pending);
      const next = await createProviderPaymentSession(
        context.checkoutId,
        context.lookupToken,
        idempotencyKey,
        {
          provider: method.provider,
          paymentMethod: method.paymentMethod,
          ...(method.provider === 'payphone' ? {
            buyerPhone: phone,
            buyerCountryCode: countryCode,
          } : {}),
        },
      );
      // Leave the original pending marker intact if navigation changed the
      // checkout while this response was in flight. Never publish it to another order.
      if (recoveryScopeRef.current !== requestScope) return;
      publishSession(next);
      const resumeSaved = saveProviderPaymentResume({
        version: 1,
        checkoutId: context.checkoutId,
        attemptId: next.attemptId,
        provider: method.provider,
        paymentMethod: method.paymentMethod,
        lookupToken: context.lookupToken,
        returnPath: context.returnPath,
        createdAt: Date.now(),
      });
      if (resumeSaved) {
        clearProviderPaymentPending(context.checkoutId);
        setPendingCreation(null);
      }
      if (next.provider === 'placetopay' && next.state === 'requires_customer_action') {
        if (!resumeSaved) {
          setPotentiallyAmbiguous(true);
          setError(english
            ? 'Secure return recovery could not be stored. Stay on this page while TDF reconciles the attempt.'
            : 'No se pudo guardar la recuperación segura. Permanece en esta página mientras TDF concilia el intento.');
          return;
        }
        const redirect = safePlaceToPayRedirect(next.redirectUrl);
        if (!redirect) {
          setError(english
            ? 'The provider returned an unsafe redirect. Do not retry with another method; support must reconcile this attempt.'
            : 'El proveedor devolvió una redirección insegura. No reintentes con otro método; soporte debe conciliar este intento.');
          return;
        }
        navigate(redirect);
      }
    } catch (startError) {
      if (recoveryScopeRef.current !== requestScope) return;
      // A failed lookup (including 404 after revocation/expiry) is not evidence
      // that an earlier transmitted request did not create a charge.
      const ambiguous = Boolean(original) || isPotentiallyAmbiguous(startError);
      setPotentiallyAmbiguous(ambiguous);
      setError(startError instanceof PaymentRecoveryUnavailableError
        ? (english
          ? 'The original payment recovery key is unavailable. Do not pay again; contact support to reconcile this order.'
          : 'La clave original de recuperación no está disponible. No pagues otra vez; contacta a soporte para conciliar esta orden.')
        : publicError(english, ambiguous));
      if (!ambiguous && attemptedContext) {
        clearProviderPaymentPending(attemptedContext.checkoutId);
        clearPaymentIdempotencyKey(
          attemptedContext.checkoutId,
          method.provider,
          method.paymentMethod,
        );
        setPendingCreation(null);
      }
    } finally {
      if (recoveryScopeRef.current === requestScope) setBusy(false);
    }
  };

  const resetKnownNoCharge = () => {
    if (!session?.canRetryOrFallback || !selected || !activeContext) return;
    clearProviderPaymentResume(activeContext.checkoutId);
    clearProviderPaymentPending(activeContext.checkoutId);
    clearPaymentIdempotencyKey(
      activeContext.checkoutId,
      selected.provider,
      selected.paymentMethod,
    );
    setSession(null);
    setPendingCreation(null);
    setSelected(null);
    setPotentiallyAmbiguous(false);
    setError(null);
  };

  const copy = session ? stateCopy(session, english) : null;
  const existingRedirect = session?.provider === 'placetopay'
    ? safePlaceToPayRedirect(session.redirectUrl)
    : null;

  return (
    <Stack spacing={1.5} component="section" aria-label={english ? 'Additional secure payment methods' : 'Métodos de pago seguros adicionales'}>
      <Typography variant="subtitle2" fontWeight={800}>
        {english ? 'More secure payment methods' : 'Más métodos de pago seguros'}
      </Typography>
      {methods.some((method) => method.provider === 'payphone') && (
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
          <TextField
            size="small"
            label={english ? 'Country code' : 'Código país'}
            disabled={safetyLocked}
            value={buyerCountryCode}
            onChange={(event) => setBuyerCountryCode(digitsOnly(event.target.value).slice(0, 3))}
            inputProps={{ inputMode: 'numeric', maxLength: 3 }}
            sx={{ maxWidth: { sm: 150 } }}
          />
          <TextField
            size="small"
            label={english ? 'PayPhone number' : 'Número PayPhone'}
            disabled={safetyLocked}
            value={buyerPhone}
            onChange={(event) => setBuyerPhone(digitsOnly(event.target.value).slice(0, 15))}
            inputProps={{ inputMode: 'tel', maxLength: 15 }}
            helperText={english ? 'Digits only, without the country code.' : 'Solo dígitos, sin el código de país.'}
          />
        </Stack>
      )}
      <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} useFlexGap flexWrap="wrap">
        {methods.map((method) => {
          const isSelected = selected?.label === method.label;
          const blockedByAttempt = session?.canRetryOrFallback === false;
          return (
            <Button
              key={method.label}
              variant={method.provider === 'placetopay' ? 'contained' : 'outlined'}
              disabled={disabled || busy || ((safetyLocked || blockedByAttempt) && !isSelected) || (Boolean(session) && isSelected)}
              onClick={() => void startPayment(method)}
            >
              {busy && isSelected ? <CircularProgress size={20} color="inherit" /> : methodCopy(method.label, english)}
            </Button>
          );
        })}
      </Stack>
      {pendingCreation && selected && !session && (
        <Button variant="outlined" disabled={busy} onClick={() => void startPayment(selected)}>
          {english ? 'Recover original payment' : 'Recuperar pago original'}
        </Button>
      )}
      {copy && <Alert severity={copy.severity}>{copy.message}</Alert>}
      {error && <Alert severity="warning">{error}</Alert>}
      {existingRedirect && session?.state === 'requires_customer_action' && (
        <Button variant="contained" onClick={() => navigate(existingRedirect)} disabled={disabled || busy}>
          {english ? 'Continue on the secure provider page' : 'Continuar en la página segura del proveedor'}
        </Button>
      )}
      {session && activeContext && POLLABLE_STATES.has(session.state) && (
        <Button
          variant="text"
          disabled={busy}
          onClick={() => void refreshSession(activeContext, session.attemptId)}
        >
          {english ? 'Refresh verified status' : 'Actualizar estado verificado'}
        </Button>
      )}
      {session?.canRetryOrFallback && (session.state === 'failed' || session.state === 'confirmed_no_charge') && (
        <Button variant="outlined" onClick={resetKnownNoCharge} disabled={disabled || busy}>
          {english ? 'Choose another payment method' : 'Elegir otro método de pago'}
        </Button>
      )}
      <Typography variant="caption" color="text.secondary">
        {english
          ? 'TDF never collects card numbers or security codes here. Payment success is shown only after server verification.'
          : 'TDF no recopila aquí números de tarjeta ni códigos de seguridad. Solo mostramos éxito después de la verificación del servidor.'}
      </Typography>
    </Stack>
  );
}
