import {
  Alert,
  Button,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import { useCallback, useEffect, useMemo, useRef, useState } from 'react';
import type { ReactNode } from 'react';
import PaymentMethodChoices, { PaymentMethodButton } from './PaymentMethodChoices';
import PaymentAlternativesNotice from './PaymentAlternativesNotice';

import { ApiError } from '../../api/client';
import {
  createProviderPaymentSession,
  getProviderPaymentSession,
  HOSTED_PAYMENT_METHODS,
  safeProviderRedirect,
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
  markProviderPaymentResumeCompleted,
  paymentAttemptCanBeReleased,
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
  /** Existing local payment buttons, with availability/handlers owned by the caller. */
  additionalMethods?: ReactNode;
  alternativeMethods?: readonly string[];
  availabilityUnknown?: boolean;
  paymentFailure?: { method?: string | null; needsVerification: boolean } | null;
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
    payphone_card: ['Card · PayPhone', 'Tarjeta · PayPhone'],
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
      ? 'We do not know yet whether the payment went through. Do not use another payment method. Retry this same option to check its status.'
      : 'Todavía no sabemos si el pago se completó. No uses otro método. Vuelve a intentar con esta misma opción para consultar su estado.';
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
          : 'Pago confirmado.',
      };
    case 'confirmed_no_charge':
      if (!session.canRetryOrFallback) {
        return { severity: 'warning', message: english
          ? 'Contact TDF before making another payment.'
          : 'Consulta con TDF antes de realizar otro pago.' };
      }
      return {
        severity: 'info',
        message: english
          ? 'The server confirmed that no charge was completed. You may try again or choose another available method.'
          : 'No se realizó ningún cobro. Puedes reintentar o elegir otro método disponible.',
      };
    case 'failed':
      return {
        severity: session.canRetryOrFallback ? 'warning' : 'error',
        message: session.canRetryOrFallback
          ? (english
            ? 'The payment was declined before a charge was created. You may choose another available method or try again.'
            : 'El pago fue rechazado antes de crear un cobro. Puedes elegir otro método disponible o reintentar.')
          : (english
            ? 'The result needs review. Do not use another payment method.'
            : 'El resultado necesita revisión. No uses otro método de pago.'),
      };
    case 'ambiguous':
      return {
        severity: 'warning',
        message: english
          ? 'The result is ambiguous and is being reconciled. Do not retry or use another provider.'
          : 'Todavía no sabemos si el pago se completó y lo estamos revisando. No vuelvas a intentar ni uses otro método.',
      };
    case 'requires_customer_action':
      return {
        severity: 'info',
        message: english
          ? 'Complete the payment on the provider-hosted page. Returning to TDF is not proof of payment.'
          : 'Completa el pago en la página del procesador de pagos. El pago solo cuenta cuando lo confirmemos aquí.',
      };
    default:
      return {
        severity: 'info',
        message: session.provider === 'payphone'
          ? (english
            ? 'Complete or approve the payment in PayPhone. TDF is waiting for verified provider confirmation.'
            : 'Completa o aprueba el pago en PayPhone. TDF espera la confirmación verificada del proveedor.')
          : (english
            ? 'The payment is processing. TDF is waiting for verified provider confirmation.'
            : 'El pago está en proceso. Estamos esperando la confirmación.'),
      };
  }
};

export default function HostedProviderCheckout({
  checkout,
  offeredMethods,
  additionalMethods,
  alternativeMethods = offeredMethods,
  availabilityUnknown = false,
  paymentFailure,
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
  const [recoveryRevision, setRecoveryRevision] = useState(0);
  const [potentiallyAmbiguous, setPotentiallyAmbiguous] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [buyerCountryCode, setBuyerCountryCode] = useState('593');
  const [buyerPhone, setBuyerPhone] = useState(() => digitsOnly(initialBuyerPhone ?? ''));
  const [payphoneRequested, setPayphoneRequested] = useState(false);
  const [countryCodeError, setCountryCodeError] = useState<string | null>(null);
  const [phoneError, setPhoneError] = useState<string | null>(null);
  const countryInput = useRef<HTMLInputElement | null>(null);
  const phoneInput = useRef<HTMLInputElement | null>(null);
  useEffect(() => {
    if (payphoneRequested) phoneInput.current?.focus();
  }, [payphoneRequested]);
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
      markProviderPaymentResumeCompleted(next.checkoutId);
      try {
        const refresh = onPaymentConfirmedRef.current?.();
        if (refresh) void refresh.catch(() => undefined);
      } catch {
        // Payment truth remains the durable session; order refresh is recoverable.
      }
    }
  }, []);

  useEffect(() => {
    let cancelled = false;
    setSession(null);
    setSelected(null);
    setPendingCreation(null);
    setPotentiallyAmbiguous(false);
    setError(null);
    setCountryCodeError(null);
    setPhoneError(null);
    setBusy(false);
    const expectedCheckoutId = checkout?.checkoutId;
    const pending = loadProviderPaymentPending(expectedCheckoutId, pendingReturnPathPrefix);
    const resume = loadProviderPaymentResume(expectedCheckoutId, pendingReturnPathPrefix);
    const resumedMethod = resume && HOSTED_PAYMENT_METHODS.find((candidate) =>
      candidate.provider === resume.provider && candidate.paymentMethod === resume.paymentMethod);
    const resumeInScope = resume && (expectedCheckoutId !== undefined
      || (pendingReturnPathPrefix && resume.returnPath.startsWith(pendingReturnPathPrefix)));
    if (!pending && resume && resumedMethod && resumeInScope) {
      const context = {
        checkoutId: resume.checkoutId,
        lookupToken: resume.lookupToken,
        returnPath: resume.returnPath,
      };
      clearProviderPaymentPending(resume.checkoutId);
      setPendingCreation(null);
      setActiveContext(context);
      setSelected(resumedMethod);
      setBusy(true);
      getProviderPaymentSession(context.checkoutId, resume.attemptId, context.lookupToken)
        .then((next) => {
          if (cancelled) return;
          if (!expectedCheckoutId && next.state === 'succeeded') {
            if (markProviderPaymentResumeCompleted(context.checkoutId)) {
              setRecoveryRevision((revision) => revision + 1);
            }
            setActiveContext(null);
            setSelected(null);
            return;
          }
          publishSession(next);
        })
        .catch(() => {
          if (cancelled) return;
          setPotentiallyAmbiguous(true);
          setError(publicError(english, true));
        })
        .finally(() => { if (!cancelled) setBusy(false); });
      return () => { cancelled = true; };
    }
    // Another product's saved session must not hide this flow's transmitted request.

    if (!pending
        || (!expectedCheckoutId
          && (!pendingReturnPathPrefix
            || !pending.returnPath.startsWith(pendingReturnPathPrefix)))) return undefined;
    const pendingMethod = HOSTED_PAYMENT_METHODS.find((candidate) =>
      candidate.provider === pending.provider
      && candidate.paymentMethod === pending.paymentMethod);
    if (!pendingMethod) return undefined;
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
    return undefined;
  }, [checkout?.checkoutId, english, pendingReturnPathPrefix, publishSession, recoveryRevision]);

  const refreshStatus = useRef<(() => void) | null>(null);
  const [statusCheck, setStatusCheck] = useState<'ready' | 'checking' | 'unavailable'>('ready');
  const pollAttemptId = session && POLLABLE_STATES.has(session.state) ? session.attemptId : null;
  const checkoutId = activeContext?.checkoutId;
  const lookupToken = activeContext?.lookupToken;
  useEffect(() => {
    setStatusCheck('ready');
    if (!pollAttemptId || !checkoutId || !lookupToken) return;
    let cancelled = false;
    let inFlight = false;
    const check = async () => {
      if (inFlight || cancelled) return;
      inFlight = true;
      window.clearTimeout(timer);
      setStatusCheck('checking');
      try {
        const next = await getProviderPaymentSession(checkoutId, pollAttemptId, lookupToken);
        if (cancelled || recoveryScopeRef.current !== recoveryScope) return;
        setStatusCheck('ready');
        publishSession(next);
        if (!POLLABLE_STATES.has(next.state)) return;
      } catch {
        if (cancelled || recoveryScopeRef.current !== recoveryScope) return;
        setStatusCheck('unavailable');
      } finally {
        inFlight = false;
      }
      timer = window.setTimeout(startCheck, POLL_INTERVAL_MS);
    };
    const startCheck = () => { void check(); };
    let timer = window.setTimeout(startCheck, POLL_INTERVAL_MS);
    refreshStatus.current = startCheck;
    return () => {
      cancelled = true;
      refreshStatus.current = null;
      window.clearTimeout(timer);
    };
  }, [checkoutId, lookupToken, pollAttemptId, publishSession, recoveryScope]);

  const canReleaseAttempt = paymentAttemptCanBeReleased(session?.state, session?.canRetryOrFallback);
  const safetyLocked = busy || potentiallyAmbiguous || Boolean(session && !canReleaseAttempt);

  useEffect(() => {
    onSafetyLockChangeRef.current?.(safetyLocked);
    return () => onSafetyLockChangeRef.current?.(false);
  }, [safetyLocked]);

  if (!additionalMethods && methods.length === 0 && !pendingCreation && !selected && !session && !error && !paymentFailure) return null;

  const navigate = (provider: ProviderPaymentSession['provider'], url: string) => {
    if (navigateToProvider) {
      navigateToProvider(url);
      return;
    }
    if (provider !== 'payphone') {
      window.location.assign(url);
      return;
    }
    // PayPhone checks the referring origin. Keep the ticket document's private
    // path and query under its existing referrer policy for all other requests.
    const link = document.createElement('a');
    link.href = url;
    link.referrerPolicy = 'origin';
    document.body.append(link);
    link.click();
    link.remove();
  };

  const startPayment = async (method: HostedPaymentMethodDefinition) => {
    if (busy) return;
    const requestScope = recoveryScopeRef.current;
    const original = pendingCreation?.provider === method.provider
      && pendingCreation.paymentMethod === method.paymentMethod ? pendingCreation : null;
    if (!original && (disabled || availabilityUnknown || !methods.some((candidate) => candidate.label === method.label))) return;
    if (safetyLocked && selected?.label !== method.label) {
      setError(publicError(english, true));
      return;
    }
    const phone = original?.buyerPhone ?? digitsOnly(buyerPhone);
    const countryCode = original?.buyerCountryCode ?? digitsOnly(buyerCountryCode);
    if (method.paymentMethod === 'payphone_wallet') {
      const invalidCountry = countryCode.length < 1 || countryCode.length > 3;
      const invalidPhone = phone.length < 6 || phone.length > 15;
      setCountryCodeError(invalidCountry
        ? (english ? 'Enter a country calling code of 1 to 3 digits.' : 'Ingresa un código de país de 1 a 3 dígitos.') : null);
      setPhoneError(invalidPhone
        ? (english ? 'Enter a phone number of 6 to 15 digits, without the country code.' : 'Ingresa un teléfono de 6 a 15 dígitos, sin el código de país.') : null);
      if (invalidCountry || invalidPhone) {
        (invalidCountry ? countryInput : phoneInput).current?.focus();
        return;
      }
    }
    setCountryCodeError(null);
    setPhoneError(null);
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
        ...(method.paymentMethod === 'payphone_wallet' ? {
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
          ...(method.paymentMethod === 'payphone_wallet' ? {
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
        ...(next.state === 'succeeded' ? { completedAt: Date.now() } : {}),
      });
      if (resumeSaved) {
        clearProviderPaymentPending(context.checkoutId);
        setPendingCreation(null);
      }
      if (next.state === 'requires_customer_action') {
        if (!resumeSaved) {
          setPotentiallyAmbiguous(true);
          setError(english
            ? 'Secure return recovery could not be stored. Stay on this page while TDF reconciles the attempt.'
            : 'No pudimos guardar los datos para retomar este pago. Quédate en esta página mientras lo revisamos.');
          return;
        }
        const redirect = safeProviderRedirect(next.provider, next.redirectUrl);
        if (!redirect) {
          setError(english
            ? 'The provider returned an unsafe redirect. Do not retry with another method; support must reconcile this attempt.'
            : 'El procesador devolvió un enlace que no es seguro. No intentes con otro método; contacta a soporte para revisar este intento.');
          return;
        }
        navigate(next.provider, redirect);
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
          : 'No podemos retomar el pago original desde aquí. No pagues otra vez; contacta a soporte para revisar esta orden.')
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
    if (!canReleaseAttempt || !selected || !activeContext) return;
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

  // PayPhone's number fields appear once that wallet is chosen, or when an
  // earlier PayPhone attempt is being recovered.
  const payphoneFieldsVisible = payphoneRequested || selected?.paymentMethod === 'payphone_wallet'
    || pendingCreation?.paymentMethod === 'payphone_wallet';
  const copy = session ? stateCopy(session, english) : null;
  const existingRedirect = safeProviderRedirect(session?.provider, session?.redirectUrl);
  const showAlternatives = session?.state !== 'succeeded' && (
    (Boolean(error) || Boolean(paymentFailure))
    || session?.state === 'failed' || session?.state === 'confirmed_no_charge'
  );

  return (
    <Stack spacing={1.5} component="section" aria-label={english ? 'Additional secure payment methods' : 'Métodos de pago seguros adicionales'}>
      {showAlternatives && (
        <PaymentAlternativesNotice
          methods={alternativeMethods}
          availabilityUnknown={availabilityUnknown}
          failedMethod={selected?.label ?? paymentFailure?.method}
          blocked={safetyLocked || Boolean(paymentFailure?.needsVerification)}
          english={english}
        />
      )}
    <PaymentMethodChoices english={english} methods={<>
      {additionalMethods}
      {methods.map((method) => {
        const isSelected = selected?.label === method.label;
        return (
          <PaymentMethodButton
            key={method.label}
            busy={busy && isSelected}
            disabled={disabled || availabilityUnknown || busy || (safetyLocked && !isSelected) || (Boolean(session) && isSelected)}
            onClick={() => {
              // The first tap on PayPhone only asks for the wallet number.
              if (method.paymentMethod === 'payphone_wallet' && !payphoneFieldsVisible) { setPayphoneRequested(true); return; }
              void startPayment(method);
            }}
          >{methodCopy(method.label, english)}</PaymentMethodButton>
        );
      })}
    </>}>
      {payphoneFieldsVisible && methods.some((method) => method.paymentMethod === 'payphone_wallet') && (
        <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1}>
          <TextField
            size="small"
            label={english ? 'Country code' : 'Código país'}
            disabled={safetyLocked}
            value={buyerCountryCode}
            inputRef={countryInput}
            error={Boolean(countryCodeError)}
            helperText={countryCodeError}
            autoComplete="tel-country-code"
            onChange={(event) => {
              setBuyerCountryCode(digitsOnly(event.target.value).slice(0, 3));
              setCountryCodeError(null);
            }}
            inputProps={{ inputMode: 'numeric', maxLength: 3 }}
            sx={{ maxWidth: { sm: 150 } }}
          />
          <TextField
            size="small"
            label={english ? 'PayPhone number' : 'Número PayPhone'}
            disabled={safetyLocked}
            value={buyerPhone}
            inputRef={phoneInput}
            error={Boolean(phoneError)}
            autoComplete="tel-national"
            onChange={(event) => {
              setBuyerPhone(digitsOnly(event.target.value).slice(0, 15));
              setPhoneError(null);
            }}
            inputProps={{ inputMode: 'tel', maxLength: 15 }}
            helperText={phoneError ?? (english ? 'Digits only, without the country code.' : 'Solo dígitos, sin el código de país.')}
          />
        </Stack>
      )}
      {pendingCreation && selected && !session && (
        <Button variant="outlined" disabled={busy} onClick={() => void startPayment(selected)}>
          {english ? 'Recover original payment' : 'Recuperar pago original'}
        </Button>
      )}
      {copy && statusCheck !== 'unavailable' && <Alert severity={copy.severity}>{copy.message}</Alert>}
      {error && <Alert severity="warning">{error}</Alert>}
      {statusCheck === 'unavailable' && <Alert severity="warning">{english
        ? 'Status could not be refreshed. TDF will check this payment again.'
        : 'No pudimos actualizar el estado. Volveremos a consultar este pago.'}</Alert>}
      {statusCheck !== 'unavailable' && existingRedirect && session?.state === 'requires_customer_action' && (
        <Button variant="contained" onClick={() => navigate(session.provider, existingRedirect)} disabled={disabled || busy || statusCheck === 'checking'}>
          {english ? 'Continue on the secure provider page' : 'Continuar con el pago'}
        </Button>
      )}
      {session && activeContext && POLLABLE_STATES.has(session.state) && (
        <Button
          variant="text"
          disabled={busy || statusCheck === 'checking'}
          onClick={() => refreshStatus.current?.()}
        >
          {english ? 'Refresh verified status' : 'Actualizar estado del pago'}
        </Button>
      )}
      {canReleaseAttempt && (
        <Button variant="outlined" onClick={resetKnownNoCharge} disabled={disabled || availabilityUnknown || busy}>
          {english ? 'Choose another payment method' : 'Elegir otro método de pago'}
        </Button>
      )}
      <Typography variant="caption" color="text.secondary">
        {english
          ? 'TDF never collects card numbers or security codes here. Payment success is shown only after server verification.'
          : 'TDF no pide aquí números de tarjeta ni códigos de seguridad. Un pago aparece como completado solo cuando está confirmado.'}
      </Typography>
    </PaymentMethodChoices>
    </Stack>
  );
}
