import { get, post } from './client';
import type { components } from './generated/types';

export type ProviderPaymentSession = components['schemas']['PaymentSession'];
export type ProviderPaymentSessionCreate = components['schemas']['PaymentSessionCreate'];
export type HostedPaymentProvider = ProviderPaymentSessionCreate['provider'];
export type HostedPaymentMethod = ProviderPaymentSessionCreate['paymentMethod'];

export type HostedPaymentMethodLabel =
  | 'placetopay_card'
  | 'placetopay_bank_redirect'
  | 'placetopay_deuna_qr'
  | 'payphone_wallet';

export interface HostedPaymentMethodDefinition {
  label: HostedPaymentMethodLabel;
  provider: HostedPaymentProvider;
  paymentMethod: HostedPaymentMethod;
}

export const HOSTED_PAYMENT_METHODS: readonly HostedPaymentMethodDefinition[] = [
  { label: 'placetopay_card', provider: 'placetopay', paymentMethod: 'card' },
  { label: 'placetopay_bank_redirect', provider: 'placetopay', paymentMethod: 'bank_redirect' },
  { label: 'placetopay_deuna_qr', provider: 'placetopay', paymentMethod: 'deuna_qr' },
  { label: 'payphone_wallet', provider: 'payphone', paymentMethod: 'payphone_wallet' },
];

const UUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
const IDEMPOTENCY_PATTERN = /^[A-Za-z0-9][A-Za-z0-9._:-]{15,127}$/;
const PLACE_TO_PAY_HOSTS = new Set([
  'checkout-test.placetopay.ec',
  'checkout.placetopay.ec',
]);

const requireUuid = (value: string, field: string): string => {
  const normalized = value.trim();
  if (!UUID_PATTERN.test(normalized)) throw new Error(`${field} must be a UUID.`);
  return normalized;
};

const requireLookupToken = (value: string): string => {
  const normalized = value.trim();
  if (normalized.length < 16 || normalized.length > 512) {
    throw new Error('The checkout lookup token is invalid.');
  }
  return normalized;
};

const requireIdempotencyKey = (value: string): string => {
  const normalized = value.trim();
  if (!IDEMPOTENCY_PATTERN.test(normalized)) {
    throw new Error('The payment idempotency key is invalid.');
  }
  return normalized;
};

const checkoutHeaders = (lookupToken: string, idempotencyKey?: string): HeadersInit => ({
  'X-Checkout-Lookup-Token': requireLookupToken(lookupToken),
  ...(idempotencyKey ? { 'Idempotency-Key': requireIdempotencyKey(idempotencyKey) } : {}),
});

export const createProviderPaymentSession = (
  checkoutId: string,
  lookupToken: string,
  idempotencyKey: string,
  payload: ProviderPaymentSessionCreate,
) => post<ProviderPaymentSession>(
  `/commerce/checkouts/${requireUuid(checkoutId, 'checkoutId')}/payment-sessions`,
  payload,
  { headers: checkoutHeaders(lookupToken, idempotencyKey) },
);

export const getProviderPaymentSession = (
  checkoutId: string,
  attemptId: string,
  lookupToken: string,
) => get<ProviderPaymentSession>(
  `/commerce/checkouts/${requireUuid(checkoutId, 'checkoutId')}/payment-sessions/${requireUuid(attemptId, 'attemptId')}`,
  { headers: checkoutHeaders(lookupToken) },
);

export const paymentMethodDefinition = (
  label: string,
): HostedPaymentMethodDefinition | null =>
  HOSTED_PAYMENT_METHODS.find((candidate) => candidate.label === label) ?? null;

export const safePlaceToPayRedirect = (rawUrl: string | null | undefined): string | null => {
  if (!rawUrl) return null;
  try {
    const parsed = new URL(rawUrl);
    if (parsed.protocol !== 'https:' || parsed.username || parsed.password || parsed.port) return null;
    return PLACE_TO_PAY_HOSTS.has(parsed.hostname.toLowerCase()) ? parsed.toString() : null;
  } catch {
    return null;
  }
};
