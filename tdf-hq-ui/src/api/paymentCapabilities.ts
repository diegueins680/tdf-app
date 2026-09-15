import { get } from './client';

export type CanonicalPaymentMethod =
  | 'card'
  | 'paypal_wallet'
  | 'bank_redirect'
  | 'deuna_qr'
  | 'payphone_wallet'
  | 'manual_bank_transfer';

export type CanonicalProductFlow =
  | 'merchandise'
  | 'booking'
  | 'professional_service'
  | 'course'
  | 'event_ticket'
  | 'digital_product'
  | 'subscription'
  | 'marketplace';

export interface PaymentRouteDTO {
  provider: string;
  paymentMethod: string;
  capabilities: string[];
  priority: number;
}

export interface PaymentCapabilityResponseDTO {
  environment: string;
  buyerCountry: string;
  currency: string;
  amountMinor: number;
  paymentMethod: string;
  productFlow: string;
  routes: PaymentRouteDTO[];
  fallbackPolicy: string;
}

export interface AvailableCheckoutMethods {
  datafast: boolean;
  paypal: boolean;
  placeToPayCard: boolean;
  placeToPayBankRedirect: boolean;
  placeToPayDeunaQr: boolean;
  payPhoneWallet: boolean;
  bankTransfer: boolean;
}

interface AvailabilityRequest {
  buyerCountry?: string;
  currency: string;
  amountMinor: number;
  productFlow: CanonicalProductFlow;
  marketplace?: boolean;
}

const routeRequest = (
  request: AvailabilityRequest,
  paymentMethod: CanonicalPaymentMethod,
): Promise<PaymentCapabilityResponseDTO> => {
  const normalizedBuyerCountry = request.buyerCountry?.trim().toUpperCase() ?? '';
  const query = new URLSearchParams({
    buyerCountry: normalizedBuyerCountry === '' ? 'ZZ' : normalizedBuyerCountry,
    currency: request.currency.trim().toUpperCase(),
    amountMinor: String(request.amountMinor),
    paymentMethod,
    productFlow: request.productFlow,
  });
  query.append('requires', 'one_time');
  if (request.marketplace) {
    query.append('requires', 'connected_accounts');
    query.append('requires', 'split_settlement');
    query.append('requires', 'seller_payouts');
  }
  return get<PaymentCapabilityResponseDTO>(`/commerce/payment-capabilities?${query.toString()}`);
};

/**
 * Fail closed: a method is visible only when the canonical server route names
 * the provider whose executor this UI invokes. A missing or failed response is
 * handled by React Query as unavailable, never as permission to optimistically
 * expose a checkout control.
 */
export const loadAvailableCheckoutMethods = async (
  request: AvailabilityRequest,
): Promise<AvailableCheckoutMethods> => {
  const [card, paypal, bankRedirect, deunaQr, payPhone, bank] = await Promise.all([
    routeRequest(request, 'card'),
    routeRequest(request, 'paypal_wallet'),
    routeRequest(request, 'bank_redirect'),
    routeRequest(request, 'deuna_qr'),
    routeRequest(request, 'payphone_wallet'),
    routeRequest(request, 'manual_bank_transfer'),
  ]);
  return {
    datafast: card.routes.some((route) => route.provider === 'datafast'),
    paypal: paypal.routes.some((route) => route.provider === 'paypal'),
    placeToPayCard: card.routes.some((route) => route.provider === 'placetopay'),
    placeToPayBankRedirect: bankRedirect.routes.some((route) => route.provider === 'placetopay'),
    placeToPayDeunaQr: deunaQr.routes.some((route) => route.provider === 'placetopay'),
    payPhoneWallet: payPhone.routes.some((route) => route.provider === 'payphone'),
    bankTransfer: bank.routes.some((route) => route.provider === 'bank_transfer'),
  };
};
