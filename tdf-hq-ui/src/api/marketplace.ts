import { get, post, put } from './client';
import type {
  MarketplaceItemDTO,
  MarketplaceCartDTO,
  MarketplaceOrderDTO,
  PaypalCreateDTO,
  PaypalCaptureRequest,
  MarketplaceOrderUpdatePayload,
  DatafastCheckoutDTO,
  StripePaymentIntentDTO,
  MarketplaceShippingAddress,
  MarketplaceFulfillmentUpdatePayload,
  MarketplaceRentalUpdatePayload,
  MarketplaceRentalTermsUpdatePayload,
} from './types';

export interface CartItemUpdate {
  mciuListingId: string;
  mciuQuantity: number;
  mciuRentalStartDate?: string;
  mciuRentalEndDate?: string;
}

export interface CheckoutRequest {
  mcrBuyerName: string;
  mcrBuyerEmail: string;
  mcrBuyerPhone?: string | null;
  mcrFulfillmentMethod?: 'pickup' | 'local_delivery' | 'shipping';
  mcrShippingAddress?: MarketplaceShippingAddress;
  mcrRentalTermsAccepted?: boolean;
  mcrIdentityDocumentType?: 'cedula' | 'passport' | 'ruc';
  mcrIdentityDocumentNumber?: string;
}

const lookupStorageKey = (orderId: string) => `tdf-marketplace-order-lookup:${orderId}`;
const idempotencyStorageKey = (cartId: string) =>
  `tdf-marketplace-checkout-idempotency:${cartId}`;

const randomUuid = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return crypto.randomUUID();
  }
  if (typeof crypto !== 'undefined' && typeof crypto.getRandomValues === 'function') {
    const bytes = crypto.getRandomValues(new Uint8Array(16));
    bytes[6] = ((bytes[6] ?? 0) & 0x0f) | 0x40;
    bytes[8] = ((bytes[8] ?? 0) & 0x3f) | 0x80;
    const hex = Array.from(bytes, (value) => value.toString(16).padStart(2, '0')).join('');
    return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`;
  }
  throw new Error('Secure browser randomness is required to start checkout');
};

export const getMarketplaceCheckoutIdempotencyKey = (cartId: string, _provider: string): string => {
  const storageKey = idempotencyStorageKey(cartId);
  const stored = typeof window !== 'undefined' ? window.localStorage.getItem(storageKey) : null;
  if (stored) return stored;
  const created = randomUuid();
  if (typeof window !== 'undefined') window.localStorage.setItem(storageKey, created);
  return created;
};

export const storeMarketplaceLookupToken = (orderId: string, token?: string | null): void => {
  if (!token || typeof window === 'undefined') return;
  window.sessionStorage.setItem(lookupStorageKey(orderId), token);
};

export const loadMarketplaceLookupToken = (orderId: string): string | null => {
  if (typeof window === 'undefined') return null;
  return window.sessionStorage.getItem(lookupStorageKey(orderId));
};

const idempotencyHeaders = (key: string): RequestInit => ({
  headers: { 'Idempotency-Key': key },
});

const lookupHeaders = (token: string): RequestInit => ({
  headers: { 'X-Order-Lookup-Token': token },
});

export const Marketplace = {
  list: () => get<MarketplaceItemDTO[]>('/marketplace'),
  detail: (listingId: string) => get<MarketplaceItemDTO>(`/marketplace/${listingId}`),
  updateRentalTerms: (listingId: string, payload: MarketplaceRentalTermsUpdatePayload) =>
    put<MarketplaceItemDTO>(`/marketplace/${listingId}/rental-terms`, payload),
  createCart: () => post<MarketplaceCartDTO>('/marketplace/cart', {}),
  getCart: (cartId: string) => get<MarketplaceCartDTO>(`/marketplace/cart/${cartId}`),
  upsertItem: (cartId: string, payload: CartItemUpdate) =>
    post<MarketplaceCartDTO>(`/marketplace/cart/${cartId}/items`, payload),
  checkout: (cartId: string, payload: CheckoutRequest, idempotencyKey: string) =>
    post<MarketplaceOrderDTO>(`/marketplace/cart/${cartId}/checkout`, payload, idempotencyHeaders(idempotencyKey)),
  stripePaymentIntent: (cartId: string, payload: CheckoutRequest, idempotencyKey: string) =>
    post<StripePaymentIntentDTO>(`/marketplace/cart/${cartId}/stripe/payment-intent`, payload, idempotencyHeaders(idempotencyKey)),
  datafastCheckout: (cartId: string, payload: CheckoutRequest, idempotencyKey: string) =>
    post<DatafastCheckoutDTO>(`/marketplace/cart/${cartId}/datafast/checkout`, payload, idempotencyHeaders(idempotencyKey)),
  confirmDatafastPayment: (orderId: string, resourcePath: string, lookupToken: string) => {
    const qs = new URLSearchParams();
    qs.set('orderId', orderId);
    qs.set('resourcePath', resourcePath);
    return get<MarketplaceOrderDTO>(`/marketplace/datafast/status?${qs.toString()}`, lookupHeaders(lookupToken));
  },
  createPaypalOrder: (cartId: string, payload: CheckoutRequest, idempotencyKey: string) =>
    post<PaypalCreateDTO>(`/marketplace/cart/${cartId}/paypal/create`, payload, idempotencyHeaders(idempotencyKey)),
  capturePaypalOrder: (payload: PaypalCaptureRequest, lookupToken: string) =>
    post<MarketplaceOrderDTO>('/marketplace/paypal/capture', payload, lookupHeaders(lookupToken)),
  getOrder: (orderId: string, lookupToken: string) =>
    get<MarketplaceOrderDTO>(`/marketplace/orders/${orderId}`, lookupHeaders(lookupToken)),
  listOrders: (params?: { status?: string; limit?: number; offset?: number }) => {
    const qs = new URLSearchParams();
    const status = params?.status?.trim();
    if (status) qs.set('status', status);
    if (typeof params?.limit === 'number' && Number.isFinite(params.limit)) {
      const normalizedLimit = Math.trunc(params.limit);
      if (normalizedLimit > 0) {
        qs.set('limit', String(normalizedLimit));
      }
    }
    if (typeof params?.offset === 'number' && Number.isFinite(params.offset)) {
      const normalizedOffset = Math.trunc(params.offset);
      if (normalizedOffset >= 0) {
        qs.set('offset', String(normalizedOffset));
      }
    }
    const query = qs.toString();
    const suffix = query ? `?${query}` : '';
    return get<MarketplaceOrderDTO[]>(`/marketplace/orders${suffix}`);
  },
  updateOrder: (orderId: string, payload: MarketplaceOrderUpdatePayload) =>
    put<MarketplaceOrderDTO>(`/marketplace/orders/${orderId}`, payload),
  updateFulfillment: (orderId: string, payload: MarketplaceFulfillmentUpdatePayload) =>
    put<MarketplaceOrderDTO>(`/marketplace/orders/${orderId}/fulfillment`, payload),
  updateRental: (orderId: string, payload: MarketplaceRentalUpdatePayload) =>
    put<MarketplaceOrderDTO>(`/marketplace/orders/${orderId}/rental`, payload),
};
