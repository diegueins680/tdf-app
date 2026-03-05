import { get, post, put } from './client';
import type {
  MarketplaceItemDTO,
  MarketplaceCartDTO,
  MarketplaceOrderDTO,
  PaypalCreateDTO,
  PaypalCaptureRequest,
  MarketplaceOrderUpdatePayload,
  DatafastCheckoutDTO,
} from './types';

export interface CartItemUpdate {
  mciuListingId: string;
  mciuQuantity: number;
}

export interface CheckoutRequest {
  mcrBuyerName: string;
  mcrBuyerEmail: string;
  mcrBuyerPhone?: string | null;
}

export const Marketplace = {
  list: () => get<MarketplaceItemDTO[]>('/marketplace'),
  detail: (listingId: string) => get<MarketplaceItemDTO>(`/marketplace/${listingId}`),
  createCart: () => post<MarketplaceCartDTO>('/marketplace/cart', {}),
  getCart: (cartId: string) => get<MarketplaceCartDTO>(`/marketplace/cart/${cartId}`),
  upsertItem: (cartId: string, payload: CartItemUpdate) =>
    post<MarketplaceCartDTO>(`/marketplace/cart/${cartId}/items`, payload),
  checkout: (cartId: string, payload: CheckoutRequest) =>
    post<MarketplaceOrderDTO>(`/marketplace/cart/${cartId}/checkout`, payload),
  datafastCheckout: (cartId: string, payload: CheckoutRequest) =>
    post<DatafastCheckoutDTO>(`/marketplace/cart/${cartId}/datafast/checkout`, payload),
  confirmDatafastPayment: (orderId: string, resourcePath: string) => {
    const qs = new URLSearchParams();
    qs.set('orderId', orderId);
    qs.set('resourcePath', resourcePath);
    return get<MarketplaceOrderDTO>(`/marketplace/datafast/status?${qs.toString()}`);
  },
  createPaypalOrder: (cartId: string, payload: CheckoutRequest) =>
    post<PaypalCreateDTO>(`/marketplace/cart/${cartId}/paypal/create`, payload),
  capturePaypalOrder: (payload: PaypalCaptureRequest) =>
    post<MarketplaceOrderDTO>('/marketplace/paypal/capture', payload),
  getOrder: (orderId: string) => get<MarketplaceOrderDTO>(`/marketplace/orders/${orderId}`),
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
};
