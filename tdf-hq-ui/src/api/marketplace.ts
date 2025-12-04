import { get, post, put } from './client';
import type {
  MarketplaceItemDTO,
  MarketplaceCartDTO,
  MarketplaceCartItemDTO,
  MarketplaceOrderDTO,
  PaypalCreateDTO,
  PaypalCaptureRequest,
  MarketplaceOrderUpdatePayload,
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
  createPaypalOrder: (cartId: string, payload: CheckoutRequest) =>
    post<PaypalCreateDTO>(`/marketplace/cart/${cartId}/paypal/create`, payload),
  capturePaypalOrder: (payload: PaypalCaptureRequest) =>
    post<MarketplaceOrderDTO>('/marketplace/paypal/capture', payload),
  getOrder: (orderId: string) => get<MarketplaceOrderDTO>(`/marketplace/orders/${orderId}`),
  listOrders: (params?: { status?: string; limit?: number; offset?: number }) => {
    const qs = new URLSearchParams();
    if (params?.status) qs.set('status', params.status);
    if (params?.limit) qs.set('limit', String(params.limit));
    if (params?.offset) qs.set('offset', String(params.offset));
    const query = qs.toString();
    const suffix = query ? `?${query}` : '';
    return get<MarketplaceOrderDTO[]>(`/marketplace/orders${suffix}`);
  },
  updateOrder: (orderId: string, payload: MarketplaceOrderUpdatePayload) =>
    put<MarketplaceOrderDTO>(`/marketplace/orders/${orderId}`, payload),
};
