import { get, post } from './client';
import type {
  MarketplaceItemDTO,
  MarketplaceCartDTO,
  MarketplaceCartItemDTO,
  MarketplaceOrderDTO,
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
  getOrder: (orderId: string) => get<MarketplaceOrderDTO>(`/marketplace/orders/${orderId}`),
};
