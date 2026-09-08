import type { components } from './generated/types';
import { del, get, patch, post, postForm, put } from './client';

export type MerchCapabilities = components['schemas']['MerchCapabilities'];
export type MerchStorefront = components['schemas']['MerchStorefront'];
export type MerchProduct = components['schemas']['MerchProduct'];
export type MerchProductRequest = components['schemas']['MerchProductRequest'];
export type MerchStockRequest = components['schemas']['MerchStockRequest'];
export type MerchCart = components['schemas']['MerchCart'];
export type MerchCartItemRequest = components['schemas']['MerchCartItemRequest'];
export type MerchCheckoutRequest = components['schemas']['MerchCheckoutRequest'];
export type MerchOrder = components['schemas']['MerchOrder'];
export type MerchStoreApplicationRequest = components['schemas']['MerchStoreApplicationRequest'];
export type MerchStoreUpdateRequest = components['schemas']['MerchStoreUpdateRequest'];
export type MerchPermissions = components['schemas']['MerchPermissions'];
export type MerchStoreMember = components['schemas']['MerchStoreMember'];
export type MerchMemberInviteRequest = components['schemas']['MerchMemberInviteRequest'];
export type MerchPolicyRequest = components['schemas']['MerchPolicyRequest'];
export type MerchShippingZoneRequest = components['schemas']['MerchShippingZoneRequest'];
export type MerchFulfillmentRequest = components['schemas']['MerchFulfillmentRequest'];

const CART_KEY_PREFIX = 'tdf-merch-cart:';
const ORDER_KEY_PREFIX = 'tdf-merch-order:';

type StoredCapability = Readonly<{ id: string; token: string }>;

const storage = () => (typeof window === 'undefined' ? null : window.sessionStorage);

const readCapability = (key: string): StoredCapability | null => {
  try {
    const raw = storage()?.getItem(key);
    if (!raw) return null;
    const value = JSON.parse(raw) as Partial<StoredCapability>;
    return typeof value.id === 'string' && typeof value.token === 'string'
      ? { id: value.id, token: value.token }
      : null;
  } catch {
    return null;
  }
};

const writeCapability = (key: string, value: StoredCapability) => {
  storage()?.setItem(key, JSON.stringify(value));
};

export const createMerchIdempotencyKey = (scope: string) =>
  `${scope}:${globalThis.crypto?.randomUUID?.() ?? `${Date.now()}-${Math.random().toString(16).slice(2)}`}`;

export const readStoredMerchCart = (storeSlug: string) =>
  readCapability(`${CART_KEY_PREFIX}${storeSlug}`);

export const storeMerchCart = (storeSlug: string, cart: MerchCart) => {
  if (!cart.lookupToken) throw new Error('La API no devolvió la capacidad privada del carrito.');
  writeCapability(`${CART_KEY_PREFIX}${storeSlug}`, { id: cart.id, token: cart.lookupToken });
};

export const readStoredMerchOrder = (orderId: string) =>
  readCapability(`${ORDER_KEY_PREFIX}${orderId}`);

export const storeMerchOrder = (order: MerchOrder, fallbackToken?: string) => {
  const token = order.lookupToken ?? fallbackToken;
  if (!token) throw new Error('La API no devolvió la capacidad privada de seguimiento.');
  writeCapability(`${ORDER_KEY_PREFIX}${order.id}`, { id: order.id, token });
};

const cartHeaders = (token: string): HeadersInit => ({ 'X-Cart-Lookup-Token': token });
const orderHeaders = (token: string): HeadersInit => ({ 'X-Order-Lookup-Token': token });

export const Merch = {
  capabilities: () => get<MerchCapabilities>('/merch/capabilities'),
  storefronts: (query: { q?: string; category?: string } = {}) => {
    const search = new URLSearchParams();
    if (query.q) search.set('q', query.q);
    if (query.category) search.set('category', query.category);
    const suffix = search.size ? `?${search}` : '';
    return get<MerchStorefront[]>(`/merch/storefronts${suffix}`);
  },
  storefront: (storeSlug: string) =>
    get<MerchStorefront>(`/merch/storefronts/${encodeURIComponent(storeSlug)}`),
  product: (storeSlug: string, productSlug: string) =>
    get<MerchProduct>(`/merch/storefronts/${encodeURIComponent(storeSlug)}/products/${encodeURIComponent(productSlug)}`),
  createCart: (storeSlug: string) =>
    post<MerchCart>('/merch/carts', { storeSlug }),
  cart: (cartId: string, token: string) =>
    get<MerchCart>(`/merch/carts/${encodeURIComponent(cartId)}`, { headers: cartHeaders(token) }),
  putCartItem: (cartId: string, token: string, payload: MerchCartItemRequest) =>
    put<MerchCart>(`/merch/carts/${encodeURIComponent(cartId)}/items`, payload, { headers: cartHeaders(token) }),
  deleteCartItem: (cartId: string, token: string, variantId: string) =>
    del<MerchCart>(`/merch/carts/${encodeURIComponent(cartId)}/items/${encodeURIComponent(variantId)}`, { headers: cartHeaders(token) }),
  checkout: (cartId: string, token: string, idempotencyKey: string, payload: MerchCheckoutRequest) =>
    post<MerchOrder>(`/merch/carts/${encodeURIComponent(cartId)}/checkout`, payload, {
      headers: { ...cartHeaders(token), 'Idempotency-Key': idempotencyKey },
    }),
  order: (orderId: string, token: string) =>
    get<MerchOrder>(`/merch/orders/${encodeURIComponent(orderId)}`, { headers: orderHeaders(token) }),
  reportIssue: (orderId: string, token: string, issueType: string, message: string, idempotencyKey: string) =>
    post<Record<string, unknown>>(`/merch/orders/${encodeURIComponent(orderId)}/issues`, { issueType, message }, {
      headers: { ...orderHeaders(token), 'Idempotency-Key': idempotencyKey },
    }),
  favorite: (productId: string) => put<void>(`/merch/favorites/${encodeURIComponent(productId)}`, {}),
  unfavorite: (productId: string) => del<void>(`/merch/favorites/${encodeURIComponent(productId)}`),
  sellerStores: () => get<MerchStorefront[]>('/merch/seller/stores'),
  applyForStore: (payload: MerchStoreApplicationRequest, idempotencyKey: string) =>
    post<MerchStorefront>('/merch/seller/applications', payload, { headers: { 'Idempotency-Key': idempotencyKey } }),
  updateStore: (storeId: string, payload: MerchStoreUpdateRequest) =>
    put<MerchStorefront>(`/merch/seller/stores/${encodeURIComponent(storeId)}`, payload),
  members: (storeId: string) =>
    get<MerchStoreMember[]>(`/merch/seller/stores/${encodeURIComponent(storeId)}/members`),
  inviteMember: (storeId: string, payload: MerchMemberInviteRequest, idempotencyKey: string) =>
    post<MerchStoreMember>(`/merch/seller/stores/${encodeURIComponent(storeId)}/members`, payload, {
      headers: { 'Idempotency-Key': idempotencyKey },
    }),
  createPolicy: (storeId: string, payload: MerchPolicyRequest) =>
    post<Record<string, unknown>>(`/merch/seller/stores/${encodeURIComponent(storeId)}/policies`, payload),
  createShippingZone: (storeId: string, payload: MerchShippingZoneRequest) =>
    post<Record<string, unknown>>(`/merch/seller/stores/${encodeURIComponent(storeId)}/shipping-zones`, payload),
  sellerProducts: (storeId: string) =>
    get<MerchProduct[]>(`/merch/seller/stores/${encodeURIComponent(storeId)}/products`),
  createProduct: (storeId: string, payload: MerchProductRequest, idempotencyKey: string) =>
    post<MerchProduct>(`/merch/seller/stores/${encodeURIComponent(storeId)}/products`, payload, {
      headers: { 'Idempotency-Key': idempotencyKey },
    }),
  updateProductStatus: (storeId: string, productId: string, status: string, reason?: string) =>
    patch<MerchProduct>(`/merch/seller/stores/${encodeURIComponent(storeId)}/products/${encodeURIComponent(productId)}/status`, { status, reason }),
  uploadProductImage: (storeId: string, productId: string, file: File, altText: string, sortOrder: number) => {
    const form = new FormData();
    form.set('file', file);
    form.set('altText', altText);
    form.set('sortOrder', String(sortOrder));
    return postForm<Record<string, unknown>>(`/merch/seller/stores/${encodeURIComponent(storeId)}/products/${encodeURIComponent(productId)}/images`, form);
  },
  updateVariantStock: (storeId: string, variantId: string, payload: MerchStockRequest) =>
    patch<components['schemas']['MerchVariant']>(`/merch/seller/stores/${encodeURIComponent(storeId)}/variants/${encodeURIComponent(variantId)}/stock`, payload),
  sellerOrders: (storeId: string, status?: string) =>
    get<MerchOrder[]>(`/merch/seller/stores/${encodeURIComponent(storeId)}/orders${status ? `?status=${encodeURIComponent(status)}` : ''}`),
  updateFulfillment: (storeId: string, orderId: string, payload: MerchFulfillmentRequest) =>
    patch<MerchOrder>(`/merch/seller/stores/${encodeURIComponent(storeId)}/orders/${encodeURIComponent(orderId)}/fulfillment`, payload),
  adminStores: (status?: string) =>
    get<MerchStorefront[]>(`/merch/admin/stores${status ? `?status=${encodeURIComponent(status)}` : ''}`),
  reviewStore: (storeId: string, payload: components['schemas']['MerchStoreReviewRequest']) =>
    post<MerchStorefront>(`/merch/admin/stores/${encodeURIComponent(storeId)}/review`, payload),
  adminProducts: (status?: string) =>
    get<MerchProduct[]>(`/merch/admin/products${status ? `?status=${encodeURIComponent(status)}` : ''}`),
  reviewProduct: (productId: string, status: 'published' | 'rejected', reason?: string) =>
    post<MerchProduct>(`/merch/admin/products/${encodeURIComponent(productId)}/review`, { status, reason }),
};
