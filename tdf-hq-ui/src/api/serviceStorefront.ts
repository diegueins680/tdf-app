import { get, post, put } from './client';

export interface ServiceStorefrontPackageDTO {
  sspId: string;
  sspServiceKind: string;
  sspTier: string;
  sspName: string;
  sspDescription?: string | null;
  sspPriceUsdCents: number;
  sspCurrency: string;
  sspMinSongCount: number;
  sspMaxSongCount: number;
  sspTurnaroundDays: number;
  sspRevisionCount: number;
  sspDeliverables?: string[] | null;
  sspFeatures?: string[] | null;
  sspActive: boolean;
  sspSortOrder: number;
}

export interface ServiceStorefrontOrderCreate {
  ssocPackageId: string;
  ssocBuyerName: string;
  ssocBuyerEmail: string;
  ssocBuyerPhone?: string | null;
  ssocArtistName?: string | null;
  ssocGenre?: string | null;
  ssocSongCount?: number | null;
  ssocNotes?: string | null;
  ssocReferenceTrackUrl?: string | null;
  ssocDeadline?: string | null;
}

export interface ServiceStorefrontOrderDTO {
  ssoId: string;
  ssoOrderNumber: string;
  ssoBuyerName: string;
  ssoBuyerEmail: string;
  ssoBuyerPhone?: string | null;
  ssoArtistName?: string | null;
  ssoPackageId: string;
  ssoServiceKind: string;
  ssoTier: string;
  ssoPriceUsdCents: number;
  ssoCurrency: string;
  ssoStatus: string;
  ssoPaymentProvider?: string | null;
  /** Returned only once, when a guest order is created. */
  ssoLookupToken?: string | null;
  ssoPaidAt?: string | null;
  ssoGenre?: string | null;
  ssoSongCount: number;
  ssoNotes?: string | null;
  ssoReferenceTrackUrl?: string | null;
  ssoDeadline?: string | null;
  ssoDeliverablesUrl?: string | null;
  ssoCreatedAt: string;
  ssoUpdatedAt: string;
}

export interface ServiceStorefrontOrderUpdate {
  ssouStatus?: string | null;
  ssouDeliverablesUrl?: string | null;
  ssouNotes?: string | null;
}

export interface ServiceStorefrontRevisionCreate {
  ssrcFeedback: string;
}

export interface ServiceStorefrontRevisionDTO {
  ssrId: string;
  ssrOrderId: string;
  ssrRevisionNumber: number;
  ssrFeedback: string;
  ssrStatus: string;
  ssrCreatedAt: string;
  ssrCompletedAt?: string | null;
}

const lookupHeaders = (lookupToken: string): HeadersInit => ({
  'X-Order-Lookup-Token': lookupToken,
});

export const ServiceStorefront = {
  // Public endpoints
  listPackages: () =>
    get<ServiceStorefrontPackageDTO[]>('/services/storefront'),

  getPackage: (packageId: string) =>
    get<ServiceStorefrontPackageDTO>(`/services/storefront/${packageId}`),

  createOrder: (idempotencyKey: string, payload: ServiceStorefrontOrderCreate) =>
    post<ServiceStorefrontOrderDTO>('/services/storefront/order', payload, {
      headers: { 'Idempotency-Key': idempotencyKey },
    }),

  getOrder: (orderId: string, lookupToken: string) =>
    get<ServiceStorefrontOrderDTO>(`/services/storefront/order/${orderId}`, {
      headers: lookupHeaders(lookupToken),
    }),

  createRevision: (orderId: string, lookupToken: string, payload: ServiceStorefrontRevisionCreate) =>
    post<ServiceStorefrontRevisionDTO>(`/services/storefront/order/${orderId}/revision`, payload, {
      headers: lookupHeaders(lookupToken),
    }),

  // Payment endpoints
  createStripePaymentIntent: (orderId: string, lookupToken: string) =>
    post<{ spiPaymentIntentId: string; spiClientSecret: string }>(
      `/services/storefront/order/${orderId}/stripe/payment-intent`,
      {},
      { headers: lookupHeaders(lookupToken) },
    ),

  createDatafastCheckout: (orderId: string, lookupToken: string) =>
    post<{
      dcOrderId: string;
      dcCheckoutId: string;
      dcWidgetUrl: string;
      dcAmount: string;
      dcCurrency: string;
    }>(`/services/storefront/order/${orderId}/datafast/checkout`, {}, {
      headers: lookupHeaders(lookupToken),
    }),

  confirmDatafastPayment: (orderId: string, lookupToken: string, resourcePath: string) => {
    const qs = new URLSearchParams();
    qs.set('orderId', orderId);
    qs.set('resourcePath', resourcePath);
    return get<ServiceStorefrontOrderDTO>(
      `/services/storefront/datafast/status?${qs.toString()}`,
      { headers: lookupHeaders(lookupToken) },
    );
  },

  createPaypalOrder: (orderId: string, lookupToken: string) =>
    post<{
      pcOrderId: string;
      pcPaypalOrderId: string;
      pcApprovalUrl?: string | null;
    }>(`/services/storefront/order/${orderId}/paypal/create`, {}, {
      headers: lookupHeaders(lookupToken),
    }),

  capturePaypalOrder: (paypalOrderId: string, orderId: string, lookupToken: string) =>
    post<ServiceStorefrontOrderDTO>('/services/storefront/paypal/capture', {
      pcCaptureOrderId: orderId,
      pcCapturePaypalId: paypalOrderId,
    }, {
      headers: lookupHeaders(lookupToken),
    }),

  selectManualPayment: (orderId: string, lookupToken: string, paymentMethod = 'bank_transfer') =>
    post<ServiceStorefrontOrderDTO>(`/services/storefront/order/${orderId}/manual-payment`, {
      ssmPaymentMethod: paymentMethod,
    }, {
      headers: lookupHeaders(lookupToken),
    }),

  // Admin endpoints
  listOrders: (params?: { status?: string; limit?: number; offset?: number }) => {
    const qs = new URLSearchParams();
    if (params?.status) qs.set('status', params.status);
    if (params?.limit) qs.set('limit', String(params.limit));
    if (params?.offset) qs.set('offset', String(params.offset));
    const query = qs.toString();
    return get<ServiceStorefrontOrderDTO[]>(
      `/admin/services/storefront/orders${query ? `?${query}` : ''}`,
    );
  },

  updateOrder: (orderId: string, payload: ServiceStorefrontOrderUpdate) =>
    put<ServiceStorefrontOrderDTO>(
      `/admin/services/storefront/orders/${orderId}`,
      payload,
    ),
};
