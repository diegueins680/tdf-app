import { get, post, put } from './client';

export interface ServiceStorefrontPackageDTO {
  sspId: string;
  sspServiceKind: string;
  sspTier: string;
  sspName: string;
  sspDescription?: string | null;
  sspPriceUsdCents: number;
  sspCurrency: string;
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

export const ServiceStorefront = {
  // Public endpoints
  listPackages: () =>
    get<ServiceStorefrontPackageDTO[]>('/services/storefront'),

  getPackage: (packageId: string) =>
    get<ServiceStorefrontPackageDTO>(`/services/storefront/${packageId}`),

  createOrder: (payload: ServiceStorefrontOrderCreate) =>
    post<ServiceStorefrontOrderDTO>('/services/storefront/order', payload),

  getOrder: (orderId: string) =>
    get<ServiceStorefrontOrderDTO>(`/services/storefront/order/${orderId}`),

  createRevision: (orderId: string, payload: ServiceStorefrontRevisionCreate) =>
    post<ServiceStorefrontRevisionDTO>(`/services/storefront/order/${orderId}/revision`, payload),

  // Payment endpoints
  createStripePaymentIntent: (orderId: string) =>
    post<{ spiPaymentIntentId: string; spiClientSecret: string }>(
      `/services/storefront/order/${orderId}/stripe/payment-intent`,
      {},
    ),

  createDatafastCheckout: (orderId: string) =>
    post<{
      dcOrderId: string;
      dcCheckoutId: string;
      dcWidgetUrl: string;
      dcAmount: string;
      dcCurrency: string;
    }>(`/services/storefront/order/${orderId}/datafast/checkout`, {}),

  confirmDatafastPayment: (orderId: string, resourcePath: string) => {
    const qs = new URLSearchParams();
    qs.set('orderId', orderId);
    qs.set('resourcePath', resourcePath);
    return get<ServiceStorefrontOrderDTO>(
      `/services/storefront/datafast/status?${qs.toString()}`,
    );
  },

  createPaypalOrder: (orderId: string) =>
    post<{
      pcOrderId: string;
      pcPaypalOrderId: string;
      pcApprovalUrl?: string | null;
    }>(`/services/storefront/order/${orderId}/paypal/create`, {}),

  capturePaypalOrder: (paypalOrderId: string, orderId: string) =>
    post<ServiceStorefrontOrderDTO>('/services/storefront/paypal/capture', {
      pcCaptureOrderId: orderId,
      pcCapturePaypalId: paypalOrderId,
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
