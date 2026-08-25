import { get, post } from './client';

export type CommerceProviderEventStatus =
  | 'pending'
  | 'processing'
  | 'processed'
  | 'retry'
  | 'dead_letter'
  | 'ignored';

export interface CommerceProviderEvent {
  cpeId: string;
  cpeProvider: string;
  cpeEnvironment: 'sandbox' | 'production';
  cpeProviderEventId: string;
  cpeEventType: string;
  cpeProviderResourceId?: string | null;
  cpeStatus: CommerceProviderEventStatus;
  cpeAttemptCount: number;
  cpeCheckoutId?: string | null;
  cpePaymentAttemptId?: string | null;
  cpeRefundId?: string | null;
  cpeReceivedAt: string;
  cpeProviderCreatedAt?: string | null;
  cpeProcessingStartedAt?: string | null;
  cpeLastAttemptAt?: string | null;
  cpeNextAttemptAt?: string | null;
  cpeProcessedAt?: string | null;
  cpeErrorSummary?: string | null;
}

export const CommerceOperations = {
  listProviderEvents: (params?: {
    status?: CommerceProviderEventStatus;
    limit?: number;
    offset?: number;
  }) => {
    const query = new URLSearchParams();
    if (params?.status) query.set('status', params.status);
    if (params?.limit) query.set('limit', String(params.limit));
    if (params?.offset) query.set('offset', String(params.offset));
    const suffix = query.toString();
    return get<CommerceProviderEvent[]>(
      `/admin/commerce/provider-events${suffix ? `?${suffix}` : ''}`,
    );
  },

  replayProviderEvent: (eventId: string, reason: string) =>
    post<CommerceProviderEvent>(
      `/admin/commerce/provider-events/${encodeURIComponent(eventId)}/replay`,
      { cperReason: reason },
    ),
};
