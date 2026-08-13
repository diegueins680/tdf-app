import { get, patch, post } from './client';
import type { FeatureAction } from '../features/featureRegistry';

export type FeatureAccessRequestStatus = 'pending' | 'approved' | 'rejected' | 'cancelled' | 'expired';

export interface FeatureAccessRequestHistoryDTO {
  id: number;
  transition: string;
  fromStatus: FeatureAccessRequestStatus | null;
  toStatus: FeatureAccessRequestStatus;
  note: string | null;
  createdAt: string;
}

export interface FeatureAccessRequestDTO {
  id: number;
  requesterPartyId: number;
  featureId: string;
  action: FeatureAction;
  roleContext: string[];
  moduleContext: string[];
  status: FeatureAccessRequestStatus;
  reviewerGroup: string;
  justification: string | null;
  reviewerNotes: string | null;
  requestedAt: string;
  updatedAt: string;
  decidedAt: string | null;
  cancelledAt: string | null;
  expiresAt: string | null;
  history: FeatureAccessRequestHistoryDTO[];
}

export interface CreateFeatureAccessRequest {
  featureId: string;
  action: FeatureAction;
  justification: string | null;
}

export const AccessRequests = {
  listMine: () => get<FeatureAccessRequestDTO[]>('/access-requests'),
  create: (payload: CreateFeatureAccessRequest) =>
    post<FeatureAccessRequestDTO>('/access-requests', payload),
  listReview: (status: FeatureAccessRequestStatus = 'pending') =>
    get<FeatureAccessRequestDTO[]>(`/access-requests/review?status=${encodeURIComponent(status)}`),
  decide: (requestId: number, decision: 'approved' | 'rejected', notes: string | null) =>
    patch<FeatureAccessRequestDTO>(`/access-requests/${requestId}/decision`, { decision, notes }),
  cancel: (requestId: number, cancellationNote: string | null = null) =>
    patch<FeatureAccessRequestDTO>(`/access-requests/${requestId}/cancel`, { cancellationNote }),
};
