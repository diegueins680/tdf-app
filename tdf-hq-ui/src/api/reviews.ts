import { get, post } from './client';
import type { components } from './generated/types';

export type ExperienceReviewTargetKind = components['schemas']['ExperienceReviewTargetKind'];
export type ExperienceReviewPage = components['schemas']['ExperienceReviewPage'];
export type ExperienceReview = components['schemas']['ExperienceReview'];
export type ExperienceReviewEligibility = components['schemas']['ExperienceReviewEligibility'];
export type ExperienceReviewCreate = components['schemas']['ExperienceReviewCreate'];

const idempotencyHeaders = (key?: string) => ({
  headers: { 'Idempotency-Key': key ?? crypto.randomUUID() },
});

export const Reviews = {
  list: (targetKind: ExperienceReviewTargetKind, targetId: string, cursor?: string, limit = 20) => {
    const params = new URLSearchParams({ limit: String(limit) });
    if (cursor) params.set('cursor', cursor);
    return get<ExperienceReviewPage>(
      `/reviews/${encodeURIComponent(targetKind)}/${encodeURIComponent(targetId)}?${params.toString()}`,
    );
  },
  eligibility: (targetKind?: ExperienceReviewTargetKind, targetId?: string) => {
    const params = new URLSearchParams();
    if (targetKind) params.set('targetKind', targetKind);
    if (targetId) params.set('targetId', targetId);
    const query = params.toString();
    return get<ExperienceReviewEligibility[]>(`/reviews/eligibility${query ? `?${query}` : ''}`);
  },
  create: (body: ExperienceReviewCreate, idempotencyKey?: string) =>
    post<ExperienceReview>('/reviews', body, idempotencyHeaders(idempotencyKey)),
};
