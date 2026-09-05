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

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === 'object' && value !== null;

const reviewTargetKinds = new Set(['event', 'marketplace_listing', 'service_offering', 'service_package']);
const reviewSourceKinds = new Set([
  'event_ticket_order',
  'marketplace_order',
  'service_booking',
  'service_storefront_order',
]);
const reviewStatuses = new Set(['published', 'hidden', 'removed']);

const isReviewPage = (value: unknown): value is ExperienceReviewPage => {
  if (!isRecord(value)) return false;
  const items = value['items'];
  const summary = value['summary'];
  const nextCursor = value['nextCursor'];
  if (!Array.isArray(items) || !isRecord(summary)) return false;
  if (
    typeof summary['targetKind'] !== 'string'
    || !reviewTargetKinds.has(summary['targetKind'])
    || typeof summary['targetId'] !== 'string'
    || typeof summary['count'] !== 'number'
    || !Number.isFinite(summary['count'])
    || (summary['average'] != null && (
      typeof summary['average'] !== 'number' || !Number.isFinite(summary['average'])
    ))
    || (nextCursor != null && typeof nextCursor !== 'string')
  ) return false;

  return items.every((review) => (
    isRecord(review)
    && typeof review['id'] === 'string'
    && typeof review['targetKind'] === 'string'
    && reviewTargetKinds.has(review['targetKind'])
    && typeof review['targetId'] === 'string'
    && typeof review['rating'] === 'number'
    && Number.isFinite(review['rating'])
    && (review['body'] == null || typeof review['body'] === 'string')
    && typeof review['status'] === 'string'
    && reviewStatuses.has(review['status'])
    && typeof review['createdAt'] === 'string'
    && review['verified'] === true
    && typeof review['sourceKind'] === 'string'
    && reviewSourceKinds.has(review['sourceKind'])
    && isRecord(review['author'])
    && typeof review['author']['name'] === 'string'
    && (review['author']['avatarUrl'] == null || typeof review['author']['avatarUrl'] === 'string')
  ));
};

export const Reviews = {
  list: async (targetKind: ExperienceReviewTargetKind, targetId: string, cursor?: string, limit = 20) => {
    const params = new URLSearchParams({ limit: String(limit) });
    if (cursor) params.set('cursor', cursor);
    const response = await get<unknown>(
      `/reviews/${encodeURIComponent(targetKind)}/${encodeURIComponent(targetId)}?${params.toString()}`,
    );
    if (!isReviewPage(response)) {
      throw new Error('La respuesta de reseñas no tiene el formato esperado.');
    }
    return response;
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
