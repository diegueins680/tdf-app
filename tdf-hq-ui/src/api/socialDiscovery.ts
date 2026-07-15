import { get, put } from './client';

export type DiscoveryReviewStatus = 'pending' | 'approved' | 'dismissed';

export interface SocialDiscoveryPost {
  id: string;
  platform: string;
  sourceHandle?: string | null;
  caption?: string | null;
  permalink?: string | null;
  mediaUrls: string[];
  postedAt?: string | null;
  fetchedAt: string;
  detectedTerms: string[];
  reviewStatus: DiscoveryReviewStatus;
  reviewNotes?: string | null;
  reviewedAt?: string | null;
}

const buildQuery = (params: Record<string, string | number | undefined>) => {
  const query = new URLSearchParams();
  Object.entries(params).forEach(([key, value]) => {
    if (value !== undefined) query.set(key, String(value));
  });
  const encoded = query.toString();
  return encoded ? `?${encoded}` : '';
};

export const SocialDiscoveryAPI = {
  list: async (params: { status?: DiscoveryReviewStatus; limit?: number } = {}) =>
    get<SocialDiscoveryPost[]>(`/social-discovery/posts${buildQuery(params)}`),
  review: async (postId: string, payload: { status: DiscoveryReviewStatus; notes?: string }) =>
    put<SocialDiscoveryPost>(`/social-discovery/posts/${encodeURIComponent(postId)}/review`, payload),
};
