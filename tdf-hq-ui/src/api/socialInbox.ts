import { get } from './client';

export interface SocialMessage {
  externalId: string;
  senderId: string;
  senderName?: string | null;
  text?: string | null;
  direction: string;
  repliedAt?: string | null;
  replyText?: string | null;
  replyError?: string | null;
  createdAt: string;
}

type SocialInboxFilters = {
  limit?: number;
  direction?: 'incoming' | 'outgoing' | 'all';
  repliedOnly?: boolean;
};

const buildQuery = (params: Record<string, string | number | boolean | undefined | null>) => {
  const qs = new URLSearchParams();
  Object.entries(params).forEach(([key, value]) => {
    if (value === undefined || value === null) return;
    qs.set(key, String(value));
  });
  const raw = qs.toString();
  return raw ? `?${raw}` : '';
};

export const SocialInboxAPI = {
  listInstagramMessages: (filters: SocialInboxFilters = {}) =>
    get<SocialMessage[]>(
      `/instagram/messages${buildQuery({
        limit: filters.limit,
        direction: filters.direction,
        repliedOnly: filters.repliedOnly,
      })}`,
    ),
  listWhatsAppMessages: (filters: SocialInboxFilters = {}) =>
    get<SocialMessage[]>(
      `/whatsapp/messages${buildQuery({
        limit: filters.limit,
        direction: filters.direction,
        repliedOnly: filters.repliedOnly,
      })}`,
    ),
};
