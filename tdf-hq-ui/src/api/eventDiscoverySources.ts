import { get, post, put } from './client';

export type EventDiscoverySourceType = 'ticketmaster' | 'buenplan' | 'ical' | 'json' | 'web';

export interface EventDiscoverySource {
  discoverySourceId: string;
  discoverySourceKey: string;
  discoverySourceName: string;
  discoverySourceType: EventDiscoverySourceType;
  discoverySourceFeedUrl?: string | null;
  discoverySourceCityId?: string | null;
  discoverySourceCityName?: string | null;
  discoverySourceCountryCode?: string | null;
  discoverySourceEnabled: boolean;
  discoverySourcePriority: number;
  discoverySourceConsecutiveFailures: number;
  discoverySourceLastSuccessAt?: string | null;
  discoverySourceLastError?: string | null;
  discoverySourceUpdatedAt: string;
}

export interface EventDiscoverySourceWrite {
  discoverySourceWriteKey: string;
  discoverySourceWriteName: string;
  discoverySourceWriteType: EventDiscoverySourceType;
  discoverySourceWriteFeedUrl?: string | null;
  discoverySourceWriteCityId?: string | null;
  discoverySourceWriteEnabled: boolean;
  discoverySourceWritePriority: number;
}

export const EventDiscoverySourcesAPI = {
  list: () => get<EventDiscoverySource[]>('/social-events/event-sources'),
  create: (payload: EventDiscoverySourceWrite) =>
    post<EventDiscoverySource>('/social-events/event-sources', payload),
  update: (sourceId: string, payload: EventDiscoverySourceWrite) =>
    put<EventDiscoverySource>(`/social-events/event-sources/${sourceId}`, payload),
};
