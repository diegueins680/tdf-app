import { get, post, put } from './client';

import type { components } from './generated/types';
export type EventMemoryPreference = components['schemas']['EventMemoryPreference'];
export type EventMemoryCampaign = components['schemas']['EventMemoryCampaign'];
const path = (eventId: string) => `/social-events/events/${encodeURIComponent(eventId)}/memory-invitations`;
export const EventMemoryInvitations = {
  preferences: () => get<EventMemoryPreference>('/social-events/me/memory-invitations'),
  setPreference: (enabled: boolean) => put<EventMemoryPreference>('/social-events/me/memory-invitations', enabled),
  summary: (eventId: string) => get<EventMemoryCampaign>(path(eventId)),
  setCampaign: (eventId: string, enabled: boolean) => put<EventMemoryCampaign>(path(eventId), enabled),
  open: (eventId: string, invitation: string) => post<{ recorded: boolean }>(`${path(eventId)}/${encodeURIComponent(invitation)}/open`, undefined),
};
