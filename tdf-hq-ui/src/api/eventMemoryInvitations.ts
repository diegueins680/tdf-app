import { get, post, put } from './client';

import type { components } from './generated/types';
export type EventMemoryPreference = components['schemas']['EventMemoryPreference'];
export type EventMemoryCampaign = components['schemas']['EventMemoryCampaign'];
const path = (eventId: string) => `/social-events/events/${encodeURIComponent(eventId)}/memory-invitations`;
const record = (value: unknown): value is Record<string, unknown> => Boolean(value) && typeof value === 'object' && !Array.isArray(value);
function preference(value: unknown): EventMemoryPreference {
  if (!record(value) || typeof value['inApp'] !== 'boolean' || typeof value['supportsExtendedMoments'] !== 'boolean') {
    throw new Error('No se pudo confirmar la preferencia de invitaciones.');
  }
  return value as unknown as EventMemoryPreference;
}
function campaign(value: unknown): EventMemoryCampaign {
  if (!record(value) || typeof value['enabled'] !== 'boolean' || typeof value['eventFinished'] !== 'boolean'
    || (value['dispatcherEnabled'] !== undefined && typeof value['dispatcherEnabled'] !== 'boolean')
    || ['candidates', 'consented', 'invitations', 'reminders', 'opened', 'published', 'moments', 'uniqueAuthors'].some((key) => !Number.isSafeInteger(value[key]) || Number(value[key]) < 0)) {
    throw new Error('No se pudieron confirmar los resultados de la campaña.');
  }
  return value as unknown as EventMemoryCampaign;
}
export const EventMemoryInvitations = {
  preferences: async () => preference(await get<unknown>('/social-events/me/memory-invitations')),
  setPreference: async (enabled: boolean) => {
    const value = preference(await put<unknown>('/social-events/me/memory-invitations', enabled));
    if (value.inApp !== enabled) throw new Error('No se pudo confirmar el cambio de preferencia.');
    return value;
  },
  summary: async (eventId: string) => campaign(await get<unknown>(path(eventId))),
  setCampaign: async (eventId: string, enabled: boolean) => {
    const value = campaign(await put<unknown>(path(eventId), enabled));
    if (value.enabled !== enabled) throw new Error('No se pudo confirmar el cambio de campaña.');
    return value;
  },
  open: async (eventId: string, invitation: string) => {
    const value = await post<unknown>(`${path(eventId)}/${encodeURIComponent(invitation)}/open`, undefined);
    if (!record(value) || typeof value['recorded'] !== 'boolean') throw new Error('No se pudo confirmar la apertura.');
    return { recorded: value['recorded'] };
  },
};
