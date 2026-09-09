import type { SocialRsvpStatus } from '../api/socialEvents';
import { canonicalEventPath } from './eventSharing';

const STORAGE_KEY = 'tdf:event-rsvp-intent:v1';
const TTL_MS = 30 * 60 * 1000;
const ALLOWED_ORIGINS = new Set(['public_event_detail', 'internal_event_detail', 'event_card']);

export interface EventRsvpIntent {
  version: 1;
  nonce: string;
  eventId: string;
  status: SocialRsvpStatus;
  showOnProfile: boolean;
  returnTo: string;
  origin: 'public_event_detail' | 'internal_event_detail' | 'event_card';
  sharedAttribution: boolean;
  createdAt: number;
  expiresAt: number;
}

type StorageLike = Pick<Storage, 'getItem' | 'setItem' | 'removeItem'>;

export function saveEventRsvpIntent(
  input: Pick<EventRsvpIntent, 'eventId' | 'status' | 'showOnProfile' | 'origin'> & { sharedAttribution?: boolean },
  storage: StorageLike = window.sessionStorage,
  now = Date.now(),
): EventRsvpIntent {
  const returnTo = canonicalEventPath(input.eventId);
  if (!ALLOWED_ORIGINS.has(input.origin)) throw new Error('Invalid RSVP intent origin.');
  const nonce = globalThis.crypto?.randomUUID?.() ?? `${now}-${Math.random().toString(36).slice(2)}`;
  const intent: EventRsvpIntent = {
    version: 1,
    nonce,
    eventId: input.eventId.trim(),
    status: input.status,
    showOnProfile: input.showOnProfile,
    returnTo,
    origin: input.origin,
    sharedAttribution: input.sharedAttribution === true,
    createdAt: now,
    expiresAt: now + TTL_MS,
  };
  storage.setItem(STORAGE_KEY, JSON.stringify(intent));
  return intent;
}

export function readEventRsvpIntent(
  expectedEventId?: string | null,
  storage: StorageLike = window.sessionStorage,
  now = Date.now(),
): EventRsvpIntent | null {
  const raw = storage.getItem(STORAGE_KEY);
  if (!raw) return null;
  try {
    const value = JSON.parse(raw) as Partial<EventRsvpIntent>;
    const eventId = typeof value.eventId === 'string' ? value.eventId.trim() : '';
    canonicalEventPath(eventId);
    if (
      value.version !== 1
      || typeof value.nonce !== 'string'
      || value.nonce.length < 8
      || !['accepted', 'maybe', 'declined'].includes(value.status ?? '')
      || typeof value.showOnProfile !== 'boolean'
      || !ALLOWED_ORIGINS.has(value.origin ?? '')
      || typeof value.sharedAttribution !== 'boolean'
      || value.returnTo !== canonicalEventPath(eventId)
      || typeof value.createdAt !== 'number'
      || typeof value.expiresAt !== 'number'
      || value.expiresAt <= now
      || value.expiresAt - value.createdAt !== TTL_MS
      || (expectedEventId && expectedEventId.trim() !== eventId)
    ) return null;
    return value as EventRsvpIntent;
  } catch {
    return null;
  }
}

export function clearEventRsvpIntent(storage: StorageLike = window.sessionStorage): void {
  storage.removeItem(STORAGE_KEY);
}

export const eventRsvpIntentStorageKey = STORAGE_KEY;
