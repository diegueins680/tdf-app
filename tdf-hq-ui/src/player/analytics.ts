import { buildAuthorizationHeader } from '../api/authHeader';
import { resolveApiBase } from '../config/apiBase';
import { logger } from '../utils/logger';
import { PLAYER_ANALYTICS_EVENT } from './types';

const ANONYMOUS_ID_KEY = 'tdf-player-anonymous-id/v1';
const UUID_PATTERN = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-8][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;

export interface PlayerAnalyticsDetail {
  schemaVersion: number;
  eventId: string;
  eventType: string;
  recordingId?: string;
  releaseVersionId?: string;
  positionMs: number;
  listenedDeltaMs?: number;
  quality?: string;
  occurredAt: string;
  [key: string]: unknown;
}

interface PlaybackSession {
  id: string;
  sequence: number;
  completed: boolean;
}

const sessions = new Map<string, PlaybackSession>();
let activeIdentity: string | undefined;
let volatileAnonymousId: string | undefined;
let storageUnavailable = false;

const anonymousId = (): string => {
  if (storageUnavailable) {
    volatileAnonymousId ??= `${crypto.randomUUID()}${crypto.randomUUID()}`;
    return volatileAnonymousId;
  }
  try {
    const existing = window.localStorage.getItem(ANONYMOUS_ID_KEY)?.trim();
    if (existing && existing.length >= 16) {
      volatileAnonymousId = existing;
      return existing;
    }
    const created = `${crypto.randomUUID()}${crypto.randomUUID()}`;
    volatileAnonymousId = created;
    window.localStorage.setItem(ANONYMOUS_ID_KEY, created);
    return created;
  } catch {
    storageUnavailable = true;
    volatileAnonymousId ??= `${crypto.randomUUID()}${crypto.randomUUID()}`;
    return volatileAnonymousId;
  }
};

const sessionFor = (detail: PlayerAnalyticsDetail): PlaybackSession => {
  const key = `${detail.releaseVersionId}:${detail.recordingId}`;
  const existing = sessions.get(key);
  if (!existing || (detail.eventType === 'play_start' && existing.completed)) {
    const created = { id: crypto.randomUUID(), sequence: 0, completed: false };
    sessions.set(key, created);
    return created;
  }
  return existing;
};

const endpoint = (authenticated: boolean): string => {
  const base = resolveApiBase().replace(/\/$/, '');
  return `${base}${authenticated ? '/music/me/playback-events' : '/music/playback-events'}`;
};

export const sendPlayerAnalytics = async (detail: PlayerAnalyticsDetail): Promise<void> => {
  if (!detail.recordingId || !detail.releaseVersionId) return;
  if (!UUID_PATTERN.test(detail.recordingId) || !UUID_PATTERN.test(detail.releaseVersionId)) return;
  const authHeader = buildAuthorizationHeader();
  const visitorId = authHeader ? undefined : anonymousId();
  const identity = authHeader ? `authenticated:${authHeader}` : `anonymous:${visitorId}`;
  if (identity !== activeIdentity) {
    sessions.clear();
    activeIdentity = identity;
  }
  const session = sessionFor(detail);
  const sequenceNumber = session.sequence;
  session.sequence += 1;
  if (detail.eventType === 'complete') session.completed = true;

  const { eventId, eventType, recordingId, releaseVersionId, positionMs, listenedDeltaMs, quality, occurredAt, ...metadata } = detail;
  const payload = {
    eventId,
    sessionId: session.id,
    sequenceNumber,
    ...(authHeader ? {} : { anonymousId: visitorId }),
    releaseVersionId,
    recordingId,
    eventType,
    positionMs,
    listenedDeltaMs: Math.max(0, Math.min(30_000, listenedDeltaMs ?? 0)),
    ...(quality ? { quality } : {}),
    occurredAt,
    metadata: {
      ...metadata,
      schemaVersion: detail.schemaVersion,
      pagePath: window.location.pathname,
    },
  };
  try {
    const response = await fetch(endpoint(Boolean(authHeader)), {
      method: 'POST',
      credentials: 'include',
      keepalive: true,
      headers: {
        'Content-Type': 'application/json',
        ...(authHeader ? { Authorization: authHeader } : {}),
      },
      body: JSON.stringify(payload),
    });
    if (!response.ok && response.status !== 404 && response.status !== 503) {
      logger.warn('Player analytics event was rejected', { status: response.status, eventType });
    }
  } catch (error) {
    logger.warn('Player analytics event could not be delivered', error);
  }
};

export const installPlayerAnalyticsBridge = (): (() => void) => {
  const handler = (event: Event) => {
    const detail = (event as CustomEvent<PlayerAnalyticsDetail>).detail;
    if (!detail || typeof detail.eventId !== 'string') return;
    void sendPlayerAnalytics(detail);
  };
  window.addEventListener(PLAYER_ANALYTICS_EVENT, handler as EventListener);
  return () => window.removeEventListener(PLAYER_ANALYTICS_EVENT, handler as EventListener);
};
