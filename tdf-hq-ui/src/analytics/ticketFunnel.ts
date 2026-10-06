import type { AnalyticsClient } from './posthog';
import type { GrowthAttribution } from './growthAttribution';

export type TicketFunnelPhase = 'event_impression' | 'event_view' | 'ticket_selected'
  | 'checkout_started' | 'payment_initiated' | 'payment_completed' | 'ticket_issued'
  | 'ticket_opened' | 'check_in';

export interface TicketFunnelObservation {
  eventId: number;
  tierId?: number;
  quantity?: number;
  provider?: 'paypal' | 'datafast' | 'placetopay' | 'payphone';
  hasPromotion?: boolean;
  // Local deduplication only: never included in an analytics payload.
  privateScope?: string;
}

const STORAGE_KEY = 'tdf:ticket-funnel:observed:v1';
const MAX_OBSERVATIONS = 200;
const positiveInteger = (value: unknown): value is number =>
  typeof value === 'number' && Number.isSafeInteger(value) && value > 0;

const campaignCode = (value: unknown): string | undefined => {
  if (typeof value !== 'string' || !/^[a-z][a-z0-9_-]{0,63}$/i.test(value)) return undefined;
  if (/^(?:tdf-|event-ticket-|[a-f0-9]{8}-[a-f0-9]{4}-)/i.test(value)) return undefined;
  return value;
};

/** Browser observations only. Financial totals and admissions remain server-authoritative. */
export function createTicketFunnelTracker(
  analytics: Pick<AnalyticsClient, 'ready' | 'capture'>,
  storage: Pick<Storage, 'getItem' | 'setItem'> | null,
  attribution: () => GrowthAttribution | null,
) {
  const seen = new Set<string>();
  const pruneSeen = () => {
    while (seen.size > MAX_OBSERVATIONS) {
      const oldest = seen.values().next().value;
      if (oldest === undefined) break;
      seen.delete(oldest);
    }
  };
  const readSeen = () => { try {
    const stored: unknown = JSON.parse(storage?.getItem(STORAGE_KEY) ?? '[]');
    if (Array.isArray(stored)) {
      stored.slice(-MAX_OBSERVATIONS).forEach((key: unknown) => {
        if (typeof key === 'string' && key.length <= 240) seen.add(key);
      });
      pruneSeen();
    }
  } catch { /* Storage cannot interrupt a purchase. */ } };

  return (phase: TicketFunnelPhase, observation: TicketFunnelObservation): void => {
    if (!analytics.ready || !positiveInteger(observation.eventId)) return;
    const key = JSON.stringify([phase, observation.eventId, observation.privateScope ?? '',
      observation.tierId ?? null, observation.quantity ?? null, observation.provider ?? null]);
    // Components share session storage: merge before checking/writing so mounting
    // multiple event cards cannot discard observations from another component.
    readSeen();
    // A revisit can carry a new campaign even when this observation is already
    // counted. Refresh attribution before deduplication so the next phase uses it.
    let source: GrowthAttribution | null;
    try { source = attribution(); } catch { return; }
    if (seen.has(key)) return;
    const properties: Record<string, unknown> = {
      platform: 'web', event_id: observation.eventId, evidence: 'browser_observation',
    };
    if (positiveInteger(observation.tierId)) properties['tier_id'] = observation.tierId;
    if (positiveInteger(observation.quantity) && observation.quantity <= 100) properties['quantity'] = observation.quantity;
    if (['paypal', 'datafast', 'placetopay', 'payphone'].includes(observation.provider ?? '')) {
      properties['provider'] = observation.provider;
    }
    if (typeof observation.hasPromotion === 'boolean') properties['has_promotion'] = observation.hasPromotion;
    try {
      for (const name of ['source', 'medium', 'campaign'] as const) {
        const code = campaignCode(source?.[name]);
        if (code) properties[`attribution_${name}`] = code;
      }
      // Intentionally exclude order/ticket IDs, credentials, raw promo/referral
      // strings, person data, URL paths and all browser-derived monetary totals.
      analytics.capture(`ticketing_${phase}`, properties);
      seen.add(key);
      pruneSeen();
      storage?.setItem(STORAGE_KEY, JSON.stringify([...seen]));
    } catch { /* Neither SDK nor storage failures affect checkout. */ }
  };
}
