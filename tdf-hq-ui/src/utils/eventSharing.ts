import type { SocialRsvpStatus } from '../api/socialEvents';

const PUBLIC_EVENT_ID = /^[1-9]\d{0,18}$/;
const SAFE_ATTRIBUTION_KEYS = new Set(['utm_source', 'utm_medium', 'utm_campaign']);
const SAFE_ATTRIBUTION_VALUES: Record<string, Set<string>> = {
  utm_source: new Set(['tdf_web', 'tdf_mobile']),
  utm_medium: new Set(['share', 'copy', 'whatsapp']),
  utm_campaign: new Set(['event_rsvp']),
};

export interface EventShareInput {
  eventId: string;
  title: string;
  start?: string | null;
  timezone?: string | null;
  venue?: string | null;
  status?: Extract<SocialRsvpStatus, 'accepted' | 'maybe'> | null;
  locale?: string | null;
}

export function canonicalEventPath(eventId: string): string {
  const normalized = eventId.trim();
  if (!PUBLIC_EVENT_ID.test(normalized)) throw new Error('Invalid public event identifier.');
  return `/eventos/${normalized}`;
}

export function canonicalEventUrl(
  origin: string,
  eventId: string,
  attribution?: Record<string, string | null | undefined>,
): string {
  const url = new URL(canonicalEventPath(eventId), origin);
  Object.entries(attribution ?? {}).forEach(([key, rawValue]) => {
    const value = rawValue?.trim();
    if (!SAFE_ATTRIBUTION_KEYS.has(key) || !value || !SAFE_ATTRIBUTION_VALUES[key]?.has(value)) return;
    url.searchParams.set(key, value);
  });
  return url.toString();
}

export function buildEventShareMessage(input: EventShareInput): string {
  const english = input.locale?.toLowerCase().startsWith('en') ?? false;
  const date = input.start ? formatShareDate(input.start, input.locale, input.timezone) : null;
  const place = input.venue?.trim() ?? null;
  const details = [date, place].filter(Boolean).join(english ? ' at ' : ' en ');
  const suffix = details ? ` ${english ? 'on' : 'el'} ${details}` : '';

  if (input.status === 'accepted') {
    return english
      ? `I'm going to ${input.title}${suffix}. See you there?`
      : `Voy a ${input.title}${suffix}. ¿Nos vemos ahí?`;
  }
  if (input.status === 'maybe') {
    return english
      ? `I'm interested in ${input.title}${suffix}. Take a look.`
      : `Me interesa ${input.title}${suffix}. Mira los detalles.`;
  }
  return english
    ? `${input.title}${suffix}. View the event details.`
    : `${input.title}${suffix}. Mira los detalles del evento.`;
}

function formatShareDate(value: string, locale?: string | null, timezone?: string | null): string | null {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return null;
  const dateLocale = locale?.trim() ? locale : 'es';
  try {
    return new Intl.DateTimeFormat(dateLocale, {
      dateStyle: 'medium',
      timeStyle: 'short',
      ...(timezone ? { timeZone: timezone } : {}),
    }).format(date);
  } catch {
    return new Intl.DateTimeFormat('es', { dateStyle: 'medium', timeStyle: 'short' }).format(date);
  }
}

export function safePublicImageUrl(value: unknown, origin: string): string | undefined {
  if (typeof value !== 'string' || !value.trim()) return undefined;
  try {
    const imageUrl = new URL(value.trim(), origin);
    const pageOrigin = new URL(origin);
    const sameOrigin = imageUrl.origin === pageOrigin.origin;
    if (imageUrl.username || imageUrl.password) return undefined;
    if (imageUrl.protocol !== 'https:' && !sameOrigin) return undefined;
    if (!['http:', 'https:'].includes(imageUrl.protocol)) return undefined;
    return imageUrl.toString();
  } catch {
    return undefined;
  }
}
