/**
 * Events imported from outside (Eventbrite, Ticketmaster…) sell their tickets
 * there, not through the TDF checkout. The public DTOs carry no ticket URL
 * yet, so the external link is recovered from the published description.
 */
export interface ExternalTicketLink {
  url: string;
  vendor: string | null;
}

const URL_PATTERN = /https?:\/\/[^\s<>"')\]]+/gi;
const TRAILING_PUNCTUATION = /[.,;:!?]+$/;

const VENDORS: readonly (readonly [RegExp, string])[] = [
  [/(^|\.)eventbrite\.[a-z.]+$/i, 'Eventbrite'],
  [/(^|\.)ticketmaster\.[a-z.]+$/i, 'Ticketmaster'],
  [/(^|\.)livenation\.[a-z.]+$/i, 'Live Nation'],
  [/(^|\.)dice\.fm$/i, 'DICE'],
  [/(^|\.)feverup\.com$/i, 'Fever'],
  [/(^|\.)passline\.com$/i, 'Passline'],
  [/(^|\.)ticketshow\.[a-z.]+$/i, 'TicketShow'],
  [/(^|\.)buenplan\.com\.ec$/i, 'Buen Plan'],
  [/(^|\.)tuboleta\.com$/i, 'Tu Boleta'],
  [/(^|\.)tickeri\.com$/i, 'Tickeri'],
  [/(^|\.)axs\.com$/i, 'AXS'],
  [/(^|\.)seetickets\.[a-z.]+$/i, 'See Tickets'],
  [/(^|\.)ticketek\.[a-z.]+$/i, 'Ticketek'],
  [/(^|\.)entradas\.com$/i, 'Entradas.com'],
  [/(^|\.)boletia\.com$/i, 'Boletia'],
  [/(^|\.)joinnus\.com$/i, 'Joinnus'],
];

const TICKET_LABEL = /(?:entradas?|tickets?|boletos?|boleter[ií]a)[^\n]{0,40}?:\s*$/i;

function parse(raw: string): URL | null {
  try {
    const url = new URL(raw.replace(TRAILING_PUNCTUATION, ''));
    return url.protocol === 'https:' || url.protocol === 'http:' ? url : null;
  } catch {
    return null;
  }
}

function vendorFor(host: string): string | null {
  return VENDORS.find(([pattern]) => pattern.test(host))?.[1] ?? null;
}

/**
 * The first link in `text` that points at a known ticketing vendor, or, failing
 * that, the first link introduced by an "Entradas:" / "Tickets:" label.
 */
export function findExternalTicketLink(text: string | null | undefined): ExternalTicketLink | null {
  if (!text) return null;
  let labelled: ExternalTicketLink | null = null;
  for (const match of text.matchAll(URL_PATTERN)) {
    const url = parse(match[0]);
    if (!url) continue;
    const vendor = vendorFor(url.hostname);
    if (vendor) return { url: url.toString(), vendor };
    if (!labelled && TICKET_LABEL.test(text.slice(0, match.index))) labelled = { url: url.toString(), vendor: null };
  }
  return labelled;
}

const UNAVAILABLE_REASONS: Record<string, { es: string; en: string }> = {
  'Public ticket checkout is disabled in this environment': {
    es: 'La venta de entradas en TDF no está habilitada en este entorno.',
    en: 'TDF ticket checkout is not enabled in this environment.',
  },
  'This event has no approved active ticket price and fee policy': {
    es: 'Este evento todavía no vende entradas a través de TDF.',
    en: 'This event does not sell tickets through TDF yet.',
  },
  'No ticket tiers are currently on sale': {
    es: 'No hay tipos de entrada a la venta por ahora.',
    en: 'No ticket types are on sale right now.',
  },
  'Ticket inventory is currently exhausted': {
    es: 'Las entradas están agotadas.',
    en: 'Tickets are sold out.',
  },
  'Ticket sales open once electronic invoicing is configured': {
    es: 'La venta abrirá cuando la facturación electrónica esté configurada.',
    en: 'Sales open once electronic invoicing is configured.',
  },
};

/** The storefront's server-side reason, in the visitor's language. */
export function localizeTicketUnavailableReason(reason: string | null | undefined, english: boolean): string {
  const known = reason ? UNAVAILABLE_REASONS[reason.trim()] : undefined;
  if (known) return english ? known.en : known.es;
  return english ? 'Tickets are not available through TDF right now.' : 'Las entradas no están disponibles en TDF por ahora.';
}
