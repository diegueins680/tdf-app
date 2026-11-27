export const EVENT_PAGE_SIZE = 20;

export function eventArtistIds(raw: string | null): string[] {
  return [...new Set((raw ?? '').split(',').map(id => id.trim())
    .filter(id => /^[1-9]\d{0,18}$/.test(id) && BigInt(id) <= 9223372036854775807n))].slice(0, 50);
}

export function eventPage(raw: string | null): number {
  const page = Number(raw ?? '1');
  return Number.isInteger(page) && page >= 1 && page <= 501 ? page : 1;
}

/** Explicit end dates keep ongoing events visible; unknown ends use start time. */
export function isPastEvent(event: { eventStart: string; eventEnd?: string | null }, now: number): boolean {
  return Date.parse(event.eventEnd ?? event.eventStart) < now;
}

/**
 * EVT-AFFINITY-ORDER-001. The server permutes each page by the listener's
 * affinity, so the extra look-ahead row of the date order cannot be told
 * apart after the permutation: a "for you" page asks for exactly one page and
 * offers a next page whenever it came back full.
 */
export function eventPageLimit(forYou: boolean): number {
  return forYou ? EVENT_PAGE_SIZE : EVENT_PAGE_SIZE + 1;
}

export function eventHasNextPage(forYou: boolean, received: number): boolean {
  return forYou ? received >= EVENT_PAGE_SIZE : received > EVENT_PAGE_SIZE;
}

/** The order applies only where the build enables it and a listener is signed in. */
export function eventForYouOrder(raw: string | null, enabled: boolean, signedIn: boolean): boolean {
  return enabled && signedIn && raw === 'for_you';
}
