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
