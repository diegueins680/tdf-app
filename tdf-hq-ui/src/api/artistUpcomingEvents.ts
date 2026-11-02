import { get } from './client';
import type { PublicUpcomingEventDTO } from './socialEvents';

// The public upcoming projection caps a single page at 200 rows; an artist with
// more than 200 future public dates is not a case the carousel needs to show.
const PUBLIC_UPCOMING_LIMIT = 200;

// Profile URLs identify a Party. The server resolves that Party to its social
// artist profiles and applies every anonymous-listing predicate (public-listable
// state, public metadata, no suppressed external reference) before returning,
// so a signed-in viewer never sees more here than the public listing shows.
export async function listArtistUpcomingEvents(
  artistPartyId: number,
  startAfter: string,
  signal?: AbortSignal,
): Promise<PublicUpcomingEventDTO[]> {
  const query = new URLSearchParams({
    artistPartyId: String(artistPartyId),
    startAfter,
    limit: String(PUBLIC_UPCOMING_LIMIT),
  });
  const rows = await get<PublicUpcomingEventDTO[]>(`/social-events/upcoming?${query.toString()}`, { signal });
  if (!Array.isArray(rows)) throw new Error('Expected a public upcoming events page');
  const startAfterMs = Date.parse(startAfter);
  // The server already orders and filters; re-check the window defensively and
  // drop anything a lenient server might still return without an id or start.
  return Array.from(new Map(rows
    .filter((event) => event.publicUpcomingEventId && Number.isFinite(Date.parse(event.publicUpcomingEventStart))
      && Date.parse(event.publicUpcomingEventStart) >= startAfterMs)
    .map((event) => [event.publicUpcomingEventId, event] as const)).values())
    .sort((a, b) => Date.parse(a.publicUpcomingEventStart) - Date.parse(b.publicUpcomingEventStart)
      || a.publicUpcomingEventId.localeCompare(b.publicUpcomingEventId));
}
