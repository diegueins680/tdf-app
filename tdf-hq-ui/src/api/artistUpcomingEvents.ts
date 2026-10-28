import { get } from './client';
import type { PublicUpcomingEventDTO, SocialArtistDTO, SocialEventDTO, SocialVenueDTO } from './socialEvents';

const PAGE_SIZE = 500;

async function readPages<T>(path: string, signal?: AbortSignal): Promise<T[]> {
  const rows: T[] = [];
  for (let offset = 0; ; offset += PAGE_SIZE) {
    const page = await get<T[]>(`${path}${path.includes('?') ? '&' : '?'}limit=${PAGE_SIZE}&offset=${offset}`, { signal });
    if (!Array.isArray(page)) throw new Error('Expected an event catalog page');
    rows.push(...page);
    if (page.length < PAGE_SIZE) return rows;
  }
}

// Profile URLs identify a Party; event filters identify a separate social Artist.
// Resolve that relationship explicitly, without matching names or assuming equal IDs.
export async function listArtistUpcomingEvents(
  artistPartyId: number,
  startAfter: string,
  signal?: AbortSignal,
): Promise<PublicUpcomingEventDTO[]> {
  const partyId = String(artistPartyId);
  const artists = await readPages<SocialArtistDTO>('/social-events/artists', signal);
  const matches = artists.filter((artist) => artist.artistId && artist.artistPartyId === partyId);
  if (!matches.length) return [];
  const pages = await Promise.all(matches.map((artist) => readPages<SocialEventDTO>(
    `/social-events/events?artistId=${encodeURIComponent(artist.artistId!)}&start_after=${encodeURIComponent(startAfter)}`,
    signal,
  )));
  // Admin reads may contain drafts: only project events eligible for public listing.
  const events = Array.from(new Map(pages.flat()
    .filter((event) => event.eventId && event.eventIsPublic !== false && event.eventPublicListable === true
      && Date.parse(event.eventStart) >= Date.parse(startAfter)
      && event.eventArtists.some((artist) => artist.artistPartyId === partyId))
    .map((event) => [event.eventId!, event])).values())
    .sort((a, b) => Date.parse(a.eventStart) - Date.parse(b.eventStart) || a.eventId!.localeCompare(b.eventId!));
  const venueIds = [...new Set(events.flatMap((event) => event.eventVenueId ? [event.eventVenueId] : []))];
  const venues = new Map(await Promise.all(venueIds.map(async (id) => [
    id, await get<SocialVenueDTO>(`/social-events/venues/${encodeURIComponent(id)}`, { signal }),
  ] as const)));
  return events.map((event) => ({
    publicUpcomingEventId: event.eventId!,
    publicUpcomingEventTitle: event.eventTitle,
    publicUpcomingEventDescription: event.eventDescription,
    publicUpcomingEventStart: event.eventStart,
    publicUpcomingEventEnd: event.eventEnd,
    publicUpcomingEventTimezone: event.eventTimezone,
    publicUpcomingEventVenueName: event.eventVenueId ? venues.get(event.eventVenueId)?.venueName : null,
    publicUpcomingEventCity: event.eventVenueId ? venues.get(event.eventVenueId)?.venueCity : null,
    publicUpcomingEventPriceCents: event.eventPriceCents,
    publicUpcomingEventCurrency: event.eventCurrency,
    publicUpcomingEventTicketUrl: event.eventTicketUrl,
    publicUpcomingEventImageUrl: event.eventImageUrl,
    publicUpcomingEventWorkflowStateCode: event.eventWorkflowStateCode!,
  }));
}
