// Shared by every RSVP surface so one write is visible to all of them.
export const eventRsvpQueryKeys = {
  mine: (eventId: string, partyId?: string | number | null) => ['event-rsvp', 'mine', String(partyId ?? 'anonymous'), eventId] as const,
  summary: (eventId: string) => ['event-rsvp', 'summary', eventId] as const,
  feed: (partyId: string) => ['event-rsvp', 'feed', partyId] as const,
};
