import { get, post } from './client';

export interface SocialArtistDTO {
  artistId?: string | null;
  artistName: string;
  artistGenres?: string[];
  artistBio?: string | null;
  artistAvatarUrl?: string | null;
}

export interface SocialVenueDTO {
  venueId?: string | null;
  venueName: string;
  venueAddress?: string | null;
  venueCity?: string | null;
  venueCountry?: string | null;
}

export interface SocialEventDTO {
  eventId?: string | null;
  eventTitle: string;
  eventDescription?: string | null;
  eventStart: string;
  eventEnd: string;
  eventVenueId?: string | null;
  eventPriceCents?: number | null;
  eventCapacity?: number | null;
  eventArtists: SocialArtistDTO[];
}

export interface SocialInvitationDTO {
  invitationId?: string | null;
  invitationEventId?: string | null;
  invitationFromPartyId?: string | null;
  invitationToPartyId: string;
  invitationStatus?: string | null;
  invitationMessage?: string | null;
  invitationCreatedAt?: string | null;
}

export type SocialRsvpStatus = 'Accepted' | 'Declined' | 'Maybe';

export interface SocialRsvpDTO {
  rsvpId?: string | null;
  rsvpEventId: string;
  rsvpPartyId: string;
  rsvpStatus: SocialRsvpStatus;
  rsvpCreatedAt?: string | null;
}

const buildQuery = (params: Record<string, string | null | undefined>) => {
  const entries = Object.entries(params).filter(([, v]) => v != null && v !== '');
  if (!entries.length) return '';
  const qs = entries.map(([k, v]) => `${encodeURIComponent(k)}=${encodeURIComponent(v ?? '')}`).join('&');
  return `?${qs}`;
};

export const SocialEventsAPI = {
  listEvents: (opts?: { city?: string; startAfter?: string }) => {
    const qs = buildQuery({
      city: opts?.city,
      start_after: opts?.startAfter,
    });
    return get<SocialEventDTO[]>(`/social-events/events${qs}`);
  },
  listVenues: (opts?: { city?: string }) => {
    const qs = buildQuery({ city: opts?.city });
    return get<SocialVenueDTO[]>(`/social-events/venues${qs}`);
  },
  listInvitations: (eventId: string) =>
    get<SocialInvitationDTO[]>(`/social-events/events/${encodeURIComponent(eventId)}/invitations`),
  sendInvitation: (eventId: string, payload: Pick<SocialInvitationDTO, 'invitationToPartyId' | 'invitationMessage'>) =>
    post<SocialInvitationDTO>(`/social-events/events/${encodeURIComponent(eventId)}/invitations`, {
      invitationEventId: eventId,
      invitationToPartyId: payload.invitationToPartyId,
      invitationMessage: payload.invitationMessage ?? null,
      invitationStatus: 'Pending',
    }),
  respondInvitation: (eventId: string, invitationId: string, status: string, message?: string | null) =>
    post<SocialInvitationDTO>(`/social-events/events/${encodeURIComponent(eventId)}/invitations/${encodeURIComponent(invitationId)}`, {
      invitationStatus: status,
      invitationMessage: message ?? null,
    }),
  rsvp: (eventId: string, partyId: string, status: SocialRsvpDTO['rsvpStatus']) =>
    post<SocialRsvpDTO>(`/social-events/events/${encodeURIComponent(eventId)}/rsvps`, {
      rsvpEventId: eventId,
      rsvpPartyId: partyId,
      rsvpStatus: status,
    }),
};
