import { get, post, put } from './client';

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
  eventOrganizerPartyId?: string | null;
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

export interface SocialTicketTierDTO {
  ticketTierId?: string | null;
  ticketTierEventId?: string | null;
  ticketTierCode: string;
  ticketTierName: string;
  ticketTierDescription?: string | null;
  ticketTierPriceCents: number;
  ticketTierCurrency: string;
  ticketTierQuantityTotal: number;
  ticketTierQuantitySold: number;
  ticketTierSalesStart?: string | null;
  ticketTierSalesEnd?: string | null;
  ticketTierActive: boolean;
  ticketTierPosition?: number | null;
}

export interface SocialTicketPurchaseRequestDTO {
  ticketPurchaseTierId: string;
  ticketPurchaseQuantity: number;
  ticketPurchaseBuyerPartyId?: string | null;
  ticketPurchaseBuyerName?: string | null;
  ticketPurchaseBuyerEmail?: string | null;
}

export interface SocialTicketDTO {
  ticketId?: string | null;
  ticketEventId?: string | null;
  ticketTierId?: string | null;
  ticketOrderId?: string | null;
  ticketCode: string;
  ticketStatus: string;
  ticketHolderName?: string | null;
  ticketHolderEmail?: string | null;
  ticketCheckedInAt?: string | null;
  ticketCreatedAt?: string | null;
  ticketUpdatedAt?: string | null;
}

export interface SocialTicketOrderDTO {
  ticketOrderId?: string | null;
  ticketOrderEventId?: string | null;
  ticketOrderTierId?: string | null;
  ticketOrderBuyerPartyId?: string | null;
  ticketOrderBuyerName?: string | null;
  ticketOrderBuyerEmail?: string | null;
  ticketOrderQuantity: number;
  ticketOrderAmountCents: number;
  ticketOrderCurrency: string;
  ticketOrderStatusValue: string;
  ticketOrderPurchasedAt?: string | null;
  ticketOrderCreatedAt?: string | null;
  ticketOrderUpdatedAt?: string | null;
  ticketOrderTickets: SocialTicketDTO[];
}

const buildQuery = (params: Record<string, string | number | null | undefined>) => {
  const entries = Object.entries(params).filter(([, v]) => v != null && v !== '');
  if (!entries.length) return '';
  const qs = entries.map(([k, v]) => `${encodeURIComponent(k)}=${encodeURIComponent(String(v ?? ''))}`).join('&');
  return `?${qs}`;
};

export const SocialEventsAPI = {
  listEvents: (opts?: { city?: string; startAfter?: string; artistId?: string; venueId?: string }) => {
    const qs = buildQuery({
      city: opts?.city,
      start_after: opts?.startAfter,
      artistId: opts?.artistId,
      venueId: opts?.venueId,
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
    put<SocialInvitationDTO>(`/social-events/events/${encodeURIComponent(eventId)}/invitations/${encodeURIComponent(invitationId)}`, {
      invitationStatus: status,
      invitationMessage: message ?? null,
    }),
  listTicketTiers: (eventId: string) =>
    get<SocialTicketTierDTO[]>(`/social-events/events/${encodeURIComponent(eventId)}/ticket-tiers`),
  createTicketTier: (eventId: string, payload: SocialTicketTierDTO) =>
    post<SocialTicketTierDTO>(`/social-events/events/${encodeURIComponent(eventId)}/ticket-tiers`, payload),
  updateTicketTier: (eventId: string, tierId: string, payload: SocialTicketTierDTO) =>
    put<SocialTicketTierDTO>(`/social-events/events/${encodeURIComponent(eventId)}/ticket-tiers/${encodeURIComponent(tierId)}`, payload),
  listTicketOrders: (eventId: string, opts?: { buyerPartyId?: string; status?: string }) => {
    const qs = buildQuery({ buyerPartyId: opts?.buyerPartyId, status: opts?.status });
    return get<SocialTicketOrderDTO[]>(`/social-events/events/${encodeURIComponent(eventId)}/ticket-orders${qs}`);
  },
  buyTickets: (eventId: string, payload: SocialTicketPurchaseRequestDTO) =>
    post<SocialTicketOrderDTO>(`/social-events/events/${encodeURIComponent(eventId)}/ticket-orders`, payload),
  updateTicketOrderStatus: (eventId: string, orderId: string, status: string) =>
    put<SocialTicketOrderDTO>(`/social-events/events/${encodeURIComponent(eventId)}/ticket-orders/${encodeURIComponent(orderId)}/status`, {
      ticketOrderStatus: status,
    }),
  listTickets: (eventId: string, opts?: { orderId?: string; status?: string }) => {
    const qs = buildQuery({ orderId: opts?.orderId, status: opts?.status });
    return get<SocialTicketDTO[]>(`/social-events/events/${encodeURIComponent(eventId)}/tickets${qs}`);
  },
  checkInTicket: (eventId: string, payload: { ticketCheckInTicketId?: string | null; ticketCheckInTicketCode?: string | null }) =>
    post<SocialTicketDTO>(`/social-events/events/${encodeURIComponent(eventId)}/tickets/check-in`, payload),
  rsvp: (eventId: string, partyId: string, status: SocialRsvpDTO['rsvpStatus']) =>
    post<SocialRsvpDTO>(`/social-events/events/${encodeURIComponent(eventId)}/rsvps`, {
      rsvpEventId: eventId,
      rsvpPartyId: partyId,
      rsvpStatus: status,
    }),
};
