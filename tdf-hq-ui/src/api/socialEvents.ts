import { get, post, postForm, put } from './client';

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
  eventType?: string | null;
  eventStatus?: string | null;
  eventCurrency?: string | null;
  eventBudgetCents?: number | null;
  eventTicketUrl?: string | null;
  eventImageUrl?: string | null;
  eventIsPublic?: boolean | null;
  eventArtists: SocialArtistDTO[];
}

export interface SocialEventImageUploadDTO {
  eiuEventId: string;
  eiuFileName: string;
  eiuPath: string;
  eiuPublicUrl: string;
  eiuImageUrl: string;
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

export interface SocialEventBudgetLineDTO {
  eblId?: string | null;
  eblEventId?: string | null;
  eblCode: string;
  eblName: string;
  eblType: string;
  eblCategory: string;
  eblPlannedCents: number;
  eblActualCents?: number | null;
  eblNotes?: string | null;
  eblCreatedAt?: string | null;
  eblUpdatedAt?: string | null;
}

export interface SocialEventFinanceEntryDTO {
  efeId?: string | null;
  efeEventId?: string | null;
  efeBudgetLineId?: string | null;
  efeDirection: string;
  efeSource: string;
  efeCategory: string;
  efeConcept: string;
  efeAmountCents: number;
  efeCurrency: string;
  efeStatus: string;
  efeExternalRef?: string | null;
  efeNotes?: string | null;
  efeOccurredAt: string;
  efeRecordedByPartyId?: string | null;
  efeCreatedAt?: string | null;
  efeUpdatedAt?: string | null;
}

export interface SocialEventFinanceSummaryDTO {
  efsEventId: string;
  efsCurrency: string;
  efsBudgetCents?: number | null;
  efsPlannedIncomeCents: number;
  efsPlannedExpenseCents: number;
  efsActualIncomeCents: number;
  efsActualExpenseCents: number;
  efsNetCents: number;
  efsTicketPaidRevenueCents: number;
  efsTicketRefundedRevenueCents: number;
  efsTicketPendingRevenueCents: number;
  efsAccountsPayableCents?: number;
  efsAccountsReceivableCents?: number;
  efsContractCommittedCents?: number;
  efsContractPaidCents?: number;
  efsProcurementCommittedCents?: number;
  efsProcurementPaidCents?: number;
  efsAssetInvestmentCents?: number;
  efsLiabilityBalanceCents?: number;
  efsBudgetVarianceCents?: number | null;
  efsBudgetUtilizationPct?: number | null;
  efsGeneratedAt: string;
}

const normalizeComparableId = (value: string | null | undefined): string => {
  const trimmed = value?.trim() ?? '';
  if (!trimmed) return '';

  if (/^-?\d+$/.test(trimmed)) {
    const parsed = Number.parseInt(trimmed, 10);
    if (Number.isSafeInteger(parsed)) return String(parsed);
  }

  return trimmed;
};

const buildQuery = (params: Record<string, string | number | null | undefined>) => {
  const entries = Object.entries(params)
    .map((entry): [string, string | number] | null => {
      const [key, value] = entry;
      if (value == null) return null;
      if (typeof value === 'string') {
        const trimmed = value.trim();
        if (!trimmed) return null;
        return [key, trimmed];
      }
      return [key, value];
    })
    .filter((entry): entry is [string, string | number] => entry !== null);
  if (!entries.length) return '';
  const qs = entries.map(([k, v]) => `${encodeURIComponent(k)}=${encodeURIComponent(String(v))}`).join('&');
  return `?${qs}`;
};

export const SocialEventsAPI = {
  listEvents: (opts?: { city?: string; startAfter?: string; eventType?: string; eventStatus?: string; artistId?: string; venueId?: string }) => {
    const qs = buildQuery({
      city: opts?.city,
      start_after: opts?.startAfter,
      event_type: opts?.eventType,
      event_status: opts?.eventStatus,
      artistId: opts?.artistId,
      venueId: opts?.venueId,
    });
    return get<SocialEventDTO[]>(`/social-events/events${qs}`);
  },
  createEvent: (payload: SocialEventDTO) =>
    post<SocialEventDTO>('/social-events/events', payload),
  updateEvent: (eventId: string, payload: SocialEventDTO) =>
    put<SocialEventDTO>(`/social-events/events/${encodeURIComponent(eventId)}`, payload),
  uploadEventImage: (eventId: string, file: File, name?: string) => {
    const form = new FormData();
    form.append('file', file);
    const trimmed = name?.trim() ?? '';
    if (trimmed) form.append('name', trimmed);
    return postForm<SocialEventImageUploadDTO>(`/social-events/events/${encodeURIComponent(eventId)}/image`, form);
  },
  getEvent: (eventId: string) =>
    get<SocialEventDTO>(`/social-events/events/${encodeURIComponent(eventId)}`),
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
  respondInvitation: async (eventId: string, invitationId: string, status: string, message?: string | null) => {
    const normalizedEventId = eventId.trim();
    const normalizedInvitationId = invitationId.trim();
    if (!normalizedEventId) {
      throw new Error('eventId is required to respond to an invitation.');
    }
    if (!normalizedInvitationId) {
      throw new Error('invitationId is required to respond to an invitation.');
    }

    const invitations = await get<SocialInvitationDTO[]>(
      `/social-events/events/${encodeURIComponent(normalizedEventId)}/invitations`,
    );
    const invitationKey = normalizeComparableId(normalizedInvitationId);
    const invitation = invitations.find(
      (candidate) => normalizeComparableId(candidate.invitationId) === invitationKey,
    );
    if (!invitation) {
      throw new Error(`Invitation ${normalizedInvitationId} not found for event ${normalizedEventId}.`);
    }
    const invitationToPartyId = invitation.invitationToPartyId?.trim() ?? '';
    if (!invitationToPartyId) {
      throw new Error(`Invitation ${normalizedInvitationId} is missing invitationToPartyId.`);
    }
    const invitationPathId = normalizeComparableId(invitation.invitationId) || invitationKey;

    return put<SocialInvitationDTO>(
      `/social-events/events/${encodeURIComponent(normalizedEventId)}/invitations/${encodeURIComponent(invitationPathId)}`,
      {
        invitationToPartyId,
        invitationStatus: status,
        invitationMessage: message ?? null,
      },
    );
  },
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
  listBudgetLines: (eventId: string) =>
    get<SocialEventBudgetLineDTO[]>(`/social-events/events/${encodeURIComponent(eventId)}/budget-lines`),
  createBudgetLine: (eventId: string, payload: SocialEventBudgetLineDTO) =>
    post<SocialEventBudgetLineDTO>(`/social-events/events/${encodeURIComponent(eventId)}/budget-lines`, payload),
  updateBudgetLine: (eventId: string, lineId: string, payload: SocialEventBudgetLineDTO) =>
    put<SocialEventBudgetLineDTO>(`/social-events/events/${encodeURIComponent(eventId)}/budget-lines/${encodeURIComponent(lineId)}`, payload),
  listFinanceEntries: (eventId: string, opts?: { direction?: string; source?: string; status?: string }) => {
    const qs = buildQuery({ direction: opts?.direction, source: opts?.source, status: opts?.status });
    return get<SocialEventFinanceEntryDTO[]>(`/social-events/events/${encodeURIComponent(eventId)}/finance-entries${qs}`);
  },
  createFinanceEntry: (eventId: string, payload: SocialEventFinanceEntryDTO) =>
    post<SocialEventFinanceEntryDTO>(`/social-events/events/${encodeURIComponent(eventId)}/finance-entries`, payload),
  updateFinanceEntry: (eventId: string, entryId: string, payload: SocialEventFinanceEntryDTO) =>
    put<SocialEventFinanceEntryDTO>(`/social-events/events/${encodeURIComponent(eventId)}/finance-entries/${encodeURIComponent(entryId)}`, payload),
  getFinanceSummary: (eventId: string) =>
    get<SocialEventFinanceSummaryDTO>(`/social-events/events/${encodeURIComponent(eventId)}/finance-summary`),
  rsvp: (eventId: string, partyId: string, status: SocialRsvpDTO['rsvpStatus']) =>
    post<SocialRsvpDTO>(`/social-events/events/${encodeURIComponent(eventId)}/rsvps`, {
      rsvpEventId: eventId,
      rsvpPartyId: partyId,
      rsvpStatus: status,
    }),
};
