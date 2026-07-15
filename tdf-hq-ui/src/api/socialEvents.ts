import { del, get, post, postForm, put } from './client';

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

export interface SocialEventMomentDTO {
  emId?: string | null;
  emEventId?: string | null;
  emAuthorPartyId?: string | null;
  emAuthorName: string;
  emCaption?: string | null;
  emMediaUrl: string;
  emMediaType: 'image' | 'video' | string;
  emMediaWidth?: number | null;
  emMediaHeight?: number | null;
  emMediaDurationMs?: number | null;
  emCreatedAt?: string | null;
}

export interface SocialEventMomentCreateDTO {
  emCreateAuthorName?: string;
  emCreateCaption?: string;
  emCreateMediaUrl: string;
  emCreateMediaType: 'image' | 'video';
  emCreateMediaWidth?: number;
  emCreateMediaHeight?: number;
  emCreateMediaDurationMs?: number;
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

// Promo Codes
export interface PromoCodeDTO {
  promoCodeId?: string | null;
  promoCodeEventId?: string | null;
  promoCodeCode: string;
  promoCodeDescription?: string | null;
  promoCodeDiscountType: 'percentage' | 'fixed';
  promoCodeDiscountValue: number;
  promoCodeCurrency: string;
  promoCodeMaxRedemptions?: number | null;
  promoCodeCurrentRedemptions: number;
  promoCodeValidFrom?: string | null;
  promoCodeValidUntil?: string | null;
  promoCodeTierIds?: string[] | null;
  promoCodeMinPurchaseAmountCents?: number | null;
  promoCodeIsActive: boolean;
  promoCodeCreatedAt?: string | null;
  promoCodeUpdatedAt?: string | null;
}

export interface TicketPurchaseWithPromoDTO {
  ticketPurchaseTierId: string;
  ticketPurchaseQuantity: number;
  ticketPurchaseBuyerPartyId?: string | null;
  ticketPurchaseBuyerName?: string | null;
  ticketPurchaseBuyerEmail?: string | null;
  ticketPurchasePromoCode?: string | null;
  ticketPurchaseMobileSdkStripeVersion?: string | null;
}

export interface StripePaymentIntentDTO {
  spiClientSecret: string;
  spiPaymentIntentId?: string | null;
  spiOrderId: string;
  spiAmountCents: number;
  spiCurrency: string;
}

// Refunds
export interface RefundRequestDTO {
  refundRequestReason?: string | null;
}

export interface RefundDTO {
  refundId?: string | null;
  refundOrderId?: string | null;
  refundRequestedByPartyId?: string | null;
  refundReason?: string | null;
  refundAmountCents: number;
  refundStatus: string;
  refundApprovedByPartyId?: string | null;
  refundApprovedAt?: string | null;
  refundRejectionReason?: string | null;
  refundStripeRefundId?: string | null;
  refundProcessedAt?: string | null;
  refundCreatedAt?: string | null;
  refundUpdatedAt?: string | null;
}

export interface RejectionReasonDTO {
  rrReason: string;
}

// Transfers
export interface TicketTransferCreateDTO {
  ttcToEmail: string;
  ttcToName?: string | null;
  ttcMessage?: string | null;
}

export interface TicketTransferDTO {
  ticketTransferId?: string | null;
  ticketTransferTicketId?: string | null;
  ticketTransferFromPartyId?: string | null;
  ticketTransferToEmail: string;
  ticketTransferToName?: string | null;
  ticketTransferMessage?: string | null;
  ticketTransferCode: string;
  ticketTransferStatus: string;
  ticketTransferAcceptedAt?: string | null;
  ticketTransferExpiresAt?: string | null;
  ticketTransferCreatedAt?: string | null;
  ticketTransferUpdatedAt?: string | null;
}

// Waitlist
export interface WaitlistJoinDTO {
  wjEmail: string;
  wjName?: string | null;
  wjTierId?: string | null;
  wjQuantity: number;
}

export interface WaitlistEntryDTO {
  waitlistId?: string | null;
  waitlistEventId?: string | null;
  waitlistEmail: string;
  waitlistName?: string | null;
  waitlistTierId?: string | null;
  waitlistQuantity: number;
  waitlistStatus: string;
  waitlistPriority: number;
  waitlistNotifiedAt?: string | null;
  waitlistExpiresAt?: string | null;
  waitlistCreatedAt?: string | null;
}

// QR Codes
export interface TicketWithQRDTO {
  twqTicket: SocialTicketDTO;
  twqQRData: string;
  twqQRImageUrl?: string | null;
}

type RequestWithoutBody = (path: string) => Promise<unknown>;
type RequestWithBody = (path: string, body: unknown) => Promise<unknown>;
type FormRequest = (path: string, form: FormData) => Promise<unknown>;

const getUnknown: RequestWithoutBody = get;
const delUnknown: RequestWithoutBody = del;
const postUnknown: RequestWithBody = post;
const putUnknown: RequestWithBody = put;
const postFormUnknown: FormRequest = postForm;

const normalizeComparableId = (value: string | null | undefined): string => {
  const trimmed = value?.trim() ?? '';
  if (!trimmed) return '';

  if (/^-?\d+$/.test(trimmed)) {
    const parsed = Number.parseInt(trimmed, 10);
    if (Number.isSafeInteger(parsed)) return String(parsed);
  }

  return trimmed;
};

type QueryParams = Record<string, string | number | null | undefined>;

interface SocialInvitationCreatePayload {
  invitationToPartyId: string;
  invitationMessage?: string | null;
}

const buildQuery = (params: QueryParams) => {
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
  listEvents: async (opts?: { city?: string; startAfter?: string; eventType?: string; eventStatus?: string; artistId?: string; venueId?: string }) => {
    const qs = buildQuery({
      city: opts?.city,
      start_after: opts?.startAfter,
      event_type: opts?.eventType,
      event_status: opts?.eventStatus,
      artistId: opts?.artistId,
      venueId: opts?.venueId,
    });
    return await getUnknown(`/social-events/events${qs}`) as SocialEventDTO[];
  },
  createEvent: async (payload: SocialEventDTO) =>
    await postUnknown('/social-events/events', payload) as SocialEventDTO,
  updateEvent: async (eventId: string, payload: SocialEventDTO) =>
    await putUnknown(`/social-events/events/${encodeURIComponent(eventId)}`, payload) as SocialEventDTO,
  uploadEventImage: async (eventId: string, file: File, name?: string) => {
    const form = new FormData();
    form.append('file', file);
    const trimmed = name?.trim() ?? '';
    if (trimmed) form.append('name', trimmed);
    return await postFormUnknown(`/social-events/events/${encodeURIComponent(eventId)}/image`, form) as SocialEventImageUploadDTO;
  },
  listMoments: async (eventId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/moments`) as SocialEventMomentDTO[],
  createMoment: async (eventId: string, payload: SocialEventMomentCreateDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/moments`, payload) as SocialEventMomentDTO,
  uploadMomentImage: async (eventId: string, file: File, name?: string) => {
    const form = new FormData();
    form.append('file', file);
    const trimmed = name?.trim() ?? '';
    if (trimmed) form.append('name', trimmed);
    return await postFormUnknown(`/social-events/events/${encodeURIComponent(eventId)}/moments/image`, form) as SocialEventImageUploadDTO;
  },
  getEvent: async (eventId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}`) as SocialEventDTO,
  listVenues: async (opts?: { city?: string }) => {
    const qs = buildQuery({ city: opts?.city });
    return await getUnknown(`/social-events/venues${qs}`) as SocialVenueDTO[];
  },
  listInvitations: async (eventId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/invitations`) as SocialInvitationDTO[],
  sendInvitation: async (eventId: string, payload: SocialInvitationCreatePayload) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/invitations`, {
      invitationEventId: eventId,
      invitationToPartyId: payload.invitationToPartyId,
      invitationMessage: payload.invitationMessage ?? null,
      invitationStatus: 'Pending',
    }) as SocialInvitationDTO,
  respondInvitation: async (eventId: string, invitationId: string, status: string, message?: string | null) => {
    const normalizedEventId = eventId.trim();
    const normalizedInvitationId = invitationId.trim();
    if (!normalizedEventId) {
      throw new Error('eventId is required to respond to an invitation.');
    }
    if (!normalizedInvitationId) {
      throw new Error('invitationId is required to respond to an invitation.');
    }

    const invitations = await getUnknown(
      `/social-events/events/${encodeURIComponent(normalizedEventId)}/invitations`,
    ) as SocialInvitationDTO[];
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

    return await putUnknown(
      `/social-events/events/${encodeURIComponent(normalizedEventId)}/invitations/${encodeURIComponent(invitationPathId)}`,
      {
        invitationToPartyId,
        invitationStatus: status,
        invitationMessage: message ?? null,
      },
    ) as SocialInvitationDTO;
  },
  listTicketTiers: async (eventId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/ticket-tiers`) as SocialTicketTierDTO[],
  createTicketTier: async (eventId: string, payload: SocialTicketTierDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/ticket-tiers`, payload) as SocialTicketTierDTO,
  updateTicketTier: async (eventId: string, tierId: string, payload: SocialTicketTierDTO) =>
    await putUnknown(`/social-events/events/${encodeURIComponent(eventId)}/ticket-tiers/${encodeURIComponent(tierId)}`, payload) as SocialTicketTierDTO,
  listTicketOrders: async (eventId: string, opts?: { buyerPartyId?: string; status?: string }) => {
    const qs = buildQuery({ buyerPartyId: opts?.buyerPartyId, status: opts?.status });
    return await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/ticket-orders${qs}`) as SocialTicketOrderDTO[];
  },
  buyTickets: async (eventId: string, payload: SocialTicketPurchaseRequestDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/ticket-orders`, payload) as SocialTicketOrderDTO,
  updateTicketOrderStatus: async (eventId: string, orderId: string, status: string) =>
    await putUnknown(`/social-events/events/${encodeURIComponent(eventId)}/ticket-orders/${encodeURIComponent(orderId)}/status`, {
      ticketOrderStatus: status,
    }) as SocialTicketOrderDTO,
  listTickets: async (eventId: string, opts?: { orderId?: string; status?: string }) => {
    const qs = buildQuery({ orderId: opts?.orderId, status: opts?.status });
    return await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/tickets${qs}`) as SocialTicketDTO[];
  },
  checkInTicket: async (eventId: string, payload: { ticketCheckInTicketId?: string | null; ticketCheckInTicketCode?: string | null }) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/tickets/check-in`, payload) as SocialTicketDTO,
  listBudgetLines: async (eventId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/budget-lines`) as SocialEventBudgetLineDTO[],
  createBudgetLine: async (eventId: string, payload: SocialEventBudgetLineDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/budget-lines`, payload) as SocialEventBudgetLineDTO,
  updateBudgetLine: async (eventId: string, lineId: string, payload: SocialEventBudgetLineDTO) =>
    await putUnknown(`/social-events/events/${encodeURIComponent(eventId)}/budget-lines/${encodeURIComponent(lineId)}`, payload) as SocialEventBudgetLineDTO,
  listFinanceEntries: async (eventId: string, opts?: { direction?: string; source?: string; status?: string }) => {
    const qs = buildQuery({ direction: opts?.direction, source: opts?.source, status: opts?.status });
    return await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/finance-entries${qs}`) as SocialEventFinanceEntryDTO[];
  },
  createFinanceEntry: async (eventId: string, payload: SocialEventFinanceEntryDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/finance-entries`, payload) as SocialEventFinanceEntryDTO,
  updateFinanceEntry: async (eventId: string, entryId: string, payload: SocialEventFinanceEntryDTO) =>
    await putUnknown(`/social-events/events/${encodeURIComponent(eventId)}/finance-entries/${encodeURIComponent(entryId)}`, payload) as SocialEventFinanceEntryDTO,
  getFinanceSummary: async (eventId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/finance-summary`) as SocialEventFinanceSummaryDTO,
  rsvp: async (eventId: string, partyId: string, status: SocialRsvpDTO['rsvpStatus']) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/rsvps`, {
      rsvpEventId: eventId,
      rsvpPartyId: partyId,
      rsvpStatus: status,
    }) as SocialRsvpDTO,
  // Promo Codes
  listPromoCodes: async (eventId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/promo-codes`) as PromoCodeDTO[],
  createPromoCode: async (eventId: string, data: PromoCodeDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/promo-codes`, data) as PromoCodeDTO,
  updatePromoCode: async (eventId: string, codeId: string, data: PromoCodeDTO) =>
    await putUnknown(`/social-events/events/${encodeURIComponent(eventId)}/promo-codes/${encodeURIComponent(codeId)}`, data) as PromoCodeDTO,
  validatePromoCode: async (eventId: string, codeId: string, code?: string, tierId?: string) => {
    const query = buildQuery({ code, tierId });
    return await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/promo-codes/${encodeURIComponent(codeId)}/validate${query}`) as PromoCodeDTO;
  },
  // Stripe Payment
  createPaymentIntent: async (data: TicketPurchaseWithPromoDTO) =>
    await postUnknown('/social-events/stripe/create-payment-intent', data) as StripePaymentIntentDTO,
  // Refunds
  requestRefund: async (eventId: string, orderId: string, data: RefundRequestDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/ticket-orders/${encodeURIComponent(orderId)}/refund`, data) as RefundDTO,
  listRefunds: async (eventId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/refunds`) as RefundDTO[],
  approveRefund: async (eventId: string, refundId: string) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/refunds/${encodeURIComponent(refundId)}/approve`, {}) as RefundDTO,
  rejectRefund: async (eventId: string, refundId: string, data: RejectionReasonDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/refunds/${encodeURIComponent(refundId)}/reject`, data) as RefundDTO,
  // Transfers
  createTransfer: async (eventId: string, ticketId: string, data: TicketTransferCreateDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/tickets/${encodeURIComponent(ticketId)}/transfer`, data) as TicketTransferDTO,
  listTransfers: async (eventId: string, ticketId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/tickets/${encodeURIComponent(ticketId)}/transfers`) as TicketTransferDTO[],
  acceptTransfer: async (transferCode: string) =>
    await postUnknown(`/social-events/ticket-transfers/${encodeURIComponent(transferCode)}/accept`, {}) as SocialTicketDTO,
  cancelTransfer: async (transferCode: string) =>
    await postUnknown(`/social-events/ticket-transfers/${encodeURIComponent(transferCode)}/cancel`, {}) as TicketTransferDTO,
  // Waitlist
  joinWaitlist: async (eventId: string, data: WaitlistJoinDTO) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/waitlist`, data) as WaitlistEntryDTO,
  listWaitlist: async (eventId: string, tierId?: string) => {
    const waitlistQuery = buildQuery({ tierId });
    return await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/waitlist${waitlistQuery}`) as WaitlistEntryDTO[];
  },
  notifyWaitlist: async (eventId: string, entryId: string) =>
    await postUnknown(`/social-events/events/${encodeURIComponent(eventId)}/waitlist/${encodeURIComponent(entryId)}/notify`, {}) as WaitlistEntryDTO,
  removeFromWaitlist: async (eventId: string, entryId: string) => {
    await delUnknown(`/social-events/events/${encodeURIComponent(eventId)}/waitlist/${encodeURIComponent(entryId)}`);
  },
  // QR Codes
  getTicketQR: async (eventId: string, ticketId: string) =>
    await getUnknown(`/social-events/events/${encodeURIComponent(eventId)}/tickets/${encodeURIComponent(ticketId)}/qr`) as TicketWithQRDTO,
};
