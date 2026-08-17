import { get, post, put } from './client';
import type { BookingDTO } from './types';
import { decodeLegacyServiceOfferingId } from './services';

const requirePositiveInteger = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${field} debe ser un entero positivo.`);
  }
  return value;
};

const normalizeOptionalPositiveInteger = (
  value: number | null | undefined,
  field: string,
): number | null | undefined => {
  if (value == null) return value;
  return requirePositiveInteger(value, field);
};

const setOptionalPositiveIntParam = (
  search: URLSearchParams,
  key: string,
  value?: number,
): void => {
  if (value == null) return;
  search.set(key, String(requirePositiveInteger(value, key)));
};

export interface BookingUpdatePayload {
  ubTitle?: string;
  ubServiceOfferingId?: string;
  ubStatus?: string;
  ubNotes?: string;
  ubStartsAt?: string;
  ubEndsAt?: string;
  ubResourceIds?: string[] | null;
  ubPartyId?: number | null;
  ubEngineerPartyId?: number | null;
  ubEngineerName?: string | null;
}

export interface PublicBookingQuoteDTO {
  policyVersion: string;
  currency: string;
  durationMinutes: number;
  subtotalMinor: number;
  taxMinor: number;
  totalMinor: number;
  depositMinor: number;
  balanceMinor: number;
  depositBps: number;
  termsVersion: string;
}

export interface PublicBookingAvailabilityDTO {
  available: boolean;
  reason?: string | null;
  serviceOfferingId: string;
  startsAt: string;
  endsAt: string;
  resourceIds: string[];
  resourceNames: string[];
  quote?: PublicBookingQuoteDTO | null;
}

export interface PublicBookingCheckoutDTO {
  booking: BookingDTO;
  checkoutId: string;
  lookupToken?: string | null;
  paymentStatus: string;
  fulfillmentStatus: string;
  holdExpiresAt: string;
  quote: PublicBookingQuoteDTO;
}

export interface PublicBookingCheckoutPayload {
  pbcFullName: string;
  pbcEmail: string;
  pbcPhone?: string | null;
  pbcServiceOfferingId: string;
  pbcStartsAt: string;
  pbcDurationMinutes: number;
  pbcNotes?: string | null;
  pbcEngineerPartyId?: number | null;
  pbcEngineerName?: string | null;
  pbcResourceIds?: string[] | null;
  pbcTermsAccepted: boolean;
}

export const Bookings = {
  publicAvailability: (params: {
    serviceOfferingId: string;
    startsAt: string;
    durationMinutes: number;
  }) => {
    const search = new URLSearchParams();
    search.set('serviceOfferingId', params.serviceOfferingId);
    search.set('startsAt', params.startsAt);
    search.set('durationMinutes', String(requirePositiveInteger(params.durationMinutes, 'durationMinutes')));
    return get<PublicBookingAvailabilityDTO>(`/bookings/public/availability?${search.toString()}`);
  },
  list: (params?: { bookingId?: number; partyId?: number; engineerPartyId?: number }) => {
    const search = new URLSearchParams();
    setOptionalPositiveIntParam(search, 'bookingId', params?.bookingId);
    setOptionalPositiveIntParam(search, 'partyId', params?.partyId);
    setOptionalPositiveIntParam(search, 'engineerPartyId', params?.engineerPartyId);
    const qs = search.toString();
    return get<BookingDTO[]>(`/bookings${qs ? `?${qs}` : ''}`);
  },
  create: (body: {
    cbTitle: string;
    cbStartsAt: string;
    cbEndsAt: string;
    cbStatus: string;
    cbNotes?: string | null;
    cbServiceOfferingId: string;
    cbPartyId?: number | null;
    cbResourceIds?: string[] | null;
    cbEngineerPartyId?: number | null;
    cbEngineerName?: string | null;
  }) =>
    post<BookingDTO>('/bookings', {
      ...body,
      cbPartyId: normalizeOptionalPositiveInteger(body.cbPartyId, 'cbPartyId'),
      cbEngineerPartyId: normalizeOptionalPositiveInteger(body.cbEngineerPartyId, 'cbEngineerPartyId'),
    }),
  update: (bookingId: number, body: BookingUpdatePayload) =>
    put<BookingDTO>(`/bookings/${requirePositiveInteger(bookingId, 'bookingId')}`, body),
  createPublic: (body: {
    pbFullName: string;
    pbEmail: string;
    pbPhone?: string | null;
    pbServiceOfferingId: string;
    pbStartsAt: string;
    pbDurationMinutes?: number | null;
    pbNotes?: string | null;
    pbEngineerPartyId?: number | null;
    pbEngineerName?: string | null;
    pbResourceIds?: string[] | null;
  }) => {
    const { pbServiceOfferingId, ...rest } = body;
    const legacyServiceType = decodeLegacyServiceOfferingId(pbServiceOfferingId);
    return post<BookingDTO>('/bookings/public', {
      ...rest,
      ...(legacyServiceType
        ? { pbServiceType: legacyServiceType }
        : { pbServiceOfferingId }),
      pbEngineerPartyId: normalizeOptionalPositiveInteger(body.pbEngineerPartyId, 'pbEngineerPartyId'),
    });
  },
  createPublicCheckout: (body: PublicBookingCheckoutPayload, idempotencyKey: string) =>
    post<PublicBookingCheckoutDTO>('/bookings/public/checkout', {
      ...body,
      pbcEngineerPartyId: normalizeOptionalPositiveInteger(body.pbcEngineerPartyId, 'pbcEngineerPartyId'),
    }, {
      headers: { 'Idempotency-Key': idempotencyKey },
    }),
  getPublicCheckout: (bookingId: number, lookupToken: string) =>
    get<PublicBookingCheckoutDTO>(
      `/bookings/public/orders/${requirePositiveInteger(bookingId, 'bookingId')}`,
      { headers: { 'X-Order-Lookup-Token': lookupToken } },
    ),
};
