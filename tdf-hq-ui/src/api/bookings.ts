import { get, post, put } from './client';
import type { BookingDTO } from './types';

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
  ubServiceType?: string;
  ubStatus?: string;
  ubNotes?: string;
  ubStartsAt?: string;
  ubEndsAt?: string;
  ubResourceIds?: string[] | null;
  ubPartyId?: number | null;
  ubEngineerPartyId?: number | null;
  ubEngineerName?: string | null;
}

export const Bookings = {
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
    cbServiceType?: string | null;
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
    pbServiceType: string;
    pbStartsAt: string;
    pbDurationMinutes?: number | null;
    pbNotes?: string | null;
    pbEngineerPartyId?: number | null;
    pbEngineerName?: string | null;
    pbResourceIds?: string[] | null;
  }) =>
    post<BookingDTO>('/bookings/public', {
      ...body,
      pbEngineerPartyId: normalizeOptionalPositiveInteger(body.pbEngineerPartyId, 'pbEngineerPartyId'),
    }),
};
