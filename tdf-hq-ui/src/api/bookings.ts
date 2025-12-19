import { get, post, put } from './client';
import type { BookingDTO } from './types';

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
    if (params?.bookingId != null) search.set('bookingId', String(params.bookingId));
    if (params?.partyId != null) search.set('partyId', String(params.partyId));
    if (params?.engineerPartyId != null) search.set('engineerPartyId', String(params.engineerPartyId));
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
  }) => post<BookingDTO>('/bookings', body),
  update: (bookingId: number, body: BookingUpdatePayload) =>
    put<BookingDTO>(`/bookings/${bookingId}`, body),
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
  }) => post<BookingDTO>('/bookings/public', body),
};
