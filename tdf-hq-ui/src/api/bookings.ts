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
  list: () => get<BookingDTO[]>('/bookings'),
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
  }) => post<BookingDTO>('/bookings/public', body),
};
