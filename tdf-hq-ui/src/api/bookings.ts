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
  }) => post<BookingDTO>('/bookings', body),
  update: (bookingId: number, body: BookingUpdatePayload) =>
    put<BookingDTO>(`/bookings/${bookingId}`, body),
};
