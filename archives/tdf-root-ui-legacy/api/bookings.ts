
import { get, post } from './client';
import type { BookingDTO } from './types';

export type CreateBookingReq = {
  cbTitle: string;
  cbStartsAt: string;
  cbEndsAt: string;
  cbStatus: string;
  cbNotes?: string | null;
  cbPartyId?: number | null;
  cbServiceType?: string | null;
  cbResourceIds?: string[];
};

export const Bookings = {
  list: () => get<BookingDTO[]>('/bookings'),
  listByParty: async (partyId: number) => {
    const params = new URLSearchParams({ partyId: String(partyId) });
    try {
      return await get<BookingDTO[]>(`/bookings?${params.toString()}`);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error ?? '');
      if (message.includes('404')) {
        return [];
      }
      throw error;
    }
  },
  create: (body: CreateBookingReq) => post<BookingDTO>('/bookings', body),
};
