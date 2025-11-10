import { get, post } from './client';
import type { BookingDTO } from './types';

export const Bookings = {
  list: () => get<BookingDTO[]>('/bookings'),
  create: (body: { cbTitle: string; cbStartsAt: string; cbEndsAt: string; cbStatus: string; cbNotes?: string | null }) =>
    post<BookingDTO>('/bookings', body),
};
