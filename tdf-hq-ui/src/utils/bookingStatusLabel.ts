/** Spanish names for the booking statuses the API reports in English; the same labels studio orders (OrdersPage) show. */
export const BOOKING_STATUS_LABELS = {
  Tentative: 'Tentativa',
  Confirmed: 'Confirmada',
  InProgress: 'En preparación',
  Completed: 'Completada',
  Cancelled: 'Cancelada',
  NoShow: 'No asistencia',
} as const;

export type BookingStatusValue = keyof typeof BOOKING_STATUS_LABELS;

const isBookingStatus = (value: string): value is BookingStatusValue =>
  Object.prototype.hasOwnProperty.call(BOOKING_STATUS_LABELS, value);

/** An unknown status is shown as received rather than hidden. */
export function bookingStatusLabel(status: string | null | undefined): string {
  const value = status?.trim() ?? '';
  return isBookingStatus(value) ? BOOKING_STATUS_LABELS[value] : value;
}
