import { bookingStatusLabel } from './bookingStatusLabel';

describe('bookingStatusLabel', () => {
  it('names the known statuses in Spanish', () => {
    expect(bookingStatusLabel('Tentative')).toBe('Tentativa');
    expect(bookingStatusLabel('NoShow')).toBe('No asistencia');
    expect(bookingStatusLabel(' Confirmed ')).toBe('Confirmada');
  });

  it('shows an unknown status as received and tolerates a missing one', () => {
    expect(bookingStatusLabel('Rescheduled')).toBe('Rescheduled');
    expect(bookingStatusLabel(null)).toBe('');
    expect(bookingStatusLabel('toString')).toBe('toString');
  });
});
