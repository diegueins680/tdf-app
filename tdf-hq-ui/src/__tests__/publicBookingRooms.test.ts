import { defaultRoomsForService, sameRooms } from '../utils/publicBookingRooms';

describe('public booking room rules', () => {
  const rooms = ['Live Room', 'Control Room', 'Vocal Booth', 'DJ Booth'];

  it('suggests live + control for band recording', () => {
    expect(defaultRoomsForService('Grabación de Banda', rooms)).toEqual(['Live Room', 'Control Room']);
  });

  it('suggests live + control for audiovisual live recording', () => {
    expect(defaultRoomsForService('Grabación Audiovisual Live', rooms)).toEqual([
      'Live Room',
      'Control Room',
    ]);
  });

  it('suggests live + vocal for vocal recording (diacritics/case insensitive)', () => {
    expect(defaultRoomsForService('GRABACIÓN DE VOZ', rooms)).toEqual(['Live Room', 'Vocal Booth']);
  });

  it('suggests control for mastering', () => {
    expect(defaultRoomsForService('Mastering', rooms)).toEqual(['Control Room']);
  });

  it('suggests live for rehearsal/ensayo', () => {
    expect(defaultRoomsForService('Ensayo', rooms)).toEqual(['Live Room']);
    expect(defaultRoomsForService('Rehearsal', rooms)).toEqual(['Live Room']);
  });

  it('falls back to first two rooms for unknown services', () => {
    expect(defaultRoomsForService('Servicio raro', rooms)).toEqual(['Live Room', 'Control Room']);
  });

  it('compares room sets ignoring order/case/whitespace', () => {
    expect(sameRooms(['Control Room', 'Live Room'], ['live room', ' control room '])).toBe(true);
    expect(sameRooms(['Control Room'], ['Live Room'])).toBe(false);
  });
});
