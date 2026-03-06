import { defaultRoomsForService, sameRooms } from '../utils/publicBookingRooms';

describe('public booking room rules', () => {
  const rooms = ['Live Room', 'Control Room', 'Vocal Booth', 'DJ Booth'];

  it('suggests live + control for band recording', () => {
    expect(defaultRoomsForService('Grabación de Banda', rooms)).toEqual(['Live Room', 'Control Room']);
  });

  it('does not duplicate the same room when it matches multiple rules', () => {
    expect(defaultRoomsForService('Grabación de Banda', ['Live Control Room'])).toEqual([
      'Live Control Room',
    ]);
  });

  it('suggests live + control for audiovisual live recording', () => {
    expect(defaultRoomsForService('Grabación Audiovisual Live', rooms)).toEqual([
      'Live Room',
      'Control Room',
    ]);
  });

  it('matches audiovisual-live services with punctuation and ignores room ordering fallback', () => {
    const mixedOrderRooms = ['Vocal Booth', 'DJ Booth', 'Live Room', 'Control Room'];
    expect(defaultRoomsForService('Grabación Audiovisual (Live Session)', mixedOrderRooms)).toEqual([
      'Live Room',
      'Control Room',
    ]);
  });

  it('suggests live + vocal for vocal recording (diacritics/case insensitive)', () => {
    expect(defaultRoomsForService('GRABACIÓN DE VOZ', rooms)).toEqual(['Live Room', 'Vocal Booth']);
  });

  it('matches room names that include diacritics', () => {
    expect(defaultRoomsForService('Grabación de voz', ['Lívé Room', 'Bóóth Vócal'])).toEqual([
      'Lívé Room',
      'Bóóth Vócal',
    ]);
  });

  it('suggests control for mastering', () => {
    expect(defaultRoomsForService('Mastering', rooms)).toEqual(['Control Room']);
  });

  it('does not classify "masterclass" as mastering', () => {
    expect(defaultRoomsForService('Masterclass', rooms)).toEqual(['Live Room', 'Control Room']);
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

  it('compares room sets ignoring diacritics', () => {
    expect(sameRooms(['Lívé Room', 'Bóóth Vócal'], ['live room', 'booth vocal'])).toBe(true);
  });

  it('compares room sets ignoring punctuation differences', () => {
    expect(sameRooms(['Control-Room'], ['control room'])).toBe(true);
  });
});
