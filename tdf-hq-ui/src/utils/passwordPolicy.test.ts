import { isValidAuthPassword } from './passwordPolicy';

describe('web authentication password policy', () => {
  it('matches the server Unicode length, UTF-8 byte, and hidden-character bounds', () => {
    expect(isValidAuthPassword('12345678')).toBe(true);
    expect(isValidAuthPassword('🎵🎵🎵🎵🎵🎵🎵🎵')).toBe(true);
    expect(isValidAuthPassword('🎵'.repeat(19))).toBe(false);
    expect(isValidAuthPassword('1234567')).toBe(false);
    expect(isValidAuthPassword('abcd\u200befgh')).toBe(false);
    expect(isValidAuthPassword('abcd\nefgh')).toBe(false);
  });
});
