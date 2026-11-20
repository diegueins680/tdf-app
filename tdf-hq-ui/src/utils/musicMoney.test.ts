import { canonicalMusicMinor, formatMusicMinor, musicMinorForWire, formatMusicPriceInput, parseMusicPriceInput } from './musicMoney';

describe('PAY-MUSIC-MONEY-WIRE-001', () => {
  it.each([
    ['0', '$0.00'], ['1', '$0.01'], ['250', '$2.50'],
    ['9007199254740991', '$90,071,992,547,409.91'],
    ['9007199254740993', '$90,071,992,547,409.93'],
    ['9223372036854775807', '$92,233,720,368,547,758.07'],
  ])('formats %s without converting its whole amount to Number', (minor, expected) => {
    expect(formatMusicMinor(minor, 'USD', 'en-US')).toBe(expected);
  });

  it('round trips deterministic samples across the supported minor-unit range', () => {
    const maximum = 9223372036854775807n;
    let value = 9007199254740993n;
    for (let index = 0; index < 2000; index += 1) {
      value = (value * 48271n + 1234567n) % maximum;
      const text = value.toString();
      const response: unknown = JSON.parse(JSON.stringify(musicMinorForWire(text)));
      expect(canonicalMusicMinor(response)).toBe(text);
      expect(parseMusicPriceInput(formatMusicPriceInput(text))).toBe(text);
    }
  });

  it('preserves legacy safe numbers but never rounds an unsafe number', () => {
    expect(musicMinorForWire('250')).toBe(250);
    expect(musicMinorForWire('9007199254740991')).toBe(9007199254740991);
    expect(musicMinorForWire('9007199254740993')).toBe('9007199254740993');
    expect(canonicalMusicMinor(Number('9007199254740993'))).toBeNull();
    expect(() => formatMusicMinor(Number('9007199254740993'), 'USD')).toThrow();
  });

  it.each(['01', '-0', '-1', '+1', ' 1', '1 ', '1e2', '1.0', '0x10',
    '9223372036854775808', '9'.repeat(100)])('refuses noncanonical price %s', (value) => {
    expect(canonicalMusicMinor(value)).toBeNull();
    expect(() => musicMinorForWire(value)).toThrow();
  });

  it('does not invent support for another sale currency', () => {
    expect(() => formatMusicMinor('250', 'JPY')).toThrow();
  });
});


describe('music price entry in USD units', () => {
  it.each([
    ['0', '0.00'], ['1', '0.01'], ['499', '4.99'],
    ['9007199254740993', '90071992547409.93'],
    ['9223372036854775807', '92233720368547758.07'],
  ])('round trips exact minor text %s through decimal input', (minor, decimal) => {
    expect(formatMusicPriceInput(minor)).toBe(decimal);
    expect(parseMusicPriceInput(decimal)).toBe(minor);
    expect(parseMusicPriceInput(decimal.replace('.', ','))).toBe(minor);
  });
  it.each(['-1', '1e2', '4.999', '92233720368547758.08'])('rejects %s without narrowing the wire range or rounding', value => {
    expect(() => parseMusicPriceInput(value)).toThrow();
  });
  it('distinguishes missing price and zero, and rejects imprecise loaded numbers', () => {
    expect(formatMusicPriceInput(null)).toBe('');
    expect(parseMusicPriceInput('')).toBeNull();
    expect(parseMusicPriceInput('0')).toBe('0');
    expect(() => formatMusicPriceInput(9007199254740992)).toThrow();
  });
});
