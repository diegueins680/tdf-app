import { formatEventMoney } from './eventFinanceMoney';

describe('event finance cents', () => {
  beforeEach(() => window.localStorage.setItem('tdf-hq-ui/locale-preferences', JSON.stringify({ locale: 'en-US' })));
  afterEach(() => window.localStorage.clear());
  it.each([
    [9007199254740991, '$90,071,992,547,409.91'],
    [7036874417766401, '$70,368,744,177,664.01'],
    [-9007199254740991, '-$90,071,992,547,409.91'],
    [-1, '-$0.01'], [250, '$2.50'], [0, '$0.00'],
  ])('formats exact signed safe cents %s', (cents, expected) => {
    expect(formatEventMoney(cents, 'USD')).toBe(expected);
  });
  it.each([9007199254740992, -9007199254740992, NaN, Infinity, 1.5])('does not display unrepresentable cents %s', cents => {
    expect(formatEventMoney(cents, 'USD')).toBe('Monto no disponible');
  });
  it('keeps missing prices free', () => {
    expect(formatEventMoney(null, 'USD')).toBe('Gratis');
    expect(formatEventMoney(undefined, 'USD')).toBe('Gratis');
  });
  it('keeps currency fraction conventions', () => {
    expect(formatEventMoney(150, 'JPY')).toBe('¥2');
    expect(formatEventMoney(-150, 'JPY')).toBe('-¥2');
    expect(formatEventMoney(101, 'KWD')).toBe('KWD\u00a01.010');
  });
  it('matches normal currency rounding across small signed amounts and locale digit systems', () => {
    for (const locale of ['en-US', 'de-DE', 'ar-EG']) {
      window.localStorage.setItem('tdf-hq-ui/locale-preferences', JSON.stringify({ locale }));
      for (const currency of ['USD', 'JPY', 'KWD']) {
        const expected = new Intl.NumberFormat(locale, { style: 'currency', currency });
        for (let cents = -250; cents <= 250; cents++) {
          expect(formatEventMoney(cents, currency)).toBe(expected.format(cents / 100));
        }
      }
    }
  });
  it('preserves negative zero formatting', () => {
    expect(formatEventMoney(-0, 'USD')).toBe('-$0.00');
    expect(formatEventMoney(-1, 'JPY')).toBe('-¥0');
  });
  it('keeps the selected locale' , () => {
    window.localStorage.setItem('tdf-hq-ui/locale-preferences', JSON.stringify({ locale: 'de-DE' }));
    expect(formatEventMoney(9007199254740991, 'EUR')).toBe('90.071.992.547.409,91\u00a0€');
  });
});
