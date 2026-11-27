import { formatCourseMetadataPriceInput } from './coursePriceInput';
import { parseOptionalPriceCents } from './priceInput';

describe('course metadata price reads', () => {
  it.each([[25.5, '25.50'], [1.1, '1.10'], [0.29, '0.29'], [0, '0.00'], [null, ''], [undefined, '']] as const)('formats %s without rounding whole units', (price, expected) => {
    expect(formatCourseMetadataPriceInput(price)).toBe(expected);
  });
  it.each([-1, 1.005, NaN, Infinity, Number.MAX_SAFE_INTEGER / 100, 1e20])('refuses an invalid or ambiguous read %s', price => {
    expect(() => formatCourseMetadataPriceInput(price)).toThrow();
  });
  it('never reconstructs a different cent from accepted numeric metadata across the safe write range', () => {
    let seed = 921779n, accepted = 0;
    for (let i = 0; i < 2000; i += 1) {
      seed = (seed * 48271n) % BigInt(Number.MAX_SAFE_INTEGER);
      const originalCents = Number(seed);
      let input: string;
      try { input = formatCourseMetadataPriceInput(originalCents / 100); }
      catch { continue; }
      accepted += 1;
      expect(parseOptionalPriceCents(input)).toBe(originalCents);
    }
    expect(accepted).toBeGreaterThan(1000);
  });
});
