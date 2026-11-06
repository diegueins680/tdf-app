import { estimateTicketFees, parseOptionalPriceCents } from './eventTicketPricing';

describe('standard ticket fee estimate', () => {
  it('shows the real Psique presale and door estimates before saving', () => {
    expect(estimateTicketFees(2000)).toEqual({ buyer: 40, organizer: 40, checkout: 2040, proceeds: 1960 });
    expect(estimateTicketFees(3000)).toEqual({ buyer: 60, organizer: 60, checkout: 3060, proceeds: 2940 });
  });
  it('conserves cents and assigns an odd fee cent to the buyer', () => {
    for (let face = 0; face <= 100000; face++) {
      const fee = estimateTicketFees(face);
      const total = Math.floor(face * 4 / 100);
      expect(fee.buyer + fee.organizer).toBe(total);
      expect(fee.checkout - fee.proceeds).toBe(total);
      expect(fee.buyer - fee.organizer).toBe(total % 2);
    }
  });
  it('rejects fractional cents, unsafe totals and ambiguous amounts', () => {
    for (const value of [-1, 1.5, Infinity, NaN, Number.MAX_SAFE_INTEGER]) {
      expect(() => estimateTicketFees(value)).toThrow();
    }
    for (const value of ['20.001', '-20', '1e3', '90071992547409.92']) {
      expect(() => parseOptionalPriceCents(value)).toThrow();
    }
    expect(parseOptionalPriceCents('90071992547409.91')).toBe(Number.MAX_SAFE_INTEGER);
    expect(parseOptionalPriceCents('20,40')).toBe(2040);
  });
});
