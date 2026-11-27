import { formatPriceCentsInput, parseOptionalPriceCents, parseOptionalSignedAmountCents, formatSignedAmountCentsInput } from './priceInput';

it('round-trips cents exactly, including values that floating point division rounds incorrectly', () => {
  expect(formatPriceCentsInput(2550)).toBe('25.50');
  expect(formatPriceCentsInput(0)).toBe('0.00');
  expect(formatPriceCentsInput(1)).toBe('0.01');
  expect(formatPriceCentsInput(Number.MAX_SAFE_INTEGER)).toBe('90071992547409.91');
  expect(parseOptionalPriceCents(formatPriceCentsInput(Number.MAX_SAFE_INTEGER))).toBe(Number.MAX_SAFE_INTEGER);
  expect(parseOptionalPriceCents('25,50')).toBe(2550);
});

it('distinguishes no quoted price from a zero price and refuses lossy source amounts', () => {
  expect(formatPriceCentsInput(null)).toBe('');
  expect(parseOptionalPriceCents('')).toBeNull();
  expect(() => formatPriceCentsInput(Number.MAX_SAFE_INTEGER + 1)).toThrow();
  expect(() => formatPriceCentsInput(1.5)).toThrow();
  expect(() => formatPriceCentsInput(-1)).toThrow();
});


describe('signed amount range input', () => {
  it.each([['0', 0], ['0,00', 0], ['-1.01', -101], ['25,50', 2550], ['', null]])('reads %s exactly', (raw, expected) => {
    expect(parseOptionalSignedAmountCents(String(raw))).toBe(expected);
  });
  it('round trips both safe integer bounds and keeps blank distinct from zero', () => {
    for (const value of [Number.MAX_SAFE_INTEGER, Number.MIN_SAFE_INTEGER, 0]) {
      expect(parseOptionalSignedAmountCents(formatSignedAmountCentsInput(value))).toBe(value);
    }
    expect(formatSignedAmountCentsInput(undefined)).toBe('');
    expect(formatSignedAmountCentsInput(0)).toBe('0.00');
  });
  it.each(['-', '- 1', '1e2', '-1.001', '90071992547409.92'])('refuses invalid signed range %s', value => {
    expect(() => parseOptionalSignedAmountCents(value)).toThrow();
  });
});
