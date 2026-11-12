import { formatPriceCentsInput, parseOptionalPriceCents } from './priceInput';

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
