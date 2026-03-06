import { parsePositiveSafeInt, parseUnsignedSafeInt } from './ids';

describe('parseUnsignedSafeInt', () => {
  it('parses non-negative numbers and digit strings', () => {
    expect(parseUnsignedSafeInt(0)).toBe(0);
    expect(parseUnsignedSafeInt(42)).toBe(42);
    expect(parseUnsignedSafeInt(' 0042 ')).toBe(42);
  });

  it('rejects invalid integer formats', () => {
    expect(parseUnsignedSafeInt('')).toBeNull();
    expect(parseUnsignedSafeInt('abc')).toBeNull();
    expect(parseUnsignedSafeInt('-1')).toBeNull();
    expect(parseUnsignedSafeInt('12.5')).toBeNull();
    expect(parseUnsignedSafeInt('12abc')).toBeNull();
    expect(parseUnsignedSafeInt('abc12')).toBeNull();
  });

  it('rejects unsafe integer values', () => {
    expect(parseUnsignedSafeInt(Number.MAX_SAFE_INTEGER + 1)).toBeNull();
    expect(parseUnsignedSafeInt('9007199254740992')).toBeNull();
  });
});

describe('parsePositiveSafeInt', () => {
  it('parses positive integer numbers and digit strings', () => {
    expect(parsePositiveSafeInt(42)).toBe(42);
    expect(parsePositiveSafeInt(' 0042 ')).toBe(42);
  });

  it('rejects non-positive and non-integer values', () => {
    expect(parsePositiveSafeInt(0)).toBeNull();
    expect(parsePositiveSafeInt(-5)).toBeNull();
    expect(parsePositiveSafeInt(12.5)).toBeNull();
    expect(parsePositiveSafeInt('abc')).toBeNull();
    expect(parsePositiveSafeInt('12.5')).toBeNull();
    expect(parsePositiveSafeInt('12abc')).toBeNull();
  });

  it('rejects unsafe integer values', () => {
    expect(parsePositiveSafeInt(Number.MAX_SAFE_INTEGER + 1)).toBeNull();
    expect(parsePositiveSafeInt('9007199254740992')).toBeNull();
  });
});
