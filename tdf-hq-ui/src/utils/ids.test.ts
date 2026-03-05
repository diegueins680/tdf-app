import { parsePositiveSafeInt } from './ids';

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
  });

  it('rejects unsafe integer values', () => {
    expect(parsePositiveSafeInt(Number.MAX_SAFE_INTEGER + 1)).toBeNull();
    expect(parsePositiveSafeInt('9007199254740992')).toBeNull();
  });
});
