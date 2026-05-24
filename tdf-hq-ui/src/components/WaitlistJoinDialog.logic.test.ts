import { parseWaitlistQuantity } from './WaitlistJoinDialog.logic';

describe('parseWaitlistQuantity', () => {
  it('parses decimal quantity input with explicit radix semantics', () => {
    expect(parseWaitlistQuantity('8')).toBe(8);
    expect(parseWaitlistQuantity('08')).toBe(8);
    expect(parseWaitlistQuantity('010')).toBe(10);
  });

  it('falls back to one for empty, invalid, and zero values', () => {
    expect(parseWaitlistQuantity('')).toBe(1);
    expect(parseWaitlistQuantity('not-a-number')).toBe(1);
    expect(parseWaitlistQuantity('0')).toBe(1);
  });

  it('preserves out-of-range values for submit-time validation', () => {
    expect(parseWaitlistQuantity('-2')).toBe(-2);
    expect(parseWaitlistQuantity('11')).toBe(11);
  });
});
