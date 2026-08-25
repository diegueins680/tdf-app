import {
  WAITLIST_DEFAULT_QUANTITY,
  WAITLIST_MAX_QUANTITY,
  parseWaitlistQuantity,
} from './WaitlistJoinDialog.logic';

const WAITLIST_SINGLE_DIGIT_QUANTITY = 8;
const WAITLIST_SINGLE_DIGIT_TEXT = String(WAITLIST_SINGLE_DIGIT_QUANTITY);
const WAITLIST_SINGLE_DIGIT_WITH_LEADING_ZERO_TEXT = `0${WAITLIST_SINGLE_DIGIT_QUANTITY}`;
const WAITLIST_MAX_WITH_LEADING_ZERO_TEXT = `0${WAITLIST_MAX_QUANTITY}`;
const WAITLIST_ABOVE_VISIBLE_RANGE_QUANTITY = WAITLIST_MAX_QUANTITY + 1;
const WAITLIST_ABOVE_VISIBLE_RANGE_TEXT = String(WAITLIST_ABOVE_VISIBLE_RANGE_QUANTITY);

describe('parseWaitlistQuantity', () => {
  it('parses decimal quantity input with explicit radix semantics', () => {
    expect(parseWaitlistQuantity(WAITLIST_SINGLE_DIGIT_TEXT)).toBe(WAITLIST_SINGLE_DIGIT_QUANTITY);
    expect(parseWaitlistQuantity(WAITLIST_SINGLE_DIGIT_WITH_LEADING_ZERO_TEXT)).toBe(WAITLIST_SINGLE_DIGIT_QUANTITY);
    expect(parseWaitlistQuantity(WAITLIST_MAX_WITH_LEADING_ZERO_TEXT)).toBe(WAITLIST_MAX_QUANTITY);
  });

  it('falls back to one for empty, invalid, and zero values', () => {
    expect(parseWaitlistQuantity('')).toBe(WAITLIST_DEFAULT_QUANTITY);
    expect(parseWaitlistQuantity('not-a-number')).toBe(WAITLIST_DEFAULT_QUANTITY);
    expect(parseWaitlistQuantity('0')).toBe(WAITLIST_DEFAULT_QUANTITY);
  });

  it('preserves out-of-range values for submit-time validation', () => {
    expect(parseWaitlistQuantity('-2')).toBe(-2);
    expect(parseWaitlistQuantity(WAITLIST_ABOVE_VISIBLE_RANGE_TEXT)).toBe(WAITLIST_ABOVE_VISIBLE_RANGE_QUANTITY);
  });
});
