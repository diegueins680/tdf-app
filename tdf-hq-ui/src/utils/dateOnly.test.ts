import { toLocalDateInputValue } from './dateOnly';

describe('toLocalDateInputValue', () => {
  it('formats a valid date in YYYY-MM-DD using local calendar values', () => {
    const date = new Date(2026, 2, 4, 18, 45, 12);
    expect(toLocalDateInputValue(date)).toBe('2026-03-04');
  });

  it('returns an empty string for invalid dates', () => {
    const invalidDate = new Date('not-a-date');
    expect(toLocalDateInputValue(invalidDate)).toBe('');
  });

  it('returns a YYYY-MM-DD value when no date is provided', () => {
    expect(toLocalDateInputValue()).toMatch(/^\d{4}-\d{2}-\d{2}$/);
  });
});
