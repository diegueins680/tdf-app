import { parseDateForDisplay, toLocalDateInputValue } from './dateOnly';

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

describe('parseDateForDisplay', () => {
  it('parses date-only strings in local calendar without shifting day', () => {
    const parsed = parseDateForDisplay('2026-03-04');
    expect(parsed).not.toBeNull();
    expect(parsed?.getFullYear()).toBe(2026);
    expect(parsed?.getMonth()).toBe(2);
    expect(parsed?.getDate()).toBe(4);
  });

  it('rejects impossible date-only values', () => {
    expect(parseDateForDisplay('2026-02-30')).toBeNull();
  });

  it('parses full timestamp values', () => {
    const parsed = parseDateForDisplay('2026-03-04T18:30:00Z');
    expect(parsed).not.toBeNull();
    expect(parsed?.toISOString()).toBe('2026-03-04T18:30:00.000Z');
  });
});
