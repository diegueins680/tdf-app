import { formatTimestampForDisplay, parseTimestamp } from './dateTime';

describe('parseTimestamp', () => {
  it('returns null for empty values', () => {
    expect(parseTimestamp(undefined)).toBeNull();
    expect(parseTimestamp(null)).toBeNull();
    expect(parseTimestamp('')).toBeNull();
    expect(parseTimestamp('   ')).toBeNull();
  });

  it('returns null for invalid timestamp values', () => {
    expect(parseTimestamp('not-a-date')).toBeNull();
  });

  it('parses valid timestamps', () => {
    const parsed = parseTimestamp('2026-03-04T18:30:00.000Z');
    expect(parsed?.toISOString()).toBe('2026-03-04T18:30:00.000Z');
  });
});

describe('formatTimestampForDisplay', () => {
  it('returns fallback for missing values', () => {
    expect(formatTimestampForDisplay(undefined)).toBe('—');
    expect(formatTimestampForDisplay('   ', 'N/A')).toBe('N/A');
  });

  it('returns original value when timestamp is invalid', () => {
    expect(formatTimestampForDisplay('not-a-date')).toBe('not-a-date');
  });

  it('formats valid timestamps using local datetime formatting', () => {
    const iso = '2026-03-04T18:30:00.000Z';
    expect(formatTimestampForDisplay(iso)).toBe(new Date(iso).toLocaleString());
  });
});
