import { formatInEventTimeZone, formatTimestampForDisplay, parseTimestamp } from './dateTime';

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

  it('returns null for impossible ISO calendar dates', () => {
    expect(parseTimestamp('2026-02-30T18:30:00Z')).toBeNull();
  });

  it('returns null for impossible ISO dates with direct timezone suffixes', () => {
    expect(parseTimestamp('2026-02-30Z')).toBeNull();
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

  it('returns original value for impossible ISO calendar dates', () => {
    expect(formatTimestampForDisplay('2026-02-30T18:30:00Z')).toBe('2026-02-30T18:30:00Z');
  });

  it('returns original value for impossible ISO dates with direct timezone suffixes', () => {
    expect(formatTimestampForDisplay('2026-02-30Z')).toBe('2026-02-30Z');
  });

  it('formats valid timestamps using local datetime formatting', () => {
    const iso = '2026-03-04T18:30:00.000Z';
    expect(formatTimestampForDisplay(iso)).toBe(new Date(iso).toLocaleString());
  });
});

describe('formatInEventTimeZone', () => {
  const date = new Date('2026-11-28T23:00:00Z');

  it('formats in a supported IANA zone', () => {
    expect(formatInEventTimeZone(date, 'en-US', 'America/Guayaquil', { hour: 'numeric', hour12: false }))
      .toBe('18');
  });

  it('falls back to the viewer zone instead of throwing for a zone Intl rejects', () => {
    expect(() => date.toLocaleString('en-US', { timeZone: 'UTC+5' })).toThrow(RangeError);
    expect(formatInEventTimeZone(date, 'en-US', 'UTC+5', { hour: 'numeric', hour12: false }))
      .toBe(date.toLocaleString('en-US', { hour: 'numeric', hour12: false }));
  });
});
