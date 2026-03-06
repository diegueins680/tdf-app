import {
  compareReleaseDateValues,
  formatReleaseDateLabel,
  parseReleaseTimestamp,
} from './releaseDate';

describe('parseReleaseTimestamp', () => {
  it('returns null for blank or invalid values', () => {
    expect(parseReleaseTimestamp(undefined)).toBeNull();
    expect(parseReleaseTimestamp('   ')).toBeNull();
    expect(parseReleaseTimestamp('not-a-date')).toBeNull();
  });

  it('keeps date-only values on their local calendar day', () => {
    const parsed = parseReleaseTimestamp('2026-03-05');
    expect(parsed).not.toBeNull();

    const local = new Date(parsed!);
    expect(local.getFullYear()).toBe(2026);
    expect(local.getMonth()).toBe(2);
    expect(local.getDate()).toBe(5);
  });

  it('rejects impossible date-only values', () => {
    expect(parseReleaseTimestamp('2026-02-30')).toBeNull();
  });

  it('parses full ISO timestamps', () => {
    const parsed = parseReleaseTimestamp('2026-03-05T18:30:00.000Z');
    expect(parsed).toBe(Date.parse('2026-03-05T18:30:00.000Z'));
  });
});

describe('compareReleaseDateValues', () => {
  it('sorts newest first and keeps invalid values last', () => {
    const values = ['invalid', '2024-01-01', '2026-01-01', null] as (string | null)[];
    const sorted = values.sort((a, b) => compareReleaseDateValues(a, b, 'desc'));

    expect(sorted).toEqual(['2026-01-01', '2024-01-01', 'invalid', null]);
  });

  it('sorts oldest first and keeps invalid values last', () => {
    const values = ['2026-01-01', 'invalid', '2024-01-01'] as (string | null)[];
    const sorted = values.sort((a, b) => compareReleaseDateValues(a, b, 'asc'));

    expect(sorted).toEqual(['2024-01-01', '2026-01-01', 'invalid']);
  });
});

describe('formatReleaseDateLabel', () => {
  it('returns fallback for invalid values', () => {
    expect(formatReleaseDateLabel('not-a-date', { day: 'numeric' })).toBe('Sin fecha');
  });

  it('formats values using Intl.DateTimeFormat', () => {
    const options: Intl.DateTimeFormatOptions = { month: 'short', day: 'numeric' };
    const expected = new Intl.DateTimeFormat('es-CO', options).format(
      new Date(parseReleaseTimestamp('2026-07-01')!),
    );

    expect(formatReleaseDateLabel('2026-07-01', options)).toBe(expected);
  });
});
