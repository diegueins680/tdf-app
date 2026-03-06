const DATE_ONLY_PATTERN = /^(\d{4})-(\d{2})-(\d{2})$/;

const parseDateOnly = (value: string): number | null => {
  const match = DATE_ONLY_PATTERN.exec(value);
  if (!match) return null;

  const yearRaw = match[1];
  const monthRaw = match[2];
  const dayRaw = match[3];
  if (!yearRaw || !monthRaw || !dayRaw) return null;

  const year = Number.parseInt(yearRaw, 10);
  const month = Number.parseInt(monthRaw, 10);
  const day = Number.parseInt(dayRaw, 10);

  if (!Number.isInteger(year) || !Number.isInteger(month) || !Number.isInteger(day)) {
    return null;
  }

  const parsed = new Date(year, month - 1, day, 0, 0, 0, 0);
  const isSameCalendarDate =
    parsed.getFullYear() === year &&
    parsed.getMonth() === month - 1 &&
    parsed.getDate() === day;

  return isSameCalendarDate ? parsed.getTime() : null;
};

export const parseReleaseTimestamp = (value?: string | null): number | null => {
  const trimmed = value?.trim();
  if (!trimmed) return null;

  if (DATE_ONLY_PATTERN.test(trimmed)) {
    return parseDateOnly(trimmed);
  }

  const parsed = Date.parse(trimmed);
  return Number.isNaN(parsed) ? null : parsed;
};

export const compareReleaseDateValues = (
  left?: string | null,
  right?: string | null,
  direction: 'asc' | 'desc' = 'desc',
): number => {
  const leftTs = parseReleaseTimestamp(left);
  const rightTs = parseReleaseTimestamp(right);

  if (leftTs === null && rightTs === null) return 0;
  if (leftTs === null) return 1;
  if (rightTs === null) return -1;

  return direction === 'asc' ? leftTs - rightTs : rightTs - leftTs;
};

export const formatReleaseDateLabel = (
  value: string | null | undefined,
  options: Intl.DateTimeFormatOptions,
  locale = 'es-CO',
  fallback = 'Sin fecha',
): string => {
  const ts = parseReleaseTimestamp(value);
  if (ts === null) return fallback;
  return new Intl.DateTimeFormat(locale, options).format(new Date(ts));
};
